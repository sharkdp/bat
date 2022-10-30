with Chests.Ring_Buffers;
with USB.Device.HID.Keyboard;

package body Click is
   ----------------
   --  DEBOUNCE  --
   ----------------

   --  Ideally, in a separate package.

   --  should be [], but not fixed yet in GCC 11.
   Current_Status : Key_Matrix := [others => [others => False]];
   New_Status : Key_Matrix := [others => [others => False]];
   Since : Natural := 0;
   --   Nb_Bounce : Natural := 5;

   function Update (NewS : Key_Matrix) return Boolean is
   begin
      --  The new state is the same as the current stable state => Do nothing.
      if Current_Status = NewS then
         Since := 0;
         return False;
      end if;

      if New_Status /= NewS then
         --  The new state differs from the previous
         --  new state (bouncing) => reset
         New_Status := NewS;
         Since := 1;
      else
         --  The new state hasn't changed since last
         --  update => towards stabilization.
         Since := Since + 1;
      end if;

      if Since > Nb_Bounce then
         declare
            Tmp : constant Key_Matrix := Current_Status;
         begin
            --  New state has been stable enough.
            --  Latch it and notifies caller.
            Current_Status := New_Status;
            New_Status := Tmp;
            Since := 0;
         end;

         return True;
      else
         --  Not there yet
         return False;
      end if;
   end Update;

   procedure Get_Matrix;
   --  Could use := []; but GNAT 12 has a bug (fixed in upcoming 13)
   Read_Status : Key_Matrix := [others => [others => False]];

   function Get_Events return Events is
      Num_Evt : Natural := 0;
      New_S : Key_Matrix renames Read_Status;
   begin
      Get_Matrix;
      if Update (New_S) then
         for I in Current_Status'Range (1) loop
            for J in Current_Status'Range (2) loop
               if (not New_Status (I, J) and then Current_Status (I, J))
                 or else (New_Status (I, J) and then not Current_Status (I, J))
               then
                  Num_Evt := Num_Evt + 1;
               end if;
            end loop;
         end loop;

         declare
            Evts : Events (Natural range 1 .. Num_Evt);
            Cursor : Natural range 1 .. Num_Evt + 1 := 1;
         begin
            for I in Current_Status'Range (1) loop
               for J in Current_Status'Range (2) loop
                  if not New_Status (I, J)
                    and then Current_Status (I, J)
                  then
                     --  Pressing I, J
                     Evts (Cursor) := [
                                       Evt => Press,
                                       Col => I,
                                       Row => J
                                      ];
                     Cursor := Cursor + 1;
                  elsif New_Status (I, J)
                    and then not Current_Status (I, J)
                  then
                     --  Release I, J
                     Evts (Cursor) := [
                                       Evt => Release,
                                       Col => I,
                                       Row => J
                                      ];
                     Cursor := Cursor + 1;
                  end if;
               end loop;
            end loop;
            return Evts;
         end;
      end if;

      return [];
   end Get_Events;

   procedure Get_Matrix  is -- return Key_Matrix is
   begin
      for Row in Keys.Rows'Range loop
         Keys.Rows (Row).Clear;

         for Col in Keys.Cols'Range loop
            Read_Status (Col, Row) := not Keys.Cols (Col).Set;
         end loop;
         Keys.Rows (Row).Set;
      end loop;
   end Get_Matrix;

   --  End of DEBOUNCE

   --------------
   --  Layout  --
   --------------

   package Events_Ring_Buffers is new Chests.Ring_Buffers
     (Element_Type => Event,
      Capacity     => 16);

   Queued_Events : Events_Ring_Buffers.Ring_Buffer;

   type Statet is (Normal_Key, Layer_Mod, None);
   type State is record
      Typ : Statet;
      Code : Key_Code_T;
      Layer_Value : Natural;
      --  Col : ColR;
      --  Row : RowR;
   end record;

   type State_Array is array (ColR, RowR) of State;
   States : State_Array := [others => [others => (Typ => None, Code => No, Layer_Value => 0)]];

   function Kw (Code : Key_Code_T) return Action is
   begin
      return (T => Key, C => Code, L => 0);
   end Kw;

   function Lw (V : Natural) return Action is
   begin
      return (T => Layer, C => No, L => V);
   end Lw;

   --  FIXME: hardcoded max number of events
   subtype Events_Range is Natural range 0 .. 60;
   type Array_Of_Reg_Events is array (Events_Range) of Event;

   Stamp : Natural := 0;

   procedure Register_Events (L : Layout; Es : Events) is
   begin
      Stamp := Stamp + 1;

      Log ("Reg events: " & Stamp'Image);
      Log (Es'Length'Image);
      for E of Es loop
         declare
         begin
            if Events_Ring_Buffers.Is_Full (Queued_Events) then
               raise Program_Error;
            end if;

            Events_Ring_Buffers.Append (Queued_Events, E);
         end;
         --         Log ("Reg'ed events:" &  Events_Mark'Image);
         Log ("Reg'ed events:" &  Events_Ring_Buffers.Length (Queued_Events)'Image);
      end loop;
   end Register_Events;

   procedure Release (Col: Colr; Row: Rowr) is
   begin
      if States (Col, Row).Typ = None then
         raise Program_Error;
      end if;
      States (Col, Row) := (Typ => None, Code => No, Layer_Value => 0);
   end Release;

   function Get_Current_Layer return Natural is
      L : Natural := 0;
   begin
      for S of States loop
         if S.Typ = Layer_Mod then
            L := L + S.Layer_Value;
         end if;
      end loop;

      return L;
   end Get_Current_Layer;

   --  Tick the event.
   --  Returns TRUE if it needs to stay in the queued events
   --  FALSE if the event has been consumed.

   function Tick (L: Layout; E : in out Event) return Boolean is
      Current_Layer : Natural := Get_Current_Layer;
      A : Action renames L (Current_Layer, E.Row, E.Col);
   begin
      case E.Evt is
         when Press =>
            case A.T is
               when Key =>
                  States (E.Col, E.Row) :=
                    (Typ => Normal_Key,
                     Code => A.C,
                     Layer_Value => 0);
               when Layer =>
                  States (E.Col, E.Row) := (Typ => Layer_Mod, Layer_Value => A.L, Code => No);
               when others =>
                  raise Program_Error;
            end case;

         when Release =>
            Release (E.Col, E.Row);
      end case;
      return False;
   end Tick;

   Last_Was_Empty_Log : Boolean := False;

   procedure Tick (L : Layout) is
   begin
      for I in 1 .. Events_Ring_Buffers.Length(Queued_Events) loop
         declare
            E : Event := Events_Ring_Buffers.Last_Element (Queued_Events);
         begin
            Events_Ring_Buffers.Delete_Last (Queued_Events);
            if Tick (L, E) then
               Events_Ring_Buffers.Prepend (Queued_Events, E);
            end if;
         end;
      end loop;
      if not Last_Was_Empty_Log or else Events_Ring_Buffers.Length(Queued_Events) /= 0 then
         Log ("End Tick layout, events: " & Events_Ring_Buffers.Length(Queued_Events)'Image);
         Last_Was_Empty_Log := Events_Ring_Buffers.Length(Queued_Events) = 0;
      end if;
   end Tick;

   function Get_Key_Codes return Key_Codes_T is
      Codes : Key_Codes_T (0 .. 10);
      Wm: Natural := 0;
   begin
      for S of States loop
         if S.Typ = Normal_Key and then
            (S.Code < LCtrl or else S.Code > RGui)
         then
            Codes (Wm) := S.Code;
            Wm := Wm + 1;
         end if;
      end loop;

      if Wm = 0 then
         return [];
      else
         return Codes (0 .. Wm - 1);
      end if;
   end Get_Key_Codes;

   function Get_Modifiers return Key_Modifiers is
      use USB.Device.HID.Keyboard;
      KM : Key_Modifiers (1..8);
      I : Natural := 0;
   begin
      for S of States loop
         if S.Typ = Normal_Key then
            I := I + 1;
            case S.Code is
              when LCtrl =>
                 KM(I) := Ctrl_Left;
              when RCtrl =>
                 KM(I) := Ctrl_Right;
              when LShift =>
                 KM(I) := Shift_Left;
              when RShift =>
                 KM(I) := Shift_Right;
              when LAlt =>
                 KM(I) := Alt_Left;
              when RAlt =>
                 KM(I) := Alt_Right;
              when LGui =>
                 KM(I) := Meta_Left;
              when RGui =>
                 KM(I) := Meta_Right;
              when others =>
                 I := I - 1;
            end case;
         end if;
      end loop;
      return KM (1..I);
   end Get_Modifiers;

   procedure Init is
   begin
      Events_Ring_Buffers.Clear (Queued_Events);
   end Init;

end Click;
