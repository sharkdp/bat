with HAL.GPIO;
with USB.Device.HID.Keyboard;

generic
   Nb_Bounce : Natural;
   type ColR is (<>);
   type RowR is (<>);

   type GPIOP is new HAL.GPIO.GPIO_Point with private;

   type Cols_T is array (ColR) of GPIOP;
   type Rows_T is array (RowR) of GPIOP;

   Cols : Cols_T;
   Rows : Rows_T;
   Num_Layers : Natural;

   with procedure Log (S : String; L :  Integer := 1; Deindent : Integer := 0);
package Click is

   type Keys_T is record
      Cols : Cols_T;
      Rows : Rows_T;
   end record;

   Keys : Keys_T :=
     (Rows => Rows, Cols => Cols);

   type Key_Matrix is array (ColR, RowR) of Boolean;

   --------------------------
   --  Events & Debouncing --
   --------------------------

   MaxEvents : constant Positive := 20;

   type EventT is (Press, Release);
   type Event is record
      Evt : EventT;
      Col : ColR;
      Row : RowR;
   end record;

   type Events is array (Natural range <>) of Event;

   function Get_Events return Events;
   function Update (NewS : Key_Matrix) return Boolean;

   -------------
   --  Layout --
   -------------
   ---------------
   --  Keycodes --
   ---------------

   --  Keycodes copy/pasted from the excelent Keyberon Rust firmware:
   --  https://github.com/TeXitoi/keyberon/

   type Key_Code_T is
     (
      --   The "no" key, a placeholder to express nothing.
      No, --  = 0x00,
          --  / Error if too much keys are pressed at
          --  the same time.
      ErrorRollOver,
      --  / The POST fail error.
      PostFail,
      --  / An undefined error occured.
      ErrorUndefined,
      --  / `a` and `A`.
      A,
      B,
      C,
      D,
      E,
      F,
      G,
      H,
      I,
      J,
      K,
      L,
      M, --  0x10
      N,
      O,
      P,
      Q,
      R,
      S,
      T,
      U,
      V,
      W,
      X,
      Y,
      Z,
      --   `1` and `!`.
      Kb1,
      --   `2` and `@`.
      Kb2,
      --   `3` and `#`.
      Kb3, --  0x20
      --  / `4` and `$`.
      Kb4,
      --   `5` and `%`.
      Kb5,
      --   `6` and `^`.
      Kb6,
      --   `7` and `&`.
      Kb7,
      --   `8` and `*`.
      Kb8,
      --   `9` and `(`.
      Kb9,
      --   `0` and `)`.
      Kb0,
      Enter,
      Escape,
      BSpace,
      Tab,
      Space,
      --   `-` and `_`.
      Minus,
      --   `=` and `+`.
      Equal,
      --   `[` and `{`.
      LBracket,
      --   `]` and `}`.
      RBracket, --  0x30
      --  / `\` and `|`.
      Bslash,
      --   Non-US `#` and `~` (Typically near the Enter key).
      NonUsHash,
      --   `;` and `:`.
      SColon,
      --   `'` and `"`.
      Quote,
      --  How to have ` as code?
      --   \` and `~`.
      Grave,
      --   `,` and `<`.
      Comma,
      --   `.` and `>`.
      Dot,
      --   `/` and `?`.
      Slash,
      CapsLock,
      F1,
      F2,
      F3,
      F4,
      F5,
      F6,
      F7, --  0x40
      F8,
      F9,
      F10,
      F11,
      F12,
      PScreen,
      ScrollLock,
      Pause,
      Insert,
      Home,
      PgUp,
      Delete,
      Endd,
      PgDown,
      Right,
      Left, --  0x50
      Down,
      Up,
      NumLock,
      --   Keypad `/`
      KpSlash,
      --   Keypad `*`
      KpAsterisk,
      --   Keypad `-`.
      KpMinus,
      --   Keypad `+`.
      KpPlus,
      --   Keypad enter.
      KpEnter,
      --   Keypad 1.
      Kp1,
      Kp2,
      Kp3,
      Kp4,
      Kp5,
      Kp6,
      Kp7,
      Kp8, --  0x60
      Kp9,
      Kp0,
      KpDot,
      --   Non-US `\` and `|` (Typically near the Left-Shift key)
      NonUsBslash,
      Application, --  0x65
      --  / not a key, used for errors
      Power,
      --   Keypad `=`.
      KpEqual,
      F13,
      F14,
      F15,
      F16,
      F17,
      F18,
      F19,
      F20,
      F21, --  0x70
      F22,
      F23,
      F24,
      Execute,
      Help,
      Menu,
      Selectt,
      Stop,
      Again,
      Undo,
      Cut,
      Copy,
      Paste,
      Find,
      Mute,
      VolUp, --  0x80
      VolDown,
      --   Deprecated.
      LockingCapsLock,
      --   Deprecated.
      LockingNumLock,
      --   Deprecated.
      LockingScrollLock,
      --  / Keypad `,`, also used for the
      --  brazilian keypad period (.) key.
      KpComma,
      --   Used on AS/400 keyboard
      KpEqualSign,
      Intl1,
      Intl2,
      Intl3,
      Intl4,
      Intl5,
      Intl6,
      Intl7,
      Intl8,
      Intl9,
      Lang1, --  0x90
      Lang2,
      Lang3,
      Lang4,
      Lang5,
      Lang6,
      Lang7,
      Lang8,
      Lang9,
      AltErase,
      SysReq,
      Cancel,
      Clear,
      Prior,
      Returnn,
      Separator,
      Outt, --  0xA0
      Oper,
      ClearAgain,
      CrSel,
      ExSel,

      --  According to QMK, 0xA5-0xDF are not
      --  usable on modern keyboards

      --  Modifiers
      --   Left Control.
      LCtrl, --  = 0xE0,
      --  / Left Shift.
      LShift,
      --   Left Alt.
      LAlt,
      --   Left GUI (the Windows key).
      LGui,
      --   Right Control.
      RCtrl,
      --   Right Shift.
      RShift,
      --   Right Alt (or Alt Gr). 
      RAlt,
      --   Right GUI (the Windows key).
      RGui, --  0xE7

      --  Unofficial
      MediaPlayPause, --   0xE8,
      MediaStopCD,
      MediaPreviousSong,
      MediaNextSong,
      MediaEjectCD,
      MediaVolUp,
      MediaVolDown,
      MediaMute,
      MediaWWW, --  0xF0
      MediaBack,
      MediaForward,
      MediaStop,
      MediaFind,
      MediaScrollUp,
      MediaScrollDown,
      MediaEdit,
      MediaSleep,
      MediaCoffee,
      MediaRefresh,
      MediaCalc --  0xFB
     );

   type Action_Type is (Key, No_Op, Trans, Layer, Multiple_Actions);

   --   Should be a discriminated type
   type Action is record
      T : Action_Type; --  hould be the discriminant
      C : Key_Code_T;
      L : Natural;
   end record;

   function Kw (Code : Key_Code_T) return Action;
   function Lw (V : Natural) return Action;

   type Key_Modifiers is array (Natural range <>) of USB.Device.HID.Keyboard.Modifiers;

   type Key_Codes_T is array (Natural range <>) of Key_Code_T;

   subtype Ac is Action;
   type Layout is array (0 .. Num_Layers - 1, RowR, ColR) of Action;
   procedure Register_Events (L : Layout; Es : Events);
   procedure Tick (L : Layout);

   function Get_Key_Codes return Key_Codes_T;
   function Get_Modifiers return Key_Modifiers;
   procedure Init;
end Click;
