program Hello;
uses crt;

type str = string[1];
     arr = array[1..20, 1..60] of char;

var x, y:integer;
    carr:arr;
    c:char;

Procedure start;
{comment here}
begin write (' Press enter to begin. ');
readln;
end;

Function Valid (var choice:char): boolean;
begin 
    valid:= false;
    case choice of 
        '1':valid:= true;
        '2': valid:= true;
        '3': valid:= true;
        '4': valid:= true;
        '5': valid:= true;
        '6': valid:= true;
    end;
end;

begin
    for y:=1 to 3 do
    begin
        writeln (y);
    end;
    
    repeat
        writeln(y);
        y := y + 1;
    until y > 5;
    writeln ('Hello World');
end.

