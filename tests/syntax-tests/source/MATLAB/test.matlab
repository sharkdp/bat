function zz=sample(aa)
%%%%%%%%%%%%%%%%%%
% some comments
%%%%%%%%%%%%%%%%%%

x = 'a string';    % some 'ticks' in a comment
y = 'a string with ''interal'' quotes';

for i=1:20
  disp(i);
end

a = rand(30);
b = rand(30);

c = a .* b ./ a \ ... comment at end of line and continuation
    (b .* a + b - a);

c = a' * b';  % note: these ticks are for transpose, not quotes.

disp('a comment symbol, %, in a string');

!echo abc % this isn't a comment - it's passed to system command

function y=myfunc(x)
y = exp(x);

%{
  a block comment
%}

function no_arg_func
fprintf('%s\n', 'function with no args')
end
