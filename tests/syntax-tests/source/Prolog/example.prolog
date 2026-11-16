% Facts, variables, rules, arithmetic, lists, and quoted atoms.
parent(alice, bob).
parent(bob, carol).
label('hello world').
answer(42).
ratio(3.14).

/* A comment spanning
   multiple lines. */
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
positive(X) :- X > 0, !.
increment(X, Y) :- Y is X + 1.
print_name(Name) :- write("Name: "), writeln(Name).
empty([]).
head([H|_], H).
word --> [hello], [world].
