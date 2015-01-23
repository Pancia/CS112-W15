father(al, bud).
father(al, kelly).
mother(peggy, kelly).
mother(peggy, bud).
mother(martha, peggy).

grandma(X,Y) :- mother(X,Z), mother(Z,Y).

descendants(X,Y) :- father(X,Y); mother(X,Y);
                    descendants(X,Z), descendants(Z,Y).

parent(X,Y) :- mother(X,Y); father(X,Y).

siblings(X,Y) :- parent(P,X), parent(P,Y), X \= Y.

% Transition relation:
transition(q0,q1,a).
transition(q1,q2,b).
transition(q0,q3,a).
transition(q3,q3,a).

% Accepting states:
accepting(q2).
accepting(q3).

accepts(S, [])    :- accepting(S).
accepts(S, [F|R]) :- (transition(S, Q, F),
                     (accepting(Q); accepts(Q, R)));
                     accepting(S), F = [].
