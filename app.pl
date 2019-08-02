test(R) :-
    go(s([0, 0, 1, 0, 1, 0, 2, 0, 0, 0, 1, 0], 10, [1, 2, 3, 4], 0), R).


    :- use_module(library(lists)).
    move(S, Snew) :-
    up(S, Snew).
    move(S, Snew) :-
    right(S, Snew).
    move(S, Snew) :-
    left(S, Snew).
    move(S, Snew) :-
    down(S, Snew).

    up(s([R00, R01, R02, R03, R10, R11, R12, R13, R20, R21, R22, R23], X, [Lcost, Rcost, Ucost, Dcost], Num), s(FinalList, NewX, [Lcost, Rcost, Ucost, Dcost], NewNum)) :-
    X>=Ucost,
    R00\=2,
    R01\=2,
    R02\=2,
    R03\=2,
    nth0(I,
         
         [ R00,
           R01,
           R02,
           R03,
           R10,
           R11,
           R12,
           R13,
           R20,
           R21,
           R22,
           R23
         ],
         2),
    AboveI is I-4,
    nth0(AboveI,
         
         [ R00,
           R01,
           R02,
           R03,
           R10,
           R11,
           R12,
           R13,
           R20,
           R21,
           R22,
           R23
         ],
         Above),
    Above\=1,
    substitute(2,
               
               [ R00,
                 R01,
                 R02,
                 R03,
                 R10,
                 R11,
                 R12,
                 R13,
                 R20,
                 R21,
                 R22,
                 R23
               ],
               0,
               NewList),
    replace(AboveI, NewList, 2, FinalList),
    NewX is X-Ucost,
    NewNum is Num+1.



    down(s([R00, R01, R02, R03, R10, R11, R12, R13, R20, R21, R22, R23], X, [Lcost, Rcost, Ucost, Dcost], Num), s(FinalList, NewX, [Lcost, Rcost, Ucost, Dcost], NewNum)) :-
    X>=Dcost,
    R20\=2,
    R21\=2,
    R22\=2,
    R23\=2,
    nth0(I,
         
         [ R00,
           R01,
           R02,
           R03,
           R10,
           R11,
           R12,
           R13,
           R20,
           R21,
           R22,
           R23
         ],
         2),
    BelowI is I+4,
    nth0(BelowI,
         
         [ R00,
           R01,
           R02,
           R03,
           R10,
           R11,
           R12,
           R13,
           R20,
           R21,
           R22,
           R23
         ],
         Below),
    Below\=1,
    substitute(2,
               
               [ R00,
                 R01,
                 R02,
                 R03,
                 R10,
                 R11,
                 R12,
                 R13,
                 R20,
                 R21,
                 R22,
                 R23
               ],
               0,
               NewList),
    replace(BelowI, NewList, 2, FinalList),
    NewX is X-Dcost,
    NewNum is Num+1.

    left(s([R00, R01, R02, R03, R10, R11, R12, R13, R20, R21, R22, R23], X, [Lcost, Rcost, Ucost, Dcost], Num), s(FinalList, NewX, [Lcost, Rcost, Ucost, Dcost], NewNum)) :-
    X>=Lcost,
    R00\=2,
    R10\=2,
    R20\=2,
    nth0(I,
         
         [ R00,
           R01,
           R02,
           R03,
           R10,
           R11,
           R12,
           R13,
           R20,
           R21,
           R22,
           R23
         ],
         2),
    LeftI is I-1,
    nth0(LeftI,
         
         [ R00,
           R01,
           R02,
           R03,
           R10,
           R11,
           R12,
           R13,
           R20,
           R21,
           R22,
           R23
         ],
         Left),
    Left\=1,
    substitute(2,
               
               [ R00,
                 R01,
                 R02,
                 R03,
                 R10,
                 R11,
                 R12,
                 R13,
                 R20,
                 R21,
                 R22,
                 R23
               ],
               0,
               NewList),
    replace(LeftI, NewList, 2, FinalList),
    NewX is X-Lcost,
    NewNum is Num+1.

    right(s([R00, R01, R02, R03, R10, R11, R12, R13, R20, R21, R22, R23], X, [Lcost, Rcost, Ucost, Dcost], Num), s(FinalList, NewX, [Lcost, Rcost, Ucost, Dcost], NewNum)) :-
    X>=Rcost,
    R03\=2,
    R13\=2,
    R23\=2,
    nth0(I,
         
         [ R00,
           R01,
           R02,
           R03,
           R10,
           R11,
           R12,
           R13,
           R20,
           R21,
           R22,
           R23
         ],
         2),
    RightI is I+1,
    nth0(RightI,
         
         [ R00,
           R01,
           R02,
           R03,
           R10,
           R11,
           R12,
           R13,
           R20,
           R21,
           R22,
           R23
         ],
         Right),
    Right\=1,
    substitute(2,
               
               [ R00,
                 R01,
                 R02,
                 R03,
                 R10,
                 R11,
                 R12,
                 R13,
                 R20,
                 R21,
                 R22,
                 R23
               ],
               0,
               NewList),
    replace(RightI, NewList, 2, FinalList),
    NewX is X-Rcost,
    NewNum is Num+1.



    replace(I, L, E, K) :-
    nth0(I, L, _, R),
    nth0(I, K, E, R).

    substitute(_, [], _, []) :-
    !.
    substitute(X, [X|T], Y, [Y|T1]) :-
    substitute(X, T, Y, T1),
    !.

    substitute(X, [H|T], Y, [H|T1]) :-
    substitute(X, T, Y, T1).

      %query of user and takes start state and next state
go(Start, R) :-
    bagof(X, path([[Start, null]], [], X), Children),
    distinct(Children, X),
    length(X, M),
    R is M-1.

path([], _, _) :-
    !.

path([[Goal, Parent]|_], Closed, Goal) :-
    nl,
    printsolution([Goal, Parent], Closed).

    %main predicate that takes open list, closed list and goal state
path(Open, Closed, Goal) :-
    removeFromOpen(Open, [State, Parent], RestOfOpen),
    getchildren(State, Open, Closed, Children),
    addListToOpen(Children, RestOfOpen, NewOpen),
    path(NewOpen, [[State, Parent]|Closed], Goal).

    %gets Children of State that aren't in Open or Close
getchildren(State, Open, Closed, Children) :-
    bagof(X, moves(State, Open, Closed, X), Children),
    !.
    getchildren(_, _, _, []).

    %adds children to open list (without head child) to form new open list
    %here it is like append i.e.Breadth First
addListToOpen(Children, [], Children).
    addListToOpen(Children, [H|Open], [H|NewOpen]) :-
    addListToOpen(Children, Open, NewOpen).

    %gets head of open list to get its children later
removeFromOpen([State|RestOpen], State, RestOpen).

    %gets next state given the current state
moves(State, Open, Closed, [Next, State]) :-
    move(State, Next),
    \+ member([Next, _], Open),
    \+ member([Next, _], Closed).

    %prints the path from start state to goal state
printsolution([State, null], _) :-
    write(State),
    nl.
    printsolution([State, Parent], Closed) :-
    member([Parent, GrandParent], Closed),
    printsolution([Parent, GrandParent], Closed),
    write(State),
    nl.



    member1(s(X, _, _, _), [s(H, _, _, _)|_]) :-
    X==H,
    !.
    member1(X, [_|T]) :-
    member1(X, T).

    distinct([], []).
    distinct([H|T], C) :-
    member1(H, T),
    !,
    distinct(T, C).
    distinct([H|T], [H|C]) :-
    distinct(T, C).