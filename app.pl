test :-
    go(s(['.', '.', "#", '.', "#", '.', "*", '.', '.', '.', "#", '.'], 2, [1, 2, 3, 4], 1),
       s(_, 0, _, _)).

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
    not(R00="*"),
    not(R01="*"),
    not(R02="*"),
    not(R03="*"),
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
         "*"),
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
    not(Above="#"),
    substitute("*",
               
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
               ".",
               NewList),
    replace(AboveI, NewList, "*", FinalList),
    NewX is X-Ucost,
    NewNum is Num+1.
  


down(s([R00, R01, R02, R03, R10, R11, R12, R13, R20, R21, R22, R23], X, [Lcost, Rcost, Ucost, Dcost], Num), s(FinalList, NewX, [Lcost, Rcost, Ucost, Dcost], NewNum)) :-
    X>=Dcost,
    not(R20="*"),
    not(R21="*"),
    not(R22="*"),
    not(R23="*"),
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
         "*"),
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
    not(Below="#"),
    substitute("*",
               
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
               ".",
               NewList),
    replace(BelowI, NewList, "*", FinalList),
    NewX is X-Dcost,
    NewNum is Num+1.

left(s([R00, R01, R02, R03, R10, R11, R12, R13, R20, R21, R22, R23], X, [Lcost, Rcost, Ucost, Dcost], Num), s(FinalList, NewX, [Lcost, Rcost, Ucost, Dcost], NewNum)) :-
    X>=Lcost,
    not(R00="*"),
    not(R10="*"),
    not(R20="*"),
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
         "*"),
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
    not(Left="#"),
    substitute("*",
               
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
               ".",
               NewList),
    replace(LeftI, NewList, "*", FinalList),
    NewX is X-Lcost,
    NewNum is Num+1.

right(s([R00, R01, R02, R03, R10, R11, R12, R13, R20, R21, R22, R23], X, [Lcost, Rcost, Ucost, Dcost], Num), s(FinalList, NewX, [Lcost, Rcost, Ucost, Dcost], NewNum)) :-
    X>=Rcost,
    not(R03="*"),
    not(R13="*"),
    not(R23="*"),
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
         "*"),
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
    not(Right="#"),
    substitute("*",
               
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
               ".",
               NewList),
    replace(RightI, NewList, "*", FinalList),
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
go(Start, Goal) :-
    path([[Start, null]], [], Goal).

%main predicate that takes open list, closed list and goal state
path([], _, _) :-
    write('No solution'),
    nl,
    !.
path([[Goal, Parent]|_], Closed, Goal) :-
    write('Goal solution is found'),
    nl,
    printsolution([Goal, Parent], Closed),
    !.
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
