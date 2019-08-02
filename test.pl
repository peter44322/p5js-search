replace(I, L, E, K) :-
    nth0(I, L, _, R),
    nth0(I, K, E, R).
