rav([H1], [H2]):-
	H1 = H2, !.
rav([], []):- !.
rav([H1, E1 | T1],[H2, E2 | T2]):-
	H1 = E2, H2 = E1, rav(T1, T2).

srav([], []):- !.
srav([E1], [E2]):-
	E1 = E2, !.
srav([E1, E2], [E3, E4]):-
	E1 = E3, E2 = E4, !.	
srav([H1 | T1], [H2 | T2]):-
	H1 = H2, rav(T1, T2).
	