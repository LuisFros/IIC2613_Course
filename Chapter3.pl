parent(X,Y):-father(X,Y),!.
parent(X,Y):-mother(X,Y).
father(john,paul).
mother(mary,paul).


likes(peter,Y):-
    friendly(Y).
likes(T,S):-
    student_of(S,T),!.
student_of(paul,peter).
student_of(maria,peter).
friendly(maria).