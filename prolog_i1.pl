poda_par(L1,L2):-poda_par(L1,1,L2).
poda_par([],N,L2).
poda_par([h|T],N,L2):-
    \+ N mod 2 =:= 0,
    N is N+1,
    poda_par(T,N,L2).
poda_par([h|T],N,L2):-
    N mod 2 =:= 0,
    N is N+1,
    L2=[L2|h],
    poda_par(T,N,L2).
    
