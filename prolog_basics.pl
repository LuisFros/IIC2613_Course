raton(jerry).
perro(tribilin).
gato(felix).
gato(tom).
raton(ratatouie).

animal(X) :- perro(X).
animal(X) :- gato(X).
animal(X) :- raton(X).

% raton(Y)^gato(X)-->puede_comer(X,Y)
puede_comer(X,Y) :- raton(Y),gato(X).

pertenece(X,[X|_]).
pertenece(X,[_|L]):- pertenece(X,L).

members(El, [H|T]) :-
    members_(T, El, H).
members_(_, El, El).
members_([H|T], El, _) :- 
    members_(T, El, H).




% enesimo(X, 1, [M,X,Y,Q,T]).
% enesimo(X, 2, [X,M,X]).
% enesimo(X, N, [L|]).




enesimo(X, 0, [X|_]).

enesimo(X,N,L):-
    L=[_|Lp],
    Np is N-1.
    enesimo(X,Np,Lp),


# my_last(X,Y):- Y=[Head|Tail],
# my_last(X,[Head|Tail]):-
#     my_last(X,[Tail]).


my_last(X,Y):- Y=[A|B], A=X, B=[].
my_last(X,Y):-Y=[C|D], my_last(X,D).

last_but(X,L):- L=[Element|[Last]],X=L.
last_but(X,L):- L=[Head|Tail] ,last_but(X,[Tail]).



kth_element(X,L,K):-
    K=0,L=[X|_].
kth_element(X,L,K):-
    L=[_|Tail],
    kth_element(X,Tail,Kp),
    K is Kp+1.

element_at(X,[X|_],1).
element_at(X,[_|L],K) :- K > 1, K1 is K - 1, element_at(X,L,K1).

number_elements(L,N):- L=[],N=0.

number_elements(L,N):- L=[Head|Tail],number_elements(Tail,Np),N is Np+1.


lenght_tail(L,N):-lenght_tail(L,0,N).
lenght_tail([],N,N).
lenght_tail(L,N0,N):L=[H|],N1 is N0+1,lenght_tail(Tail,N1,N).

length_acc(L,N):-length_acc(L,0,N).
length_acc([],N,N).
length_acc(L,N0,N):L=[H|T],-N1 is N0+1,length_acc(T,N1,N).

length_acc2(L,N):-length_acc2(L,0,N).
length_acc2([],N,N).
length_acc2([H|T],N0,N):-N1 is N0+1,length_acc2(T,N1,N).

reverse_list(X,Y):-reverse_list(X,[],Y).
reverse_list([],Y,Y).
reverse_list([H|T],Y0,Y):-reverse_list(T,[H|Y0],Y).
