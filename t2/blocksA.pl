%%%%% First, the a blocksworld
:- discontiguous holds/2,is_negative_effect/2,is_positive_effect/2,poss/2,block/1.
% :- consult('./astar.pl').
:- include('astar.pl').
% :- consult(astar).


%% Object Declaration (problem-specific)
block(B) :- member(B,[a,b,c,d]).

%% Initial Situation (problem-specific)
holds(F,s0) :- member(F,[on(a,b),on(b,c),on(c,table), on(d,table)]).


%% Blocks World Preconditions (domain-specific)
poss(move(X,Y,Z),S) :-
	block(X), % X debe ser un bloque, se puede obviar en este caso
    holds(on(X,Y),S), % En S el bloque X debe estar sobre Y
    (Z=table; % Z es la mesa, por lo que siempre esta disponible. OR
    	block(Z),\+ holds(on(_,Z),S)), %Z es un bloque, y no hay nada arriba de Z
    X\=Z,Y\=Z,  %Diferenciar variables
    \+ holds(on(_,X),S). %No hay nada sobre X.

%% Blocks World Effects (domain-specific)
is_negative_effect(move(X,Y,_),on(X,Y)).
is_positive_effect(move(X,_,Z),on(X,Z)).


%%%%% Situation Calculus Successor State Axiom a la Reiter (domain-independent)
holds(F,do(A,S)) :-
    holds(F,S),
    \+ is_negative_effect(A,F).

holds(F,do(A,_)) :-
    is_positive_effect(A,F).

%%%%% Legal Situations are those produced by executing
%%%%% generates situations in a breadth-first manner

legal(s0).
legal(do(A,S)) :-
    legal(S),
    poss(A,S).


% If you want to generate a plan use a query like
% legal(S),holds(on(b,a),S).

goal_condition([on(b,a), on(d,c), on(c,table)]).

%% wA* entonces multiplicamos por un w
astar_heuristic(State,N) :- astar_heuristic1(State,M), N is 2*M.

astar_heuristic0(_,0).

%N es el valor de la heuristica, en este caso la cantidad de bloques mal posicionados.
astar_heuristic1(State,N) :-
    goal_condition(GState),
    findall(X,(member(on(Block,Gpos),GState), member(on(Block,Pos),State), Gpos\=Pos),List), %La lista van a ser todos los X que cumple Condicion
    length(List,N).

