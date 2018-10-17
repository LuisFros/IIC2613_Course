:- discontiguous holds/2,is_negative_effect/2,is_positive_effect/2,poss/2.

%%%%% First, the a blocksworld

%% Object Declaration (problem-specific)

container(C) :- member(C,[c1,c2,c3,c4,c5,c6]).
paleta(P) :- member(P,[p11,p21,p12,p22,p13]).
superficie(S) :- container(S).
superficie(S) :- paleta(S).
lugar(L) :- member(L,[cargo1,cargo2,cargo3]).
conectada(C1,C2) :- member([X1,X2],[[cargo1,cargo2],
                                    [cargo2,cargo3]]),
                    (C1=X1,C2=X2; C1=X2,C2=X1).

grua(G) :- member(G,[g1,g2,g3]).

camion(Cam) :- member(Cam,[cam1]).

%% Initial Situation (problem-specific)
holds(F,s0) :- member(F,[en(cam1,cargo1),

                         en(g1,cargo1),en(g2,cargo2),en(g3,cargo3),
                         disponible(g1),disponible(g2),

                         en(p11,cargo1),en(p21,cargo1),
                         en(c1,cargo1),en(c2,cargo1),
                         sobre(c1,p11),sobre(c2,c1),
                         despejada(c2),despejada(p21),

                         en(p12,cargo2),en(p22,cargo2),
                         en(c3,cargo2),en(c4,cargo2),en(c5,cargo2),
                         sobre(c3,p12),sobre(c4,c3),sobre(c5,c4),
                         sobre(c6,p22),
                         despejada(c5),
                         despejada(c6),

                         en(p13,cargo3),despejada(p13)]).

%%% Accion manejar: precondicion

poss(manejar(Cam,L1,L2),S) :-
    camion(Cam),conectada(L1,L2),
    holds(en(Cam,L1),S).

%%% Accion manejar: efecto positivo

is_positive_effect(manejar(Cam,_,L2),en(Cam,L2)).

%%% Accion manejar: efecto negativo
is_negative_effect(manejar(Cam,L1,_),en(Cam,L1)).

%%% Accion levantar: precondicion

poss(levantar(G,C,Sup,L),S) :-
    %% COMPLETE
    grua(G),superficie(Sup),lugar(L).


%%% Accion levantar: efectos positivos

is_positive_effect(levantar(G,C,_,_),levantando(G,C)).
%% COMPLETE

%%% Accion levantar: efectos negativos

is_negative_effect(levantar(_,C,Sup,_),sobre(C,Sup)).
%% COMPLETE


%%% Accion soltar: precondicion

poss(soltar(G,C,Sup,L),S) :-
    %% COMPLETE
    grua(G),superficie(Sup),lugar(L).


%%% Accion soltar: efectos positivos

%% COMPLETE

%%% Accion soltar: efectos negativos

%% COMPLETE


%%% Accion cargar: precondicion

poss(cargar(G,Container,Camion,L),S) :-
    %% COMPLETE
    grua(G),camion(Camion),lugar(L).


%%% Accion soltar: cargar positivos

%% COMPLETE


%%% Accion soltar: cargar negativos

%% COMPLETE



%%% Accion descargar: precondicion

poss(descargar(G,Container,Camion,L),S) :-
%% COMPLETE
    grua(G),camion(Camion),superficie(Sup),lugar(L).


%%% Accion descargar: cargar positivos
%% COMPLETE

%%% Accion descargar: cargar negativos
%% COMPLETE

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

%%

goal_condition([en(c1,cargo2),en(c5,cargo1)]).

astar_heuristic(State,N) :- astar_heuristic0(State,N).

astar_heuristic0(_,0).

%%astar_heuristic1(State,N) :-

%%astar_heuristic2(State,N) :-

%%astar_heuristic3(State,N) :-

%%astar_heuristic4(State,N) :-
