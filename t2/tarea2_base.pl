:- discontiguous holds/2,is_negative_effect/2,is_positive_effect/2,poss/2.

:-['astar.pl'].

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

% La grua G, levanta a un container C, 
% que se encuentra sobre Sup (container; paleta) en el lugar L
poss(levantar(G,C,Sup,L),S) :-
    holds(en(G,L),S),holds(en(C,L),S), % Mismo lugar
    holds(sobre(C,Sup),S), % El contenedor tiene que estar sobre Sup
    % \+paleta(C), %No es una paleta
    \+holds(levantando(G,_),S), % La grua no esta levantando ningun container
    \+holds(sobre(_,C),S),% no hay nada sobre C!
    container(C),grua(G),superficie(Sup),lugar(L),
    G\=C,C\=Sup,Sup\=L.  

%%% Accion levantar: efectos positivos

is_positive_effect(levantar(G,C,_,_),levantando(G,C)).
%% COMPLETE

%%% Accion levantar: efectos negativos

is_negative_effect(levantar(_,C,Sup,_),sobre(C,Sup)).
is_negative_effect(levantar(_,C,_,L),en(C,L)). % Contenedor deja de estar en L
%% COMPLETE


%%% Accion soltar: precondicion

poss(soltar(G,C,Sup,L),S) :-
    holds(en(G,L),S),holds(en(Sup,L),S), % Mismo lugar
    holds(levantando(G,C),S), % Si la grua esta levantando a C
    \+ holds(sobre(Sup,C),S),
    \+ holds(sobre(_,Sup),S), % se puede soltar, si no hay nada sobre Sup    
    container(C),grua(G),superficie(Sup),lugar(L),
    G\=C,C\=Sup,Sup\=L.


%%% Accion soltar: efectos positivos
is_positive_effect(soltar(_,C,Sup,_),sobre(C,Sup)). % Container esta sobre Sup
is_positive_effect(soltar(_,C,_,L),en(C,L)). % Container esta en lugar L
%% COMPLETE

%%% Accion soltar: efectos negativos
is_negative_effect((soltar(G,C,_,_),levantando(G,C))). % Grua no levanta mas

%% COMPLETE


%%% Accion cargar: precondicion

poss(cargar(G,Container,Camion,L),S) :-
    %% COMPLETE
    
    holds(levantando(G,Container),S), % Si la grua esta levantando el container
    holds(en(Camion,L),S), % Camion en L
    holds(en(G,L),S), % Grua en L
    container(Container),grua(G),camion(Camion),lugar(L),
    G\=Container,Container\=Camion,Camion\=L.
%%% Accion soltar: cargar positivos
is_positive_effect(cargar(_,Container,Camion,_),dentro(Container,Camion)).
%% COMPLETE


%%% Accion soltar: cargar negativos
is_negative_effect(cargar(G,Container,_,_),levantando(G,Container)).
%% COMPLETE



%%% Accion descargar: precondicion

poss(descargar(G,Container,Camion,L),S) :-
    %% COMPLETE
    holds(dentro(Container,Camion),S),
    holds(en(Camion,L),S), % Camion en L
    holds(en(G,L),S), % Grua en L
    \+holds(levantando(G,_),S),% Si la grua no esta levantando nada
    container(Container),grua(G),camion(Camion),lugar(L),
    G\=Container,Container\=Camion,Camion\=L.

%%% Accion descargar: cargar positivos
is_positive_effect(descargar(G,Container,_,_),levantando(G,Container)).
%% COMPLETE

%%% Accion descargar: cargar negativos
is_negative_effect(descargar(_,Container,Camion,_),dentro(Container,Camion)).
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

astar_heuristic(State,N) :- (astar_heuristic2(State,N);astar_heuristic0(State,N)).
    % format("H is ~q \n",[N]).

astar_heuristic0(_,0).

% el_sobre(X,Y,S):-
%     holds(sobre(X,Y),S).
% el_sobre(X,Y,S):-
%     holds(sobre(Z,Y),S),
%     el_sobre(Z,Y,S).

% hay_sobre([],S,N):-N is 0.
%     % \+holds(sobre(_,Contenedor),S).
% hay_sobre(Contenedor,S,N):-
%     holds(sobre(_,Contenedor),S),
%     findall(X,(member(sobre(X,Contenedor),S)),Lista),
%     Lista=[H|T],
%     el_sobre(H,Contenedor,S),
%     % (hay_sobre(Lista,S,N);
%     % Lista=[Head|T],hay_sobre(Head,S,N)),
%     N is N +1.    


function_s(Superficie,M,S):-
    function_s(Superficie,0,M,S).

function_s(Superficie,M,M,S):-
    \+holds(sobre(_,Superficie),S).
function_s(Superficie,M0,N,S):-
    % format('function 3 Superficie ~q \n',[Superficie]),
    % holds(sobre(_,Superficie),S),
    findall(X,(holds(sobre(X,Superficie),S)),Lista),
    % findall(X,(holds(sobre(_,X),S)),Lista2),
    % findall(X,(!),ListaEstados),
    % \+ list_empty(Lista),
    length(Lista,Len),
    Len >0,
    % atomic_list_concat(Lista, ',', Atom), atom_string(Atom, String),
    % format('List is ~w \n',[String]),
    % atomic_list_concat(Lista2, ',', Atom2), atom_string(Atom, String2),
    % format('Lista2  ~q \n',[String2]),
    M1 is M0 + 1,
    Lista=[Head|_],
    % format('function 3 Head ~q \n',[Head]),
    function_s(Head,M1,N,S).


% dosomething(List,S,R):-
%     dosomething(List,0,S,R).
dosomething(List,S,R):-
    dosomething(List,_,S,R).
dosomething([],M,_,M).
dosomething([H|T],M,S,R) :- function_s(H,M,S),dosomething(T,M,S,R).



astar_heuristic1(State,N) :-
    goal_condition(GState),
    %numero de containers es posiciones errones
    findall(C,(container(C),member(en(C,Gpos),GState), member(en(C,Pos),State), Gpos\=Pos),List), %La lista van a ser todos los X que cumple Condicion
    findall(G,(grua(G),member(levantando(G,Contenedor),State),member(en(G,L),State),member(X,List),member(en(X,L),State),Contenedor\=X),GruasUsadas),
    % Numero de gruas que estan levantando un elemento distinto del objetivo
    length(GruasUsadas,Gruas),
    findall(Camion,(camion(Camion),member(en(Camion,L),State),member(X,List),member(en(X,L),State),member(en(X,ObjL),GState),ObjL\=L),ElementosCamion),
    % Numero de camiones que estan en un cargo distinto al de los contenedroes objetivos,
    length(ElementosCamion,Camiones),

    % findall(X,(container(C),member(en(C,Gpos),GState), member(en(C,Pos),State), Gpos\=Pos),List), %La lista van a ser todos los X que cumple Condicion
    length(List,M), % M es el numero de en() que no estan en orden deseado
    M>0, % Si no hay ninguno, se usa heuristica 0
    % findall(X,(member(sobre(_,X),State)),Lista2),
    % atomic_list_concat(Lista2, ',', Atom), atom_string(Atom, String2),
    % format('Lista antes de dosomething  ~q \n',[String2]),

    dosomething(List,State,R), 
    % numero de containers arriba de los que no estan en objetivo

    % findall(X,(container(X),member(sobre(X,C)),container(C),member(en(C,Gpos),GState), member(en(C,Pos),State), Gpos\=Pos),List)
    % findall(Y,(member(en(Block,Pos),State)),List2),
    % findall(Y,(holds(sobre(_,Gstate))))
    % format('M is ~q \n',[M]),
    % format('R is ~q \n',[R]),
    % format('Gruas is ~q \n',[Gruas]),
    % format('Camiones is ~q \n',[Camiones]),

    N is 3*(M+R)+1*Gruas+1*Camiones.  % por lo menos 2 acciones para cada cambio de cargo

    % length(List2,Q),

%%astar_heuristic1(State,N) :-

astar_heuristic2(State,N) :-
    goal_condition(GState),
    %numero de containers es posiciones errones
    findall(C,(container(C),member(en(C,Gpos),GState), member(en(C,Pos),State), Gpos\=Pos),List), %La lista van a ser todos los X que cumple Condicion
    findall(G,(grua(G),member(levantando(G,Contenedor),State),member(en(G,L),State),member(X,List),member(en(X,L),State),Contenedor\=X),GruasUsadas),
    % Numero de gruas que estan levantando un elemento distinto del objetivo
    length(GruasUsadas,Gruas),
    findall(Camion,(camion(Camion),member(en(Camion,L),State),member(X,List),member(en(X,L),State),member(en(X,ObjL),GState),ObjL\=L),ElementosCamion),
    % Numero de camiones que estan en un cargo distinto al de los contenedroes objetivos,
    length(ElementosCamion,Camiones),

    % findall(X,(container(C),member(en(C,Gpos),GState), member(en(C,Pos),State), Gpos\=Pos),List), %La lista van a ser todos los X que cumple Condicion
    length(List,M), % M es el numero de en() que no estan en orden deseado
    M>0, % Si no hay ninguno, se usa heuristica 0
    % findall(X,(member(sobre(_,X),State)),Lista2),
    % atomic_list_concat(Lista2, ',', Atom), atom_string(Atom, String2),
    % format('Lista antes de dosomething  ~q \n',[String2]),

    dosomething(List,State,R), 
    % numero de containers arriba de los que no estan en objetivo

    % findall(X,(container(X),member(sobre(X,C)),container(C),member(en(C,Gpos),GState), member(en(C,Pos),State), Gpos\=Pos),List)
    % findall(Y,(member(en(Block,Pos),State)),List2),
    % findall(Y,(holds(sobre(_,Gstate))))
    % format('M is ~q \n',[M]),
    % format('R is ~q \n',[R]),
    % format('Gruas is ~q \n',[Gruas]),
    % format('Camiones is ~q \n',[Camiones]),
    N is 4*(M)+4*R+2*Gruas+1*Camiones. % por lo menos 2 acciones para cada cambio de cargo

    

%%astar_heuristic3(State,N) :-

%%astar_heuristic4(State,N) :-

