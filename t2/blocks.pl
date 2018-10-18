:- discontiguous holds/2,is_negative_effect/2,is_positive_effect/2,poss/2,block/1.
-include('astar').

%% Calculo de situacion

% holds(F,s): si f es cierto en la situacion s

%% Acciones
% poss (A,s) Verdader si A es aplicable en S.
% Efectos
  %Positivos (A,F)
  %Negativos(A,F)

%% move: mover X desde Y hasta Z, siendo Y y Z los nid de lo que está abajo del bloque.

%% Situacion: Se define como una composición de acciones. Ej:
%% S = do(move(a,b,table), S0) -> do -> referencia al estado que queda luego de que se ejecuta cierta accion.

%%%%% First, the a blocksworld

%% Object Declaration (problem-specific)
block(B) :- member(B,[a,b,c,d]).

%% Initial Situation (problem-specific)
%% Asignacion inincial, holds dice lo que es real para una situacionn.
%% F sera cierto si se define todo lo de la derecha.
%% member: predicado de prolog. F sera cierto si pertenece al segundo argumento.

holds(F,s0) :- member(F,[on(a,b),on(b,c),on(c,table), on(d,table)]).


%% Blocks World Preconditions (domain-specific)
%% Estamos viendo si en la situacion S podemos aplicar move(x,y,z).
%%
poss(move(X,Y,Z),S) :-
    block(X), % X debe ser un bloque.
    holds(on(X,Y),S), % En S el bloque X debe estar sobre Y.
    (Z=table; % Z es la mesa, pero lo que siempre está disponible
      block(Z),\+ holds(on(_,Z),S)), %Z es un bloque. El _ es cualquier cosa "para todo x". Y no hay nada arriba de Z.
      % Lo anterior de holds es porque, pr ejemplo move() debe permitir mover cosas a la mesa siempre.
    X\=Z,Y\=Z, % Diferenciar variables.
    \+ holds(on(_,X),S). % No hay nada sobre X.

% Si hacemos move(x,y,z) entonces on(x,z) se producira como efecto positivo.
% Por otro lado, el efecto negativo es on(x,y).

% Efecto positivo: Lo que se hace positivo al hacer una acción.
% Efectivo negativo: Lo que se hace negativo al hacer una acción.

%% Blocks World Effects (domain-specific)
is_negative_effect(move(X,Y,_),on(X,Y)).
is_positive_effect(move(X,_,Z),on(X,Z)).


%%%%% Situation Calculus Successor State Axiom a la Reiter (domain-independent)

%% Un F es cierto en una situaacion, despues de haber aplicado el do, si la situacion anteior es:
% Antes de haber aplicado la accion se cumplía holds(f,s) y la negación de is_negative_effect.
holds(F,do(A,S)) :-
    holds(F,S),
    \+ is_negative_effect(A,F).

%% Si despues de hber aplicado la opción, se hace verdadero.
holds(F,do(A,_)) :-
    is_positive_effect(A,F).

%%%%% Legal Situations are those produced by executing
%%%%% generates situations in a breadth-first manner
%% Los movimientos que son legales.
legal(s0).
legal(do(A,S)) :-
    legal(S),
    poss(A,S).

% If you want to generate a plan use a query like
% legal(S),holds(on(b,a),S).


%% Heuristica: UTilizaremos

%% h: Cantidad de bloques mal posicionados.
goal_condition([on(b,a), on(d,c), on(c,table)]).

% La multiplicacion sirve para hacer AA*.
astar_heuristic(State,N) :- astar_heuristic0(State,M), N is 4 * M.

astar_heuristic0(_,0).

% astar_heuristic1(State, N):-
%   %% Por aca se comunica con el archivo.
%   goal_condition(GState),
%   findall(X, (member(on(Block, Gpos), GState), member(on(Block, Pos), State), Gpos \=Pos),List).
%   length(List,N).
  %% finall(X, Condicion ,lista) : todos los X que cumplen con cierta condicion en una lista.