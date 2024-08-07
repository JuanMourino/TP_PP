%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%% Auxiliares:
%%%%%%%%%%%%%%%

%%fila(+Tam, ?F)
fila(0, []).
fila(Tam, [_ | F1]) :- Tam > 0, N1 is Tam-1, fila(N1, F1).

%%ocupadaEnFila(+N, +T)
ocupadaEnFila(0, [ocupada | _]).
ocupadaEnFila(N, [_ | Xs]) :- N > 0, N1 is N-1, ocupadaEnFila(N1, Xs).

%%desde(+X, -Y)
desde(X, X).
desde(X, Y) :- N is X+1, desde(N, Y).

%%generarDimensiones(+F, +C, -F1, -C1), paso generate. El minimo es F+C, porque para poder ocupar esa posicion, debe haber al menos tantas filas y columnas como
generarDimensiones(F, C, F1, C1) :- Min is F+C, desde(Min, S), between(F, S, F1), C1 is S-F1.

%%dimensionesValidas(+F, +C, +F1, +C1), paso test. F1 >= F+1, porque empezamos a contar en 0 las posiciones y en 1 el tamaño, analogo para las columnas
dimensionesValidas(F, C, F1, C1) :- F1 >= F+1, C1 >= C+1.

%%crearDimensiones(+F, +C, -F1, -C1) Usa generate & test, genera un par de dimensiones posibles y despues verifica que sean validas
crearDimensiones(F, C, F1, C1) :- generarDimensiones(F, C, F1, C1), dimensionesValidas(F, C, F1, C1).

%%sonVecinos(+Pos, ?Pos1)
sonVecinos(pos(X, Y), pos(X1, Y1)) :- X1 is X-1, Y1 is Y.
sonVecinos(pos(X, Y), pos(X1, Y1)) :- X1 is X+1, Y1 is Y.
sonVecinos(pos(X, Y), pos(X1, Y1)) :- X1 is X, Y1 is Y-1.
sonVecinos(pos(X, Y), pos(X1, Y1)) :- X1 is X, Y1 is Y+1.

%posicionValida(+Pos, +T)
%Solo vale porque es un tablero, si no fueran todas las filas iguales, habría que ir a la fila X para comprobar que Y es válido
posicionValida(pos(X, Y), [Z | Zs]) :- length(Z, C), length([Z | Zs], F), 0 =< X, 0 =< Y, X < F, Y < C.

%%posLibreFila(+Pos, +Tablero)
posLibreFila(0, [Z | _]) :- var(Z).
posLibreFila(0, [Z | _]) :- nonvar(Z), Z \= ocupada.
posLibreFila(Y, [_ | Zs]) :- Y > 0, N is Y-1, posLibreFila(N, Zs).

%%posLibreTablero(+Pos, +Tablero)
posLibreTablero(pos(0, Y), [Z | _]) :- posLibreFila(Y, Z).
posLibreTablero(pos(X, Y), [_ | Zs]) :- X > 0, N is X-1, posLibreTablero(pos(N, Y), Zs).

%%destinosValidos(+Inicio, +Fin, +T)
destinosValidos(Inicio, Fin, T) :- posLibreTablero(Fin, T), posLibreTablero(Inicio, T).

%%cantFilas(+T, ?F)
cantFilas(T, F) :- length(T, F).

%%cantColumnas(+T, ?C)
cantColumnas([X | _], C) :- length(X, C).
%%Solo vale porque es un tablero

%%distancia(+Inicio, +Fin, ?D)
distancia(pos(X, Y), pos(X1, Y1), D) :- D is abs(X-X1) + abs(Y-Y1).

%%caminoAux(+Inicio, +Fin, +Tablero, ?Camino, +Visitados)
%%Usa generate & test, genera un posible vecino libre y despues verifica que sea posible armar un camino agregando esa posicion
caminoAux(Pos, Pos, T, [Pos], _) :- posLibreTablero(Pos, T).
caminoAux(Inicio, Fin, T, [Inicio | Xs], Visitados) :- Inicio \= Fin, vecinoLibre(Inicio, T, V), not(member(V, Visitados)), caminoAux(V, Fin, T, Xs, [V | Visitados]).

%% todasPosLibres(+T, +C)
todasPosLibres(_, []).
todasPosLibres(T, [X | Xs]) :- posLibreTablero(X, T), todasPosLibres(T, Xs).

%%sinRepetidos(+Xs)
sinRepetidos([]).
sinRepetidos([X | Xs]) :- sinRepetidos(Xs), not(member(X, Xs)).

%%tamanoCreciente(+L)
tamanoCreciente([]).
tamanoCreciente([_]).
tamanoCreciente([X, Y | Xs]) :- length(X, Lenx), length(Y, Leny), Lenx =< Leny, tamanoCreciente([Y | Xs]).

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(0,C,[]) :- C >= 0.
tablero(Filas, Columnas, [F | T1]) :- Filas > 0, Columnas >= 0, fila(Columnas, F), Filas1 is Filas-1, tablero(Filas1, Columnas, T1).

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
%% Usa generate & test con crearDimensiones
ocupar(pos(0, C), T) :- nonvar(T), T = [X | _], ocupadaEnFila(C, X).
ocupar(pos(F, C), T) :- F > 0, nonvar(T), T = [_ | Xs], F1 is F-1, ocupar(pos(F1, C), Xs).
ocupar(pos(F, C), T) :- var(T), crearDimensiones(F, C, F1, C1), tablero(F1, C1, T), ocupar(pos(F, C), T).

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
%%Usa generate & test, genera una posible posicion con sonVecinos y prueba que sea valida con posicionValida
vecino(pos(X, Y), T, pos(X1, Y1)) :- posicionValida(pos(X, Y), T), sonVecinos(pos(X, Y), pos(X1, Y1)), posicionValida(pos(X1, Y1), T).

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
%% Usa generate & test, porque genera un vecino posible con vecino y prueba que sea solucion valida con posLibreTablero
vecinoLibre(Pos, T, V) :- vecino(Pos, T, V), posLibreTablero(V, T).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas
%%caminoAux usa generate & test
camino(Inicio, Fin, T, Camino) :-  destinosValidos(Inicio, Fin, T), caminoAux(Inicio, Fin, T, Camino, [Inicio]).

%% 5 1 Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace
%%
%% Fin debe estar instanciado para esta implementación porque al llamar posLibreTablero, donde Pos = Fin, es necesario que Fin esté instanciado,
%% caso contrario hay un error porque al preguntar si X>0 en posLibreTablero, el cual debe estar instanciado para que no halla un error. 
%% Pasa lo mismo cuando dentro de filaLibre donde, al usar "is", como Y (que viene de Pos) está del lado derecho, debe estar instanciada
%%
%% En el caso de Camino, puede estar instanciado, en el caso de que Inicio = Fin, unifica con el primer caso de caminoAux (En cuyo caso es un camino válido)
%% o no unifica con ningún caso, entonces no era un camino válido
%% Si Inicio \= Fin, solo puede unificar con el segundo caso de caminoAux, luego al hacer la llamada caminoAux(V, Fin, T, Xs, Zs), o nos encontramos con el caso anterior
%% o volvemos a entrar al caso recursivo, entonces si este vecino era el siguiente en Camino, entonces unifica y sigue ejecutándose, sino, prueba un caso diferente de vecinoLibre
%% de Inicio. Si en algún momento no puede unificar con ninguno de los vecinos Libres de Inicio, entonces el Camino era imposible, pues requería que el robot pasara por una
%% casilla ocupada o que fuera de una casilla a otra que no fuera su casilla vecina.

%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.
%% Usa generate & test porque primero genera un camino posible y despues ve si cumple el tamaño pedido
camino2(Inicio, Fin, T, C) :- cantFilas(T, Fil), cantColumnas(T, Col), S is Fil*Col, distancia(Inicio, Fin, D), between(D, S, N), camino(Inicio, Fin, T, C), length(C, N).

%% 6.1 Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.
%%
%% Inicio bajo esta implementación debe estar instanciado, pues lo usamos en la función de distancia para instanciar D
%% usando la posición de Inicio
%%
%% Camino puede o no estar instanciado, pues la primera vez que lo usamos es en la función camino, que antes definimos que podía estar instanciado
%% Despues es usado en length, donde siempre está instanciado.


%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.
%% Usa generate & test porque genera un camino posible y despues ve que tenga menor longitud a todos los demas caminos
caminoOptimo(Inicio, Fin, T, C) :- camino(Inicio, Fin, T, C), length(C, Len), not((camino(Inicio, Fin, T, C1), length(C1, Len1), Len1 < Len)).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% Usa generate & test porque genera un camino del primer tablero y despues ve si es posible en el segundo tablero
caminoDual(Inicio, Fin, T1, T2, C) :- camino(Inicio, Fin, T1, C), todasPosLibres(T2, C).


%%%%%%%%
%% TESTS
%%%%%%%%

%%Tableros
tableros(ocupado2x2, T) :- tablero(2, 2, T), ocupar(pos(0, 0), T), ocupar(pos(1, 1), T).
tableros(ocupado3x3, T) :- tablero(3, 3, T), ocupar(pos(0, 0), T), ocupar(pos(1, 1), T).
tableros(sinCamino00, T) :- tablero(4, 4, T), ocupar(pos(1, 0), T), ocupar(pos(0, 1), T).
tableros(ocupado5x5, T) :- tablero(5, 5, T), ocupar(pos(2, 1), T).
tableros(ocupado3x3Dual, T) :- tablero(3, 3, T), ocupar(pos(1, 1), T), ocupar(pos(2, 2), T).
tableros(ocupado4x4, T) :- tablero(4, 4, T), ocupar(pos(0, 0), T), ocupar(pos(0, 1), T), ocupar(pos(1, 0), T), ocupar(pos(1, 1), T).

cantidadTestsTablero(4). % Actualizar con la cantidad de tests que entreguen
testTablero(1) :- tablero(0,0,[]).
testTablero(2) :- ocupar(pos(0,0), [[ocupada]]).
% Agregar mas tests
testTablero(3) :- tablero(50, 50, T), length(T, 50), cantColumnas(T, 50).
testTablero(4) :- tableros(ocupado3x3, T), T = [[X1, X2, X3],
                                                [Y1, Y2, Y3],
                                                [Z1, Z2, Z3]],
    nonvar(X1), X1 = ocupada, nonvar(Y2), Y2 = ocupada, var(X2), var(X3), var(Y1), var(Y3), var(Z1), var(Z2), var(Z3).



cantidadTestsVecino(5). % Actualizar con la cantidad de tests que entreguen
testVecino(1) :- vecino(pos(0,0), [[_,_]], pos(0,1)).
% Agregar mas tests
testVecino(2) :- tableros(ocupado3x3, T), setof(V, vecino(pos(1, 1), T, V), L), L = [pos(0, 1), pos(1, 0), pos(1, 2), pos(2, 1)].
testVecino(3) :- tableros(ocupado3x3, T), setof(V, vecinoLibre(pos(1, 1), T, V), L), L = [pos(0, 1), pos(1, 0), pos(1, 2), pos(2, 1)].
testVecino(4) :- tableros(ocupado3x3, T), bagof(V, vecinoLibre(pos(0,1), T, V), B), B = [pos(0, 2)].
testVecino(5) :- tableros(ocupado2x2, T), not(vecinoLibre(pos(0, 1), T, _)).


cantidadTestsCamino(7). % Actualizar con la cantidad de tests que entreguen
% Agregar mas tests
testCamino(1) :- tableros(sinCamino00, T), not(camino(pos(0, 0), pos(1, 2), T, _)).
testCamino(2) :- tableros(sinCamino00, T), not(camino(pos(1, 2), pos(0, 1), T, _)).
testCamino(3) :- tableros(ocupado3x3, T), not(camino(pos(1, 1), pos(0, 1), T, _)).
testCamino(4) :- tablero(3, 3, T), not(camino(pos(1, 1), pos(3, 1), T, _)).
testCamino(5) :- tablero(3, 3, T), not(camino(pos(1, -3), pos(1, 1), T, _)).
testCamino(6) :- tablero(3, 3, T), bagof(C, camino2(pos(0, 2), pos(2, 0), T, C), B), tamanoCreciente(B).
testCamino(7) :- tablero(4, 4, T), bagof(C, camino2(pos(0, 1), pos(2, 0), T, C), B), sinRepetidos(B).


cantidadTestsCaminoOptimo(4). % Actualizar con la cantidad de tests que entreguen
testCaminoOptimo(1) :- tablero(3, 3, T), bagof(C, caminoOptimo(pos(0, 2), pos(2, 0), T, C), B), sinRepetidos(B), length(B, 6).
testCaminoOptimo(2) :- tablero(5, 5, T), caminoOptimo(pos(0, 4), pos(4, 2), T, Co), length(Co, Leno), forall(camino(pos(0, 4), pos(4, 2), T, C), (length(C, Len1), Leno =< Len1)).
testCaminoOptimo(3) :- tableros(ocupado5x5, T), not(caminoOptimo(pos(0, 0), pos(2, 1), T, _)).
testCaminoOptimo(4) :- tablero(4, 5, T), caminoOptimo(pos(0, 0), pos(0, 4), T, C),C = [pos(0, 0), pos(0, 1), pos(0, 2), pos(0, 3), pos(0, 4)].

cantidadTestsCaminoDual(5). % Actualizar con la cantidad de tests que entreguen
% Agregar mas tests
testCaminoDual(1) :- tablero(2, 2, T1), ocupar(pos(1, 0), T1), tablero(3, 3, T2), caminoDual(pos(0, 0), pos(1, 1), T1, T2, C), C = [pos(0, 0), pos(0, 1), pos(1, 1)], length(C, 3).  
testCaminoDual(2) :- tableros(ocupado3x3, T1), tableros(ocupado3x3Dual, T2), not(caminoDual(pos(2, 0), pos(0, 2), T1, T2, _)).
testCaminoDual(3) :- tableros(ocupado3x3, T1), tablero(3, 3, T2), bagof(C, caminoDual(pos(1, 0), pos(1, 2), T1, T2, C), B), B = [[pos(1, 0), pos(2, 0), pos(2, 1),
                                                                                                                                  pos(2, 2), pos(1, 2)]].
testCaminoDual(4) :- tablero(4, 5, T), setof(C, caminoDual(pos(1, 0), pos(2, 1), T, T, C), S1), setof(C, camino(pos(1, 0), pos(2, 1), T, C), S2), S1 = S2.
testCaminoDual(5) :- tablero(2, 2, T1), tableros(ocupado4x4, T2), not(caminoDual(pos(0, 1), pos(1, 1), T1, T2, _)).

tests(tablero) :- cantidadTestsTablero(M), forall(between(1,M,N), testTablero(N)).
tests(vecino) :- cantidadTestsVecino(M), forall(between(1,M,N), testVecino(N)).
tests(camino) :- cantidadTestsCamino(M), forall(between(1,M,N), testCamino(N)).
tests(caminoOptimo) :- cantidadTestsCaminoOptimo(M), forall(between(1,M,N), testCaminoOptimo(N)).
tests(caminoDual) :- cantidadTestsCaminoDual(M), forall(between(1,M,N), testCaminoDual(N)).

tests(todos) :-
  tests(tablero),
  tests(vecino),
  tests(camino),
  tests(caminoOptimo),
  tests(caminoDual).

tests :- tests(todos).
