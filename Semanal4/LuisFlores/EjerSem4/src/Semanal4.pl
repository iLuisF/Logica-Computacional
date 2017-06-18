
%Se define el arbol binario de busqueda como un arbol vacio o un arbol con
%subarboles binarios.
arbol(empty).
arbol(binario(root, left, right)) :- arbol(left), arbol(right).

%Se crea un arbol cumpliendo con la definición de arbol(raiz, izquierdo, derecho).
insertar(empty, X, arbol(X, empty, empty)).
insertar(Arbol, X, Arbol) :- Arbol = arbol(X, _, _).
insertar(arbol(R, I, D), X, arbol(R, I2, D)) :- X < R, insertar(I, X, I2).
insertar(arbol(R, I, D), X, arbol(R, I, D2)) :- X > R, insertar(D, X, D2).                              

%Relación para concatenar.
concatenar([], L, L).
concatenar([Head| Tail1], L2, [Head|Tail2]):- concatenar(Tail1, L2, Tail2).

%Relación de elementos visitados en preorden
preorden(empty,[]).
preorden(arbol(R,I,D),L):- preorden(I,L1),preorden(D,L2), concatenar([R],L1,L3),
                            concatenar(L3,L2,L).

%Relación de elementos visitados en inorden
inorden(empty,[]).
inorden(arbol(R,I,D),L):- inorden(I,L1),inorden(D,L2), concatenar(L1,[R],L3),
                           concatenar(L3,L2,L).

%Relación de elementos visitados en postorden
postorden(empty,[]).
postorden(arbol(R,I,D),L):- postorden(I,L1),postorden(D,L2), concatenar(L1,L2,L3),
                             concatenar(L3,[R],L).                              

