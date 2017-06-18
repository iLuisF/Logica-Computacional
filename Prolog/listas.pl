
%Concatena la Cola con la cabeza, ei, la salida es L = [1, 2, 3, 4, 5]
listar(L) :- Cola = [2, 3, 4, 5], L = [1 | Cola].

%Concatena la cola con la cabeza, ei, la salida es L = [2, 3, 4, [4.1, 4.2, 4.3], 5, 6].
listar2(L) :- Cabeza = 2, Cola = [3, 4, [4.1, 4.2, 4.3], 5, 6], L = [Cabeza|Cola].

%Concatena la cola con la cabeza, ei, la salida es L = [1, 2, 3, 4, [4.1, 4.2, 4.3], 5, 6].
listar3(L) :- Cabeza = 2, Cola = [3, 4, [4.1, 4.2, 4.3], 5, 6], L = [1, Cabeza|Cola].

%Regresa solo la cabeza de la lista, esto se hace por medio de la unificación.
%Ejemplo dameCabeza([1, 2 ,3, 4, 5], Cabeza). Devuelve Cabeza = 1 ;
dameCabeza([C|_], C).

%Elimina la cabeza de la lista haciendo unificación.
%Ejemplo eliminaCabeza([1, 2 ,3, 4, 5], L). Devuelve L = [2, 3, 4, 5] ;
eliminaCabeza([_|L], L).

%Se quitan dos elementos de la cabeza.
%eliminaDosCabeza([1, 2, 3, 4, 5], L). Devuelve como resultado L = [3, 4, 5].
eliminaDosCabeza([_, _ | L], L).

%Base de conocimientos con listas.
progenitor(pedro, [ana, ramon]).
progenitor(ana, [ramon, pepe, juan]).

%Ejemplo, dervolver el primer hijo.
%primerHijo(pedro, H). Devuelve:
%H = ana ;
primerHijo(P, Hijo) :- progenitor(P, [Hijo| _]).

%Buscando en listas.
progenitor(pedro, [ana, ramon, pedro, javier, josh, vilma, nicolas]).
progenitor(juan, [ben, pepe, josue, jesica, pavel, keith, kyle]).

%Verifica si un hijo corresponde a un padre. Por ejemplo: padreDe(pedro, josh). 
%regresa true.
padreDe(Padre, Hijo) :- progenitor(Padre, Hijos), buscar(Hijo, Hijos).

%Caso base.
buscar(Hijo, []) :- !, fail.
%Se busca en la lista de hijos.
buscar(Hijo, [Hijo, L]) :- !, true.
buscar(Hijo, [C|L]) :- buscar(Hijo, L).