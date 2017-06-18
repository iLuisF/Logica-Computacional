

%Concatena la Cola con la cabeza, ei, la salida es L = [1, 2, 3, 4, 5]
listar(L) :- Cola = [2, 3, 4, 5], L = [1 | Cola].

%Concatena la cola con la cabeza, ei, la salida es L = [2, 3, 4, [4.1, 4.2, 4.3], 5, 6].
listar2(L) :- Cabeza = 2, Cola = [3, 4, [4.1, 4.2, 4.3], 5, 6], L = [Cabeza|Cola].

%Concatena la cola con la cabeza, ei, la salida es L = [1, 2, 3, 4, [4.1, 4.2, 4.3], 5, 6].
listar3(L) :- Cabeza = 2, Cola = [3, 4, [4.1, 4.2, 4.3], 5, 6], L = [1, Cabeza|Cola].

%Regresa solo la cabeza de la lista, esto se hace por medio de la unificación.
%Ejemplo dameCabeza([1, 2 ,3, 4, 5], Cabeza). Devuelve Cabeza = 1 ;
dameCabeza([C|L], C).

%Elimina la cabeza de la lista haciendo unificación.
%Ejemplo eliminaCabeza([1, 2 ,3, 4, 5], Cabeza). Devuelve L = [2, 3, 4, 5] ;
eliminaCabeza([C|L], L).