%Ejercicio 1: Red semantica con 20 nodos.
uni_mexico(universidad, ipn).
uni_mexico(universidad, unam).
facultad(unam, ciencias).
facultad(unam, arquitectura).
facultad(unam, filosofia).
facultad(unam, ingenieria).
facultad(unam, psicologia).
alumno(ciencias, luis).
alumno(ciencias, mayra).
alumno(ciencias, aylin).
alumno(arquitectura, javier).
alumno(arquitectura, rodrigo).
alumno(filosofia, angela).
alumno(filosofia, eliza).
alumno(filosofia, mariana).
alumno(ingeneria, gustavo).
alumno(ingeneria, alex).
alumno(psicologia, jaime).
alumno(psicologia, david).

%Ejercicio 2: Relacion satisface
%Se considera caso por caso para poder dar un valor de verdad.
eres_facultad(X, Y, Z) :- facultad(Y, Z), uni_mexico(universidad, _), X = facultad.
eres_alumno(X, Y, Z) :- facultad(unam, Y), alumno(Y, Z), X = alumno.		
eres_uni_mexico(X, Y, Z) :- uni_mexico(Y, Z), X = uni_mexico.
satisface(X, Y, Z) :- eres_facultad(X, Y, Z) 
					; eres_alumno(X, Y, Z)
					; eres_uni_mexico(X, Y, Z). 

%Ejercicio 3
% U es la respuesta en findall.
% call(U) es el objetivo, es decir, como se hallaran las respuestas.
% L es la lista de respuestas.
obtenTodos(X, L) :- U =.. [X,_,_], findall(U, call(U), L).