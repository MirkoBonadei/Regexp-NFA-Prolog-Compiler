% Progetto Prolog - Linguaggi di programmazione I A.A. 2011-2012
% 
% Autore: Mirko Bonadei - 735300
%

is_regular_expression(Re) :-
	atomic(Re),
	!.
is_regular_expression(Re) :-
	Re =.. Rs,
	is_regular_expression_helper(Rs).

is_regular_expression_helper([bar|[R]]) :-
	is_regular_expression(R).
is_regular_expression_helper([star|[R]]) :-
	is_regular_expression(R).
is_regular_expression_helper([or|[R1,R2]]) :-
	is_regular_expression(R1),
	is_regular_expression(R2).
is_regular_expression_helper([plus|[R]]) :-
	is_regular_expression(R).
is_regular_expression_helper([seq|Rs]) :-
	all(is_regular_expression, Rs).
is_regular_expression_helper([oneof|Rs]) :-
	all(is_regular_expression, Rs).

% Predicato all(predicato, Lista) -> vero se tutti gli elementi della Lista
% soddisfano il predicato.
all(_, []).
all(P, [H|T]) :-
	ToCall =.. [P, H],
	call(ToCall),
	all(P, T).

% Predicato nfa_recognize(FA_Id, Input) -> vero se la Stringa in Input viene
% riconosciuta dall'NFA contenuto nella Knowledge Base, il quale avrà ID pari
% a FA_Id.
nfa_recognize(FA_Id, Input) :-
	nfa_initial(FA_Id, Initial),
	nfa_recognize(FA_Id, Input, Initial).
nfa_recognize(FA_Id, [], State) :-
	nfa_final(FA_Id, State).
nfa_recognize(FA_Id, [Head|Tail], State) :-
	nfa_delta(FA_Id, State, Head, State2),
	nfa_recognize(FA_Id, Tail, State2).
nfa_recognize(FA_Id, Input, State) :-
	nfa_delta(FA_Id, State, epsilon, State2),
	nfa_recognize(FA_Id, Input, State2).

% nfa_re_compile/2
nfa_re_compile(FA_Id, Exp) :-
	is_regular_expression(Exp),
	nfa_re_compile_helper(FA_Id, Exp, Initial, Final),
	assert(nfa_initial(FA_Id, Initial)),
	assert(nfa_final(FA_Id, Final)).

nfa_re_compile_helper(FA_Id, A, Initial, Final) :-
	atomic(A),
	gensym(q, Initial),
	gensym(q, Final),
	assert(nfa_delta(FA_Id, Initial, A, Final)).
nfa_re_compile_helper(FA_Id, or(First, Second), Initial, Final) :-
	% genero gli identificativi degli stati iniziali e finali dell'automa
	gensym(q, Initial),
	gensym(q, Final),

	% compilo i sotto stati
	nfa_re_compile_helper(FA_Id, First, Initial1, Final1),
	nfa_re_compile_helper(FA_Id, Second, Initial2, Final2),

	% epsilon transizioni di completamento
	assert(nfa_delta(FA_Id, Initial, epsilon, Initial1)),
	assert(nfa_delta(FA_Id, Initial, epsilon, Initial2)),
	assert(nfa_delta(FA_Id, Final1, epsilon, Final)),
	assert(nfa_delta(FA_Id, Final2, epsilon, Final)).
nfa_re_compile_helper(FA_Id, star(Exp), Initial, Final) :-
	% genero gli identificativi degli stati iniziali e finali dell'automa
	gensym(q, Initial),
	gensym(q, Final),

	% compilo i sotto stati riguardanti l'espresione regolare interna
	nfa_re_compile_helper(FA_Id, Exp, Initial1, Final1),

	% epsilon transizioni di completamento
	assert(nfa_delta(FA_Id, Initial, epsilon, Final)),
	assert(nfa_delta(FA_Id, Initial, epsilon, Initial1)),
	assert(nfa_delta(FA_Id, Final1, epsilon, Final)),
	assert(nfa_delta(FA_Id, Final1, epsilon, Initial1)).
nfa_re_compile_helper(FA_Id, plus(Exp), Initial, Final) :-
	% compilo subito l'espressione interna
	nfa_re_compile_helper(FA_Id, Exp, Initial, Final),
	
	% aggioungo la epsilon transizione per far si che sia possibile inserire
	% altre stringhe che soddisfino l'espressione.
	assert(nfa_delta(FA_Id, Final, epsilon, Initial)).
nfa_re_compile_helper(FA_Id, Exp, Initial, Final) :-
	Exp =.. [seq|[First|Others]],
	nfa_re_compile_helper(FA_Id, First, Initial, FinalFirst),
	nfa_re_compile_seq_helper(FA_Id, Others, FinalFirst, Final).
nfa_re_compile_helper(FA_Id, Exp, Initial, Final) :-
	Exp =.. [oneof|P],
	% genero gli stati iniziali e finali dell'automa
	gensym(q, Initial),
	gensym(q, Final),

	% compilo tutti gli automi e memorizzo coppie dei loro stati iniziali e
	% finali in un formato che viene spiegato nella documentazione dei predicati
	% nfa_re_compile_oneof_helper e nfa_re_compile_oneof_linker.
	nfa_re_compile_oneof_helper(FA_Id, P, [], Pairs),

	% Collego gli stati iniziali e finali di ogni automa con delle espilon
	% transizioni dallo stato iniziale dell'automa e verso lo stato finale
	% dell'automa.
	nfa_re_compile_oneof_linker(FA_Id, Initial, Final, Pairs).

% Predicato che risulta vero se nel quarto parametro sono presenti termini
% pair/2 per ogni espressione regolare contanuta nel secondo parametro.
% pari(Initial, Final) rappresenta lo stato iniziale e finale di un automa
% precedentemente compilato. Non ci interessa sapere altro, perchè questo
% predicato viene esclusivamente chiamato dal predicato che compile
% l'espressione regolare oneof.
nfa_re_compile_oneof_helper(_, [], Pairs, Pairs).
nfa_re_compile_oneof_helper(FA_Id, [R|Rs], Acc, Pairs) :-
	nfa_re_compile_helper(FA_Id, R, Initial, Final),
	nfa_re_compile_oneof_helper(FA_Id, Rs, [pair(Initial, Final)|Acc], Pairs).

% Predicato che produce solo side effects. In modo da collegare ogni automa
% rappresentato dai fatti pair/2 contenuti nel secondo parametro con gli stati
% iniziale e finale passati come terzo e quarto parametro.
nfa_re_compile_oneof_linker(_, _, _, []).
nfa_re_compile_oneof_linker(FA_Id, Initial, Final, [pair(InitialP, FinalP)|Rs]) :-
	assert(nfa_delta(FA_Id, Initial, epsilon, InitialP)),
	assert(nfa_delta(FA_Id, FinalP, epsilon, Final)),
	nfa_re_compile_oneof_linker(FA_Id, Initial, Final, Rs).

	
% Aiuta a generare l'automa in caso di sequenza con un numero non definito di
% espressioni regolari.
nfa_re_compile_seq_helper(_, [], Final, Final).
nfa_re_compile_seq_helper(FA_Id, [Last], FinalP, Final) :-
	nfa_re_compile_helper(FA_Id, Last, InitialLast, FinalLast),
	assert(nfa_delta(FA_Id, FinalP, epsilon, InitialLast)),
	!,
	nfa_re_compile_seq_helper(FA_Id, [], FinalLast, Final).
nfa_re_compile_seq_helper(FA_Id, [H|T], FinalP, Final) :-
	nfa_re_compile_helper(FA_Id, H, InitialH, FinalH),
	assert(nfa_delta(FA_Id, FinalP, epsilon, InitialH)),
	nfa_re_compile_seq_helper(FA_Id, T, FinalH, Final).


% Predicati di utility.
%
% nfa_clear/0 "pulisce" la knowledge base da tutti i fatti generati da
% precedenti compilazioni.
nfa_clear :-
	%retract(nfa_initial(_, _)),
	%retract(nfa_final(_, _)),
	%retract(nfa_delta(_, _, _, _)),
	%fail.
	nfa_clear(_).

% nfa_clear/1 "pulisce" knowledge base da tutti i fatti generati da una
% compilazione precedente, attenendosi però solo a quelli generati per la
% compilazione dell'automa identificato dall'ID nell'unico parametro.
nfa_clear(FA_Id) :-
	retract(nfa_initial(FA_Id, _)),
	retract(nfa_final(FA_Id, _)),
	retract(nfa_delta(FA_Id, _, _, _)),
	fail.

% nfa_list/0 esegue il listing dei soli fatti riguardanti gli NFA
% precedentemente compilati e quindi presenti nella knowledge base.
nfa_list :-
	%listing(nfa_initial(_, _)),
	%listing(nfa_final(_, _)),
	%listing(nfa_delta(_, _, _, _)),
	%fail.
	nfa_list(_).

% nfa_list/1 esegue i listing dei soli predicati riguardanti l'NFA
% identificato con l'id presente nell'unico parametro.
nfa_list(FA_Id) :-
	listing(nfa_initial(FA_Id, _)),
	listing(nfa_final(FA_Id, _)),
	listing(nfa_delta(FA_Id, _, _, _)),
	fail.

% Per evitare errori nella nfa_list/0 e nella nda_list/1 in caso non ci siano
% fatti riguardanti gli NFA caricati nella knowledge base, ho deciso di
% inserire queste linee di codice che verrebbero poi ad ogni modo inserite da
% Prolog.
:- dynamic nfa_initial/2.
:- dynamic nfa_final/2.
:- dynamic nfa_delta/4.
