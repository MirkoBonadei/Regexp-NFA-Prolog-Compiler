% Progetto Prolog - Corso LP - A.A. 2011-2012
% Autore: Mirko Bonadei 
% Matricola: 735300
% -*- Mode: Prolog -*-
%
load_test_file('test.plt').

% is_regular_expression/1 -> true se il parametro è conforme alle norme di
% riconoscimento definite per le Regular Expression.
% true -> se è un'espressione regolare valida.
is_regular_expression(Re) :-
	% esclusione forzata della lista vuota che è un 'atomic'
	Re \= [],
	is_regular_expression_helper(Re),
	!.

% is_regular_expression_helper/1 -> true se il parametro soffisfa uno dei 
% pattern base delle Regular Expression. 
% true -> se è un'espressione regolare valida.
is_regular_expression_helper(Re) :-
	atomic(Re).
is_regular_expression_helper(Re) :-
	Re =.. Rs,
	% Nel caso non fosse 'atomic' viene richiesto
	% l'aiuto di un secondo predicato che tratta specificamente i pattern dei
	% vari casi.
	is_regular_expression_case(Rs).

% is_regular_expression_case/1 -> esamina ricorsivamente i vari pattern
% delle Regular Expression, determinandone la loro validità.
% true -> se è un'esprezzione regolare valida.
is_regular_expression_case([bar|[R]]) :-
	is_regular_expression(R).
is_regular_expression_case([star|[R]]) :-
	is_regular_expression(R).
is_regular_expression_case([or|[R1,R2]]) :-
	is_regular_expression(R1),
	is_regular_expression(R2).
is_regular_expression_case([plus|[R]]) :-
	is_regular_expression(R).
is_regular_expression_case([seq|Rs]) :-
	all(is_regular_expression, Rs).
is_regular_expression_case([oneof|Rs]) :-
	all(is_regular_expression, Rs).

% all/2 chiama in seguenza il predicato espresso primo argomento 
% con argomento gli elementi della lista al secondo parametro.
% true -> se il predicato da chiamare risulta vero con tutti i parametri.
all(_, []).
all(P, [H|T]) :-
	ToCall =.. [P, H],
	call(ToCall),
	all(P, T).

% nfa_recognize/2
% E' il predicato di riconoscimento di una stringa di caratteri passata 
% ovviamente sottoforma di lista di atoms.
% Ha 2 parametri:
% 				* FA_Id -> Id dell'NFA
% 				* Input -> lista di caratteri da analizzare
% true -> quando dopo aver analizzato l'Input l'NFA si trova in uno 
% 		  stato finale.
nfa_recognize(_, Input) :-
	var(Input), % non posso dare variabili in da riconoscere
	!,
	fail.
nfa_recognize(FA_Id, Input) :-
	% nel caso FA_Id sia una variabile unbound, sfrutto il backtracking per 
	% generare tutte le possibili soluzioni (ho dovuto implementare questa
	% clausola perchè solo con la successiva il comportamento non era 
	% completo dato che bloccavo il backtracking con un cut)
	var(FA_Id),
	nfa_initial(FA_Id, _),
	nfa_recognize(FA_Id, Input).
nfa_recognize(FA_Id, Input) :-
	% ho implementato una memoria anti-loop (vedere README per spiegazioni)
	not(clear_loop_memory(_)), % pulisco la memoria anti loop)
	% se entro in questa clausola FA_Id deve essere bound
	nonvar(FA_Id), 
	nfa_initial(FA_Id, Initial),
	nfa_recognize(FA_Id, Input, Initial, Final),
	!,
	not(clear_loop_memory(FA_Id)),
	nfa_final(FA_Id, Final).

% nfa_recognize/4
% E' il predicato di riconoscimento vero e proprio che si muove nell'NFA 
% in base all'Input da analizzare. Prevede tutti i casi necessari.
% Ha 4 parametri:
% 				* FA_Id -> Id dell'NFA
% 				* Input -> lista di caratteri da analizzare
% 				* Initial -> Stato Iniziale dell'NFA 
% 				* Final -> Stato finale dell'NFA
% true -> quando dopo aver analizzato l'Input partendo da Initial, 
% 		  l'NFA identificato con FA_Id si trova nello stato Final.
% 		  In questo caso Final è solo un identificativo, non è 
% 		  realmente detto che esso sia uno stato accettante. 
% 		  Il tutto verrà controllato da chi ha invocato questo predicato.
nfa_recognize(FA_Id, [], State, State) :-
	nfa_fail(FA_Id, State). % caso base
nfa_recognize(FA_Id, [], State, State) :-
	nfa_final(FA_Id, State). % caso base
nfa_recognize(FA_Id, [Head|Tail], State, Final) :-
	nonvar(Head), % escludo anche qua le variabili dal riconoscimento
	nfa_delta(FA_Id, State, Head, State2),
	nfa_recognize(FA_Id, Tail, State2, Final).
nfa_recognize(FA_Id, [Head|Tail], State, Final) :-
	nonvar(Head), % escludo anche qua le variabili dal riconoscimento
	Head = epsilon,
	nfa_recognize(FA_Id, Tail, State, Final).
nfa_recognize(FA_Id, Input, State, Final) :-
	% in caso di epsilon transizioni 
	% se non ancora presente in knowledge base, 
	% carico la memoria anti-loop
	% vedere README per funzionamento
	nfa_delta(FA_Id, State, epsilon, State2),
	not(loop_memory(FA_Id, State, State2, _)),
	assert(loop_memory(FA_Id, State, State2, 1)),
	nfa_recognize(FA_Id, Input, State2, Final).
nfa_recognize(FA_Id, Input, State, Final) :-
	% se la memoria anti-loop è già in knowledge base allora 
	% prima di muovermi su una epsilon transizione controllo 
	% la memoria per evitare di andare in loop in casi particolari.
	% Ovviamente aggiorno la memoria ad ogni passaggio.
	nfa_delta(FA_Id, State, epsilon, State2),
	loop_memory(FA_Id, State, State2, N),
	
	% il numero di passaggi per arco è di 100, un valore "hard-coded",
	% che permette di passare anche degli stress test di un certo tipo,
	% se si vuole spigersi oltre si può astrarre.
	N =< 100, 
	retract(loop_memory(FA_Id, State, State2, N)),
	N1 is N + 1,
	assert(loop_memory(FA_Id, State, State2, N1)),
	nfa_recognize(FA_Id, Input, State2, Final).

% nfa_re_compile/2
% Compila l'espressione regolare in un NFA. E' un predicato che genera 
% side effects sulla knowledge base.
% Ha 2 parametri:
% 				* FA_Id -> Id dell'NFA
% 				* Exp -> espressione regolare da compilare
% true -> se la compilazione è andata a buon fine.
nfa_re_compile(FA_Id, Exp) :-
	% vieto la compilazione di una espressione regolare in un NFA 
	% con un NFA_Id già presente nella Knowledge Base
	not(nfa_initial(FA_Id, _)), 
	% vieto che FA_Id sia una variabile
	nonvar(FA_Id),
	% ovviamente controllo che l'espressione regolare sia valida
	is_regular_expression(Exp),
	% se lo è, allora posso passare il lavoroad un metodo di aiuto
	nfa_re_compile_helper(FA_Id, Exp, Initial, Final),
	!,
	% definisco ufficialmente lo stato iniziale e finale dell'NFA
	assert(nfa_initial(FA_Id, Initial)),
	assert(nfa_final(FA_Id, Final)).

% nfa_re_compile_helper/4
% Compila l'espressione regolare passata come secondo parametro in 
% un NFA con Id pari al valore passato come primo parametro.
% Initial rappresenta lo stato iniziale dell'NFA dopo la compilazione, 
% Final rappresente lo stato finale dell'NFA dopo la compilazione.
% true -> quando compilando l'espressione regolare in un NFA con 
% 		  NFA_Id pari al primo parametro, Initial è stato iniziale 
% 		  dell'NFA e Final è stato finale.
nfa_re_compile_helper(FA_Id, A, Initial, Final) :-
	% compilazione di una regexp banale
	atomic(A),
	gensym(q, Initial),
	gensym(q, Final),
	assert(nfa_delta(FA_Id, Initial, A, Final)).
nfa_re_compile_helper(FA_Id, or(First, Second), Initial, Final) :-
	% compilazione di una regexp or

	% genero gli identificativi degli stati iniziali e finali dell'automa
	gensym(q, Initial),
	gensym(q, Final),

	% compilo le "sotto regexp" della or
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

	% compilo la regexp interna alla star
	nfa_re_compile_helper(FA_Id, Exp, Initial1, Final1),

	% epsilon transizioni di completamento
	assert(nfa_delta(FA_Id, Initial, epsilon, Initial1)),
	assert(nfa_delta(FA_Id, Final1, epsilon, Final)),
	assert(nfa_delta(FA_Id, Final1, epsilon, Initial1)),
	assertz(nfa_delta(FA_Id, Initial, epsilon, Final)).
nfa_re_compile_helper(FA_Id, plus(Exp), Initial, Final) :-
	% per compilare una plus che altro non è che una star con una 
	% ripetizione obbligatoria, quindi mi creo grazie alla 
	% regexp seq, una sequenza che soffisfi la struttura della plus
	nfa_re_compile_helper(FA_Id, seq(Exp, star(Exp)), Initial, Final).
nfa_re_compile_helper(FA_Id, Exp, Initial, Final) :-
	% la seq è una regexp con un numero indefinito di parametri 
	% o sotto regexp.
	% Per compilarla quindi mi avvaloro di un metodo helper 
	% per linkare tra loro gli stati delle sotto-regexp 
	% successive alla prima
	Exp =.. [seq|[First|Others]],
	% compilo la prima ed ottengo lo stato iniziale
	nfa_re_compile_helper(FA_Id, First, Initial, FinalFirst),
	% chiamo il metodo helper accennato in precedenza per gestire la
	% compilazione di tutte le altre regexp che tra le altre cose 
	% ci daranno lo stato finale
	nfa_re_compile_seq_helper(FA_Id, Others, FinalFirst, Final).
nfa_re_compile_helper(FA_Id, Exp, Initial, Final) :-
	% anche la oneof come la seq ha un numero indefinito 
	% di parametri composti da regexp. 
	% In questo caso, gli automi generati dalle sotto regexp non
	% sono collegati in sequenza, quindi avremo poi bisogno di 
	% un altro metodo helper per creare le giuste transizioni
	Exp =.. [oneof|P],
	% genero gli stati iniziali e finali dell'automa
	gensym(q, Initial),
	gensym(q, Final),

	% compilo tutti gli automi e memorizzo in una lista 
	% delle coppie dei loro stati iniziali queste coppie
	% hanno la seguente forma:
	% 	pair(StatoIniziale, StatoFinale)
	% questa codifica interna mi consente poi di dare "in pasto" la lista 
	% ad un predicato che mi creerà tutte le corrette epsilon transizioni
	nfa_re_compile_oneof_helper(FA_Id, P, [], Pairs),
	nfa_re_compile_oneof_linker(FA_Id, Initial, Final, Pairs).
nfa_re_compile_helper(FA_Id, bar(Exp), Initial, Final) :-
	% la compilazione della bar è stata affrontata anche se presenta dei 
	% problemi di gestione in caso di riconoscimento di regexp anndate.
	% Vedere il README per ogni chiarimento a riguardo.
	% Fare fede al README per capire inoltre come ragiona la bar.
	
	% compilo la regexp interna alla bar
	nfa_re_compile_helper(FA_Id, Exp, Initial, Final1),

	% creo lo stato eater
	gensym(q, Eater),
	% creo lo stato finale
	gensym(q, Final),
	
	% rendo lo stato accettante dell'NFA generato dalla regexp uno stato
	% di fallimento certo, in modo da poter implementare il comportamento 
	% della bar in un NFA con epsilon transizioni, 
	% senza doverlo trasformare in DFA.
	asserta(nfa_fail(FA_Id, Final1)),
	% genero le opportune transizioni
	assertz(nfa_delta(FA_Id, Initial, epsilon, Eater)),
	assert(nfa_delta(FA_Id, Eater, _, Eater)),
	assert(nfa_delta(FA_Id, Eater, epsilon, Final)).

% nfa_re_compile_oneof_helper/4.
% Parametri:
% 	* FA_Id -> Id dell'NFA con il quale saranno registrate tutte le 
% 			   sotto regexp che varranno compilate
% 	* RS -> lista regexp da compilare
% 	* Acc -> lista coppie pairs(Initial, Final) di NFA già compilati
% 	* Pairs -> lista di tutti i pairs/2 generata dalla compilazione di 
% 			   tutte le regexp
% true -> quando il quarto parametro è una lista di pairs/2 generati 
% 		  dalla compilazione delle regexp presenti nella lista al 
% 		  secondo parametro.
nfa_re_compile_oneof_helper(_, [], Pairs, Pairs).
nfa_re_compile_oneof_helper(FA_Id, [R|Rs], Acc, Pairs) :-
	nfa_re_compile_helper(FA_Id, R, Initial, Final),
	nfa_re_compile_oneof_helper(FA_Id, Rs, 
								[pair(Initial, Final)|Acc], Pairs).

% nfa_re_compile_oneof_linker/4.
% Crea le giuste transizioni per l'FA_Id tra gli stati iniziali 
% e finali dei "sotto NFA" con il reale stato iniziale 
% e finale dell'NFA.
% Parametri:
% 	* FA_Id -> Id dell'NFA
% 	* Initial -> Stato iniziale dell'NFA
% 	* Final -> Stato finale dell'NFA
% 	* Pairs -> lista di pair/2 
% true -> se il predicato è in grado di ampliare la knowledge base 
% 		  con le giuste epsilon transizioni, suotando la lista dei pair/2
nfa_re_compile_oneof_linker(_, _, _, []).
nfa_re_compile_oneof_linker(FA_Id, 
							Initial, 
							Final, 
							[pair(InitialP, FinalP)|Rs]) :-
	assert(nfa_delta(FA_Id, Initial, epsilon, InitialP)),
	assert(nfa_delta(FA_Id, FinalP, epsilon, Final)),
	nfa_re_compile_oneof_linker(FA_Id, Initial, Final, Rs).

	
% nfa_re_compile_seq_helper/4.
% Parametri:
% 	* FA_Id -> Id dell'NFA
% 	* Rs -> lista di regexp da compilare
% 	* FinalP -> Stato finale della NFA precedente al finale
% 	* Final -> stato finale dell'ultimo automa, generato 
% 			   dall'ultima regexp.
% true -> questo predicato è molto basato sui side effects generati da 
% 		  compilazioni in sequeza. Ad ogni compilazione poi, collega 
% 		  l'NFA generato con il precedente. E' un predicato speciale 
% 		  creato solo ed esclusivamente per la compilazione della seq. 
% 		  Da quindi true se e solo se il predicato è in grado di compilare
% 		  e collegare tutti gli NFA generati.
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


%% ----- Predicati di Utility -----

% nfa_clear/0 : pulisce la knowledge base da tutti i fatti riguardanti
% compilazioni di regexp avvenute in precedenza.
% Viene valutata sempre true.
nfa_clear :-
	nfa_clear_nfa(_).

% nfa_clear_nfa/1 : pulisce al knowledge base da tutti i fatti 
% riguardanti la compilazione di una regexp in un NFA con 
% FA_Id pari al parametro.
nfa_clear_nfa(FA_Id) :-
	retractall(nfa_fail(FA_Id, _)),
	retractall(nfa_initial(FA_Id, _)),
	retractall(nfa_final(FA_Id, _)),
	retractall(nfa_delta(FA_Id, _, _, _)).

% nfa_list/0 : esegue il listing di tutti i fatti riguardanti 
% le compilazioni registrate nella knowledge base.
nfa_list :-
	nfa_list(_).

% nfa_list/1 : esegue il listing di tutti i fatti 
% riguardanti le compilazioni registrate nella knowledge 
% base riguardanti l'NFA con FA_Id pari al parametro
% del predicato.
nfa_list(FA_Id) :-
	listing(nfa_initial(FA_Id, _)),
	listing(nfa_final(FA_Id, _)),
	listing(nfa_delta(FA_Id, _, _, _)),
	listing(nfa_fail(FA_Id, _)).

% clear_loop_memory/1 : pulisce la Knowledge Base, eliminando i fatti
% riguardanti una memoria anti-loop utlizzata in fase di riconoscimento.
% Vedere il README per maggiori spiegazioni riguardo questa scelta di
% implementazione.
clear_loop_memory(FA_Id) :-
	retract(loop_memory(FA_Id, _, _, _)),
	fail.

% predicati dinamici
:- dynamic nfa_initial/2.
:- dynamic nfa_final/2.
:- dynamic nfa_delta/4.
:- dynamic nfa_fail/2.
:- dynamic loop_memory/4.
