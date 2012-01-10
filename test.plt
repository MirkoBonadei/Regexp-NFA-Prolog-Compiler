:- begin_tests(is_regular_expression).

%%%%%%%      TEST IS_REGULAR_EXPRESSION      %%%%%%%
test(is_regular_expression) :-
	is_regular_expression(a).
test(is_regular_expression, [fail]) :-
	is_regular_expression([]).
test(is_regular_expression, [error(_, _)]) :-
	is_regular_expression(q, w, e, r, t, y).
test(is_regular_expression, [fail]) :-
	is_regular_expression(_).
test(is_regular_expression):-
	is_regular_expression(epsilon).
test(is_regular_expression, [fail]) :-
	is_regular_expression([q, w, e, r, t, y]).
test(is_regular_expression, [error(_, _)]) :-
	is_regular_expression.
test(is_regular_expression) :-
	is_regular_expression(is_regular_expression).
test(is_regular_expression, [fail]) :-
	is_regular_expression([epsilon]).
test(is_regular_expression) :-
	is_regular_expression(or(a, b)).
test(is_regular_expression) :-
	is_regular_expression(seq(a, s, d, e, r)).
test(is_regular_expression) :-
	is_regular_expression(oneof(q, w, e, r, t, y)).
test(is_regular_expression) :-
	is_regular_expression(star(seq(a, s, d))).
test(is_regular_expression) :-
	is_regular_expression(bar(a)).
test(is_regular_expression) :-
	is_regular_expression(plus(or(seq(q, w, e, r, t, y), or(ciao, notte)))).
test(is_regular_expression) :-
	is_regular_expression(star(seq(or(a, s), plus(seq(q, w, e, r, t, y)),
	oneof(a, s, d, ciao, or(prova1, prova2), seq(t, e, s, t))))).
test(is_regular_expression) :-
	is_regular_expression(bar(seq(or(a, s), plus(seq(q, w, e, r, t, y)),
	oneof(a, s, d, ciao, or(prova1, prova2), seq(t, e, s, t))))).
test(is_regular_expression):-
	is_regular_expression(or(a, b)).
test(is_regular_expression):-
	is_regular_expression(or(123, c)).
test(is_regular_expression):-
	is_regular_expression(or(klalalalala, 100000000000200)).
test(is_regular_expression):-
	is_regular_expression(or(0, 0)).
test(is_regular_expression):-
	is_regular_expression(or(or, or)).
test(is_regular_expression):-
	is_regular_expression(or(100000000000000000, 10000000000000000000)).
test(is_regular_expression,[error(_, _)]):-
	is_regular_expression(or, or).
test(is_regular_expression,[fail]):-
	is_regular_expression(or(0, _)).
test(is_regular_expression,[fail]):-
	is_regular_expression(or(_, 0)).
test(is_regular_expression,[fail]):-
	is_regular_expression(or([], 0)).
test(is_regular_expression,[fail]):-
	is_regular_expression(or(0, [])).
test(is_regular_expression,[fail]):-
	is_regular_expression(or([], [])).
test(is_regular_expression):-
	Y=l,
	X=Y,
	is_regular_expression(or(X, a)).
test(is_regular_expression):-
	X=2, 
	Y=a,
	is_regular_expression(or(X, Y)).
test(is_regular_expression,[fail]):-
	is_regular_expression(or(or(a), 0)).
test(is_regular_expression,[fail]):-
	is_regular_expression(or(a, b, c, d, e)).
test(is_regular_expression,[fail]):-
	is_regular_expression(or(_)).
test(is_regular_expression,[fail]):-
	is_regular_expression(or(a, b, c)).
test(is_regular_expression,[fail]):-
	is_regular_expression(or(0)).
test(is_regular_expression,[fail]):-
	is_regular_expression(or([ahaha, jajaj])).
test(is_regular_expression,[fail]):-
	is_regular_expression(or([ciao])).
test(is_regular_expression,[fail]):-
	is_regular_expression(or([a], [b])).
test(is_regular_expression,[fail]):-
	is_regular_expression(or(_, _)).
test(is_regular_expression,[fail]):-
	is_regular_expression(or(a, _)).
test(is_regular_expression,[fail]):-
	is_regular_expression(or(_, a)).
test(is_regular_expression):-
	X=..[or, a, b],
	is_regular_expression(X).
test(is_regular_expression):-
	is_regular_expression(or(or, or)).
test(is_regular_expression):-
	is_regular_expression(or(bar, plus)).
test(is_regular_expression):-
	is_regular_expression(or(member, concat)).
test(is_regular_expression):-
	is_regular_expression(or(epsilon, epsilon)).
test(is_regular_expression):-
	is_regular_expression(
				or(or(or(or(or(or(or(or(or(or(a, b),
				or(or(or(or(or(or(or(c, d), e), f), h),
				 i), l), m)), n), o), p), q), r), s), t), u)).
test(is_regular_expression,[fail]):-
	is_regular_expression(
				or(or(or(or(or(or(or(or(or(or(a, b),
				or(or(or(or(or(or(or(c, d), e), f), h),
				 i), l), m)), n), o),[a]), q), r), s), t), u)).

test(is_regular_expression):-
	is_regular_expression(star(a)).
test(is_regular_expression):-
	is_regular_expression(star(epsilon)).
test(is_regular_expression):-
	is_regular_expression(star(10929902)).
test(is_regular_expression):-
	is_regular_expression(star(kkksjjs)).
test(is_regular_expression):-
	is_regular_expression(star(9)).
test(is_regular_expression,[fail]):-
	is_regular_expression(star(_)).
test(is_regular_expression,[fail]):-
	is_regular_expression(star([])).
test(is_regular_expression,[fail]):-
	is_regular_expression(star([1, 2, 3, 4])).
test(is_regular_expression,[fail]):-
	is_regular_expression(star(or([]))).
test(is_regular_expression):-
	is_regular_expression(star(star)).
test(is_regular_expression):-
	is_regular_expression(star(or)).
test(is_regular_expression):-
	is_regular_expression(star(000000000)).
test(is_regular_expression,[fail]):-
	is_regular_expression(star(l, l)).
test(is_regular_expression,[fail]):-
	is_regular_expression(star([kakkakksjd], [sa])).
test(is_regular_expression,[fail]):-
	is_regular_expression(star([ciao])).
test(is_regular_expression,[fail]):-
	is_regular_expression(star(_)).
test(is_regular_expression,[fail]):-
	X=..[or, _, b],
	is_regular_expression(star(X)).
test(is_regular_expression):-
	is_regular_expression(bar(q6678687)).
test(is_regular_expression):-
	is_regular_expression(bar(lili)).
test(is_regular_expression):-
	is_regular_expression(bar(cc)).
test(is_regular_expression):-
	is_regular_expression(bar(bar)).
test(is_regular_expression,[fail]):-
	is_regular_expression(bar(_)).
test(is_regular_expression,[fail]):-
	is_regular_expression(bar([])).
test(is_regular_expression,[fail]):-
	is_regular_expression(bar([1, 2, 3])).
test(is_regular_expression):-
	is_regular_expression(bar(m)).
test(is_regular_expression,[fail]):-
	is_regular_expression(bar(a, b, c, d)).
test(is_regular_expression,[fail]):-
	is_regular_expression(bar([a])).
test(is_regular_expression,[fail]):-
	is_regular_expression(bar(_)).
test(is_regular_expression,[fail]):-
	is_regular_expression(
							bar(bar(bar(bar(bar(bar(bar(bar(bar(
							seq(oneof(bar(bar(bar(bar(plus(or(star(
							plus(bar(bar(bar(oneof(seq(seq(oneof(
							plus(star(a)), b, _), a ,c), s, t), r, m)))
							)))), t)))))), w, e))))))))))).
test(is_regular_expression):-
	is_regular_expression(or(or, or)).
test(is_regular_expression):-
	is_regular_expression(or(bar, star)).
test(is_regular_expression):-
	is_regular_expression(plus(llllll)).
test(is_regular_expression):-
	is_regular_expression(plus(or)).
test(is_regular_expression):-
	is_regular_expression(plus(999989889)).
test(is_regular_expression):-
	is_regular_expression(plus(as9)).
test(is_regular_expression):-
	is_regular_expression(plus(plus)).
test(is_regular_expression,[fail]):-
	is_regular_expression(plus([])).
test(is_regular_expression,[fail]):-
	is_regular_expression(plus(plus(_))).
test(is_regular_expression,[fail]):-
	is_regular_expression(plus(_)).
test(is_regular_expression,[fail]):-
	is_regular_expression(seq([])).
test(is_regular_expression):-
	is_regular_expression(seq(seq, bar, or)).
test(is_regular_expression):-
	is_regular_expression(seq(12, 45, 66, s, d, c, ds, s, a, c)).
test(is_regular_expression,[fail]):-
	is_regular_expression(seq(12, 45, 66, s, d, c, [a], s, a, c)).
test(is_regular_expression):-
	is_regular_expression(seq).
test(is_regular_expression,[fail]):-
	is_regular_expression(seq(_, _, _)).
test(is_regular_expression,[fail]):-
	is_regular_expression(seq(_)).
test(is_regular_expression):-
	is_regular_expression(seq(a)).
test(is_regular_expression):-
	is_regular_expression(oneof(a, b, c, d, e, f, g)).
test(is_regular_expression):-
	is_regular_expression(oneof(a)).
test(is_regular_expression):-
	is_regular_expression(oneof(1, 2, 3, 4, 5, 6, 7)).
test(is_regular_expression):-
	is_regular_expression(oneof(star, bar, plus, oneof)).
test(is_regular_expression,[fail]):-
	is_regular_expression(oneof(star, bar, _, oneof)).
test(is_regular_expression,[fail]):-
	is_regular_expression(oneof(star, bar, [plus], oneof)).
test(is_regular_expression,[fail]):-
	is_regular_expression(oneof(_)).
test(is_regular_expression,[fail]):-
	is_regular_expression(oneof(star, bar, _, oneof)).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(or(a, _)).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(or(a)).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(or(or(_, 1), a)).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(star(_)).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(star(or(a))).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(star(star(_))).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(plus(_)).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(plus(star(_))).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(oneof([1, 2, 3, 4])).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(oneof(a, a, s, d, v, b, d, s, oneof(or(_)))).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(bar(_)).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(seq(a, a, a, a, _)).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(seq(_)).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(seq(seq(seq([]), a), a)).
test(is_regular_expressionfail,[fail]):-
	is_regular_expression(oneof(a, a, s, d, v, b, d, s, 
						  oneof(bar(bar(bar(bar(bar(_)))))))).
test(is_regular_expression):-
	is_regular_expression(oneof).
test(is_regular_expression):-
	is_regular_expression(seq).
test(is_regular_expression):- 
	is_regular_expression(seq(bar(or(a, b)), c)).
test(is_regular_expression):-
	is_regular_expression(seq(seq(seq(seq(seq(a, b), b), b), b), b)).
test(is_regular_expression):-
	is_regular_expression(bar(bar(bar(bar(bar(bar(
							oneof(a, b, or(c, d))))))))).
test(is_regular_expression):-
	is_regular_expression(bar(plus(star(or(a, b))))).
test(is_regular_expression):-
	is_regular_expression(bar(or(bar(bar(seq(a, oneof(c, d)))), x))).
test(is_regular_expression):-
	is_regular_expression(star(star(star(plus(plus(a)))))).
test(is_regular_expression,[fail]):-
	is_regular_expression(seq(bar(or(a, _)), c)).
test(is_regular_expression,[fail]):-
	is_regular_expression(seq(seq(seq(seq(seq(a, [b]), b), b), b), b)).
test(is_regular_expression,[fail]):-
	is_regular_expression(bar(bar(bar(bar(bar(bar(
								oneof(a, _, or(c, d))))))))).
test(is_regular_expression,[fail]):-
	is_regular_expression(bar(plus(star(or(a, b, d))))).
test(is_regular_expression,[fail]):-
	is_regular_expression(bar(or(bar(bar(seq(a, oneof([a], d)))), x))).
test(is_regular_expression,[fail]):-
	is_regular_expression(star(star(star(plus(plus(_)))))).

:- end_tests(is_regular_expression).
%%%%%%%      FINE TEST IS_REGULAR_EXPRESSION %%%%%%%


%%%%%%%      TEST SULLA COMPILAZIONE         %%%%%%%
:- begin_tests(compile).

% Atomico
test(compile, [fail]) :-
	nfa_re_compile(automa_vuoto, []).
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, ciao).
% Seq
test(compile, [fail, cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, seq([])).
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, seq(q, w, e, r, t, y)).
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, seq(a)).
test(compile, [fail, cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, seq([])).
% Or 
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, or(a, b)).
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, or(seq(q, w, e, r, t, y), ciao)).
test(compile, [fail, cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, or([], seq(q, w, e, r, t, y))).
test(compile, [fail, cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, or(or(a, c), [])).
% Star
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, star(a)).
test(compile, [fail, cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, star([])).
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, star(seq(q, w, e, r, t, y))).
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, star(
									star(or(seq(q, w, e, r, t, y), ciao)))
										).
% Oneof
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, oneof(a)).
test(compile, [fail, cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, oneof([])).
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, oneof(q, w, e, r, t, y)).
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, oneof(seq(q, w, e, r, t, y), 
									or(ciao, seq(t, y, u, i, o, p)), 
									qwerty)).
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, oneof(star(or(seq(star(c), d), 
										seq(a, s, d, or(z, x)))), 
										seq(q, w, e, r, t, y), qwerty)).
% Plus
test(compile, [fail, cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, plus([])).
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, plus(a)).
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, plus(seq(q, w, e, r, t, y))).
test(compile, [cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, plus(seq(or(a, s), 
										seq(q, w, e, r, t, y), 
										star(seq(m, n)), 
										oneof(u, i, o, p)))).
test(compile, [fail, cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, plus(seq([]))).
test(compile, [fail, cleanup(nfa_clear_nfa(automa_compile))]) :-
	nfa_re_compile(automa_compile, plus(star(oneof(ciao, [])))).

% Non compilazione di una regexp in un automa già presente in KB
test(compile, [fail, cleanup(nfa_clear_nfa(automa_compile))]):- 
	nfa_re_compile(automa_compile, oneof(star(bar(a)))),
	nfa_re_compile(automa_compile, b).

test(compile, [fail]) :-
	nfa_re_compile(compile, non_regexp(a)).
test(compile, [fail]) :-
	nfa_re_compile(_, a).
test(compile, [fail]) :-
	nfa_re_compile(compile, []).
test(compile, [fail]) :-
	nfa_re_compile(compile, _).
test(compile, [fail]) :-
	nfa_re_compile(compile, oneof(star(bar(_)))).

:- end_tests(compile).
%%%%%        FINE TEST SULLA COMPILAZIONE %%%%%

%%%%%        TEST SULLA PULIZIA DELLA KNOWLEDGE BASE %%%%%

:- begin_tests(clean).
test(clean) :-
	nfa_re_compile(clean, oneof(seq(q, w, e, r, t, y), bar(a), or(1, 2))),
	nfa_clear_nfa(clean),
	not(nfa_initial(clean, _)).
test(clean) :-
	nfa_re_compile(clean, oneof(seq(q, w, e, r, t, y), bar(a), or(1, 2))),
	nfa_clear,
	not(nfa_initial(clean, _)).

:- end_tests(clean).

%%%%%        FINE TEST SULLA PULIZIA DELLA KB        %%%%%

% Test unitari sul riconoscimento di un NFA 'atomico'.
:- begin_tests(nfa_atomico).

test(nfa_atomico, [
					setup(nfa_re_compile(atomico, a)), 
					cleanup(nfa_clear_nfa(atomico))
					]) :-
	nfa_recognize(atomico, [a]).
test(nfa_atomico, [
					fail, 
					setup(nfa_re_compile(atomico, ciao)), 
					cleanup(nfa_clear_nfa(atomico))
					]) :-
	nfa_recognize(atomico, [a]).
test(nfa_atomico, [
					fail, 
					setup(nfa_re_compile(atomico, ciao)), 
					cleanup(nfa_clear_nfa(atomico))
					]) :-
	nfa_recognize(atomico, []).
test(nfa_atomico, [
					setup(nfa_re_compile(atomico, ciao)), 
					cleanup(nfa_clear_nfa(atomico))
					]) :-
	nfa_recognize(atomico, [ciao]).
test(nfa_atomico, [
					fail, 
					setup(nfa_re_compile(atomico, ciao)), 
					cleanup(nfa_clear_nfa(atomico))
					]) :-
	nfa_recognize(atomico, [c, i, a, o]).
test(nfa_atomico, [
					fail, 
					setup(nfa_re_compile(atomico, ciao)), 
					cleanup(nfa_clear_nfa(atomico))
					]) :-
	nfa_recognize(atomico, [ciao, c]).
test(nfa_atomico, [
					setup(nfa_re_compile(atomico, 1)), 
					cleanup(nfa_clear_nfa(atomico))
					]) :-
	nfa_recognize(atomico, [1]).

:- end_tests(nfa_atomico).

% Test unitari sul riconoscimento di un NFA derivato da una regexp seq.
:- begin_tests(nfa_seq).

test(nfa_seq, [
				setup(nfa_re_compile(nfa_seq, seq(q, w, e, r, t, y))), 
			   	cleanup(nfa_clear_nfa(nfa_seq))
			  	]) :-
	nfa_recognize(nfa_seq, [q, w, e, r, t, y]).
test(nfa_seq, [
				setup(nfa_re_compile(nfa_seq, seq(qwerty))), 
				cleanup(nfa_clear_nfa(nfa_seq))
				]) :-
	nfa_recognize(nfa_seq, [qwerty]).
test(nfa_seq, [
				fail, 
				setup(nfa_re_compile(nfa_seq, seq(q, w, e, r, t, y))), 
				cleanup(nfa_clear_nfa(nfa_seq))
				]) :-
	nfa_recognize(nfa_seq, []).
test(nfa_seq, [
				fail, 
				setup(nfa_re_compile(nfa_seq, seq(q, w, e, r, t, y))), 
				cleanup(nfa_clear_nfa(nfa_seq))
				]) :-
	nfa_recognize(nfa_seq, [q, w, e, r, t, y, g]).
test(nfa_seq, [
				fail, 
				setup(nfa_re_compile(nfa_seq, seq(q, w, e, r, t, y))), 
				cleanup(nfa_clear_nfa(nfa_seq))
				]) :-
	nfa_recognize(nfa_seq, [q, w, e, f, r, t, y]).
test(nfa_seq, [
				setup(nfa_re_compile(nfa_seq, seq(q, w, e, r, t, y, 
											      or(z, x)))),
				cleanup(nfa_clear_nfa(nfa_seq))
				]) :-
	nfa_recognize(nfa_seq, [q, w, e, r, t, y, z]),
	nfa_recognize(nfa_seq, [q, w, e, r, t, y, x]).
test(nfa_seq, [
				setup(nfa_re_compile(nfa_seq, seq(or(q, w), star(e), 
									 star(seq(r, t, y))))), 
				cleanup(nfa_clear_nfa(nfa_seq))
				]) :-
	nfa_recognize(nfa_seq, [q]),
	nfa_recognize(nfa_seq, [w]),
	nfa_recognize(nfa_seq, [q, e, e, e, e]),
	nfa_recognize(nfa_seq, [w, e, e, e, e]),
	nfa_recognize(nfa_seq, [q, r, t, y, r, t, y]),
	nfa_recognize(nfa_seq, [w, r, t, y, r, t, y, r, t, y]),
	nfa_recognize(nfa_seq, [q, e, e, r, t, y, r, t, y, r, t, y, r, t, y, r, 
							t, y]),
	nfa_recognize(nfa_seq, 
			[w, e, e, e, e, r, t, y, r, t, y, r, t, y, r, t, y, r, t, y, r, 
			 t, y]).
test(nfa_seq, [
				fail, 
				setup(nfa_re_compile(nfa_seq, seq(or(q, w), star(e), 
									 star(seq(r, t, y))))), 
				cleanup(nfa_clear_nfa(nfa_seq))
				]) :-
	nfa_recognize(nfa_seq, []).
test(nfa_seq, [
				fail, 
				setup(nfa_re_compile(nfa_seq, seq(or(q, w), star(e), 
									 star(seq(r, t, y))))), 
				cleanup(nfa_clear_nfa(nfa_seq))
				]) :-
	nfa_recognize(nfa_seq, [q, w, e, r, t, y]).
test(nfa_seq, [
				fail, 
				setup(nfa_re_compile(nfa_seq, seq(or(q, w), star(e), 
									 star(seq(r, t, y))))), 
				cleanup(nfa_clear_nfa(nfa_seq))
				]) :-
	nfa_recognize(nfa_seq, [w, e, e, e, r, t]).
test(nfa_seq, [
				fail, 
				setup(nfa_re_compile(nfa_seq, seq(or(q, w), star(e), 
									 star(seq(r, t, y))))), 
				cleanup(nfa_clear_nfa(nfa_seq))
				]) :-
	nfa_recognize(nfa_seq, [e, e, r, t, y]).
test(nfa_seq, [
				fail, 
				setup(nfa_re_compile(nfa_seq, seq(or(q, w), star(e), 
									 star(seq(r, t, y))))), 
				cleanup(nfa_clear_nfa(nfa_seq))
				]) :-
	nfa_recognize(nfa_seq, [e]).
test(nfa_seq, [
				fail, 
				setup(nfa_re_compile(nfa_seq, seq(or(q, w), star(e), 
									 star(seq(r, t, y))))), 
				cleanup(nfa_clear_nfa(nfa_seq))
				]) :-
	nfa_recognize(nfa_seq, [r, t, y]).
test(nfa_seq, [
				setup(nfa_re_compile(nfa_seq, seq(ciao, star(ciao)))), 
				cleanup(nfa_clear_nfa(nfa_seq))
				]) :-
	nfa_recognize(nfa_seq, [ciao]),
	nfa_recognize(nfa_seq, [ciao, ciao, ciao]).
test(nfa_seq, [
				setup(nfa_re_compile(nfa_seq, seq(
					oneof(star(star(a)), seq(q, w, e, r, t, y), 
					or(c, d)),
					seq(z, x, c),
					star(or(test1, test2))))),
				cleanup(nfa_clear_nfa(nfa_seq))
				]):-
	nfa_recognize(nfa_seq, [z, x, c]),
	nfa_recognize(nfa_seq, [a, a, a, a, z, x, c, test1]),
	nfa_recognize(nfa_seq, [q, w, e, r, t, y, z, x, c, test1, test2, 
							test2, test1]),
	nfa_recognize(nfa_seq, [c, z, x, c]),
	nfa_recognize(nfa_seq, [d, z, x, c, test1, test1]).

test(nfa_seq, [
				setup(nfa_re_compile(nfa_seq, seq(a, a, a, a))),
				cleanup(nfa_clear_nfa(nfa_seq))
				]):- 
	not(nfa_recognize(nfa_seq, [a])),
	not(nfa_recognize(nfa_seq, [a, a])),
	not(nfa_recognize(nfa_seq, [a, a, a])),
	nfa_recognize(nfa_seq,[a, a, a, a]),
	not(nfa_recognize(nfa_seq, [a, a, a, a, a])),
	not(nfa_recognize(nfa_seq, [a, a, a, a, a, a, a])),
	not(nfa_recognize(nfa_seq, [a, a, a, a, a, a, a, a, a, a, a, a])),
	not(nfa_recognize(nfa_seq, [b])),
	not(nfa_recognize(nfa_seq, [b, b, b])),
	not(nfa_recognize(nfa_seq, [a, a, a, b])).

test(nfa_seq, [
				setup(nfa_re_compile(nfa_seq, seq(or(a, b), or(c, d), 
									 or(e, f)))),
				cleanup(nfa_clear_nfa(nfa_seq))
				]):- 
	nfa_recognize(nfa_seq, [a, c, e]),
	nfa_recognize(nfa_seq, [b, c, e]),
	nfa_recognize(nfa_seq, [a, d, e]),
	nfa_recognize(nfa_seq, [a, d, f]),
	nfa_recognize(nfa_seq, [b, d, f]),
	nfa_recognize(nfa_seq, [b, d, e]),
	nfa_recognize(nfa_seq, [b, c, f]),
	nfa_recognize(nfa_seq, [a, c, f]),
	not(nfa_recognize(nfa_seq, [c, a, f])).


test(nfa_seq, [
				setup(nfa_re_compile(nfa_seq, seq(a, a, a, a, a, a, 
				a, a, a, a, a, a, a, a, a, a, a, a, 
				a, a, a, a, a, a, a, a))),
				cleanup(nfa_clear_nfa(nfa_seq))
			]):-
	nfa_recognize(nfa_seq, [a, a, a, a, a, a, a, a, a, a, a, 
							a, a, a, a, 
							a, a, a, a, a, a, a, a, a, a, a]).


test(nfa_seq, [
				setup(nfa_re_compile(nfa_seq, seq(or(a, b), or(c, d), 
				or(e, f), or(g, h),	or(i, l), or(m, n), or(o, p), 
				or(q, r), or(s, t), or(u, v),or(w, x), or(y, z)))),
				cleanup(nfa_clear_nfa(nfa_seq))
				]):-
	nfa_recognize(nfa_seq, [a, c, e, g, i, m, o, q, s, u, w, y]).

:- end_tests(nfa_seq).

% Test unitari sul riconoscimento di una NFA derivato da una regexp or.
:- begin_tests(nfa_or).

test(nfa_or, [
				setup(nfa_re_compile(nfa_or, or(q, w))), 
				cleanup(nfa_clear_nfa(nfa_or))
				]) :-
	nfa_recognize(nfa_or, [q]).
test(nfa_or, [
				setup(nfa_re_compile(nfa_or, or(q, w))), 
				cleanup(nfa_clear_nfa(nfa_or))
				]) :-
	nfa_recognize(nfa_or, [w]).
test(nfa_or, [
				fail, 
				setup(nfa_re_compile(nfa_or, or(q, w))), 
				cleanup(nfa_clear_nfa(nfa_or))
				]) :-
	nfa_recognize(nfa_or, []).
test(nfa_or, [
				fail, 
				setup(nfa_re_compile(nfa_or, or(q, w))), 
				cleanup(nfa_clear_nfa(nfa_or))
				]) :-
	nfa_recognize(nfa_or, [ciao]).
test(nfa_or, [
				setup(nfa_re_compile(nfa_or, or(seq(q, w, e, r, t, y),
									 seq(u, i, o, p)))), 
				cleanup(nfa_clear_nfa(nfa_or))
				]) :-
	nfa_recognize(nfa_or, [q, w, e, r, t, y]).
test(nfa_or, [
				setup(nfa_re_compile(nfa_or, or(or(qwerty, uiop), 
									 seq(q, w, e, r, t, y)))), 
				cleanup(nfa_clear_nfa(nfa_or))
				]) :-
	nfa_recognize(nfa_or, [qwerty]).
test(nfa_or, [
				setup(nfa_re_compile(nfa_or, or(or(qwerty, uiop), 
									 seq(q, w, e, r, t, y)))), 
				cleanup(nfa_clear_nfa(nfa_or))
				]) :-
	nfa_recognize(nfa_or, [q, w, e, r, t, y]).
test(nfa_or, [
				setup(nfa_re_compile(nfa_or, or(or(qwerty, uiop), 
									 seq(q, w, e, r, t, y)))), 
				cleanup(nfa_clear_nfa(nfa_or))
				]) :-
	nfa_recognize(nfa_or, [uiop]).
test(nfa_or, [
				fail, 
				setup(nfa_re_compile(nfa_or, or(or(qwerty, uiop), 
									 seq(q, w, e, r, t, y)))), 
				cleanup(nfa_clear_nfa(nfa_or))
				]) :-
	nfa_recognize(nfa_or, [ciao]).
test(nfa_or, [
				setup(nfa_re_compile(nfa_or, or(a, b))),
				cleanup(nfa_clear_nfa(nfa_or))
			]) :-
	nfa_recognize(nfa_or, [a]),
	nfa_recognize(nfa_or, [b]),
	nfa_recognize(nfa_or, [a, epsilon, epsilon, epsilon]),
	nfa_recognize(nfa_or, [b, epsilon, epsilon]),
	not(nfa_recognize(nfa_or, [a, epsilon, epsilon, b])),
	not(nfa_recognize(nfa_or, [_])),
	not(nfa_recognize(nfa_or, _)),
	not(nfa_recognize(nfa_or, [c])),
	not(nfa_recognize(nfa_or, [d])),
	not(nfa_recognize(nfa_or, [a, a])),
	not(nfa_recognize(nfa_or, [a, b])),
	not(nfa_recognize(nfa_or, [b, a])),
	not(nfa_recognize(nfa_or, [b, b])),
	not(nfa_recognize(nfa_or, [epsilon])),
	not(nfa_recognize(nfa_or, [a, a, a])).
test(nfa_or, [
				setup(nfa_re_compile(nfa_or, or(or(a, b), or(c, d)))),
				cleanup(nfa_clear_nfa(nfa_or))
			]) :-
	nfa_recognize(nfa_or, [a]),
	nfa_recognize(nfa_or, [b]),
	nfa_recognize(nfa_or, [c]),
	nfa_recognize(nfa_or, [d]),
	not(nfa_recognize(nfa_or, [a, b])),
	not(nfa_recognize(nfa_or, [e])),
	not(nfa_recognize(nfa_or, [f])),
	not(nfa_recognize(nfa_or, [epsilon])),
	not(nfa_recognize(nfa_or, [])),
	not(nfa_recognize(nfa_or, [_])),
	not(nfa_recognize(nfa_or, [_, _])),
	not(nfa_recognize(nfa_or, [a, d])),
	not(nfa_recognize(nfa_or, [b, b])),
	not(nfa_recognize(nfa_or, [b, c])),
	not(nfa_recognize(nfa_or, [a, a])),
	not(nfa_recognize(nfa_or, [b, d])).

test(nfa_or, [			
				setup(nfa_re_compile(nfa_or, or(or(or(
						or(or(a, b), c), d), e),or(or(or(
						or(or(or(g, h), i), l), m), n), o)))),
				cleanup(nfa_clear_nfa(nfa_or))
			]):-
	nfa_recognize(nfa_or, [a]),
	nfa_recognize(nfa_or, [b]),
	nfa_recognize(nfa_or, [c]),
	nfa_recognize(nfa_or, [d]),
	nfa_recognize(nfa_or, [e]),
	not(nfa_recognize(nfa_or, [f])),
	nfa_recognize(nfa_or, [g]),
	nfa_recognize(nfa_or, [h]),
	nfa_recognize(nfa_or, [i]),
	nfa_recognize(nfa_or, [l]),
	nfa_recognize(nfa_or, [m]),
	nfa_recognize(nfa_or, [n]),
	nfa_recognize(nfa_or, [o]),
	not(nfa_recognize(nfa_or, [a, b, c])),
	not(nfa_recognize(nfa_or, [p])),
	not(nfa_recognize(nfa_or, [_])),
	not(nfa_recognize(nfa_or, [_])),
	not(nfa_recognize(nfa_or, [a, a, a])),
	not(nfa_recognize(nfa_or, [c, a])).
test(nfa_or, [
				setup(nfa_re_compile(nfa_or, oneof(oneof(a, b, c, d, e),
					oneof(f, g, h, i, l), oneof(m, n, o, p, q, r), 
					oneof(s, t, u, v, y, z), or(w, x), oneof(or(1, 2), 
					or(3, 4), or(5, 6),
					or(7, 8), or(9, 0))))),
				cleanup(nfa_clear_nfa(nfa_or))
			]) :-
	nfa_recognize(nfa_or, [a]),
	nfa_recognize(nfa_or, [b]),
	nfa_recognize(nfa_or, [c]),
	nfa_recognize(nfa_or, [d]),
	nfa_recognize(nfa_or, [e]),
	nfa_recognize(nfa_or, [f]),
	nfa_recognize(nfa_or, [g]),
	nfa_recognize(nfa_or, [h]),
	nfa_recognize(nfa_or, [i]),
	nfa_recognize(nfa_or, [l]),
	nfa_recognize(nfa_or, [m]),
	nfa_recognize(nfa_or, [n]),
	nfa_recognize(nfa_or, [o]),
	nfa_recognize(nfa_or, [p]),
	nfa_recognize(nfa_or, [r]),
	nfa_recognize(nfa_or, [s]),
	nfa_recognize(nfa_or, [t]),
	nfa_recognize(nfa_or, [u]),
	nfa_recognize(nfa_or, [v]),
	nfa_recognize(nfa_or, [z]),
	nfa_recognize(nfa_or, [1]),
	nfa_recognize(nfa_or, [2]),
	nfa_recognize(nfa_or, [3]),
	nfa_recognize(nfa_or, [4]),
	nfa_recognize(nfa_or, [5]),
	nfa_recognize(nfa_or, [6]),
	nfa_recognize(nfa_or, [7]),
	nfa_recognize(nfa_or, [8]),
	nfa_recognize(nfa_or, [9]),
	nfa_recognize(nfa_or, [0]),
	nfa_recognize(nfa_or, [x]),
	nfa_recognize(nfa_or, [y]),
	nfa_recognize(nfa_or, [w]).

:- end_tests(nfa_or).

% Test unitari sul riconoscimento di un NFA derivato da una regexp star.
:- begin_tests(nfa_star).

test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(a))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, []).
test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(a))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, [a]).
test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(a))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, [a, a, a, a, a]).
test(nfa_star, [
				fail, 
				setup(nfa_re_compile(nfa_star, star(a))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, [aa]).
test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(star(a)))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, []).
test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(star(a)))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, [a]).
test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(star(a)))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, [a, a, a, a]).
test(nfa_star, [
				fail, 
				setup(nfa_re_compile(nfa_star, star(star(a)))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, [aa]).
test(nfa_star, [
				fail, 
				setup(nfa_re_compile(nfa_star, star(star(
									 seq(q, w, e, r, t, y))))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, [q, w, s, e, r, f, t, y]).
test(nfa_star, [
				fail, 
				setup(nfa_re_compile(nfa_star, star(star(
											   or(test1, test2))))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, [test3]).
test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(star(
													seq(q, w, e, r, t, y)
												)))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, []),
	nfa_recognize(nfa_star, [q, w, e, r, t, y]),
	nfa_recognize(nfa_star, [q, w, e, r, t, y, q, w, e, 
							 r, t, y, q, w, e, r, t, y]).
test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(
										       seq(q, w, e, r, t, y)
									))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, []).
test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(
											   seq(q, w, e, r, t, y)
								    ))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, [q, w, e, r, t, y]).
test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(
											   seq(q, w, e, r, t, y)
								    ))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, [q, w, e, r, t, y, q, w, e, r, t, y, q, 
							 w, e, r, t, y]).
test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(or(
											   seq(q, w, e, r, t, y), 
									 ciao)))), 
				cleanup(nfa_clear_nfa(nfa_star))
				]) :-
	nfa_recognize(nfa_star, [ciao, ciao]).
test(nfa_star, [setup(nfa_re_compile(nfa_star, star(oneof(star(a), 
									 seq(q, w, e, r, t, y), 
									 or(c, d))))),
				cleanup(nfa_clear_nfa(nfa_star))]) :-
	nfa_recognize(nfa_star, []),
	nfa_recognize(nfa_star, [a]),
	nfa_recognize(nfa_star, [a, a, a, a, a, a]),
	nfa_recognize(nfa_star, [q, w, e, r, t, y]),
	nfa_recognize(nfa_star, [c]),
	nfa_recognize(nfa_star, [d]).

test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(a))),
				cleanup(nfa_clear_nfa(nfa_star))
				]):- 
	nfa_recognize(nfa_star, [epsilon]),
	nfa_recognize(nfa_star, [a]),
	nfa_recognize(nfa_star, [a, a]),
	nfa_recognize(nfa_star, [a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a, a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a, a, a, a, a, a, a, a, a]),
	not(nfa_recognize(nfa_star, [b])),
	nfa_recognize(nfa_star, [epsilon]).

test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(or(a, b)))),
				cleanup(nfa_clear_nfa(nfa_star))
				]):- 
	nfa_recognize(nfa_star, [epsilon]),
	nfa_recognize(nfa_star, [a]),
	nfa_recognize(nfa_star, [a, a]),
	nfa_recognize(nfa_star, [a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a, a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a, a, a, a, a, a, a, a, a]),
	nfa_recognize(nfa_star, [b]),
	nfa_recognize(nfa_star, [b, b]),
	nfa_recognize(nfa_star, [b, b, b]),
	nfa_recognize(nfa_star, [b, b, b, b]),
	nfa_recognize(nfa_star, [b, b, b, b, b]),
	nfa_recognize(nfa_star, [b, b, b, b, b, b]),
	nfa_recognize(nfa_star, [b, b, b, b, b, b, b, b, b, b, b, b]),
	nfa_recognize(nfa_star, [b, a, a, b, a]),
	nfa_recognize(nfa_star, [a, a, a, a, b]),
	nfa_recognize(nfa_star, [b, b, b, b, b, b, a]),
	not(nfa_recognize(nfa_star, [b, b, b, b, b, c])),
	not(nfa_recognize(nfa_star, [b, b, b, b, b, a, a, c])),
	not(nfa_recognize(nfa_star, [c, a, a, a])),
	not(nfa_recognize(nfa_star, [c])),
	not(nfa_recognize(nfa_star, [_])).

test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(epsilon))),
				cleanup(nfa_clear_nfa(nfa_star))
				]):-
	nfa_recognize(nfa_star, [epsilon]),
	nfa_recognize(nfa_star, []),
	not(nfa_recognize(nfa_star, [a])),
	not(nfa_recognize(nfa_star, [b])),
	not(nfa_recognize(nfa_star, [b, b])),
	not(nfa_recognize(nfa_star, [b, b, b])),
	not(nfa_recognize(nfa_star, [b, b, b, b])),
	not(nfa_recognize(nfa_star, [b, b, b, b, b])),
	not(nfa_recognize(nfa_star, [b, b, b, b, b, b])),
	not(nfa_recognize(nfa_star, [b, b, b, b, b, b, b, b, b, b, b, b])),
	not(nfa_recognize(nfa_star, [b, a, a, b, a])),
	not(nfa_recognize(nfa_star, [a, a, a, a, b])),
	not(nfa_recognize(nfa_star, [b, b, b, b, b, b, a])).
test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(star(star(a))))),
				cleanup(nfa_clear_nfa(nfa_star))
				]):- 
	nfa_recognize(nfa_star, [epsilon]),
	nfa_recognize(nfa_star, [a]),
	nfa_recognize(nfa_star, [a, a]),
	nfa_recognize(nfa_star, [a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a, a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a, a, a, a, a, a, a, a, a]).
test(nfa_star,[
			fail,		
			setup(nfa_re_compile(nfa_star, 
								star(star(star(star(
									 star(star(star(a))))))))),
			cleanup(nfa_clear_nfa(nfa_star))
			]):- 
	nfa_recognize(nfa_star,[b]).
test(nfa_star, [
				setup(nfa_re_compile(nfa_star, star(star(star(
												star(or(a, b))))))),
				cleanup(nfa_clear_nfa(nfa_star))
				]):- 
	nfa_recognize(nfa_star, [epsilon]),
	nfa_recognize(nfa_star, [a]),
	nfa_recognize(nfa_star, [a, a]),
	nfa_recognize(nfa_star, [a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a, a, a, a]),
	nfa_recognize(nfa_star, [a, a, a, a, a, a, a, a, a, a, a, a]),
	nfa_recognize(nfa_star, [b]),
	nfa_recognize(nfa_star, [b, b]),
	nfa_recognize(nfa_star, [b, b, b]),
	nfa_recognize(nfa_star, [b, b, b, b]),
	nfa_recognize(nfa_star, [b, b, b, b, b]),
	nfa_recognize(nfa_star, [b, b, b, b, b, b]),
	nfa_recognize(nfa_star, [b, b, b, b, b, b, b, b, b, b, b, b]),
	nfa_recognize(nfa_star, [b, a, a, b, a]),
	nfa_recognize(nfa_star, [a, a, a, a, b]),
	nfa_recognize(nfa_star, [b, b, b, b, b, b, a]),
	not(nfa_recognize(nfa_star, [b, b, b, b, b, c])),
	not(nfa_recognize(nfa_star, [b, b, b, b, b, a, a, c])),
	not(nfa_recognize(nfa_star, [c, a, a, a])),
	not(nfa_recognize(nfa_star, [c])),
	not(nfa_recognize(nfa_star, [_])).

:- end_tests(nfa_star).

% Test unitari sul riconoscimento di un NFA derivato da una regexp oneof.
:- begin_tests(nfa_oneof).

test(nfa_oneof, [
				setup(nfa_re_compile(nfa_oneof, oneof(q, w, e, r, t, y))), 
				cleanup(nfa_clear_nfa(nfa_oneof))
				]) :-
	nfa_recognize(nfa_oneof, [q]),
	nfa_recognize(nfa_oneof, [w]),
	nfa_recognize(nfa_oneof, [e]),
	nfa_recognize(nfa_oneof, [r]),
	nfa_recognize(nfa_oneof, [t]),
	nfa_recognize(nfa_oneof, [y]).
test(nfa_oneof, [
				setup(nfa_re_compile(nfa_oneof, oneof(q, star(w), 
												or(e, r), seq(t, y)))), 
				cleanup(nfa_clear_nfa(nfa_oneof))
				]) :-
	nfa_recognize(nfa_oneof, []),
	nfa_recognize(nfa_oneof, [q]),
	nfa_recognize(nfa_oneof, [r]),
	nfa_recognize(nfa_oneof, [w]),
	nfa_recognize(nfa_oneof, [w, w, w, w, w, w, w, w, w, w, w, w]),
	nfa_recognize(nfa_oneof, [t, y]).
test(nfa_oneof, [
				fail, 
				setup(nfa_re_compile(nfa_oneof, oneof(q, w, e, r, t, y))), 
				cleanup(nfa_clear_nfa(nfa_oneof))
				]) :-
	nfa_recognize(nfa_oneof, []).
test(nfa_oneof, [
				setup(nfa_re_compile(nfa_oneof, oneof(seq(q, w, e), 
												oneof(r, t, y)))), 
				cleanup(nfa_clear_nfa(nfa_oneof))
				]) :-
	nfa_recognize(nfa_oneof, [q, w, e]),
	nfa_recognize(nfa_oneof, [r]),
	nfa_recognize(nfa_oneof, [t]),
	nfa_recognize(nfa_oneof, [y]).
test(nfa_oneof, [
				setup(nfa_re_compile(nfa_oneof, oneof(oneof(q, w, e), 
												oneof(r, t, y), 
									 			seq(a, s, d), 
												or(ciao, test)))), 
				cleanup(nfa_clear_nfa(nfa_oneof))
				]) :-
	nfa_recognize(nfa_oneof, [q]),
	nfa_recognize(nfa_oneof, [w]),
	nfa_recognize(nfa_oneof, [e]),
	nfa_recognize(nfa_oneof, [r]),
	nfa_recognize(nfa_oneof, [t]),
	nfa_recognize(nfa_oneof, [y]),
	nfa_recognize(nfa_oneof, [a, s, d]),
	nfa_recognize(nfa_oneof, [ciao]),
	nfa_recognize(nfa_oneof, [test]).
test(nfa_oneof, [fail, 
				setup(nfa_re_compile(nfa_oneof, oneof(
												seq(q, w, e, r, t, y), 
												or(ciao, test)
									))), 
				cleanup(nfa_clear_nfa(nfa_oneof))
				]) :-
	nfa_recognize(nfa_oneof, [x]).
test(nfa_oneof, [
				fail, 
				setup(nfa_re_compile(nfa_oneof, oneof(
												seq(q, w, e, r, t, y), 
												or(ciao, test)
									))), 
				cleanup(nfa_clear_nfa(nfa_oneof))
				]) :-
	nfa_recognize(nfa_oneof, [ciao, test]).
test(nfa_oneof, [
				fail, 
				setup(nfa_re_compile(nfa_oneof, oneof(
												seq(q, w, e, r, t, y), 
												or(ciao, test)
									))), 
				cleanup(nfa_clear_nfa(nfa_oneof))
				]) :-
	nfa_recognize(nfa_oneof, [q, w, e, r, t, y, ciao]).
test(nfa_oneof, [
				setup(nfa_re_compile(nfa_oneof, oneof(bar(a), a))),
				cleanup(nfa_clear_nfa(nfa_oneof))
				]) :-
	nfa_recognize(nfa_oneof, [a]),
	nfa_recognize(nfa_oneof, [aa]),
	nfa_recognize(nfa_oneof, [q, w, e, r, t, y]),
	nfa_recognize(nfa_oneof, [b]),
	nfa_recognize(nfa_oneof, []).
test(nfa_oneof, [
				setup(nfa_re_compile(nfa_oneof, oneof(a, b, c, d))),
				cleanup(nfa_clear_nfa(nfa_oneof))
]):- 
	not(nfa_recognize(nfa_oneof, [a, l])),
	not(nfa_recognize(nfa_oneof, [a, a])),
	nfa_recognize(nfa_oneof, [a]),
	nfa_recognize(nfa_oneof, [b]),
	nfa_recognize(nfa_oneof, [c]),
	nfa_recognize(nfa_oneof, [d]).

test(nfa_oneof, [
				setup(nfa_re_compile(nfa_oneof, oneof(star(a), 
												or(b, c),
												seq(d, c, e), 
												star(x)
									))),
				cleanup(nfa_clear_nfa(nfa_oneof))
]):-
	not(nfa_recognize(nfa_oneof, [a, l])),
	nfa_recognize(nfa_oneof, [a, a]),
	nfa_recognize(nfa_oneof, [a]),
	nfa_recognize(nfa_oneof, [b]),
	nfa_recognize(nfa_oneof, [c]),
	nfa_recognize(nfa_oneof, [d, c, e]),
	not(nfa_recognize(nfa_oneof, [d])),
	not(nfa_recognize(nfa_oneof, [d, c])),
	nfa_recognize(nfa_oneof, [x, x, x, x, x, x, x, x, x, x, x]),
	nfa_recognize(nfa_oneof, [x]),
	nfa_recognize(nfa_oneof, [epsilon]),
	nfa_recognize(nfa_oneof, [x, x, x]),
	nfa_recognize(nfa_oneof, [x, x]).

test(nfa_oneof, [
				setup(nfa_re_compile(nfa_oneof, oneof(oneof(star(k), 
									or(or(x,y),or(a,b))), 
									or(star(j), 
									star(seq(a,a)))))),
				cleanup(nfa_clear_nfa(nfa_oneof))
]):-
	nfa_recognize(nfa_oneof, [k, k, k, k, k]),
	nfa_recognize(nfa_oneof, [epsilon]),
	nfa_recognize(nfa_oneof, [x]),
	not(nfa_recognize(nfa_oneof, [x, x, x, x])),
	nfa_recognize(nfa_oneof, [a]),
	not(nfa_recognize(nfa_oneof, [a, a, a])),
	nfa_recognize(nfa_oneof, [a, a]),
	nfa_recognize(nfa_oneof, [a, a, a, a]),
	nfa_recognize(nfa_oneof, [j, j, j, j]).

:- end_tests(nfa_oneof).

% Test unitari sul riconoscimento di un NFA derivato da una regexp plus.
:- begin_tests(nfa_plus).

test(nfa_plus, [
				fail, 
				setup(nfa_re_compile(nfa_plus, plus(a))), 
				cleanup(nfa_clear_nfa(nfa_plus))
				]) :-
	nfa_recognize(nfa_plus, []).
test(nfa_plus, [
				fail, 
				setup(nfa_re_compile(nfa_plus, plus(a))), 
				cleanup(nfa_clear_nfa(nfa_plus))
				]) :-
	nfa_recognize(nfa_plus, [aa]).
test(nfa_plus, [
				setup(nfa_re_compile(nfa_plus, plus(a))), 
				cleanup(nfa_clear_nfa(nfa_plus))
				]) :-
	nfa_recognize(nfa_plus, [a]).
test(nfa_plus, [
				setup(nfa_re_compile(nfa_plus, plus(a))), 
				cleanup(nfa_clear_nfa(nfa_plus))
				]) :-	
	nfa_recognize(nfa_plus, [a, a, a, a, a]).
test(nfa_plus, [
				setup(nfa_re_compile(nfa_plus, plus(
											   seq(q, w, e, r, t, y)
								    ))), 
				cleanup(nfa_clear_nfa(nfa_plus))
				]) :-
	nfa_recognize(nfa_plus, [q, w, e, r, t, y, q, w, e, r, t, y]).
test(nfa_plus, [
				setup(nfa_re_compile(nfa_plus, plus(or(uno, due)))), 
				cleanup(nfa_clear_nfa(nfa_plus))
				]) :-
	nfa_recognize(nfa_plus, [uno, uno, due, uno, due]).
test(nfa_plus, [
				fail, 
				setup(nfa_re_compile(nfa_plus, plus(plus(a)))), 
				cleanup(nfa_clear_nfa(nfa_plus))
				]) :-
	nfa_recognize(nfa_plus, []).
test(nfa_plus, [
				setup(nfa_re_compile(nfa_plus, plus(plus(star(a))))), 
				cleanup(nfa_clear_nfa(nfa_plus))
				]) :-
	nfa_recognize(nfa_plus, []),
	nfa_recognize(nfa_plus, [a]),
	nfa_recognize(nfa_plus, [a, a, a, a, a, a]).
test(nfa_plus, [
				setup(nfa_re_compile(nfa_plus, plus(ciao))), 
				cleanup(nfa_clear_nfa(nfa_plus))
				]) :-
	nfa_recognize(nfa_plus, [ciao, ciao, ciao]).
test(nfa_plus, [
				setup(nfa_re_compile(nfa_plus, plus(oneof(star(star(a)), 
													seq(q, w, e, r, t, y), 
													plus(x))))), 
				cleanup(nfa_clear_nfa(nfa_plus))
				]) :-
	nfa_recognize(nfa_plus, [x]),
	nfa_recognize(nfa_plus, []),
	nfa_recognize(nfa_plus, [a, a, q, w, e, r, t, y, 
							 a, q, w, e, r, t, y, 
							 a, a, a, x, x]).

:- end_tests(nfa_plus).

% Test unitari sul riconoscimento di un NFA generato da una regexp bar.
:- begin_tests(bar).

test(nfa_bar, [
				setup(nfa_re_compile(nfa_bar, bar(a))),
				cleanup(nfa_clear_nfa(nfa_bar))
			  ]) :-
	nfa_recognize(nfa_bar, []),
	nfa_recognize(nfa_bar, [q, w, e, r, t, y]),
	nfa_recognize(nfa_bar, [qwerty]),
	nfa_recognize(nfa_bar, [epsilon]).

test(nfa_bar, [
				setup(nfa_re_compile(nfa_bar, bar(or(a, b)))),
				cleanup(nfa_clear_nfa(nfa_bar))
				]):- 
	nfa_recognize(nfa_bar, [b, a, a]),
	nfa_recognize(nfa_bar, [b, b, b, b, a]),
	nfa_recognize(nfa_bar, [b, b, b, b, b, b, b, a, a, ac]),
	nfa_recognize(nfa_bar, [a, a, a]),
	nfa_recognize(nfa_bar, [12, 23]),
	not(nfa_recognize(nfa_bar, [_])),
	not(nfa_recognize(nfa_bar, [a])),
	not(nfa_recognize(nfa_bar, [b])).

test(nfa_bar, [
				setup(nfa_re_compile(nfa_bar, bar(star(a)))),
				cleanup(nfa_clear_nfa(nfa_bar))
				]):-
	nfa_recognize(nfa_bar, [b]),
	nfa_recognize(nfa_bar, [b, b]),
	nfa_recognize(nfa_bar, [b, b, b]),
	nfa_recognize(nfa_bar, [b, b, b, b]),
	nfa_recognize(nfa_bar, [b, b, b, b, b]),
	nfa_recognize(nfa_bar, [b, b, b, b, b, b]),
	nfa_recognize(nfa_bar, [b, b, b, b, b, b, b, b, b, b, b, b]),
	nfa_recognize(nfa_bar, [b, a, a, b, a]),
	nfa_recognize(nfa_bar, [a, a, a, a, b]),
	nfa_recognize(nfa_bar, [b, b, b, b, b, b, a]),
	nfa_recognize(nfa_bar, [b, b, b, b, b, c]),
	nfa_recognize(nfa_bar, [b, b, b, b, b, a, a, c]),
	not(nfa_recognize(nfa_bar, [a])).

test(nfa_bar, [
				setup(nfa_re_compile(nfa_bar, star(bar(a)))),
				cleanup(nfa_clear_nfa(nfa_bar))
			]):- 
	nfa_recognize(nfa_bar, [b, b, b, b, b, b]),
	nfa_recognize(nfa_bar, [c, c, c, c, c, c]),
	not(nfa_recognize(nfa_bar, [a])),
	nfa_recognize(nfa_bar,[epsilon]).

test(nfa_bar, [
				setup(nfa_re_compile(nfa_bar, bar(star(or(a, b))))),
				cleanup(nfa_clear_nfa(nfa_bar))
			]):-
	not(nfa_recognize(nfa_bar, [a, a, a, b, b, b])),
	not(nfa_recognize(nfa_bar, [a])),
	not(nfa_recognize(nfa_bar, [b, b, b, b])),
	not(nfa_recognize(nfa_bar, [a, b, b, b, a])),
	not(nfa_recognize(nfa_bar, [b])),
	nfa_recognize(nfa_bar, [a, b, b, b, c]),
	nfa_recognize(nfa_bar, [c, c, c, c]).

:- end_tests(bar).

% Test di stress del programma
:- begin_tests(stress_tests).

test(nfa_stress, [
					setup(nfa_re_compile(nfa_stress, oneof(star(or(a, b)),
						seq(j, j, or(e, f)), star(star(star(l))), 
						plus(seq(oneof(q, w, e), bar, plus(s))), 
						or(seq(x, y), seq(1, 2))))),
					cleanup(nfa_clear_nfa(nfa_stress))
]):-
	nfa_recognize(nfa_stress, [a, a, a, a]),
	nfa_recognize(nfa_stress, []),
	nfa_recognize(nfa_stress, [epsilon]),
	nfa_recognize(nfa_stress, [a, b, b, b, b, b, a]),
	nfa_recognize(nfa_stress, [j, j, e]),
	nfa_recognize(nfa_stress, [j, j, f]),
	nfa_recognize(nfa_stress, [l, l, l, l, l, l, l, l, l, l, l, l]),
	nfa_recognize(nfa_stress, [q, bar, s]),
	nfa_recognize(nfa_stress, [q, bar, s, w, bar, s, s]),
	nfa_recognize(nfa_stress, [x, y]),
	nfa_recognize(nfa_stress, [1, 2]),
	not(nfa_recognize(nfa_stress, [x, 1])),
	not(nfa_recognize(nfa_stress, [q, bar, epsilon])),
	not(nfa_recognize(nfa_stress, [j, epsilon, e])),
	not(nfa_recognize(nfa_stress, [l, l, l, a])).


:- end_tests(stress_tests).

:- begin_tests(var_recognize).

setup_seq :-
	nfa_re_compile(1, seq(q, w, e, r, t, y)),
	nfa_re_compile(2, seq(q, w, e, r, t, y)),
	nfa_re_compile(3, seq(seq(q, w), e, or(r, v), or(m, t), y)).

cleanup_seq :-
	nfa_clear_nfa(1),
	nfa_clear_nfa(2),
	nfa_clear_nfa(3).

test(recognize, [
	 	setup(
				setup_seq
			),
		cleanup(cleanup_seq)
	 ]) :-
	findall(FA_Id, nfa_recognize(FA_Id, [q, w, e, r, t, y]), FA_Ids),
	FA_Ids == [1, 2, 3].

setup_seq_fail :-
	nfa_re_compile(1, seq(q, w, e, r, t, y)),
	nfa_re_compile(2, seq(q, w, s, r, c, y)),
	nfa_re_compile(3, seq(seq(q, w), e, or(z, v), or(m, t), y)).

cleanup_seq_fail :-
	nfa_clear_nfa(1),
	nfa_clear_nfa(2),
	nfa_clear_nfa(3).

test(recognize, [fail, 
	 	setup(
				setup_seq_fail
			),
		cleanup(cleanup_seq_fail)
	 ]) :-
	findall(FA_Id, nfa_recognize(FA_Id, [q, w, e, r, t, y]), FA_Ids),
	FA_Ids == [1, 2, 3].

setup_oneof :-
	nfa_re_compile(1, oneof(star(a), plus(b), star(
							seq(q, w, e, r, t, y)))),
	nfa_re_compile(2, oneof(or(a, aa), or(b, bb), or(c, cc))),
	nfa_re_compile(3, oneof(plus(a), b, c, 
							oneof(test1, or(test2, test3)))).

cleanup_oneof :-
	nfa_clear_nfa(1),
	nfa_clear_nfa(2),
	nfa_clear_nfa(3),
	nfa_clear_nfa(4).

test(recognize, [fail, 
	 	setup(
				setup_seq_fail
			),
		cleanup(cleanup_seq_fail)
	 ]) :-
	findall(FA_Id, nfa_recognize(FA_Id, [a]), FA_Ids),
	FA_Ids == [1, 2, 3, 4].

:- end_tests(var_recognize).

