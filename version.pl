divides(A,B) :- A =\= 0, (B mod A) =:= 0.
factor_list(S,E,L) :- S > E // 2, !, L=[E].
factor_list(S,E,L) :- divides(S,E), !, L=[S|T], S1 is S+1, factor_list(S1, E, T).
factor_list(S,E,L) :- S1 is S+1, factor_list(S1,E,L).
factor_list(S,E,L) :- E is 1,S is 1, L=[].

possible_zeros(LC, [], []).
possible_zeros(LC, [H1|T1],[H2|T2]) :-
	H2 is LC / H1,
	possible_zeros(LC, T1, T2).
	
negative_pz(LC, [], []).
negative_pz(LC, [H1|T1],[H2|T2]) :-
	H2 is -1 * LC / H1,
	negative_pz(LC, T1, T2).

aLL([],_,[]).
aLL([LCH|LCT],[CFH|CFT],[H2|T2]) :-
	possible_zeros(LCH,[CFH|CFT],List1),
	H2 = List1,
	aLL(LCT,[CFH|CFT],T2).

aLLN([],[]).
aLLN([LCH|LCT],[H2|T2]) :-
	H2 is LCH * -1,
	aLLN(LCT, T2).
	

square(x1).
linear(x2).
constant(x3).

polynomial(1,1,1).
polynomial(1,2,1).
polynomial(1,1,0).
polynomial(-5,5,0).
polynomial(0,-10,2).
polynomial(-27,3,9).
polynomial(-8,2,6).
polynomial(17,0,-17).
polynomial(15,5,-5).
polynomial(1,124,-512).
polynomial(5,-10, 5).
polynomial(0,7,-21).
polynomial(1,-2,1).


evaluate(Value_at, polynomial(C2, C1, Const), X) :-
	value2(C2, Value_at, Total),
	value1(C1, Value_at, Total2),
	X is float(round(Total + Total2 + Const)).

value2(C2, Value_at, Total) :-
	Total is float(Value_at * Value_at * C2).

value1(C1, Value_at, Total) :-
	 Total is float(Value_at * C1).

isFactor(Polynomial, X) :-
	evaluate(X, Polynomial, Total),
	Total is 0.0.

factors(Polynomial, [],[],[]).
factors(Polynomial,[H1|T1], [H1|T2],N) :-
	evaluate(H1,Polynomial,Total),
	Total is 0.0,
	factors(Polynomial,T1,T2,N).

factors(Polynomial,[H1|T1], P,[H1|T2]) :-
	evaluate(H1,Polynomial,Total),
	Total =\= 0,
	factors(Polynomial,T1,P,T2).

degree(polynomial(A,B,C), X) :-
	A =\= 0,X is 2. 
degree(polynomial(A,B,C), X) :-
	A is 0,B =\= 0,X is 1. 
degree(polynomial(A,B,C), X) :-
	A is 0,B is 0,C =\= 0,X is 0.  
degree(polynomial(A,B,C), X) :-
	A is 0,B is 0,C is 0,
	X = "negative infinity".  
 
genList(polynomial(A,B,C), List) :-
	C =\= 0, A =\= 0,
	LC is abs(A), Const is abs(C),
	factor_list(1,Const,L),
	factor_list(1,LC,L2),
	aLL(L,L2,Special_list),
	flatten(Special_list, Final),
	aLLN(Final,Final2),
	append(Final,Final2, List).

genList(polynomial(A,B,C), List):-
	C is 0, A is 0, B is 0,
	List = [].
genList(polynomial(A,B,C), List):-
	C is 0, A =\= 0 , B is 0,
	List = [0].
genList(polynomial(A,B,C), List):-
	C is 0, A is 0, B=\=0,
	List = [0].
genList(polynomial(A,B,C),List):-
	A is 0, B=\=0, C=\=0,
	LC is abs(B), CT is abs(C),
	factor_list(1,LC,L),
	factor_list(1,CT,L1),
	aLL(L1,L,Special_list),
	flatten(Special_list, Final),
	aLLN(Final, Final2),
	append(Final,Final2,List).

genList(polynomial(A,B,C), List):-
	C is 0, A=\=0, B=\=0,
	LC is abs(A), NT is abs(B),
	factor_list(1,LC,L),
	factor_list(1,NT,L1),
	aLL(L1,L,Special_list),
	flatten(Special_list,Final),
	aLLN(Final,Final2),
	append(Final, Final2, Almost),
	append(Almost,[0], List).

genFactors(Polynomial, Factors) :-
	genList(Polynomial, [H1|T1]),
	append([H1], T1, List),
	factors(Polynomial,List,Factors,_).

findFactors(L):-
	polynomial(X,Y,Z),
	genFactors(polynomial(X,Y,Z),L1),
	append([polynomial(X,Y,Z)], L1, L). 