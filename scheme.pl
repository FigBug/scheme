/* Symbol Table - all the built in functions*/

st(pLus,bi).
st(mInus,bi).
st(mUlt,bi).
st(dIv,bi).
st(gT,bi).
st(gTeQ,bi).
st(lT,bi).
st(lTeQ,bi).
st(eQ,bi).
st(eqQ,bi).
st(zeroQ,bi).
st(listQ,bi).
st(nullQ,bi).
st(numberQ,bi).
st(symbolQ,bi).
st(procedureQ,bi).

st(quOte,bi).
st(quote,bi).
st(list,bi).
st(or,bi).
st(and,bi).
st(car,bi).
st(cons,bi).
st(cdr,bi).
st(cond,bi).

st(else,bi).

isBuiltIn(X) :- st(X,Y),!, Y = bi.

/* All types of output functions */

output(X) :- nl,write(';value: '),writeData(X),nl,nl.

writeData(X) :- var(X),write('undefined').
writeData([lIst|L]) :- write('('),outList(L),write(')').
writeData([sYm,X]) :- write(X).
writeData(X) :- number(X),write(X).
writeData(hAsht) :- write('#t').
writeData(hAshf) :- write('()').
writeData(X) :- isBuiltIn(X), write('built in procedure').
writeData(added) :- write('symbol table updated').
writeData(error) :- write('error').
writeData([lambda|_]) :- write('user defined procedure').
writeData([_|_]) :- write('error').
writeData(_) :- write('undefined').

outList([]).
outList([X|[]]) :- writeData(X).
outList([X|L]) :- writeData(X),write(' '),outList(L).

/* Built in functions */

builtIn(pLus, Z, [L|R]) :- Z is L + R.
builtIn(mInus, Z, [L|R]) :- Z is L - R.
builtIn(mUlt, Z, [L|R]) :- Z is L * R.
builtIn(dIv, Z, [L|R]) :- Z is L / R.
builtIn(gT, Z, [L|R]) :- L > R, Z = hAsht.
builtIn(gT, Z, _) :- Z = hAshf.
builtIn(gTeQ, Z, [L|R]) :- (L > R; L is R), Z = hAsht.
builtIn(gTeQ, Z, _) :- Z = hAshf.
builtIn(lT, Z, [L|R]) :- L < R, Z = hAsht.
builtIn(lT, Z, _) :- Z = hAshf.
builtIn(lTeQ, Z, [L|R]) :- (L < R; L is R), Z = hAsht.
builtIn(lTeQ, Z, _) :- Z = hAshf.

builtIn(eQ, Z, L) :- eqList(Z, L).
builtIn(eQ, Z, _) :- Z = hAshf.

builtIn(eqQ, Z, [[sYm,A],[sYm,A]]) :- Z = hAsht.
builtIn(eqQ, Z, [[lIst|[]],[lIst|[]]]) :- Z = hAsht.
builtIn(eqQ, Z, [X,X]) :- Z = hAsht.
builtIn(eqQ, Z, _) :- Z = hAshf.
builtIn(zeroQ, Z, [X]) :- (X = 0, Z = hAsht);Z = hAshf.

builtIn(listQ, Z, [[lIst|_]]) :- Z = hAsht.
builtIn(listQ, Z, _) :- Z = hAshf.
builtIn(nullQ, Z, [[lIst|[]]]) :- Z = hAsht.
builtIn(nullQ, Z, _) :- Z = hAshf.
builtIn(numberQ, Z, [L]) :- number(L),Z = hAsht.
builtIn(numberQ, Z, _) :- Z = hAshf.
builtIn(symbolQ, Z, [[sYm,_]]) :- Z = hAsht.
builtIn(symbolQ, Z, _) :- Z = hAshf.

builtIn(procedureQ, Z, [X|_]) :- isBuiltIn(X), Z = hAsht.
builtIn(procedureQ, Z, [[lambda|_]]) :- Z = hAsht.
builtIn(procedureQ, Z, _) :- Z = hAshf.

builtIn(quote, Z, [A]) :- atom(A),Z = [sYm|[A]].
builtIn(quote, Z, [L|[]]) :- Z = [lIst|L].
builtIn(list, Z , L) :- Z = [lIst|L].
builtIn(or, Z, L) :- firstTrue(Z, L).
builtIn(and, Z, L) :- lastTrue(Z, L).
builtIn(car, Z, [[lIst,X|_]]) :- Z = X.
builtIn(cons, Z, [X,[lIst|L]]) :- Z = [lIst,X|L].
builtIn(cdr, Z, [[lIst,_|L]]) :- Z = [lIst|L].

builtIn(cond, Z, L) :- fTrue(Z, L).
builtIn(else, Z, [X]) :- Z = X.

builtIn(quOte, Z, L) :- Z = [lIst|L].

builtIn(_, Z, _) :- Z = error.

/* Built in helper functions */
eqList(Z, []) :- Z = hAsht.
eqList(Z, [_|[]]) :- Z = hAsht.
eqList(Z, [X,X]) :- Z = hAsht.
eqList(Z, [X,X|L]) :- eqList(Z, [X|L]).

fTrue(Z, [X|[]]) :- reduce(X,Y), Z = Y.
fTrue(Z, [[C,_]|L]) :- reduce(C,K),(K = hAshf; K = [lIst]),fTrue(Z,L).
fTrue(Z, [[_,R]|_]) :- reduce(R,Z).

firstTrue(Z, []) :- Z = hAshf.
firstTrue(Z, [X|L]) :- (X = [lIst] ; X = hAshf), firstTrue(Z, L).
firstTrue(Z, [X|_]) :- Z = X.

lastTrue(Z, [X|_]) :- (X = [lIst] ; X = hAshf), Z = hAshf.
lastTrue(Z, [X|[]]) :- Z = X.
lastTrue(Z, [_|L]) :- lastTrue(Z,L).

/* Reductions */

reduce(X,X) :- number(X).

reduce([lIst|L],[lIst|L]).
reduce([sYm,L],[sYm,L]).
reduce(hAsht,hAsht).
reduce(hAshf,hAshf).

reduce([exit],_) :- !,fail.

reduce([if,C,T,F], O) :- reduce(C, CC),(((CC = hAshf;CC = [lIst]),reduce(F,O));(reduce(T,O))).

reduce([let,A,R], O) :- storeLocal(A), reduce(R, O), unstoreLocal(A).

reduce([load,L], O) :- cfname(L,L2),see(L2),!,doFile(O),!,seen,retract(str(_)).

reduce([define,X|[[[lambda,A,B]|_]]],O) :- asserta(data(X,[lambda,A,B])), O = added.

reduce([define,X|[L|_]],O) :- reduce(L,L2),asserta(data(X,L2)), O = added.

reduce([X|L],O) :- data(X,V), reduce([V|L], O).

reduce(X,Y) :- data(X,Val),Y = Val.

reduce([X|L],O) :- isBuiltIn(X), reduceList(L,L2),builtIn(X,O,L2).

reduce([[lambda,P,F]|V], O) :- atom(P),reduceList(V,V2),
                               asserta(data(P,[lIst|V2])),
                               reduce(F,O),retract(data(P,_)).

reduce([[lambda,P,F]|V], O) :- cmpLen(P,V),reduceList(V,V2),storeLocal(P,V2),reduce(F,O),
                               unStore(P).

reduce(X,X).

reduceList([],[]).
reduceList([X|L],[Y|L2]) :- reduce(X,Y),reduceList(L,L2).

storeLocal([]).
storeLocal([[N,V]|L]) :- reduce(V,V2),asserta(data(N,V2)),storeLocal(L).

storeLocal([],[]).
storeLocal([X|N],[Y|M]) :- asserta(data(X,Y)),storeLocal(N,M).

unStore([]).
unStore([X|L]) :- retract(data(X,_)),unStore(L).

unstoreLocal([]).
unstoreLocal([[N,_]|L]) :- retract(data(N,_)),unstoreLocal(L).

/* Main Program Loop */

mainLoop :- write('1 ]=> '),getInput,makeList(X),((reduce(X,Y),output(Y),
                            mainLoop);nl).
mainLoop.

run :-
asserta(data(fAil,fAil)),
write('     - Welcome to Rolands Limited Scheme Interpreter, v6.66 -'),nl,nl,
(mainLoop;nl),clearV,write('Happy Happy Joy Joy.'),nl.

clearV :- retract(data(_,_)),clearV.
clearV.

/* Get the text */

openBrac(X) :- X = 40.
closeBrac(X) :- X = 41.
lower(U,L) :- U > 64, U < 91, L is U + 32.
lower(63,81).
lower(34,64).
lower(L,L).

space(X) :- X = 32 ; X = 10 ; X = 13.

readString([C|L],N) :- myGet0(K),lower(K,C),!,(
(C = 10, N = 0, L = []);
(openBrac(C), N1 is N + 1, readString(L,N1));
(closeBrac(C), N1 is N - 1, readString(L,N1));
(readString(L,N))).

store(L) :- asserta(str(L)).
getInput :- readString(L,0),store(L).

alpha(X) :- X > 59, X < 123.

makeList(L) :- rs,(peek(39);peek(96)),pop(_),
(
(peek(A),alpha(A),getSym(Sym),L = [sYm,Sym]);
(pop(_),buildList(L2),L = [quOte|L2])
).

makeList(L) :- rs,peek(40),!,getSym(_),buildList(L).
makeList(L) :- peek(39),getSym(A),L = [sYm,A].
makeList(L) :- getSym(L).

buildList([]) :- rs,peek(41),!,getSym(_).
buildList([X|L]) :- makeList(X),buildList(L).

quoteChar(X) :- X = 39; X = 96.

getSym(X) :- rs,peek(X),openBrac(X),!,pop(X).
getSym(X) :- rs,peek(X),closeBrac(X),!,pop(X).
getSym(X) :- rs,getWord(X2),conv(X2,X3),name(X,X3).

peek(X) :- str([X|_]).
pop(X) :- str([X|L]),!, retract(str([X|L])), asserta(str(L)).
push(X) :- str(L), retract(str(L)), asserta(str([X|L])).

rs :- (pop(32);pop(10);pop(13)),rs.
rs.

getWord([]) :- peek(32);peek(40);peek(41);peek(10);peek(13).
getWord([X|L]) :- pop(X),getWord(L).

doFile(O) :- getFile(L),store(L),!,execute(O).

findCR :- get0(C),(C = 10;findCR).

myGet0(C) :- get0(K),((K = 59,C = 32, findCR);(K = C)).

getFile([X|L]) :- myGet0(K),!,lower(K,C),((C = -1, X = 10, L = []);
                  (X = C,getFile(L))).

execute(O) :- makeList(X),reduce(X,Y),!,(execute(O);O = Y).


/* Name Conversions */
conv([43],[112,76,117,115]).  
conv([45],[109,73,110,117,115]).
conv([42],[109,85,108,116]).
conv([47],[100,73,118]).
conv([62],[103,84]).
conv([62,61],[103,84,101,81]).
conv([60],[108,84]).
conv([60,61],[108,84,101,81]).
conv([61],[101,81]).
conv([35,116],[104,65,115,104,116]).
conv([35,102],[104,65,115,104,102]).
conv(X,X).

cfname(N, N2) :- name(N, X), deAt(X, XX), addExt(XX,XXX), name(N2, XXX).

deAt([_|L], XX) :- dropLast(L, XX).

dropLast([_|[]], []).
dropLast([X|L], [X|M]) :- dropLast(L,M).

addExt([], [46,115,99,109]).
addExt([46|N],[46|M]) :- gotExt(N,M).
addExt([X|N],[X|M]) :- addExt(N,M).

gotExt([],[]).
gotExt([X|N],[X|M]) :- gotExt(N,M).

cmpLen([],[]).
cmpLen([_|N],[_|M]) :- cmpLen(N,M).


