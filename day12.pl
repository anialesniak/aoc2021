small(nu).
small(rt).
small(db).
small(qh).
small(sl).
small(uf).
small(ne).

big(pe).
big(zh).
big(qg).

edge(start, nu).
edge(start, rt).
edge(db, qh).
edge(pe, end).
edge(sl, rt).
edge(qh, end).
edge(zh, rt).
edge(nu, rt).
edge(pe, db).
edge(db, sl).
edge(nu, zh).
edge(nu, qh).
edge(pe, qh).
edge(zh, db).
edge(ne, end).
edge(ne, zh).
edge(qg, db).
edge(qh, sl).
edge(zh, qh).
edge(start, zh).
edge(nu, pe).
edge(uf, db).
edge(ne, sl).

connected(X,Y) :- edge(X,Y) ; edge(Y,X).

path(A, B, Path) :-
  travel(A, B, [A], Q),
  reverse(Q, Path).

travel(A, B, Visited, [B|Visited]) :-
  connected(A, B).
travel(A, B, Visited, Path) :-
  small(C),
  connected(A, C),
  \+member(C, Visited),
  travel(C, B, [C|Visited], Path).
travel(A, B, Visited, Path) :-
  big(C),
  connected(A, C),
  travel(C, B, [C|Visited], Path).

part1(S) :-
  findall(Path, path(start, end, Path), Paths),
  length(Paths, S).

path2(A, B, Path) :-
  travel2(A, B, [A], [], Q),
  reverse(Q, Path).

noTwoDoubles(X) :-
  list_to_set(X, S),
  length(X, LX),
  length(S, LS),
  LS >= LX - 1.

travel2(A, B, Visited, _, [B|Visited]) :-
  connected(A, B).
travel2(A, B, Visited, VisitedSmall, Path) :-
  small(C),
  connected(A, C),
  noTwoDoubles([C|VisitedSmall]),
  travel2(C, B, [C|Visited], [C|VisitedSmall], Path).
travel2(A, B, Visited, VisitedSmall, Path) :-
  big(C),
  connected(A, C),
  travel2(C, B, [C|Visited], VisitedSmall, Path).

part2(S) :-
  findall(Path, path2(start, end, Path), Paths),
  length(Paths, S).
