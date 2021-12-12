:-[
    './utils.pl'
].

:- dynamic [board/6, last_used_id/1, visited/1, count/1].

% board(row, column, type, color, id,stackPosition)
%stackPosition !=0 si es un 
% board(3, 3, q1, b, 1, 0).
% board(5, 3, a2, b, 2, 0).
% board(3, 2, b1, w, 1,0).
% board(2, 3, b2, b, 4).
% board(5, 3, aa1, w, 5).
% board(4, 4, aa2, b, 6).
% board(2, 4, s1, w, 2,0).
% board(4, 2, b2, w, 3,0).
% board(3, 4, s2, w, 4,0).

last_used_id(0). % marca la cantidad de piezas que hay en el tablero y ademas sirve para ponerle el id a las piezas nuevas.

count(0). % variable auxiliar que se usa en el metodo is_valid_board


% get adjacents to (R, C); but valid adjacents with card into. Return [ID] of adjacents.
get_ady_taken(_, _, [], []) :- !.

get_ady_taken(OldRow, OldColumn, [R, C | T], Ans) :-
    NewRow is OldRow + R,
    NewCol is OldColumn + C,
    findall(Id, board(NewRow, NewCol, _, _, Id, _), Neighbors),
    get_ady_taken(OldRow, OldColumn, T, TempAns),
    append(TempAns, Neighbors, Ans),
    !.

get_ady_taken(OldRow, OldColumn, [_, _ | T], Ans) :-
    get_ady_taken(OldRow, OldColumn, T, Ans).


get_ady_free(_, _, [], []) :- !.

get_ady_free(OldRow, OldColumn, [R, C | T], Ans) :-
    NewRow is OldRow + R,
    NewCol is OldColumn + C,
    not(board(NewRow, NewCol, _, _, _,_)),
    get_ady_free(OldRow, OldColumn, T, AnsTemp),
    append([[NewRow, NewCol]], AnsTemp, Ans),
    !.

get_ady_free(OldRow, OldColumn, [_, _ | T], Ans) :-
    get_ady_free(OldRow, OldColumn, T, Ans),
    !.

get_id(Id) :-
    board(_, _, _, _, Id, _),
    !.

is_valid_board() :-
    findall(Id, board(_, _, _, _, Id, _), Ans),
    length(Ans, X),
    X =:= 1,
    !.

is_valid_board() :- 
    clear(),
    get_id(Id),
    is_valid_board_aux(Id),
    findall(_, board(_, _, _, _, _, _), Ans),
    length(Ans, X),
    count(Amount),
    Amount =:= X,
    !.

is_valid_board_aux(_) :-
    findall(_, board(_, _, _, _, _, _), Ans),
    length(Ans, X),
    count(Amount),
    Amount =:= X,
    !.

is_valid_board_aux(Id) :-
    findall(X, visited(X), Visited),
    is_in_visited(Id, Visited, R),
    % format("R vale ~w\n", [R]),
    fix_count(R),
    % count(TemporalCounter),
    % format("counter ~w", [TemporalCounter]),
    % format("el id es ~w\n", [Id]),
    board(Row, Col, _, _, Id,_),
    % format("las posiciones de la ficha son ~w , ~w\n", [Row, Col]),
    address(Address),
    get_ady_taken(Row, Col, Address, Adj),

    member(Now, Adj),
    is_valid_board_aux(Now),
    !.

is_in_visited(Id, Visited, R) :-
    R is 0,
    member(Id, Visited),
    !,
    fail.

is_in_visited(Id, _, R) :-
    assert(visited(Id)),
    R is 1.

fix_count(Sum) :-
    count(C),
    T is C + Sum,
    assertz(count(T)),
    retract(count(_)), 
    !.

clear() :-
    retractall(visited(_)),
    retractall(count(_)),
    assert(count(0)).