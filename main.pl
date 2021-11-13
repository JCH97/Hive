%% hormigas => a         arannas => s
%% saltamontes => aa     abeja reina => q
%% escarabajos => b

:- [
    '/workstate_prolog/Hive/utils.pl'
].

% board(0, 0, z, z, 100). % row, column, type, color, id
board(3, 3, a1, w, 1).
% board(4, 3, a2, b, 2).
% board(3, 2, b1, w, 3).
% board(2, 3, b2, b, 4).
% board(5, 3, aa1, w, 5).
board(4, 4, aa2, b, 6).
board(4, 2, s1, w, 7).

last_used_id(3).
% visited(0).
count(0).

:- dynamic [board/5, last_used_id/1, visited/1, count/1].

% get location by Type
place_piece(R, C, Type, Color) :-
    not(board(_, _, Type, Color, _)),
    last_used_id(X),
    Id is X + 1,
    retract(last_used_id(X)),
    assert(last_used_id(Id)),
    assert(board(R, C, Type, Color, Id)),
    !.

% get adjacents to (R, C); but valid adjacents with card into. Return [ID] of adjacents.
get_ady(_, _, [], []) :- !.

get_ady(OldRow, OldColumn, [R, C | T], [Id | T1]) :-
    NewRow is OldRow + R,
    NewCol is OldColumn + C,
    board(NewRow, NewCol, _, _, Id),
    get_ady(OldRow, OldColumn, T, T1),
    !.

get_ady(OldRow, OldColumn, [_, _ | T], T1) :-
    get_ady(OldRow, OldColumn, T, T1).


% get_ady2(R, C, Ans) :- 
%     row_directions(Row),
%     columns_directions(Col),
%     member(Hr, row_directions),
%     member(Hc, columns_directions),
%     get_ady2(R, C, Ans, Row, Col).
%     %member(Z, Ans).         % TODO: remove this line, testing purposes.

% get_ady2(_, _, _, [], []) :- !.

% get_ady2(R, C, [H | T], [Hr | Tr], [Hc | Tc]) :-
%     Nr is Hr + R,
%     Nc is Hc + C,
%     board(Nr, Nc, _, _, H),
%     get_ady2(R, C, T, Tr, Tc),
%     !.

% get_ady2(R, C, A, [_ | Tr], [_ | Tc]) :- get_ady2(R, C, A, Tr, Tc).

is_valid_board() :- 
    retractall(visited(_)),
    fix_count(0),
    is_valid_board(1),
    last_used_id(X),
    count(Amount),
    Amount =:= X,
    !.

is_valid_board() :-
    last_used_id(X),
    count(Amount),
    Amount =:= X,
    !.

is_valid_board(Id) :-
    findall(X, visited(X), Visited),
    is_in_visited(Id, Visited, R),
    % format("R vale ~w\n", [R]),
    fix_count(R),
    count(TemporalCounter),
    % format("counter ~w", [TemporalCounter]),
    % format("el id es ~w\n", [Id]),
    board(Row, Col, _, _, Id),
    % format("las posiciones de la ficha son ~w , ~w\n", [Row, Col]),
    address(Address),
    get_ady(Row, Col, Address, Adj),
    member(Now, Adj),
    is_valid_board(Now).

is_in_visited(Id, Visited, R) :-
    R is 0,
    member(Id, Visited),
    !.

is_in_visited(Id, _, R) :-
    assert(visited(Id)),
    R is 1.

fix_count(Sum) :-
    count(C),
    T is C + Sum,
    assertz(count(T)),
    retract(count(_)), 
    !.