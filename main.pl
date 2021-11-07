%% hormigas => a         arannas => s
%% saltamontes => aa     abeja reina => q
%% escarabajos => b

board(0, 0, z, z, 100). % row, column, type, color, id

last_used_id(0).


:- dynamic [board/5, last_used_id/1].

row_directions([1, 1, 0, -1, -1, 0]).
columns_directions([-1, 0, 1, 1, 0, -1]).

% get location by Type
find_location(Row, Column, Type, Color, Id) :- 
    board(Row, Column, Type, Color, Id).

place_piece(Q, R, Type, Color) :-
    not(board(_, _, Type, Color, _)),
    last_used_id(X),
    Id is X + 1,
    print(Id),
    % retract(last_used_id(X)),
    % assert(last_used_id(Id)),
    assert(board(Q, R, Type, Color, Id)),
    !.

% place_piece(Q, R, Type, Color) :-
%     !.s