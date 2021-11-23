:- [
    '/workstate_prolog/Hive/utils.pl',
    '/workstate_prolog/Hive/board.pl'
].

:- dynamic [board/5].

% Queen Bee
move_queen_bee(OldRow, OldColumn, Type, Adj) :-
    board(OldRow, OldColumn, Type, Color, Id),
    retract(board(OldRow, OldColumn, Type, Color, Id)),
    last_used_id(Temp),
    New is Temp - 1,
    retract(last_used_id(_)),
    assert(last_used_id(New)),
    is_valid_board(New),
    address(Address),
    get_ady_free(OldRow, OldColumn, Address, Adj),
    !.

move_queen_bee(_, _, _, _) :-
    format("Invalid move"),
    !.