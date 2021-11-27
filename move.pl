:- [
    './utils.pl',
    './board.pl'
].

:- dynamic [board/6].

%% hormigas => a         arannas => s
%% saltamontes => aa     abeja reina => q
%% escarabajos => b

% board(3, 3, q1, b, 1,0).
% % board(4, 3, a2, b, 2).
% board(3, 2, b1, w, 1,0).
% % board(2, 3, b2, b, 4).
% % board(5, 3, aa1, w, 5).
% % board(4, 4, aa2, b, 6).
% board(2, 4, s1, w, 2,0).
% board(2, 4, b2, w, 3,1).
% board(3, 4, s2, w, 4,0).

board(0, 0, q, b, 1,0).
% board(1, 1, q1, b, 1,1).

board(1, -1, q1, b, 2,0).
board(2, 0, q1, b, 3,0).
board(1, 1, q1, b, 4,0).
board(-1, 1, q1, b, 5,0).
% board(-2, 0, q1, b, 6,0).
% board(-1, -1, q1, b, 7,0).



any([X|Y], C) :- 
    append(C,[X],D),
    T=..D,
    T,
    !;
    any(Y,C).


insect_above_me(board(R,C,_,_,_,SP)):-
    board(R,C,_,_,_,SP1),
    SP1 > SP.

will_insect_not_break_hive(board(R,C,Type,Color,Id,SP)):-
    retract(board(R,C,_,_,Id,_)),
    last_used_id(Count),
    print(Count),
    Aux is Count - 1 ,
    retract(last_used_id(_)),
    assert(last_used_id(Aux)),
    is_valid_board(Aux),
    print('board is valid \n'),
    assert(board(R,C,Type,Color,Id,SP)),
    retract(last_used_id(_)),
    assert(last_used_id(Count)).

adj_path_out(R,C,[R_Dir1,C_Dir1, R_Dir2, C_Dir2 | Addr], Moves):-
    R1_aux is R+R_Dir1,C1_aux is C+C_Dir1,
    R2_aux is R+R_Dir2,C2_aux is C+C_Dir2,
    print('hi'),
    not(board(R1_aux,C1_aux,_,_,_,_)),not(board(R2_aux,C2_aux,_,_,_,_)),
    Validated = [[R1_aux,C1_aux],[R2_aux,C2_aux]],
    !,
    adj_path_out(R,C,[R_Dir2,C_Dir2|Addr],MovesAux),
    append(Validated,MovesAux,Moves),
    print('Moves: ' ),
    print(MovesAux).

adj_path_out(R,C,[R_Dir1,C_Dir1, R_Dir2, C_Dir2 | Addr], Moves):-
    adj_path_out(R,C,[R_Dir2,C_Dir2|Addr],Moves).

adj_path_out(R,C,[R_Dir1,C_Dir1],Moves):-
    R1_aux is R+R_Dir1,C1_aux is C+C_Dir1,
    close_cycle_addr([R_Dir2,C_Dir2]),
    R2_aux is R+R_Dir2,C2_aux is C+C_Dir2,
    not(board(R1_aux,C1_aux,_,_,_,_)),not(board(R2_aux,C2_aux,_,_,_,_)),
    Moves = [[R1_aux,C1_aux],[R2_aux,C2_aux]].

adj_path_out(R,C,[R_Dir1,C_Dir1],[]).

% Queen Bee
% move_queen_bee(OldRow, OldColumn, Type, Adj) :-
%     board(OldRow, OldColumn, Type, Color, Id),
%     retract(board(OldRow, OldColumn, Type, Color, Id)),
%     last_used_id(Temp),
%     New is Temp - 1,
%     retract(last_used_id(_)),
%     assert(last_used_id(New)),
%     is_valid_board(New),
%     address(Address),
%     get_ady_free(OldRow, OldColumn, Address, Adj),
%     !.

% move_queen_bee(_, _, _, _) :-
%     format('Invalid move'),
%     !.


valid_moves(board(R,C,q,Color,Id, StackPosition),Moves):-
    valid_queen_moves(board(R,C,q,Color,Id,StackPosition),MovesList),
    print(MovesList),    
    list_to_set(MovesList,Moves).

move(board(R,C,q,Color,Id, StackPosition), R_new,C_new):-
    move_queen(board(R,C,q,Color,Id, StackPosition),R_new,C_new).
% ----------------Queen Move-------------------------------------

valid_queen_moves(board(R,C,q,Color,Id,SP),MovesList):-
    not(insect_above_me(board(R,C,q,Color,Id,SP))),
    print('no insect above me. \n'),
    will_insect_not_break_hive(board(R,C,q,Color,Id,SP)),
    print('will_insect_not_break_hive \n'),
    address(Addr),
    adj_path_out(R,C,Addr,MovesList).   



valid_queen_moves_aux(R,C,[R_Dir1,C_Dir1, R_Dir2, C_Dir2 | Addr], Moves):-
    R1_aux is R+R_Dir1,C1_aux is C+C_Dir1,
    R2_aux is R+R_Dir2,C2_aux is C+C_Dir2,
    not(board(R1_aux,C1_aux,_,_,_,_)),not(board(R2_aux,C2_aux,_,_,_,_)),
    Validated = [[R1_aux,C1_aux],[R2_aux,C2_aux]],
    !,
    valid_queen_moves_aux(R,C,[R_Dir2,C_Dir2|Addr],MovesAux),
    append(Validated,MovesAux,Moves),
    print('Moves: ' ),
    print(MovesAux).

valid_queen_moves_aux(R,C,[R_Dir1,C_Dir1, R_Dir2, C_Dir2 | Addr], Moves):-
    valid_queen_moves_aux(R,C,[R_Dir2,C_Dir2|Addr],Moves).

valid_queen_moves_aux(R,C,[R_Dir1,C_Dir1],Moves):-
    R1_aux is R+R_Dir1,C1_aux is C+C_Dir1,
    close_cycle_addr([R2_Dir2,C2_Dir2]),
    R2_aux is R+R_Dir2,C2_aux is C+C_Dir2,
    not(board(R1_aux,C1_aux,_,_,_,_)),not(board(R2_aux,C2_aux,_,_,_,_)),
    Moves = [[R1_aux,C1_aux],[R2_aux,C2_aux]].
    
move_queen(board(R,C,q,Color,Id, StackPosition),R_new,C_new):-
    board(R,C,q,Color,Id, StackPosition),

    valid_moves(board(R,C,q,Color,Id, StackPosition), Moves),
    X = [R_new,C_new],
    print(Moves),
    member(X,Moves),

    retract(board(R,C,q,Color,Id, StackPosition)),
    assert(board(R_new,C_new,q,Color,Id, 0)).

move_queen(board(R,C,q,Color,Id, StackPosition),C_new,R_new):-
    format('Invalid move'),
    !.








