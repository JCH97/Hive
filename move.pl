:- [
    '/workstate_prolog/Hive/utils.pl',
    '/workstate_prolog/Hive/board.pl'
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

board(1, 1, q, b, 1,0).
% board(1, 1, q1, b, 1,1).

board(1, 0, q1, b, 2,0).
board(2, 1, q1, b, 3,0).
board(2, 2, q1, b, 4,0).
board(1, 2, q1, b, 5,0).
% board(0, 2, q1, b, 6,0).
% board(0, 1, q1, b, 7,0).



any([X|Y], C) :- 
    append(C,[X],D),
    T=..D,
    T,
    !;
    any(Y,C).


insect_above_me(board(R,C,_,_,_,SP)):-
    board(R,C,_,_,_,SP1),
    SP1 > SP.

will_insect_not_break_hive(board(C,R,Type,Color,Id,SP)):-
    retract(board(C,R,_,_,Id,_)),
    last_used_id(Count),
    print(Count),
    Aux is Count - 1 ,
    retract(last_used_id(_)),
    assert(last_used_id(Aux)),
    is_valid_board(Aux),
    print('board is valid \n'),
    assert(board(C,R,Type,Color,Id,SP)),
    retract(last_used_id(_)),
    assert(last_used_id(Count)).


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


valid_moves(board(C,R,q,Color,Id, StackPosition),Moves):-
    valid_queen_moves(board(C,R,q,Color,Id,StackPosition),MovesList),
    print(MovesList),    
    list_to_set(MovesList,Moves).

move(board(C,R,q,Color,Id, StackPosition), C,R):-
    move_queen(board(C,R,q,Color,Id, StackPosition),C,R).
% ----------------Queen Move-------------------------------------

valid_queen_moves(board(C,R,q,Color,Id,SP),MovesList):-
    not(insect_above_me(board(C,R,q,Color,Id,SP))),
    print('no insect above me.'),
    will_insect_not_break_hive(board(C,R,q,Color,Id,SP)),
    print('will_insect_not_break_hive'),
    address(Addr),
    valid_queen_moves_aux(C,R,Addr,MovesList).   

valid_queen_moves(board(C,R,q,Color,Id,SP),[]).

valid_queen_moves_aux(C,R,[C_Dir1,R_Dir1, C_Dir2, R_Dir2 | Addr], Moves):-
    C1_aux is C+C_Dir1,R1_aux is R+R_Dir1,
    C2_aux is C+C_Dir2, R2_aux is R+R_Dir2,
    not(board(C1_aux,R1_aux,_,_,_,_)),not(board(C2_aux,R2_aux,_,_,_,_)),

    Validated = [[C1_aux,R1_aux],[C2_aux,R2_aux]],
    !,
    valid_queen_moves_aux(C,R,[C_Dir2,R_Dir2|Addr],MovesAux),
    append(Validated,MovesAux,Moves),
    print('Moves:' ),
    print(MovesAux).

valid_queen_moves_aux(C,R,[C_Dir1,R_Dir1, C_Dir2, R_Dir2 | Addr], Moves):-
    valid_queen_moves_aux(C,R,[C_Dir2,R_Dir2|Addr],Moves).
    
valid_queen_moves_aux(C,R,Addr,[]).
    
move_queen(board(C,R,q,Color,Id, StackPosition),C_new,R_new):-
    board(C,R,q,Color,Id, StackPosition),
    % print(board(C,R,q,Color,Id, StackPosition)),
    valid_moves(board(C,R,q,Color,Id, StackPosition), Moves),
    X = [C_new,R_new],
    print(Moves),
    % member(X,Moves),
    Z = 'yatusae',
    retract(board(C,R,q,Color,Id, StackPosition)),
    assert(board(C_new,R_new,q,Color,Id, 0)).

move_queen(board(C,R,q,Color,Id, StackPosition),C_new,R_new):-
    format('Invalid move'),
    !.








