:-[
    './board.pl' ,
    './move.pl',
    './utils.pl'
 ].
 
 :- dynamic [board/6, plays/1, turn/1].
 
 minimax_depth(4).

% board(0,0,q,w,1,0).
% board(1,1,q,b,2,0).
% % board(-1,-1,b,w,3,0).

% board(0, 0, q, w, 1, 0).
% board(1, -1, q, b, 2, 0).
% board(-2, 0, b, w, 3, 0).
% board(2, -2, a, b, 4, 0).

board(0, 0, q, w, 1, 0).
board(-1, -1, b, w, 2, 0).
board(-2, 0, b, w, 3, 0).
board(2, 0, b, w, 4, 0).
board(-1, 1, b, w, 5, 0).
board(1, 1, b, w, 6, 0).
board(0, 2, s, b, 7, 0).



functioning1(CurrPlayer,Move):-
    functioning_aux1(CurrPlayer,0,_,Move).

functioning_aux1(CurrPlayer,Level,Value,Move):-
    get_all_player_chips(CurrPlayer,Chips),
    get_all_moves1(Chips,Moves),
    print(Moves),
    move_simulation1(Moves,CurrPlayer,Level,Value,Move).

move_simulation1([HMove|Moves],CurrPlayer,Level,Value,Move):-
    move_chip1(HMove,CurrPlayer,Level,Value1),
    (
        (
            move_won(Value1,Level),
            Value = Value1, Move= HMove
        );           
        (
            move_simulation1(Moves,CurrPlayer,Level,Value2,Move2),
            better_move(HMove,Value1,Move2,Value2,Move,Value)
        )
    ).

move_simulation1([],CurrPlayer,Level,-100000000,[]).

move_chip1([board(R,C,Type,Color,Id,SP), RNew,CNew],CurrPlayer,Level,Value):-
    Before = board(R,C,Type,Color,Id,SP),
    move(board(R,C,Type,Color,Id,SP), RNew,CNew),
    board_value(CurrPlayer,Level,BValue),!,  
    (
        (
            (move_won(BValue,Level);
            (
                minimax_depth(MaxLevel),
                MaxLevel is Level
            ) ),           
            Value = BValue
        );
        (            
            change_player(CurrPlayer,Opponent),
            functioning_aux1(Opponent,Level+1,Value1,Move1),            
            Value = BValue - Value1
        )
        
    ),
    retract(board(_,_,_,_,Id,_)),
    assert(Before).
        
%------------------------Utils----------------------

better_move(Move1,Value1,Move2,Value2,Move1,Value1):-
    Value1>Value2.
better_move(Move1,Value1,Move2,Value2,Move2,Value2).

change_player(b,w).
change_player(w,b).

move_won(Value,Level):-
    !, Value >= (0.5** Level ) * 1000000.

get_all_moves1([H |Chips],Moves):-
    valid_moves(H,HMoves),
    expand_moves(H,HMoves,Expanded),
    get_all_moves1(Chips,Moves1),
    append(Expanded,Moves1,Moves).
get_all_moves1([],[]).

expand_moves(Chip,[[R,C] |Moves],FormatedMoves):-
    expand_moves(Chip,Moves,FormatedMoves1),
    append([[Chip,R,C]],FormatedMoves1,FormatedMoves).
expand_moves(Chip,[],[]).


board_value(Player, CurrLevel,Value):-
    !,change_player(Player,Opponent),
    opponent_chips_blocked(Opponent,Value1),
    winning_move(Opponent,CurrLevel,Value2),
    queen_blocked(Opponent,Value3),
    chips_sorrounding_queen(Opponent,Value4),
    Value is Value1 + Value2 + Value3 + Value4.
    % Value is Value1+ Value3 + Value4.


opponent_chips_blocked(Opponent,Value):-
    get_all_player_chips(Opponent,Chips),
    filter_blocked_chips(Chips,Chips1),
    length(Chips1,Value).

winning_move(Opponent,CurrLevel, Value):-
    address(Addr),
    board(R,C,q,Opponent,_,_),
    get_ady_taken(R,C,Addr,Taken),
    length(Taken,L),
    L=6,
    Value is (0.5**CurrLevel)*1 000 000.
    
winning_move(_,_,0).

queen_blocked(Opponent,Value):-
    address(Addr),
    board(R,C,q,Opponent,_,_),
    adj_path_out(R,C,Addr,Moves),
    length(Moves,L),
    L =0,
    Value is 3.
queen_blocked(_,0).

chips_sorrounding_queen(Opponent,Value):-
    address(Addr),
    board(R,C,q,Opponent,_,_),
    get_ady_taken(R,C,Addr,Taken),
    length(Taken,L),    
    Value is  L*2.
chips_sorrounding_queen(_,0).



get_all_player_chips(Player,Chips):-
    findall(X, get_a_chip(Player,X),Chips ).
    

get_a_chip(Player,Chip):-
    board(R,C,T,Player,Id,SP),
    Chip = board(R,C,T,Player,Id,SP).




filter_blocked_chips([board(R,C,_,_,_,SP)|Chips], Res):-
    insect_above_me(board(R,C,_,_,_,SP)),
    filter_blocked_chips(Chips, Res1),
    append([board(R,C,_,_,_,SP)],Res1,Res). 
filter_blocked_chips([board(R,C,Type,Color,Id,SP)|Chips], Res):-    
    not(will_insect_not_break_hive(board(R,C,Type,Color,Id,SP))),
    filter_blocked_chips(Chips, Res1),
    append([board(R,C,_,_,_,SP)],Res1,Res). 

filter_blocked_chips([board(R,C,_,_,_,SP)|Chips], Res):-
    address(Addr),
    
    adj_path_out(R,C,Addr,Moves),
    length(Moves,Length),
    Length >0,
    filter_blocked_chips(Chips, Res1),
    append([board(R,C,_,_,_,SP)],Res1,Res). 
filter_blocked_chips([board(R,C,_,_,_,SP)|Chips], Res):-
    filter_blocked_chips(Chips, Res).
filter_blocked_chips([], []).
    