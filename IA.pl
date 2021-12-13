:-[
    './board.pl' ,
    './move.pl',
    './utils.pl'

 ].
 
%  :- dynamic [board/6, plays/1, turn/1].
 
 minimax_depth(3).

% aux_board(bw,w,0,1).
% aux_board(aw,w,0,2).
% aux_board(bb,b,0,3).
% aux_board(bw,w,0,4).
% aux_board(gb,b,0,5).
% aux_board(gw,w,0,6).
% aux_board(qb,b,0,7).
% aux_board(qw,w,0,8).
% aux_board(sb,b,0,9).
% aux_board(sw,w,0,10).
% aux_board(ab,b,0,11).

% aux_board(qw, w, 0, 0).
% aux_board(aw, w, 0, 55).
% aux_board(aw, w, 0, 110).
% aux_board(aw, w, 0, 165).
% aux_board(gw, w, 0, 220).
% aux_board(gw, w, 0, 275).
% aux_board(gw, w, 0, 330).
% aux_board(bw, w, 0, 385).
% aux_board(bw, w, 0, 440).
% aux_board(sw, w, 0, 495).
% aux_board(sw, w, 0, 550).
% aux_board(ab, b, 945, 110).
% aux_board(ab, b, 945, 165).
% aux_board(gb, b, 945, 220).
% aux_board(gb, b, 945, 275).
% aux_board(gb, b, 945, 330).
% aux_board(bb, b, 945, 385).
% aux_board(bb, b, 945, 440).
% aux_board(sb, b, 945, 495).
% aux_board(sb, b, 945, 550).
% aux_board(qb, b, 945, 0).
% aux_board(ab, b, 945, 55).

% board(0,0,q,w,1,0).
% board(1,1,q,b,2,0).
% % board(-1,-1,b,w,3,0).

% board(0, 0, q, w, 1, 0).
% board(1, -1, q, b, 2, 0).
% board(-2, 0, b, w, 3, 0).
% board(2, -2, a, b, 4, 0).

% board(0, 0, l, w, 1, 0).
% board(-1, -1, b, w, 2, 0).
% board(-2, 0, b, w, 3, 0).
% board(2, 0, b, w, 4, 0).
% board(-1, 1, b, w, 5, 0).
% board(1, 1, b, w, 6, 0).
% board(0, 2, s, b, 7, 0).



functioning1(CurrPlayer,Move):-
    functioning_aux1(CurrPlayer,0,_,Move).

functioning_aux1(CurrPlayer,Level,Value,Move):-
    get_all_player_chips(CurrPlayer,Chips),
    get_all_moves1(Chips,Moves),
    print(Moves),
    move_simulation1(Moves,CurrPlayer,Level,Value1,Move1),
    get_all_insert_moves1(CurrPlayer,ValidInserts),
    insert_simulation1(ValidInserts,CurrPlayer,Level,Value2,Move2),
    better_move(Move1,Value1,Move2,Value2,Move,Value),

    !.

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

insert_simulation1([HMove|Moves],CurrPlayer,Level,Value,Move):-
    insert_chip1(HMove,CurrPlayer,Level,Value1),
    insert_simulation1(Moves, CurrPlayer, Level, Value2, Move2),
    better_move(HMove,Value1,Move2,Value2,Move,Value).
insert_simulation1([],CurrPlayer,Level,-100000000,[]).

move_chip1([board(R,C,Type,Color,Id,SP), RNew,CNew],CurrPlayer,Level,Value):-
    Before = board(R,C,Type,Color,Id,SP),
    move(board(R,C,Type,Color,Id,SP), RNew,CNew),
    board_value(CurrPlayer,Level,BValue),!,  
    (
        (
            (
                move_won(BValue,Level);
                (
                    minimax_depth(MaxLevel),
                    MaxLevel is Level
                ) 
            ),           
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


insert_chip1([board(C,R,Type,Color,Id,SP),aux_board(Name,Color,XPixel,YPixel)],CurrPlayer,Level,Value):-
    assert(board(C,R,Type,Color,Id,SP)),
    last_used_id(Before),
    retract(last_used_id(_)),
    assert(last_used_id(Id)),
    retract(aux_board(Name,Color,XPixel,YPixel)),
    board_value(CurrPlayer,Level,BValue),!,
    (
        (      
            minimax_depth(MaxLevel),
            MaxLevel is Level,        
            Value = BValue
        );
        (            
            change_player(CurrPlayer,Opponent),
            functioning_aux1(Opponent,Level+1,Value1,Move1),            
            Value = BValue - Value1
        )
        
    ),
    retract(last_used_id(_)),
    assert(last_used_id(Before)),
    assert(aux_board(Name,Color,XPixel,YPixel)),
    retract(board(C,R,Type,Color,Id,SP)).

%------------------------Utils----------------------

better_move(Move1,Value1,Move2,Value2,Move1,Value1):-
    Value1>Value2.
better_move(Move1,Value1,Move2,Value2,Move2,Value2).

change_player(b,w).
change_player(w,b).

move_won(Value,Level):-
    !, Value >= (0.5** Level ) * 1000000.

get_all_moves1([H |Chips],Moves):-
    print('heilou'),
    valid_moves(H,HMoves),
    expand_moves(H,HMoves,Expanded),
    get_all_moves1(Chips,Moves1),
    print('heilou1'),
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
    distance_to_opponent_queen(Player,Value5),
    Value is Value1 + Value2 + Value3 + Value4+Value5.
    % Value is Value1+ Value3 + Value4.


distance_to_opponent_queen(Player,Value):-
    get_all_player_chips(Player,Chips),
    change_player(Player,Opponent),
    board(C,R,q,Opponent,Id,SP),
    X = board(C,R,q,Opponent,Id,SP),
    sum_distance_from_queen(X,Chips,Value).
distance_to_opponent_queen(_,0).

sum_distance_from_queen(Queen,[HChip|Chips],Value):-
    distance(Queen,HChip,Dist),
    sum_distance_from_queen(Queen, Chips, Value1),
    (
        (Dist>0,
        Value2 is 200/Dist);
        Value2 is 200

    ),
    Value is Value2 + Value1.
sum_distance_from_queen(_,[],0).



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
    

get_all_insert_moves1(Color,Answer):-
    get_from_out_cards_for_ia(Color,InsertPosibilities),
    get_aux_board_from_pixels(InsertPosibilities,InsertChips),
    get_valid_insert_pos_ia(Color,Pos),
    create_insert_moves(InsertChips,Pos,Answer),
    !.

get_aux_board_from_pixels([[X,Y]| Coordinates], Answer):-
    aux_board(Name,Color,X,Y),
    get_aux_board_from_pixels(Coordinates,Answer1),
    append([aux_board(Name,Color,X,Y)],Answer1,Answer).
get_aux_board_from_pixels([], []).

create_insert_moves([aux_board(Name,Color,X,Y)|Chips],  Pos,Answer):-
    format_insert_moves(aux_board(Name,Color,X,Y),Pos,Moves1),
    create_insert_moves(Chips,Pos,Moves2),
    append(Moves1,Moves2,Answer).
create_insert_moves([],  _,[]).

format_insert_moves(aux_board(Name,Color,X,Y),[[C,R|_]|Pos],Moves):-
    sub_atom(Name, 0, 1, _, Type),
    last_used_id(Id),
    AuxId is Id +1,
    format_insert_moves(aux_board(Name,Color,X,Y),Pos,Moves1),
    append([[board(C,R,Type,Color,AuxId,0),aux_board(Name,Color,X,Y)]],Moves1,Moves).
format_insert_moves(_,[],[]).

distance(board(C,R,_,_,_,_),board(C1,R1,_,_,_,_),Ans):-
    sqrt((C-C1)**2 + (R-R1)**2, Ans).