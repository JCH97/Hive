:-[
    './board.pl' ,
    './logic.pl',
    './utils.pl',
    './ui.pl'
 ].
 
 :- dynamic [board/6, plays/1, turn/1].
 
 minimax_depth(3).

% aux_board(bw,w,0,1).
% aux_board(aw,w,0,2).
% aux_board(bb,b,0,3).
% % aux_board(bw,w,0,4).
% aux_board(gb,b,0,5).
% % aux_board(gw,w,0,6).
% aux_board(qb,b,0,7).
% aux_board(qw,w,0,8).
% aux_board(sb,b,0,9).
% aux_board(sw,w,0,10).
% aux_board(ab,b,0,11).

% board(0,0,q,w,1,0).
% board(1,1,b,b,2,0).

% minimax_depth(3).

% board(0, 0, m, w, 1, 0).
% board(-1, -1, b, w, 2, 0).
% board(-2, 0, s, b, 3, 0).
% board(2, 0, a, w, 4, 0).
% board(-1, 1, g, w, 5, 0).
% board(1, 1, l, w, 6, 0).
% board(0, 2, q, b, 7, 0).

% board(1, 1, b, w, 6, 0).
% board(-2, 0, b, w, 3, 0).
% board(-1, -1, b, w, 2, 0).
% board(2, 0, b, w, 4, 0).
% board(-1, 1, b, w, 5, 0).
% board(0, 2, s, b, 7, 0).
% board(2, 2, g, b, 8, 0).
% board(4, 2, l, w, 1, 0).

functioning1(CurrPlayer,Move):-
    (get_insert_moves1(CurrPlayer,Moves),
    member([board(C,R,q,CurrPlayer,Id,SP),B],Moves),
    Move = [board(C,R,q,CurrPlayer,Id,SP),B]);

    functioning_aux1(CurrPlayer,0,-1000000000,1000000000,Value,Move),
    writeln(Value).

functioning_aux1(CurrPlayer,Level,AlphaIn,BetaIn,Value,Move):-
     tab(Level*10),
    writeln('functioning_aux1'),
    get_all_player_chips(CurrPlayer,Chips),

    writeln(Chips), 
    get_all_moves1(Chips,Moves), 
    get_insert_moves1(CurrPlayer,ValidInserts), 
    append(Moves,ValidInserts,AllMoves),
     tab(Level*10),writeln('Moves:'), tab(Level*10),writeln(AllMoves),
    (
        (
            0=:= Level mod 2,
            CurrValue = -1000000000,!
        );
        CurrValue= 1000000000,!
    ),

     tab(Level*10),writeln('alphabeta call...'),
    alphabeta(AllMoves,CurrPlayer,Level,AlphaIn,BetaIn,CurrValue,Value,Move),
    !.

alphabeta([HMove|Moves],Player,Level,AlphaIn,BetaIn,CurrValue,Value,Move):-
     tab(Level*10),writeln(HMove),
    % format("~n ~a ~a ~a ~a ~a ~n",[Player,Level,AlphaIn,BetaIn,CurrValue]),
     tab(Level*10),write(Player ),write(" "),write(Level ),write(" "),write(AlphaIn ),write(" "),write(BetaIn ),write(" "),writeln(CurrValue ),
    writeln(" "),listing(board), tab(Level*10),writeln(" "),
    
    tab(Level*10),writeln('Start alpabeta'),
    (
        tab(Level*10),writeln('S-Leaf'),
        minimax_depth(MaxLevel),
        MaxLevel =:= Level,
        board_value(Player,Level,Value),
        tab(Level*10),writeln('E-Leaf')

    );
    (
         tab(Level*10),writeln('S-Maximizing'),
        0 =:= Level mod 2,
        (
            (
                HMove = [board(C,R,T,Col,Id,SP),aux_board(Name,Type,XPixel,YPixel)],
                 tab(Level*10),writeln('S-move aux'),
                move(board(C,R,T,Col,Id,SP),aux_board(Name,Type,XPixel,YPixel)),
                 tab(Level*10),writeln('E-move aux')
            );
            (
                HMove = [board(C,R,T,Col,Id,SP),CNew,RNew],
                 tab(Level*10),writeln('S-move'),
                move(board(C,R,T,Col,Id,SP),CNew,RNew),
                 tab(Level*10),writeln('E-move')

            )
        ),
        change_player(Player,Opponent),
        NextLevel is Level+1,
         tab(Level*10),writeln('S-Go to next Level Max'),
        functioning_aux1(Opponent,NextLevel,AlphaIn,BetaIn,Value1,_),
         tab(Level*10),writeln('E-Go to next Level Max'),

        (
            (
                HMove = [board(C,R,T,Col,Id,SP),aux_board(Name,Type,XPixel,YPixel)],
                 tab(Level*10),writeln('S-retract aux'),
                retract(board(C,R,T,Col,Id,SP)),
                assert(aux_board(Name,Type,XPixel,YPixel)),                
                retract(last_used_id(_)),
                LastId is Id -1,
                assert(last_used_id(LastId)),
                 tab(Level*10),writeln('E-retract aux')

            );
            (
                HMove = [board(C,R,T,Col,Id,SP),CNew,RNew],
                 tab(Level*10),writeln('S-retract '),
                retract(board(CNew,RNew,T,Col,Id,_)),
                assert(board(C,R,T,Col,Id,SP)),
                 tab(Level*10),writeln('E-retract')
            )
        ),
        CurrValue1 is max(CurrValue,Value1),
        AlphaIn2 is max(AlphaIn,CurrValue1),
        
        (
            (
                CurrValue1>= BetaIn,
                Value is CurrValue1,
                Move = HMove
            );
            (
                 tab(Level*10),writeln('S-alpabeta Next Child Node'),
                alphabeta(Moves,Player,Level,AlphaIn2,BetaIn,CurrValue1,Value2,Move2),
                 tab(Level*10),writeln('E-alpabeta Next Child Node'),

                better_move(HMove,CurrValue1,Move2,Value2,Move,Value,Level)
            )
        ),
         tab(Level*10),writeln('E-Maximizing')
    );
    (
         tab(Level*10),writeln('S-Minimizer'),
        1 =:= Level mod 2,
        (
            (
                HMove = [board(C,R,T,Col,Id,SP),aux_board(Name,Type,XPixel,YPixel)],
                 tab(Level*10),writeln('S-move aux'),
                move(board(C,R,T,Col,Id,SP),aux_board(Name,Type,XPixel,YPixel)),
                 tab(Level*10),writeln('E-move aux')

            );
            (
                HMove = [board(C,R,T,Col,Id,SP),CNew,RNew],
                 tab(Level*10),writeln('S-move'),
                move(board(C,R,T,Col,Id,SP),CNew,RNew),
                 tab(Level*10),writeln('E-move')

            )
        ),
        
        change_player(Player,Opponent),
        NextLevel is Level+1,
         tab(Level*10),writeln('S-Go to next Level Max'),
        functioning_aux1(Opponent,NextLevel,AlphaIn,BetaIn,Value1,_),
         tab(Level*10),writeln('E-Go to next Level Max'),

        (
            (
                HMove = [board(C,R,T,Col,Id,SP),aux_board(Name,Type,XPixel,YPixel)],
                 tab(Level*10),writeln('S-retract aux'),
                retract(board(C,R,T,Col,Id,SP)),
                assert(aux_board(Name,Type,XPixel,YPixel)),                
                retract(last_used_id(_)),
                LastId is Id -1,
                assert(last_used_id(LastId)),
                 tab(Level*10),writeln('E-retract aux')

            );
            (
                HMove = [board(C,R,T,Col,Id,SP),CNew,RNew],
                 tab(Level*10),writeln('S-retract'),
                retract(board(CNew,RNew,T,Col,Id,_)),
                assert(board(C,R,T,Col,Id,SP)),
                 tab(Level*10),writeln('S-retract')
            )
        ),

        CurrValue1 is min(CurrValue,Value1),
        BetaIn2 is min(BetaIn,CurrValue1),
        
        (
            (
                CurrValue1=< AlphaIn,
                Value is CurrValue1,
                Move = HMove
            );
            (
                 tab(Level*10),writeln('S-alpabeta Next Child Node'),
                alphabeta(Moves,Player,Level,AlphaIn,BetaIn2,CurrValue1,Value2,Move2),
                 tab(Level*10),writeln('E-alpabeta Next Child Node'),

                better_move(HMove,CurrValue1,Move2,Value2,Move,Value,Level)
            )
        ),
         tab(Level*10),writeln('E-Minimizer')

    ).


alphabeta([],_,Level,_,_,_,Value,[]):-
     tab(Level*10),writeln('S-alphabeta empty'),
    0=:= Level mod 2, Value = -10000000000,!;
    Value = 10000000000,
     tab(Level*10),writeln('E-alphabeta empty'),
    !.




get_insert_moves1(Color,Answer):-
    % get_from_out_cards_for_ia(Color,InsertPosibilities),
    % get_aux_board_from_pixels(InsertPosibilities,InsertChips)
    get_valid_insert_pos_ia(Color,Pos),
    get_insert_moves(Color,Pos,Answer).

get_insert_moves(Color,Pos,Answer):-
    (
        plays(Plays),
        Plays>=7,
        not(board(_,_,q,Color,_,_)),
        atom_concat(q, Color, Name1),
        aux_board(Name1,Color,X1,Y1),
        format_insert_moves(aux_board(Name1,Color,X1,Y1),Pos,Answer)       
    );
    (

        (
            findall(X,board(_,_,_,Color,X,_),P),
            length(P,Length),
            Length=<4,
            (
                (
                    atom_concat(q, Color, Name2),
                    aux_board(Name2,Color,X2,Y2),
                    Queen = [aux_board(Name2,Color,X2,Y2)]
                );
                Queen = []                
            ),
            (
                (
                    atom_concat(b, Color, Name3),
                    aux_board(Name3,Color,X3,Y3),
                    Beetle = [aux_board(Name3,Color,X3,Y3)]
                );
                Bettle = []  

            ),
            (
                (
                    atom_concat(g, Color, Name4),
                    aux_board(Name4,Color,X4,Y4),
                    Grasshopper = [aux_board(Name4,Color,X4,Y4)]
                );
                Grasshopper = []  
            ),
            append(Queen,Beetle,AnsAux),
            append(AnsAux,Grasshopper,AnsAux1),
            random_between(0, 99, R),
            length(AnsAux1,L),
            (
                L>0,
                Index is R mod L,
                nth0(Index, AnsAux1, AuxBoard),
                format_insert_moves(AuxBoard,Pos,Answer);
                Answer=[]
            )
        );
        (
            findall(X,board(_,_,_,Color,X,_),P),
            length(P,Length),
            Length=<7,
            (
                (
                    atom_concat(a, Color, Name5),
                    aux_board(Name5,Color,X5,Y5),
                    Ant = [aux_board(Name5,Color,X5,Y5)]
                );
                Ant = []                
            ),
            (
                (
                    atom_concat(b, Color, Name6),
                    aux_board(Name6,Color,X6,Y6),
                    Spider = [aux_board(Name6,Color,X6,Y6)]
                );
                Spider = []  

            ),

            append(Ant,Spider,AnsAux2),

            random_between(0, 99, R),
            length(AnsAux2,L),
            (
                L>0,
                Index is R mod L,
                nth0(Index, AnsAux2, AuxBoard2),
                format_insert_moves(AuxBoard2,Pos,Answer);
                Answer=[]
            )
        );
        (
           aux_board(Name8,Color,X7,Y7),
           format_insert_moves(aux_board(Name8,Color,X7,Y7),Pos,Answer)
            
        )
    ),!.

format_insert_moves(aux_board(Name,Color,X,Y),[[C,R|_]|Pos],Moves):-
    sub_atom(Name, 0, 1, _, Type),
    last_used_id(Id),
    AuxId is Id +1,
    format_insert_moves(aux_board(Name,Color,X,Y),Pos,Moves1),
    append([[board(C,R,Type,Color,AuxId,0),aux_board(Name,Color,X,Y)]],Moves1,Moves).
format_insert_moves(_,[],[]).

better_move(Move1,Value1,Move2,Value2,Move1,Value1):-
    Value1>Value2.
better_move(Move1,Value1,Move2,Value2,Move2,Value2).

better_move(Move1,Value1,Move2,Value2,Move1,Value1,Level):-
    (0 =:= Level mod 2,
    Value1>Value2);
    (1 =:= Level mod 2,
    Value1<Value2),
    tab(Level *10),
    writeln(Value1).
    
better_move(Move1,Value1,Move2,Value2,Move2,Value2,Level):-
    (0 =:= Level mod 2,
    Value1=<Value2);
    (1 =:= Level mod 2,
    Value1>=Value2),
    tab(Level *10),
    writeln(Value2).
    

change_player(b,w).
change_player(w,b).

move_won(Value,Level):-
    !, Value >= (0.5** Level ) * 1000000.

get_all_moves1([H |Chips],Moves):-
    % print('heilou'),
    valid_moves(H,HMoves),
    expand_moves(H,HMoves,Expanded),
    get_all_moves1(Chips,Moves1),
    % print('heilou1'),
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
    


get_aux_board_from_pixels([[X,Y]| Coordinates], Answer):-
    aux_board(Name,Color,X,Y),
    get_aux_board_from_pixels(Coordinates,Answer1),
    append([aux_board(Name,Color,X,Y)],Answer1,Answer).
get_aux_board_from_pixels([], []).


distance(board(C,R,_,_,_,_),board(C1,R1,_,_,_,_),Ans):-
    sqrt((C-C1)**2 + (R-R1)**2, Ans).