:- [
    './utils.pl',
    './board.pl'
].

:- dynamic [board/6, plays/1, turn/1].

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

% board(0, 0, aa, b, 1,0).
% board(1, 1, q1, b, 1,1).

board(2, -2, a, b, 1, 0).
board(1, -1, q, b, 2, 0).
board(2, 0, b, b, 3, 0).
board(1, 1, q, b, 4, 0).
board(-1, 1, q, b, 5, 0).
board(-2, 0, aa, b, 6, 0).
board(-1, -1, aa, n, 7, 0).
% board(-2, 0, q1, b, 4,0).
% board(-1, -1, q1, b, 7,0).

plays(0).

% place_piece:
place_piece(Color, Ans) :- 
    findall([R, C], board(R, C, _, Color, _, _), SameColor),
    place_piece_aux(SameColor, Ans),
    !.

place_piece_aux([[HR, HC] | T], Ans) :-
    address(Addr),
    adj_path_out(HR, HC, Addr, Ans).

new_play() :- 
    plays(R),
    TR is R + 1,
    retract(plays(_)),
    assert(plays(TR)).

get_turn(T) :- 
    plays(R),
    C is R div 2,
    Remain is R mod 2,
    T is C + Remain.


any([X|Y], C) :- 
    append(C,[X],D),
    T=..D,
    T,
    !;
    any(Y,C).

list_max([H | T],Max):-
    listmax_aux(T,H,Max).
listmax_aux([H |List], Currentmax,Max):-
    Currentmax < H,
    listmax_aux(List,H,Max).

listmax_aux([H |List], Currentmax,Max):-    
    listmax_aux(List,Currentmax,Max).
listmax_aux([], Currentmax,Max):- 
    Max is Currentmax. 


highest_SP(R,C,SP):-
    findall(X, board(R,C,_,_,_,X),P),
    list_max(P,SP).


get_location_by_id([Id|T], Locations):-
    board(R,C,_,_,Id,_),
    get_location_by_id(T, LocAux),
    append([[R,C]],LocAux,Locations).

get_location_by_id([],[]).

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
    !,
    assert(board(R,C,Type,Color,Id,SP)),
    retract(last_used_id(_)),
    assert(last_used_id(Count)).

will_insect_not_break_hive(board(R,C,Type,Color,Id,SP)):-
    last_used_id(Count),
    Aux is Count+1,
    assert(board(R,C,Type,Color,Id,SP)),
    retract(last_used_id(_)),
    assert(last_used_id(Aux)),
    !,fail.


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

get_adj_valid(board(R,C,T,Color,Id, StackPosition),[[R1,C1]|AdjFree],MovesList):-
    retract(board(R,C,T,Color,Id, StackPosition)),
    assert(board(R1,C1,T,Color,Id, StackPosition)),
    is_valid_board(Id),
    retract(board(R1,C1,T,Color,Id, StackPosition)),
    assert(board(R,C,T,Color,Id, StackPosition)),

    get_adj_valid(board(R,C,T,Color,Id, StackPosition),AdjFree,MovesListAux),
    append([[R1,C1]],MovesListAux,MovesList).
    
get_adj_valid(board(R,C,T,Color,Id, StackPosition),[[R1,C1]|AdjFree],MovesList):-
    retract(board(R1,C1,T,Color,Id, StackPosition)),
    assert(board(R,C,T,Color,Id, StackPosition)),

    get_adj_valid(board(R,C,T,Color,Id, StackPosition),AdjFree,MovesList).
    
get_adj_valid(board(R,C,T,Color,Id, StackPosition),[],[]).

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
    list_to_set(MovesList,Moves),!.

valid_moves(board(R,C,b,Color,Id, StackPosition),Moves):-
    valid_beetle_moves(board(R,C,b,Color,Id,StackPosition),MovesList),
    print(MovesList),    
    list_to_set(MovesList,Moves),!.


move(board(R,C,q,Color,Id, StackPosition), R_new,C_new):-
    move_queen(board(R,C,q,Color,Id, StackPosition),R_new,C_new),
    !.

move(board(R, C, aa, Color, Id, StackPosition), R_new, C_new):-
    move_queen(board(R, C, aa, Color, Id, StackPosition), R_new, C_new),
    !.

move(board(R,C,b,Color,Id, StackPosition), R_new,C_new):-
    move_beetle(board(R,C,b,Color,Id, StackPosition),R_new,C_new),!.


% ----------------Queen Move-------------------------------------

valid_queen_moves(board(R,C,q,Color,Id,SP),MovesList):-
    board(R,C,q,Color,Id,SP),
    not(insect_above_me(board(R,C,q,Color,Id,SP))),
    print('no insect above me. \n'),
    will_insect_not_break_hive(board(R,C,q,Color,Id,SP)),
    print('will_insect_not_break_hive \n'),
    address(Addr),
    adj_path_out(R,C,Addr,MovesList1),   
    get_adj_valid(board(R,C,q,Color,Id,SP),MovesList1,MovesList).

    
move_queen(board(R,C,q,Color,Id, StackPosition),R_new,C_new):-
    board(R,C,q,Color,Id, StackPosition),
    valid_moves(board(R,C,q,Color,Id, StackPosition), Moves),
    X = [R_new,C_new],
    print(Moves),
    member(X,Moves),
    retract(board(R,C,q,Color,Id, StackPosition)),
    assert(board(R_new,C_new,q,Color,Id, 0)),    
    !.

move_queen(board(R,C,q,Color,Id, StackPosition),R_new,C_new):-
    format('Invalid move'),
    board(R,C,q,Color,Id, StackPosition),
    retract(board(R_new,C_new,q,Color,Id, 0)),
    assert(board(R,C,q,Color,Id, 0)),
    !,fail.

% ----------------------- Grasshopper Move ---------------------------------

valid_aa_move(board(R, C, aa, Color, Id, StackPosition), ValidPos) :-
    board(R, C, aa, Color, Id, StackPosition),

    not(insect_above_me(board(R, C, aa, Color, Id, StackPosition))),
    format("no insect above me. \n"),

    % will_insect_not_break_hive(board(R,C,q,Color,Id,SP)), => TODO: fail this

    address(Address),
    get_ady_taken(R, C, Address, Adj),

    valid_aa_move_aux(board(R, C, aa, Color, Id, StackPosition), Adj , ValidPos).


valid_aa_move_aux(board(R, C, aa, Color, Id, StackPosition), [HAdj | TAdj], ValidPos) :-
    board(R, C, aa, Color, Id, StackPosition),

    board(TR, TC, TType, TColor, HAdj, TStackPosition),

    DirectionRow is TR - R,
    DirectionCol is TC - C,
  
    format("Check from <~w ~w> with direction <~w ~w> \n", [TR, TC, DirectionRow, DirectionCol]),

    walk_for_direction(TR, TC, aa, DirectionRow, DirectionCol, AuxValidPos1),

    print(AuxValidPos),

    valid_aa_move_aux(board(R, C, aa, Color, Id, StackPosition), TAdj, AuxValidPos2),

    append(AuxValidPos1, AuxValidPos2, ValidPos).
    
valid_aa_move_aux(board(_, _, aa, _, _, _), [], []).
    % append(H, )
    % valid_aa_move_aux(board())


valid_aa_move_aux(board(_, _, aa, _, _, _), [], []).

walk_for_direction(R, C, Type, DirectionRow, DirectionCol, ValidPos) :-
    board(R, C, _, _, _, _),

    NewRow is DirectionRow + R,
    NewCol is DirectionCol + C,
    walk_for_direction(NewRow, NewCol, Type, DirectionRow, DirectionCol, ValidPosAux),

    append([[NewRow, NewCol]], ValidPosAux, ValidPos).

walk_for_direction(_, _, _, _, _, []).

%-----------------Beetle move----------------------------------------
% valid_beetle_moves(board(R,C,b,Color,Id,StackPosition),MovesList):-

valid_beetle_moves(board(R,C,b,Color,Id,SP),MovesList):-
    board(R,C,b,Color,Id,SP),
    not(insect_above_me(board(R,C,b,Color,Id,SP))),
    print('no insect above me. \n'),
    will_insect_not_break_hive(board(R,C,b,Color,Id,SP)),
    print('will_insect_not_break_hive \n'),
    address(Addr),
    adj_path_out(R,C,Addr,Adjs),
    get_adj_valid(board(R,C,b,Color,Id,SP),Adjs, AdjMoves),
    get_ady_taken(R,C,Addr, AdjTakenId),
    get_location_by_id(AdjTakenId, AdjTakenLoc),
    append(AdjMoves,AdjTakenLoc,MovesList).

move_beetle(board(R,C,b,Color,Id, StackPosition),R_new,C_new):-
    board(R,C,b,Color,Id, StackPosition),
    valid_moves(board(R,C,b,Color,Id, StackPosition), Moves),
    X = [R_new,C_new],
    print(Moves),
    member(X,Moves),
    retract(board(R,C,b,Color,Id, StackPosition)),
    highest_SP(R_new,C_new,SP),
    SP1 is SP+1,
    assert(board(R_new,C_new,b,Color,Id, SP1)),
    !.

move_beetle(board(R,C,b,Color,Id, StackPosition),R_new,C_new):-
    format('Invalid move'),
    !,fail.
    
%--------------------------------------------------------------------

%-----------------Ant move----------------------------------------
valid_ant_moves(board(R,C,a,Color,Id,SP),MovesList):-
    board(R,C,a,Color,Id,SP),
    not(insect_above_me(board(R,C,a,Color,Id,SP))),
    print('no insect above me. \n'),
    will_insect_not_break_hive(board(R,C,a,Color,Id,SP)),
    print('will_insect_not_break_hive \n'),
    address(Addr),
    adj_path_out(R,C,Addr,Adjs),
    get_adj_valid(board(R,C,a,Color,Id,SP),Adjs, AdjMoves),
    not(AdjMoves is []),

    retract(board(R,C,a,Color,Id,SP)),
    let_adj_do_their_thing(AdjMoves,[[R,C]],Visited, MovesList),
    assert(board(R,C,a,Color,Id,SP)).

valid_ant_moves(board(R,C,a,Color,Id,SP),[]).

let_adj_do_their_thing([[R,C] |Adj],AuxVisited,Visited,Moves):-
    not(member([R,C],AuxVisited)),    
    address(Addr),
    adj_path_out(R,C,Addr,Adj1),
    adj_connected_to_board(Adj1,AdjValid),
    append([[R,C]],AuxVisited, AuxVisited1),
    let_adj_do_their_thing(AdjValid,AuxVisited1, Visited1, Moves1),
    let_adj_do_their_thing(Adj,Visited1,Visited,Moves2),
    append(Moves1,Moves2,Moves).



let_adj_do_their_thing([[R,C] |Adj],AuxVisited,Visited,Moves):-
    let_adj_do_their_thing(Adj,AuxVisited,Visited,Moves).

let_adj_do_their_thing([],AuxVisited,Visited,[]):-
    Visited is AuxVisited.


adj_connected_to_board([[R,C]| AdjT], AdjValid):-
    address(Addr),
    get_ady_taken(R, C, Addr, AdjTaken),
    not(AdjTaken is []),
    adj_connected_to_board(AdjT,AdjValid1),
    append([[R,C]],AdjValid1,AdjValid).

adj_connected_to_board([[R,C]| AdjT], AdjValid):-
    adj_connected_to_board(AdjT,AdjValid).   

adj_connected_to_board([], []).


%---------------------------------------------------------------------
%-----------------------------Spider Move--------------------------
valid_spider_moves(board(R,C,s,Color,Id,SP),MovesList):-
    board(R,C,s,Color,Id,SP),
    not(insect_above_me(board(R,C,s,Color,Id,SP))),
    print('no insect above me. \n'),
    will_insect_not_break_hive(board(R,C,s,Color,Id,SP)),
    print('will_insect_not_break_hive \n'),
    address(Addr),
    adj_path_out(R,C,Addr,Adjs),
    get_adj_valid(board(R,C,a,Color,Id,SP),Adjs, AdjMoves),
    not(AdjMoves is []),

    retract(board(R,C,a,Color,Id,SP)),
    % let_adj_do_their_thing_spider(AdjMoves,[[R,C]], MovesList, 1),
    assert(board(R,C,a,Color,Id,SP)).
%------------------------------------------------------------------

    

    % adj_path_out(R,C,[R_Dir1,C_Dir1, R_Dir2, C_Dir2 | Addr], Moves):-
%--------------------- Ant Move -------------------------------------

valids_a_move(board(R, C, a, Color, Id, StackPosition), ValidPos) :- 
    board(R, C, a, Color, Id, StackPosition),

    not(insect_above_me(board(R, C, aa, Color, Id, StackPosition))),
    format("no insect above me. \n"),

    % will_insect_not_break_hive(board(R, C, a, Color, Id, StackPosition)),
    % print('will_insect_not_break_hive \n'),

    address(Addr),
    adj_path_out(R, C, Addr, MovesListOut), 
    length(MovesListOut, Len),
    Len > 0,

    retract(board(_, _, a, _, Id, _)),
    valids_a_move_aux(R, C, ValidPos),
    !.

valids_a_move_aux(R, C , ValidPos) :-
    % print(HR),
    % print(HC),
    format("Check now position <~w ~w>", [R, C]),
    address(Addr),
    get_ady_free(R, C, Addr, FreeAdj),
    get_adj_valids_for_ant_aux(FreeAdj, ValidPos).

get_adj_valids_for_ant_aux([[R | [C | _]] | TF], Ans) :-
    address(Addr),
    % board(R, C, _, _, HF, _),
    format("get_adj_valids_for_ant_aux for <~w ~w>", [R, C]),
    get_ady_taken(R, C, Addr, AdjTaken),
    length(AdjTaken, L),
    L > 0,

    % valids_a_move_aux(R, C, ),
    
    get_adj_valids_for_ant_aux(TF, AnsAux),
    append([[R, C]], AnsAux, Ans).





