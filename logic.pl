:- [
    './utils.pl',
    './board.pl'
].


:- dynamic [board/6, plays/1, turn/1].

%% hormigas => a        arannas => s        mosquito => m
%% saltamontes => g     abeja reina => q    ladybug => l
%% escarabajos => b

plays(0).
last_chip_moved_id(0).

% board(0, 0, gb, b, 1, 0).

% where_place_piece

end_game(NoWinner) :-
    findall(Id, board(_, _, q, _, Id, _), Queens),
    end_game_aux(Queens, NoWinner).

end_game_aux([], n) :- !.

end_game_aux([Id | _], NoWinner) :-
    board(R, C, _, Color, Id, _),
    address(Addr),
    get_ady_taken(R, C, Addr, Ans),
    length(Ans, L),
    L >= 6,
    NoWinner = Color,
    !.

end_game_aux([_ | T], NoWinner) :-
    end_game_aux(T, NoWinner).


where_place_piece(R, C, Color, Ans) :- 
    address(Addr),
    adj_path_out(R, C, Addr, TempAns),
    list_to_set(TempAns, WithOutRepeted),
    apply_sieve(WithOutRepeted, Color, Ans).

apply_sieve(WithOutRepeted, _, Ans) :-
    plays(P),
    P < 2,
    append([], WithOutRepeted, Ans),
    !.

apply_sieve(WithOutRepeted, Color, Ans) :-
    apply_sieve_aux(WithOutRepeted, Color, Ans).

apply_sieve_aux([], _, []) :- !.

apply_sieve_aux([[R, C | _] | T], Color, [[TR, TC | _] | TT]) :-
    address(Addr),
    get_ady_taken(R, C, Addr, IdsAdy),
    findall(TColor, (member(Id, IdsAdy), board(_, _, _, TColor, Id, _)), TAns),
    sort(TAns, AvailableColors),
    length(AvailableColors, L),
    L =:= 1,
    sort(AvailableColors, [H | _]),
    H = Color,
    TR is R,
    TC is C,
    apply_sieve_aux(T, Color, TT),
    !.

apply_sieve_aux([[_, _ | _] | T], Color, Ans) :-
    apply_sieve_aux(T, Color, Ans).
    

fix_last_used_id() :-
    retractall(last_used_id(_)),
    findall(Id, board(_, _, _, _, Id, _), Ans),
    max_member(M, Ans),
    assert(last_used_id(M)).
    

add_entry_in_board(R, C, Type, Color, Stack, NewId) :-
    % IsFromBoard =:= 0,
    last_used_id(Id),
    NewId is Id + 1,
    assert(board(R, C, Type, Color, NewId, Stack)),
    retractall(last_used_id(_)),
    assert(last_used_id(NewId)),
    % fix_ids(),
    new_play().

% add_entry_in_board(R, C, Type, Color, _, Stack) :-
%     last_used_id(Id),
%     NewId is Id + 1,
%     assert(board(R, C, Type, Color, NewId, Stack)),
%     new_play().

fix_ids() :-
    findall([R, C, Type, Color, Id, SP], board(R, C, Type, Color, Id, SP), Ans),
    length(Ans, L),
    retractall(last_used_id(_)),
    assert(last_used_id(L)),
    retractall(board(_, _, _, _, _, _)),
    fix_ids_aux(Ans, 1).


fix_ids_aux([], _) :- !.

fix_ids_aux([ [R, C, Type, Color, _, SP | _] | T], IdTemp) :-
    assert(board(R, C, Type, Color, IdTemp, SP)),
    NewIdTemp is IdTemp + 1,
    fix_ids_aux(T, NewIdTemp).
    

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
    % print(Count),
    Aux is Count - 1 ,
    retract(last_used_id(_)),
    assert(last_used_id(Aux)),
    is_valid_board(),
    % print('board is valid \n'),
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
    % print('hi'),
    not(board(R1_aux,C1_aux,_,_,_,_)),not(board(R2_aux,C2_aux,_,_,_,_)),
    Validated = [[R1_aux,C1_aux],[R2_aux,C2_aux]],
    !,
    adj_path_out(R,C,[R_Dir2,C_Dir2|Addr],MovesAux),
    append(Validated,MovesAux,Moves),
    % print('Moves: ' ),
    % print(MovesAux).
    !.

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
    % listing(board),
    is_valid_board(),
    retract(board(R1,C1,T,Color,Id, StackPosition)),
    assert(board(R,C,T,Color,Id, StackPosition)),

    get_adj_valid(board(R,C,T,Color,Id, StackPosition),AdjFree,MovesListAux),
    append([[R1,C1]],MovesListAux,MovesList).
    
get_adj_valid(board(R,C,T,Color,Id, StackPosition),[[R1,C1]|AdjFree],MovesList):-
    retract(board(R1,C1,T,Color,Id, StackPosition)),
    assert(board(R,C,T,Color,Id, StackPosition)),

    get_adj_valid(board(R,C,T,Color,Id, StackPosition),AdjFree,MovesList).
    
get_adj_valid(board(R,C,T,Color,Id, StackPosition),[],[]).

valid_moves(board(R,C,q,Color,Id, StackPosition),Moves):-
    valid_queen_moves(board(R,C,q,Color,Id,StackPosition),MovesList),
    % print(MovesList),    
    list_to_set(MovesList,Moves),!.

valid_moves(board(R,C,b,Color,Id, StackPosition),Moves):-
    valid_beetle_moves(board(R,C,b,Color,Id,StackPosition),MovesList),
    % print(MovesList),    
    list_to_set(MovesList,Moves),!.

valid_moves(board(R,C,a,Color,Id, StackPosition),Moves):-
    valid_ant_moves(board(R,C,a,Color,Id,StackPosition),MovesList),
    % print(MovesList),    
    list_to_set(MovesList,Moves),!.

valid_moves(board(R,C,g,Color,Id, StackPosition),Moves):-
    valid_g_move(board(R,C,g,Color,Id,StackPosition),MovesList),
    % print(MovesList),    
    list_to_set(MovesList,Moves),!.

valid_moves(board(R,C,s,Color,Id, StackPosition),Moves):-
    valid_spider_moves(board(R,C,s,Color,Id,StackPosition),MovesList),
    %print(MovesList),    
    list_to_set(MovesList,Moves),!.

valid_moves(board(R, C, m, Color, Id, StackPosition), Moves) :-
    valid_mosquito_moves(board(R, C, m, Color, Id, StackPosition), M),
    list_to_set(M, TempMoves),
    fixed_mosquito_move(Id, TempMoves, Moves),
    !.

valid_moves(board(R, C, l, Color, Id, StackPosition), Moves) :-
    valid_ladybug_moves(board(R, C, l, Color, Id, StackPosition), M),
    list_to_set(M, Moves),
    !.
valid_moves(board(R, C, p, Color, Id, StackPosition), Moves) :-
    valid_pillbug_moves(board(R, C, p, Color, Id, StackPosition), M),
    list_to_set(M, Moves),
    !.

move(board(R, C, q, Color, Id, StackPosition), R_new, C_new):-
    move_queen(board(R, C, q, Color, Id, StackPosition), R_new, C_new), !.
move(board(R,C,b,Color,Id, StackPosition), R_new,C_new):-
    move_beetle(board(R,C,b,Color,Id, StackPosition),R_new,C_new),!.

move(board(R,C,a,Color,Id, StackPosition), R_new,C_new):-
    move_ant(board(R,C,a,Color,Id, StackPosition),R_new,C_new),!.


move(board(R,C,s,Color,Id, StackPosition), R_new,C_new):-
    move_spider(board(R,C,s,Color,Id, StackPosition),R_new,C_new),!.

move(board(R,C,l,Color,Id, StackPosition), R_new,C_new):-
    move_ladybug(board(R,C,l,Color,Id, StackPosition),R_new,C_new),!.

move(board(R,C,g,Color,Id, StackPosition), R_new,C_new):-
    move_grasshopper(board(R,C,g,Color,Id, StackPosition),R_new,C_new),!.

move(board(R,C,m,Color,Id, StackPosition), R_new,C_new):-
    move_mosquito(board(R,C,m,Color,Id, StackPosition),R_new,C_new),!.
move(board(R,C,p,Color,Id, StackPosition), A,B):-
    move_pillbug(board(R,C,p,Color,Id, StackPosition),A,B),!.
%Insert
move(board(C,R,T,Col,Id,SP),aux_board(Name,Type,XPixel,YPixel)):-
    assert(board(C,R,T,Col,Id,SP)),
    retract(aux_board(Name,Type,XPixel,YPixel)),
    retract(last_used_id(_)),
    assert(last_used_id(Id)),!.


% ----------------Queen Move-------------------------------------

valid_queen_moves(board(R,C,q,Color,Id,SP),MovesList):-
    print('queen'),
    board(R,C,q,Color,Id,SP),
    not(insect_above_me(board(R,C,q,Color,Id,SP))),
    %print('no insect above me. \n'),
    will_insect_not_break_hive(board(R,C,q,Color,Id,SP)),
    %print('will_insect_not_break_hive \n'),
    address(Addr),
    adj_path_out(R,C,Addr,MovesList1),   
    get_adj_valid(board(R,C,q,Color,Id,SP),MovesList1,MovesList).
valid_queen_moves(_,[]).
    
move_queen(board(R,C,q,Color,Id, StackPosition),R_new,C_new):-
    board(R,C,q,Color,Id, StackPosition),
    valid_moves(board(R,C,q,Color,Id, StackPosition), Moves),
    X = [R_new,C_new],
    %print(Moves),
    member(X,Moves),
    retract(board(R,C,q,Color,Id, StackPosition)),
    assert(board(R_new,C_new,q,Color,Id, 0)),    
    !.

move_queen(board(R,C,q,Color,Id, StackPosition),R_new,C_new):-
    format('Invalid move'),
    !,fail.

% ----------------------- Grasshopper Move ---------------------------------

valid_g_move(board(R, C, g, Color, Id, StackPosition), ValidPos) :-
    print('Grasshopper'),
    board(R, C, g, Color, Id, StackPosition),

    not(insect_above_me(board(R, C, g, Color, Id, StackPosition))),
    % format("no insect above me. \n"),

    will_insect_not_break_hive(board(R,C,g,Color,Id,StackPosition)),

    address(Address),
    get_ady_taken(R, C, Address, Adj),

    valid_g_move_aux(board(R, C, g, Color, Id, StackPosition), Adj , ValidPos).

valid_g_move(board(R, C, g, Color, Id, StackPosition), []).

valid_g_move_aux(board(R, C, g, Color, Id, StackPosition), [HAdj | TAdj], ValidPos) :-
    board(R, C, g, Color, Id, StackPosition),

    board(TR, TC, _, _, HAdj, _),

    DirectionRow is TR - R,
    DirectionCol is TC - C,
  
    format("Check from <~w ~w> with direction <~w ~w> \n", [TR, TC, DirectionRow, DirectionCol]),

    walk_for_direction(TR, TC, g, DirectionRow, DirectionCol, AuxValidPos1),

    % print(AuxValidPos),

    valid_g_move_aux(board(R, C, g, Color, Id, StackPosition), TAdj, AuxValidPos2),

    append(AuxValidPos1, AuxValidPos2, ValidPos),
    !.
    
valid_g_move_aux(board(_, _, g, _, _, _), [], []).

move_grasshopper(board(R,C,g,Color,Id, StackPosition),R_new,C_new):-
    board(R,C,g,Color,Id, StackPosition),
    valid_moves(board(R,C,g,Color,Id, StackPosition), Moves),
    X = [R_new,C_new],
    %print(Moves),
    member(X,Moves),
    retract(board(R,C,g,Color,Id, StackPosition)),
    assert(board(R_new,C_new,g,Color,Id, 0)),    
    !.

move_grasshopper(board(R,C,g,Color,Id, StackPosition),R_new,C_new):-
    format('Invalid move'),
    !,fail.

walk_for_direction(R, C, _, _, _, ValidPos) :-
    not(board(R, C, _, _, _, _)),

    append([], [[R, C]], ValidPos),
    !.

walk_for_direction(R, C, Type, DirectionRow, DirectionCol, ValidPos) :-
    board(R, C, _, _, _, _),
    
    NewRow is DirectionRow + R,
    NewCol is DirectionCol + C,

    walk_for_direction(NewRow, NewCol, Type, DirectionRow, DirectionCol, ValidPos).

% walk_for_direction(_, _, _, _, _, []).

%-----------------Beetle move----------------------------------------
% valid_beetle_moves(board(R,C,b,Color,Id,StackPosition),MovesList):-

valid_beetle_moves(board(R,C,b,Color,Id,SP),MovesList):-
    print('beetle'),
    board(R,C,b,Color,Id,SP),
    not(insect_above_me(board(R,C,b,Color,Id,SP))),
    %print('no insect above me. \n'),
    will_insect_not_break_hive(board(R,C,b,Color,Id,SP)),
    %print('will_insect_not_break_hive \n'),
    address(Addr),
    adj_path_out(R,C,Addr,Adjs),
    get_adj_valid(board(R,C,b,Color,Id,SP),Adjs, AdjMoves),
    get_ady_taken(R,C,Addr, AdjTakenId),
    get_location_by_id(AdjTakenId, AdjTakenLoc),
    append(AdjMoves,AdjTakenLoc,MovesList).
valid_beetle_moves(_,[]).

move_beetle(board(R,C,b,Color,Id, StackPosition),R_new,C_new):-
    board(R,C,b,Color,Id, StackPosition),
    valid_moves(board(R,C,b,Color,Id, StackPosition), Moves),
    X = [R_new,C_new],
    %print(Moves),
    member(X,Moves),
    retract(board(R,C,b,Color,Id, StackPosition)),
    (
        (highest_SP(R_new,C_new,SP),
        SP1 is SP+1);
        SP1=0
    ),
    assert(board(R_new,C_new,b,Color,Id, SP1)),
    !.

move_beetle(board(R,C,b,Color,Id, StackPosition),R_new,C_new):-
    format('Invalid move'),
    !,fail.
    
%--------------------------------------------------------------------

%-----------------Ant move----------------------------------------
valid_ant_moves(board(R,C,a,Color,Id,SP),MovesList):-
    print('ant'),
    board(R,C,a,Color,Id,SP),
    not(insect_above_me(board(R,C,a,Color,Id,SP))),
    %print('no insect above me. \n'),
    will_insect_not_break_hive(board(R,C,a,Color,Id,SP)),
    %print('will_insect_not_break_hive \n'),
    address(Addr),
    adj_path_out(R,C,Addr,Adjs),
    get_adj_valid(board(R,C,a,Color,Id,SP),Adjs, AdjMoves),
    AdjMoves \= [],

    retract(board(R,C,a,Color,Id,SP)),
    let_adj_do_their_thing(AdjMoves,[[R,C]],Visited, MovesList),
    assert(board(R,C,a,Color,Id,SP)).

valid_ant_moves(board(R,C,a,Color,Id,SP),[]).

move_ant(board(R,C,a,Color,Id,SP),R_new,C_new):-
    
    board(R,C,a,Color,Id,SP),
    valid_moves(board(R,C,a,Color,Id,SP),Moves),
    X = [R_new,C_new],
    %print(Moves),
    member(X,Moves),
    retract(board(R,C,a,Color,Id, StackPosition)),
    assert(board(R_new,C_new,a,Color,Id, SP)),
    !.
move_ant(board(R,C,a,Color,Id,SP),R_new,C_new):-
    format('Invalid move'),
    !,fail.

let_adj_do_their_thing([[R,C] |Adj],AuxVisited,Visited,Moves):-
    not(member([R,C],AuxVisited)),    
    address(Addr),
    adj_path_out(R,C,Addr,Adj1),
    adj_connected_to_board(Adj1,AdjValid),
    append([[R,C]],AuxVisited, AuxVisited1),
    let_adj_do_their_thing(AdjValid,AuxVisited1, Visited1, Moves1),
    let_adj_do_their_thing(Adj,Visited1,Visited,Moves2),
    append([[R,C]],Moves1,MovesAux),
    append(MovesAux,Moves2,Moves).



let_adj_do_their_thing([[R,C] |Adj],AuxVisited,Visited,Moves):-
    let_adj_do_their_thing(Adj,AuxVisited,Visited,Moves).

let_adj_do_their_thing([],AuxVisited,Visited,[]):-
    Visited = AuxVisited.


adj_connected_to_board([[R,C]| AdjT], AdjValid):-
    address(Addr),
    get_ady_taken(R, C, Addr, AdjTaken),
    AdjTaken \= [],
    adj_connected_to_board(AdjT,AdjValid1),
    append([[R,C]],AdjValid1,AdjValid).

adj_connected_to_board([[R,C]| AdjT], AdjValid):-
    adj_connected_to_board(AdjT,AdjValid).   

adj_connected_to_board([], []).


%---------------------------------------------------------------------
%-----------------------------Spider Move--------------------------
valid_spider_moves(board(R,C,s,Color,Id,SP),MovesList):-
    print('Spider'),
    board(R,C,s,Color,Id,SP),
    not(insect_above_me(board(R,C,s,Color,Id,SP))),
    %print('no insect above me. \n'),
    will_insect_not_break_hive(board(R,C,s,Color,Id,SP)),
    %print('will_insect_not_break_hive \n'),
    address(Addr),
    adj_path_out(R,C,Addr,Adjs),
    adj_with_taken_in_common(R,C,Adjs,AdjMoves),
    AdjMoves \= [],

    retract(board(R,C,s,Color,Id,SP)),
    let_adj_do_their_thing_spider(AdjMoves,[[R,C]],2 ,MovesList),
    assert(board(R,C,s,Color,Id,SP)).

valid_spider_moves(board(R,C,s,Color,Id,SP),[]).

move_spider(board(R,C,s,Color,Id,SP),R_new,C_new):-
    board(R,C,s,Color,Id,SP),
    valid_moves(board(R,C,s,Color,Id,SP),Moves),
    X = [R_new,C_new],
    %print(Moves),
    member(X,Moves),
    retract(board(R,C,s,Color,Id, StackPosition)),
    assert(board(R_new,C_new,s,Color,Id, SP)),
    !.
move_spider(board(R,C,s,Color,Id,SP),R_new,C_new):-
    format('Invalid move'),
    !,fail.

let_adj_do_their_thing_spider([[R,C] |Adj],AuxVisited,3,Moves):-
    not(member([R,C],AuxVisited)),    
    address(Addr),
    adj_path_out(R,C,Addr,Adj1),
    adj_with_taken_in_common(R,C,Adj1,AdjCommon),
    no_backtrack_adj(AuxVisited,AdjCommon, AdjValid),
    Moves = AdjValid,
    !.

let_adj_do_their_thing_spider([[R,C] |Adj],AuxVisited,3,Moves):-
    let_adj_do_their_thing_spider(Adj,AuxVisited,3,Moves).

let_adj_do_their_thing_spider([[R,C] |Adj],AuxVisited,Level,Moves):-
    not(member([R,C],AuxVisited)),    
    address(Addr),
    adj_path_out(R,C,Addr,Adj1),
    adj_with_taken_in_common(R,C,Adj1,AdjValid1),
    list_to_set(AdjValid1,AdjValid),
    append([[R,C]],AuxVisited, AuxVisited1),
    LevelAux is Level +1,
    let_adj_do_their_thing_spider(AdjValid,AuxVisited1, LevelAux, Moves1),
    let_adj_do_their_thing_spider(Adj,AuxVisited,Level,Moves2),    
    append(Moves1,Moves2,Moves).



let_adj_do_their_thing_spider([[R,C] |Adj],AuxVisited,Level,Moves):-
    let_adj_do_their_thing_spider(Adj,AuxVisited,Level,Moves).

let_adj_do_their_thing_spider([],AuxVisited,Level,[]).




adj_with_taken_in_common(R,C,[[R1,C1]| AdjT], AdjValid):-
    address(Addr),
    get_ady_taken(R1, C1, Addr, AdjTaken),
    get_ady_taken(R, C, Addr, AdjTaken1),
    common_elements(AdjTaken,AdjTaken1),
    adj_with_taken_in_common(R,C,AdjT,AdjValid1),
    append([[R1,C1]],AdjValid1,AdjValid).

adj_with_taken_in_common(R,C,[[R1,C1]| AdjT], AdjValid):-
    adj_with_taken_in_common(R,C,AdjT,AdjValid).   

adj_with_taken_in_common(R,C,[], []). 

no_backtrack_adj(AuxVisited,[H|AdjCommon], AdjValid):-
    not(member(H,AuxVisited)),
    no_backtrack_adj(AuxVisited,AdjCommon,AdjVAlid1),
    append([H],AdjVAlid1,AdjValid).
no_backtrack_adj(AuxVisited,[H|AdjCommon], AdjValid):-
    no_backtrack_adj(AuxVisited,AdjCommon, AdjValid).
no_backtrack_adj(AuxVisited,[], []).

%---------------------LadyBug Move---------------------------------
valid_ladybug_moves(board(R,C,l,Color,Id,SP),MovesList):-
    print('LadyBug'),
    board(R,C,l,Color,Id,SP),
    not(insect_above_me(board(R,C,l,Color,Id,SP))),
    %print('no insect above me. \n'),
    will_insect_not_break_hive(board(R,C,l,Color,Id,SP)),
    %print('will_insect_not_break_hive \n'),
    address(Addr),
    get_ady_taken(R,C,Addr,Ans),
    get_location_by_id(Ans,AdjTaken),
    AdjTaken \=[],

    % retract(board(R,C,l,Color,Id,SP)),
    let_adj_do_their_thing_ladybug(AdjTaken,[[R,C]],2 ,Moves12),
    list_to_set(Moves12,MovesList).
    % assert(board(R,C,l,Color,Id,SP)).

valid_ladybug_moves(board(R,C,l,Color,Id,SP),[]).

move_ladybug(board(R,C,l,Color,Id,SP),R_new,C_new):-
    board(R,C,l,Color,Id,SP),
    valid_moves(board(R,C,l,Color,Id,SP),Moves),
    X = [R_new,C_new],
    %print(Moves),
    member(X,Moves),
    retract(board(R,C,l,Color,Id, StackPosition)),
    assert(board(R_new,C_new,l,Color,Id, SP)),
    !.
move_ladybug(board(R,C,l,Color,Id,SP),R_new,C_new):-
    format('Invalid move'),
    !,fail.

let_adj_do_their_thing_ladybug([[R,C] |Adj],AuxVisited,3,Moves):-
    not(member([R,C],AuxVisited)),    
    address(Addr),
    get_ady_free(R,C,Addr,AdjFree), 
    let_adj_do_their_thing_ladybug(Adj,AuxVisited,3,Moves1),
    append(AdjFree,Moves1,Moves),
    !.

let_adj_do_their_thing_ladybug([[R,C] |Adj],AuxVisited,3,Moves):-
    let_adj_do_their_thing_ladybug(Adj,AuxVisited,3,Moves).

let_adj_do_their_thing_ladybug([[R,C] |Adj],AuxVisited,Level,Moves):-
    not(member([R,C],AuxVisited)),    
    address(Addr),
    get_ady_taken(R,C,Addr,AdjTakenIds),
    get_location_by_id(AdjTakenIds,AdjTaken1),
    list_to_set(AdjTaken1,AdjTaken),
    append([[R,C]],AuxVisited, AuxVisited1),
    LevelAux is Level +1,
    let_adj_do_their_thing_ladybug(AdjTaken,AuxVisited1, LevelAux, Moves1),
    let_adj_do_their_thing_ladybug(Adj,AuxVisited,Level,Moves2),    
    append(Moves1,Moves2,Moves).



let_adj_do_their_thing_ladybug([[R,C] |Adj],AuxVisited,Level,Moves):-
    let_adj_do_their_thing_ladybug(Adj,AuxVisited,Level,Moves).

let_adj_do_their_thing_ladybug([],AuxVisited,Level,[]).


% ----------------------------------------------- Mosquito Moves ------------------------------------------
valid_mosquito_moves(board(R, C, m, Color, Id, StackPosition), M) :-
    print('mosquito'),
    not(insect_above_me(board(R, C, m, Color, Id, StackPosition))),
    
    format("no insect above me. \n"),

    will_insect_not_break_hive(board(R, C, m, Color, Id, StackPosition)),

    address(Addr),

    get_ady_taken(R, C, Addr, AdjIds),

    remove_m_from_AdjIds(AdjIds, FilterAdjIds),

    valid_mosquito_moves_aux(FilterAdjIds, M).
valid_mosquito_moves(board(R, C, m, Color, Id, StackPosition), []). 
move_mosquito(board(R,C,m,Color,Id,SP),R_new,C_new):-
    board(R,C,m,Color,Id,SP),
    valid_moves(board(R,C,m,Color,Id,SP),Moves),
    X = [R_new,C_new],
    %print(Moves),
    member(X,Moves),
    retract(board(R,C,m,Color,Id, StackPosition)),
    assert(board(R_new,C_new,m,Color,Id, SP)),
    !.
move_mosquito(board(R,C,m,Color,Id,SP),R_new,C_new):-
    format('Invalid move'),
    !,fail.


valid_mosquito_moves_aux([], []) :- !.

valid_mosquito_moves_aux([Id | T], M) :-
    board(R, C, Type, Color, Id, Sp),

    valid_moves(board(R, C, Type, Color, Id, Sp), AdjValidMoves),

    valid_mosquito_moves_aux(T, TM),

    append(AdjValidMoves, TM, M),
    !.

remove_m_from_AdjIds([], []) :- !.

remove_m_from_AdjIds([Id | T], FilterAdjIds) :-
    board(_, _, Type, _, Id, _),

    Type = m,

    remove_m_from_AdjIds(T, FilterAdjIds),
    !.

remove_m_from_AdjIds([Id | T], [Id | TT]) :-
    remove_m_from_AdjIds(T, TT).

fixed_mosquito_move(Id, TempMoves, Moves) :-
    board(R, C, Type, Color, Id, StackPosition),
    
    retract(board(R, C, Type, Color, Id, StackPosition)),

    fixed_mosquito_move_aux(board(R, C, Type, Color, Id, StackPosition), TempMoves, Moves),

    assert(board(R, C, Type, Color, Id, StackPosition)).

fixed_mosquito_move_aux(board(_, _, _, _, _, _), [], []).

fixed_mosquito_move_aux(board(R, C, Type, Color, Id, StackPosition), [[TR, TC | _] | T], Moves) :-

    assert(board(TR, TC, Type, Color, Id, StackPosition)),

    is_valid_board(),

    retract(board(TR, TC, Type, Color, Id, StackPosition)),

    fixed_mosquito_move_aux(board(R, C, Type, Color, Id, StackPosition), T, TempMoves),

    append([[TR, TC]], TempMoves, Moves),
    !.

fixed_mosquito_move_aux(board(R, C, Type, Color, Id, StackPosition), [[TR, TC | _] | T], Moves) :-
    retract(board(TR, TC, Type, Color, Id, StackPosition)),

    fixed_mosquito_move_aux(board(R, C, Type, Color, Id, StackPosition), T, Moves).


%------------------------------------------------------------------

%----------------------PillBug-------------------------------------

valid_pillbug_moves(board(R,C,p,Color,Id,SP),MovesList):-
    print('pillbug'),
    board(R,C,p,Color,Id,SP),
    not(insect_above_me(board(R,C,p,Color,Id,SP))),
    %print('no insect above me. \n'),
    %print('will_insect_not_break_hive \n'),
    address(Addr),
    adj_path_out(R,C,Addr,MovesList1),  
    get_ady_taken(R,C,Addr,Ids),
    get_location_by_id(Ids,AdjTaken),
    drag_adj_taken_moves(MovesList1,AdjTaken,MovesList2), 
    (
        (        
            will_insect_not_break_hive(board(R,C,p,Color,Id,SP)),
            get_adj_valid(board(R,C,p,Color,Id,SP),MovesList1,MovesList3),
            append(MovesList3,MovesList2,MovesList)    
        );
        (
             MovesList = MovesList2
        )
    ).


valid_pillbug_moves(_,[]).

move_pillbug(board(R,C,p,Color,Id, StackPosition),board(R1,C1,T,Color,Id,SP),[R2,C2]):-
    board(R,C,p,Color,Id, StackPosition),
    valid_moves(board(R,C,p,Color,Id, StackPosition), Moves),
    X = [board(R1,C1,T,Color,Id,SP),[R2,C2]],
    %print(Moves),
    member(X,Moves),
    retract(board(R1,C1,T,Color,Id, SP)),
    assert(board(R2,C2,T,Color,Id, 0)),    
    !.

move_pillbug(board(R,C,p,Color,Id, StackPosition),R_new,C_new):-
    board(R,C,p,Color,Id, StackPosition),
    valid_moves(board(R,C,p,Color,Id, StackPosition), Moves),
    X = [R_new,C_new],
    %print(Moves),
    member(X,Moves),
    retract(board(R,C,p,Color,Id, StackPosition)),
    assert(board(R_new,C_new,T,Color,Id, 0)),    
    !.
move_pillbug(board(R,C,g,Color,Id, StackPosition),_,_):-
    format('Invalid move'),
    !,fail.

drag_adj_taken_moves(MovesL,[[R1,C1]|AdjTaken],Moves):-
    process_adj_taken(MovesL,[R1,C1],Moves1),
    drag_adj_taken_moves(MovesL, AdjTaken, Moves2),
    append(Moves1,Moves2,Moves).
drag_adj_taken_moves(MovesL,[],[]).

process_adj_taken([[R,C]|MovesL], [R1,C1],Moves):-
    highest_SP(R1,C1,SP),
    board(R1,C1,T,Color,Id,SP),
    will_insect_not_break_hive(board(R1,C1,T,Color,Id,SP)),
    not(last_chip_moved_id(Id)),
    M = [board(R1,C1,T,Color,Id,SP),[R,C]],
    process_adj_taken(MovesL, [R1, C1], Moves2),
    append([M],Moves2,Moves).
process_adj_taken([[R,C]|MovesL], [R1,C1],Moves):-
    process_adj_taken(MovesL, [R1,C1],Moves).
process_adj_taken([], [R1,C1],[]).



%------------------------------------------------------------------

    

 