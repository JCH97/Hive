:- use_module(library(pce)).
:- pce_image_directory('./resources').

% dimentions: 55 x 55

:- [
    './move.pl',
    './board.pl'
].

height(1000).
width(600).
size_cards(55).
start_pos(point(455, 300)).
left_arrow_pos(55, 0).
right_arrow_pos(890, 0).

resource(bw, image, image('bw-reduce.jpg')).
resource(aw, image, image('aw-reduce.jpg')).
resource(bb, image, image('bb-reduce.jpg')).
resource(bw, image, image('bw-reduce.jpg')).
resource(gb, image, image('gb-reduce.jpg')).
resource(gw, image, image('gw-reduce.jpg')).
resource(qb, image, image('qb-reduce.jpg')).
resource(qw, image, image('qw-reduce.jpg')).
resource(sb, image, image('sb-reduce.jpg')).
resource(sw, image, image('sw-reduce.jpg')).
resource(ab, image, image('ab-reduce.jpg')).
resource(white, image, image('white.jpg')).
resource(valid, image, image('valid.jpg')).
resource(leftArrow, image, image('left-arrow.jpg')).
resource(rightArrow, image, image('right-arrow.jpg')).

% card-place => name, Id, X, Y => para almacenar las cartas puestas en el tablero
% aux_board => name, Color, X, Y => para almacenar las cartas con las que se juega
% drawed_positions X, Y => se usa para almacenar las posiciones que tienen la imagen valid, para despues saber cuales
    % cuales son las posiciones validas para el proximo click
% clicked_card Name, X, Y, IsFromBoard => almacena el nombre y la posicion de la carta sobre la que se hizo clic y que por tanto es la proxima que hay que pintar en el tablero .
:- dynamic [card_place/4, aux_board/4, drawed_positions/2, clicked_card/4].

draw_test(Window) :- 
    new_image(Window, _, gb, point(455, 300)),
    % new_image(Window, _, qb, point(427.5, 355)),
    new_image(Window, _, qb, point(482.5, 245)).
    % new_image(Window, _, qb, point(482.5, 355)),
    % new_image(Window, _, qb, point(482.5, 245)),
    % new_image(Window, _, qb, point(455, 190)),
    % new_image(Window, _, qb, point(427.5, 245)).


start :-
    width(W),
    height(H),
    new(Window, window("Hive", size(H, W))),
    send(Window, open),
    draw_board(Window),
    draw_arrows(Window),
    % draw_test(Window),
    send(Window, recogniser, click_gesture(left,
                                            '',
                                            single,
                                            message(@prolog, click_event_handler, Window, @event?position))).
move_image_to(Figure, X, Y) :-
    send(Figure, status, 0),
    send(Figure, move, point(X, Y)).

move_image(Figure, X, Y) :-
    send(Figure, relative_move, point(X, Y)).

new_image(Win, Fig, Imagen, Pos) :-
    new(Fig, figure),
    new(Bitmap, bitmap(resource(Imagen),@on)),
    send(Bitmap, name, 1),
    send(Fig, display, Bitmap),
    send(Fig, status, 1),
    send(Win, display, Fig, Pos).
    % TODO: add card to board => Set last used Id when testing


% si hay que poner una carta de las que hay afuera del tablero
click_event_handler(Window, Pos) :-
    get_card_clicked(Pos, [X, Y | _]),
    
    % width(W),
    % height(H),
    % new(Window, window("Hive", size(H, W))),
    
    (X =:= 0; X =:= 945),

    !,

    clean_drawed_positions(Window),
    handle_card_out_game(Window, X, Y, Pos, DrawedPositions), 
    aux_board(Name, _, X, Y),
    save_drawed_positions(Name, X, Y, 0, DrawedPositions),
    draw_arrows(Window).
    

% si se va a mover una carta del tablero
click_event_handler(Window, Pos) :-
    get_card_clicked(Pos, [X, Y | _]),
    
    handle_card(Window, X, Y, DrawedPositions), 
    % el parametro name indica el nombre de la carta sobre la que se hizo click, es decir la carta que se kiere mover (ya sea de las que estan afuera o del tablero)

    clean_drawed_positions(Window),

    choose_card_to_move(X, Y, Name),

    % aux_board(Name, _, X, Y),
    save_drawed_positions(Name, X, Y, 1, DrawedPositions),

    draw_arrows(Window).


% se usa para manejar las cartas que estan fuera del tablero 
handle_card_out_game(Window, X, Y, _, DrawedPositions) :-   
    aux_board(_, Color, X, Y),
    
    fix_color(Color, NewColor),

    findall(Id, board(_, _, _, NewColor, Id, _), SameColorIds),
    % SameColor indica todas las cartas en el tablero con el mismo color que sobre la que se hizo click

    % where_place_piece(NewColor, PathOut, StartPoint),

    length(SameColorIds, L),
    L > 0,

    draw_list(Window, SameColorIds, DrawedPositions).

    % remove las cartas dentro de TemporalDrawedPositions que estan adj a otras con color distinto a NewColor
    % filter_drawed_positions(TemporalDrawedPositions, NewColor, DrawedPositions).

% primera vez que se pone una carta en el tablero
handle_card_out_game(Window, X, Y, _, []) :-
    aux_board(Type, Color, X, Y),

    start_pos(S),
    new_image(Window, _, Type, S),
    get(S, x, Nx),
    get(S, y, Ny),
    new_image(Window, _, white, point(X, Y)),
    retract(aux_board(Type, Color, X, Y)),
    make_entry_in_board(0, 0, Type, Color, 0, Nx, Ny),
    draw_arrows(Window),
    !.

% poner una carta en el tablero de juego una vez que se haya seleccionado la carta que se kiere mover
handle_card(Window, X, Y, _) :- 

     % verificando que sea el predicado correcto
    findall([TX, TY], drawed_positions(TX, TY), Ans),
    length(Ans, L),
    L > 0,

    member([X, Y], Ans),

    clicked_card(Name, CR, CC, IsFromBoard),

    clean_drawed_positions(Window),
    
    new_image(Window, _, Name, point(X, Y)),

    get_board_position_with_pixeles(X, Y, R, C),
    
    retract_from_aux_board_or_remove_from_board(Window, Name, CR, CC, IsFromBoard),

    sub_atom(Name, 1, 1, _, Color),

    get_stack_position(R, C, SP),

    make_entry_in_board(R, C, Name, Color,  SP, X, Y),

    draw_arrows(Window), 
    !.

% para mover una carta del tablero 
handle_card(Window, X, Y, DrawedPositions) :-
    get_board_position_with_pixeles(X, Y, R, C),

    board(R, C, Type, Color, Id, SP),

    valid_moves(board(R, C, Type, Color, Id, SP), DrawedPositionsBoardCoordenates),

    draw_list_aux(Window, DrawedPositionsBoardCoordenates, point(X, Y), DrawedPositions).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_stack_position(R, C, SP) :-
    board(R, C, _, _, _, TSP),
    SP is TSP + 1,
    !.

get_stack_position(_, _, 0).

get_board_position_with_pixeles(X, Y, R, C) :-
    start_pos(StartPos),
    get(StartPos, x, SX),
    get(StartPos, y, SY),
    TempR is ((X - SX) / 55) * 2,
    TempC is (Y - SY) / 55,
    R is round(TempR),
    C is round(TempC).


draw_list(_, [], []) :- !.

draw_list(Window, [Id | T], DrawedPositions) :-
    card_place(_, Id, X, Y),
    board(R, C, _, Color, Id, _),
    where_place_piece(R, C, Color, PathOut),
    draw_list_aux(Window, PathOut, point(X, Y), Aux1DrawedPositions),
    draw_list(Window, T, Aux2DrawedPositions),
    append(Aux1DrawedPositions, Aux2DrawedPositions, DrawedPositions).
    

draw_list_aux(_, [], _, []) :- !.

draw_list_aux(Window, [[C, R | _] | T], Pos, [[X, Y] | TT]) :-
    start_pos(StartPos),
    get(StartPos, x, SX),
    get(StartPos, y, SY),
    X is (C / 2) * 55 + SX,
    Y is R * 55 + SY,
    % X is TempX + OldX,
    % Y is TempY + OldY,
    % aqui siempre se pinta valid xq siempre vamos a pintar la lista de las posiciones validas
    new_image(Window, _, valid, point(X, Y)),
    draw_list_aux(Window, T, Pos, TT).

% retorna la carta a la que se le hizo clic, sino existe carta entonces falla y retorna falso
% si la carta fue del tablero
get_card_clicked(ClickPosition, Ans) :- 
    findall([X, Y], card_place(_, _, X, Y), Aux),
    get_card_clicked_aux(Aux, ClickPosition, Ans),
    !.

% si la carta fue de las de seleccion
get_card_clicked(ClickPosition, Ans) :-
    findall([X, Y], aux_board(_, _, X, Y), Aux),
    get_card_clicked_aux(Aux, ClickPosition, Ans),
    !.

get_card_clicked(ClickPosition, Ans) :-
    findall([X, Y], drawed_positions(X, Y), Aux),
    get_card_clicked_aux(Aux, ClickPosition, Ans),
    !.
    

get_card_clicked_aux([[SX, SY | _] | _], ClickPosition, [AnsX, AnsY]) :-
    get(ClickPosition, x, X),
    get(ClickPosition, y, Y),
    size_cards(SZ),
    EX is SX + SZ,
    EY is SY + SZ,
    X >= SX, X =< EX, 
    Y >= SY, Y =< EY,
    AnsX is SX,
    AnsY is SY,
    !.

get_card_clicked_aux([[_, _ | _] | []], _, []).

get_card_clicked_aux([[_, _ | _] | T], ClickPosition, Ans) :-
    get_card_clicked_aux(T, ClickPosition, Ans).

draw_board(Window) :-
    size_cards(SZ),
    draw_board_white_cards(Window, SZ, [
            qw, aw, aw, aw, gw, gw, gw, bw, bw, sw, sw
        ], 0, 0),
    draw_board_black_cards(Window, SZ, [
            qb, ab, ab, ab, gb, gb, gb, bb, bb, sb, sb
        ], 945, 0).

draw_board_white_cards(_, _, [], _, _) :- !.

draw_board_white_cards(Window, SZ, [H | T], RS, CS) :-
    new_image(Window, _, H, point(RS, CS)),
    assert(aux_board(H, w, RS, CS)),
    NewCS is CS + SZ,
    draw_board_white_cards(Window, SZ, T, RS, NewCS).

draw_board_black_cards(_, _, [], _, _) :- !.

draw_board_black_cards(Window, SZ, [H | T], RS, CS) :-
    new_image(Window, _, H, point(RS, CS)),
    assert(aux_board(H, b, RS, CS)),
    NewCS is CS + SZ,
    draw_board_black_cards(Window, SZ, T, RS, NewCS).

clean_drawed_positions(Window) :-
    findall([X, Y], drawed_positions(X, Y), Ans),
    retractall(drawed_positions(_, _)),
    retractall(clicked_card(_, _, _, _)),
    clean_drawed_positions_aux(Window, Ans).

clean_drawed_positions_aux(_, []) :- !.

clean_drawed_positions_aux(Window, [[X, Y | _] | T]) :- 
    new_image(Window, _, white, point(X, Y)),
    clean_drawed_positions_aux(Window, T).

save_drawed_positions(Name, X, Y, IsFromBoard, DrawedPositions) :-
    save_drawed_positions_aux(DrawedPositions),
    save_clicked_card(Name, X, Y, IsFromBoard).

save_clicked_card(Name, X, Y, IsFromBoard) :-
    findall(_, drawed_positions(_, _), Ans),
    length(Ans, L),
    L > 0,
    assert(clicked_card(Name, X, Y, IsFromBoard)),
    !.

save_clicked_card(_, _, _, _) :- !.


save_drawed_positions_aux([]) :- !.

save_drawed_positions_aux([[X, Y | _] | T]) :-
    assert(drawed_positions(X, Y)),
    save_drawed_positions_aux(T).

fix_color(Color, NewColor) :-
    plays(P),
    P > 1,
    NewColor = Color, 
    !.

fix_color(_, NewColor) :-
    NewColor = w.

make_entry_in_board(R, C, Type, Color, SP, X, Y) :-
    % last_used_id(TId),
    % NewId is TId + 1,
    % assert(card_place(Type, NewId, X, Y)),
    map_from_compuest_type(Type, NewType),
    add_entry_in_board(R, C, NewType, Color, SP, NewId),
    assert(card_place(Type, NewId, X, Y)).

draw_arrows(Window) :-
    plays(P),
    R is P mod 2,
    R =:= 0,
    
    % draw left arrow

    right_arrow_pos(RX, RY),
    left_arrow_pos(LX, LY),
    new_image(Window, _, white, point(RX, RY)),
    new_image(Window, _, leftArrow, point(LX, LY)),
    !.

draw_arrows(Window) :-
    % draw right arrow

    left_arrow_pos(LX, LY),
    right_arrow_pos(RX, RY),
    new_image(Window, _, white, point(LX, LY)),
    new_image(Window, _, rightArrow, point(RX, RY)).

map_from_compuest_type(Type, NewType) :- 
    sub_atom(Type, 0, 1, _, NewType).

map_to_compuest_type(Type, Color, NewType) :-
    atom_concat(Type, Color, NewType).

choose_card_to_move(X, Y, Name) :-
    aux_board(Name, _, X, Y), 
    !.

choose_card_to_move(X, Y, Name) :-
    card_place(Name, _, X, Y),
    !.

% la carta que hay que quitar es de las que estan afuera del tablero para seleccionar
retract_from_aux_board_or_remove_from_board(Window, Name, X, Y, IsFromBoard) :-
    IsFromBoard =:= 0, 

    aux_board(Name, Color, X, Y),
    retract(aux_board(Name, Color, X, Y)),
    new_image(Window, _, white, point(X, Y)),
    !.

% la carta que hay que quitar es de las que estan puestas en el tablero.
retract_from_aux_board_or_remove_from_board(Window, Name, X, Y, _) :-
    card_place(Name, Id, X, Y),

    % map_from_compuest_type(Name, Type),

    retract(board(_, _, _, _, Id, _)),
    retract(card_place(_, Id, _, _)),

    fix_last_used_id(),

    new_image(Window, _, white, point(X, Y)).

filter_drawed_positions([], _, []) :- !.

filter_drawed_positions([[X, Y | _] | T], Color, [[AX, AY | _] | TT]) :-
    get_board_position_with_pixeles(X, Y, R, C),
    address(Addr),
    get_ady_taken(R, C, Addr, IdsAdy),
    findall([TR, TC, TColor], (member(Id, IdsAdy), board(TR, TC, _, TColor, Id, _)), TAns),
    valids_colors(Color, TAns),
    AX is X,
    AY is Y,
    filter_drawed_positions(T, Color, TT).

filter_drawed_positions([[_, _ | _] | T], Color, Ans) :-
    filter_drawed_positions(T, Color, Ans).

valids_colors(_, []) :- !.

valids_colors(Color, [[_, _, TColor | _] | T]) :-
    Color = TColor,
    valids_colors(Color, T).

get_from_out_cards_for_ia(Color, Ans) :-
    findall(Name, aux_board(Name, Color, _, _), TempAns),
    sort(TempAns, WithOutRepeted),
    get_from_out_cards_for_ia_aux(WithOutRepeted, Ans).

get_from_out_cards_for_ia_aux([], []) :- !.

get_from_out_cards_for_ia_aux([N | T], Ans) :-
    aux_board(N, _, X, Y),
    get_from_out_cards_for_ia_aux(T, AnsTemp),
    append([[X, Y]], AnsTemp, Ans),
    !.

get_valid_insert_pos_ia(Color, Ans) :-
    findall(Id, board(_, _, _, Color, Id, _), SameColorIds),
    get_valid_insert_pos_ia_aux(SameColorIds, Ans).

get_valid_insert_pos_ia_aux([], []) :- !.

get_valid_insert_pos_ia_aux([Id | T], ValidsPos) :-
    board(R, C, _, Color, Id, _),
    where_place_piece(R, C, Color, PathOut),
    get_valid_insert_pos_ia_aux(T, Aux2DrawedPositions),
    append(PathOut, Aux2DrawedPositions, ValidsPos).