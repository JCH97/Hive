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
% card_to_draw Name => almacena el nombre de la carta de las que estan afuera que se eligio para entrar al tablero.
:- dynamic [card_place/4, aux_board/4, drawed_positions/2, card_to_draw/1].

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
    save_drawed_positions(Name, DrawedPositions),
    draw_arrows(Window).
    

% si se va a mover una carta del tablero
click_event_handler(Window, Pos) :-
    get_card_clicked(Pos, [X, Y | _]),
    
    handle_card(Window, X, Y, DrawedPositions),

    clean_drawed_positions(Window),
    aux_board(Name, _, X, Y),
    save_drawed_positions(Name, DrawedPositions),

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

    card_to_draw(Name),

    clean_drawed_positions(Window),
    
    new_image(Window, _, Name, point(X, Y)),

    get_board_position_with_pixeles(X, Y, R, C),
    
    aux_board(Name, Color, BX, BY),
    retract(aux_board(Name, Color, BX, BY)),
    new_image(Window, _, white, point(BX, BY)),
    make_entry_in_board(R, C, Name, Color,  0, X, Y),

    draw_arrows(Window), 
    !.


% para mover una carta del tablero 
handle_card(Window, X, Y, DrawedPositions) :-
    get_board_position_with_pixeles(X, Y, R, C),

    board(R, C, Type, Color, Id, SP),

    valid_moves(board(R, C, Type, Color, Id, SP), Moves),
    
    length(Moves, L),
    L > 0,

    draw_list(Window, Moves, DrawedPositions).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    board(R, C, _, _, Id, _),
    where_place_piece(R, C, PathOut),
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
    retractall(card_to_draw(_)),
    clean_drawed_positions_aux(Window, Ans).

clean_drawed_positions_aux(_, []) :- !.

clean_drawed_positions_aux(Window, [[X, Y | _] | T]) :- 
    new_image(Window, _, white, point(X, Y)),
    clean_drawed_positions_aux(Window, T).

save_drawed_positions(Name, DrawedPositions) :-
    assert(card_to_draw(Name)),
    save_drawed_positions_aux(DrawedPositions).

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
    last_used_id(TId),
    NewId is TId + 1,
    assert(card_place(Type, NewId, X, Y)),
    map_from_compuest_type(Type, NewType),
    add_entry_in_board(R, C, NewType, Color, NewId, SP).

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