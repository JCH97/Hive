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
% card-place => name, X, Y

:- dynamic [card_place/3].

start :-
    width(W),
    height(H),
    new(Window, window("Hive", size(H, W))),
    send(Window, open),
    draw_board(Window),
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
    send(Win, display, Fig, Pos),
    get(Pos, x, X),
    get(Pos, y, Y),
    assert(card_place(Fig, X, Y)).
    % TODO: add card to board => Set last used Id when testing

% si hay que poner una carta de las que hay afuera del tablero
click_event_handler(Window, Pos) :-
    get_card_clicked(Pos, [X, Y | _]),
    
    % width(W),
    % height(H),
    % new(Window, window("Hive", size(H, W))),
    
    (X =:= 0; X =:= 945),
    !,
    handle_card_out_game(Window, X, Y, Pos).


click_event_handler(Window, Pos) :-
    get_card_clicked(Pos, [X, Y | _]),
    
    handle_card(Window, X, Y, Pos).

% se usa para manejar las cartas de seleccion de bichos.
handle_card_out_game(Window, X, Y, Pos) :-   
    aux_board(_, _, _, Color, _, _, X, Y),
    
    where_place_piece(Color, Ans),

    length(Ans, L),
    L > 0,

    draw_list(Window, Ans, Pos).

% primera vez que se pone una carta en el tablero
handle_card_out_game(Window, X, Y, _) :-
    aux_board(_, _, Type, Color, _, _, X, Y),

    start_pos(S),
    new_image(Window, _, Type, S),
    add_entry_in_board(0, 0, Type, Color, _, 0, X, Y).

handle_card(Window, X, Y, Pos) :- 
    board(R, C, Type, Color, Id, Stack, X, Y), 

    valid_moves(board(R, C, Type, Color, Id, Stack, X, Y), MovesList),
    draw_list(Window, MovesList, Pos).

draw_list(_, [], _) :- !.

draw_list(Window, [[R, C | _] | T], Pos) :-
    get(Pos, x, OldX),
    get(Pos, y, OldY),
    TempX is (C / 2) * 55,
    TempY is R * 55,
    X is TempX + OldX,
    Y is TempY + OldY,
    new_image(Window, _, aw, point(Y, X)),
    draw_list(Window, T, Pos).

% retorna la carta a la que se le hizo clic, sino existe carta entonces falla y retorna falso
get_card_clicked(ClickPosition, Ans) :- 
    findall([X, Y], card_place(_, X, Y), Aux),
    get_card_clicked_aux(Aux, ClickPosition, Ans).

get_card_clicked_aux([[SX, SY | _] | _], ClickPosition, [AnsX, AnsY]) :-
    get(ClickPosition, x, X),
    get(ClickPosition, y, Y),
    size_cards(SZ),
    EX is SX + SZ,
    EY is SY + SZ,
    X > SX, X < EX, 
    Y > SY, Y < EY,
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
    assert(aux_board(-100, -100, H, w, -1, 0, RS, CS)),
    NewCS is CS + SZ,
    draw_board_white_cards(Window, SZ, T, RS, NewCS).

draw_board_black_cards(_, _, [], _, _) :- !.

draw_board_black_cards(Window, SZ, [H | T], RS, CS) :-
    new_image(Window, _, H, point(RS, CS)),
    assert(aux_board(-100, -100, H, b, -1, 0, RS, CS)),
    NewCS is CS + SZ,
    draw_board_black_cards(Window, SZ, T, RS, NewCS).