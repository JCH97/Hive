:- use_module(library(pce)).
:- pce_image_directory('./resources').

% dimentions: 55 x 55

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
resource(test, image, image('test.ico')).

% card-place => name, X, Y

:- dynamic [card_place/3].

mover_imagen_a(Figure, X, Y) :-
    send(Figure, status, 0),
    send(Figure, move, point(X, Y)).

mover_imagen(Figure, X, Y) :-
    send(Figure, relative_move, point(X, Y)).

nueva_imagen(Ventana, Figura, Imagen, Posicion) :-
    new(Figura, figure),
    new(Bitmap, bitmap(resource(Imagen),@on)),
    send(Bitmap, name, 1),
    send(Figura, display, Bitmap),
    send(Figura, status, 1),
    send(Ventana, display, Figura, Posicion),
    get(Posicion, x, X),
    get(Posicion, y, Y),
    assert(card_place(Figura, X, Y)).


% retorna la carta a la que se le hizo clic, sino existe carta entonces falla y retorna falso
get_card_clicked(ClickPosition, Ans) :- 
    findall([X, Y], card_place(_, X, Y), Aux),
    get_card_clicked_aux(Aux, ClickPosition, Ans).

get_card_clicked_aux([[SX, SY | _] | _], ClickPosition, [AnsX, AnsY]) :-
    get(ClickPosition, x, X),
    get(ClickPosition, y, Y), 
    EX is SX + 55,
    EY is SY + 55,
    X > SX, X < EX, 
    Y > SY, Y < EY,
    AnsX is SX,
    AnsY is SY,
    !.

get_card_clicked_aux([[_, _ | _] | []], _, []).

get_card_clicked_aux([[_, _ | _] | T], ClickPosition, Ans) :-
    get_card_clicked_aux(T, ClickPosition, Ans).


start :-
    new(Window, window("Hive", size(1000, 600))),
    send(Window, open),
    nueva_imagen(Window, _, aw, point(25, 0)),
    nueva_imagen(Window, _, aw, point(0, 50)),
    nueva_imagen(Window, _, ab, point(50, 50)),
    send(Window, recogniser, click_gesture(left,
                                            '',
                                            single,
                                            message(@prolog, click_event_handler, Window, @event?position))).
click_event_handler(Window, Pos) :-
    get(Pos, x, X), 
    get(Pos, y, Y),
    get_card_clicked(Pos, [X, Y | _]),
    handle_card(Window, X, Y).
    % new(L, label(label1, X)),
    % new(L1, label(label2, Y)),
    % send(Window, display, L, point(100, 100)),
    % send(Window, display, L1, point(110, 110)).
    % get(Pos, x, X),
    % get(Pos, y, Y),
    % new(L, label(label1, X)),
    % new(L1, label(label2, Y)),
    % random(50, 500, R1),
    % random(50, 500, R2),
    % T is 100 + R1,
    % T2 is 110 + R2,
    % send(Window, display, L, point(T, 100)),
    % send(Window, display, L1, point(T2, 110)).
    % nueva_imagen(Window, _, white, Pos).
    
handle_card(Window, X, Y).