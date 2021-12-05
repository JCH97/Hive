%% hormigas => a         arannas => s
%% saltamontes => aa     abeja reina => q
%% escarabajos => b

:- [
    '/workstate_prolog/Hive/utils.pl',
    '/workstate_prolog/Hive/board.pl',
    '/workstate_prolog/Hive/move.pl'
].

:- dynamic [board/6, last_used_id/1, visited/1, count/1].


% % get location by Type
% place_piece(R, C, Type, Color) :-
%     not(board(_, _, Type, Color, _, _)),
%     last_used_id(X),
%     Id is X + 1,
%     retract(last_used_id(X)),
%     assert(last_used_id(Id)),
%     assert(board(R, C, Type, Color, Id)),
%     !.

% get_ady_taken2(R, C, Ans) :- 
%     row_directions(Row),
%     columns_directions(Col),
%     member(Hr, row_directions),
%     member(Hc, columns_directions),
%     get_ady_taken2(R, C, Ans, Row, Col).
%     %member(Z, Ans).         % TODO: remove this line, testing purposes.

% get_ady_taken2(_, _, _, [], []) :- !.

% get_ady_taken2(R, C, [H | T], [Hr | Tr], [Hc | Tc]) :-
%     Nr is Hr + R,
%     Nc is Hc + C,
%     board(Nr, Nc, _, _, H),
%     get_ady_taken2(R, C, T, Tr, Tc),
%     !.

% get_ady_taken2(R, C, A, [_ | Tr], [_ | Tc]) :- get_ady_taken2(R, C, A, Tr, Tc).

