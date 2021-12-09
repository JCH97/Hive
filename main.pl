%% hormigas => a         arannas => s
%% saltamontes => g     abeja reina => q
%% escarabajos => b

% :- [
%     './utils.pl',
%     './board.pl',
%     './move.pl'
% ].

test(Z) :-
    X = ab,
    Y = b,
    Z is X - Y.