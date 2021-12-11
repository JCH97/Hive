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

pool(roy, -75.702744, 5).
pool(marth, -75.731638, 10).
pool(jiggy, -75.7449645, 3).
pool(yamaha, -75.7114829, 15).


my_sort():-
    findall([Lat,Name],pool(Name,Long,Lat),List),
    msort(List,Sorted),
    write(Sorted).