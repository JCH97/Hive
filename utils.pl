:- use_module(library(ordsets)).

common_elements(A, B) :-
    sort(A, AS), % can omit if using sorted A list
    sort(B, BS), % can omit if using sorted B list
    ord_intersect(AS, BS).

row_directions([1, 1, 0, -1, -1, 0]).
columns_directions([-1, 0, 1, 1, 0, -1]).

%offset: odd-q

address([1,-1,  /*up-right*/
         2,0,   /*right*/
         1,1,   /*down-right*/
         -1,1,   /*down-left*/
         -2,0,  /*left*/
         -1,-1 /*up-left*/        
         ]).

close_cycle_addr([1,-1]).

address1([-1,0,  /*up*/
    0,1,   /*up-right*/
    1,1,   /*down-right*/
    1,0,   /*down*/
    1,-1,  /*down-left*/
    0,-1, /*up-left*/
    0,-1  /*up*/
    ]).
% address([1, -1]).

add_tail([], X, [X]).
add_tail([H | T], X, [H | L]) :- add_tail(T, X, L).

