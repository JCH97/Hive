row_directions([1, 1, 0, -1, -1, 0]).
columns_directions([-1, 0, 1, 1, 0, -1]).

%offset: odd-q

address([-1,0,  /*up*/
         0,1,   /*up-right*/
         1,1,   /*down-right*/
         1,0,   /*down*/
         1,-1,  /*down-left*/
         0,-1 /*up-left*/
        %  0,-1  /*up*/
         ]).
% address([1, -1]).