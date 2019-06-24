/*
 * K - King
 * Q - Queen
 * R - Rook
 * B - Bishop
 * N - kNight
 * P - Pawn
 */
 
 %board initialisation (rysunek 1)
 %board(Colour, Column_index, Row_index, Chessman_identificator)
 board('W',a,2,'N').
 board('W',c,3,'Q').
 board('W',e,1,'K').
 board('W',e,2,'P').
 board('W',f,2,'P').
 board('W',f,3,'N').
 board('B',a,5,'B').
 board('B',b,6,'R').
 board('B',d,3,'P').
 board('B',d,5,'P').
 board('B',d,6,'K').

%transforming column letter identificators to numbers (1-6) for arithmetic operations
colNum('a',Q) :- Q = 1.
colNum('b',Q) :- Q = 2.
colNum('c',Q) :- Q = 3.
colNum('d',Q) :- Q = 4.
colNum('e',Q) :- Q = 5.
colNum('f',Q) :- Q = 6.

%transforming column number identificators to letters (a-f) for board operations (navigation)
colLet(1, Q) :- Q = 'a'.
colLet(2, Q) :- Q = 'b'.
colLet(3, Q) :- Q = 'c'.
colLet(4, Q) :- Q = 'd'.
colLet(5, Q) :- Q = 'e'.
colLet(6, Q) :- Q = 'f'.

isAdjacent(X1,Y1,X2,Y2) :-			%true if (X2,Y2) is adjacent to (X1,Y1) (could be any of the 8 squares)
    colNum(X1,Col1),
    colNum(X2,Col2),
    abs(Col1-Col2) =< 1,
    abs(Y1-Y2) =< 1.

isDiagonalAdj(X1,Y1,X2,Y2) :-		%true if is diagonal and adjacent 
    colNum(X1,Col1),
    colNum(X2,Col2),
    abs(Col1-Col2) =:= 1,
    abs(Y1-Y2) =:= 1.

isDiagonal(X1,Y1,X2,Y2) :-			%true if is diagonal, including adjacent diagonal
    colNum(X1,Col1),
    colNum(X2,Col2),
    Value1 is abs(Col1-Col2),
    Value2 is abs(Y1-Y2),
    Value1 =:= Value2.
    
isTaken(X,Y) :-						%true if there is a piece placed on the given coordinates (X, Y)
    board(_,X,Y,_).

isOpposite(X1,Y1,X2,Y2) :-			%true if piece on (X1,X2) is of different colour than the one on (X2, Y2)
    board('W',X1,Y1,_), board('B',X2,Y2,_)	;
    board('B',X1,Y1,_), board('W',X2,Y2,_).

isInside(X,Y) :-					%true if (X, Y) are inside the board
    Y<7,
    Y>0,
    member(X,[a,b,c,d,e,f]).

canMove(X1,Y1,X2,Y2) :-			%true if the destination (X2, Y2) is empty or the piece there is of diff. colour
    not(isTaken(X2,Y2)) ;
    isTaken(X2,Y2), isOpposite(X1,Y1,X2,Y2).
    

%%%%% Types of movement %%%%%
cantMoveVertical(X,Y,Y2) :-		%true if there are some pieces on the squares which aren't the last square (X2,Y2)
    isTaken(X,Y),
    Y \= Y2.

moveVertical(_,Y2,Y2) :- !. 	%?
moveVertical(X,Y1,Y2) :- 	%down
    Y2<Y1,
    Y is Y1-1,
    \+ cantMoveVertical(X,Y,Y2),
	moveVertical(X,Y,Y2).

moveVertical(X,Y1,Y2) :- 	%up
    Y2>Y1,
    Y is Y1+1,
    \+ cantMoveVertical(X,Y,Y2),
	moveVertical(X,Y,Y2), !.


cantMoveHorizontal(X,Y,X2) :-	%true if there are some pieces on the squares which aren't the last square (X2,Y2)
    isTaken(X,Y),
    X \= X2.

moveHorizontal(X2,_,X2) :- !. 	%?
moveHorizontal(X1,Y,X2) :- 	%right
    X2>X1,
    X is X1+1,
    \+ cantMoveHorizontal(X,Y,X2),
	moveHorizontal(X,Y,X2).

moveHorizontal(X1,Y,X2) :- 	%left
    X2<X1,
    X is X1-1,
    \+ cantMoveHorizontal(X,Y,X2),
	moveHorizontal(X,Y,X2).


cantMoveDiagonal(X,Y,X2,Y2) :-	%true if there are some pieces on the squares which aren't the last square (X2,Y2)
    colLet(X,Let1), 
    isTaken(Let1, Y),
    dif(X =:= X2, Y =:= Y2).

checkIfBigger(Coord1, Coord2, Output) :-	%output is used to determine the direction of the diagonal move
    Coord2 > Coord1,
    Output is Coord1 + 1 	;
    Coord2 < Coord1,
    Output is Coord1 - 1.

moveDiagonal(X2,Y2,X2,Y2).
moveDiagonal(Col1,Y1,Col2,Y2) :-
    checkIfBigger(Col1, Col2, X),
    checkIfBigger(Y1, Y2, Y),
    \+ cantMoveDiagonal(X, Y, Col2, Y2),
    moveDiagonal(X,Y,Col2,Y2), !.


canKnightMove(1,2).		%difference between the distance must be 2 and 1 for any coordinate (axes)
canKnightMove(2,1).

moveKnight(X1,Y1,X2,Y2) :-
    colNum(X1,Col1), 
    colNum(X2,Col2),
    X is abs(Col1-Col2),
    Y is abs(Y1-Y2),
    canKnightMove(X,Y).
    

%% below are 'dummy predicates', neccessary for generating all possible moves. dx and dy are used only in pos(). %%
dx(a,Q) :- Q ='a'.
dx(b,Q) :- Q ='b'.
dx(c,Q) :- Q ='c'.
dx(d,Q) :- Q ='d'.
dx(e,Q) :- Q ='e'.
dx(f,Q) :- Q ='f'.

dy(1,Q) :- Q = 1.
dy(2,Q) :- Q = 2.
dy(3,Q) :- Q = 3.
dy(4,Q) :- Q = 4.
dy(5,Q) :- Q = 5.
dy(6,Q) :- Q = 6.
              
pos(X01,Y01,X02,Y02) :-
    dx(X01, X1),
    dy(Y01, Y1),
    dx(X02, X2),
    dy(Y02, Y2),
    isInside(X2,Y2),
    canMove(X1,Y1,X2,Y2),
    board(Col, X1, Y1, Fig),
	cases(X1,Y1,X2,Y2,Col,Fig).		%, !.

wszystkie_pos_bialych(X1,Y1,X2,Y2) :-		%shows all possible moves for white pieces
     board('W',X1,Y1,_),
     pos(X1,Y1,X2,Y2).
    

%%%%% Cases for each chessman type %%%%%
%Pawn
cases(X,2,X,3, 'W', 'P').
cases(X,2,X,4, 'W', 'P') :- not(isTaken(X,3)).	%can move 2 squares if it's first move of this piece
cases(X,5,X,4, 'B', 'P').
cases(X,5,X,3, 'B', 'P') :- not(isTaken(X,4)).  %can move 2 squares if it's first move of this piece
%pawns can also move forward to adjacent diagonals if they are capturing:
cases(X1,Y1,X2,Y2, 'W', 'P') :- Y2>Y1, isTaken(X2,Y2), isDiagonalAdj(X1,Y1,X2,Y2), isOpposite(X1,Y1,X2,Y2).
cases(X1,Y1,X2,Y2, 'B', 'P') :- Y2<Y1, isTaken(X2,Y2), isDiagonalAdj(X1,Y1,X2,Y2), isOpposite(X1,Y1,X2,Y2).

%Rook
cases(X,Y1,X,Y2, _, 'R') :- moveVertical(X,Y1,Y2).
cases(X1,Y,X2,Y, _, 'R') :- colNum(X1,Col1), colNum(X2,Col2), moveHorizontal(Col1,Y,Col2).

%Bishop
cases(X1,Y1,X2,Y2, _, 'B') :- isDiagonal(X1,Y1,X2,Y2), colNum(X1,Col1), colNum(X2,Col2), 
    moveDiagonal(Col1,Y1,Col2,Y2).

%Queen
cases(X,Y1,X,Y2, _, 'Q') :- moveVertical(X,Y1,Y2).
cases(X1,Y,X2,Y, _, 'Q') :- colNum(X1,Col1), colNum(X2,Col2), moveHorizontal(Col1,Y,Col2).
cases(X1,Y1,X2,Y2, _, 'Q') :- isDiagonal(X1,Y1,X2,Y2), colNum(X1,Col1), colNum(X2,Col2), 
    moveDiagonal(Col1,Y1,Col2,Y2).

%Knight
cases(X1,Y1,X2,Y2, _, 'N') :- moveKnight(X1,Y1,X2,Y2).

%King
cases(X1,Y1,X2,Y2, _, 'K') :- isAdjacent(X1,Y1,X2,Y2).