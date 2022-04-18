:- module(proylcc, 
	[  
		flick/3,
		gameStatus/2
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla. 

flick(Grid, Color, FGrid):-
	Grid = [F|Fs],
	F = [X|Xs],
	Color \= X,
	FGrid = [[Color|Xs]|Fs].


%
% gameStatus(+Grid, +Winner)
%
% Retorna Winner que es el estado que indica si el juego ha sido completado.

%gameStatus(Grid,Winner):- 
	
%Este flick2 sirve para posicionarnos y nos da la posibilidad de cambiar el elemento viejo en esa posicion
flick2(Grid,Color,FGrid,I,J):-
    nth0(I,Grid,F),
    nth0(J,F,X),
    Color \= X, %X es el color viejo, en esta linea se detecta si estamos cambiando de color
    replace_nth0(F,J,X,Color,NF), % Remplaza en la Fila F columna J, el color viejo X por Color nuevo y devuelve NF
	replace_nth0(Grid, I, F, NF, FGrid). % Remplaza en la Grid la fila I, la fila vieja F por la nueva fila NF y devuelve FGrid
	remplazar_Ady(X,Color,I,J) /*lo que se me ocurre es que a partir de la pos I,J entre en bucle hasta no poder remplazar mas*/

replace_nth0(List, Index, OldElem, NewElem, NewList) :-
   % predicate works forward: Index,List -> OldElem, Transfer
   nth0(Index,List,OldElem,Transfer),
   % predicate works backwards: Index,NewElem,Transfer -> NewList
   nth0(Index,NewList,NewElem,Transfer).

   adyacenteUp(0,_,_,_,Grid,Grid).
/*  

adyacenteUp(F,C,ColorAl,ColorNo,Grid,NewGrid2):-
    succ(Fmas,F),
    nth0(Fmas,Grid,NewFila),
    nth0(C,NewFila,ElemAd),
	ElemAd == ColorAl,
	replace_nth0(NewFila,C,ElemAd,ColorNo,NF), 
	replace_nth0(Grid, Fmas, NewFila, NF, NewGrid),
 	adyacenteUp(Fmas,C,ColorAl,ColorNo,NewGrid,NewGrid2) , !.

adyacenteUp(F,C,ColorAl,_,Grid,Grid):-    
    nth0(F,Grid,NewFila),
    nth0(C,NewFila,ElemAd),
	ElemAd \= ColorAl , !.
	
*/
	