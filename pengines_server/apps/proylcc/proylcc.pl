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

gameStatus(Grid, Winner):-
	