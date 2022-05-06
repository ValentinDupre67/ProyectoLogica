:- module(proylcc, 
	[  
		flick/6,
		getColor/4,
		adyC/4,
		adyacentesC/4,
		adyUp/6,
		adyDer/6,
		adyDown/6,
		adyIzq/6,
		pintar/6,
		replace_nth0/5,
		remplazar_Color_En_Grilla/7,
		buscar_Color_En_Grilla/5,
		resta/2,
		adyacenteUp/6,
		adyacenteDown/6,
		adyacenteLeft/6,
		adyacenteRight/6
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Caso en el que se toca el mismo color, entonces no hacemos laburar demas a prolog tuki
flick(Grid,F,C,Color,Grid,ListaAdyacentes):-
    getColor(F,C,Grid,Color),
    adyacentesC(Grid,F,C,ListaAdyacentes).

%flick(+Grid,+F,+C,+ColorNuevo,-FGrid,-ListaAdyacentes) Hace el trabajo sucio de pintar todo y devolver lista de adyacentes
flick(Grid,F,C,ColorNuevo,FGrid,ListaAdyacentes):-
    getColor(F,C,Grid,ColorOriginal),
    pintar(F,C,ColorOriginal,ColorNuevo,Grid,FGrid),
    adyacentesC(FGrid,F,C,ListaAdyacentes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Pintar la grilla
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Este método cáscara se encarga de ir disparando los distintos adyacentes para que vayan
%pintando cada cuadradito a medida que va recorriendo la grilla.
%El método consta a su vez de cuatro métodos más, que se encargan de ir en las
%distintas direcciones para así recorrer la totalidad de la grilla.
%pintar(+F,+C,+ColorAl,+ColorNo,+Grid,-NewGrid) => devuelve una grilla con todos los cambios hechos
pintar(F,C,ColorAl,ColorNo,Grid,NewGrid):-
   	adyacenteUp(F,C,ColorAl,ColorNo,Grid,NewGrid2),
    adyacenteDown(F,C,ColorAl,ColorNo,NewGrid2,NewGrid3),
    adyacenteLeft(F,C,ColorAl,ColorNo,NewGrid3,NewGrid4),
	adyacenteRight(F,C,ColorAl,ColorNo,NewGrid4,NewGrid).

%Este método es el que se encarga de reemplazar un color por otro en una de las filas de la grilla.
% replace_nth0(+List, +Index, +OldElem, +NewElem, -NewList) => devuelve una lista con el color cambiado. 
replace_nth0(List, Index, OldElem, NewElem, NewList) :- 
   nth0(Index,List,OldElem,Transfer),
   nth0(Index,NewList,NewElem,Transfer).

%Este método es el que se encarga de reemplazar un color por otro en la grilla.
% remplazar_Color_En_Grilla(+Lista,+Grid,+F,+C,+ElemAd,+ColorNo,-NuevaGrid) => devuelve la nueva grilla con un color cambiado
remplazar_Color_En_Grilla(Lista,Grid,F,C,ElemAd,ColorNo,NuevaGrid):-
    replace_nth0(Lista,C,ElemAd,ColorNo,NuevaFila),
    replace_nth0(Grid,F,Lista,NuevaFila,NuevaGrid).

%Este método dado un F(fila) y un C(columna), busca el elemento dentro de la grilla,
% y te lo devuelve junto con la fila donde se encuentra guardado.
%buscar_Color_En_Grilla(+F,+C,+Grid,-NewFila,-ElemAd) => devuelve el color a buscar y la fila que lo contiene. 
buscar_Color_En_Grilla(F,C,Grid,NewFila,ElemAd):-
	nth0(F,Grid,NewFila),
    nth0(C,NewFila,ElemAd).

% Este método se encarga de decrementar en 1 el número que se le da como parámetro.
% resta(+Numero,-Resultado) => devuelve el número decrementado en uno.
resta(Numero,Resultado):- Resultado is Numero-1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Caso base me fui de la grilla 
adyacenteUp(-1,_,_,_,Grid,Grid).

%Caso base cuando el color no unifica, ya que es el color que no tengo que reemplazar  
adyacenteUp(F,C,ColorAl,_,Grid,Grid):-    
    buscar_Color_En_Grilla(F,C,Grid,_,ElemAd),    
	ElemAd \= ColorAl.

%Caso recursivo primero pintó la posición actual y luego voy subiendo en la grilla 
adyacenteUp(F,C,ColorAl,ColorNo,Grid,NewGrid):- 
    buscar_Color_En_Grilla(F,C,Grid,NewFila,ElemAd),
	ElemAd == ColorAl,    
    remplazar_Color_En_Grilla(NewFila,Grid,F,C,ElemAd,ColorNo,NewGrid1),    
    adyacenteLeft(F,C,ColorAl,ColorNo,NewGrid1,NewGrid2),
    adyacenteRight(F,C,ColorAl,ColorNo,NewGrid2,NewGrid3),
    resta(F,Fmenos),
 	adyacenteUp(Fmenos,C,ColorAl,ColorNo,NewGrid3,NewGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Caso base me fui de la grilla
adyacenteLeft(_,0,_,_,Grid,Grid).

%Caso base cuando el color no unifica, ya que es el color que no tengo que reemplazar 
adyacenteLeft(F,C,ColorAl,_,Grid,Grid):-
    succ(Cmenos,C),
    buscar_Color_En_Grilla(F,Cmenos,Grid,_,ElemAd),
	ElemAd \= ColorAl.

%Caso recursivo primero me muevo a la izquierda y luego pinto 
adyacenteLeft(F,C,ColorAl,ColorNo,Grid,NewGrid):-
	succ(Cmenos,C),
    buscar_Color_En_Grilla(F,Cmenos,Grid,NewFila,ElemAd),
	ElemAd == ColorAl,
    remplazar_Color_En_Grilla(NewFila,Grid,F,Cmenos,ElemAd,ColorNo,NewGrid1),    
    resta(F,Fmenos),
    adyacenteUp(Fmenos,Cmenos,ColorAl,ColorNo,NewGrid1,NewGrid2),
    adyacenteDown(F,Cmenos,ColorAl,ColorNo,NewGrid2,NewGrid3),    
   	adyacenteLeft(F,Cmenos,ColorAl,ColorNo,NewGrid3,NewGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Caso base me fui de la grilla
adyacenteRight(_,CantCol,_,_,[X|Xs],[X|Xs]):-
    length(X,L),
    CantCol is L-1.

%Caso base cuando el color no unifica, ya que es el color que no tengo que reemplazar
adyacenteRight(F,C,ColorAl,_,Grid,Grid):-
    succ(C,Cmas),
    buscar_Color_En_Grilla(F,Cmas,Grid,_,ElemAd),
	ElemAd \= ColorAl.

%Caso recursivo primero me muevo a la derecha y luego pinto  
adyacenteRight(F,C,ColorAl,ColorNo,Grid,NewGrid):-
	succ(C,Cmas),
    buscar_Color_En_Grilla(F,Cmas,Grid,NewFila,ElemAd),
	ElemAd == ColorAl,    
    remplazar_Color_En_Grilla(NewFila,Grid,F,Cmas,ElemAd,ColorNo,NewGrid1),    
    resta(F,Fmenos),
    adyacenteUp(Fmenos,Cmas,ColorAl,ColorNo,NewGrid1,NewGrid2),
    adyacenteDown(F,Cmas,ColorAl,ColorNo,NewGrid2,NewGrid3),    
 	adyacenteRight(F,Cmas,ColorAl,ColorNo,NewGrid3,NewGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Caso base me fui de la grilla
adyacenteDown(CnatFil,_,_,_,Grid,Grid):-
    length(Grid,L),
    CnatFil is L-1.

%Caso base cuando el color no unifica, ya que es el color que no tengo que reemplazar
adyacenteDown(F,C,ColorAl,_,Grid,Grid):-
    succ(F,Fmas),
    buscar_Color_En_Grilla(Fmas,C,Grid,_,ElemAd),
	ElemAd \= ColorAl.

%Caso recursivo primero me muevo hacia abajo y luego pinto 
adyacenteDown(F,C,ColorAl,ColorNo,Grid,NewGrid):-
    succ(F,Fmas),
    buscar_Color_En_Grilla(Fmas,C,Grid,NewFila,ElemAd),
	ElemAd == ColorAl,    
    remplazar_Color_En_Grilla(NewFila,Grid,Fmas,C,ElemAd,ColorNo,NewGrid1),    
    adyacenteLeft(Fmas,C,ColorAl,ColorNo,NewGrid1,NewGrid2),
    adyacenteRight(Fmas,C,ColorAl,ColorNo,NewGrid2,NewGrid3),    
 	adyacenteDown(Fmas,C,ColorAl,ColorNo,NewGrid3,NewGrid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%Obtener lista de adyacentes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Get color devuelve el color que en la Fila F Columna C de grilla
getColor(F,C,Grilla,Color):-
    nth0(F,Grilla,Fila),
    nth0(C,Fila,Color).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%determina si dos posiciones son adyacentes
%Como va a ser llamado de una posicion que ya viene siendo adyC desde antes me despreocupo
%Lo que hace es ver si sigue siendo del mismo color
%obtiene el color en F,C y verifica si este ColorNuevo unifica con ColorOriginal
%F y C Serian nuevas posiciones que se estan checkeando, no las F y C originales
adyC(F,C,Grilla,ColorOriginal):-
    getColor(F,C,Grilla,ColorNuevo),
    ColorNuevo = ColorOriginal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*METODO CASCARA adyacentesC(+Grilla,+F,+C,-ListaAdyacentes) => Devuelve
 * una lista con los adyacentes de F,C, utiliza adyUp, adyDown, adyDer, adyIzq para hacer todo el trabajo*/

adyacentesC(Grilla, F, C, ListaAdyacentes):-
    getColor(F,C,Grilla,ColorOriginal), %obtenes el color con el cual empezas
    adyUp(Grilla,F,C,ColorOriginal,[],ListaAdyacentesUp), 
    FDown is F+1,
    adyDown(Grilla,FDown,C,ColorOriginal,ListaAdyacentesUp,ListaAdyacentesDown),
    Cder is C+1,
    adyDer(Grilla,F,Cder,ColorOriginal,ListaAdyacentesDown,ListaAdyacentesDer),
    Cizq is C-1,
    adyIzq(Grilla,F,Cizq,ColorOriginal,ListaAdyacentesDer,ListaAdyacentes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%CASO BASE ME FUI DE LA GRILLA
adyUp(_Grilla,-1,_C,_ColorOriginal,LVisi,LVisi).
  
%CASO BASE NO UNIFICAN LOS COLORES => NO ACTUALIZO LA LISTA
adyUp(Grilla,F,C,ColorOriginal,LVisi,LVisi):-
    getColor(F,C,Grilla,ColorNuevo),
    ColorNuevo \= ColorOriginal.
  
%CASO BASE F,C YA ES MIEMBRO => NO ACTUALIZO LA LISTA
adyUp(_Grilla,F,C,_ColorOriginal,LVisi,LVisi):-
    member([F,C],LVisi).

%
%CASO RECURSIVO F y C ya son nuevos, es decir FUp y C
% ESTE ES EL CASO EN EL QUE F,C es ADYC y NO ES MIEMBRO => Lo agrego
adyUp(Grilla,F,C,ColorOriginal,LVisi,LVisiUp2):-
	getColor(F,C,Grilla,ColorNuevo),
    ColorNuevo = ColorOriginal,
    \+ member([F,C],LVisi), %FUp y C no estan en LVisi y los debemos agregar
    append([[F,C]],LVisi,LVisiUp),
    CDer is C+1,
    adyDer(Grilla,F,CDer,ColorOriginal,LVisiUp,LVisiDer),
    CIzq is C-1,
    adyIzq(Grilla,F,CIzq,ColorOriginal,LVisiDer,LVisiIzq),
    FUp is F-1,
    adyUp(Grilla,FUp,C,ColorOriginal,LVisiIzq,LVisiUp2).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Caso BASE Nos salimos de la grilla
adyDer([X|_Xs],_F,L,_ColorOriginal,LVisi,LVisi):-
    length(X,L). %Obtenemos la longitud de la primera fila de la Grilla es igual a la Columna si L es 5 y C es 5 entonces me fui de la grilla

%CASO BASE NO UNIFICAN LOS COLORES 
adyDer(Grilla,F,C,ColorOriginal,LVisi,LVisi):-
    getColor(F,C,Grilla,ColorNuevo),
    ColorNuevo \= ColorOriginal.

%CASO BASE YA ES MEMBER
adyDer(_Grilla,F,C,_ColorOriginal,LVisi,LVisi):-
    member([F,C], LVisi).

%RECORDAR que F = FOriginal y C = COriginal+1
adyDer(Grilla,F,C,ColorOriginal,LVisi,LVisiDer2):-
    getColor(F,C,Grilla,ColorNuevo),
    ColorNuevo = ColorOriginal,
    \+ member([F,C],LVisi), %F y CDer no estan en LVisi y los debemos agregar
    append([[F,C]],LVisi,LVisiDer),
    FUp is F-1,
    adyUp(Grilla,FUp,C,ColorOriginal,LVisiDer,LVisiUp),
    FDown is F+1,
    adyDown(Grilla,FDown,C,ColorOriginal,LVisiUp,LVisiDown),
    CDer is C+1,
    adyDer(Grilla,F,CDer,ColorOriginal,LVisiDown,LVisiDer2).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%CASO BASE me fui de la grilla 
adyDown(Grilla,F,_C,_ColorOriginal,LVisi,LVisi):-
    length(Grilla,F). %Basicamente si la F es la longitud de la Grilla

%CASO BASE No unifican los colores
adyDown(Grilla,F,C,ColorOriginal,LVisi,LVisi):-
    getColor(F,C,Grilla,ColorNuevo),
    ColorNuevo \= ColorOriginal.

%CASO BASE F,C YA ES MIEMBRO => NO ACTUALIZO LA LISTA
adyDown(_Grilla,F,C,_ColorOriginal,LVisi,LVisi):-
    member([F,C],LVisi).

%Caso recursivo
adyDown(Grilla,F,C,ColorOriginal,LVisi,LVisiDown2):-
    getColor(F,C,Grilla,ColorNuevo),
    ColorNuevo = ColorOriginal,
    \+ member([F,C],LVisi),  %Fdown y C no estan en LVisi y los debemos agregar
    append([[F,C]],LVisi,LVisiDown),
    %Disparar otras direcciones
    CDer is C+1,
    adyDer(Grilla,F,CDer,ColorOriginal,LVisiDown,LVisiDer),
    CIzq is C-1,
    adyIzq(Grilla,F,CIzq,ColorOriginal,LVisiDer,LVisiIzq),
    FDown is F+1,
    adyDown(Grilla,FDown,C,ColorOriginal,LVisiIzq,LVisiDown2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Caso BASE me fui de la grilla
adyIzq(_Grilla,_F,-1,_ColorOriginal,LVisi,LVisi).

%Caso Base No unifican los colores
adyIzq(Grilla,F,C,ColorOriginal,LVisi,LVisi):-
    getColor(F,C,Grilla,ColorNuevo),
    ColorNuevo \= ColorOriginal.

%Caso base F,C Ya es miembro => No actualizo lista visitados
adyIzq(_Grilla,F,C,_ColorOriginal,LVisi,LVisi):-
    member([F,C],LVisi).

%Caso recursivo
adyIzq(Grilla,F,C,ColorOriginal,LVisi,LVisiIzq2):-
    getColor(F,C,Grilla,ColorNuevo),
    ColorNuevo = ColorOriginal,
    \+ member([F,C],LVisi),  %F y CIzq no estan en LVisi y los debemos agregar
    append([[F,C]],LVisi,LVisiIzq),
    %Disparar otras direcciones
    FUp is F-1,
    adyUp(Grilla,FUp,C,ColorOriginal,LVisiIzq,LVisiUp),
    FDown is F+1,
    adyDown(Grilla,FDown,C,ColorOriginal,LVisiUp,LVisiDown),
    CIzq is C-1,
    adyIzq(Grilla,F,CIzq,ColorOriginal,LVisiDown,LVisiIzq2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%