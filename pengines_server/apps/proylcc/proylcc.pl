:- module(proylcc, 
	[  
		flick/6,
        adyCStar/3,
        pintar/4
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Caso en el que se toca el mismo color, entonces no hacemos laburar demas a prolog tuki
flick(Grid,F,C,Color,Grid,ListaAdyacentes):-
    getColor(F,C,Grid,Color),
    adyacentesC(Grid,F,C,ListaAdyacentes).

%flick(+Grid,+F,+C,+ColorNuevo,-FGrid,-ListaAdyacentes) Hace el trabajo sucio de pintar todo y devolver lista de adyacentes
flick(Grid,F,C,ColorNuevo,FGrid,ListaAdyacentes):-
    %getColor(F,C,Grid,ColorOriginal),
    %pintar(F,C,ColorOriginal,ColorNuevo,Grid,FGrid),
    %adyacentesC(FGrid,F,C,ListaAdyacentes).   
    pintar([F,C],ColorNuevo,Grid,FGrid),
    adyCStar([F,C],FGrid,ListaAdyacentes).

%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%&& NUEVA VERSION DEL PINTAR &&&&&&&
%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


pintar([F,C],ColorN,Grid,NewGrid):-
    adyCStar([F,C],Grid,Res),
    pintarOptimo(ColorN,Res,Grid,NewGrid).
 
  pintarOptimo(_,[],Grid,Grid).
  pintarOptimo(ColorN,[H|T],Grid,NewGrid):- 
      nth0(0,H,F),
      nth0(1,H,C),
      buscar_Color_En_Grilla(F,C,Grid,NewFila,ElemAd),
      remplazar_Color_En_Grilla(NewFila,Grid,F,C,ElemAd,ColorN,NewGrid1),
      pintarOptimo(ColorN,T,NewGrid1,NewGrid).

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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Caso base me fui de la grilla 
adyacenteUp(-1,_,_,_,Grid,Grid).

%Caso base cuando el color no unifica, no me tengo que seguir moviendo hacia arriba    
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

%Caso base cuando el color no unifica, no me tengo que seguir moviendo a la izquierda 
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

%Caso base cuando el color no unifica, no me tengo que seguir moviendo a la derecha 
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

%Caso base cuando el color no unifica, no me tengo que seguir moviendo hacia abajo 
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%CODIGO CATEDRA

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 * adyCStar(Origin, +Grid, -Res)
 * Calcula el conjunto de celdas adyacentesC* de la celda Origin en la grilla Grid
 * siguiendo una estrategia de propagación o expansión.
 */

adyCStar(Origin, Grid, Res) :-
    adyCStarSpread([Origin], [], Grid, Res).

/*
 * adyCStarSpread(+Pend, +Vis, +Grid, -Res)
 * Pend: por "pendientes", inicialmente es la lista [Origin], y en general es 
 * el conjunto de celdas adyacentesC* a Origin que aún no fueron consideradas.
 * Vis: por "visitados", inicialmente [], son las celdas adyacentesC* a la Origen 
 * que ya fueron consideradas.
 * Grid: idem adyCStar
 * Res: idem adyCStar
 * En cada paso se selecciona una celda de las pendientes, se pasa a visitados, y
 * se agregan a pendientes todas aquellas adyacentes a la celda, del mismo color, que no estén
 * ya ni en pendientes ni visitados.
 */

adyCStarSpread([], Vis, _Grid, Vis).

adyCStarSpread(Pend, Vis, Grid, Res):-
    Pend = [P|Ps],
    findall(A, 
	        (
    	        adyC(P, Grid, A),
        	    not(member(A, Pend)),
            	not(member(A, Vis))
	        ), 
            AdyCP),
    append(AdyCP, Ps, NPend),
    adyCStarSpread(NPend, [P|Vis], Grid, Res).

/* 
 * adyC(+P, +Grid, -A)
 */

adyC(P, Grid, A):-
    ady(P, Grid, A),
    color(P, Grid, C),
    color(A, Grid, C).

/* 
 * ady(+P, +Grid, -A)
 */

ady([X, Y], Grid, [X1, Y]):-
    length(Grid, L),
    X < L - 1,
    X1 is X + 1.

ady([X, Y], _Grid, [X1, Y]):-
    X > 0,
    X1 is X - 1.

ady([X, Y], Grid, [X, Y1]):-
    Grid = [F|_],
    length(F, L),
    Y < L - 1,
    Y1 is Y + 1.

ady([X, Y], _Grid, [X, Y1]):-
    Y > 0,
    Y1 is Y - 1.


/* 
 * color(P, Grid, C)
 */

color([X,Y], Grid, C):-
    nth0(X, Grid, F),
    nth0(Y, F, C).    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%PEDIR AYUDA

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%structure strat(CaminoColores,CantidadCapturados,ListaAdyacentes,UltimaGrilla)
chequearGrillaCompleta([Jugada|_Jugadas]):-
    Jugada = strat(_CaminoColores,196,_ListaAdy,_UltGrilla),!.

chequearGrillaCompleta([_Jugada|Jugadas]):-
    chequearGrillaCompleta(Jugadas).
    
colores([r,v,p,g,b,y]).

remover( _, [], []).
remover( R, [R|T], T).
remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2).

%mejorEstrategia([F,C],Grilla,Profundidad,Resultado):-
%    analizarCaminos([F,C],Grilla,Profundidad,PosiblesSoluciones),
%    mejorResultado(PosiblesSoluciones,Profundidad,Resultado).

mejorCamino([F,C],Grid,Profundidad,SecuenciaGanadora):-
    %Se inicia encontrando los primeros caminos con ayuda
    ayuda([F,C],Grid,JugadasIniciales),
    %Ayuda bis se encarga de armar las soluciones recursivamente
    %utilizando las estructuras generadas en ayuda
    ayudaBis([F,C],JugadasIniciales,Profundidad,JugadasCompletas),
    %Se recorren las distintas jugadas buscando la mejor
    descubrirMejorJugada(JugadasCompletas,JugadaGanadora),
    %Se obtiene la secuencia de colores del mejor camino
    JugadaGanadora = strat(SecuenciaGanadora,_CantCaup,_Ady,_Gridd).

ayuda([F,C],Grid,Resultado):-
    adyCStar([F,C],Grid,AdyacentesActual),
    length(AdyacentesActual,CantCapActual),
    color([F,C],Grid,ColorActual),
    colores(Colores),
    remover(ColorActual,Colores,ListaColores),
    findall(strat([Color],CantCapNew,AdyacentesNew,GridNew),(
             member(Color,ListaColores),
             flick(Grid,F,C,Color,GridNew,AdyacentesNew),
             length(AdyacentesNew,CantCapNew),
             CantCapNew>CantCapActual
            ),Resultado).
 
%Casos Base
ayudaBis(_Origen,Jugadas,1,Jugadas):-!.
ayudaBis(_Origen,[],_Profundidad,[]):-!.
%Capturo Todo
ayudaBis(_Origen,Jugadas,_Profundidad,Jugadas):-
    chequearGrillaCompleta(Jugadas),!.

%Caso Recursivo
ayudaBis([F,C],[Jugada|Jugadas],Profundidad,JugadasCompletas):-
	Jugada = strat(SolucionColores,CantCaptActual,_AdyCS,Grid),
    color([F,C],Grid,ColorActual),
    colores(Colores),
    remover(ColorActual,Colores,ListaColores),
    findall(strat(NewSolucionColores,NewCantCapt,NewAdyCS,NewGrid),
            (member(ColorNuevo,ListaColores),
            flick(Grid,F,C,ColorNuevo,NewGrid,NewAdyCS),
            length(NewAdyCS,NewCantCapt),
            NewCantCapt>CantCaptActual,
            append(SolucionColores,[ColorNuevo],NewSolucionColores)
            ),JugadasParciales),
    %Obtenemos las otras (ramas)jugadas de este nivel
    ayudaBis([F,C],Jugadas,Profundidad,JugadasNivelActual),
    ProfundidadNext is Profundidad - 1,
    %obtenemos las jugadas del proximo nivel de esta rama
    ayudaBis([F,C],JugadasParciales,ProfundidadNext,JugadasSiguienteNivel),
    append(JugadasNivelActual,JugadasSiguienteNivel,JugadasCompletas).


%DescubrirMejorJugada(+JugadasCompletas, -JugadaGanadora)
%JugadasCompletas = [Jugada|Jugadas]

descubrirMejorJugada([],_JugadaGanadora).

descubrirMejorJugada(JugadasCompletas,Jugada):-
    JugadasCompletas = [Jugada|[]],
    Jugada = strat(_Colores,_CantCapturados,_AdyCS,_Grid).

descubrirMejorJugada(JugadasCompletas,JugadaGanadora):-
    JugadasCompletas = [Jugada|Jugadas],
    Jugadas = [NextJugada|NextJugadas],
    Jugada = strat(_Colores,CantCapturados,_AdyCS,_Grid),
    NextJugada = strat(_NextColores,NextCantCapturados,_NextAdyc,_NextGrid),
    CantCapturados>NextCantCapturados,
    descubrirMejorJugada([Jugada|NextJugadas],JugadaGanadora),!.

descubrirMejorJugada(JugadasCompletas,JugadaGanadora):-
    JugadasCompletas = [Jugada|Jugadas],
    Jugadas = [NextJugada|NextJugadas],
    Jugada = strat(_Colores,_CantCapturados,_AdyCS,_Grid),
    NextJugada = strat(_NextColores,_NextCantCapturados,_NextAdyc,_NextGrid),
	descubrirMejorJugada([NextJugada|NextJugadas],JugadaGanadora).
    
    


