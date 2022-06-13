:- module(proylcc, 
	[  
		flick/6,
        adyCStar/3,
        mejorCamino/4,
        encontraUnCamino/3
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Caso en el que se toca el mismo color, entonces no hacemos laburar demas a prolog tuki
flick(Grid,F,C,Color,Grid,ListaAdyacentes):-
    getColor(F,C,Grid,Color),
    adyCStar([F,C],Grid,ListaAdyacentes). %Tenemos que calcular adyCStar devuelta pq sino en React se borra la cant de capturados

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%Obtener lista de adyacentes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Get color devuelve el color que en la Fila F Columna C de grilla
getColor(F,C,Grilla,Color):-
    nth0(F,Grilla,Fila),
    nth0(C,Fila,Color).

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
    Jugada = strat(_CaminoColores,196,_UltGrilla),!.

chequearGrillaCompleta([_Jugada|Jugadas]):-
    chequearGrillaCompleta(Jugadas).
    
colores([r,v,p,g,b,y]).

%SACAR Y CAMBIAR POR EL DELETE%%%%%%%%%%%%%%%
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
    JugadaGanadora = strat(SecuenciaGanadora,_CantCaup,_Gridd).

ayuda([F,C],Grid,Resultado):-
    adyCStar([F,C],Grid,AdyacentesActual),
    length(AdyacentesActual,CantCapActual),
    color([F,C],Grid,ColorActual),
    colores(Colores),
    remover(ColorActual,Colores,ListaColores),
    findall(strat([Color],CantCapNew,GridNew),(
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
	Jugada = strat(SolucionColores,CantCaptActual,Grid),
    color([F,C],Grid,ColorActual),
    colores(Colores),
    remover(ColorActual,Colores,ListaColores),
    findall(strat(NewSolucionColores,NewCantCapt,NewGrid),
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
    Jugada = strat(_Colores,_CantCapturados,_Grid).

descubrirMejorJugada(JugadasCompletas,JugadaGanadora):-
    JugadasCompletas = [Jugada|Jugadas],
    Jugadas = [NextJugada|NextJugadas],
    Jugada = strat(_Colores,CantCapturados,_Grid),
    NextJugada = strat(_NextColores,NextCantCapturados,_NextGrid),
    CantCapturados>NextCantCapturados,
    descubrirMejorJugada([Jugada|NextJugadas],JugadaGanadora),!.

descubrirMejorJugada(JugadasCompletas,JugadaGanadora):-
    JugadasCompletas = [Jugada|Jugadas],
    Jugadas = [NextJugada|NextJugadas],
    Jugada = strat(_Colores,_CantCapturados,_Grid),
    NextJugada = strat(_NextColores,_NextCantCapturados,_NextGrid),
	descubrirMejorJugada([NextJugada|NextJugadas],JugadaGanadora).

encontraUnCamino([F,C],Grid,Color):- 
    ayuda([F,C],Grid,Resultado),
    descubrirMejorJugada(Resultado,JugadaGanadora),
    JugadaGanadora = strat([Color|_],_CantCaup,_NewGrid). 


