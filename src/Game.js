import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import Square from './Square';
import { useState } from 'react/cjs/react.production.min';
import userEvent from '@testing-library/user-event';
import swal from 'sweetalert';

/**
 * List of colors.
 */

const colors = ["r", "v", "p", "g", "b", "y"];  // red, violet, pink, green, blue, yellow

/**
 * Returns the CSS representation of the received color.
 */

export function colorToCss(color) {
  switch (color) {
    case "r": return "#f38630";
    case "v": return "#67917a";
    case "p": return "#170409";
    case "g": return "#b8af03";
    case "b": return "#ccbf82";
    case "y": return "#e33258";
  }
  return color;
}

class Game extends React.Component {

  pengine;

  constructor(props) {
    super(props);
    this.state = {
      origin: undefined,
      turns: 0,
      grid: null,
      complete: false,  // true if game is complete, false otherwise
      waiting: false,
      movements: [],
      adyacentes: [],
    };
    this.handleClick = this.handleClick.bind(this);
    this.onOriginSelected = this.onOriginSelected.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);
  }

  handlePengineCreate() {
    const queryS = 'init(Grid),adyacentesC(Grid,0,0,ListaAdyacentes)';    
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grid'],
          adyacentes: response['ListaAdyacentes']
        });
      }
    });
  }

  handleClick(color) {
    // No action on click if game is complete or we are waiting.
    this.state.movements.push(color);

    if (this.state.complete || this.state.waiting) {
      return;
    }
    // Build Prolog query to apply the color flick.
    // The query will be like:
    // flick([[g,g,b,g,v,y,p,v,b,p,v,p,v,r],
    //        [r,r,p,p,g,v,v,r,r,b,g,v,p,r],
    //        [b,v,g,y,b,g,r,g,p,g,p,r,y,y],
    //        [r,p,y,y,y,p,y,g,r,g,y,v,y,p],
    //        [y,p,y,v,y,g,g,v,r,b,v,y,r,g],
    //        [r,b,v,g,b,r,y,p,b,p,y,r,y,y],
    //        [p,g,v,y,y,r,b,r,v,r,v,y,p,y],
    //        [b,y,v,g,r,v,r,g,b,y,b,y,p,g],
    //        [r,b,b,v,g,v,p,y,r,v,r,y,p,g],
    //        [v,b,g,v,v,r,g,y,b,b,b,b,r,y],
    //        [v,v,b,r,p,b,g,g,p,p,b,y,v,p],
    //        [r,p,g,y,v,y,r,b,v,r,b,y,r,v],
    //        [r,b,b,v,p,y,p,r,b,g,p,y,b,r],
    //        [v,g,p,b,v,v,g,g,g,b,v,g,g,g]],r, Grid)
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const Fila = !this.state.origin ? 0: this.state.origin[0];
    const Columna = !this.state.origin ? 0 :this.state.origin[1];
    if(!this.state.origin){
      this.setState({
        origin:[0,0]
      });
    }
    const queryS = "flick(" + gridS +","+ Fila +","+ Columna +","+ color +",Grid, ListaAdyacentes)";
    this.setState({
      waiting: true
    });
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grid'],
          turns: this.state.turns + 1,
          adyacentes: response['ListaAdyacentes'],
          waiting: false,
        });
        if(this.state.adyacentes.length === 196){
          this.setState({
            complete: true,
          })
          this.mostrarAlerta();
        }
      } else {
        // Prolog query will fail when the clicked color coincides with that in the top left cell.
        this.setState({
          waiting: false
        });
      }
    });
  }

  mostrarAlerta=()=>{
    swal({
      title: "¡¡Enhorabuena!! Has ganado",
      text: "Terminaste el juego en "+this.state.turns+" turnos, eres un capo bien ahí UwU ;)",
      buttons: {
        restart: {
          text: "Volver a intentarlo",
          value: "restart",
        },
      },
    })
    .then((value) => {
      if (value==="restart"){
        window.location.reload()
      }
        });
  }
  

  onOriginSelected(pos){
    this.setState({
      origin:pos
    })
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const Fila = pos[0];
    const Columna = pos[1];
    const queryS = "adyacentesC("+gridS+","+Fila+","+Columna+",ListaAdyacentes)";
    this.pengine.query(queryS, (success,response)=>{
      if(success){
        this.setState({
          adyacentes: response['ListaAdyacentes']
        })
      }
    });
  }

  render() {
    if (this.state.grid === null) {
      return null;
    }
    return (
      <div className='Conteiner'>
      <div className="game">
        <div className="leftPanel">
          <div className="buttonsPanel">
            {colors.map(color =>
              <button
                className="colorBtn"
                style={{ backgroundColor: colorToCss(color) }}
                onClick={() => this.handleClick(color)}              
                key={color}
              />)}
          </div>
          <div className="turnsPanel">
            <div className="turnsLab">Turnos</div>
            <div className="turnsNum">{this.state.turns}</div>
          </div>             
          <div className='capturedPanel'>
              <div className='capturedLab'>Capturados</div>
              <div className='capturedNum'>{this.state.adyacentes.length}</div>
          </div>
          
        </div>
        <Board 
          grid={this.state.grid}
          onOriginSelected = {!this.state.origin ? this.onOriginSelected : undefined} 
          origin={this.state.origin}
        />
      </div>
      <div className='movementsPanel'>
            <div className='movementsAux'>Historial</div>
            <div className='movements'> 
            {this.state.movements.map((colors, i) =>
              <Square 
                value={colors}
                key={i}
                className={"movementSquare"}
              />
              )}
            </div>
          </div>
      </div>

      
    );
  }
}

export default Game;