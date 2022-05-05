import React from 'react';
import Square from './Square';

class Board extends React.Component {
    render() {
        return (
            <div className={"board" + (this.props.onOriginSelected ? " clickableBoard" : "")}>
                {this.props.grid.map((row, i) =>
                    row.map((cell, j) =>
                        <Square
                            value={cell}
                            key={i + "." + j}
                            className={this.props.origin && this.props.origin[0] === i && this.props.origin[1] === j ? "origin" : undefined}
                            onClick = {this.props.onOriginSelected && (() =>
                                this.props.onOriginSelected([i,j]))}
                        />
                    )
                )}
            </div>
        );
    }
}

export default Board;