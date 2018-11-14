import React, { Component } from 'react';
import './App.css';
import CssBaseline from '@material-ui/core/CssBaseline';

function presentObject(object) {
    const objectType = typeof object;

    if (objectType === 'object') {
        const objectRows = Object.keys(object).map(
            (key) =>
                <tr key={key}>
                    <td style={{fontWeight: 'bold'}}>{key}:</td>
                    <td style={{border: '1px solid black'}}>{presentObject(object[key])}</td>
                </tr>
        );

        return (
            <React.Fragment>
                <CssBaseline />
                <table 
                    style={{
                        marginLeft: 2 + 'rem',
                        marginRight: 2 + 'rem',
                        }}>
                    <tbody>
                        {objectRows}
                    </tbody>
                </table>
            </React.Fragment>
        );
    } else {
        return (
            <React.Fragment>
                <CssBaseline />
                <p>{String(object)}</p>
            </React.Fragment>
        );
    }

};

class ObjectPresentation extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        const object = this.props.object;
        return (presentObject(object));
    }
}

// exports
// ---------------------------------------------------------

export default ObjectPresentation;