import React, { Component } from 'react';
import './App.css';


const styles = {
    appBackdrop: {
        width: '100%',
    },
    delectusPane: {
        border: '1px solid #884422',
        marginLeft: 'auto',
        marginRight: 'auto',
        marginTop: '2rem', 
        maxWidth: '24rem',
    },
    title: {
        background: '#663322',
        fontSize: '20pt',
        marginTop: '0',
        textAlign: 'center',
    },
};

class Delectus extends Component {

    render() {
        const controls = this;
        const app = controls.props.app;

        return (
            <div id='appBackdrop' style={styles.appBackdrop}>
                <div id='delectusPane' style={styles.delectusPane}>
                    <h1 style={styles.title}>Delectus</h1>
                </div>
            </div>
        );
    }
}

// exports
// ---------------------------------------------------------

export default Delectus;