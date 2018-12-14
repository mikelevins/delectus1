import React, { Component } from 'react';
import './App.css';


import PasswordField from './PasswordField.js';
import UsernameField from './UsernameField.js';

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
    formContainer: {
        marginLeft: 'auto',
        marginRight: 'auto',
        padding: '1rem',
        width: '85%',
    },
    messageText: {
        marginLeft: '1.5rem',
    },
    title: {
        background: '#663322',
        color: 'white',
        fontSize: '16pt',
        marginTop: '0',
        textAlign: 'center',
    },
};

class DelectusLogin extends Component {

    render() {
        const controls = this;
        const app = controls.props.app;

        return (
            <div id='appBackdrop' style={styles.appBackdrop}>
                <div id='delectusPane' style={styles.delectusPane}>
                    <h1 style={styles.title}>Delectus</h1>
                    <p style={styles.messageText}>Log in to Delectus:</p>
                    <div id='formContainer' style={styles.formContainer}>
                        <UsernameField />
                        <PasswordField />
                    </div>
                </div>
            </div>
        );
    }
}

// exports
// ---------------------------------------------------------

export default DelectusLogin;