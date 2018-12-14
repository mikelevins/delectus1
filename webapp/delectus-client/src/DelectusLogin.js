import React, { Component } from 'react';
import './App.css';

import indigo from '@material-ui/core/colors/indigo';

import LoginButtonBar from './LoginButtonBar.js';
import PasswordField from './PasswordField.js';
import UsernameField from './UsernameField.js';

const styles = {
    appBackdrop: {
        width: '100%',
    },
    delectusPane: {
        border: '1px solid #224488',
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
        background: indigo['600'],
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
                        <LoginButtonBar />
                    </div>
                </div>
            </div>
        );
    }
}

// exports
// ---------------------------------------------------------

export default DelectusLogin;