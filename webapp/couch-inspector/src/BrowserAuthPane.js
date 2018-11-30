import React, { Component } from 'react';
import './App.css';

import Button from '@material-ui/core/Button';
import TextField from '@material-ui/core/TextField';

// BrowserAuthPane styles
// ---------------------------------------------------------

const styles = {
    browserPane: {
        border: '1px solid black',
        height: '14rem',
        overflow: 'auto',
        padding: '6px',
        width: '24rem',
    },
    browserPaneTitle: {
        fontWeight: 'bold',
        height: '1rem',
    },
    button: {
        marginLeft: '1rem',
        marginTop: '1.5rem',
        textTransform: 'none'
    },
    textField: {
        marginLeft: '1rem',
        marginTop: '1rem',
        width: '95%',
    },
};

// Auxiliary components
// ---------------------------------------------------------

function PasswordField(props) {
    return (
        <div>
            <TextField
                id="password"
                label='Password:'
                style={styles.textField}
                type='password'
                defaultValue=''
            />
        </div>
    );
}

function UsernameField(props) {
    return (
        <div>
            <TextField
                id="username"
                label='Username:'
                style={styles.textField}
                defaultValue=''
            />
        </div>
    );
}

function LoginButton(props) {
    const pane = props.pane;
    const app = props.app;
    const dbName = props.database;

    const clickHandler = () => {
        const username = app.getLoginUsername();
        const password = app.getLoginPassword();
        app.updateLogin(dbName, username, password);
    };

    return (
        <Button
            style={styles.button}
            variant="contained"
            color="primary"
            onClick={clickHandler} >
            Log In
        </Button>
    );
}

// BrowserAuthPane class
// ---------------------------------------------------------

class BrowserAuthPane extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        const pane = this;
        const app = pane.props.app;
        const dbName = app.getSelectedDatabase();
        const username = app.getLoginUsername();
        const password = app.getLoginPassword();

        return (
            <div>
                <div><p style={styles.browserPaneTitle}>Log in to '{dbName}'</p></div>
                <div style={styles.browserPane}>
                    <UsernameField />
                    <PasswordField />
                    <LoginButton
                        app={app}
                        database={dbName} />
                </div>
            </div>
        );
    }
}

// exports
// ---------------------------------------------------------

export default BrowserAuthPane;

