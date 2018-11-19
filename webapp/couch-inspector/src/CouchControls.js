import React, { Component } from 'react';
import './App.css';

import Button from '@material-ui/core/Button';
import TextField from '@material-ui/core/TextField';

const styles = {
    button: {
        marginLeft: '2rem',
        marginTop: '1rem',
    },
    textField: { marginLeft: '2rem' },
};

class CouchControls extends Component {

    render() {
        const controls = this;
        const app = controls.props.app;

        const urlField = (
            <div>
                <TextField
                    id="CouchDB_URL"
                    label='CouchDB URL:'
                    fullWidth
                    style={styles.textField}
                    defaultValue='http://mars.local:5984'
                />
            </div>);

const passwordField = (
    <div>
        <TextField
            id="password"
            label='Password:'
            fullWidth
            type='password'
            style={styles.textField}
        />
    </div>);

const usernameField = (
    <div>
        <TextField
            id="username"
            label='Username:'
            fullWidth
            style={styles.textField}
        />
    </div>);

        const connectButton = (
            <div>
                <Button
                    style={styles.button}
                    variant="contained"
                    color="primary"
                    onClick={app.handleConnect}>
                    Connect
                </Button>
            </div>
        );

        return (
            <React.Fragment>
                {usernameField}
                {passwordField}
                {urlField}
                {connectButton}
            </React.Fragment>
        );
    }
}

// exports
// ---------------------------------------------------------

export default CouchControls;