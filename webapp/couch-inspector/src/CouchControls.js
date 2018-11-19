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

function URLField(props) {
    return (
        <div>
            <TextField
                id="CouchDB_URL"
                label='CouchDB URL:'
                fullWidth
                style={styles.textField}
                defaultValue='http://mars.local:5984'
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
                fullWidth
                style={styles.textField}
            />
        </div>
    );
}

function PasswordField(props) {
    return (
        <div>
            <TextField
                id="password"
                label='Password:'
                fullWidth
                type='password'
                style={styles.textField}
            />
        </div>
    );
}

function ConnectButton(props) {
    const app = props.app;
    return (
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
}

class CouchControls extends Component {

    render() {
        const controls = this;
        const app = controls.props.app;

        return (
            <React.Fragment>
                <UsernameField app={app} />
                <PasswordField app={app} />
                <URLField app={app} />
                <ConnectButton app={app} />
            </React.Fragment>
        );
    }
}

// exports
// ---------------------------------------------------------

export default CouchControls;