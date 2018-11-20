import React, { Component } from 'react';
import './App.css';

import Button from '@material-ui/core/Button';
import TextField from '@material-ui/core/TextField';

const styles = {
    controlsPane: {
        border: '1px solid black',
        marginLeft: '2rem',
    },
    button: {
        marginBottom: '01rem',
        marginLeft: '01rem',
        marginTop: '1rem',
    },
    textField: { 
        marginLeft: '1rem',
        marginTop: '1rem',
        width: '95%',
     },
};

function URLField(props) {
    return (
        <div>
            <TextField
                id="CouchDB_URL"
                label='CouchDB URL:'
                style={styles.textField}
                defaultValue='http://mars.local:5984'
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
            <div style={styles.controlsPane}>
                <URLField app={app} />
                <ConnectButton app={app} />
            </div>
        );
    }
}

// exports
// ---------------------------------------------------------

export default CouchControls;