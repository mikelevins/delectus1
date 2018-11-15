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
        const app = this.props.app;

        return (
            <React.Fragment>
                <div>
                    <TextField
                        id="CouchDB_URL"
                        label='CouchDB URL:'
                        fullWidth
                        style={styles.textField}
                        defaultValue='http://mars.local:5984'
                    />
                </div>
                <div>
                    <Button
                        style={styles.button}
                        variant="contained"
                        color="primary"
                        onClick={app.handleGetInfo}>
                        Get Couch Info
            </Button>
                    <Button
                        style={styles.button}
                        variant="contained"
                        color="primary"
                        onClick={app.handleGetDBs}>
                        Get Databases
            </Button>
                </div>

            </React.Fragment>
        );
    }
}

// exports
// ---------------------------------------------------------

export default CouchControls;