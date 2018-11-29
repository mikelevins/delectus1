import React from 'react';
import './App.css';

import CssBaseline from '@material-ui/core/CssBaseline';

import Browser from './Browser.js';
import CouchControls from './CouchControls.js';

// CouchInspector styles
// ---------------------------------------------------------

const styles = {
    controls: { width: '70%' },
    browser: {
      marginLeft: '1rem',
      marginTop: '0.5rem',
    },
    title: { marginLeft: '2rem' },
  };
  
// CouchInspector class
// ---------------------------------------------------------

function CouchInspector(props) {
    return (
        <React.Fragment>
            <CssBaseline />

            <h1 style={styles.title}>Couch Inspector</h1>

            <div style={styles.controls}>
                <CouchControls app={props.app} />
            </div>

            <div style={styles.browser}>
                <Browser app={props.app} />
            </div>

        </React.Fragment>
    );
}

// exports
// ---------------------------------------------------------

export default CouchInspector;

