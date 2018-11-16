import React, { Component } from 'react';
import './App.css';

import CouchControls from './CouchControls.js';
import Browser from './Browser.js';

import CssBaseline from '@material-ui/core/CssBaseline';
import axios from 'axios';

const styles = {
  controls: {
    width: '70%',
  },
  browser: {
    marginLeft: '2rem',
    marginTop: '1rem',
  },
  title: { marginLeft: '2rem' },
};

class App extends Component {

  // constructor
  // ---------------------------------------------------------

  constructor(props) {
    super(props);

    this.state = {
      couchURL: '',
      databases: null,
      selectedDatabase: [],
      selectedDocuments: [],
      keyPath: [], // the sequence of keys displayed in the browser
    };
  }

  // methods
  // ---------------------------------------------------------

  componentDidMount() {
    document.title = "Couch Inspector"
  } // end componentDidMount

  handleConnect = () => {
    const couch_url = document.getElementById('CouchDB_URL').value;

    axios.get(couch_url + '/_all_dbs')
      .then(response => this.setState({
        couchURL: couch_url,
        databases: response.data
      }));
  }

  // main render
  // ---------------------------------------------------------

  render() {
    const app = this;

    return (
      <React.Fragment>
        <CssBaseline />

        <h1 style={styles.title}>Couch Inspector</h1>
        <div style={styles.controls}>
          <CouchControls app={app} />
        </div>

        {(app.state.databases) ? (

          // we have databases; display them
          <div style={styles.browser}>
            <Browser app={app} />
          </div>

        ) : (
            // we don't have databases; display a blank div
            <div style={styles.browser}>&nbsp;</div>

          )}

      </React.Fragment>
    );
  }
}

// exports
// ---------------------------------------------------------

export default App;