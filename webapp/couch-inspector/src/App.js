import React, { Component } from 'react';
import './App.css';

import CouchControls from './CouchControls.js';
import Browser from './Browser.js';

import CssBaseline from '@material-ui/core/CssBaseline';
import axios from 'axios';

const styles = {
  controls: { width: '70%' },
  browser: {
    marginLeft: '1rem',
    marginTop: '0.5rem',
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
      databasesPerPage: 10,
      databasesPageOffset: 0,
      selectedDatabase: null,
      selectedDocuments: [],
      keyPath: [], // the sequence of keys displayed in the browser
    };
  }

  // accessors
  // ---------------------------------------------------------

  getDatabases = () => {
    return this.state.databases;
  }

  setDatabases = (dbs) => {
    this.setState({ databases: dbs });
  }

  getDatabasesPerPage = () => {
    return this.state.databasesPerPage;
  }

  setDatabasesPerPage = (dbCount) => {
    this.setState({ databasesPerPage: dbCount });
  }

  getDatabasePageOffset = () => {
    return this.state.databasesPageOffset;
  }

  setDatabasePageOffset = (offset) => {
    this.setState({ databasePageOffset: offset });
  }

  getSelectedDatabase = () => {
    return this.state.selectedDatabase;
  }

  setSelectedDatabase = (dbName) => {
    this.setState({ selectedDatabase: dbName });
  }

  getSelectedDocuments = () => {
    return this.state.selectedDocuments;
  }

  setSelectedDocuments = (docList) => {
    this.setState({ selectedDocuments: docList });
  }


  // methods
  // ---------------------------------------------------------

  componentDidMount() {
    document.title = "Couch Inspector"
  } // end componentDidMount

  handleConnect = () => {
    const couch_url = document.getElementById('CouchDB_URL').value;
    axios.get(couch_url + '/_all_dbs')
      .then(
        (response) => this.setState({
          couchURL: couch_url,
          databases: response.data
        }));
  }

  setSelectedDatabase = (dbName) => {
    const app = this;
    const couchURL = app.state.couchURL;
    const offset = app.state.databasesPageOffset;
    const limit = app.state.databasesPerPage;
    const docsRequest = ('/' + dbName + '/_all_docs?limit=' + String(limit) + '&skip=' + String(offset));

    axios.get(couchURL + docsRequest)
      .then(response => app.setState({
        selectedDatabase: dbName,
        selectedDocuments: response.data.rows
      }))
      .catch((error) => {
        app.setState({
          selectedDatabase: null,
          selectedDocuments: []
        });
        if (error.response) {
          // the server returned an error response
          console.log(error.response.data);
          console.log(error.response.status);
          console.log(error.response.headers);
        } else {
          // the server never responded
          console.log("No response from the server");
        }
      })
  }

  // main render
  // ---------------------------------------------------------

  render() {
    const app = this;
    const dbList = app.getDatabases();

    if (dbList) {
      // we have databases; display them
      return (
        <React.Fragment>
          <CssBaseline />

          <h1 style={styles.title}>Couch Inspector</h1>

          <div style={styles.controls}>
            <CouchControls app={app} />
          </div>

          <div style={styles.browser}>
            <Browser app={app} />
          </div>

        </React.Fragment>
      );

    } else {
      // we don't have databases; display a blank div
      return (
        <React.Fragment>
          <CssBaseline />

          <h1 style={styles.title}>Couch Inspector</h1>

          <div style={styles.controls}>
            <CouchControls app={app} />
          </div>

          <div style={styles.browser}>&nbsp;</div>
          
        </React.Fragment>
      );
    }
  }
}

// exports
// ---------------------------------------------------------

export default App;

