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

function ConstructDocsRequest(dbName, limit, offset) {
  return ('/' + dbName + '/_all_docs?limit=' + String(limit) + '&skip=' + String(offset));
}

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

  // state accessors
  // ---------------------------------------------------------

  getCouchURL = () => {
    return this.state.couchURL;
  }

  setCouchURL = (url) => {
    this.setState({ couchURL: url });
  }

  getDatabases = () => {
    return this.state.databases;
  }

  setDatabases = (dbs) => {
    this.setState({
      databases: dbs,
      selectedDocuments: [],
    });
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

  // view accessors
  // ---------------------------------------------------------

  getFormURL = () => {
    return (document.getElementById('CouchDB_URL').value);
  }

  // methods
  // ---------------------------------------------------------

  componentDidMount() {
    document.title = "Couch Inspector"
  } // end componentDidMount

  handleConnect = () => {
    const couch_url = this.getFormURL();
    const requestStr = couch_url + '/_all_dbs';

    this.setCouchURL(couch_url);
    axios.get(requestStr).then(
      (response) => this.setDatabases(response.data)
    );
  }

  setSelectedDatabase = (dbName) => {
    const app = this;
    const couchURL = app.state.couchURL;
    const offset = app.state.databasesPageOffset;
    const limit = app.state.databasesPerPage;
    const docsRequest = ConstructDocsRequest(dbName, limit, offset);

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

