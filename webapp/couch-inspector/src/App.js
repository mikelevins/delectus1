import React, { Component } from 'react';
import './App.css';

import CouchControls from './CouchControls.js';
import Browser from './Browser.js';

import CssBaseline from '@material-ui/core/CssBaseline';
import axios from 'axios';

// App styles
// ---------------------------------------------------------

const styles = {
  controls: { width: '70%' },
  browser: {
    marginLeft: '1rem',
    marginTop: '0.5rem',
  },
  title: { marginLeft: '2rem' },
};

// auxiliary constructors
// ---------------------------------------------------------

function MakeDocsRequest(dbName, limit, offset) {
  return ('/' + dbName + '/_all_docs?limit=' + String(limit) + '&skip=' + String(offset));
}

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

function EmptyInspector(props) {
  return (
    <React.Fragment>
      <CssBaseline />

      <h1 style={styles.title}>Couch Inspector</h1>

      <div style={styles.controls}>
        <CouchControls app={props.app} />
      </div>

      <div style={styles.browser}>&nbsp;</div>

    </React.Fragment>
  );
}

// App class
// ---------------------------------------------------------

class App extends Component {

  // constructor
  // ---------------------------------------------------------

  constructor(props) {
    super(props);

    this.state = {
      couchURL: '',
      databases: null,
      documentsPerPage: 10,
      documentsPageOffset: 0,
      selectedDatabase: null,
      selectedDocuments: [],
      keyPath: [], // the sequence of keys displayed in the browser
    };
  }

  // state accessors
  // ---------------------------------------------------------

  getCouchURL = () => { return this.state.couchURL; }

  getDatabases = () => { return this.state.databases; }

  getdocumentsPerPage = () => { return this.state.documentsPerPage; }

  getDatabasePageOffset = () => { return this.state.documentsPageOffset; }

  getSelectedDatabase = () => { return this.state.selectedDatabase; }

  getSelectedDocuments = () => { return this.state.selectedDocuments; }

  // view accessors
  // ---------------------------------------------------------

  getFormURL = () => { return (document.getElementById('CouchDB_URL').value); }

  // event handling
  // ---------------------------------------------------------

  componentDidMount() {
    document.title = "Couch Inspector"
  }

  handleConnect = () => {
    const app = this;
    const new_couch_url = this.getFormURL();
    const requestStr = new_couch_url + '/_all_dbs';

    axios.get(requestStr)
      .then(
        (response) => this.setState({
          couchURL: new_couch_url,
          databases: response.data,
          selectedDocuments: []
        })
      )
      .catch((error) => {
        app.setState({
          couchURL: new_couch_url,
          databases: [],
          selectedDatabase: null,
          selectedDocuments: []
        });

        if (error.response) {
          // the server returned an error response
          console.log("handleConnect: the server returned an error");
          console.log(error.response.status);
          console.log(error.response.data);
          console.log(error.response.headers);
        } else {
          // the server never responded
          console.log("handleConnect: no response from the server");
        }
      })
  }

  updateSelectedDatabase = (dbName) => {
    const app = this;
    const couchURL = app.getCouchURL();
    const offset = app.getDatabasePageOffset();
    const limit = app.getdocumentsPerPage();
    const docsRequest = MakeDocsRequest(dbName, limit, offset);

    axios.get(couchURL + docsRequest)
      .then(response => {
        console.log(response);
        app.setState({
          selectedDatabase: dbName,
          selectedDocuments: response.data.rows
        })
      })
      .catch((error) => {
        app.setState({
          selectedDatabase: null,
          selectedDocuments: []
        });
        if (error.response) {
          // the server returned an error response
          console.log(error.response.status);
          console.log(error.response.data);
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
      return (<CouchInspector app={app} />);

    } else {
      // we don't have databases; display a blank div
      return (<EmptyInspector app={app} />);
    }
  }
}

// exports
// ---------------------------------------------------------

export default App;

