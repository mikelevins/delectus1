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

function MakeAllDocumentsRequest(dbName, limit, offset) {
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
      databaseCount: 0,
      documentsCount: 0,
      documentsPerPage: 10,
      documentsPageOffset: 0,
      selectedDatabase: null,
      documents: [],
      selectedDocumentID: null,
      selectedDocument: null,
      keyPath: [], // the sequence of keys displayed in the browser
    };
  }

  // state accessors
  // ---------------------------------------------------------

  getCouchURL = () => { return this.state.couchURL; }
  getDatabases = () => { return this.state.databases; }
  getDocumentsPerPage = () => { return this.state.documentsPerPage; }
  getDatabasePageOffset = () => { return this.state.documentsPageOffset; }
  getSelectedDatabase = () => { return this.state.selectedDatabase; }
  getDocuments = () => { return this.state.documents; }
  getSelectedDocument = () => { return this.state.selectedDocument; }
  getSelectedDocumentID = () => { return this.state.selectedDocumentID; }

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
        (response) => {
          this.setState({
            databaseCount: response.data.length,
            couchURL: new_couch_url,
            databases: response.data,
            documentsPageOffset: 0,
            documents: [],
            selectedDocument: null
          })
        }
      )
      .catch((error) => {
        app.setState({
          couchURL: new_couch_url,
          databases: [],
          selectedDatabase: null,
          documentsPageOffset: 0,
          documents: [],
          selectedDocument: null
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
    const limit = app.getDocumentsPerPage();
    const docsRequest = MakeAllDocumentsRequest(dbName, limit, offset);

    axios.get(couchURL + docsRequest)
      .then(response => {
        app.setState({
          documentsCount: response.data.total_rows,
          selectedDatabase: dbName,
          documentsPageOffset: 0,
          documents: response.data.rows,
          selectedDocument: null
        })
      })
      .catch((error) => {
        app.setState({
          selectedDatabase: null,
          documentsPageOffset: 0,
          documents: [],
          selectedDocument: null
        });
        if (error.response) {
          // the server returned an error response
          console.log('App.updateSelectedDatabase: the server returned an error');
          console.log(error.response.status);
          console.log(error.response.data);
          console.log(error.response.headers);
        } else {
          // the server never responded
          console.log("App.updateSelectedDatabase: no response from the server");
        }
      })
  }

  // BUG: if you select a database and page forward, then when
  // you select a second database, the displayed results for the new
  // database are also paged forward

  updateNextDatabasePage = () => {
    const app = this;
    const couchURL = app.getCouchURL();
    const dbName = app.getSelectedDatabase();
    const limit = app.getDocumentsPerPage();
    const offset = app.getDatabasePageOffset() + limit;
    const docsRequest = MakeAllDocumentsRequest(dbName, limit, offset);

    axios.get(couchURL + docsRequest)
      .then(response => {
        app.setState({
          documentsCount: response.data.total_rows,
          documentsPageOffset: offset,
          documents: response.data.rows,
          selectedDocument: null
        })
      })
      .catch((error) => {
        if (error.response) {
          // the server returned an error response
          console.log('App.updateNextDatabasePage: the server returned an error');
          console.log(error.response.status);
          console.log(error.response.data);
          console.log(error.response.headers);
        } else {
          // the server never responded
          console.log("App.updateNextDatabasePage: no response from the server");
        }
      })
  }

  updatePreviousDatabasePage = () => {
    const app = this;
    const couchURL = app.getCouchURL();
    const dbName = app.getSelectedDatabase();
    const limit = app.getDocumentsPerPage();
    const oldOffset = app.getDatabasePageOffset();
    const newOffset = (oldOffset - limit <= 0) ? 0 : (oldOffset - limit);
    const docsRequest = MakeAllDocumentsRequest(dbName, limit, newOffset);

    axios.get(couchURL + docsRequest)
      .then(response => {
        app.setState({
          documentsCount: response.data.total_rows,
          documentsPageOffset: newOffset,
          documents: response.data.rows,
          selectedDocument: null
        })
      })
      .catch((error) => {
        if (error.response) {
          // the server returned an error response
          console.log('App.updateNextDatabasePage: the server returned an error');
          console.log(error.response.status);
          console.log(error.response.data);
          console.log(error.response.headers);
        } else {
          // the server never responded
          console.log("App.updateNextDatabasePage: no response from the server");
        }
      })
  }

  updateSelectedDocument = (documentID) => {
    const app = this;
    const couchURL = app.getCouchURL();
    const dbName = app.getSelectedDatabase();
    const docRequest = ('/' + dbName + '/' + documentID);

    axios.get(couchURL + docRequest)
      .then(response => {
        console.log(response);
        app.setState({
          selectedDocumentID: documentID,
          selectedDocument: response.data,
        })
      })
      .catch((error) => {
        if (error.response) {
          // the server returned an error response
          console.log('App.updateSelectedDocument: the server returned an error');
          console.log(error.response.status);
          console.log(error.response.data);
          console.log(error.response.headers);
        } else {
          // the server never responded
          console.log("App.updateSelectedDocument: no response from the server");
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

