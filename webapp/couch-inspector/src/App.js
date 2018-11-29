import React, { Component } from 'react';
import './App.css';

import axios from 'axios';

import CouchInspector from './CouchInspector.js';
import EmptyInspector from './EmptyInspector.js';

// auxiliary constructors
// ---------------------------------------------------------

function MakeAllDocumentsRequest(dbName, limit, offset) {
  return ('/' + dbName + '/_all_docs?limit=' + String(limit) + '&skip=' + String(offset));
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
  getDocumentsCount = () => { return this.state.documentsCount; }
  getSelectedDocument = () => { return this.state.selectedDocument; }
  getSelectedDocumentID = () => { return this.state.selectedDocumentID; }

  // view accessors
  // ---------------------------------------------------------

  getFormURL = () => { return (document.getElementById('CouchDB_URL').value); }

  // lifecycle events
  // ---------------------------------------------------------

  componentDidMount() { document.title = "Couch Inspector" }

  // update state with data from the server
  // ---------------------------------------------------------

  logServerError = (error, message) => {
    console.log(message);
    console.log(error.response.status);
    console.log(error.response.data);
    console.log(error.response.headers);
  }

  logConnectionError = (error, message) => {
    console.log(message);
    console.log(error);
  }

  updateServerConnection = () => {
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
          this.logServerError(error,"handleConnect: the server returned an error");
        } else {
          this.logConnectionError(error,"handleConnect: no response from the server");
        }
      })
  }

  updateSelectedDatabase = (dbName) => {
    const app = this;
    const couchURL = app.getCouchURL();
    const limit = app.getDocumentsPerPage();
    const docsRequest = MakeAllDocumentsRequest(dbName, limit, 0);

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
          this.logServerError(error,'App.updateSelectedDatabase: the server returned an error');
        } else {
          this.logConnectionError(error,"App.updateSelectedDatabase: no response from the server");
        }
      })
  }

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
          this.logServerError(error,'App.updateNextDatabasePage: the server returned an error');
        } else {
          this.logConnectionError(error,"App.updateNextDatabasePage: no response from the server");
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
          this.logServerError(error,'App.updateNextDatabasePage: the server returned an error');
        } else {
          this.logConnectionError(error,"App.updateNextDatabasePage: no response from the server");
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
          this.logServerError(error,'App.updateSelectedDocument: the server returned an error');
        } else {
          // the server never responded
          this.logConnectionError(error,"App.updateSelectedDocument: no response from the server");
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

