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
      selectedDBDocuments: [],
      selectedDocument: null,
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
  getSelectedDBDocuments = () => { return this.state.selectedDBDocuments; }
  getSelectedDocument = () => { return this.state.selectedDocument; }

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
          selectedDBDocuments: [],
          selectedDocument: null
        })
      )
      .catch((error) => {
        app.setState({
          couchURL: new_couch_url,
          databases: [],
          selectedDatabase: null,
          selectedDBDocuments: [],
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
    const limit = app.getdocumentsPerPage();
    const docsRequest = MakeDocsRequest(dbName, limit, offset);

    axios.get(couchURL + docsRequest)
      .then(response => {
        app.setState({
          selectedDatabase: dbName,
          selectedDBDocuments: response.data.rows,
          selectedDocument: null
        })
      })
      .catch((error) => {
        app.setState({
          selectedDatabase: null,
          selectedDBDocuments: [],
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

  updateNextDatabasePage = () => {
    const app = this;
    const couchURL = app.getCouchURL();
    const dbName = app.getSelectedDatabase();
    const limit = app.getdocumentsPerPage();
    const offset = app.getDatabasePageOffset()+limit;
    const docsRequest = MakeDocsRequest(dbName, limit, offset);

    axios.get(couchURL + docsRequest)
      .then(response => {
        app.setState({ 
          documentsPageOffset: offset,
          selectedDBDocuments: response.data.rows,
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
    const limit = app.getdocumentsPerPage();
    const oldOffset = app.getDatabasePageOffset();
    const newOffset = (oldOffset-limit <= 0) ? 0 : (oldOffset-limit);
    const docsRequest = MakeDocsRequest(dbName, limit, newOffset);

    axios.get(couchURL + docsRequest)
      .then(response => {
        app.setState({ 
          documentsPageOffset: newOffset,
          selectedDBDocuments: response.data.rows,
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
    app.setState({ 
      selectedDocument: documentID
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

