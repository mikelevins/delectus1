import React, { Component } from 'react';
import './App.css';

import CssBaseline from '@material-ui/core/CssBaseline';
import { MuiThemeProvider, createMuiTheme, withTheme } from '@material-ui/core/styles';

import PouchDB from 'pouchdb';

import Landing from './Landing.js';

// ---------------------------------------------------------
// theme
// ---------------------------------------------------------

const delectusTheme = createMuiTheme({
  typography: { useNextVariants: true }
});

// ---------------------------------------------------------
// App component
// ---------------------------------------------------------

class App extends Component {

  // constructor
  // ---------------------------------------------------------

  constructor(props) {
    super(props);

    var remoteCouch = localStorage.getItem('delectus_couchURL');
    if (!remoteCouch) { remoteCouch = this.defaultCouchURL(); }

    this.state = {
      username: localStorage.getItem('delectus_username'),
      couchURL: remoteCouch
    }
  }

  // default values
  // ---------------------------------------------------------

  // TODO: change to public Delectus couch when it's available
  defaultCouchURL = () => 'http://mars.local:5984';

  // lifecycle methods
  // ---------------------------------------------------------

  initDatabases = () => {
    const url = this.state.couchURL;
    const username = this.state.username;
    const localDB = new PouchDB('Delectus');
    var remoteDB = null;

    if (username && url) { remoteDB = (url + '/' + username); }

    this.setState({
      localDB: localDB,
      remoteDB: remoteDB
    });
  }

  componentDidMount() { this.initDatabases(); }

  // main render
  // ---------------------------------------------------------

  render() {
    console.log(this.props.theme);
    return (
      <React.Fragment>
        <CssBaseline />
        <MuiThemeProvider theme={delectusTheme}>
          <Landing />
        </MuiThemeProvider>
      </React.Fragment>
    );
  }

}

// ---------------------------------------------------------
// main export
// ---------------------------------------------------------

export default withTheme()(App);
