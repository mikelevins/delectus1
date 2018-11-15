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
      databases: null
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
      .then(response => this.setState({ databases: response.data }));
  }

  // main render
  // ---------------------------------------------------------

  render() {
    const app = this;
    const leftPaneTitle = 'Databases';
    const leftPaneList = app.state.databases;

    return (
      <React.Fragment>
        <CssBaseline />

        <h1 style={styles.title}>Couch Inspector</h1>
        <div style={styles.controls}>
          <CouchControls app={app} />
        </div>

        <div style={styles.browser}>
          <Browser title={leftPaneTitle} leftPaneList={leftPaneList}/>
        </div>

      </React.Fragment>
    );
  }
}

// exports
// ---------------------------------------------------------

export default App;