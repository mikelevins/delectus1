import React, { Component } from 'react';
import './App.css';
import EmptyBlock from './EmptyBlock.js';
import Presenter from './Presenter.js';

import axios from 'axios';

import Button from '@material-ui/core/Button';
import TextField from '@material-ui/core/TextField';


const styles = {
  button: {
    marginLeft: '2rem',
    marginTop: '1rem',
  },
  presenter: { 
    marginLeft: '2rem',
    marginTop: '1rem',
  },
  title: { marginLeft: '2rem' },
  textField: { marginLeft: '2rem' },
};

class App extends Component {

  // constructor
  // ---------------------------------------------------------

  constructor(props) {
    super(props);

    this.state = {
      couchInfo: null
    };
  }

  // methods
  // ---------------------------------------------------------


  componentDidMount() {
    document.title = "Couch Inspector"
  } // end componentDidMount

  handleGetInfo = () => {
    const couch_url = document.getElementById('CouchDB_URL').value;
    axios.get(couch_url)
      .then(response => this.setState({ couchInfo: response.data }));
  }

  handleGetDBs = () => {
    const couch_url = document.getElementById('CouchDB_URL').value;
    axios.get(couch_url + '/_all_dbs')
      .then(response => this.setState({ couchInfo: response.data }));
  }

  // main render
  // ---------------------------------------------------------

  render() {
    const app = this;
    const couchInfo = app.state.couchInfo;

    return (
      <React.Fragment>
        <h1 style={styles.title}>Couch Inspector</h1>
        <div>
          <TextField
            id="CouchDB_URL"
            label='CouchDB URL:'
            style={styles.textField}
            defaultValue='http://mars.local:5984'
          />
          <Button
            style={styles.button}
            variant="contained"
            color="primary"
            onClick={app.handleGetInfo}>
            Get Couch Info
        </Button>
          <Button
            style={styles.button}
            variant="contained"
            color="primary"
            onClick={app.handleGetDBs}>
            Get Databases
        </Button>
        </div>
        <div style={styles.presenter}>

          {(couchInfo) ?
            (<Presenter object={couchInfo} />) :
            (<EmptyBlock />)}

        </div>
      </React.Fragment>
    );
  }
}

// exports
// ---------------------------------------------------------

export default App;