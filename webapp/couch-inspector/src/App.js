import React, { Component } from 'react';
import './App.css';
import CouchControls from './CouchControls.js';
import EmptyBlock from './EmptyBlock.js';
import Presenter from './Presenter.js';


import axios from 'axios';

const styles = {
  controls: {
    width: '70%',
  },
  presenter: {
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
        <div style={styles.controls}>
          <CouchControls app={app} />
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