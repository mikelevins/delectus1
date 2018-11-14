import React, { Component } from 'react';
import './App.css';

import axios from 'axios';

import Button from '@material-ui/core/Button';
import CssBaseline from '@material-ui/core/CssBaseline';
import TextField from '@material-ui/core/TextField';
import { withStyles } from '@material-ui/core/styles';

const styles = theme => ({
  button: {
    marginLeft: '2rem',
    marginTop: '1rem',
  },
  h1: {
    marginLeft: '2rem',
  },
  textField: {
    marginLeft: '2rem',
  },
});

class App extends Component {

  // constructor
  // ---------------------------------------------------------

  constructor(props) {
    super(props);

    this.state = {};
  }

  // methods
  // ---------------------------------------------------------


  componentDidMount() {
    document.title = "Couch Inspector"
  } // end componentDidMount

  handleClick = () => {
    const couch_url = document.getElementById('CouchDB_URL').value;
    axios.get(couch_url)
      .then(response => this.setState({ couchInfo: response }));
  }

  // main render
  // ---------------------------------------------------------

  render() {
    const app = this;
    const { classes } = this.props;

    return (
      <React.Fragment>
        <CssBaseline />
        <h1 className={classes.h1}>Couch Inspector</h1>
        <div>
          <TextField
            id="CouchDB_URL"
            label='CouchDB URL:'
            className={classes.textField}
            defaultValue='http://mars.local:5984'
            margin="normal"
          />
        </div>
        <div>
          <Button
            className={classes.button}
            variant="contained"
            color="primary"
            onClick={app.handleClick}>
            Connect
        </Button>
        </div>
      </React.Fragment>
    );
  }
}

// exports
// ---------------------------------------------------------

export default withStyles(styles)(App);