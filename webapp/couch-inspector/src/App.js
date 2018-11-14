import React, { Component } from 'react';
import './App.css';

import axios from 'axios';

import Button from '@material-ui/core/Button';
import CssBaseline from '@material-ui/core/CssBaseline';
import { withStyles } from '@material-ui/core/styles';

const styles = theme => ({
  button: { marginLeft: '1em' },
  buttonBar: { display: 'inline' },
  container: { display: 'block' },
  dense: { marginTop: 19 },
  editBox: { width: '90%' },
  menu: { width: 200 },
  sectionHead: {
      fontWeight: 'bold',
      marginLeft: '1em',
      marginTop: '1em',
  },
  textField: {
      marginLeft: '2em',
      marginRight: '2em',
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
    axios.get('http://mars.local:5984')
      .then(response => this.setState({ couchInfo: response }));
    console.log('Sucess!');
  }

  // main render
  // ---------------------------------------------------------

  render() {
    const app = this;
    const { classes } = this.props;

    return (
      <React.Fragment>
        <CssBaseline />
        <Button
          className={classes.button}
          variant="contained"
          color="primary"
          onClick={app.handleClick}>
          Click Me
        </Button>

      </React.Fragment>
    );
  }
}

// exports
// ---------------------------------------------------------

export default withStyles(styles)(App);