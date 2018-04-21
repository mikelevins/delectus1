import React, { Component } from 'react';
import { Helmet } from "react-helmet";
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import darkBaseTheme from 'material-ui/styles/baseThemes/darkBaseTheme';
import lightBaseTheme from 'material-ui/styles/baseThemes/lightBaseTheme';
import getMuiTheme from 'material-ui/styles/getMuiTheme';
import AppBar from 'material-ui/AppBar';
import IconButton from 'material-ui/IconButton';
import NavigationClose from 'material-ui/svg-icons/navigation/close';
import Settings from 'material-ui/svg-icons/action/settings';
import PouchDB from 'pouchdb';
import cuid from 'cuid';
import './App.css';

const localDB = new PouchDB('delectus');

class App extends Component {
  constructor() {
    super();
    this.state = { 'theme': lightBaseTheme };
  }

  render() {
    return (
      <MuiThemeProvider muiTheme={getMuiTheme(this.state.theme)}>
        <Helmet>
          <title>Delectus</title>
        </Helmet>
        <AppBar
          title={<span>Delectus</span>}
          iconElementLeft={<IconButton><NavigationClose /></IconButton>}
          iconElementRight={<IconButton><Settings /></IconButton>}
        />
      </MuiThemeProvider>
    );
  }
}

export default App;
