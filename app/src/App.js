// react base
import React, { Component } from 'react';

// material-ui stuff
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import lightBaseTheme from 'material-ui/styles/baseThemes/lightBaseTheme';
import getMuiTheme from 'material-ui/styles/getMuiTheme';

// delectus' own includes
import './Delectus';
import './App.css';
import DelectusList from './DelectusList';

class App extends Component {
  constructor() {
    super();
    this.state = { 'theme': lightBaseTheme };
  }

  render() {
    return (
      <MuiThemeProvider muiTheme={getMuiTheme(this.state.theme)}>
        <DelectusList />
      </MuiThemeProvider>
    );
  }
}

export default App;
