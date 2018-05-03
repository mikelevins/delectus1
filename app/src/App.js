// react base
import React, { Component } from "react";

// material-ui stuff
import MuiThemeProvider from "material-ui/styles/MuiThemeProvider";
import lightBaseTheme from "material-ui/styles/baseThemes/lightBaseTheme";
import getMuiTheme from "material-ui/styles/getMuiTheme";
import {brown700, brown900, redA200} from 'material-ui/styles/colors';

// delectus' own includes
import "./App.css";
import DelectusList from "./DelectusList";

const muiTheme = getMuiTheme({
  palette: {
      primary1Color: brown700,
      primary2Color: brown900,
      accent1Color: redA200,
      pickerHeaderColor: brown700,
  },
});

class App extends Component {
  constructor() {
    super();
    this.state = { theme: muiTheme };
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
