// react base
import React, { Component } from "react";

// PouchDB for storage
import PouchDB from 'pouchdb';


// material-ui stuff
import MuiThemeProvider from "material-ui/styles/MuiThemeProvider";
import getMuiTheme from "material-ui/styles/getMuiTheme";
import {brown700, brown900, redA200} from 'material-ui/styles/colors';

// delectus' own includes
import "./App.css";
import DelectusList from "./DelectusList";
import mockList from "./MockData";

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
    this.localdb = PouchDB('delectus');
    this.state = { theme: muiTheme };

    // peek at the PuchDB
    this.localdb.info().then(function(info){
      console.log(info);
    });
  }

  render() {
    return (
      <MuiThemeProvider muiTheme={getMuiTheme(this.state.theme)}>
        <DelectusList list={mockList} />
      </MuiThemeProvider>
    );
  }
}

export default App;
