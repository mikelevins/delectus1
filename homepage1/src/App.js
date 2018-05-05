import React, { Component } from "react";

// Helmet for controlling the head section
import { Helmet } from "react-helmet";

// material-ui stuff
import AppBar from "material-ui/AppBar";
import Card from "material-ui/Card";
import { GridList, GridTile } from "material-ui/GridList";
import MuiThemeProvider from "material-ui/styles/MuiThemeProvider";
import lightBaseTheme from "material-ui/styles/baseThemes/lightBaseTheme";
import getMuiTheme from "material-ui/styles/getMuiTheme";

// delectus' own includes
import "./App.css";
import "typeface-roboto";

const appBarStyle = {
  backgroundColor: "#4F3B27"
};

const rootStyle = {
  display: "flex",
  flexWrap: "wrap",
  justifyContent: "space-around"
};

const gridListStyle = {
  width: "90%",
  margin: "auto",
  overflowY: "auto"
};

const cardStyle = {
  margin: "auto",
  paddingLeft: "1rem",
  paddingRight: "1rem",
  paddingTop: "1rem",
  height: "100%",
  textAlign: "center"
};

class App extends Component {
  constructor() {
    super();
    this.state = { theme: lightBaseTheme };
  }

  render() {
    document.body.style.backgroundColor = "#dfd6c4";
    return (
      <div>
        <Helmet>
          <title>Delectus</title>
        </Helmet>
        <MuiThemeProvider muiTheme={getMuiTheme(this.state.theme)}>
          <div style={rootStyle}>
            <AppBar title="Delectus" style={appBarStyle} />

            <Card style={cardStyle}>
              <img src="icon.png" alt="Delectus icon" width="54" height="65" />
              <p>
                <strong>Your lists. Your way.</strong>
              </p>
              <p>
                $1.99 on the{" "}
                <a href="https://itunes.apple.com/us/app/delectus/id431505280?ls=1&mt=12">
                  Mac App Store.
                </a>
              </p>
            </Card>
            <Card style={cardStyle}>
              <h2>Manual</h2>
              <div>
                <p>
                  Download the Delectus manual &nbsp;
                  <a href="https://delectus.evins.net/downloads/Delectus_manual.zip">
                    here
                  </a>.
                </p>
              </div>
            </Card>
            <Card style={cardStyle}>
              <h2>Support</h2>
              <div>
                <p>
                  Click &nbsp;
                  <a href="mailto:delectus@evins.net?subject=About%20Delectus">
                    here
                  </a>{" "}
                  for help and support.
                </p>
                <p>or send email to delectus@evins.net.</p>
              </div>
            </Card>
          </div>
        </MuiThemeProvider>
      </div>
    );
  }
}

export default App;
