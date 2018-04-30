import React, { Component } from "react";

// material-ui stuff
import AppBar from "material-ui/AppBar";
import Card from "material-ui/Card";
import MuiThemeProvider from "material-ui/styles/MuiThemeProvider";
import lightBaseTheme from "material-ui/styles/baseThemes/lightBaseTheme";
import getMuiTheme from "material-ui/styles/getMuiTheme";

// delectus' own includes
import "./App.css";

const cardStyle = {
  marginBottom: "1rem",
  marginLeft: "15%",
  marginRight: "15%",
  marginTop: "1rem",
  padding: "0.75rem",
  textAlign: "center"
};

const appBarStyle = {
  backgroundColor: "#4F3B27",
};

class App extends Component {
  constructor() {
    super();
    this.state = { theme: lightBaseTheme };
  }

  render() {
    document.body.style.backgroundColor = "#dfd6c4";
    return (
      <MuiThemeProvider muiTheme={getMuiTheme(this.state.theme)}>
        <div>
          <AppBar title="Delectus" style={appBarStyle} />
          <Card style={cardStyle}>
            <img src="icon.png" width="54" height="65" />
            <p>Your lists. Your way.</p>
            <p>
              $1.99 on the{" "}
              <a href="https://itunes.apple.com/us/app/delectus/id431505280?ls=1&mt=12">
                Mac App Store.
              </a>
            </p>
          </Card>

          <Card style={cardStyle}>
            <h2>Manual</h2>
            <p>
              Download the Delectus manual &nbsp;
              <a href="https://delectus.evins.net/downloads/Delectus_manual.zip">
                here
              </a>.
            </p>
          </Card>

          <Card style={cardStyle}>
            <h2>Support</h2>
            <p>
              Click &nbsp;
              <a href="mailto:delectus@evins.net?subject=About%20Delectus">
                here
              </a>{" "}
              for help and support.
            </p>
            <p>or send email to delectus@evins.net.</p>
          </Card>
        </div>
      </MuiThemeProvider>
    );
  }
}

export default App;
