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
  marginLeft: "20%",
  marginRight: "20%",
  marginTop: "1rem",
  padding: "1rem",
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
      <MuiThemeProvider muiTheme={getMuiTheme(this.state.theme)}>
        <div>
          <AppBar title="Delectus" />
          <Card style={cardStyle}>
            <img src="icon.png" width="108" height="131" />
            <p>Your lists. Your way.</p>
          </Card>
          <Card style={cardStyle}>
            <p>
              $1.99 on the{" "}
              <a href="https://itunes.apple.com/us/app/delectus/id431505280?ls=1&mt=12">
                Mac App Store.
              </a>
            </p>
            <h2>Manual</h2>
            <p>
              To download the Delectus manual, click{" "}
              <a href="https://delectus.evins.net/downloads/Delectus_manual.zip">
                this link
              </a>.
            </p>
            <h2>Support</h2>
            <p>
              For questions, comments, and help, contact us by
              <a href="mailto:delectus@evins.net?subject=About%20Delectus">
                {" "}
                clicking here
              </a>,
            </p>
            <p>or send email to delectus@evins.net.</p>
          </Card>
        </div>
      </MuiThemeProvider>
    );
  }
}

export default App;
