// react base
import React, { Component } from "react";
// helmet for meta tags in the head
import { Helmet } from "react-helmet";

// material-ui stuff

import lightBaseTheme from "material-ui/styles/baseThemes/lightBaseTheme";

// utilities
import cuid from "cuid";

// delectus' own includes
import DelectusAppBar from "./DelectusAppBar";
import ListBox from "./ListBox";
import "./DelectusList.css";
import mockList from "./MockData";

class DelectusList extends Component {
  constructor() {
    super();
    this.state = {
      theme: lightBaseTheme,
      list: mockList
    };
  }

  render() {
    return (
      <div>
        <Helmet>
          <title>Delectus</title>
        </Helmet>

        <DelectusAppBar title={this.state.list.title} />

        <p className="DelectusListNote">{this.state.list.note}</p>

        <ListBox theme={this.state.theme} list={this.state.list} />
      </div>
    );
  }
}

export default DelectusList;
