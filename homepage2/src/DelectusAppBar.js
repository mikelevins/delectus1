// react base
import React from "react";

// material-ui stuff

import AppBar from "material-ui/AppBar";

// Delectus stuff
import AppBarButtons from "./AppBarButtons";

const DelectusAppBar = props => (
  <AppBar
    title={<span>{props.title}</span>}
    iconElementRight={<AppBarButtons />}
  />
);

export default DelectusAppBar;
