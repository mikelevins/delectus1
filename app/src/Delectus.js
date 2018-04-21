// react base
import React, { Component } from 'react';
// helmet for meta tags in the head
import { Helmet } from "react-helmet";

// material-ui stuff

import lightBaseTheme from 'material-ui/styles/baseThemes/lightBaseTheme';
import AppBar from 'material-ui/AppBar';
import IconButton from 'material-ui/IconButton';
import NavigationClose from 'material-ui/svg-icons/navigation/close';
import Settings from 'material-ui/svg-icons/action/settings';

// delectus' own includes
import ListBox from './ListBox';
import './Delectus.css';

class Delectus extends Component {
  constructor() {
    super();
    this.state = {
      'theme': lightBaseTheme,
    };
  }

  render() {
    return (
      <div>
        <Helmet>
          <title>Delectus</title>
        </Helmet>
        <AppBar
          title={<span>Delectus</span>}
          iconElementLeft={<IconButton><NavigationClose /></IconButton>}
          iconElementRight={<IconButton><Settings /></IconButton>}
        />
        <ListBox theme={this.state.theme}/>
      </div>
    );
  }
}

export default Delectus;
