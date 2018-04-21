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

const mockColumns = ["Name", "Color", "Distinction", "Count"];

const mockFields = [
  ["Apples", "red", "tangy", 3],
  ["Bananas", "yellow", "mushy", 4],
  ["Cherries", "red", "sweet", 35],
  ["Dates", "brown", "fibery", 22],
  ["Elephants", "gray", "awesome", 0],
  ["Waffles", "tan", "buttery", 2]
];

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
        <ListBox 
        theme={this.state.theme}
        columns={mockColumns}
        rows={mockFields}
        />
      </div>
    );
  }
}

export default Delectus;
