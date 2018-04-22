import React, { Component } from 'react';
import FlatButton from 'material-ui/FlatButton';
import AddRow from 'material-ui/svg-icons/action/list';
import { BottomNavigation } from 'material-ui/BottomNavigation';
import Paper from 'material-ui/Paper';
import TextField from 'material-ui/TextField';

import Pager from './Pager';

const addRowStyle = {
    flex: "none",
};

class BottomBar extends Component {
    state = {
        selectedIndex: 0,
    };

    select = (index) => this.setState({ selectedIndex: index });

    render() {
        return (
            <Paper zDepth={1}>
                <BottomNavigation selectedIndex={this.state.selectedIndex}>
                    <FlatButton
                        style={addRowStyle}
                        label="+"
                        labelPosition="before"
                        icon={<AddRow />}
                    />
                    <TextField hintText="Search" />
                    <Pager />
                </BottomNavigation>
            </Paper>
        );
    }
}

export default BottomBar;