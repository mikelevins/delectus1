import React, { Component } from 'react';
import IconButton from 'material-ui/IconButton';
import AddRow from 'material-ui/svg-icons/action/list';
import { BottomNavigation, BottomNavigationItem } from 'material-ui/BottomNavigation';
import Paper from 'material-ui/Paper';
import TextField from 'material-ui/TextField';

const addRowIcon = <IconButton><AddRow /></IconButton>;

/**
 * A simple example of `BottomNavigation`, with three labels and icons
 * provided. The selected `BottomNavigationItem` is determined by application
 * state (for instance, by the URL).
 */
class BottomBar extends Component {
    state = {
        selectedIndex: 0,
    };

    select = (index) => this.setState({ selectedIndex: index });

    render() {
        return (
            <Paper zDepth={1}>
                <BottomNavigation selectedIndex={this.state.selectedIndex}>
                    <BottomNavigationItem
                        label=""
                        icon={addRowIcon}
                        onClick={() => this.select(0)}
                    />
                    <TextField hintText="Search" />
                </BottomNavigation>
            </Paper>
        );
    }
}

export default BottomBar;