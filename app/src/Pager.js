import React, { Component } from 'react';
import FlatButton from 'material-ui/FlatButton';
import Paper from 'material-ui/Paper';
import GoLeft from 'material-ui/svg-icons/hardware/keyboard-arrow-left';
import GoRight from 'material-ui/svg-icons/hardware/keyboard-arrow-right';

/**
 * A simple example of `BottomNavigation`, with three labels and icons
 * provided. The selected `BottomNavigationItem` is determined by application
 * state (for instance, by the URL).
 */
class Pager extends Component {
    state = {
        pageStart: 0,
        pageCount: 10,
    };

    render() {
        return (
            <Paper zDepth={0}>
            <FlatButton
                        icon={<GoLeft />}
                    />
            <span>{this.state.pageStart+1} of 1</span>
            <FlatButton
                        icon={<GoRight />}
                    />
            </Paper>
        );
    }
}

export default Pager;