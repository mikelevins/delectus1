import React, { Component } from 'react';
import './App.css';

import CssBaseline from '@material-ui/core/CssBaseline';
import { withStyles } from '@material-ui/core/styles';


const styles = theme => ({
    h1: {
        marginLeft: '4rem',
        marginRight: '4rem',
    },
});

class ObjectPresentation extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        const { classes } = this.props;

        return (
            <React.Fragment>
                <CssBaseline />
                <div className={classes.h1}>
                    <p>ObjectPresentation</p>
                </div>
            </React.Fragment>
        );
    }
}

// exports
// ---------------------------------------------------------

export default withStyles(styles)(ObjectPresentation);