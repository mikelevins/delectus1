import React, { Component } from 'react';
import './App.css';

import AppBar from '@material-ui/core/AppBar';
import Button from '@material-ui/core/Button';
import Paper from '@material-ui/core/Paper';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';
import { withStyles } from '@material-ui/core/styles';

import LoginButton from './LoginButton.js';
import SignUpButton from './SignUpButton.js';

const styles = {
    grow: {
        flexGrow: 1,
    },
    menuButton: {
        marginLeft: -12,
        marginRight: 20,
    },
    root: {
        marginLeft: 'auto',
        marginRight: 'auto',
        width: '100%',
    },
};

class Landing extends Component {
    render() {
        const { classes } = this.props;
        return (
            <div id='appBackdrop' className={classes.root}>
                <AppBar position='static'>
                    <Toolbar>
                        <Typography variant='h6' 
                        color='inherit' 
                        className={classes.grow}>
                            Delectus
                        </Typography>
                        <LoginButton/>
                        <SignUpButton/>
                    </Toolbar>
                </AppBar>
                <Paper square={true} elevation={1}>
                    <Typography variant='h6' align='center'>Welcome to Delectus</Typography>
                </Paper>
            </div>
        );
    }
}

// exports
// ---------------------------------------------------------

export default withStyles(styles)(Landing);