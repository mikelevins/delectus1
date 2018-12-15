import React, { Component } from 'react';
import './App.css';

import AppBar from '@material-ui/core/AppBar';
import Button from '@material-ui/core/Button';
import IconButton from '@material-ui/core/IconButton';
import MenuIcon from '@material-ui/icons/Menu';
import Paper from '@material-ui/core/Paper';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';
import { withStyles } from '@material-ui/core/styles';

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
                        <Typography variant='title' color='inherit' className={classes.grow}>
                            Delectus
                        </Typography>
                        <Button color="inherit">Log in</Button>
                        <Button color="inherit">Sign up</Button>
                    </Toolbar>
                </AppBar>
                <Paper square='true' elevation='1'>
                    <Typography variant='title' align='center'>Welcome to Delectus</Typography>
                </Paper>
            </div>
        );
    }
}

// exports
// ---------------------------------------------------------

export default withStyles(styles)(Landing);