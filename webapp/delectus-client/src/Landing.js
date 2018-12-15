import React, { Component } from 'react';
import './App.css';

import Paper from '@material-ui/core/Paper';
import Typography from '@material-ui/core/Typography';

class Landing extends Component {

    render() {
        return (
            <div id='appBackdrop' className='appBackdrop'>
                <Paper>
                    <div className='top-bar'>
                        <Typography component='h1'>Delectus</Typography>
                        <div className='top-bar-right'>
                            <a href='/login'>Log in</a>
                            <a href='/signup'>Sign up</a>
                        </div>
                        <Typography component='p'>Welcome to Delectus</Typography>
                    </div>
                </Paper>
            </div>
        );
    }
}

// exports
// ---------------------------------------------------------

export default Landing;