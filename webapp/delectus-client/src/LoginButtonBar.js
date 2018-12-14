import React, { Component } from 'react';
import './App.css';

import Button from '@material-ui/core/Button';

import CancelLoginButton from './CancelLoginButton.js';
import LoginButton from './LoginButton.js';

const styles = {
    loginButtonBar: {
        textAlign: 'center'
    }
};

class LoginButtonBar extends Component {

    render() {
        return (
            <div id='loginButtonBar' style={styles.loginButtonBar}>
                <LoginButton />
                <CancelLoginButton />
            </div>
        );
    }
}

// exports
// ---------------------------------------------------------

export default LoginButtonBar;