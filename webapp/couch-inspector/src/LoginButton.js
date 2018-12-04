import React from 'react';

import Button from '@material-ui/core/Button';

// LoginButton styles
// ---------------------------------------------------------

const styles = {
    button: {
        marginLeft: '1rem',
        marginTop: '1.5rem',
        textTransform: 'none'
    },
};

function LoginButton(props) {
    const app = props.app;
    const dbName = props.database;

    const clickHandler = () => {
        const username = app.getLoginUsername();
        const password = app.getLoginPassword();
        app.handleLogin(dbName, username, password);
    };

    return (
        <Button
            style={styles.button}
            variant="contained"
            color="primary"
            onClick={clickHandler} >
            Log In
        </Button>
    );
}


// exports
// ---------------------------------------------------------

export default LoginButton;
