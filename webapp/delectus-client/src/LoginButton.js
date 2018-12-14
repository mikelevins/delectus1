import React from 'react';

import Button from '@material-ui/core/Button';

// LoginButton styles
// ---------------------------------------------------------

const styles = {
    button: {
        marginLeft: '0rem',
        marginTop: '1.5rem',
        textTransform: 'none'
    },
};

function LoginButton(props) {

    const clickHandler = () => {
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
