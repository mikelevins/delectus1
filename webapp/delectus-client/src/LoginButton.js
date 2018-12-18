import React from 'react';

import Button from '@material-ui/core/Button';

function LoginButton(props) {

    const clickHandler = () => {
        console.log('Log In clicked');
    };

    return (
        <Button
            color="inherit"
            onClick={clickHandler} >
            Log In
        </Button>
    );
}


// exports
// ---------------------------------------------------------

export default LoginButton;
