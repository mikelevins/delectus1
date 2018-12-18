import React from 'react';

import Button from '@material-ui/core/Button';

function SignUpButton(props) {

    const clickHandler = () => {
        console.log('Sign Up clicked');
    };

    return (
        <Button
            color="inherit"
            onClick={clickHandler} >
            Sign Up
        </Button>
    );
}


// exports
// ---------------------------------------------------------

export default SignUpButton;
