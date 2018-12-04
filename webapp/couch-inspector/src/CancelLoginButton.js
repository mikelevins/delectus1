import React from 'react';

import Button from '@material-ui/core/Button';

// CancelLoginButton styles
// ---------------------------------------------------------

const styles = {
    button: {
        marginLeft: '1rem',
        marginTop: '1.5rem',
        textTransform: 'none'
    },
};

function CancelLoginButton(props) {
    const app = props.app;
    const dbName = props.database;

    const clickHandler = () => {
        app.handleLoginCanceled();
    };

    return (
        <Button
            style={styles.button}
            variant="contained"
            color="primary"
            onClick={clickHandler} >
            Cancel
        </Button>
    );
}


// exports
// ---------------------------------------------------------

export default CancelLoginButton;
