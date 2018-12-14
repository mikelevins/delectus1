import React from 'react';
import TextField from '@material-ui/core/TextField';

// UsernameField styles
// ---------------------------------------------------------

const styles = {
    textField: {
        marginLeft: '0rem',
        marginTop: '0rem',
        width: '100%',
    },
};

function UsernameField(props) {
    return (
        <div>
            <TextField
                id="username"
                label='Username:'
                style={styles.textField}
                defaultValue=''
            />
        </div>
    );
}


// exports
// ---------------------------------------------------------

export default UsernameField;

