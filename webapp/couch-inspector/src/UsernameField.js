import React from 'react';
import TextField from '@material-ui/core/TextField';

// UsernameField styles
// ---------------------------------------------------------

const styles = {
    textField: {
        marginLeft: '1rem',
        marginTop: '0.5rem',
        width: '95%',
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

