import React from 'react';

import TextField from '@material-ui/core/TextField';

// PasswordField styles
// ---------------------------------------------------------

const styles = {
    textField: {
        marginLeft: '1rem',
        marginTop: '0.5rem',
        width: '95%',
    },
};

function PasswordField(props) {
    return (
        <div>
            <TextField
                id="password"
                label='Password:'
                style={styles.textField}
                type='password'
                defaultValue=''
            />
        </div>
    );
}

// exports
// ---------------------------------------------------------

export default PasswordField;
