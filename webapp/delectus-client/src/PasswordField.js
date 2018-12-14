import React from 'react';

import TextField from '@material-ui/core/TextField';

// PasswordField styles
// ---------------------------------------------------------

const styles = {
    textField: {
        marginLeft: '0rem',
        marginTop: '0rem',
        width: '100%',
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
