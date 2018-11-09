import React from 'react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Button from '@material-ui/core/Button';
import TextField from '@material-ui/core/TextField';

const styles = theme => ({
    button: {
        marginLeft: '1em',
    },
    buttonBar: {
        display: 'inline',
    },
    editBox: {
        width: '90%',
    },
    sectionHead: {
        fontWeight: 'bold',
        marginLeft: '1em',
        marginTop: '1em',
    },
    container: {
        display: 'block',
    },
    textField: {
        marginLeft: '2em',
        marginRight: '2em',
    },
    dense: {
        marginTop: 19,
    },
    menu: {
        width: 200,
    },
});

class DocEditor extends React.Component {
    state = {
        name: 'Cat in the Hat',
        age: '',
        multiline: 'Controlled',
        currency: 'EUR',
    };

    handleChange = name => event => {
        this.setState({
            [name]: event.target.value,
        });
    };

    render() {
        const { classes } = this.props;
        const app = this.props.app;
        const doc = this.props.doc;
        const doc_keys = Object.keys(doc).reverse();
        const doc_fields = doc_keys.map(
            (key) => {
                return (
                    <TextField
                        key={key}
                        id={key}
                        label={key}
                        className={classes.textField}
                        fullWidth={true}
                        multiline={true}
                        variant='outlined'
                        value={doc[key]}
                        onChange={this.handleChange('name')}
                        margin="normal"
                    />
                );
            }
        );

        return (
            <form className={classes.container} noValidate autoComplete="off">
                <div className={classes.sectionHead}>{"Editing document " + String(doc._id)}

                    <div className={classes.buttonBar}>
                        <Button
                            className={classes.button}
                            variant="contained"
                            color="primary"
                            onClick={app.cancelAndDismissDocumentEditor}>
                            Save
                        </Button>
                        <Button
                            className={classes.button}
                            variant="contained"
                            color="primary"
                            onClick={app.cancelAndDismissDocumentEditor}>
                            Cancel
                        </Button>
                    </div>
                </div>
                <div className={classes.editBox}>
                    {doc_fields}
                </div>
            </form>
        );
    }
}

DocEditor.propTypes = {
    classes: PropTypes.object.isRequired,
};

export default withStyles(styles)(DocEditor);