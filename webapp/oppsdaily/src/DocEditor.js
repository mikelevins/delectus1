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
    render() {
        const readOnlyFieldNames = ['_rev', '_id', 'date_received', 'message_id', 'content'];
        const { classes } = this.props;
        const app = this.props.app;
        const doc = this.props.doc;
        const doc_keys = Object.keys(doc).reverse();

        // create the button-click handlers
        const handleSaveClicked = (event) => {
            console.log('Handling Save...');

            var updatedDoc = {};
            // fill in the data from the displayed form
            doc_keys.forEach((key) => {
                updatedDoc[key] = document.getElementById(key).value;
            });
            console.log(updatedDoc);

            app.cancelAndDismissDocumentEditor();
        };

        const handleCancelClicked = (event) => {
            console.log('Handling Cancel...');
            console.log(doc);
            app.cancelAndDismissDocumentEditor();
        };

        // build the editor's text fields
        const doc_fields = doc_keys.map(
            (key) => {
                if (readOnlyFieldNames.includes(key)) {
                    // set the value attribute, so the text field is not editable
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
                            margin="normal"
                        />
                    );
                } else {
                    // set the defaultValue attribute, but not the value attribute,
                    // so the text field is editable
                    return (
                        <TextField
                            key={key}
                            id={key}
                            label={key}
                            className={classes.textField}
                            fullWidth={true}
                            multiline={true}
                            variant='outlined'
                            defaultValue={doc[key]}
                            margin="normal"
                        />
                    );
                }
            }
        );

        // build and return the DocEditor UI
        return (
            <form
                id="DocEditor"
                className={classes.container}
                noValidate
                autoComplete="off">
                <div className={classes.sectionHead}>{"Editing document " + String(doc._id)}

                    <div className={classes.buttonBar}>
                        {/* Save button */}
                        <Button
                            className={classes.button}
                            variant="contained"
                            color="primary"
                            onClick={handleSaveClicked}>
                            Save
                        </Button>
                        {/* Cancel button */}
                        <Button
                            className={classes.button}
                            variant="contained"
                            color="primary"
                            onClick={handleCancelClicked}>
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