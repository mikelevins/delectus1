import React from 'react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Button from '@material-ui/core/Button';
import TextField from '@material-ui/core/TextField';

const styles = theme => ({
    button: { marginLeft: '1em' },
    buttonBar: { display: 'inline' },
    container: { display: 'block' },
    dense: { marginTop: 19 },
    editBox: { width: '90%' },
    menu: { width: 200 },
    sectionHead: {
        fontWeight: 'bold',
        marginLeft: '1em',
        marginTop: '1em',
    },
    textField: {
        marginLeft: '2em',
        marginRight: '2em',
    },
});

const readOnlyFieldNames = ['_rev', '_id', 'date_received', 'message_id', 'content'];

function getDocKeys(doc) { return Object.keys(doc).reverse(); };

class DocEditor extends React.Component {

    docValuesFromForm = () => {
        const doc = this.props.doc;
        const doc_keys = getDocKeys(doc);
        var updatedDoc = {};
        // fill in the updated doc data from the displayed form
        doc_keys.forEach((key) => {
            updatedDoc[key] = document.getElementById(key).value;
        });
        return updatedDoc;
    };

    render() {
        const { classes } = this.props;
        const app = this.props.app;
        const doc = this.props.doc;
        const doc_keys = getDocKeys(doc);

        // create the button-click handlers
        const handleSaveClicked = (event) => {
            var updatedDoc = this.docValuesFromForm();
            app.saveDocAndDismissDocumentEditor(updatedDoc);
        };

        const handleCancelClicked = (event) => {
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