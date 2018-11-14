import React, { Component } from 'react';
import './App.css';
import CssBaseline from '@material-ui/core/CssBaseline';
import { withStyles } from '@material-ui/core/styles';

const styles = theme => ({
    button: { marginLeft: '1em' },
    buttonBar: { display: 'inline' },
    container: { display: 'block' },
    dense: { marginTop: 19 },
    editBox: { width: '90%' },
    menu: { width: 200 },
    p: {
        marginLeft: '2rem',
    },
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

class ObjectPresentation extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        const { classes } = this.props;
        const app = this.props.app;

        return (
            <React.Fragment>
                <CssBaseline />
                <p className={classes.p}>{JSON.stringify(app.state.couchInfo)}</p>
            </React.Fragment>
        );
    }
}

// exports
// ---------------------------------------------------------

export default withStyles(styles)(ObjectPresentation);