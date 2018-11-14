import React, { Component } from 'react';
import './App.css';
import PresenterObjectTable from './PresenterObjectTable.js';

import CssBaseline from '@material-ui/core/CssBaseline';
import { withStyles } from '@material-ui/core/styles';


const styles = theme => ({
    presenter: {
        marginLeft: '2rem',
    },
});

class Presenter extends Component {

    // constructor
    // ---------------------------------------------------------

    constructor(props) {
        super(props);

        this.state = {
        };
    }

    // main render
    // ---------------------------------------------------------

    render() {
        const { classes } = this.props;
        const presenter = this;

        return (
            <React.Fragment>
                <CssBaseline />
                <p className={styles.browserLabel}>{presenter.props.label}</p>
                <PresenterObjectTable object={presenter.props.object} />
            </React.Fragment>
        );
    }
}

// exports
// ---------------------------------------------------------

export default withStyles(styles)(Presenter);