import React, { Component } from 'react';
import './App.css';
import PresenterObjectTable from './PresenterObjectTable.js';

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
        const presenter = this;

        return (
            <React.Fragment>
                <PresenterObjectTable object={presenter.props.object} />
            </React.Fragment>
        );
    }
}

// exports
// ---------------------------------------------------------

export default Presenter;