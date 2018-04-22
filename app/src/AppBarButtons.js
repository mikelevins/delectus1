// react base
import React from 'react';

// material-ui stuff

import Settings from 'material-ui/svg-icons/action/settings';
import FlatButton from 'material-ui/FlatButton';
import AddColumn from 'material-ui/svg-icons/action/view-column';

const appButtonStyle = {
    color: "#fff",
    flex: "none",
};

const appButtonLabelStyle = {
    fontSize: "1.5rem",
    fontWeight: "bold",
};

const AppBarButtons = (props) => (
    <div>
        <FlatButton
            style={appButtonStyle}
            labelStyle={appButtonLabelStyle}
            label="+"
            labelPosition="before"
            icon={<AddColumn />} />
        <FlatButton
            style={appButtonStyle}
            labelStyle={appButtonLabelStyle}
            icon={<Settings />} />
    </div>);

export default AppBarButtons;
