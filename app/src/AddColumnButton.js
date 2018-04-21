import React from 'react';
import IconButton from 'material-ui/IconButton';
import AddColumn from 'material-ui/svg-icons/action/view-column';
import './App.css';

/**
 * This example uses an [IconButton](/#/components/icon-button) on the left, has a clickable `title`
 * through the `onClick` property, and a [FlatButton](/#/components/flat-button) on the right.
 */
const AddColumnButton = () => (
    <div>+<IconButton><AddColumn /></IconButton></div>
  );

export default AddColumnButton;