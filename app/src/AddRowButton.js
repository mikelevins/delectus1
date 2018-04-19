import React from 'react';
import IconButton from 'material-ui/IconButton';
import AddItem from 'material-ui/svg-icons/action/list';
import './App.css';

/**
 * This example uses an [IconButton](/#/components/icon-button) on the left, has a clickable `title`
 * through the `onClick` property, and a [FlatButton](/#/components/flat-button) on the right.
 */
const AddRowButton = () => (
    <div className="AddRowButton">+<IconButton><AddItem /></IconButton></div>
  );

export default AddRowButton;