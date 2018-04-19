import React from 'react';
import IconButton from 'material-ui/IconButton';
import AddItem from 'material-ui/svg-icons/action/list';

/**
 * This example uses an [IconButton](/#/components/icon-button) on the left, has a clickable `title`
 * through the `onClick` property, and a [FlatButton](/#/components/flat-button) on the right.
 */
const AddItemButton = () => (
    <IconButton><AddItem /></IconButton>
  );

export default AddItemButton;