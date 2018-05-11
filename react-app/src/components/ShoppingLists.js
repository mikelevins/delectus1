// ---------------------------------------------------------------------
// React imports
// ---------------------------------------------------------------------
import React from "react";

// ---------------------------------------------------------------------
// Material-ui imports
// ---------------------------------------------------------------------

import { Card, CardTitle } from "material-ui/Card";
import MenuItem from "material-ui/MenuItem";
import IconMenu from "material-ui/IconMenu";
import IconButton from "material-ui/IconButton";
import MoreVertIcon from "material-ui/svg-icons/navigation/more-vert";
import Dialog from "material-ui/Dialog";
import FlatButton from "material-ui/FlatButton";
import TextField from "material-ui/TextField";

// ---------------------------------------------------------------------
// IBM ShoppingList sample imports
// ---------------------------------------------------------------------

import "./ShoppingLists.css";

// ---------------------------------------------------------------------
// UI components
// ---------------------------------------------------------------------

const iconButtonElement = (
  <IconButton touch={true} tooltip="more" tooltipPosition="bottom-left">
    <MoreVertIcon />
  </IconButton>
);

class ShoppingLists extends React.Component {
  // all state actions are for handling the renaming dialog
  state = {
    open: false,
    activeListId: "",
    oldName: "",
    newName: ""
  };

  handleOpen = (listid, listtitle) => {
    this.setState({ open: true, activeListId: listid, oldName: listtitle });
  };

  handleClose = () => {
    this.setState({ open: false });
  };

  handleSubmit = e => {
    this.props.renameListFunc(this.state.activeListId, this.state.newName);
    this.handleClose();
  };

  updateName = e => {
    this.setState({ newName: e.target.value });
  };

  // Show the UI. The most important thing happening here is that
  // the UI elements make use of the functions passed into the
  // component as props to do all the heavy lifting of manipulating
  // shopping lists, so this component is pure UI.

  render() {
    // rename dialog
    const actions = [
      <FlatButton label="Cancel" primary={true} onClick={this.handleClose} />,
      <FlatButton
        label="Submit"
        primary={true}
        keyboardFocused={true}
        onClick={this.handleSubmit}
      />
    ];
    // end rename dialog

    let listItems = this.props.shoppingLists.map(list => (
      <Card key={list._id} style={{ margin: "12px 0" }}>
        <CardTitle
          title={list.title}
          children={
            <IconMenu
              iconButtonElement={iconButtonElement}
              className="vertmenu-list"
            >
              <MenuItem
                primaryText="Open"
                onClick={() => this.props.openListFunc(list._id)}
              />

              <MenuItem
                primaryText="Rename"
                onClick={() => this.handleOpen(list._id, list.title)}
              />

              <MenuItem
                primaryText="Delete"
                onClick={() => this.props.deleteListFunc(list._id)}
              />
            </IconMenu>
          }
        />
      </Card>
    )); // end listItems

    return (
      <div>
        <div>{listItems}</div>
        <Dialog
          title="Rename Item"
          actions={actions}
          modal={false}
          open={this.state.open}
          onRequestClose={this.handleClose}
        >
          <TextField
            className="form-control"
            type="text"
            id="textfield-item-rename"
            defaultValue={this.state.oldName}
            onChange={this.updateName}
            fullWidth={true}
          />
        </Dialog>
      </div>
    );
  }
}

export default ShoppingLists;
