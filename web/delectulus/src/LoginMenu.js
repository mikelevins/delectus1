import { AppState } from "./AppState";
import List from "@material-ui/core/List";
import ListItem from "@material-ui/core/ListItem";
import ListItemText from "@material-ui/core/ListItemText";
import MenuItem from "@material-ui/core/MenuItem";
import Menu from "@material-ui/core/Menu";
import React from "react";
import Typography from "@material-ui/core/Typography";
import { withStyles } from "@material-ui/core/styles";
import App from "./App";

const styles = (theme) => ({
  root: {
    width: "100%",
    maxWidth: 360,
  },
  grow: {
    flexGrow: 1,
  },
  menuButton: {
    color: "#ffffff",
    backgroundColor: theme.palette.background,
    fontSize: "14pt",
  },
});

const options = AppState.Users;

class LoginMenu extends React.Component {
  state = {
    anchorEl: null,
    selectedIndex: -1,
  };

  handleClickListItem = (event) => {
    this.setState({ anchorEl: event.currentTarget });
  };

  handleMenuItemClick = (event, index) => {
    this.setState({ selectedIndex: index, anchorEl: null });
    let username = AppState.Users[index];
    AppState.updateUserSelection(username);
  };

  handleClose = () => {
    this.setState({ anchorEl: null });
  };

  render() {
    const { classes } = this.props;
    const { anchorEl } = this.state;

    return (
      <div className={classes.root}>
        <List component="nav">
          <ListItem
            button
            aria-haspopup="true"
            aria-controls="lock-menu"
            aria-label="Select User"
            onClick={this.handleClickListItem}>
            <ListItemText
              classes={{ primary: classes.menuButton }}
              primary={
                this.state.selectedIndex > -1
                  ? "User: "+options[this.state.selectedIndex]
                  : "Pick a User"
              }
            />
          </ListItem>
        </List>
        <Menu
          id="lock-menu"
          anchorEl={anchorEl}
          open={Boolean(anchorEl)}
          onClose={this.handleClose}>
          {options.map((option, index) => (
            <MenuItem
              key={option}
              selected={index === this.state.selectedIndex}
              onClick={(event) => this.handleMenuItemClick(event, index)}>
              {option}
            </MenuItem>
          ))}
        </Menu>
      </div>
    );
  }
}

export default withStyles(styles)(LoginMenu);
