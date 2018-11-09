import React from 'react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import AppBar from '@material-ui/core/AppBar';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';
import IconButton from '@material-ui/core/IconButton';
import MenuIcon from '@material-ui/icons/Menu';

const styles = {
  root: {
    flexGrow: 1,
  },
  grow: {
    flexGrow: 1,
  },
  menuButton: {
    marginLeft: -12,
    marginRight: 20,
  },
};

function OppsDailyAppBar(props) {
  const { classes } = props;
  const statusText = props.statusText;

  return (
    <div className={classes.root}>
      <AppBar position="static">
        <Toolbar>
          <IconButton className={classes.menuButton} color="inherit" aria-label="Menu">
            <MenuIcon />
          </IconButton>
          <Typography variant="h4" color="inherit" className={classes.grow}>
          Opps Daily
          </Typography>
          <Typography variant="subheading" color="inherit">
          {statusText}
          </Typography>
        </Toolbar>
      </AppBar>
    </div>
  );
}

OppsDailyAppBar.propTypes = {
  classes: PropTypes.object.isRequired,
};

export default withStyles(styles)(OppsDailyAppBar);