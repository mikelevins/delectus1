import React from 'react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import Card from '@material-ui/core/Card';
import CardActions from '@material-ui/core/CardActions';
import CardContent from '@material-ui/core/CardContent';
import Button from '@material-ui/core/Button';
import Typography from '@material-ui/core/Typography';

const styles = {
    card: {
        margin: 8,
        minWidth: 275,
    },
    bullet: {
        display: 'inline-block',
        margin: '0 2px',
        transform: 'scale(0.8)',
    },
    industry: {
        fontSize: 16,
        fontWeight: 'bold',
        marginBottom: 6,
    },
    pos: {
        marginBottom: 6,
    },
    title: {
        fontSize: 16,
        fontWeight: 'bold',
        marginBottom: 6,
    },
    industry: {
        fontSize: 16,
        fontWeight: 'bold',
        marginTop: 8,
        marginBottom: 6
        ,
    },
    willing_to_pay: {
        fontSize: 16,
        marginBottom: 6,
    },
};

function DocEntry(props) {
    const app = props.app;
    const entry = props.entry;
    const entry_id = entry.id;
    const doc = entry.doc;
    const rev = doc._rev;
    const summary = doc.summary;
    const industry = doc.industry;
    const date_received = doc.date_received;
    const willing_to_pay = doc.willing_to_pay;
    const { classes } = props;

    return (
        <Card className={classes.card}>
            <CardContent>
                <Typography className={classes.pos} color="textSecondary">
                    {date_received}
                </Typography>
                <Typography className={classes.title} color="textSecondary" gutterBottom>
                    Summary:
                </Typography>
                <Typography variant="h6" component="h2">
                    {summary}
                </Typography>
                <Typography className={classes.industry} color="textSecondary">
                    industry: {industry}
                </Typography>
                <Typography className={classes.willing_to_pay} color="textSecondary">
                    willing to pay: {willing_to_pay}
                </Typography>
                <Typography className={classes.pos} color="textSecondary">
                    id: {entry_id}
                </Typography>
                <Typography className={classes.pos} color="textSecondary">
                    rev: {rev}
                </Typography>
            </CardContent>
            <CardActions>
                <Button
                    variant="contained"
                    size="small"
                    color="primary"
                    onClick={() => app.editSelectedDocument(doc)}>
                    Edit
                </Button>
            </CardActions>
        </Card>
    );
}

DocEntry.propTypes = {
    classes: PropTypes.object.isRequired,
};

export default withStyles(styles)(DocEntry);