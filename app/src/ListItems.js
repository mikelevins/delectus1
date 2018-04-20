import React from 'react';
import ListItem from './ListItem';

/**
 * This example uses an [IconButton](/#/components/icon-button) on the left, has a clickable `title`
 * through the `onClick` property, and a [FlatButton](/#/components/flat-button) on the right.
 */
const ListItems = (props) => (
    <div className="ListItems">
        <ul>
            {props.items.map(function (item) {
                return <ListItem itemValue={item}/>;
            })}
        </ul>
    </div>
);

export default ListItems;