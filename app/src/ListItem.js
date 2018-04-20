import React from 'react';

/**
 * This example uses an [IconButton](/#/components/icon-button) on the left, has a clickable `title`
 * through the `onClick` property, and a [FlatButton](/#/components/flat-button) on the right.
 */
const ListItem = (props) => (
    <li>{props.itemValue.map(function (field){
        return <span> {field} </span>
    })}
    </li>
);

export default ListItem;


