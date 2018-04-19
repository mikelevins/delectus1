import React from 'react';

/**
 * This example uses an [IconButton](/#/components/icon-button) on the left, has a clickable `title`
 * through the `onClick` property, and a [FlatButton](/#/components/flat-button) on the right.
 */
const ListItems = (props) => (
    <div className="ListItems">
        <ul>
            {props.items.map(function (listValue) {
                return <li>{listValue}</li>;
            })}
        </ul>
    </div>
);

export default ListItems;