import React from 'react';

/**
 * This example uses an [IconButton](/#/components/icon-button) on the left, has a clickable `title`
 * through the `onClick` property, and a [FlatButton](/#/components/flat-button) on the right.
 */
const ListItem = (props) => (
    <tr className="ListItemRow">{props.itemValue.map(function (field){
        return <td className="ListItemCell"> {field} </td>
    })}
    </tr>
);

export default ListItem;

