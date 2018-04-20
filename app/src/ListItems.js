import React from 'react';
import ListItem from './ListItem';
import './App.css';

/**
 * This example uses an [IconButton](/#/components/icon-button) on the left, has a clickable `title`
 * through the `onClick` property, and a [FlatButton](/#/components/flat-button) on the right.
 */
const ListItems = (props) => (
    <div>
        <table className="ListTable">
            <tr className="ListHeader">{props.columns.map(function (field) {
                return <th className="ListHeaderCell"> {field} </th>
            })}
            </tr>
            {props.items.map(function (item) {
                return <ListItem itemValue={item} />;
            })}
        </table>
    </div>
);

export default ListItems;