// utilities
import cuid from 'cuid';


const mockTitle = "Fruits";
const mockColumns = ["Name", "Color", "Distinction", "Count"];
const mockFields = [
  ["Apples", "red", "tangy", 3],
  ["Bananas", "yellow", "mushy", 4],
  ["Cherries", "red", "sweet", 35],
  ["Dates", "brown", "fibery", 22],
  ["Elephants", "gray", "awesome", 0],
  ["Waffles", "tan", "buttery", 2]
];

function makeRow(fields) {
  var result = {
    '_id': 'row' + cuid(),
    'type': 'row',
    'list': null,
    'fields': fields.slice(),
    'createdAt': new Date().toISOString(),
    'updatedAt': ''
  }
  return result;
}

const mockRows = mockFields.map(makeRow);

function makeList(title, columns, rows) {
  var result = {
    '_id': 'list' + cuid(),
    'type': 'list',
    'title': title,
    'columns': columns.slice(),
    'rows': rows.slice(),
  }
  return result;
}

const mockList = makeList(mockTitle,mockColumns,mockRows);

export default mockList;
