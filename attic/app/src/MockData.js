// utilities
import cuid from "cuid";

const mockTitle = "Fruits";
const mockNote = "A list of delicious fruits we have on hand.";
const mockColumns = ["Name", "Color", "Distinction", "Count", "Delicious?"];
const mockFields = [
  ["Apples", "red", "tangy", 3, "yes"],
  ["Bananas", "yellow", "mushy", 4, "yes"],
  ["Cherries", "red", "sweet", 35, "yes"],
  ["Dates", "brown", "fibery", 22, "yes"],
  ["Elephants", "gray", "awesome", 0, "no"],
  ["Figs", "brown", "sweet", 0, "yes"],
  ["Waffles", "tan", "buttery", 2, "yes"]
];

function makeRow(fields) {
  var result = {
    _id: "row" + cuid(),
    type: "row",
    list: null,
    fields: fields.slice(),
    createdAt: new Date().toISOString(),
    updatedAt: ""
  };
  return result;
}

const mockRows = mockFields.map(makeRow);

function makeList(title, note, columns, rows) {
  var result = {
    _id: "list" + cuid(),
    type: "list",
    title: title,
    note: note,
    columns: columns.slice(),
    rows: rows.slice()
  };
  return result;
}

const mockList = makeList(mockTitle, mockNote, mockColumns, mockRows);

export default mockList;
