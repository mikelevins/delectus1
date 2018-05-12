var cuid = require('cuid');

class DelectusList {
    constructor(props) {
        this._id = props._id ? props._id : "list:" + cuid();
        this._rev = props._rev ? props._rev : "";
        this._deleted = props._deleted ? props._deleted : false;
        this.type = "DelectusList";
        this.version = 1;
        this.title = props.title ? props.title : "An Anonymous List";
        this.note = "";
        this.checked = props.checked ? props.checked : false;
        this.createdAt = Date.now();
        this.updatedAt = Date.now();
    }
};

export default DelectusList;

