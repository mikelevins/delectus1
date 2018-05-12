var cuid = require('cuid');

class DelectusCollection {
    constructor(props) {
        this._id = props._id ? props._id : "collection:" + cuid();
        this._rev = props._rev ? props._rev : "";
        this._deleted = props._deleted ? props._deleted : false;
        this.type = "DelectusCollection";
        this.version = 1;
        this.title = props.title ? props.title : "An Anonymous Collection";
        this.note = "";
        // ids of member lists and collections
        this.members = props.members ? props.members.map((i)=>i._id) : []; 
        this.createdAt = props.createdAt ? props.createdAt : Date.now();
        this.updatedAt = props.updatedAt ? props.updatedAt : Date.now();
    }
};

export default DelectusCollection;
