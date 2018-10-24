var express = require('express');
var router = express.Router();

// simple starter code from an example React-Express app
// TODO: replace with real Delectus code

/* GET users listing. */
router.get('/', function(req, res, next) {
	// Comment out this line:
  //res.send('respond with a resource');

  // And insert something like this instead:
  res.json([{
  	id: 1,
  	username: "delectus"
  }, {
  	id: 2,
  	username: "admin"
  }]);
});

module.exports = router;
