// delectus main app

var ENTER_KEY = 13;
var navDom = document.getElementById('nav');
var appDom = document.getElementById('app');

var newsDB = new PouchDB('http://localhost:5894/delectus_news');
