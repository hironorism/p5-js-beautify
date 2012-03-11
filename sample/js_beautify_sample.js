var js_beautify = require('./js_beautify.js').js_beautify;
var fs = require('fs');

fs.readFile('./jquery-1.5.min.js', function(error, data) {
    console.log( js_beautify(data.toString()) );
});

