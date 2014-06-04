var http = require('http');
var fs= require('fs');
var url= require('url');
var R = require('./run_R.js');

var server= http.createServer(handleRequest);
server.listen(9009);

// mime types
var mimes= {
    html: 'text/html',
    css: 'text/css',
    jpg: 'image/jpeg',
    png: 'image/png',
    ico: 'image/ico',
    svg: 'image/svg+xml',
}

// Run web server
function handleRequest(req, res) {
    try {
	var urlParts = url.parse(req.url, true);
	var userid = urlParts.query.userid || Math.floor(Math.random()*1e6);

	if (urlParts.pathname=="/" || urlParts.pathname=="/welcome.html") {
	    R.execute('source("run.R")');
	    serveFile(res, 'www/welcome.html');
	} 
	else if (urlParts.pathname=="/question.html") {
	    R.execute('nextQuestion("'+userid+'")');
	    var param = {
		userid : userid
	    }
	    serveFile(res, 'www/question.html', param);
	} 
	else if (urlParts.pathname=="/result.html") {
	    serveFile(res, 'www/result.html');
	}
	else if (urlParts.pathname=="/console.html") {
	    serveFile(res, 'www/console.html', {history : R.getHistoryHtml()});
	}
	else {
	    serveFile(res, 'www' + urlParts.pathname);
	}
    } catch (err) {
	console.log("error occured : "+err);
    }
}

function serveFile(res, filename, content) {
    var mime= mimes[filename.replace(/[^.]*\./g,'')];
    if (!mime) {
	denyAccess(res, "Access denied");
	return;
    }
    fs.readFile(filename, function(err, data) {
	if (data) {
	    if (content) {
		data= data.toString();
		for (var key in content) {
		    data= data.replace(new RegExp('{{'+key+'}}','g'), content[key]);
		}
	    }
	    res.writeHead(200, { 
		'Content-Type': mime+'; charset=utf-8',
	    });
	    res.end(data)
	} else {
	    res.writeHead(404, { 'Content-Type': 'text/plain' });
	    res.end('File does not exist.');
	}
    });
}

function denyAccess(res, message) {
    console.log(message);
    res.writeHead(406, { 'Content-Type': 'text/plain' });
    res.end(message);
}

function timeStamp() {
    var pad= function(x) {
	return x<10 ? '0'+x : ''+x;
    };
    var now= new Date();
    return now.getFullYear()+'-'+
	pad(now.getMonth()+1)+'-'+
	pad(now.getDate())+'T'+
	pad(now.getHours())+':'+
	pad(now.getMinutes())+':'+
	pad(now.getSeconds());
};
