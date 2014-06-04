var child_process= require('child_process');

var R= child_process.spawn('R',['--no-save','--vanilla']);
var history = [];

R.stdout.on('data', function(data) { 
    if (history.length>0 && history[history.length-1].type == 'output') {
	history[history.length - 1].content += data;
    } else {
	history.push({type:'output', content:data});
    }
});

R.stderr.on('data', function(data) { 
    if (history.length>0 && history[history.length-1].type == 'error') {
	history[history.length - 1].content += data;
    } else {
	history.push({type:'error', content:data});
    }
});

function execute(cmd) {
    history.push({type:'input', content:cmd});
    R.stdin.write(cmd+'\n');
}

function historyToHtml() {
    var html = '';
    for (var i in history) {
	html += '<div class="'+history[i].type+'"><pre>'+history[i].content+'</pre></div>\n';
    }
    return html;
}

exports.execute = execute;
exports.getHistoryHtml = historyToHtml;
