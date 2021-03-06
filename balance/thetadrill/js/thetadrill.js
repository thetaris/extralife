
function onRedraw(myChart,inp,id) {

  var levels = myChart.drilldownLevels;
  myChart.drilldownLevels[id]=id;
 
  var inpValue = [];
  for(idx = 0; idx<levels.length;idx++) {
    var name = levels[idx].pointOptions.name;
    inpValue.push(name);

    }
  inp.value= JSON.stringify(inpValue);
  var e = new KeyboardEvent("keyup", {bubbles : true, cancelable : true, key : "Q", char : "Q", shiftKey : true});
  inp.dispatchEvent(e);
}

function buildOptions(myJSONObject) {
if(myJSONObject === 'NoData') {
  return myJSONObject;
}

var index = [];
// to array
for (var x in myJSONObject) {
   if(myJSONObject[x] instanceof Array) continue;
   
   var tmp = myJSONObject[x];
   myJSONObject[x]=[];
   myJSONObject[x].push(tmp);
}

// build the index
for (var x in myJSONObject) {
   if(x !=='wert') index.push(x);
}

var mainCat = index[0]
index.splice(0, 1);

var options=[]
options['chart'] = {'height': 370,'width':700}
options['title']= {'text': 'Generic Title'}
options['xAxis']= {'categories': true}
options['legend']= {'enabled': false}
options['plotOptions']= {'series': {'dataLabels': { 'enabled': true },'shadow': false },'pie': {'size': '70%' } }
options['tooltip'] = {formatter: function() {
  			var s = '<b>'+ this.point.name +'</b>';				
					s += '<br/>'+ this.y.toMoney(2,',','.') + ' \u20AC';
				
            
				return s;
			}
  		
}
var series=[]
options['series'] =series
var seriesData = []
series.push({'name': 'Overview','colorByPoint':true,'data':seriesData})

var drilldown=[]
var ddmap = []

for(idx = 0; idx<myJSONObject['wert'].length;idx++) {
	var rowMainCat = myJSONObject[mainCat][idx];
	var value = myJSONObject['wert'][idx];
	var catValueDd =undefined
	for (subidx=0; subidx<seriesData.length;subidx++) {
			var foo = seriesData[subidx]
			if(foo['name'] !== rowMainCat) continue;
			catValueDd = foo
	}
	if(!catValueDd) {
		seriesData.push({'name': rowMainCat, 'y': value,'drilldown': rowMainCat})
		ddmap[rowMainCat] = {'id': rowMainCat, 'name': rowMainCat,'data': []}
		drilldown.push(ddmap[rowMainCat])
	} else {
		catValueDd['y'] = catValueDd['y'] +value
	}
	
	var localDrilldown = ddmap[rowMainCat]
	for(var subcat = 0; subcat<index.length;subcat++) {
		var cat = index[subcat]
		var catValue = myJSONObject[cat][idx];
		var catValueDd =undefined
		
		for (var subidx=0; subidx<localDrilldown['data'].length;subidx++) {
			if(localDrilldown['data'][subidx]['name'] !== catValue) continue;
			catValueDd = localDrilldown['data'][subidx]
		}
		
		if(!catValueDd) {
      if(subcat==index.length-1)  localDrilldown['data'].push({'name': catValue, 'y': value,'drilldown': ''})
      else                        localDrilldown['data'].push({'name': catValue, 'y': value,'drilldown': catValue})
		} else {
			catValueDd['y'] = catValueDd['y'] +value
		}
		if(!ddmap[catValue]) {
		ddmap[catValue] = {'id': catValue, 'name': catValue,'data': []}
		drilldown.push(ddmap[catValue])
		}
		localDrilldown = ddmap[catValue]

	}

	}

var ddmap = []	
options['drilldown']={'series':[]}
for (idx =0;idx<drilldown.length;idx++) {
	var d = drilldown[idx]
	if(d['data'].length>0) {
		options['drilldown']['series'].push(d)
		ddmap[d['name']] = d
	}
}

drilldown = options['drilldown']['series']
for (idx =0;idx<drilldown.length;idx++) {
	var d = drilldown[idx]
	for (idx2 =0;idx2<d['data'].length;idx2++) {
		var d2 = d['data'][idx2]
		if(!ddmap[d2['name']]) {
			d['data'][idx2] = [d2['name'] ,d2['y'] ]
		}
	}
}
for (idx =0;idx<seriesData.length;idx++) {
	var d = seriesData[idx]
	if(!ddmap[d['name']]) {
		seriesData[idx] = { 'name': d['name'], 'y': d['y']}
	}
}

return options;
}

Number.prototype.toMoney = function(decimals, decimal_sep, thousands_sep)
{ 
   var n = this,
   c = isNaN(decimals) ? 2 : Math.abs(decimals), //if decimal is zero we must take it, it means user does not want to show any decimal
   d = decimal_sep || '.', //if no decimal separator is passed we use the dot as default decimal separator (we MUST use a decimal separator)

   /*
   according to [http://stackoverflow.com/questions/411352/how-best-to-determine-if-an-argument-is-not-sent-to-the-javascript-function]
   the fastest way to check for not defined parameter is to use typeof value === 'undefined' 
   rather than doing value === undefined.
   */   
   t = (typeof thousands_sep === 'undefined') ? ',' : thousands_sep, //if you don't want to use a thousands separator you can pass empty string as thousands_sep value

   sign = (n < 0) ? '-' : '',

   //extracting the absolute value of the integer part of the number and converting to string
   i = parseInt(n = Math.abs(n).toFixed(c)) + '', 

   j = ((j = i.length) > 3) ? j % 3 : 0; 
   return sign + (j ? i.substr(0, j) + t : '') + i.substr(j).replace(/(\d{3})(?=\d)/g, "$1" + t) + (c ? d + Math.abs(n - i).toFixed(c).slice(2) : ''); 
}

function makeTitle(tab,options) {
  if(options instanceof Array) {
    var sum=0;
    var opts0 = options.series[0]
    for(idx = 0; idx<opts0.data.length;idx++) {
      var data = opts0.data[idx]
      sum += data.y
      }

    return (tab + ' (' + sum.toMoney(2,',','.') + ' \u20AC)') ;
    }
  
  return tab;
}

function jumpIncome() {
 jumpToTab(1)
}
function jumpExpense() {
 jumpToTab(2)
}

function jumpAsset() {
 jumpToTab(3)
}

function jumpCredit() {
 jumpToTab(4)
}
  
function jumpToTab(idx) {
 var obj= $('div.tabbable').find('a')
 var evt = document.createEvent("MouseEvents"); 
  evt.initMouseEvent("click", true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null); 
  obj[idx].dispatchEvent(evt);
  }
