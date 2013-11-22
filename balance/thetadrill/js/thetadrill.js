function buildOptions(myJSONObject) {
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
options['chart'] = {'height': 370,'width':380}
options['title']= {'text': 'Generic Title'}
options['xAxis']= {'categories': true}
options['legend']= {'enabled': false}
options['plotOptions']= {'series': {'dataLabels': { 'enabled': true },'shadow': false },'pie': {'size': '70%' } }
  
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
	for(subcat = 0; subcat<index.length;subcat++) {
		var cat = index[subcat]
		var catValue = myJSONObject[cat][idx];
		var catValueDd =undefined
		
		for (subidx=0; subidx<localDrilldown['data'].length;subidx++) {
		    var foo = localDrilldown['data'][subidx]
			if(localDrilldown['data'][subidx]['name'] !== catValue) continue;
			catValueDd = localDrilldown['data'][subidx]
		}
		
		if(!catValueDd) {
			localDrilldown['data'].push({'name': catValue, 'y': value,'drilldown': catValue})
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
	if(d['data'].length>1) {
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


