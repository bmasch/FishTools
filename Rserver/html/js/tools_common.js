	function replaceAll(find, replace, str) {
		return str.replace(new RegExp(escapeRegExp(find), 'g'), replace);
	}
	  
	function escapeRegExp(string) {
		return string.replace(/([.*+?^=!:${}()|\[\]\/\\])/g, "\\$1");
	}
	  
	  setTable = function(){
	    tabledata = new google.visualization.DataTable();
		for(i=0;i<columnnames.length;i++){
		  tabledata.addColumn(columntypes[i], columnnames[i]);
		}
		//see: https://developers.google.com/chart/interactive/docs/reference#numberformatter
		formatter = new google.visualization.NumberFormat(
			{prefix: '$', 
			negativeColor: 'red', 
			negativeParens: true});
		
		table = new google.visualization.Table(document.getElementById('resultsdiv'));

		drawTable();
	  }
	  
	  drawTable = function(){
	    //table.draw(data, {cssClassNames: tableCSS, allowHtml: true, showRowNumber: false});
		table.draw(tabledata, {alternatingRowStyle: false, height: tableheight, width: tablewidth, allowHtml: true, showRowNumber: false, sort: 'disable'});
		//set tooltips in result table
		$(".google-visualization-table-tr-head").children().each(function() {
			$(this).width(cellwidths[$(this).index()]);
			$(this).on("mousemove", function(event) {
				//$(this).css("border-style","solid");
				//$(this).css("border-width","2px");
				//alert($(this).position().left);
				var offset=0;
				var ttwidth=300;
				var tdleft=$(this).position().left;
				var tdright=tdleft + $(this).width();
				var max=$(this).parent().width();
				if(tdright+ttwidth > max)offset=tdright+ttwidth-max;
				$("div.tooltip").text(tooltips[$(this).index()]);
				$("div.tooltip").css({
					top: event.pageY + 12 + "px",
					left: (event.pageX - offset) + "px",
					//left: event.pageX + 12 + "px",
					width: ttwidth + "px"
					}).show();
				}).on("mouseout", function() {
					$("div.tooltip").hide();
					//$(this).css("border-width","0px");				
				});
		});;
		
		//format columns for input/output
		$('.google-visualization-table-tr-even').children().each(function() {
		  if($(this).index() < nparams) {
			$(this).css("background-color", "#dddddd");
			
		  }
		  $(this).css("width", cellwidths[$(this).index()]);
		});
	  }
	  
	  getClass = function(obj) {
  		if (typeof obj === "undefined")
    	return "undefined";
  		if (obj === null)return "null";
  		return Object.prototype.toString.call(obj).match(/^\[object\s(.*)\]$/)[1];
}
	  
	  setValues = function(data){
	    var thisObj;
		var name;
		var thisrow = tabledata.getNumberOfRows()-1;
		for(var i = 0;i<columnnames.length;i++){
			name = columnnames[i];
			thisObj = data[name];
			if(thisObj==null){
			  tabledata.setValue(thisrow, i, null);
			  tabledata.setFormattedValue(thisrow, i, "NA")
			}
			if(getClass(thisObj)=="Array"){
			  var varstr="";
			  var tmp;
			  for(var j=0; j<thisObj.length-1;j++){
			    tmp=thisObj[j];
				if(tmp==null)tmp="NA";
				varstr=varstr + tmp + "<br/>";
			  }
			  varstr = varstr + thisObj[thisObj.length-1];
			  //alert(varstr);
			  tabledata.setValue(thisrow, i, varstr);
			}
			if(getClass(thisObj)=="Number"){
			  if(columntypes[i]=="string"){
			    var varstr="";
				varstr = varstr + thisObj + "<br/>";
				tabledata.setValue(thisrow, i, varstr);
			  }
			  else tabledata.setValue(thisrow, i, thisObj);			  
			}
			if(getClass(thisObj)=="String")tabledata.setValue(thisrow, i, thisObj);
		}
	  }
	  
	  
	  //set tooltips in param pane
	  setToolTips = function(){
	     $("tr.input_row").each(function() {
			$(this).on("mousemove", function(event) {
				var ttwidth=300;
				$("div.tooltip").text(tooltips[$(this).index()]);
				$("div.tooltip").css({
					top: event.pageY + 8 + "px",
					left: event.pageX + 8 + "px",
					width: ttwidth + "px"
					}).show();
				}).on("mouseout", function() {
				$("div.tooltip").hide();
				});
		  });;
		setButtonToolTip("clearbutton","Clear the results table");
		setButtonToolTip("calcbutton","Calculate the results using the above input parameters, adding one row to the results table");
		setButtonToolTip("exportbutton","Export the data in the results table to a comma-separated value (CSV) file");
	  }
	  
	  function setButtonToolTip(name,txt){
		$("#"+name).on("mousemove", function(event) {
				var ttwidth=txt.length*6.5;
				$("div.tooltip").text(txt);
				$("div.tooltip").css({
					top: event.pageY -5 + "px",
					left: event.pageX + 20 + "px",
					width: ttwidth + "px"
					}).show();
				}).on("mouseout", function() {
				$("div.tooltip").hide();
				});
	  }
	    
	  
	  checkValue = function(values, isInt, mincount, maxcount, pmin, mineq, pmax, maxeq, message){
		try
		{   var ok = true;
			//var _values = trim(values).replace(/\r\n/g, "\n").split("\n");
			var _values=$.trim(values).replace(/\r\n/g, "\n").split("\n");
			if(_values.length < mincount)ok = false;
			if(_values.length > maxcount)ok = false;
			for(var i=0;i<_values.length;i++){
			    //alert(_values[i]);;
				if(isInt){
				 if (parseFloat(_values[i]) != parseInt(_values[i]))ok = false;
				}
				if(mineq){
					if(_values[i] < pmin)ok = false;
				}
				else if(_values[i] <= pmin)ok = false;
				if(maxeq){
					if(_values[i] > pmax)ok = false;
				}
				else if(_values[i] >= pmax)ok = false;			
			}
			if(!ok){
				$("#spinner").hide();
				$dialog.html(message);
				$dialog.dialog('open');
				return false;			
			}
			return true;
		}
		catch(err){
			$("#spinner").hide();
			$dialog.html(message);
			$dialog.dialog('open');
			return false;
		}
	  }
	  
	  _toJSON = function(arr){
		var st = "[";
		for(var i=0;i<arr.length;i++){
			st=st+arr[i];
			if(i==arr.length-1)st=st+"]";
			else st=st+",";
		}
		return st;
	  }
	  
	  

  