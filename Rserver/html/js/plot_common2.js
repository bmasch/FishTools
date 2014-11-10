//plotting functions

	  //opens and animates the chart gallery
	$(function() {
    $('#activator').click(function(){
        $('#overlay').fadeIn('fast',function(){
			$('#imgChart').show();
            $('#box').animate({'top':'160px'},500);
			$(function() {
				var wtf    = $('#imgChart');
				var height = wtf[0].scrollHeight;
				wtf.scrollTop(height);
			});
        });
    });
    $('#boxclose').click(function(){
        $('#box').animate({'top':'-200px'},500,function(){
            $('#overlay').fadeOut('fast');
			$('#imgChart').hide();
        });
    });
 
	});

plotData = function(){
		var selectx = document.getElementById("xaxis");
		var selecty = document.getElementById("yaxis");
		var legendp = document.getElementById("legend_pos"); 
		_xaxis = selectx.options[selectx.selectedIndex].value;
		_yaxis = selecty.options[selecty.selectedIndex].value;
		_legend = legendp.options[legendp.selectedIndex].value;
		var _xaxis_type = getInputParameterType(_xaxis);
		
		var show_legend=true;
		if(_legend=="none"){
			show_legend=false;
			_legend='ne';
		}
		$('#plot').empty();
		if(results[0].length==0)return;
		//alert(JSON.stringify(results));
		var dat = [];
		var axes_options;
		var series_defaults;
		if(_xaxis_type=="string"){
			var tic;
			series_defaults={
				markerOptions: {
					style: "dash",
					lineWidth: 4,
					size: 25.0},
				showLine: false
			};
			for(var j=0;j<results.length;j++){
				if(j==0)tic=[];
				dat.push([]);
				var value;
				for(var i=0;i<results[j].length;i++){
					dat[j].push(results[j][i][_yaxis]);
					value = results[j][i][_xaxis];
					value=value.toString();
					value=value.replace(/,/g,"\n");
					if(j==0)tic.push(value);
				}
			}
			axes_options={
				xaxis: {
					renderer: $.jqplot.CategoryAxisRenderer,
						ticks: tic,
						label: _xaxis
					},
				yaxis: {
					label: _yaxis,
				},      		
			} 
		}
		else{
		 	for(var j=0;j<results.length;j++){
				dat.push([]);
				for(var i=0;i<results[j].length;i++){
					dat[j].push([results[j][i][_xaxis],results[j][i][_yaxis]]);
				}
			}
			series_defaults={ 
				showLine: _lines
			};
			axes_options={
				xaxis: {
					label: _xaxis
				},
				yaxis: {
					label: _yaxis
				},      		
			}    		
		} 
	
		//alert(JSON.stringify(dat));
		//return;
	
	plot2 = $.jqplot ('plot', dat, {
      title: plot_title,
	  legend:{
            show:show_legend, 
			renderer: $.jqplot.EnhancedLegendRenderer,  
			labels: legend_labels,
            rendererOptions: {
                numberRows: 0
            }, 
            location: _legend
      },
      seriesDefaults: series_defaults,
	  axesDefaults: {
        labelRenderer: $.jqplot.CanvasAxisLabelRenderer
      },	  
      axes: axes_options  
    });	
   }	

	function  getInputParameterType(name){
		var nid=0;
		for(i=1;i<nparams;i++){
			if(columnnames[i]==name)nid=i;
		}
		return columntypes[nid];
	}

   
   showSeries = function(){
		var table = document.getElementById("series");
		var row;
		var cell1;
		var cell2;
		var inpt;
		var llabel;
		for(var i=0;i<legend_labels.length;i++){
			row = table.insertRow(i);
			llabel = "legend_row" + i;
			row.setAttribute("id",llabel);
			cell1 = document.createElement('th');
			cell2 = document.createElement('td');
			cell1.setAttribute("align","right");
			cell1.setAttribute("width","25%");
			cell1.innerHTML="label " + (i+1);
			cell2.setAttribute("align","left");
			inpt = document.createElement('input');
			inpt.setAttribute("type","text");
			inpt.setAttribute("size","25");
			inpt.setAttribute("onchange","updateLabel(this," + i + ")");
			inpt.value=legend_labels[i];
			cell2.appendChild(inpt);
			row.appendChild(cell1);
			row.appendChild(cell2);
			setButtonToolTip(llabel,"Label for data series " + (i+1) + " for current chart");
		}
   }
   
   addTitleField = function(){
	var table = document.getElementById("settings_table");
	var row;
	var cell1;
	var cell2;
	var inpt;
	row = table.insertRow(1);
	row.setAttribute("id","chart_title");
	cell1 = document.createElement('th');
	cell2 = document.createElement('td');
	cell1.setAttribute("align","right");
	cell1.setAttribute("width","25%");
	cell1.innerHTML="title ";
	cell2.setAttribute("align","left");
	inpt = document.createElement('input');
	inpt.setAttribute("type","text");
	inpt.setAttribute("size","25");
	inpt.setAttribute("onchange","updateTitle(this)");
	cell2.appendChild(inpt);
	row.appendChild(cell1);
	row.appendChild(cell2);
	setButtonToolTip("chart_title","Title for current chart");
   }
   
   removeSeries = function(){
     $('#series').empty();
   }
   
   updateLabel=function(obj,i){
	 legend_labels[i]=obj.value;
	 plotData();
   }
   
   updateTitle=function(obj){
	 plot_title=obj.value;
	 plotData();
   }   
   
   ifLines = function(obj){
	 if(obj.checked)_lines=true;
	 else _lines=false;
	 plotData();
   }
   
   createImagePlot = function(){
	 //after creating your plot do
	 var imgData = $('#plot').jqplotToImageStr({}); // given the div id of your plot, get the img data
	 var imgElem = $('<img/>').attr('src',imgData); // create an img and add the data to it
	 $('#imgChart').append(imgElem);// append the img to the DOM
   }

$(document).ready(function(){
	var opt;
	var selectx = document.getElementById("xaxis");
	var selecty = document.getElementById("yaxis");
	for(var i=0;i<nparams;i++){
		opt = document.createElement("option");
		opt.value = columnnames[i];
		opt.innerHTML = columnnames[i];
		selectx.appendChild(opt);
	}
	for(var i=nparams;i<columnnames.length;i++){
		opt = document.createElement("option");
		opt.value = columnnames[i];
		opt.innerHTML = columnnames[i];
		selecty.appendChild(opt);
	}
	//set defaults
	setButtonToolTip("newsetbutton","Start a new data series");
	setButtonToolTip("addchartbutton","Add current chart to Gallery");
	setButtonToolTip("activator","View Chart Gallery");
	setButtonToolTip("clearchartsbutton","Clear all charts from Gallery");
	setButtonToolTip("x_axis","Input parameter used for x-axis in current chart");
	setButtonToolTip("y_axis","Input parameter used for y-axis in current chart");
	setButtonToolTip("leg_pos","Legend position for current chart");
	setButtonToolTip("plot_lines","Checked if data points are to be connected with lines in current chart");
	selectx.options[_defaultx].defaultSelected = true;
	selecty.options[_defaulty].defaultSelected = true;
	showSeries();
	addTitleField();
});   