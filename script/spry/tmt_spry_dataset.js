/** 
* Copyright 2008 massimocorner.com
* License: http://www.massimocorner.com/license.htm
* @author      Massimo Foti (massimo@massimocorner.com)
* @version     0.2.1, 2008-05-18
*/

if(typeof(tmt) == "undefined"){
	tmt = {};
}

if(typeof(tmt.spry) == "undefined"){
	tmt.spry = {};
}

tmt.spry.PK_PREFIX = "ds_RowID";

/**
* Extends Spry's DataSet object by adding additional APIs
*/
tmt.spry.Dataset = function(options){
	this.data = [];
	this.dataHash = {};
	Spry.Data.DataSet.call(this, options);
}

tmt.spry.Dataset.prototype = new Spry.Data.DataSet();
tmt.spry.Dataset.prototype.constructor = tmt.spry.Dataset;

/**
* Removes all the records from the dataset and notify observers
*/
tmt.spry.Dataset.prototype.clearRows = function(){
	this.unfilteredData = null;
	this.filteredData = null;
	this.data = [];
	this.dataHash = {};
	this.pageOffset = 0;
	this.notifyObservers("onDataChanged");
}

/**
* Delete rows matching given criteria 
* criteria must be an object, containing name value pairs
* Notifies observers only if some data gets deleted
*/
tmt.spry.Dataset.prototype.deleteRows = function(criteria){
	var dataChanged = false;
	var newData = [];
	for(var i=0; i<this.data.length; i++){
		if(tmt.spry.matchCriteria(this.data[i], criteria)){
			dataChanged = true;
		}
		else{
			newData.push(this.getRowCloneByNumber(i));
		}
	}
	if(dataChanged){
		this.replaceAllRows(newData);
		this.dataWasLoaded = true;
		this.notifyObservers("onDataChanged");
	}
}

/**
* Spry's native getRowByRowNumber() returns a *reference* to its internal data structure
* If you change the value you get back, the dataset is updated without notifying observers, or maintaining any related internal structures
* This method instead, returns a by-value copy of a row
*/
tmt.spry.Dataset.prototype.getRowCloneByNumber = function(number){
	var row = this.data[number];
	var rowClone = {};
	for(var x in row){
		if(x.charAt(0) == "@" || x.substr(0, 3) == "ds_"){
			continue;
		}
		rowClone[x] = row[x];
	}
	return rowClone;
}

/**
* Adds rows to a dataset
* Accept either one single object or an array of objects (multiple insert)
*/
tmt.spry.Dataset.prototype.insertRows = function(rowObj){
	/*
	If we only get one record, we put it inside an array anyway, 
	so that from now on the code is the same, 
	either for a single record or an array of rows
	*/
	var recordsHolder = [];
	if(rowObj.constructor == Array){
		recordsHolder = rowObj;
	}
	else{
		recordsHolder.push(rowObj);
	}
	for(var i=0; i<recordsHolder.length; i++){
		// Create new autonumber PK
		var recordID = this.data.length;
		recordsHolder[i][tmt.spry.PK_PREFIX] = recordID;
		this.dataHash[this.data.length] = recordsHolder[i];
		this.data.push(recordsHolder[i]);	
	}
	this.dataWasLoaded = true;
	this.notifyObservers("onDataChanged");
}

/**
* Given a row, or an array of rows, 
* remove the current content from the dataset and populate it from scratch
*/
tmt.spry.Dataset.prototype.replaceAllRows = function(rowObj){
	this.clearRows();
	this.insertRows(rowObj);
}

/**
* Select rows matching given criteria 
* criteria must be an object, containing name value pairs
* Returns an array containing by-value copies of row/s
*/
tmt.spry.Dataset.prototype.selectRows = function(criteria){
	var retData = [];
	for(var i=0; i<this.data.length; i++){
		if(tmt.spry.matchCriteria(this.data[i], criteria)){
			retData.push(this.getRowCloneByNumber(i));
		}
	}
	return retData;
}

/**
* Update rows matching given criteria 
* criteria must be an object, containing name value pairs
* Notifies observers only if some data gets modified
*/
tmt.spry.Dataset.prototype.updateRows = function(criteria, rowObj){
	var dataChanged = false;
	for(var i=0; i<this.data.length; i++){
		if(tmt.spry.matchCriteria(this.data[i], criteria)){
			dataChanged = true;
			for(var x in rowObj){
				this.data[i][x] = rowObj[x];
			}
		}
	}
	if(dataChanged || notifyAlways){
		this.dataWasLoaded = true;
		this.notifyObservers("onDataChanged");
	}
}

// Helper function
tmt.spry.matchCriteria = function(obj, criteria){
	for(var x in criteria){
		if(obj[x] != criteria[x]){
			return false;
		}
	}
	return true;
}