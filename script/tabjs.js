/*
Javascript Browser Sniff 1.0
Jim Cummins - http://www.conxiondesigns.com
*/
var isIE = false;
var isOther = false;
var isNS4 = false;
var isNS6 = false;
/*variables*/
var BOLD= 'companyL';
var BOLDS='pgck08L';
var CURRENT = 'tab1';
var CURRENT_TABSET = 'one';
var HEADER = 'companyH';
var MENUSUB = 'companyM';


if(document.getElementById)
{
	if(!document.all)
	{
		isNS6=true;
	}
	if(document.all)
	{
		isIE=true;
	}
}
else
{
	if(document.layers)
	{
		isNS4=true;
	}
	else
	{
		isOther=true;
	}
}

/*
End of Browser Sniff 1.0
*/

/*
Access Layer Style Properties
Jim Cummins - http://www.conxiondesigns.com
Required components:  Javascript Browser Sniff 1.0
*/
function aLs(layerID)
{
var returnLayer;
	if(isIE)
	{
		returnLayer = eval("document.all." + layerID + ".style");
	}
	if(isNS6)
	{
		returnLayer = eval("document.getElementById('" + layerID + "').style");
	}
	if(isNS4)
	{
		returnLayer = eval("document." + layerID);
	}
	if(isOther)
	{
		returnLayer = "null";
		alert("-[Error]-\\nDue to your browser you will probably not\\nbe able to view all of the following page\\nas it was designed to be viewed. We regret\\nthis error sincerely.");
	}
return returnLayer;
}
/*
End of Accessing Layer Style Properties
*/


function Show(ID)
{
	aLs(ID).display="block";
	CURRENT=ID;
}
function Hide(ID2){
	aLs(ID2).display="none";
}

function Header(ID){
	aLs(HEADER).display="none";
	aLs(ID).display="block";
	HEADER = ID;
}

function ShowMenu(ID){
	aLs(ID).display="block";
	MENUSUB=ID;
}

function HideALL(ALL){
	var i=0;
	for(i=0;i<ALL.length;i++){
		aLs(ALL.slice(i,i+1)).display="none";
	}
}
function Bold(ID){
	aLs(BOLD).color= '#FF6634';
	aLs(BOLDS).color= '#FF6634';
	aLs(ID).color="#FF6634";
	aLs(BOLD).fontWeight="normal";
	aLs(ID).fontWeight="Bold";
	BOLD=ID;
}

function BoldS(ID){
	aLs(ID).color="#FF6634";
	aLs(ID).fontWeight="Bold";
	aLs(BOLDS).color= '#FF6634';
	aLs(BOLDS).fontWeight="normal";
	BOLDS=ID;
}

function setTabs(ID){
//aLs(ID).display="none";
Hide(CURRENT_TABSET);
aLs(ID).display="block";
CURRENT_TABSET = ID;
}





