function searchComplete() {

  // Check that we got results
  //document.getElementById('news').innerHTML = '';
  if (newsSearch.results && newsSearch.results.length > 0) {
    for (var i = 0; i < newsSearch.results.length; i++) {
	//alert(toURL(newsSearch.results[i].url));
      // Create HTML elements for search results
      var p = document.createElement('p');
	  p.className = 'news_title';
	  var p1 = document.createElement('p');
	  var d = document.createElement('div');
	  //d.className = 'news_item';
	  d.setAttribute('className', 'news_item');
	  d.setAttribute('class', 'news_item');
	  var source = document.createElement('p');
	  source.setAttribute('className', 'news_source');
	  source.setAttribute('class', 'news_source');	  
	  var sp = document.createElement('span');
      var a = document.createElement('a');
	  //alert(newsSearch.results[i].url);
      //a.href = toURL(newsSearch.results[i].url);
	  a.href = newsSearch.results[i].unescapedUrl;
      a.innerHTML = newsSearch.results[i].title;
	  a.setAttribute('target', '_new');

      // Append search results to the HTML nodes
      p.appendChild(a);
	  d.appendChild(p);
	  p1.innerHTML = newsSearch.results[i].content;
	  d.appendChild(p1);
	  source.innerHTML = newsSearch.results[i].publishedDate + "  " + newsSearch.results[i].publisher;
	  d.appendChild(source);
	  document.getElementById('news').appendChild(d);
      //document.body.appendChild(p);
    }
  }
}

function onLoad() {

  // Create a News Search instance.
  newsSearch = new google.search.NewsSearch();
  
  // Set searchComplete as the callback function when a search is
  // complete.  The newsSearch object will have results in it.
  newsSearch.setSearchCompleteCallback(this, searchComplete, null);

  // Specify search quer(ies)
  newsSearch.execute('Snake river + salmon + fish');
  newsSearch.execute('Columbia River Salmon');
  
  // Include the required Google branding
  google.search.Search.getBranding('branding');
}

