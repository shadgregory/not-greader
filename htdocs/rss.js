function flip(link_id, desc_id) {
    if (document.getElementById(desc_id).style.display == 'none') {
	document.getElementById(desc_id).style.display = 'block';
	document.getElementById(link_id).innerHTML = 'Hide';
	var id = desc_id.replace("desc-","");
	$.ajax({
	    url: 'mark-read'
	    ,data: {entry_id : id}
	    ,context: document.body
	});

    } else {
	document.getElementById(desc_id).style.display = 'none';
	document.getElementById(link_id).innerHTML = 'Show';
    }
}

function search() {
    var q = $('#rss_search').val();
    $.ajax({
	url: 'search'
	,data: {q:q}
	,context: document.body
	,success: function(xml){
	    $('#results').html('');
	    $(xml).find("result").each(function(){
		var blog_title = $(this).find('blog_title').text();
		var entry_title = $(this).find('entry_title').text();
		var entry_date = $(this).find('entry_date').text();
		var url = $(this).find('url').text();
		$('#results').append('<p><b>' + blog_title + '</b> ' +
				     '<a href="javascript:void(0)" onclick="window.open(\'' + url + '\')">' + 
				     entry_title + " (" + entry_date +
				     ')</a></p>');
	    });
	}
    });
}

function mark_read(feed_id, entry_id) {
    $.ajax({
	url : 'mark-read'
	,data : {entry_id : entry_id} 
	,context: document.body
    });
    $('#entry-'+entry_id).hide();
    $.ajax({
	url : 'get-feed-title'
	,data: {feed_id : feed_id}
	,content: document.body
	,success: function(xml) {
	    var title = $(xml).find('title').text();
	    $('#blog_title_'+feed_id).text(title);
	}
    });
}

function retrieve_unread(feed_id) {

    if (document.getElementById("results_"+feed_id).style.display == 'none') {
	document.getElementById("results_"+feed_id).style.display = 'block';
	$.ajax({
	    url     : 'retrieve-unread'
	    ,data   : {feed_id:feed_id}
	    ,context: document.body
	    ,success: function(xml){
		$('#results_'+feed_id).html('');
		$(xml).find("result").each(function(){
		    var entry_title = $(this).find('title').text();
		    var entry_desc = $(this).find('description').text();
		    var entry_url = $(this).find('url').text();
		    var entry_id = $(this).find('id').text();
		    $('#results_'+feed_id).append('<div style="padding:4px;" id="entry-'+entry_id+'"><a onclick="mark_read('+feed_id + ","+entry_id+');window.open(\'' 
						  + entry_url + '\');" href="javascript:void(0)">' 
						  + entry_title + '</a></div>');
		});
	    }
	});
    } else {
	document.getElementById("results_"+feed_id).style.display = 'none';
    }
}
