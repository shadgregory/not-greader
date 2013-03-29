function main_init () {
    $(function() {
	$( "input[type=submit], button" ).button();
	$('#subscribe_dialog').dialog({
	    autoOpen: false
	    ,modal: true
	    ,position: ['top',100]
	    ,width:400
	    ,height:350
	});
	$('#opener').click(function() {
	    $('#subscribe_dialog').dialog("open");
	});
    });
}

function add_feed(title, link) {
    $.ajax({
	url:'add-feed'
	,context: document.body
	,data:{link:link,title:title}
	,success: function() {
	    $('#feed_link').val("");
	    $('#subscribe_results').html('');
	    $('#subscribe_dialog').dialog('close');
	}
	,error: function() {
	    $('#subscribe_results').html('');
	    $('#subscribe_results').append("<p><b>There was a problem adding: " 
					   + title + "</b></p>");
	}
    });
}

function check_url(url) {
    $('#subscribe_results').html('');
    $.ajax({
	url:'check-url'
	,context: document.body
	,data: {url : url}
	,success: function(xml) {
	    $(xml).find('site').each(function(){
		var title = $(this).find('title').text();
		var desc = $(this).find('desc').text();
		if (title) {
		    $('#subscribe_results').append("<p><b>" + title + "</b> : " + desc + 
						   "</p><p><button onclick='add_feed(\"" + 
						   title + "\",\"" + 
						   url + "\");' type='button'>Correct</button></p>");
		} else {
		    $('#subscribe_results').append("<p>Could not parse feed.</p>");
		}
	    });
	}
    });
}

function flip(link_id, desc_id) {
    if (document.getElementById(desc_id).style.display == 'none') {
	document.getElementById(desc_id).style.display = 'block';
	document.getElementById(link_id).innerHTML = 'Hide';
	var id = desc_id.replace("desc-","");
	$.ajax({
	    url: 'mark-read'
	    ,data: {item_id : id}
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
		var item_title = $(this).find('item_title').text();
		var item_date = $(this).find('item_date').text();
		var url = $(this).find('url').text();
		$('#results').append('<p><b>' + blog_title + '</b> ' +
				     '<a href="javascript:void(0)" onclick="window.open(\'' + url + '\')">' + 
				     item_title + " (" + item_date +
				     ')</a></p>');
	    });
	}
    });
}

function mark_read(feed_id, item_id) {
    $.ajax({
	url : 'mark-read'
	,data : {item_id : item_id} 
	,context: document.body
    });
    $('#item-'+item_id).hide();
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
		    var item_title = $(this).find('title').text();
		    var item_desc = $(this).find('description').text();
		    var item_url = $(this).find('url').text();
		    var item_id = $(this).find('id').text();
		    $('#results_'+feed_id).append('<div style="padding:4px;" id="item-'+item_id+'"><a onclick="mark_read('+feed_id + ","+item_id+');window.open(\'' 
						  + item_url + '\');" href="javascript:void(0)">' 
						  + item_title + '</a></div>');
		});
	    }
	});
    } else {
	document.getElementById("results_"+feed_id).style.display = 'none';
    }
}
