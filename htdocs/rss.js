function main_init () {
    $(function() {
	$( "input[type=submit], button" ).button();
	$( ".mark_all" ).button();
	$( ".mark_week" ).button();
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

function add_feed(username, title, link) {
    $.ajax({
	url:'add-feed'
	,context: document.body
	,data:{link:link,title:unescape(title),username:username}
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

function check_url(username, url) {
    $('#subscribe_results').html('');
    $('#subscribe_dialog').css('cursor', 'wait');
    $.ajax({
	url:'check-url'
	,context: document.body
	,data: {url : url}
	,error: function(xhr, ajaxoptions, thrownError) {
	    $('#subscribe_dialog').css('cursor', 'auto');
	    $('#subscribe_results').append('<div class="ui-state-highlight ui-corner-all" style="margin-top: 20px; padding: 0 .7em;"> \
                                            <span class="ui-icon ui-icon-alert" style="float: left; margin-right: .3em"> \
                                            </span><b>There was a problem with the url:</b> ' + 
					   thrownError + ":" + xhr.status + "</div>");
	}
	,success: function(xml) {
	    $('#subscribe_dialog').css('cursor', 'auto');
	    $(xml).find('site').each(function(){
		var title = $(this).find('title').text();
		var desc = $(this).find('desc').text();
		if (title) {
		    $('#subscribe_results').append("<p><b>" + title + "</b> : " + desc + 
						   "</p><p><button onclick='add_feed(\"" + 
						   username + "\",\"" + 
						   escape(title) + "\",\"" + 
						   url + "\");' type='button'>Is this correct?</button></p>");
		} else {
		    $('#subscribe_results').append("<p>Could not parse feed.</p>");
		}
	    });
	}
    });
}

function flip(desc_id) {
    if (document.getElementById(desc_id).style.display == 'none') {
	document.getElementById(desc_id).style.display = 'block';
	var id = desc_id.replace("desc-","");
	$.ajax({
	    url: 'mark-read'
	    ,data: {item_id : id}
	    ,context: document.body
	});

    } else {
	document.getElementById(desc_id).style.display = 'none';
    }
}

function mark_all_read(feed_id, user_id) {
    $.ajax({
	       url:'mark-all-read'
	       ,context: document.body
	       ,data:{feed_id:feed_id,user_id:user_id}
	       ,success:function() {
		   $.ajax({
			      url : 'get-feed-title'
			      ,data: {feed_id : feed_id}
			      ,content: document.body
			      ,success: function(xml) {
				  var title = $(xml).find('title').text();
				  $('#blog_title_'+feed_id).text(title);
				  document.getElementById("results_"+feed_id).style.display = 'none';
			      }
			  });
	       }
	   });
}

function mark_read_week(feed_id, user_id) {
    $.ajax({
	       url:'mark-read-week'
	       ,context: document.body
	       ,data:{feed_id:feed_id,user_id:user_id}
	       ,success:function() {
		   $.ajax({
			      url : 'get-feed-title'
			      ,data: {feed_id : feed_id}
			      ,content: document.body
			      ,success: function(xml) {
				  var title = $(xml).find('title').text();
				  $('#blog_title_'+feed_id).text(title);
				  document.getElementById("results_"+feed_id).style.display = 'none';
			      }
			  });
	       }
	   });
}
function search() {
    var q = $('#rss_search').val();
    var feed_id = $('#feed_select option:selected').val();
    $('#rss_search').css('cursor', 'wait');
    $('#feed_select').css('cursor', 'wait');
    $('#search_button').css('cursor', 'wait');
    $.ajax({
	url: 'search'
	,data: {q:q,feed:feed_id}
	,context: document.body
	,success: function(xml){
	    $('#results').html('');
	    $('#rss_search').css('cursor', 'auto');
	    $('#feed_select').css('cursor', 'auto');
	    $('#search_button').css('cursor', 'auto');
		if ($(xml).find("result").length > 0) {
		    $(xml).find("result").each(function(){
			var blog_title = $(this).find('blog_title').text();
			var item_title = $(this).find('item_title').text();
			var item_date = $(this).find('item_date').text();
			var item_id = $(this).find('item_id').text();
			var url = $(this).find('url').text();
			var item_star = $(this).find('star').text();
			var star_str = "ui-state-default ui-corner-all";
			if (item_star == "T")
			    star_str = "ui-state-highlight ui-corner-all";
			$('#results').append('<p><span onclick="mark_star('+ item_id + ')" id="star_'+ item_id + 
					     '" class="'+star_str+'"><span class="ui-icon ui-icon-star" style="display:inline-block"></span></span>'
					     +'<b>' + blog_title + '</b> ' +
					     '<a href="javascript:void(0)" onclick="window.open(\'' + url + '\')">' + 
					     item_title + " (" + item_date + ')</a></p>');
		    });
		} else {
		    $('#results').append('<p><div class="ui-state-highlight ui-corner-all" style="margin-top: 20px; padding: 0 .7em; border:0px;"><span class="ui-icon ui-icon-alert" style="float: left; margin-right: .3em;"></span><b>No results found.</b></div></p>');
		}
	}
    });
}

function mark_read(feed_id, item_id) {
    $.ajax({
	url : 'mark-read'
	,data : {item_id : item_id} 
	,context: document.body
    });
    $('#item-'+item_id).delay(5000).hide(0);
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

function mark_star(id) {
    if ($('#star_' + id).is('.ui-state-default')) {
	$('#star_'+id).attr('class', 'ui-state-highlight ui-corner-all');
	$.ajax({url:'star-item', data : {item_id:id}});
    } else {
	$('#star_'+id).attr('class', 'ui-state-default ui-corner-all');
	$.ajax({url:'unstar-item', data : {item_id:id}});
    }
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
		    var item_date = $(this).find('date').text();
		    var item_star = $(this).find('star').text();
		    var star_str = "ui-state-default ui-corner-all";
		    if (item_star == "T")
			star_str = "ui-state-highlight ui-corner-all";
		    $('#results_'+feed_id).append('<div id="item-'+item_id+'" style="display:inline-block"><span onclick=\'javascript:mark_star("' + 
						  item_id + '");\' id="star_' + item_id + '"class="' + star_str + 
						  '"><span class="ui-icon ui-icon-star" style="display:inline-block"></span></span><div style="padding:4px;display:inline;"><a onclick="mark_read('+feed_id + ","+item_id+');window.open(\'' 
						  + item_url + '\');" href="javascript:void(0)">' 
						  + item_title + '</a>&nbsp;(' + item_date + ')</div></div><br>');
		});
	    }
	});
    } else {
	document.getElementById("results_"+feed_id).style.display = 'none';
    }
}
