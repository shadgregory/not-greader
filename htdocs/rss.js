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
				     '<a href="' + url + '">' + 
				     entry_title + " (" + entry_date + ")" +
				     '</a></p>');
	    });
	}

    });

}
