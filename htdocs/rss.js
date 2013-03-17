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
