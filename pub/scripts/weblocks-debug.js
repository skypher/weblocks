
function onActionFailure(transport) {
    $$('head link').each(function(item) {
	if(item.getAttribute('rel') == 'stylesheet') {
	    item.remove();
	}
    });
    updateElementBody(document.body,
		      '<div style="text-align: left">' +
		      transport.responseText
		     + '</div>');
}

