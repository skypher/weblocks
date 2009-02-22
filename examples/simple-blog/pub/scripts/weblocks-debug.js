
function onActionFailure(transport) {
    document.body.innerHTML=
	'<div style="text-align: left">' +
	transport.responseText
	+ '</div>';
}

