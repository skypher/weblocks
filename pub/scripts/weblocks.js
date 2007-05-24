
function onActionSuccess(transport, json) {
    for(var i in json) {
	$(i).update(json[i]);
    }
}

function initiateAction(actionCode) {
    new Ajax.Request('/?action=' + actionCode,
		     {
			 method: 'get',
			 onSuccess: onActionSuccess
		     });
}

