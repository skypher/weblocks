
function onActionSuccess(transport, json) {
    for(var i in json) {
	$(i).update(json[i]);
    }
}

function getActionUrl(actionCode) {
    return '/?action=' + actionCode;
}

function initiateAction(actionCode) {
    new Ajax.Request(getActionUrl(actionCode),
		     {
			 method: 'get',
			 onSuccess: onActionSuccess
		     });
}

function initiateFormAction(actionCode, form) {
    new Ajax.Request(getActionUrl(actionCode),
		     {
			 method: form.method,
			 onSuccess: onActionSuccess,
			 parameters: form.serialize(true)    
		     });
}

function disableIrrelevantButtons(currentButton) {
    $(currentButton.form).getInputs('submit').each(function(obj)
						   {
						       obj.disable();
						       currentButton.enable();
						   });
}
