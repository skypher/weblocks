
// Register global AJAX handlers to show progress
Ajax.Responders.register({
  onCreate: function() {
	    $('ajax-progress').innerHTML = "<img src='/pub/images/progress.gif'>";
	}, 
  onComplete: function() {
	    $('ajax-progress').innerHTML = "";
	}
});

function onActionSuccess(transport, json) {
    // Update dirty widgets
    var dirtyWidgets = json['widgets'];
    for(var i in dirtyWidgets) {
	$(i).update(dirtyWidgets[i]);
    }

    // Perform a series of specialized operations
    var onLoadCalls = json['on-load'];
    onLoadCalls.each(function(item)
		     {
			 item.evalJSON().call();
		     });
}

function getActionUrl(actionCode, sessionString) {
    return '/?' + sessionString + '&action=' + actionCode;
}

function initiateAction(actionCode, sessionString) {
    new Ajax.Request(getActionUrl(actionCode, sessionString),
		     {
			 method: 'get',
			 onSuccess: onActionSuccess
		     });
}

function initiateFormAction(actionCode, form, sessionString) {
    new Ajax.Request(getActionUrl(actionCode, sessionString),
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
