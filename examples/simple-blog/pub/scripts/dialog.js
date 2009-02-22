
Position.GetViewportSize = function() {
    var w = window;
    var width = w.innerWidth || (w.document.documentElement.clientWidth || w.document.body.clientWidth);
    var height = w.innerHeight || (w.document.documentElement.clientHeight || w.document.body.clientHeight);
    return [width, height]
}
Position.EyeLevel = function(element, parent) {
    var w, h, pw, ph;
    var d = Element.getDimensions(element);
    w = d.width;
    h = d.height;
    Position.prepare();
    if (!parent) {
	var ws = Position.GetViewportSize();
	pw = ws[0];
	ph = ws[1];
    } else {
	pw = parent.offsetWidth;
	ph = parent.offsetHeight;
    }
    var eyeLevel = ph - (ph/4*3) - (h/2);
    var screenCenter = (ph/2) - (h/2);
    var scrollX = 0, scrollY = 0;
    if(!window.XMLHttpRequest) {
	// IE 6 only, as we can't use position: fixed
	scrollX = Position.deltaX;
	scrollY = Position.deltaY;
    }
    element.style.top = (eyeLevel < (ph / 10) ? screenCenter : eyeLevel) + scrollY + "px";
    element.style.left = (pw/2) - (w/2) + scrollX + "px";
}

function showDialog(title, body, cssClass) {
    showDialog(title, body, cssClass, null);
}


function showDialog(title, body, cssClass, close) {
    // Find or create a graybox
    var graybox = $$('.graybox')[0];
    if(!graybox) {
	graybox = Builder.node('div', { className : 'graybox' });
    } else {
	Element.show(graybox);
    }
    $$('body')[0].appendChild(graybox);
    // Create a dialog
    var dialogBody = Builder.node('div', { className : 'dialog-body' });
    dialogBody.innerHTML = body;

    var titleTextCell = Builder.node('td', {className : 'title-text-cell'},   
                                     [ Builder.node('h1',  {className : 'title-text'}, [title])]);

    var titleButtonCell = null;
    var titleRow = null;
    if (close != null) {
        var buttonDiv = Builder.node('div', { className : 'title-button' });
        buttonDiv.innerHTML = close;
        titleButtonCell = Builder.node('td', {className : 'title-button-cell'}, [ buttonDiv ]);

        titleRow = Builder.node('tr', [ titleTextCell, titleButtonCell ]);
    }
    else {
        titleRow = Builder.node('tr', [ titleTextCell ]);
    }

    var titleBar = Builder.node('table', {className : 'title-bar'}, [titleRow]);

    var dialog = Builder.node('div', { className : ('dialog ' + cssClass) },
			      [ Builder.node('div', { className : 'dialog-extra-top-1' }),
				Builder.node('div', { className : 'dialog-extra-top-2' }),
				Builder.node('div', { className : 'dialog-extra-top-3' }),
                                titleBar,
				dialogBody,
				Builder.node('div', { className : 'dialog-extra-bottom-1' }),
				Builder.node('div', { className : 'dialog-extra-bottom-2' }),
				Builder.node('div', { className : 'dialog-extra-bottom-3' }) ]);
    if(!Prototype.Browser.IE) {
	// Everything but IE, due to z-index issues
	// Necessary to avoid flicker if rendering is slow
	$(dialog).hide();
    }
    $$('div.page-wrapper')[0].appendChild(dialog);
    Position.EyeLevel(dialog);
    if(!Prototype.Browser.IE) {
	// Everything but IE, due to z-index issues
	// Necessary to avoid flicker if rendering is slow
	$(dialog).show();
    }
}

function removeDialog() {
    Element.remove($$('.dialog')[0]);
    Element.hide($$('.graybox')[0]);
}

