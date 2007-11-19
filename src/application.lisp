
(in-package :weblocks)

(export '(*webapp-description* *application-public-dependencies* defwebapp))

(defvar *webapp-name* nil
  "The name of the web application (also see 'defwebapp'). Please
note, this name will be used for the composition of the page title
displayed to the user. See 'page-title' for details.")

(defvar *webapp-description* nil
  "The description of the web application. Please note, this
description will be used for the composition of the page title
displayed to the user. See 'page-title' for details.")

(defvar *application-public-dependencies* nil
  "A list of dependencies on scripts and/or stylesheets that will
persist throughout the whole application. See documentation for
'widget-public-dependencies' for more details.")

(defun defwebapp (name &key description)
  "Sets the application name (the *webapp-name* variable). 'name'
must be a symbol. This symbol will later be used to find a
package that defined 'init-user-session' - a function responsible
for the web application setup.

Additionally, 'name' will be used by 'page-title' to generate the
title of each page.

'init-user-session' must be defined by weblocks client in the
same package as 'name'. This function will accept a single
parameter - a composite widget at the root of the
application. 'init-user-session' is responsible for adding
initial widgets to this composite.

'description' is a short description of the web application (ex: \"A
technology demonstration\"). *webapp-description* will be set to this
description (unless description is nil), and the value will later be
used by 'page-title' to generate the page titles.

By default 'defwebapp' adds the following resources to
*application-public-dependencies*:

Stylesheets: layout.css, main.css
Scripts: prototype.js, weblocks.js, scriptaculous.js"
  (check-type name symbol)
  (setf *webapp-name* name)
  (when description
    (setf *webapp-description* description))
  (setf *application-public-dependencies*
	(public-files-relative-paths
	 '(:stylesheet . "layout")
	 '(:stylesheet . "main")
	 '(:stylesheet . "dialog")
	 '(:script . "prototype")
	 '(:script . "scriptaculous")
	 '(:script . "shortcut")
	 '(:script . "weblocks")
	 '(:script . "dialog"))))


