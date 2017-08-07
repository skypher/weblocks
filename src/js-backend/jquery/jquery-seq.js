/* Version 0.0.3 */

/** 
 * @function eachStep
 * @description Map array collection to deferred callback. Callback executes on every element after previous execution was finished.
 * @example eachStep(["Hello world", "Goodbye world"], function(hello){
 *    var def = $.Deferred();
 *
 *    setTimeout(function(){
 *      alert(hello);
 *      def.resolve();
 *    }, 1000);
 *
 *    return def;
 * }, function(){alert("Just said good bye");});
 *
 * @param {Array} collection A collection of items to map one after another
 * @param {Function} callback A callback that will be called on every item from collection. Callback should return deferred object.
 * @param {Function} [endcallback] A callback that will be called after every collection element is mapped.
 */

function eachStep(collection, callback, endcallback){
  if(collection.length == 0){
    return endcallback && endcallback();
  }

  jQuery.when(callback(collection[0])).always(function(){
    eachStep(collection.slice(1), callback, endcallback);
  });
}

/**
 * @function sequentialExecute
 * @description Executes array of deferred callbacks one after another.
 * @example sequentialExecute([
 *   function(){ 
 *     var def = $.Deferred()
 *
 *     setTimeout(function(){
 *       alert("Hello world");
 *       def.resolve();
 *     }, 1000);
 *
 *     return def;
 *   },
 *   function(){ 
 *     var def = $.Deferred()
 *
 *     setTimeout(function(){
 *       alert("Goodbye world");
 *       def.resolve();
 *     }, 1000);
 *
 *     return def;
 *   }], function(){ alert('All executed'); });
 *
 * @param {Array} collection A collection of functions (callbacks) to execute one after another.
 * @param {Function} [endcallback] A callback that will be called after all callbacks from collection.
 */
function sequentialExecute(collection, endcallback){
  if(collection.length == 0){
    return endcallback && endcallback();
  }

  jQuery.when(collection[0]()).always(function(){
    sequentialExecute(collection.slice(1), endcallback);
  });
}

function getGenerator(eachStepCallback){
  return function(collection, callback){
    return eachStep(collection, eachStepCallback, callback);
  }
}

var cache = [];

function cachingGetGenerator(eachStepCallback){
  var temporary = function(collection, endcallback){
    if(collection.length == 0){
      return endcallback && endcallback();
    }

    var script = collection[0];
    var func = temporary;
    var callback = function(){
      func(collection.slice(1), endcallback);
    };

    if(cache.indexOf(script) == -1){
      eachStepCallback(script, function(){
        cache.push(script);
        callback();
      });
    }else{
      callback();
    }
  }

  return temporary;
}


var executingDeferred = {};

function withGetGenerator(allStuffCallback){
	var withFunction;

  withFunction = function(){
    var old$ = window.$;
    window.$ = jQuery;
    var scripts = Array.prototype.slice.call(arguments, 0, arguments.length - 1);
    var endcallback = arguments[arguments.length - 1];

		var functionToBeDone = function(){
			var defer = jQuery.Deferred();

			allStuffCallback(scripts, function(){
				endcallback && endcallback();
				window.$ = old$;
				defer.resolve();

			  if(executingDeferred[withFunction] && executingDeferred[withFunction].state() == 'resolved'){
					delete executingDeferred[withFunction];
				}
			});

			return defer;
		};

		if(executingDeferred[withFunction]){
			return executingDeferred[withFunction] = executingDeferred[withFunction].pipe(functionToBeDone);
		}else{  
			return executingDeferred[withFunction] = functionToBeDone();
		}
  };

	return withFunction;
}

var loadImageCache = {}
function loadImage(imageSrc) {
  var deferred = jQuery.Deferred();
  if (typeof loadImageCache[imageSrc] === "undefined") {

    preloader         = new Image();
    preloader.onload  = function() { 
      //console && console.log("Loaded image " + this.src);
      deferred.resolve(this.src) 
    };
    preloader.onerror = function() { 
      console && console.log("Can not load an image " + this.src);
      deferred.reject(this.src)  
    };
    preloader.src     = imageSrc;

    loadImageCache[imageSrc] = true;
  }else{
    console && console.log("Image cached " + imageSrc);
    deferred.resolve(imageSrc);
  }

  return deferred;
};

function appendStyleSheet(css_file){
  var html_doc = document.getElementsByTagName('head').item(0);
  var css = document.createElement('link');
  css.setAttribute('rel', 'stylesheet');
  css.setAttribute('type', 'text/css');
  css.setAttribute('href', css_file);
  html_doc.appendChild(css);
  return false;
}

function loadStyleSheet(source, callback){
  return jQuery.get(source, function(){
    appendStyleSheet(source);
    callback && callback();
  });
}

/**
 * @function getScripts
 * @description Loads javascript files one after another.
 *
 * @param {Array} collection Collection of scripts to load.
 * @param {Function} [endcallback] Callback that will be executed after all scripts loaded.
 */
getScripts = getGenerator(jQuery.getScript);

/**
 * @function getImages
 * @description Preloads images one after another. 
 *
 * @param {Array} collection Collection of images to preload.
 * @param {Function} [endcallback] Callback that will be executed after all images are preloaded.
 */
getImages = getGenerator(loadImage);

/**
 * @function getFiles
 * @description Loads files one after another. Useful for flash files for example.
 *
 * @param {Array} collection Collection of images to preload.
 * @param {Function} [endcallback] Callback that will be executed after all images are preloaded.
 */
getFiles = getGenerator(jQuery.get);

/**
 * @function getStyles
 * @description Loads stylesheets from urls one after another.
 *
 * @param {Array} collection Collection of stylesheets urls to use in document.
 * @param {Function} [endcallback] Callback that will be executed after stylesheets loaded.
 */
getStyles = getGenerator(loadStyleSheet);

/**
 * @function getScriptsNotCached
 * @description Does same as {@link getScripts} but doesn't load scripts loaded before with this function.
 *
 * @param {Array} collection Collection of scripts to load.
 * @param {Function} [endcallback] Callback that will be executed after all is loaded.
 */
getScriptsNotCached = cachingGetGenerator(jQuery.getScript);
// TODO: test commented stuff
//getImagesNotCached = cachingGetGenerator(loadImage);
/**
 * @function getFilesNotCached
 * @description Does same as {@link getFiles} but doesn't load scripts loaded before with this function.
 *
 * @param {Array} collection Collection of files to load.
 * @param {Function} [endcallback] Callback that will be executed after all is loaded.
 */
getFilesNotCached = cachingGetGenerator(jQuery.get);

/**
 * @function getStylesNotCached
 * @description Does same as {@link getStyles} but doesn't load stylesheets loaded before with this function.
 *
 * @param {Array} collection Collection of stylesheets urls to use in document.
 * @param {Function} [endcallback] Callback that will be executed after stylesheets loaded.
 */
getStylesNotCached = cachingGetGenerator(loadStyleSheet);

/**
 * @function withScripts
 * @description Tries to load as script every url given as param and after that optionally executes callback given as last parameter.
 * Does not load scripts already loaded with this function or function {@link getScriptsNotCached}
 * @example withScripts("/script-1.js", "/script-2.js", function(){
 *   Script1Function();
 *   Script2Function();
 * });
 *
 * @param {String} url Url of script
 * @param {String} url Url of script
 * @param {String} url ...
 * @param {Function} [endcallback] Callback that will be executed after all scripts loaded.
 */
withScripts = withGetGenerator(getScriptsNotCached);
//withImages = withGetGenerator(getImagesNotCached);
/**
 * @function withFiles
 * @description Tries to preload every url given as param and after that optionally executes callback given as last parameter.
 * Does not load files already loaded with this function or function {@link getFilesNotCached}
 * @example withFiles("/style-1.css", "/style-2.css", function(){
 *   $('#test-1').addClass('class-from-style-1');
 *   $('#test-2').addClass('class-from-style-2');
 * });
 *
 * @param {String} url Url of stylesheet
 * @param {String} url Url of stylesheet
 * @param {String} url ...
 * @param {Function} [endcallback] Callback that will be executed after all scripts loaded.
 */
withFiles = withGetGenerator(getFilesNotCached);

/**
 * @function withStyles
 * @description Tries to load as stylesheet every url given as param and after that optionally executes callback given as last parameter.
 * Does not load stylesheets already loaded with this function or function {@link getStylesNotCached}
 * @example withStyles("/style-1.css", "/style-2.css", function(){
 *   $('#test-1').addClass('class-from-style-1');
 *   $('#test-2').addClass('class-from-style-2');
 * });
 *
 * @param {String} url Url of stylesheet
 * @param {String} url Url of stylesheet
 * @param {String} url ...
 * @param {Function} [endcallback] Callback that will be executed after all scripts loaded.
 */
withStyles = withGetGenerator(getStylesNotCached);
