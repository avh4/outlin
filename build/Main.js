(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
module.exports = createHash

function createHash(elem) {
    var attributes = elem.attributes
    var hash = {}

    if (attributes === null || attributes === undefined) {
        return hash
    }

    for (var i = 0; i < attributes.length; i++) {
        var attr = attributes[i]

        if (attr.name.substr(0,5) !== "data-") {
            continue
        }

        hash[attr.name.substr(5)] = attr.value
    }

    return hash
}

},{}],2:[function(require,module,exports){
var createStore = require("weakmap-shim/create-store")
var Individual = require("individual")

var createHash = require("./create-hash.js")

var hashStore = Individual("__DATA_SET_WEAKMAP@3", createStore())

module.exports = DataSet

function DataSet(elem) {
    var store = hashStore(elem)

    if (!store.hash) {
        store.hash = createHash(elem)
    }

    return store.hash
}

},{"./create-hash.js":1,"individual":3,"weakmap-shim/create-store":4}],3:[function(require,module,exports){
(function (global){
var root = typeof window !== 'undefined' ?
    window : typeof global !== 'undefined' ?
    global : {};

module.exports = Individual

function Individual(key, value) {
    if (root[key]) {
        return root[key]
    }

    Object.defineProperty(root, key, {
        value: value
        , configurable: true
    })

    return value
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],4:[function(require,module,exports){
var hiddenStore = require('./hidden-store.js');

module.exports = createStore;

function createStore() {
    var key = {};

    return function (obj) {
        if (typeof obj !== 'object' || obj === null) {
            throw new Error('Weakmap-shim: Key must be object')
        }

        var store = obj.valueOf(key);
        return store && store.identity === key ?
            store : hiddenStore(obj, key);
    };
}

},{"./hidden-store.js":5}],5:[function(require,module,exports){
module.exports = hiddenStore;

function hiddenStore(obj, key) {
    var store = { identity: key };
    var valueOf = obj.valueOf;

    Object.defineProperty(obj, "valueOf", {
        value: function (value) {
            return value !== key ?
                valueOf.apply(this, arguments) : store;
        },
        writable: true
    });

    return store;
}

},{}],6:[function(require,module,exports){
var DataSet = require("data-set")

module.exports = addEvent

function addEvent(target, type, handler) {
    var ds = DataSet(target)
    var events = ds[type]

    if (!events) {
        ds[type] = handler
    } else if (Array.isArray(events)) {
        if (events.indexOf(handler) === -1) {
            events.push(handler)
        }
    } else if (events !== handler) {
        ds[type] = [events, handler]
    }
}

},{"data-set":2}],7:[function(require,module,exports){
var globalDocument = require("global/document")
var DataSet = require("data-set")

var addEvent = require("./add-event.js")
var removeEvent = require("./remove-event.js")
var ProxyEvent = require("./proxy-event.js")

module.exports = DOMDelegator

function DOMDelegator(document) {
    document = document || globalDocument

    this.target = document.documentElement
    this.events = {}
    this.rawEventListeners = {}
    this.globalListeners = {}
}

DOMDelegator.prototype.addEventListener = addEvent
DOMDelegator.prototype.removeEventListener = removeEvent

DOMDelegator.prototype.addGlobalEventListener =
    function addGlobalEventListener(eventName, fn) {
        var listeners = this.globalListeners[eventName]
        if (!listeners) {
            listeners = this.globalListeners[eventName] = []
        }

        if (listeners.indexOf(fn) === -1) {
            listeners.push(fn)
        }
    }

DOMDelegator.prototype.removeGlobalEventListener =
    function removeGlobalEventListener(eventName, fn) {
        var listeners = this.globalListeners[eventName]
        if (!listeners) {
            return
        }

        var index = listeners.indexOf(fn)
        if (index !== -1) {
            listeners.splice(index, 1)
        }
    }

DOMDelegator.prototype.listenTo = function listenTo(eventName) {
    if (this.events[eventName]) {
        return
    }

    this.events[eventName] = true
    listen(this, eventName)
}

DOMDelegator.prototype.unlistenTo = function unlistenTo(eventName) {
    if (!this.events[eventName]) {
        return
    }

    this.events[eventName] = false
    unlisten(this, eventName)
}

function listen(delegator, eventName) {
    var listener = delegator.rawEventListeners[eventName]

    if (!listener) {
        listener = delegator.rawEventListeners[eventName] =
            createHandler(eventName, delegator.globalListeners)
    }

    delegator.target.addEventListener(eventName, listener, true)
}

function unlisten(delegator, eventName) {
    var listener = delegator.rawEventListeners[eventName]

    if (!listener) {
        throw new Error("dom-delegator#unlistenTo: cannot " +
            "unlisten to " + eventName)
    }

    delegator.target.removeEventListener(eventName, listener, true)
}

function createHandler(eventName, globalListeners) {
    return handler

    function handler(ev) {
        var globalHandlers = globalListeners[eventName] || []
        var listener = getListener(ev.target, eventName)

        var handlers = globalHandlers
            .concat(listener ? listener.handlers : [])
        if (handlers.length === 0) {
            return
        }

        var arg = new ProxyEvent(ev, listener)

        handlers.forEach(function (handler) {
            typeof handler === "function" ?
                handler(arg) : handler.handleEvent(arg)
        })
    }
}

function getListener(target, type) {
    // terminate recursion if parent is `null`
    if (target === null) {
        return null
    }

    var ds = DataSet(target)
    // fetch list of handler fns for this event
    var handler = ds[type]
    var allHandler = ds.event

    if (!handler && !allHandler) {
        return getListener(target.parentNode, type)
    }

    var handlers = [].concat(handler || [], allHandler || [])
    return new Listener(target, handlers)
}

function Listener(target, handlers) {
    this.currentTarget = target
    this.handlers = handlers
}

},{"./add-event.js":6,"./proxy-event.js":13,"./remove-event.js":14,"data-set":2,"global/document":10}],8:[function(require,module,exports){
var Individual = require("individual")
var cuid = require("cuid")
var globalDocument = require("global/document")

var DOMDelegator = require("./dom-delegator.js")

var delegatorCache = Individual("__DOM_DELEGATOR_CACHE@9", {
    delegators: {}
})
var commonEvents = [
    "blur", "change", "click",  "contextmenu", "dblclick",
    "error","focus", "focusin", "focusout", "input", "keydown",
    "keypress", "keyup", "load", "mousedown", "mouseup",
    "resize", "scroll", "select", "submit", "unload"
]

/*  Delegator is a thin wrapper around a singleton `DOMDelegator`
        instance.

    Only one DOMDelegator should exist because we do not want
        duplicate event listeners bound to the DOM.

    `Delegator` will also `listenTo()` all events unless 
        every caller opts out of it
*/
module.exports = Delegator

function Delegator(opts) {
    opts = opts || {}
    var document = opts.document || globalDocument

    var cacheKey = document["__DOM_DELEGATOR_CACHE_TOKEN@9"]

    if (!cacheKey) {
        cacheKey =
            document["__DOM_DELEGATOR_CACHE_TOKEN@9"] = cuid()
    }

    var delegator = delegatorCache.delegators[cacheKey]

    if (!delegator) {
        delegator = delegatorCache.delegators[cacheKey] =
            new DOMDelegator(document)
    }

    if (opts.defaultEvents !== false) {
        for (var i = 0; i < commonEvents.length; i++) {
            delegator.listenTo(commonEvents[i])
        }
    }

    return delegator
}



},{"./dom-delegator.js":7,"cuid":9,"global/document":10,"individual":11}],9:[function(require,module,exports){
/**
 * cuid.js
 * Collision-resistant UID generator for browsers and node.
 * Sequential for fast db lookups and recency sorting.
 * Safe for element IDs and server-side lookups.
 *
 * Extracted from CLCTR
 * 
 * Copyright (c) Eric Elliott 2012
 * MIT License
 */

/*global window, navigator, document, require, process, module */
(function (app) {
  'use strict';
  var namespace = 'cuid',
    c = 0,
    blockSize = 4,
    base = 36,
    discreteValues = Math.pow(base, blockSize),

    pad = function pad(num, size) {
      var s = "000000000" + num;
      return s.substr(s.length-size);
    },

    randomBlock = function randomBlock() {
      return pad((Math.random() *
            discreteValues << 0)
            .toString(base), blockSize);
    },

    safeCounter = function () {
      c = (c < discreteValues) ? c : 0;
      c++; // this is not subliminal
      return c - 1;
    },

    api = function cuid() {
      // Starting with a lowercase letter makes
      // it HTML element ID friendly.
      var letter = 'c', // hard-coded allows for sequential access

        // timestamp
        // warning: this exposes the exact date and time
        // that the uid was created.
        timestamp = (new Date().getTime()).toString(base),

        // Prevent same-machine collisions.
        counter,

        // A few chars to generate distinct ids for different
        // clients (so different computers are far less
        // likely to generate the same id)
        fingerprint = api.fingerprint(),

        // Grab some more chars from Math.random()
        random = randomBlock() + randomBlock();

        counter = pad(safeCounter().toString(base), blockSize);

      return  (letter + timestamp + counter + fingerprint + random);
    };

  api.slug = function slug() {
    var date = new Date().getTime().toString(36),
      counter,
      print = api.fingerprint().slice(0,1) +
        api.fingerprint().slice(-1),
      random = randomBlock().slice(-2);

      counter = safeCounter().toString(36).slice(-4);

    return date.slice(-2) + 
      counter + print + random;
  };

  api.globalCount = function globalCount() {
    // We want to cache the results of this
    var cache = (function calc() {
        var i,
          count = 0;

        for (i in window) {
          count++;
        }

        return count;
      }());

    api.globalCount = function () { return cache; };
    return cache;
  };

  api.fingerprint = function browserPrint() {
    return pad((navigator.mimeTypes.length +
      navigator.userAgent.length).toString(36) +
      api.globalCount().toString(36), 4);
  };

  // don't change anything from here down.
  if (app.register) {
    app.register(namespace, api);
  } else if (typeof module !== 'undefined') {
    module.exports = api;
  } else {
    app[namespace] = api;
  }

}(this.applitude || this));

},{}],10:[function(require,module,exports){
(function (global){
var topLevel = typeof global !== 'undefined' ? global :
    typeof window !== 'undefined' ? window : {}
var minDoc = require('min-document');

if (typeof document !== 'undefined') {
    module.exports = document;
} else {
    var doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'];

    if (!doccy) {
        doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'] = minDoc;
    }

    module.exports = doccy;
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"min-document":41}],11:[function(require,module,exports){
module.exports=require(3)
},{}],12:[function(require,module,exports){
if (typeof Object.create === 'function') {
  // implementation from standard node.js 'util' module
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    ctor.prototype = Object.create(superCtor.prototype, {
      constructor: {
        value: ctor,
        enumerable: false,
        writable: true,
        configurable: true
      }
    });
  };
} else {
  // old school shim for old browsers
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    var TempCtor = function () {}
    TempCtor.prototype = superCtor.prototype
    ctor.prototype = new TempCtor()
    ctor.prototype.constructor = ctor
  }
}

},{}],13:[function(require,module,exports){
var inherits = require("inherits")

var ALL_PROPS = [
    "altKey", "bubbles", "cancelable", "ctrlKey",
    "eventPhase", "metaKey", "relatedTarget", "shiftKey",
    "target", "timeStamp", "type", "view", "which"
]
var KEY_PROPS = ["char", "charCode", "key", "keyCode"]
var MOUSE_PROPS = [
    "button", "buttons", "clientX", "clientY", "layerX",
    "layerY", "offsetX", "offsetY", "pageX", "pageY",
    "screenX", "screenY", "toElement"
]

var rkeyEvent = /^key|input/
var rmouseEvent = /^(?:mouse|pointer|contextmenu)|click/

module.exports = ProxyEvent

function ProxyEvent(ev, listener) {
    if (!(this instanceof ProxyEvent)) {
        return new ProxyEvent(ev, listener)
    }

    if (rkeyEvent.test(ev.type)) {
        return new KeyEvent(ev, listener)
    } else if (rmouseEvent.test(ev.type)) {
        return new MouseEvent(ev, listener)
    }

    for (var i = 0; i < ALL_PROPS.length; i++) {
        var propKey = ALL_PROPS[i]
        this[propKey] = ev[propKey]
    }

    this._rawEvent = ev
    this.currentTarget = listener ? listener.currentTarget : null
}

ProxyEvent.prototype.preventDefault = function () {
    this._rawEvent.preventDefault()
}

function MouseEvent(ev, listener) {
    for (var i = 0; i < ALL_PROPS.length; i++) {
        var propKey = ALL_PROPS[i]
        this[propKey] = ev[propKey]
    }

    for (var j = 0; j < MOUSE_PROPS.length; j++) {
        var mousePropKey = MOUSE_PROPS[j]
        this[mousePropKey] = ev[mousePropKey]
    }

    this._rawEvent = ev
    this.currentTarget = listener ? listener.currentTarget : null
}

inherits(MouseEvent, ProxyEvent)

function KeyEvent(ev, listener) {
    for (var i = 0; i < ALL_PROPS.length; i++) {
        var propKey = ALL_PROPS[i]
        this[propKey] = ev[propKey]
    }

    for (var j = 0; j < KEY_PROPS.length; j++) {
        var keyPropKey = KEY_PROPS[j]
        this[keyPropKey] = ev[keyPropKey]
    }

    this._rawEvent = ev
    this.currentTarget = listener ? listener.currentTarget : null
}

inherits(KeyEvent, ProxyEvent)

},{"inherits":12}],14:[function(require,module,exports){
var DataSet = require("data-set")

module.exports = removeEvent

function removeEvent(target, type, handler) {
    var ds = DataSet(target)
    var events = ds[type]

    if (!events) {
        return
    } else if (Array.isArray(events)) {
        var index = events.indexOf(handler)
        if (index !== -1) {
            events.splice(index, 1)
        }
    } else if (events === handler) {
        ds[type] = null
    }
}

},{"data-set":2}],15:[function(require,module,exports){
var createElement = require("./vdom/create-element")

module.exports = createElement

},{"./vdom/create-element":22}],16:[function(require,module,exports){
var diff = require("./vtree/diff")

module.exports = diff

},{"./vtree/diff":27}],17:[function(require,module,exports){
if (typeof document !== "undefined") {
    module.exports = document;
} else {
    module.exports = require("min-document");
}

},{"min-document":41}],18:[function(require,module,exports){
module.exports = isObject

function isObject(x) {
    return typeof x === "object" && x !== null
}

},{}],19:[function(require,module,exports){
var nativeIsArray = Array.isArray
var toString = Object.prototype.toString

module.exports = nativeIsArray || isArray

function isArray(obj) {
    return toString.call(obj) === "[object Array]"
}

},{}],20:[function(require,module,exports){
var patch = require("./vdom/patch")

module.exports = patch

},{"./vdom/patch":25}],21:[function(require,module,exports){
var isObject = require("is-object")

var isHook = require("../vtree/is-vhook")

module.exports = applyProperties

function applyProperties(node, props, previous) {
    for (var propName in props) {
        var propValue = props[propName]

        if (isHook(propValue)) {
            propValue.hook(node,
                propName,
                previous ? previous[propName] : undefined)
        } else {
            if (isObject(propValue)) {
                if (!isObject(node[propName])) {
                    node[propName] = {}
                }

                for (var k in propValue) {
                    node[propName][k] = propValue[k]
                }
            } else if (propValue !== undefined) {
                node[propName] = propValue
            }
        }
    }
}

},{"../vtree/is-vhook":28,"is-object":18}],22:[function(require,module,exports){
var document = require("global/document")

var applyProperties = require("./apply-properties")

var isVNode = require("../vtree/is-vnode")
var isVText = require("../vtree/is-vtext")
var isWidget = require("../vtree/is-widget")

module.exports = createElement

function createElement(vnode, opts) {
    var doc = opts ? opts.document || document : document
    var warn = opts ? opts.warn : null

    if (isWidget(vnode)) {
        return vnode.init()
    } else if (isVText(vnode)) {
        return doc.createTextNode(vnode.text)
    } else if (!isVNode(vnode)) {
        if (warn) {
            warn("Item is not a valid virtual dom node", vnode)
        }
        return null
    }

    var node = (vnode.namespace === null) ?
        doc.createElement(vnode.tagName) :
        doc.createElementNS(vnode.namespace, vnode.tagName)

    var props = vnode.properties
    applyProperties(node, props)

    var children = vnode.children

    for (var i = 0; i < children.length; i++) {
        var childNode = createElement(children[i], opts)
        if (childNode) {
            node.appendChild(childNode)
        }
    }

    return node
}

},{"../vtree/is-vnode":29,"../vtree/is-vtext":30,"../vtree/is-widget":31,"./apply-properties":21,"global/document":17}],23:[function(require,module,exports){
// Maps a virtual DOM tree onto a real DOM tree in an efficient manner.
// We don't want to read all of the DOM nodes in the tree so we use
// the in-order tree indexing to eliminate recursion down certain branches.
// We only recurse into a DOM node if we know that it contains a child of
// interest.

var noChild = {}

module.exports = domIndex

function domIndex(rootNode, tree, indices, nodes) {
    if (!indices || indices.length === 0) {
        return {}
    } else {
        indices.sort(ascending)
        return recurse(rootNode, tree, indices, nodes, 0)
    }
}

function recurse(rootNode, tree, indices, nodes, rootIndex) {
    nodes = nodes || {}


    if (rootNode) {
        if (indexInRange(indices, rootIndex, rootIndex)) {
            nodes[rootIndex] = rootNode
        }

        var vChildren = tree.children

        if (vChildren) {

            var childNodes = rootNode.childNodes

            for (var i = 0; i < tree.children.length; i++) {
                rootIndex += 1

                var vChild = vChildren[i] || noChild
                var nextIndex = rootIndex + (vChild.count || 0)

                // skip recursion down the tree if there are no nodes down here
                if (indexInRange(indices, rootIndex, nextIndex)) {
                    recurse(childNodes[i], vChild, indices, nodes, rootIndex)
                }

                rootIndex = nextIndex
            }
        }
    }

    return nodes
}

// Binary search for an index in the interval [left, right]
function indexInRange(indices, left, right) {
    if (indices.length === 0) {
        return false
    }

    var minIndex = 0
    var maxIndex = indices.length - 1
    var currentIndex
    var currentItem

    while (minIndex <= maxIndex) {
        currentIndex = ((maxIndex + minIndex) / 2) >> 0
        currentItem = indices[currentIndex]

        if (minIndex === maxIndex) {
            return currentItem >= left && currentItem <= right
        } else if (currentItem < left) {
            minIndex = currentIndex + 1
        } else  if (currentItem > right) {
            maxIndex = currentIndex - 1
        } else {
            return true
        }
    }

    return false;
}

function ascending(a, b) {
    return a > b ? 1 : -1
}

},{}],24:[function(require,module,exports){
var applyProperties = require("./apply-properties")

var isWidget = require("../vtree/is-widget")
var VPatch = require("../vtree/vpatch")

var render = require("./create-element")
var updateWidget = require("./update-widget")

module.exports = applyPatch

function applyPatch(vpatch, domNode, renderOptions) {
    var type = vpatch.type
    var vNode = vpatch.vNode
    var patch = vpatch.patch

    switch (type) {
        case VPatch.REMOVE:
            return removeNode(domNode, vNode)
        case VPatch.INSERT:
            return insertNode(domNode, patch, renderOptions)
        case VPatch.VTEXT:
            return stringPatch(domNode, vNode, patch, renderOptions)
        case VPatch.WIDGET:
            return widgetPatch(domNode, vNode, patch, renderOptions)
        case VPatch.VNODE:
            return vNodePatch(domNode, vNode, patch, renderOptions)
        case VPatch.ORDER:
            reorderChildren(domNode, patch)
            return domNode
        case VPatch.PROPS:
            applyProperties(domNode, patch, vNode.propeties)
            return domNode
        default:
            return domNode
    }
}

function removeNode(domNode, vNode) {
    var parentNode = domNode.parentNode

    if (parentNode) {
        parentNode.removeChild(domNode)
    }

    destroyWidget(domNode, vNode);

    return null
}

function insertNode(parentNode, vNode, renderOptions) {
    var newNode = render(vNode, renderOptions)

    if (parentNode) {
        parentNode.appendChild(newNode)
    }

    return parentNode
}

function stringPatch(domNode, leftVNode, vText, renderOptions) {
    var newNode

    if (domNode.nodeType === 3) {
        domNode.replaceData(0, domNode.length, vText.text)
        newNode = domNode
    } else {
        var parentNode = domNode.parentNode
        newNode = render(vText, renderOptions)

        if (parentNode) {
            parentNode.replaceChild(newNode, domNode)
        }
    }

    destroyWidget(domNode, leftVNode)

    return newNode
}

function widgetPatch(domNode, leftVNode, widget, renderOptions) {
    if (updateWidget(leftVNode, widget)) {
        return widget.update(leftVNode, domNode) || domNode
    }

    var parentNode = domNode.parentNode
    var newWidget = render(widget, renderOptions)

    if (parentNode) {
        parentNode.replaceChild(newWidget, domNode)
    }

    destroyWidget(domNode, leftVNode)

    return newWidget
}

function vNodePatch(domNode, leftVNode, vNode, renderOptions) {
    var parentNode = domNode.parentNode
    var newNode = render(vNode, renderOptions)

    if (parentNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    destroyWidget(domNode, leftVNode)

    return newNode
}

function destroyWidget(domNode, w) {
    if (typeof w.destroy === "function" && isWidget(w)) {
        w.destroy(domNode)
    }
}

function reorderChildren(domNode, bIndex) {
    var children = []
    var childNodes = domNode.childNodes
    var len = childNodes.length
    var i

    for (i = 0; i < len; i++) {
        children.push(domNode.childNodes[i])
    }

    for (i = 0; i < len; i++) {
        var move = bIndex[i]
        if (move !== undefined) {
            var node = children[move]
            domNode.removeChild(node)
            domNode.insertBefore(node, childNodes[i])
        }
    }
}

},{"../vtree/is-widget":31,"../vtree/vpatch":33,"./apply-properties":21,"./create-element":22,"./update-widget":26}],25:[function(require,module,exports){
var document = require("global/document")
var isArray = require("x-is-array")

var domIndex = require("./dom-index")
var patchOp = require("./patch-op")

module.exports = patch

function patch(rootNode, patches) {
    var indices = patchIndices(patches)

    if (indices.length === 0) {
        return rootNode
    }

    var index = domIndex(rootNode, patches.a, indices)
    var ownerDocument = rootNode.ownerDocument
    var renderOptions

    if (ownerDocument !== document) {
        renderOptions = {
            document: ownerDocument
        }
    }

    for (var i = 0; i < indices.length; i++) {
        var nodeIndex = indices[i]
        rootNode = applyPatch(rootNode,
            index[nodeIndex],
            patches[nodeIndex],
            renderOptions)
    }

    return rootNode
}

function applyPatch(rootNode, domNode, patchList, renderOptions) {
    if (!domNode) {
        return rootNode
    }

    var newNode

    if (isArray(patchList)) {
        for (var i = 0; i < patchList.length; i++) {
            newNode = patchOp(patchList[i], domNode, renderOptions)

            if (domNode === rootNode) {
                rootNode = newNode
            }
        }
    } else {
        newNode = patchOp(patchList, domNode, renderOptions)

        if (domNode === rootNode) {
            rootNode = newNode
        }
    }

    return rootNode
}

function patchIndices(patches) {
    var indices = []

    for (var key in patches) {
        if (key !== "a") {
            indices.push(Number(key))
        }
    }

    return indices
}

},{"./dom-index":23,"./patch-op":24,"global/document":17,"x-is-array":19}],26:[function(require,module,exports){
var isWidget = require("../vtree/is-widget")

module.exports = updateWidget

function updateWidget(a, b) {
    if (isWidget(a) && isWidget(b)) {
        if ("type" in a && "type" in b) {
            return a.type === b.type
        } else {
            return a.init === b.init
        }
    }

    return false
}

},{"../vtree/is-widget":31}],27:[function(require,module,exports){
var isArray = require("x-is-array")
var isObject = require("is-object")

var VPatch = require("./vpatch")
var isVNode = require("./is-vnode")
var isVText = require("./is-vtext")
var isWidget = require("./is-widget")

module.exports = diff

function diff(a, b) {
    var patch = { a: a }
    walk(a, b, patch, 0)
    return patch
}

function walk(a, b, patch, index) {
    if (a === b) {
        hooks(b, patch, index)
        return
    }

    var apply = patch[index]

    if (isWidget(b)) {
        apply = appendPatch(apply, new VPatch(VPatch.WIDGET, a, b))

        if (!isWidget(a)) {
            destroyWidgets(a, patch, index)
        }
    } else if (isVText(b)) {
        if (!isVText(a)) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
            destroyWidgets(a, patch, index)
        } else if (a.text !== b.text) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
        }
    } else if (isVNode(b)) {
        if (isVNode(a)) {
            if (a.tagName === b.tagName &&
                a.namespace === b.namespace &&
                a.key === b.key) {
                var propsPatch = diffProps(a.properties, b.properties, b.hooks)
                if (propsPatch) {
                    apply = appendPatch(apply,
                        new VPatch(VPatch.PROPS, a, propsPatch))
                }
            } else {
                apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
                destroyWidgets(a, patch, index)
            }

            apply = diffChildren(a, b, patch, apply, index)
        } else {
            apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
            destroyWidgets(a, patch, index)
        }
    } else if (b == null) {
        apply = appendPatch(apply, new VPatch(VPatch.REMOVE, a, b))
        destroyWidgets(a, patch, index)
    }

    if (apply) {
        patch[index] = apply
    }
}

function diffProps(a, b, hooks) {
    var diff

    for (var aKey in a) {
        if (!(aKey in b)) {
            continue
        }

        var aValue = a[aKey]
        var bValue = b[aKey]

        if (hooks && aKey in hooks) {
            diff = diff || {}
            diff[aKey] = bValue
        } else {
            if (isObject(aValue) && isObject(bValue)) {
                if (getPrototype(bValue) !== getPrototype(aValue)) {
                    diff = diff || {}
                    diff[aKey] = bValue
                } else {
                    var objectDiff = diffProps(aValue, bValue)
                    if (objectDiff) {
                        diff = diff || {}
                        diff[aKey] = objectDiff
                    }
                }
            } else if (aValue !== bValue && bValue !== undefined) {
                diff = diff || {}
                diff[aKey] = bValue
            }
        }
    }

    for (var bKey in b) {
        if (!(bKey in a)) {
            diff = diff || {}
            diff[bKey] = b[bKey]
        }
    }

    return diff
}

function getPrototype(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

function diffChildren(a, b, patch, apply, index) {
    var aChildren = a.children
    var bChildren = reorder(aChildren, b.children)

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen

    for (var i = 0; i < len; i++) {
        var leftNode = aChildren[i]
        var rightNode = bChildren[i]
        index += 1

        if (!leftNode) {
            if (rightNode) {
                // Excess nodes in b need to be added
                apply = appendPatch(apply, new VPatch(VPatch.INSERT, null, rightNode))
            }
        } else if (!rightNode) {
            if (leftNode) {
                // Excess nodes in a need to be removed
                patch[index] = new VPatch(VPatch.REMOVE, leftNode, null)
                destroyWidgets(leftNode, patch, index)
            }
        } else {
            walk(leftNode, rightNode, patch, index)
        }

        if (isVNode(leftNode) && leftNode.count) {
            index += leftNode.count
        }
    }

    if (bChildren.moves) {
        // Reorder nodes last
        apply = appendPatch(apply, new VPatch(VPatch.ORDER, a, bChildren.moves))
    }

    return apply
}

// Patch records for all destroyed widgets must be added because we need
// a DOM node reference for the destroy function
function destroyWidgets(vNode, patch, index) {
    if (isWidget(vNode)) {
        if (typeof vNode.destroy === "function") {
            patch[index] = new VPatch(VPatch.REMOVE, vNode, null)
        }
    } else if (isVNode(vNode) && vNode.hasWidgets) {
        var children = vNode.children
        var len = children.length
        for (var i = 0; i < len; i++) {
            var child = children[i]
            index += 1

            destroyWidgets(child, patch, index)

            if (isVNode(child) && child.count) {
                index += child.count
            }
        }
    }
}

// Execute hooks when two nodes are identical
function hooks(vNode, patch, index) {
    if (isVNode(vNode)) {
        if (vNode.hooks) {
            patch[index] = new VPatch(VPatch.PROPS, vNode.hooks, vNode.hooks)
        }

        if (vNode.descendantHooks) {
            var children = vNode.children
            var len = children.length
            for (var i = 0; i < len; i++) {
                var child = children[i]
                index += 1

                hooks(child, patch, index)

                if (isVNode(child) && child.count) {
                    index += child.count
                }
            }
        }
    }
}

// List diff, naive left to right reordering
function reorder(aChildren, bChildren) {

    var bKeys = keyIndex(bChildren)

    if (!bKeys) {
        return bChildren
    }

    var aKeys = keyIndex(aChildren)

    if (!aKeys) {
        return bChildren
    }

    var bMatch = {}, aMatch = {}

    for (var key in bKeys) {
        bMatch[bKeys[key]] = aKeys[key]
    }

    for (var key in aKeys) {
        aMatch[aKeys[key]] = bKeys[key]
    }

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen
    var shuffle = []
    var freeIndex = 0
    var i = 0
    var moveIndex = 0
    var moves = shuffle.moves = {}

    while (freeIndex < len) {
        var move = aMatch[i]
        if (move !== undefined) {
            shuffle[i] = bChildren[move]
            moves[move] = moveIndex++
        } else if (i in aMatch) {
            shuffle[i] = undefined
        } else {
            while (bMatch[freeIndex] !== undefined) {
                freeIndex++
            }

            if (freeIndex < len) {
                moves[freeIndex] = moveIndex++
                shuffle[i] = bChildren[freeIndex]
                freeIndex++
            }
        }
        i++
    }

    return shuffle
}

function keyIndex(children) {
    var i, keys

    for (i = 0; i < children.length; i++) {
        var child = children[i]

        if (child.key !== undefined) {
            keys = keys || {}
            keys[child.key] = i
        }
    }

    return keys
}

function appendPatch(apply, patch) {
    if (apply) {
        if (isArray(apply)) {
            apply.push(patch)
        } else {
            apply = [apply, patch]
        }

        return apply
    } else {
        return patch
    }
}

},{"./is-vnode":29,"./is-vtext":30,"./is-widget":31,"./vpatch":33,"is-object":18,"x-is-array":19}],28:[function(require,module,exports){
module.exports = isHook

function isHook(hook) {
    return hook && typeof hook.hook === "function" &&
        !hook.hasOwnProperty("hook")
}

},{}],29:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualNode

function isVirtualNode(x) {
    if (!x) {
        return false;
    }

    return x.type === "VirtualNode" && x.version === version
}

},{"./version":32}],30:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualText

function isVirtualText(x) {
    if (!x) {
        return false;
    }

    return x.type === "VirtualText" && x.version === version
}

},{"./version":32}],31:[function(require,module,exports){
module.exports = isWidget

function isWidget(w) {
    return w && typeof w.init === "function" && typeof w.update === "function"
}

},{}],32:[function(require,module,exports){
module.exports = "1"

},{}],33:[function(require,module,exports){
var version = require("./version")

VirtualPatch.NONE = 0
VirtualPatch.VTEXT = 1
VirtualPatch.VNODE = 2
VirtualPatch.WIDGET = 3
VirtualPatch.PROPS = 4
VirtualPatch.ORDER = 5
VirtualPatch.INSERT = 6
VirtualPatch.REMOVE = 7

module.exports = VirtualPatch

function VirtualPatch(type, vNode, patch) {
    this.type = Number(type)
    this.vNode = vNode
    this.patch = patch
}

VirtualPatch.prototype.version = version.split(".")
VirtualPatch.prototype.type = "VirtualPatch"

},{"./version":32}],34:[function(require,module,exports){
module.exports=require(28)
},{}],35:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualNode

function isVirtualNode(x) {
    return x && x.type === "VirtualNode" && x.version === version
}

},{"./version":37}],36:[function(require,module,exports){
module.exports = isWidget

function isWidget(w) {
    return w && w.type === "Widget"
}

},{}],37:[function(require,module,exports){
module.exports=require(32)
},{}],38:[function(require,module,exports){
var version = require("./version")
var isVNode = require("./is-vnode")
var isWidget = require("./is-widget")
var isVHook = require("./is-vhook")

module.exports = VirtualNode

var noProperties = {}
var noChildren = []

function VirtualNode(tagName, properties, children, key, namespace) {
    this.tagName = tagName
    this.properties = properties || noProperties
    this.children = children || noChildren
    this.key = key != null ? String(key) : undefined
    this.namespace = (typeof namespace === "string") ? namespace : null

    var count = (children && children.length) || 0
    var descendants = 0
    var hasWidgets = false
    var descendantHooks = false
    var hooks

    for (var propName in properties) {
        if (properties.hasOwnProperty(propName)) {
            var property = properties[propName]
            if (isVHook(property)) {
                if (!hooks) {
                    hooks = {}
                }

                hooks[propName] = property
            }
        }
    }

    for (var i = 0; i < count; i++) {
        var child = children[i]
        if (isVNode(child)) {
            descendants += child.count || 0

            if (!hasWidgets && child.hasWidgets) {
                hasWidgets = true
            }

            if (!descendantHooks && (child.hooks || child.descendantHooks)) {
                descendantHooks = true
            }
        } else if (!hasWidgets && isWidget(child)) {
            if (typeof child.destroy === "function") {
                hasWidgets = true
            }
        }
    }

    this.count = count + descendants
    this.hasWidgets = hasWidgets
    this.hooks = hooks
    this.descendantHooks = descendantHooks
}

VirtualNode.prototype.version = version
VirtualNode.prototype.type = "VirtualNode"

},{"./is-vhook":34,"./is-vnode":35,"./is-widget":36,"./version":37}],39:[function(require,module,exports){
var version = require("./version")

module.exports = VirtualText

function VirtualText(text) {
    this.text = String(text)
}

VirtualText.prototype.version = version
VirtualText.prototype.type = "VirtualText"

},{"./version":37}],40:[function(require,module,exports){

var VNode = require('vtree/vnode');
var VText = require('vtree/vtext');
var diff = require('virtual-dom/diff');
var patch = require('virtual-dom/patch');
var createElement = require('virtual-dom/create-element');
var DataSet = require("data-set");
var Delegator = require("dom-delegator");
var isHook = require("vtree/is-vhook");

Elm.Native.Html = {};
Elm.Native.Html.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Html = elm.Native.Html || {};
    if (elm.Native.Html.values) return elm.Native.Html.values;
    if ('values' in Elm.Native.Html)
        return elm.Native.Html.values = Elm.Native.Html.values;

    // This manages event listeners. Somehow...
    var delegator = Delegator();

    var RenderUtils = ElmRuntime.use(ElmRuntime.Render.Utils);
    var newElement = Elm.Graphics.Element.make(elm).newElement;
    var Utils = Elm.Native.Utils.make(elm);
    var List = Elm.Native.List.make(elm);
    var Maybe = Elm.Maybe.make(elm);
    var eq = Elm.Native.Utils.make(elm).eq;

    function listToObject(list) {
        var object = {};
        while (list.ctor !== '[]') {
            var entry = list._0;
            object[entry.key] = entry.value;
            list = list._1;
        }
        return object;
    }

    function node(name, attributes, contents) {
        var attrs = listToObject(attributes);

        // ensure that setting text of an input does not move the cursor
        var useSoftSet =
            name === 'input'
            && 'value' in attrs
            && attrs.value !== undefined
            && !isHook(attrs.value);

        if (useSoftSet) {
            attrs.value = SoftSetHook(attrs.value);
        }

        return new VNode(name, attrs, List.toArray(contents));
    }

    function pair(key, value) {
        return {
            key: key,
            value: value
        };
    }

    function style(properties) {
        return pair('style', listToObject(properties));
    }

    function on(name, coerce) {
        function createListener(handle, convert) {
            delegator.listenTo(name);
            function eventHandler(event) {
                var value = coerce(event);
                if (value.ctor === 'Just') {
                    elm.notify(handle.id, convert(value._0));
                }
            }
            return pair(name, DataSetHook(eventHandler));
        }
        return F2(createListener);
    }

    function filterMap(f, getter) {
        return function(event) {
            var maybeValue = getter(event);
            return maybeValue.ctor === 'Nothing' ? maybeValue : f(maybeValue._0);
        };
    }
    function getMouseEvent(event) {
        return !('button' in event) ?
            Maybe.Nothing :
            Maybe.Just({
                _: {},
                button: event.button,
                altKey: event.altKey,
                ctrlKey: event.ctrlKey,
                metaKey: event.metaKey,
                shiftKey: event.shiftKey
            });
    }
    function getKeyboardEvent(event) {
        return !('keyCode' in event) ?
            Maybe.Nothing :
            Maybe.Just({
                _: {},
                keyCode: event.keyCode,
                altKey: event.altKey,
                ctrlKey: event.ctrlKey,
                metaKey: event.metaKey,
                shiftKey: event.shiftKey
            });
    }
    function getChecked(event) {
        return 'checked' in event.target ?
            Maybe.Just(event.target.checked) :
            Maybe.Nothing;
    }
    function getValue(event) {
        var node = event.target;
        return 'value' in node ?
            Maybe.Just(event.target.value) :
            Maybe.Nothing;
    }
    function getValueAndSelection(event) {
        var node = event.target;
        return !('selectionStart' in node) ?
            Maybe.Nothing :
            Maybe.Just({
                _: {},
                value: node.value,
                selection: {
                    start: node.selectionStart,
                    end: node.selectionEnd,
                    direction: {
                        ctor: node.selectionDirection === 'forward' ? 'Forward' : 'Backward'
                    }
                }
            });
    }
    function getAnything(event) {
        return Maybe.Just(Utils._Tuple0);
    }

    function DataSetHook(value) {
        if (!(this instanceof DataSetHook)) {
            return new DataSetHook(value);
        }

        this.value = value;
    }

    DataSetHook.prototype.hook = function (node, propertyName) {
        var ds = DataSet(node);
        ds[propertyName] = this.value;
    };


    function SoftSetHook(value) {
      if (!(this instanceof SoftSetHook)) {
        return new SoftSetHook(value);
      }

      this.value = value;
    }

    SoftSetHook.prototype.hook = function (node, propertyName) {
      if (node[propertyName] !== this.value) {
        node[propertyName] = this.value;
      }
    };

    function text(string) {
        return new VText(string);
    }

    function toElement(width, height, html) {
        return A3(newElement, width, height,
                  { ctor: 'Custom'
                  , type: 'evancz/elm-html'
                  , render: render
                  , update: update
                  , model: html
                  });
    }

    function render(model) {
        var element = RenderUtils.newElement('div');
        element.appendChild(createElement(model));
        return element;
    }

    function update(node, oldModel, newModel) {
        var patches = diff(oldModel, newModel);
        var newNode = patch(node.firstChild, patches)
        if (newNode !== node.firstChild) {
            node.replaceChild(newNode, node.firstChild)
        }
    }

    function lazyRef(fn, a) {
        function thunk() {
            return fn(a);
        }
        return new Thunk('ref', fn, [a], thunk, shouldUpdate_refEq);
    }

    function lazyRef2(fn, a, b) {
        function thunk() {
            return A2(fn, a, b);
        }
        return new Thunk('ref', fn, [a,b], thunk, shouldUpdate_refEq);
    }

    function lazyRef3(fn, a, b, c) {
        function thunk() {
            return A3(fn, a, b, c);
        }
        return new Thunk('ref', fn, [a,b,c], thunk, shouldUpdate_refEq);
    }

    function lazyStruct(fn, a) {
        function thunk() {
            return fn(a);
        }
        return new Thunk('struct', fn, [a], thunk, shouldUpdate_structEq);
    }

    function lazyStruct2(fn, a, b) {
        function thunk() {
            return A2(fn, a, b);
        }
        return new Thunk('struct', fn, [a,b], thunk, shouldUpdate_structEq);
    }

    function lazyStruct3(fn, a, b, c) {
        function thunk() {
            return A3(fn, a, b, c);
        }
        return new Thunk('struct', fn, [a,b,c], thunk, shouldUpdate_structEq);
    }

    function Thunk(kind, fn, args, thunk, shouldUpdate) {
        this.fn = fn;
        this.args = args;
        this.vnode = null;
        this.key = undefined;
        this.thunk = thunk;

        this.kind = kind;
        this.shouldUpdate = shouldUpdate;
    }

    Thunk.prototype.type = "immutable-thunk";
    Thunk.prototype.update = updateThunk;
    Thunk.prototype.init = initThunk;

    function shouldUpdate_refEq(current, previous) {
        if (current.kind !== previous.kind || current.fn !== previous.fn) {
            return true;
        }

        // if it's the same function, we know the number of args must match
        var cargs = current.args;
        var pargs = previous.args;

        for (var i = cargs.length; i--; ) {
            if (cargs[i] !== pargs[i]) {
                return true;
            }
        }

        return false;
    }

    function shouldUpdate_structEq(current, previous) {
        if (current.kind !== previous.kind || current.fn !== previous.fn) {
            return true;
        }

        // if it's the same function, we know the number of args must match
        var cargs = current.args;
        var pargs = previous.args;

        for (var i = cargs.length; i--; ) {
            if (eq(cargs[i], pargs[i])) {
                return true;
            }
        }

        return false;
    }

    function updateThunk(previous, domNode) {
        if (!this.shouldUpdate(this, previous)) {
            this.vnode = previous.vnode;
            return;
        }

        if (!this.vnode) {
            this.vnode = this.thunk();
        }

        var patches = diff(previous.vnode, this.vnode);
        patch(domNode, patches);
    }

    function initThunk() {
        this.vnode = this.thunk();
        return createElement(this.vnode);
    }

    return Elm.Native.Html.values = {
        node: F3(node),
        text: text,
        style: style,
        on: F2(on),

        pair: F2(pair),

        getMouseEvent: getMouseEvent,
        getKeyboardEvent: getKeyboardEvent,
        getChecked: getChecked,
        getValue: getValue,
        getValueAndSelection: getValueAndSelection,
        getAnything: getAnything,
        filterMap: F2(filterMap),

        lazyRef : F2(lazyRef ),
        lazyRef2: F3(lazyRef2),
        lazyRef3: F4(lazyRef3),
        lazyStruct : F2(lazyStruct ),
        lazyStruct2: F3(lazyStruct2),
        lazyStruct3: F4(lazyStruct3),
        toElement: F3(toElement)
    };
};

},{"data-set":2,"dom-delegator":8,"virtual-dom/create-element":15,"virtual-dom/diff":16,"virtual-dom/patch":20,"vtree/is-vhook":34,"vtree/vnode":38,"vtree/vtext":39}],41:[function(require,module,exports){

},{}]},{},[40]);
Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values)
   return _elm.Main.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Main",
   $App = Elm.App.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Keys = Elm.Keys.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm),
   $Native$Ports = Elm.Native.Ports.make(_elm),
   $Outline$Entry = Elm.Outline.Entry.make(_elm),
   $SampleData = Elm.SampleData.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var dropboxIn = $Native$Ports.portIn("dropboxIn",
   $Native$Ports.incomingSignal(function (v) {
      return typeof v === "string" || typeof v === "object" && v instanceof String ? v : _E.raise("invalid input, expecting JSString but got " + v);
   }));
   var metaIn = $Native$Ports.portIn("metaIn",
   $Native$Ports.incomingSignal(function (v) {
      return typeof v === "number" ? v : _E.raise("invalid input, expecting JSNumber but got " + v);
   }));
   var downsIn = $Native$Ports.portIn("downsIn",
   $Native$Ports.incomingSignal(function (v) {
      return typeof v === "number" ? v : _E.raise("invalid input, expecting JSNumber but got " + v);
   }));
   var pressesIn = $Native$Ports.portIn("pressesIn",
   $Native$Ports.incomingSignal(function (v) {
      return typeof v === "string" || typeof v === "object" && v instanceof String ? v : _E.raise("invalid input, expecting JSString but got " + v);
   }));
   var commands = $Signal.merges(_L.fromArray([A2($Signal._op["<~"],
                                              $App.Key,
                                              A2($Signal._op["<~"],
                                              $Keys.fromPresses,
                                              pressesIn))
                                              ,A2($Signal._op["<~"],
                                              $App.Key,
                                              A2($Signal._op["<~"],
                                              $Keys.fromDowns,
                                              downsIn))
                                              ,A2($Signal._op["<~"],
                                              $App.Key,
                                              A2($Signal._op["<~"],
                                              $Keys.fromMeta,
                                              metaIn))
                                              ,A2($Signal._op["<~"],
                                              $App.Loaded,
                                              dropboxIn)]));
   var state = A3($Signal.foldp,
   $App.step,
   A2($App.Model,
   $SampleData.template,
   $Outline$Entry.InText(4)),
   commands);
   var main = A2($Signal._op["<~"],
   A2($Html.toElement,800,600),
   A2($Signal._op["<~"],
   $App.render,
   state));
   var dropboxOut = $Native$Ports.portOut("dropboxOut",
   $Native$Ports.outgoingSignal(function (v) {
      return v;
   }),
   $Signal.dropRepeats(A2($Signal._op["<~"],
   function (x) {
      return $Outline$Entry.toJson(x.value);
   },
   state)));
   _elm.Main.values = {_op: _op
                      ,commands: commands
                      ,state: state
                      ,main: main};
   return _elm.Main.values;
};Elm.App = Elm.App || {};
Elm.App.make = function (_elm) {
   "use strict";
   _elm.App = _elm.App || {};
   if (_elm.App.values)
   return _elm.App.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "App",
   $Basics = Elm.Basics.make(_elm),
   $Core$Action = Elm.Core.Action.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Decoder = Elm.Json.Decoder.make(_elm),
   $Json$Output = Elm.Json.Output.make(_elm),
   $Json$Process = Elm.Json.Process.make(_elm),
   $Keys = Elm.Keys.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Outline$Entry = Elm.Outline.Entry.make(_elm);
   var renderDocs = A3($Html.node,
   "div",
   _L.fromArray([]),
   _L.fromArray([A3($Html.node,
                "p",
                _L.fromArray([]),
                _L.fromArray([$Html.text("Cmd-A: add to inbox")]))
                ,A3($Html.node,
                "p",
                _L.fromArray([]),
                _L.fromArray([$Html.text("Cmd-D: delete")]))
                ,A3($Html.node,
                "p",
                _L.fromArray([]),
                _L.fromArray([$Html.text("Cmd-P: promote from inbox")]))]));
   var renderDocument = F2(function (value,
   cursor) {
      return A2($Outline$Entry.render,
      value,
      $Maybe.Just(A2($Debug.watch,
      "cursor",
      cursor)));
   });
   var render = function (m) {
      return A3($Html.node,
      "div",
      _L.fromArray([]),
      _L.fromArray([renderDocs
                   ,A2(renderDocument,
                   m.value,
                   m.selection)]));
   };
   var Loaded = function (a) {
      return {ctor: "Loaded"
             ,_0: a};
   };
   var KeyMeta = function (a) {
      return {ctor: "KeyMeta"
             ,_0: a};
   };
   var Key = function (a) {
      return {ctor: "Key",_0: a};
   };
   var updateModel = F2(function (action,
   _v0) {
      return function () {
         return function () {
            var _v2 = A2(action,
            _v0.value,
            _v0.selection);
            switch (_v2.ctor)
            {case "Delete": return {_: {}
                                   ,selection: _v0.selection
                                   ,value: _v0.value};
               case "EnterNext": return {_: {}
                                        ,selection: _v0.selection
                                        ,value: _v0.value};
               case "EnterPrev": return {_: {}
                                        ,selection: _v0.selection
                                        ,value: _v0.value};
               case "NoChange": return {_: {}
                                       ,selection: _v0.selection
                                       ,value: _v0.value};
               case "Split": return {_: {}
                                    ,selection: _v0.selection
                                    ,value: _v0.value};
               case "Update": return {_: {}
                                     ,selection: _v2._1
                                     ,value: _v2._0};}
            _E.Case($moduleName,
            "between lines 24 and 31");
         }();
      }();
   });
   var step = F2(function (c,m) {
      return function () {
         switch (c.ctor)
         {case "Key": switch (c._0.ctor)
              {case "Backspace":
                 return A2(updateModel,
                   $Outline$Entry.backspace,
                   m);
                 case "Character":
                 return A2(updateModel,
                   $Outline$Entry.insert(c._0._0),
                   m);
                 case "Command": switch (c._0._0)
                   {case "a":
                      return A2(updateModel,
                        $Outline$Entry.addInboxItem,
                        m);
                      case "d": return A2(updateModel,
                        $Outline$Entry.$delete,
                        m);
                      case "p": return A2(updateModel,
                        $Outline$Entry.promote,
                        m);}
                   break;
                 case "Down":
                 return A2(updateModel,
                   $Outline$Entry.goNext,
                   m);
                 case "Enter":
                 return A2(updateModel,
                   $Outline$Entry.enter,
                   m);
                 case "Left":
                 return A2(updateModel,
                   $Outline$Entry.goLeft,
                   m);
                 case "Right":
                 return A2(updateModel,
                   $Outline$Entry.goRight,
                   m);
                 case "Up":
                 return A2(updateModel,
                   $Outline$Entry.goPrev,
                   m);}
              break;
            case "Loaded":
            return function () {
                 var _v13 = A2($Json$Process.into,
                 $Json$Decoder.fromString(c._0),
                 $Outline$Entry.decoder);
                 switch (_v13.ctor)
                 {case "Success": return {_: {}
                                         ,selection: $Outline$Entry.InText(0)
                                         ,value: _v13._0};}
                 return $Basics.fst({ctor: "_Tuple2"
                                    ,_0: m
                                    ,_1: A2($Debug.log,
                                    "Load failed",
                                    _v13)});
              }();}
         return $Basics.fst({ctor: "_Tuple2"
                            ,_0: m
                            ,_1: A2($Debug.log,
                            "Unhandled command",
                            c)});
      }();
   });
   var Model = F2(function (a,b) {
      return {_: {}
             ,selection: b
             ,value: a};
   });
   _elm.App.values = {_op: _op
                     ,Model: Model
                     ,updateModel: updateModel
                     ,Key: Key
                     ,KeyMeta: KeyMeta
                     ,Loaded: Loaded
                     ,step: step
                     ,renderDocument: renderDocument
                     ,renderDocs: renderDocs
                     ,render: render};
   return _elm.App.values;
};Elm.SampleData = Elm.SampleData || {};
Elm.SampleData.make = function (_elm) {
   "use strict";
   _elm.SampleData = _elm.SampleData || {};
   if (_elm.SampleData.values)
   return _elm.SampleData.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "SampleData",
   $Outline$Entry = Elm.Outline.Entry.make(_elm);
   var template = $Outline$Entry.Entry({_: {}
                                       ,children: _L.fromArray([$Outline$Entry.Entry({_: {}
                                                                                     ,children: _L.fromArray([$Outline$Entry.Entry({_: {}
                                                                                                                                   ,children: _L.fromArray([])
                                                                                                                                   ,description: ""
                                                                                                                                   ,inbox: _L.fromArray([])
                                                                                                                                   ,text: "daily"})
                                                                                                             ,$Outline$Entry.Entry({_: {}
                                                                                                                                   ,children: _L.fromArray([])
                                                                                                                                   ,description: ""
                                                                                                                                   ,inbox: _L.fromArray([])
                                                                                                                                   ,text: "weekly"})
                                                                                                             ,$Outline$Entry.Entry({_: {}
                                                                                                                                   ,children: _L.fromArray([])
                                                                                                                                   ,description: ""
                                                                                                                                   ,inbox: _L.fromArray([])
                                                                                                                                   ,text: "waiting on"})
                                                                                                             ,$Outline$Entry.Entry({_: {}
                                                                                                                                   ,children: _L.fromArray([])
                                                                                                                                   ,description: ""
                                                                                                                                   ,inbox: _L.fromArray([])
                                                                                                                                   ,text: "monthly"})
                                                                                                             ,$Outline$Entry.Entry({_: {}
                                                                                                                                   ,children: _L.fromArray([])
                                                                                                                                   ,description: ""
                                                                                                                                   ,inbox: _L.fromArray([])
                                                                                                                                   ,text: "yearly, etc."})])
                                                                                     ,description: ""
                                                                                     ,inbox: _L.fromArray([])
                                                                                     ,text: "By time (deadline)"})
                                                               ,$Outline$Entry.Entry({_: {}
                                                                                     ,children: _L.fromArray([$Outline$Entry.Entry({_: {}
                                                                                                                                   ,children: _L.fromArray([])
                                                                                                                                   ,description: ""
                                                                                                                                   ,inbox: _L.fromArray([])
                                                                                                                                   ,text: "daily"})
                                                                                                             ,$Outline$Entry.Entry({_: {}
                                                                                                                                   ,children: _L.fromArray([])
                                                                                                                                   ,description: ""
                                                                                                                                   ,inbox: _L.fromArray([])
                                                                                                                                   ,text: "weekly"})
                                                                                                             ,$Outline$Entry.Entry({_: {}
                                                                                                                                   ,children: _L.fromArray([])
                                                                                                                                   ,description: ""
                                                                                                                                   ,inbox: _L.fromArray([])
                                                                                                                                   ,text: "monthly"})])
                                                                                     ,description: ""
                                                                                     ,inbox: _L.fromArray([])
                                                                                     ,text: "Habits"})
                                                               ,$Outline$Entry.Entry({_: {}
                                                                                     ,children: _L.fromArray([])
                                                                                     ,description: ""
                                                                                     ,inbox: _L.fromArray([])
                                                                                     ,text: "By priorty"})
                                                               ,$Outline$Entry.Entry({_: {}
                                                                                     ,children: _L.fromArray([])
                                                                                     ,description: ""
                                                                                     ,inbox: _L.fromArray([])
                                                                                     ,text: "By project"})])
                                       ,description: ""
                                       ,inbox: _L.fromArray(["sdfs"
                                                            ,"sdfsd"])
                                       ,text: "Tasks (LOADING...)"});
   _elm.SampleData.values = {_op: _op
                            ,template: template};
   return _elm.SampleData.values;
};Elm.Outline = Elm.Outline || {};
Elm.Outline.Entry = Elm.Outline.Entry || {};
Elm.Outline.Entry.make = function (_elm) {
   "use strict";
   _elm.Outline = _elm.Outline || {};
   _elm.Outline.Entry = _elm.Outline.Entry || {};
   if (_elm.Outline.Entry.values)
   return _elm.Outline.Entry.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Outline.Entry",
   $Basics = Elm.Basics.make(_elm),
   $Core$Action = Elm.Core.Action.make(_elm),
   $Core$Array = Elm.Core.Array.make(_elm),
   $Core$String = Elm.Core.String.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Decoder = Elm.Json.Decoder.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $String = Elm.String.make(_elm);
   var toJson = function (entry) {
      return function () {
         switch (entry.ctor)
         {case "Entry":
            return _L.append("{\"text\":",
              _L.append($Core$String.toJson(entry._0.text),
              _L.append(",\"description\":",
              _L.append($Core$String.toJson(entry._0.description),
              _L.append(",\"inbox\":",
              _L.append(A2($Core$Array.toJson,
              $Core$String.toJson,
              entry._0.inbox),
              _L.append(",\"children\":",
              _L.append(A2($Core$Array.toJson,
              toJson,
              entry._0.children),
              "}"))))))));}
         _E.Case($moduleName,
         "between lines 149 and 154");
      }();
   };
   var toChildrenCursor = function (mc) {
      return function () {
         switch (mc.ctor)
         {case "Just":
            switch (mc._0.ctor)
              {case "InChild":
                 switch (mc._0._0.ctor)
                   {case "_Tuple2":
                      return $Maybe.Just(A2($Core$Array.cursor,
                        mc._0._0._0,
                        mc._0._0._1));}
                   break;}
              break;}
         return $Maybe.Nothing;
      }();
   };
   var toInboxCursor = function (mc) {
      return function () {
         switch (mc.ctor)
         {case "Just":
            switch (mc._0.ctor)
              {case "InInbox":
                 switch (mc._0._0.ctor)
                   {case "_Tuple2":
                      return $Maybe.Just(A2($Core$Array.cursor,
                        mc._0._0._0,
                        mc._0._0._1));}
                   break;}
              break;}
         return $Maybe.Nothing;
      }();
   };
   var toDescriptionCursor = function (mc) {
      return function () {
         switch (mc.ctor)
         {case "Just":
            switch (mc._0.ctor)
              {case "InDescription":
                 return $Maybe.Just(mc._0._0);}
              break;}
         return $Maybe.Nothing;
      }();
   };
   var toTextCursor = function (mc) {
      return function () {
         switch (mc.ctor)
         {case "Just":
            switch (mc._0.ctor)
              {case "InText":
                 return $Maybe.Just(mc._0._0);}
              break;}
         return $Maybe.Nothing;
      }();
   };
   var render = F2(function (value,
   mc) {
      return function () {
         switch (value.ctor)
         {case "Entry":
            return A3($Html.node,
              "li",
              _L.fromArray([]),
              _L.fromArray([A2($Core$String.render,
                           value._0.text,
                           toTextCursor(mc))
                           ,A3($Html.node,
                           "i",
                           _L.fromArray([]),
                           _L.fromArray([A2($Core$String.render,
                           value._0.description,
                           toDescriptionCursor(mc))]))
                           ,A2($Html.node,
                           "ol",
                           _L.fromArray([]))($List.map(function (x) {
                              return A3($Html.node,
                              "li",
                              _L.fromArray([]),
                              _L.fromArray([x]));
                           })(A3($Core$Array.render,
                           $Core$String.render,
                           value._0.inbox,
                           toInboxCursor(mc))))
                           ,A2($Html.node,
                           "ul",
                           _L.fromArray([]))(A3($Core$Array.render,
                           render,
                           value._0.children,
                           toChildrenCursor(mc)))]));}
         _E.Case($moduleName,
         "between lines 138 and 144");
      }();
   });
   var InChild = function (a) {
      return {ctor: "InChild"
             ,_0: a};
   };
   var InInbox = function (a) {
      return {ctor: "InInbox"
             ,_0: a};
   };
   var InDescription = function (a) {
      return {ctor: "InDescription"
             ,_0: a};
   };
   var InText = function (a) {
      return {ctor: "InText"
             ,_0: a};
   };
   var findLastCursor = function (en) {
      return function () {
         switch (en.ctor)
         {case "Entry":
            return _U.cmp($List.length(en._0.children),
              0) > 0 ? InChild({ctor: "_Tuple2"
                               ,_0: -1 + $List.length(en._0.children)
                               ,_1: InText(0)}) : _U.cmp($List.length(en._0.inbox),
              0) > 0 ? InInbox({ctor: "_Tuple2"
                               ,_0: -1 + $List.length(en._0.inbox)
                               ,_1: 0}) : InText(0);}
         _E.Case($moduleName,
         "between lines 40 and 44");
      }();
   };
   var Entry = function (a) {
      return {ctor: "Entry",_0: a};
   };
   var entry = F4(function (t,
   d,
   i,
   c) {
      return Entry({_: {}
                   ,children: c
                   ,description: d
                   ,inbox: i
                   ,text: t});
   });
   var addInboxItem_ = F2(function (en,
   cur) {
      return function () {
         switch (en.ctor)
         {case "Entry":
            return A2($Core$Action.Update,
              Entry(_U.replace([["inbox"
                                ,_L.append(_L.fromArray([""]),
                                en._0.inbox)]],
              en._0)),
              InInbox({ctor: "_Tuple2"
                      ,_0: 0
                      ,_1: 0}));}
         _E.Case($moduleName,
         "between lines 47 and 48");
      }();
   });
   var promote_ = F2(function (en,
   cur) {
      return function () {
         switch (en.ctor)
         {case "Entry":
            return function () {
                 switch (cur.ctor)
                 {case "InInbox":
                    switch (cur._0.ctor)
                      {case "_Tuple2":
                         return A2($Core$Action.Update,
                           Entry(_U.replace([["inbox"
                                             ,_L.append(A2($List.take,
                                             cur._0._0,
                                             en._0.inbox),
                                             A2($List.drop,
                                             cur._0._0 + 1,
                                             en._0.inbox))]
                                            ,["children"
                                             ,A2($List._op["::"],
                                             A4(entry,
                                             $List.head(A2($List.drop,
                                             cur._0._0,
                                             en._0.inbox)),
                                             "",
                                             _L.fromArray([]),
                                             _L.fromArray([])),
                                             en._0.children)]],
                           en._0)),
                           function () {
                              var newI = A2($Basics.min,
                              cur._0._0,
                              $List.length(en._0.inbox) - 2);
                              return _U.cmp(newI,
                              0) > -1 ? InInbox({ctor: "_Tuple2"
                                                ,_0: newI
                                                ,_1: cur._0._1}) : InChild({ctor: "_Tuple2"
                                                                           ,_0: 0
                                                                           ,_1: InText(cur._0._1)});
                           }());}
                      break;}
                 return $Core$Action.NoChange;
              }();}
         _E.Case($moduleName,
         "between lines 53 and 58");
      }();
   });
   var doEntry = F3(function (action,
   en,
   cur) {
      return function () {
         switch (en.ctor)
         {case "Entry":
            return function () {
                 switch (cur.ctor)
                 {case "InChild":
                    return function () {
                         var _v34 = A5($Core$Array.$do,
                         InText(0),
                         findLastCursor,
                         doEntry(action),
                         en._0.children,
                         cur._0);
                         switch (_v34.ctor)
                         {case "NoChange":
                            return $Core$Action.NoChange;
                            case "Update":
                            return A2($Core$Action.Update,
                              Entry(_U.replace([["children"
                                                ,_v34._0]],
                              en._0)),
                              InChild(_v34._1));}
                         _E.Case($moduleName,
                         "between lines 64 and 67");
                      }();}
                 return A2(action,en,cur);
              }();}
         _E.Case($moduleName,
         "between lines 63 and 67");
      }();
   });
   var addInboxItem = doEntry(addInboxItem_);
   var promote = doEntry(promote_);
   var $do = F3(function (stringAction,
   en,
   cur) {
      return function () {
         switch (en.ctor)
         {case "Entry":
            return function () {
                 switch (cur.ctor)
                 {case "InChild":
                    return function () {
                         var _v44 = A5($Core$Array.$do,
                         InText(0),
                         findLastCursor,
                         $do(stringAction),
                         en._0.children,
                         cur._0);
                         switch (_v44.ctor)
                         {case "Delete":
                            return A2($Core$Action.Update,
                              Entry(_U.replace([["children"
                                                ,_L.fromArray([])]],
                              en._0)),
                              InText($String.length(en._0.text)));
                            case "EnterNext":
                            return $Core$Action.EnterNext;
                            case "EnterPrev":
                            return $Core$Action.Update(en)(_U.cmp($List.length(en._0.inbox),
                              0) > 0 ? InInbox({ctor: "_Tuple2"
                                               ,_0: -1 + $List.length(en._0.inbox)
                                               ,_1: 0}) : InText(0));
                            case "NoChange":
                            return $Core$Action.NoChange;
                            case "Update":
                            return A2($Core$Action.Update,
                              Entry(_U.replace([["children"
                                                ,_v44._0]],
                              en._0)),
                              InChild(_v44._1));}
                         _E.Case($moduleName,
                         "between lines 95 and 102");
                      }();
                    case "InDescription":
                    return function () {
                         var _v47 = A2(stringAction,
                         en._0.description,
                         cur._0);
                         switch (_v47.ctor)
                         {case "Delete":
                            return $Core$Action.Delete;
                            case "NoChange":
                            return $Core$Action.NoChange;
                            case "Update":
                            return A2($Core$Action.Update,
                              Entry(_U.replace([["description"
                                                ,_v47._0]],
                              en._0)),
                              InDescription(_v47._1));}
                         _E.Case($moduleName,
                         "between lines 83 and 87");
                      }();
                    case "InInbox":
                    return function () {
                         var _v50 = A5($Core$Array.$do,
                         0,
                         function (_v53) {
                            return function () {
                               return 0;
                            }();
                         },
                         stringAction,
                         en._0.inbox,
                         cur._0);
                         switch (_v50.ctor)
                         {case "Delete":
                            return A2($Core$Action.Update,
                              Entry(_U.replace([["inbox"
                                                ,_L.fromArray([])]],
                              en._0)),
                              InText($String.length(en._0.text)));
                            case "EnterNext":
                            return _U.cmp($List.length(en._0.children),
                              0) > 0 ? $Core$Action.Update(en)(InChild({ctor: "_Tuple2"
                                                                       ,_0: 0
                                                                       ,_1: InText(0)})) : $Core$Action.EnterNext;
                            case "EnterPrev":
                            return $Core$Action.Update(en)(InText(0));
                            case "NoChange":
                            return $Core$Action.NoChange;
                            case "Update":
                            return A2($Core$Action.Update,
                              Entry(_U.replace([["inbox"
                                                ,_v50._0]],
                              en._0)),
                              InInbox(_v50._1));}
                         _E.Case($moduleName,
                         "between lines 87 and 95");
                      }();
                    case "InText":
                    return function () {
                         var _v55 = A2(stringAction,
                         en._0.text,
                         cur._0);
                         switch (_v55.ctor)
                         {case "Delete":
                            return $Core$Action.Delete;
                            case "EnterNext":
                            return _U.cmp($List.length(en._0.inbox),
                              0) > 0 ? $Core$Action.Update(en)(InInbox({ctor: "_Tuple2"
                                                                       ,_0: 0
                                                                       ,_1: 0})) : _U.cmp($List.length(en._0.children),
                              0) > 0 ? $Core$Action.Update(en)(InChild({ctor: "_Tuple2"
                                                                       ,_0: 0
                                                                       ,_1: InText(cur._0)})) : $Core$Action.EnterNext;
                            case "EnterPrev":
                            return $Core$Action.EnterPrev;
                            case "NoChange":
                            return $Core$Action.NoChange;
                            case "Split":
                            switch (_v55._0.ctor)
                              {case "::":
                                 switch (_v55._0._1.ctor)
                                   {case "::":
                                      switch (_v55._0._1._1.ctor)
                                        {case "[]":
                                           return A3($Core$Action.Split,
                                             _L.fromArray([A4(entry,
                                                          _v55._0._0,
                                                          "",
                                                          _L.fromArray([]),
                                                          _L.fromArray([]))
                                                          ,Entry(_U.replace([["text"
                                                                             ,_v55._0._1._0]],
                                                          en._0))]),
                                             _v55._1,
                                             InText(_v55._2));}
                                        return $Debug.crash("Not yet implemented for splits > 2");}
                                   break;}
                              return $Debug.crash("Split has less than two children");
                            case "Update":
                            return A2($Core$Action.Update,
                              Entry(_U.replace([["text"
                                                ,_v55._0]],
                              en._0)),
                              InText(_v55._1));}
                         _E.Case($moduleName,
                         "between lines 71 and 83");
                      }();}
                 _E.Case($moduleName,
                 "between lines 70 and 102");
              }();}
         _E.Case($moduleName,
         "between lines 70 and 102");
      }();
   });
   var enter = $do($Core$String.split);
   var goLeft = $do($Core$String.goLeft);
   var goRight = $do($Core$String.goRight);
   var backspace = $do($Core$String.backspace);
   var $delete = $do($Core$String.$delete);
   var insert = function (s) {
      return $do($Core$String.insert(s));
   };
   var goNext = $do($Core$Action.always($Core$Action.EnterNext));
   var goPrev = $do($Core$Action.always($Core$Action.EnterPrev));
   var decoder = function (a) {
      return A6($Json$Decoder.decode4,
      A2($Json$Decoder._op[":="],
      "text",
      $Json$Decoder.string),
      A2($Json$Decoder._op[":="],
      "description",
      $Json$Decoder.string),
      A2($Json$Decoder._op[":="],
      "inbox",
      $Json$Decoder.listOf($Json$Decoder.string)),
      A2($Json$Decoder._op[":="],
      "children",
      $Json$Decoder.listOf(decoder)),
      F4(function (t,d,i,c) {
         return Entry({_: {}
                      ,children: c
                      ,description: d
                      ,inbox: i
                      ,text: t});
      }),
      a);
   };
   _elm.Outline.Entry.values = {_op: _op
                               ,entry: entry
                               ,insert: insert
                               ,backspace: backspace
                               ,enter: enter
                               ,addInboxItem: addInboxItem
                               ,promote: promote
                               ,$delete: $delete
                               ,goLeft: goLeft
                               ,goRight: goRight
                               ,goNext: goNext
                               ,goPrev: goPrev
                               ,render: render
                               ,decoder: decoder
                               ,toJson: toJson
                               ,Entry: Entry
                               ,InText: InText
                               ,InDescription: InDescription
                               ,InInbox: InInbox
                               ,InChild: InChild};
   return _elm.Outline.Entry.values;
};Elm.Core = Elm.Core || {};
Elm.Core.String = Elm.Core.String || {};
Elm.Core.String.make = function (_elm) {
   "use strict";
   _elm.Core = _elm.Core || {};
   _elm.Core.String = _elm.Core.String || {};
   if (_elm.Core.String.values)
   return _elm.Core.String.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Core.String",
   $Basics = Elm.Basics.make(_elm),
   $Core$Action = Elm.Core.Action.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Regex = Elm.Regex.make(_elm),
   $String = Elm.String.make(_elm);
   var quoteNewline = A3($Regex.replace,
   $Regex.All,
   $Regex.regex("\n"),
   function (_v0) {
      return function () {
         return "\\n";
      }();
   });
   var quoteQuote = A3($Regex.replace,
   $Regex.All,
   $Regex.regex("\""),
   function (_v2) {
      return function () {
         return "&quot;";
      }();
   });
   var quote = function (s) {
      return quoteNewline(quoteQuote(s));
   };
   var walk = F2(function (fn,
   _v4) {
      return function () {
         return fn;
      }();
   });
   var toJson = A2(walk,
   function (s) {
      return _L.append("\"",
      _L.append(quote(s),"\""));
   },
   {_: {}});
   var render = F2(function (value,
   msel) {
      return function () {
         switch (msel.ctor)
         {case "Just":
            return A3($Html.node,
              "span",
              _L.fromArray([]),
              _L.fromArray([$Html.text(A2($String.left,
                           msel._0,
                           value))
                           ,A3($Html.node,
                           "span",
                           _L.fromArray([$Html$Attributes.$class("cursor")]),
                           _L.fromArray([$Html.text("^")]))
                           ,$Html.text(A2($String.dropLeft,
                           msel._0,
                           value))]));
            case "Nothing":
            return A3($Html.node,
              "span",
              _L.fromArray([]),
              _L.fromArray([$Html.text(value)]));}
         _E.Case($moduleName,
         "between lines 39 and 44");
      }();
   });
   var split = F2(function (s,n) {
      return A3($Core$Action.Split,
      _L.fromArray([A2($String.left,
                   n,
                   s)
                   ,A2($String.dropLeft,n,s)]),
      1,
      0);
   });
   var $delete = $Core$Action.always($Core$Action.Delete);
   var goRight = $Core$Action.nav(F2(function (v,
   c) {
      return A2($Basics.min,
      $String.length(v),
      c + 1);
   }));
   var goLeft = $Core$Action.nav(F2(function (_v8,
   c) {
      return function () {
         return _U.cmp(c,
         0) > 0 ? c - 1 : c;
      }();
   }));
   var backspace = F2(function (v,
   c) {
      return function () {
         var _v10 = {ctor: "_Tuple2"
                    ,_0: v
                    ,_1: c};
         switch (_v10.ctor)
         {case "_Tuple2":
            switch (_v10._1)
              {case 0:
                 return $Core$Action.NoChange;}
              break;}
         return A2($Core$Action.Update,
         _L.append(A2($String.left,
         c - 1,
         v),
         A2($String.dropLeft,c,v)),
         c - 1);
      }();
   });
   var move = F3(function ($char,
   value,
   cursor) {
      return cursor + $String.length($char);
   });
   var update = F3(function ($char,
   value,
   cursor) {
      return _L.append(A2($String.left,
      cursor,
      value),
      _L.append($char,
      A2($String.dropLeft,
      cursor,
      value)));
   });
   var insert = F3(function (s,
   v,
   c) {
      return A2($Core$Action.Update,
      A3(update,s,v,c),
      A3(move,s,v,c));
   });
   var Subs = {_: {}};
   _elm.Core.String.values = {_op: _op
                             ,insert: insert
                             ,backspace: backspace
                             ,goLeft: goLeft
                             ,goRight: goRight
                             ,$delete: $delete
                             ,split: split
                             ,render: render
                             ,toJson: toJson};
   return _elm.Core.String.values;
};Elm.Core = Elm.Core || {};
Elm.Core.Array = Elm.Core.Array || {};
Elm.Core.Array.make = function (_elm) {
   "use strict";
   _elm.Core = _elm.Core || {};
   _elm.Core.Array = _elm.Core.Array || {};
   if (_elm.Core.Array.values)
   return _elm.Core.Array.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Core.Array",
   $Basics = Elm.Basics.make(_elm),
   $Core$Action = Elm.Core.Action.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm);
   var walk = F3(function (wrapFn,
   _v0,
   list) {
      return function () {
         return wrapFn(A2($List.map,
         _v0.child,
         list));
      }();
   });
   var toJson = function (fn) {
      return A2(walk,
      function (vs) {
         return _L.append("[",
         _L.append(A2($List.join,",",vs),
         "]"));
      },
      {_: {},child: fn});
   };
   var render = F3(function (fn,
   list,
   msel) {
      return function () {
         switch (msel.ctor)
         {case "Just":
            switch (msel._0.ctor)
              {case "_Tuple2":
                 return A2($List.indexedMap,
                   F2(function (i,x) {
                      return A2(fn,
                      x,
                      _U.eq(i,
                      msel._0._0) ? $Maybe.Just(msel._0._1) : $Maybe.Nothing);
                   }),
                   list);}
              break;
            case "Nothing":
            return A2($List.map,
              function (x) {
                 return A2(fn,
                 x,
                 $Maybe.Nothing);
              },
              list);}
         _E.Case($moduleName,
         "between lines 43 and 45");
      }();
   });
   var split_ = F3(function (fn,
   v,
   c) {
      return function () {
         var _v6 = A2(fn,v,c);
         switch (_v6.ctor)
         {case "_Tuple3":
            return A3($Core$Action.Split,
              _L.fromArray([_v6._0,_v6._1]),
              1,
              _v6._2);}
         _E.Case($moduleName,
         "between lines 36 and 37");
      }();
   });
   var at = F2(function (i,list) {
      return $List.head($List.drop(i)(list));
   });
   var replaceAt = F3(function (a,
   index,
   list) {
      return A2($List.indexedMap,
      F2(function (i,item) {
         return _U.eq(i,
         index) ? a : item;
      }),
      list);
   });
   var $do = F5(function (nextCursor,
   prevCursor,
   action,
   vs,
   _v10) {
      return function () {
         switch (_v10.ctor)
         {case "_Tuple2":
            return function () {
                 var _v14 = A2(action,
                 A2(at,_v10._0,vs),
                 _v10._1);
                 switch (_v14.ctor)
                 {case "Delete":
                    return _U.cmp($List.length(vs),
                      1) > 0 ? A2($Core$Action.Update,
                      _L.append(A2($List.take,
                      _v10._0,
                      vs),
                      A2($List.drop,_v10._0 + 1,vs)),
                      {ctor: "_Tuple2"
                      ,_0: $Basics.min(_v10._0)(-2 + $List.length(vs))
                      ,_1: _v10._1}) : $Core$Action.Delete;
                    case "EnterNext":
                    return _U.cmp($List.length(vs),
                      _v10._0 + 1) > 0 ? A2($Core$Action.Update,
                      vs,
                      {ctor: "_Tuple2"
                      ,_0: _v10._0 + 1
                      ,_1: nextCursor}) : $Core$Action.EnterNext;
                    case "EnterPrev":
                    return _U.cmp(_v10._0,
                      0) > 0 ? A2($Core$Action.Update,
                      vs,
                      {ctor: "_Tuple2"
                      ,_0: _v10._0 - 1
                      ,_1: prevCursor(A2(at,
                      _v10._0 - 1,
                      vs))}) : $Core$Action.EnterPrev;
                    case "NoChange":
                    return $Core$Action.NoChange;
                    case "Split":
                    return A2($Core$Action.Update,
                      _L.append(A2($List.take,
                      _v10._0,
                      vs),
                      _L.append(_v14._0,
                      A2($List.drop,_v10._0 + 1,vs))),
                      {ctor: "_Tuple2"
                      ,_0: _v14._1 + _v10._0
                      ,_1: _v14._2});
                    case "Update":
                    return A2($Core$Action.Update,
                      A3(replaceAt,
                      _v14._0,
                      _v10._0,
                      vs),
                      {ctor: "_Tuple2"
                      ,_0: _v10._0
                      ,_1: _v14._1});}
                 _E.Case($moduleName,
                 "between lines 21 and 33");
              }();}
         _E.Case($moduleName,
         "between lines 21 and 33");
      }();
   });
   var split = F3(function (nextCursor,
   prevCursor,
   fn) {
      return A3($do,
      nextCursor,
      prevCursor,
      split_(fn));
   });
   var cursor = F2(function (n,c) {
      return {ctor: "_Tuple2"
             ,_0: n
             ,_1: c};
   });
   var Subs = function (a) {
      return {_: {},child: a};
   };
   _elm.Core.Array.values = {_op: _op
                            ,cursor: cursor
                            ,$do: $do
                            ,split: split
                            ,render: render
                            ,toJson: toJson};
   return _elm.Core.Array.values;
};Elm.Core = Elm.Core || {};
Elm.Core.Action = Elm.Core.Action || {};
Elm.Core.Action.make = function (_elm) {
   "use strict";
   _elm.Core = _elm.Core || {};
   _elm.Core.Action = _elm.Core.Action || {};
   if (_elm.Core.Action.values)
   return _elm.Core.Action.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Core.Action";
   var always = F3(function (r,
   _v0,
   _v1) {
      return function () {
         return function () {
            return r;
         }();
      }();
   });
   var NoChange = {ctor: "NoChange"};
   var EnterNext = {ctor: "EnterNext"};
   var EnterPrev = {ctor: "EnterPrev"};
   var Delete = {ctor: "Delete"};
   var Split = F3(function (a,
   b,
   c) {
      return {ctor: "Split"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var Update = F2(function (a,b) {
      return {ctor: "Update"
             ,_0: a
             ,_1: b};
   });
   var nav = F3(function (fn,v,c) {
      return A2(Update,
      v,
      A2(fn,v,c));
   });
   var change = F3(function (fn,
   v,
   c) {
      return A2(Update,
      A2(fn,v,c),
      c);
   });
   _elm.Core.Action.values = {_op: _op
                             ,nav: nav
                             ,change: change
                             ,always: always
                             ,Update: Update
                             ,Split: Split
                             ,Delete: Delete
                             ,EnterPrev: EnterPrev
                             ,EnterNext: EnterNext
                             ,NoChange: NoChange};
   return _elm.Core.Action.values;
};Elm.Html = Elm.Html || {};
Elm.Html.Attributes = Elm.Html.Attributes || {};
Elm.Html.Attributes.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Attributes = _elm.Html.Attributes || {};
   if (_elm.Html.Attributes.values)
   return _elm.Html.Attributes.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Html.Attributes",
   $Html = Elm.Html.make(_elm),
   $String = Elm.String.make(_elm);
   var manifest = function (value) {
      return A2($Html.attr,
      "manifest",
      value);
   };
   var scope = function (value) {
      return A2($Html.attr,
      "scope",
      value);
   };
   var rowspan = function (value) {
      return A2($Html.attr,
      "rowspan",
      value);
   };
   var headers = function (value) {
      return A2($Html.attr,
      "headers",
      value);
   };
   var colspan = function (value) {
      return A2($Html.attr,
      "colspan",
      value);
   };
   var start = function (n) {
      return A2($Html.attr,
      "start",
      $String.show(n));
   };
   var reversed = function (bool) {
      return A2($Html.toggle,
      "reversed",
      bool);
   };
   var pubdate = function (value) {
      return A2($Html.attr,
      "pubdate",
      value);
   };
   var datetime = function (value) {
      return A2($Html.attr,
      "datetime",
      value);
   };
   var rel = function (value) {
      return A2($Html.attr,
      "rel",
      value);
   };
   var ping = function (value) {
      return A2($Html.attr,
      "ping",
      value);
   };
   var media = function (value) {
      return A2($Html.attr,
      "media",
      value);
   };
   var hreflang = function (value) {
      return A2($Html.attr,
      "hreflang",
      value);
   };
   var downloadAs = function (value) {
      return A2($Html.attr,
      "download",
      value);
   };
   var download = function (bool) {
      return A2($Html.toggle,
      "download",
      bool);
   };
   var target = function (value) {
      return A2($Html.attr,
      "target",
      value);
   };
   var href = function (value) {
      return A2($Html.attr,
      "href",
      value);
   };
   var cite = function (value) {
      return A2($Html.attr,
      "cite",
      value);
   };
   var align = function (value) {
      return A2($Html.attr,
      "align",
      value);
   };
   var keytype = function (value) {
      return A2($Html.attr,
      "keytype",
      value);
   };
   var challenge = function (value) {
      return A2($Html.attr,
      "challenge",
      value);
   };
   var coords = function (value) {
      return A2($Html.attr,
      "coords",
      value);
   };
   var shape = function (value) {
      return A2($Html.attr,
      "shape",
      value);
   };
   var usemap = function (value) {
      return A2($Html.attr,
      "usemap",
      value);
   };
   var ismap = function (value) {
      return A2($Html.attr,
      "ismap",
      value);
   };
   var wrap = function (value) {
      return A2($Html.attr,
      "wrap",
      value);
   };
   var rows = function (n) {
      return A2($Html.attr,
      "rows",
      $String.show(n));
   };
   var cols = function (n) {
      return A2($Html.attr,
      "cols",
      $String.show(n));
   };
   var step = function (n) {
      return A2($Html.attr,
      "step",
      $String.show(n));
   };
   var min = function (value) {
      return A2($Html.attr,
      "min",
      value);
   };
   var max = function (value) {
      return A2($Html.attr,
      "max",
      value);
   };
   var form = function (value) {
      return A2($Html.attr,
      "form",
      value);
   };
   var $for = function (value) {
      return A2($Html.attr,
      "htmlFor",
      value);
   };
   var size = function (n) {
      return A2($Html.attr,
      "size",
      $String.show(n));
   };
   var required = function (bool) {
      return A2($Html.toggle,
      "required",
      bool);
   };
   var readonly = function (bool) {
      return A2($Html.toggle,
      "readonly",
      bool);
   };
   var pattern = function (value) {
      return A2($Html.attr,
      "pattern",
      value);
   };
   var novalidate = function (bool) {
      return A2($Html.toggle,
      "novalidate",
      bool);
   };
   var name = function (value) {
      return A2($Html.attr,
      "name",
      value);
   };
   var multiple = function (bool) {
      return A2($Html.toggle,
      "multiple",
      bool);
   };
   var method = function (value) {
      return A2($Html.attr,
      "method",
      value);
   };
   var maxlength = function (n) {
      return A2($Html.attr,
      "maxlength",
      $String.show(n));
   };
   var list = function (value) {
      return A2($Html.attr,
      "list",
      value);
   };
   var formaction = function (value) {
      return A2($Html.attr,
      "formaction",
      value);
   };
   var enctype = function (value) {
      return A2($Html.attr,
      "enctype",
      value);
   };
   var disabled = function (bool) {
      return A2($Html.toggle,
      "disabled",
      bool);
   };
   var autosave = function (value) {
      return A2($Html.attr,
      "autosave",
      value);
   };
   var autofocus = function (bool) {
      return A2($Html.toggle,
      "autofocus",
      bool);
   };
   var autocomplete = function (bool) {
      return A2($Html.attr,
      "autocomplete",
      bool ? "on" : "off");
   };
   var action = function (value) {
      return A2($Html.attr,
      "action",
      value);
   };
   var acceptCharset = function (value) {
      return A2($Html.attr,
      "acceptCharset",
      value);
   };
   var accept = function (value) {
      return A2($Html.attr,
      "accept",
      value);
   };
   var selected = function (bool) {
      return A2($Html.toggle,
      "selected",
      bool);
   };
   var placeholder = function (value) {
      return A2($Html.attr,
      "placeholder",
      value);
   };
   var checked = function (bool) {
      return A2($Html.toggle,
      "checked",
      bool);
   };
   var value = function (value) {
      return A2($Html.attr,
      "value",
      value);
   };
   var type$ = function (value) {
      return A2($Html.attr,
      "type",
      value);
   };
   var srcdoc = function (value) {
      return A2($Html.attr,
      "srcdoc",
      value);
   };
   var seamless = function (bool) {
      return A2($Html.toggle,
      "seamless",
      bool);
   };
   var sandbox = function (value) {
      return A2($Html.attr,
      "sandbox",
      value);
   };
   var srclang = function (value) {
      return A2($Html.attr,
      "srclang",
      value);
   };
   var kind = function (value) {
      return A2($Html.attr,
      "kind",
      value);
   };
   var $default = function (bool) {
      return A2($Html.toggle,
      "default",
      bool);
   };
   var poster = function (value) {
      return A2($Html.attr,
      "poster",
      value);
   };
   var preload = function (bool) {
      return A2($Html.toggle,
      "preload",
      bool);
   };
   var loop = function (bool) {
      return A2($Html.toggle,
      "loop",
      bool);
   };
   var controls = function (bool) {
      return A2($Html.toggle,
      "controls",
      bool);
   };
   var autoplay = function (bool) {
      return A2($Html.toggle,
      "autoplay",
      bool);
   };
   var alt = function (value) {
      return A2($Html.attr,
      "alt",
      value);
   };
   var width = function (value) {
      return A2($Html.attr,
      "width",
      value);
   };
   var height = function (value) {
      return A2($Html.attr,
      "height",
      value);
   };
   var src = function (value) {
      return A2($Html.attr,
      "src",
      value);
   };
   var scoped = function (bool) {
      return A2($Html.toggle,
      "scoped",
      bool);
   };
   var language = function (value) {
      return A2($Html.attr,
      "language",
      value);
   };
   var httpEquiv = function (value) {
      return A2($Html.attr,
      "httpEquiv",
      value);
   };
   var defer = function (bool) {
      return A2($Html.toggle,
      "defer",
      bool);
   };
   var content = function (value) {
      return A2($Html.attr,
      "content",
      value);
   };
   var charset = function (value) {
      return A2($Html.attr,
      "charset",
      value);
   };
   var async = function (bool) {
      return A2($Html.toggle,
      "async",
      bool);
   };
   var tabindex = function (n) {
      return A2($Html.attr,
      "tabindex",
      $String.show(n));
   };
   var spellcheck = function (bool) {
      return A2($Html.attr,
      "spellcheck",
      bool ? "true" : "false");
   };
   var lang = function (value) {
      return A2($Html.attr,
      "lang",
      value);
   };
   var itemprop = function (value) {
      return A2($Html.attr,
      "itemprop",
      value);
   };
   var dropzone = function (value) {
      return A2($Html.attr,
      "dropzone",
      value);
   };
   var draggable = function (value) {
      return A2($Html.attr,
      "draggable",
      value);
   };
   var dir = function (value) {
      return A2($Html.attr,
      "dir",
      value);
   };
   var contextmenu = function (value) {
      return A2($Html.attr,
      "contextmenu",
      value);
   };
   var contenteditable = function (bool) {
      return A2($Html.attr,
      "contenteditable",
      bool ? "true" : "false");
   };
   var accesskey = function ($char) {
      return A2($Html.attr,
      "accesskey",
      $String.fromList(_L.fromArray([$char])));
   };
   var title = function (name) {
      return A2($Html.attr,
      "title",
      name);
   };
   var id = function (name) {
      return A2($Html.attr,
      "id",
      name);
   };
   var hidden = function (bool) {
      return A2($Html.toggle,
      "hidden",
      bool);
   };
   var $class = function (name) {
      return A2($Html.attr,
      "className",
      name);
   };
   _elm.Html.Attributes.values = {_op: _op
                                 ,$class: $class
                                 ,hidden: hidden
                                 ,id: id
                                 ,title: title
                                 ,accesskey: accesskey
                                 ,contenteditable: contenteditable
                                 ,contextmenu: contextmenu
                                 ,dir: dir
                                 ,draggable: draggable
                                 ,dropzone: dropzone
                                 ,itemprop: itemprop
                                 ,lang: lang
                                 ,spellcheck: spellcheck
                                 ,tabindex: tabindex
                                 ,async: async
                                 ,charset: charset
                                 ,content: content
                                 ,defer: defer
                                 ,httpEquiv: httpEquiv
                                 ,language: language
                                 ,scoped: scoped
                                 ,src: src
                                 ,height: height
                                 ,width: width
                                 ,alt: alt
                                 ,autoplay: autoplay
                                 ,controls: controls
                                 ,loop: loop
                                 ,preload: preload
                                 ,poster: poster
                                 ,$default: $default
                                 ,kind: kind
                                 ,srclang: srclang
                                 ,sandbox: sandbox
                                 ,seamless: seamless
                                 ,srcdoc: srcdoc
                                 ,type$: type$
                                 ,value: value
                                 ,checked: checked
                                 ,placeholder: placeholder
                                 ,selected: selected
                                 ,accept: accept
                                 ,acceptCharset: acceptCharset
                                 ,action: action
                                 ,autocomplete: autocomplete
                                 ,autofocus: autofocus
                                 ,autosave: autosave
                                 ,disabled: disabled
                                 ,enctype: enctype
                                 ,formaction: formaction
                                 ,list: list
                                 ,maxlength: maxlength
                                 ,method: method
                                 ,multiple: multiple
                                 ,name: name
                                 ,novalidate: novalidate
                                 ,pattern: pattern
                                 ,readonly: readonly
                                 ,required: required
                                 ,size: size
                                 ,$for: $for
                                 ,form: form
                                 ,max: max
                                 ,min: min
                                 ,step: step
                                 ,cols: cols
                                 ,rows: rows
                                 ,wrap: wrap
                                 ,ismap: ismap
                                 ,usemap: usemap
                                 ,shape: shape
                                 ,coords: coords
                                 ,challenge: challenge
                                 ,keytype: keytype
                                 ,align: align
                                 ,cite: cite
                                 ,href: href
                                 ,target: target
                                 ,download: download
                                 ,downloadAs: downloadAs
                                 ,hreflang: hreflang
                                 ,media: media
                                 ,ping: ping
                                 ,rel: rel
                                 ,datetime: datetime
                                 ,pubdate: pubdate
                                 ,reversed: reversed
                                 ,start: start
                                 ,colspan: colspan
                                 ,headers: headers
                                 ,rowspan: rowspan
                                 ,scope: scope
                                 ,manifest: manifest};
   return _elm.Html.Attributes.values;
};Elm.Html = Elm.Html || {};
Elm.Html.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   if (_elm.Html.values)
   return _elm.Html.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Html",
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Graphics$Input = Elm.Graphics.Input.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Html = Elm.Native.Html.make(_elm);
   var getAnything = $Native$Html.getAnything;
   var KeyboardEvent = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,altKey: b
             ,ctrlKey: c
             ,keyCode: a
             ,metaKey: d
             ,shiftKey: e};
   });
   var getKeyboardEvent = $Native$Html.getKeyboardEvent;
   var MouseEvent = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,altKey: b
             ,button: a
             ,ctrlKey: c
             ,metaKey: d
             ,shiftKey: e};
   });
   var getMouseEvent = $Native$Html.getMouseEvent;
   var getValueAndSelection = $Native$Html.getValueAndSelection;
   var Backward = {ctor: "Backward"};
   var Forward = {ctor: "Forward"};
   var getValue = $Native$Html.getValue;
   var getChecked = $Native$Html.getChecked;
   var filterMap = $Native$Html.filterMap;
   var when = F2(function (pred,
   getter) {
      return A2($Native$Html.filterMap,
      function (v) {
         return pred(v) ? $Maybe.Just(v) : $Maybe.Nothing;
      },
      getter);
   });
   var on = $Native$Html.on;
   var Get = {ctor: "Get"};
   var prop = $Native$Html.pair;
   var style = $Native$Html.style;
   var CssProperty = {ctor: "CssProperty"};
   var toggle = $Native$Html.pair;
   var attr = $Native$Html.pair;
   var Attribute = {ctor: "Attribute"};
   var toElement = $Native$Html.toElement;
   var text = $Native$Html.text;
   var node = $Native$Html.node;
   var Html = {ctor: "Html"};
   _elm.Html.values = {_op: _op
                      ,Html: Html
                      ,node: node
                      ,text: text
                      ,toElement: toElement
                      ,Attribute: Attribute
                      ,attr: attr
                      ,toggle: toggle
                      ,CssProperty: CssProperty
                      ,style: style
                      ,prop: prop
                      ,Get: Get
                      ,on: on
                      ,when: when
                      ,filterMap: filterMap
                      ,getChecked: getChecked
                      ,getValue: getValue
                      ,Forward: Forward
                      ,Backward: Backward
                      ,getValueAndSelection: getValueAndSelection
                      ,getMouseEvent: getMouseEvent
                      ,MouseEvent: MouseEvent
                      ,getKeyboardEvent: getKeyboardEvent
                      ,KeyboardEvent: KeyboardEvent
                      ,getAnything: getAnything};
   return _elm.Html.values;
};Elm.Json = Elm.Json || {};
Elm.Json.Decoder = Elm.Json.Decoder || {};
Elm.Json.Decoder.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Decoder = _elm.Json.Decoder || {};
   if (_elm.Json.Decoder.values)
   return _elm.Json.Decoder.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Json.Decoder",
   $Basics = Elm.Basics.make(_elm),
   $Json = Elm.Json.make(_elm),
   $Json$Accessor = Elm.Json.Accessor.make(_elm),
   $Json$Output = Elm.Json.Output.make(_elm),
   $Json$Process = Elm.Json.Process.make(_elm),
   $List = Elm.List.make(_elm);
   var decode4 = F6(function (_v0,
   _v1,
   _v2,
   _v3,
   g,
   json) {
      return function () {
         switch (_v3.ctor)
         {case "NDec":
            return function () {
                 switch (_v2.ctor)
                 {case "NDec":
                    return function () {
                         switch (_v1.ctor)
                         {case "NDec":
                            return function () {
                                 switch (_v0.ctor)
                                 {case "NDec":
                                    return A2($Json$Process._op[">>="],
                                      A2($Json$Process._op[">>="],
                                      A2($Json$Accessor.getVal,
                                      _v0._0,
                                      json),
                                      _v0._1),
                                      function (a1) {
                                         return A2($Json$Process._op[">>="],
                                         A2($Json$Process._op[">>="],
                                         A2($Json$Accessor.getVal,
                                         _v1._0,
                                         json),
                                         _v1._1),
                                         function (a2) {
                                            return A2($Json$Process._op[">>="],
                                            A2($Json$Process._op[">>="],
                                            A2($Json$Accessor.getVal,
                                            _v2._0,
                                            json),
                                            _v2._1),
                                            function (a3) {
                                               return A2($Json$Process._op[">>="],
                                               A2($Json$Process._op[">>="],
                                               A2($Json$Accessor.getVal,
                                               _v3._0,
                                               json),
                                               _v3._1),
                                               $Json$Process.process(A3(g,
                                               a1,
                                               a2,
                                               a3)));
                                            });
                                         });
                                      });}
                                 _E.Case($moduleName,
                                 "between lines 110 and 118");
                              }();}
                         _E.Case($moduleName,
                         "between lines 110 and 118");
                      }();}
                 _E.Case($moduleName,
                 "between lines 110 and 118");
              }();}
         _E.Case($moduleName,
         "between lines 110 and 118");
      }();
   });
   var decode3 = F5(function (_v16,
   _v17,
   _v18,
   g,
   json) {
      return function () {
         switch (_v18.ctor)
         {case "NDec":
            return function () {
                 switch (_v17.ctor)
                 {case "NDec":
                    return function () {
                         switch (_v16.ctor)
                         {case "NDec":
                            return A2($Json$Process._op[">>="],
                              A2($Json$Process._op[">>="],
                              A2($Json$Accessor.getVal,
                              _v16._0,
                              json),
                              _v16._1),
                              function (a1) {
                                 return A2($Json$Process._op[">>="],
                                 A2($Json$Process._op[">>="],
                                 A2($Json$Accessor.getVal,
                                 _v17._0,
                                 json),
                                 _v17._1),
                                 function (a2) {
                                    return A2($Json$Process._op[">>="],
                                    A2($Json$Process._op[">>="],
                                    A2($Json$Accessor.getVal,
                                    _v18._0,
                                    json),
                                    _v18._1),
                                    $Json$Process.process(A2(g,
                                    a1,
                                    a2)));
                                 });
                              });}
                         _E.Case($moduleName,
                         "between lines 100 and 106");
                      }();}
                 _E.Case($moduleName,
                 "between lines 100 and 106");
              }();}
         _E.Case($moduleName,
         "between lines 100 and 106");
      }();
   });
   var decode2 = F4(function (_v28,
   _v29,
   g,
   json) {
      return function () {
         switch (_v29.ctor)
         {case "NDec":
            return function () {
                 switch (_v28.ctor)
                 {case "NDec":
                    return A2($Json$Process._op[">>="],
                      A2($Json$Process._op[">>="],
                      A2($Json$Accessor.getVal,
                      _v28._0,
                      json),
                      _v28._1),
                      function (a1) {
                         return A2($Json$Process._op[">>="],
                         A2($Json$Process._op[">>="],
                         A2($Json$Accessor.getVal,
                         _v29._0,
                         json),
                         _v29._1),
                         $Json$Process.process(g(a1)));
                      });}
                 _E.Case($moduleName,
                 "between lines 92 and 96");
              }();}
         _E.Case($moduleName,
         "between lines 92 and 96");
      }();
   });
   var decode1 = F3(function (_v36,
   g,
   json) {
      return function () {
         switch (_v36.ctor)
         {case "NDec":
            return A2($Json$Process._op[">>="],
              A2($Json$Process._op[">>="],
              A2($Json$Accessor.getVal,
              _v36._0,
              json),
              _v36._1),
              $Json$Process.process(g));}
         _E.Case($moduleName,
         "between lines 86 and 88");
      }();
   });
   var decode = decode1;
   var fromString = function (s) {
      return $Json$Output.fromMaybe($Json.fromString(s));
   };
   var decoderErrorMsg = function (s) {
      return _L.append("Could not decode: \'",
      _L.append(s,"\'"));
   };
   var string = function (v) {
      return function () {
         switch (v.ctor)
         {case "String":
            return $Json$Output.output(v._0);}
         return $Json$Output.Error(decoderErrorMsg("{string}"));
      }();
   };
   var $float = function (v) {
      return function () {
         switch (v.ctor)
         {case "Number":
            return $Json$Output.output(v._0);}
         return $Json$Output.Error(decoderErrorMsg("{float}"));
      }();
   };
   var $int = A2($Json$Process.mappedTo,
   $float,
   $Basics.floor);
   var bool = function (v) {
      return function () {
         switch (v.ctor)
         {case "Boolean":
            return $Json$Output.output(v._0);}
         return $Json$Output.Error(decoderErrorMsg("{bool}"));
      }();
   };
   var listOf = F2(function (f,v) {
      return function () {
         switch (v.ctor)
         {case "Array":
            return $Json$Output.output($Json$Output.successes(A2($List.map,
              f,
              v._0)));}
         return $Json$Output.Error(decoderErrorMsg("{list}"));
      }();
   });
   var NDec = F2(function (a,b) {
      return {ctor: "NDec"
             ,_0: a
             ,_1: b};
   });
   _op[":="] = F2(function (k,d) {
      return A2(NDec,k,d);
   });
   _elm.Json.Decoder.values = {_op: _op
                              ,NDec: NDec
                              ,decoderErrorMsg: decoderErrorMsg
                              ,string: string
                              ,$float: $float
                              ,$int: $int
                              ,bool: bool
                              ,listOf: listOf
                              ,fromString: fromString
                              ,decode1: decode1
                              ,decode2: decode2
                              ,decode3: decode3
                              ,decode4: decode4
                              ,decode: decode};
   return _elm.Json.Decoder.values;
};Elm.Json = Elm.Json || {};
Elm.Json.Accessor = Elm.Json.Accessor || {};
Elm.Json.Accessor.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Accessor = _elm.Json.Accessor || {};
   if (_elm.Json.Accessor.values)
   return _elm.Json.Accessor.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Json.Accessor",
   $Basics = Elm.Basics.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Json = Elm.Json.make(_elm),
   $Json$Output = Elm.Json.Output.make(_elm),
   $Json$Process = Elm.Json.Process.make(_elm),
   $List = Elm.List.make(_elm),
   $String = Elm.String.make(_elm);
   var accessorError = function (_v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return $Json$Output.Error(_L.append("Could not access a \'",
              _L.append(_v0._0,
              _L.append("\' in \'",
              _L.append($String.show(_v0._1),
              "\'")))));}
         _E.Case($moduleName,
         "on line 28, column 23 to 90");
      }();
   };
   var getVal = F2(function (n,
   json) {
      return function () {
         switch (json.ctor)
         {case "Object":
            return function () {
                 var _v6 = A3($Dict.getOrElse,
                 $Json.Null,
                 n,
                 json._0);
                 switch (_v6.ctor)
                 {case "Null":
                    return accessorError({ctor: "_Tuple2"
                                         ,_0: n
                                         ,_1: json});}
                 return $Json$Output.output(_v6);
              }();}
         return accessorError({ctor: "_Tuple2"
                              ,_0: n
                              ,_1: json});
      }();
   });
   var delve = F2(function (xs,
   mv) {
      return A2($Json$Process.collapsel,
      $Json$Output.output(mv),
      A2($List.map,getVal,xs));
   });
   _elm.Json.Accessor.values = {_op: _op
                               ,delve: delve
                               ,getVal: getVal};
   return _elm.Json.Accessor.values;
};Elm.Json = Elm.Json || {};
Elm.Json.Process = Elm.Json.Process || {};
Elm.Json.Process.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Process = _elm.Json.Process || {};
   if (_elm.Json.Process.values)
   return _elm.Json.Process.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Json.Process",
   $Basics = Elm.Basics.make(_elm),
   $Json$Output = Elm.Json.Output.make(_elm),
   $List = Elm.List.make(_elm);
   var or = F3(function (p1,p2,a) {
      return function () {
         var o1 = p1(a);
         var p3 = function (a) {
            return A3($Json$Output.cata,
            function (_v0) {
               return function () {
                  return o1;
               }();
            },
            function (_v2) {
               return function () {
                  return p2(a);
               }();
            },
            o1);
         };
         return p3(a);
      }();
   });
   var from = function (f) {
      return A2($Json$Output.cata,
      f,
      $Json$Output.Error);
   };
   var into = $Basics.flip(from);
   _op[">>="] = into;
   var glue = F3(function (f,g,a) {
      return A2(_op[">>="],f(a),g);
   });
   _op[">>>"] = glue;
   var map = F3(function (f,p,a) {
      return A2(_op[">>="],
      p(a),
      function (b) {
         return $Json$Output.output(f(b));
      });
   });
   var mappedTo = $Basics.flip(map);
   var collapsel = F2(function (ob,
   xs) {
      return A3($List.foldl,
      F2(function (p,o) {
         return A2(_op[">>="],o,p);
      }),
      ob,
      xs);
   });
   var process = F2(function (f,
   a) {
      return $Json$Output.output(f(a));
   });
   _elm.Json.Process.values = {_op: _op
                              ,process: process
                              ,from: from
                              ,into: into
                              ,glue: glue
                              ,map: map
                              ,mappedTo: mappedTo
                              ,collapsel: collapsel
                              ,or: or};
   return _elm.Json.Process.values;
};Elm.Json = Elm.Json || {};
Elm.Json.Output = Elm.Json.Output || {};
Elm.Json.Output.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Output = _elm.Json.Output || {};
   if (_elm.Json.Output.values)
   return _elm.Json.Output.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Json.Output",
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm);
   var cata = F3(function (f,
   g,
   pa) {
      return function () {
         switch (pa.ctor)
         {case "Error": return g(pa._0);
            case "Success":
            return f(pa._0);}
         _E.Case($moduleName,
         "between lines 27 and 35");
      }();
   });
   var successes = function (xs) {
      return A3($List.foldl,
      F2(function (a,b) {
         return A3(cata,
         function (s) {
            return _L.append(b,
            _L.fromArray([s]));
         },
         function (_v3) {
            return function () {
               return b;
            }();
         },
         a);
      }),
      _L.fromArray([]),
      xs);
   };
   var Error = function (a) {
      return {ctor: "Error",_0: a};
   };
   var Success = function (a) {
      return {ctor: "Success"
             ,_0: a};
   };
   var output = Success;
   var fromMaybe = function (ma) {
      return function () {
         switch (ma.ctor)
         {case "Just":
            return Success(ma._0);
            case "Nothing":
            return Error("Nothing");}
         _E.Case($moduleName,
         "between lines 45 and 47");
      }();
   };
   _elm.Json.Output.values = {_op: _op
                             ,Success: Success
                             ,Error: Error
                             ,output: output
                             ,cata: cata
                             ,successes: successes
                             ,fromMaybe: fromMaybe};
   return _elm.Json.Output.values;
};Elm.Keys = Elm.Keys || {};
Elm.Keys.make = function (_elm) {
   "use strict";
   _elm.Keys = _elm.Keys || {};
   if (_elm.Keys.values)
   return _elm.Keys.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Keys",
   $List = Elm.List.make(_elm),
   $String = Elm.String.make(_elm);
   var Unrecognized = function (a) {
      return {ctor: "Unrecognized"
             ,_0: a};
   };
   var Command = function (a) {
      return {ctor: "Command"
             ,_0: a};
   };
   var fromMeta = function (code) {
      return function () {
         switch (code)
         {case 65: return Command("a");
            case 68: return Command("d");
            case 80: return Command("p");}
         return Unrecognized(_L.append("Meta-",
         $String.show(code)));
      }();
   };
   var Character = function (a) {
      return {ctor: "Character"
             ,_0: a};
   };
   var Down = {ctor: "Down"};
   var Up = {ctor: "Up"};
   var Right = {ctor: "Right"};
   var Left = {ctor: "Left"};
   var Backspace = {ctor: "Backspace"};
   var Enter = {ctor: "Enter"};
   var fromPresses = function (string) {
      return function () {
         switch (string)
         {case "\r": return Enter;}
         return Character(string);
      }();
   };
   var fromDowns = function (code) {
      return function () {
         switch (code)
         {case 8: return Backspace;
            case 13: return Enter;
            case 37: return Left;
            case 38: return Up;
            case 39: return Right;
            case 40: return Down;}
         return Unrecognized($String.show(code));
      }();
   };
   _elm.Keys.values = {_op: _op
                      ,Enter: Enter
                      ,Backspace: Backspace
                      ,Left: Left
                      ,Right: Right
                      ,Up: Up
                      ,Down: Down
                      ,Character: Character
                      ,Command: Command
                      ,Unrecognized: Unrecognized
                      ,fromPresses: fromPresses
                      ,fromDowns: fromDowns
                      ,fromMeta: fromMeta};
   return _elm.Keys.values;
};