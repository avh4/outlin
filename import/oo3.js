var argv = require('optimist').demand(1).argv;

var fs = require('fs');
var ltx = require('ltx');
var zlib = require('zlib');

function processFile(filename) {
  var string = ''
  var stream = fs.createReadStream(filename + "/contents.xml").pipe(zlib.createGunzip());
  stream.on('data',function(chunk){
    var part = chunk.toString();
    string += part;
  });

  stream.on('end',function(){
   var xml = ltx.parse(string);
   //console.log(JSON.stringify(doNode(xml), null, 2));
   console.log(doNode(xml).toString(1))
  });
}

function quote(string) {
  return '"' + string.replace(/"/g, '\\"') + '"';
}

function Entry(text, description, children) {
  this.text = text;
  this.description = description;
  this.children = children;
}

Entry.prototype.toString = function(depth) {
  var indent = Array((depth||0)*2).join("  ");
  return '{"text":' + quote(this.text) + ', "description":' + quote(this.description) + ',"inbox":[],"children":[' + this.children.map(function(c) { return c.toString((depth||0)+1); }) + "]}";
}

function doNode(node) {
  if (typeof node == 'string') {
    return node;
  } else if (node.name == 'outline') {
    return doNode(node.getChild('root'));
  } else if (node.name == 'style-attribute-registry') {
    return;
  } else if (node.name == 'named-styles') {
    return;
  } else if (node.name == 'settings') {
    return;
  } else if (node.name == 'editor') {
    return;
  } else if (node.name == 'outline-title') {
    return;
  } else if (node.name == 'columns') {
    return;
  } else if (node.name == 'root') {
    return doNode(node.getChild("item"));
  } else if (node.name == 'style') {
    return;
  } else if (node.name == 'item') {
    var children = node.getChild("children") ? doNode(node.getChild("children")) : [];
    var note = node.getChild("note") ? doNode(node.getChild("note")) : "";
    return new Entry(
      doNode(node.getChild("values").getChild("text")),
      note, children
    );
  } else if (node.name == 'text') {
    return node.getChildren("p").map(doNode).join("\n");
  } else if (node.name == 'p') {
    return node.getChildren("run").map(doNode).join("");
  } else if (node.name == 'run') {
    return node.getChildren("lit").map(doNode).join("");
  } else if (node.name == 'lit') {
    return node.children.map(doNode).join("");
  } else if (node.name == 'cell') {
    if (node.attrs.href == node.attrs.name) {
      return node.attrs.name;
    } else {
      return '[' + node.attrs.name + '](' + node.attrs.href + ')';
    }
  } else if (node.name == 'children') {
    return node.getChildren("item").map(doNode);
  } else if (node.name == 'note') {
    return doNode(node.getChild("text"));
  } else {
    throw new Error("Unknown element: " + node.name);
  }
}

// START

processFile(argv._[0]);
