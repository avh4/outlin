var fs = require('fs');
var ltx = require('ltx');

var xml = ltx.parse(fs.readFileSync("Tasks.xml"));

function quote(string) {
  return '"' + string.replace(/"/g, '\\"') + '"';
}

function Entry(text, children) {
  this.text = text;
  this.children = children;
}

Entry.prototype.toString = function(depth) {
  var indent = Array((depth||0)*2).join("  ");
  return "Entry.Entry {text=" + quote(this.text) + ", description=\"\", children=[" + this.children.map(function(c) { return "\n" + indent + c.toString((depth||0)+1); }) + "]}";
}

function doNode(node) {
  if (node.name == 'outline') {
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
    return new Entry(
      doNode(node.getChild("values").getChild("text")),
      children
    );
  } else if (node.name == 'text') {
    return node.getChildren("p").map(doNode).join("\n");
  } else if (node.name == 'p') {
    return node.getChildren("run").map(doNode).join();
  } else if (node.name == 'run') {
    return node.getChildren("lit").map(doNode).join();
  } else if (node.name == 'lit') {
    return node.getText();
  } else if (node.name == 'children') {
    return node.getChildren("item").map(doNode);
  } else {
    throw new Error("Unknown element: " + node.name);
  }
}

//console.log(JSON.stringify(doNode(xml), null, 2));
console.log("module SampleData where\n");
console.log("import Outline.Entry as Entry\n");
console.log("template = " + doNode(xml).toString(1))