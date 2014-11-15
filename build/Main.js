'use strict';
var Elm = {}; Elm.Native = {}; Elm.Native.Graphics = {};
var ElmRuntime = {}; ElmRuntime.Render = {};
Elm.Native.Array = {};
Elm.Native.Array.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Array = elm.Native.Array || {};
    if (elm.Native.Array.values) return elm.Native.Array.values;
    if ('values' in Elm.Native.Array)
      return elm.Native.Array.values = Elm.Native.Array.values;

    var List = Elm.Native.List.make(elm);

    // A RRB-Tree has two distinct data types.
    // Leaf -> "height"  is always 0
    //         "table"   is an array of elements
    // Node -> "height"  is always greater than 0
    //         "table"   is an array of child nodes
    //         "lengths" is an array of accumulated lengths of the child nodes

    // M is the maximal table size. 32 seems fast. E is the allowed increase
    // of search steps when concatting to find an index. Lower values will 
    // decrease balancing, but will increase search steps.
    var M = 32;
    var E = 2;

    // An empty array.
    var empty = { ctor:"_Array", height:0, table:new Array() };

    function get(i, array) {
        if (i < 0 || i >= length(array)) {
            throw new Error("Index " + i + " is out of range. Check the length of " +
                            "your array first or use getMaybe or getWithDefault.");
        }
        return unsafeGet(i, array);
    }

    function unsafeGet(i, array) {
      for (var x = array.height; x > 0; x--) {
        var slot = i >> (x * 5);
        while (array.lengths[slot] <= i) {
          slot++;
        }
        if (slot > 0) {
          i -= array.lengths[slot - 1];
        }
        array = array.table[slot];
      }
      return array.table[i];
    }

    // Sets the value at the index i. Only the nodes leading to i will get
    // copied and updated.
    function set(i, item, array) {
      if (i < 0 || length(array) <= i) {
        return array;
      }
      return unsafeSet(i, item, array);
    }

    function unsafeSet(i, item, array) {
      array = nodeCopy(array);

      if (array.height == 0) {
        array.table[i] = item;
      } else {
        var slot = getSlot(i, array);
        if (slot > 0) {
          i -= array.lengths[slot - 1];
        }
        array.table[slot] = unsafeSet(i, item, array.table[slot]);
      }
      return array;
    }

    function initialize(len, f) {
      if (len == 0) { return empty; }
      var h = Math.floor(Math.log(len)/Math.log(M));
      return initialize_(f, h, 0, len);
    }

    function initialize_(f, h, from, to) {
      if (h == 0) {
        var table = new Array((to - from) % (M + 1));
        for (var i = 0; i < table.length; i++) {
          table[i] = f(from + i);
        }
        return { ctor:"_Array", height:0, table:table };
      }

      var step = Math.pow(M, h);
      var table = new Array(Math.ceil((to - from) / step));
      var lengths = new Array(table.length);
      for (var i = 0; i < table.length; i++) {
        table[i] = initialize_( f, h - 1, from + (i * step)
                              , Math.min(from + ((i + 1) * step), to));
        lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
      }
      return { ctor:"_Array", height:h, table:table, lengths:lengths };
    }

    function fromList(list) {
      if (list == List.Nil) { return empty; }

      // Allocate M sized blocks (table) and write list elements to it.
      var table = new Array(M);
      var nodes = new Array();
      var i = 0;

      while (list.ctor !== '[]') {
        table[i] = list._0;
        list = list._1;
        i++;

        // table is full, so we can push a leaf containing it into the
        // next node.
        if (i == M) {
          fromListPush({ ctor:"_Array", height:0, table:table }
                      , nodes);
          table = new Array(M);
          i = 0;
        }
      }

      // Maybe there is something left on the table.
      if (i > 0) {
        fromListPush({ ctor:"_Array", height:0, table:table.splice(0,i) }
                    , nodes);
      }

      // Go through all of the nodes and eventually push them into higher nodes.
      for (var h = 0; h < nodes.length - 1; h++) {
        if (nodes[h].table.length > 0) {
          fromListPush(nodes[h], nodes);
        }
      }

      var head = nodes[nodes.length - 1];
      if (head.height > 0 && head.table.length == 1) {
        return head.table[0];
      } else {
        return head;
      }
    }

    // Push a node into a higher node as a child.
    function fromListPush(toPush, nodes) {
      var h = toPush.height;

      // Maybe the node on this height does not exist.
      if (nodes.length == h) {
        nodes.push({ ctor:"_Array", height:h + 1
                                  , table:new Array()
                                  , lengths:new Array() });
      }

      nodes[h].table.push(toPush);
      var len = length(toPush);
      if (nodes[h].lengths.length > 0) {
        len += nodes[h].lengths[nodes[h].lengths.length - 1];
      }
      nodes[h].lengths.push(len);

      if (nodes[h].table.length == M) {
        fromListPush(nodes[h], nodes);
        nodes[h] = { ctor:"_Array", height:h + 1
                                  , table:new Array()
                                  , lengths:new Array() };
      }
    }

    // Pushes an item via push_ to the bottom right of a tree.
    function push(item, a) {
      var pushed = push_(item, a);
      if (pushed !== null) {
        return pushed;
      }

      var newTree = create(item, a.height);
      return siblise(a, newTree);
    }

    // Recursively tries to push an item to the bottom-right most
    // tree possible. If there is no space left for the item,
    // null will be returned.
    function push_(item, a) {
      // Handle resursion stop at leaf level.
      if (a.height == 0) {
        if (a.table.length < M) {
          var newA = { ctor:"_Array", height:0, table:a.table.slice() };
          newA.table.push(item);
          return newA;
        } else {
          return null;
        }
      }

      // Recursively push
      var pushed = push_(item, botRight(a));

      // There was space in the bottom right tree, so the slot will
      // be updated.
      if (pushed != null) {
        var newA = nodeCopy(a);
        newA.table[newA.table.length - 1] = pushed;
        newA.lengths[newA.lengths.length - 1]++;
        return newA
      }

      // When there was no space left, check if there is space left
      // for a new slot with a tree which contains only the item
      // at the bottom.
      if (a.table.length < M) {
        var newSlot = create(item, a.height - 1);
        var newA = nodeCopy(a);
        newA.table.push(newSlot);
        newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
        return newA
      } else {
        return null;
      }
    }

    // Converts an array into a list of elements.
    function toList(a) {
      return toList_(List.Nil, a);
    }

    function toList_(list, a) {
      for (var i = a.table.length - 1; i >= 0; i--) {
        list = a.height == 0 ? List.Cons(a.table[i], list) : toList_(list, a.table[i]);
      }
      return list;
    }

    // Maps a function over the elements of an array.
    function map(f, a) {
      var newA = { ctor:"_Array", height:a.height, table:new Array(a.table) };
      if (a.height > 0) { newA.lengths = a.lengths; }
      for (var i = 0; i < a.table.length; i++) {
        newA.table[i] = a.height == 0 ? f(a.table[i]) : map(f, a.table[i]);
      }
      return newA;
    }

    // Maps a function over the elements with their index as first argument.
    function indexedMap(f, a) {
      return indexedMap_(f, a, 0);
    }

    function indexedMap_(f, a, from) {
      var newA = { ctor:"_Array", height:a.height, table:new Array(a.table) };
      if (a.height > 0) { newA.lengths = a.lengths; }
      for (var i = 0; i < a.table.length; i++) {
        newA.table[i] = a.height == 0 ? A2(f, from + i, a.table[i])
                                      : indexedMap_( f, a.table[i]
                                                   , i == 0 ? 0 : a.lengths[i - 1]);
      }
      return newA;
    }

    function foldl(f, b, a) {
      if (a.height == 0) {
        for (var i = 0; i < a.table.length; i++) {
          b = A2(f, a.table[i], b);
        }
      } else {
        for (var i = 0; i < a.table.length; i++) {
          b = foldl(f, b, a.table[i]);
        }
      }
      return b;
    }

    function foldr(f, b, a) {
      if (a.height == 0) {
        for (var i = a.table.length; i--; ) {
          b = A2(f, a.table[i], b);
        }
      } else {
        for (var i = a.table.length; i--; ) {
          b = foldl(f, b, a.table[i]);
        }
      }
      return b;
    }

    // TODO: currently, it slices the right, then the left. This can be
    // optimized.
    function slice(from, to, a) {
      if (from < 0) { from += length(a); }
      if (to < 0)   { to += length(a); }
      return sliceLeft(from, sliceRight(to, a));
    }

    function sliceRight(to, a) {
      if (to == length(a)) {
        return a;
      }

      // Handle leaf level.
      if (a.height == 0) {
        var newA = { ctor:"_Array", height:0 };
        newA.table = a.table.slice(0, to);
        return newA;
      }

      // Slice the right recursively.
      var right = getSlot(to, a);
      var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

      // Maybe the a node is not even needed, as sliced contains the whole slice.
      if (right == 0) {
        return sliced;
      }

      // Create new node.
      var newA = { ctor:"_Array", height:a.height
                                , table:a.table.slice(0, right)
                                , lengths:a.lengths.slice(0, right) };
      if (sliced.table.length > 0) {
        newA.table[right] = sliced;
        newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
      }
      return newA;
    }

    function sliceLeft(from, a) {
      if (from == 0) {
        return a;
      }

      // Handle leaf level.
      if (a.height == 0) {
        var newA = { ctor:"_Array", height:0 };
        newA.table = a.table.slice(from, a.table.length + 1);
        return newA;
      }

      // Slice the left recursively.
      var left = getSlot(from, a);
      var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

      // Maybe the a node is not even needed, as sliced contains the whole slice.
      if (left == a.table.length - 1) {
        return sliced;
      }

      // Create new node.
      var newA = { ctor:"_Array", height:a.height
                                , table:a.table.slice(left, a.table.length + 1)
                                , lengths:new Array(a.table.length - left) };
      newA.table[0] = sliced;
      var len = 0;
      for (var i = 0; i < newA.table.length; i++) {
        len += length(newA.table[i]);
        newA.lengths[i] = len;
      }

      return newA;
    }

    // Appends two trees.
    function append(a,b) {
      if (a.table.length === 0) {
        return b;
      }
      if (b.table.length === 0) {
        return a;
      }

      var c = append_(a, b);

      // Check if both nodes can be crunshed together.
      if (c[0].table.length + c[1].table.length <= M) {
        if (c[0].table.length === 0) {
          return c[1];
        }
        if (c[1].table.length === 0) {
          return c[0];
        }

        // Adjust .table and .lengths
        c[0].table = c[0].table.concat(c[1].table);
        if (c[0].height > 0) {
          var len = length(c[0]);
          for (var i = 0; i < c[1].lengths.length; i++) {
            c[1].lengths[i] += len;
          }
          c[0].lengths = c[0].lengths.concat(c[1].lengths);
        }

        return c[0];
      }

      var toRemove = calcToRemove(a, b);
      if (toRemove > E) {
        c = shuffle(c[0], c[1], toRemove);
      }

      return siblise(c[0], c[1]);
    }

    // Returns an array of two nodes; right and left. One node _may_ be empty.
    function append_(a, b) {
      if (a.height === 0 && b.height === 0) {
        return [a, b];
      }

      if (a.height !== 1 || b.height !== 1) {
        if (a.height === b.height) {
          a = nodeCopy(a);
          b = nodeCopy(b);
          var appended = append_(botRight(a), botLeft(b));

          insertRight(a, appended[1]);
          insertLeft(b, appended[0]);
        } else if (a.height > b.height) {
          a = nodeCopy(a);
          var appended = append_(botRight(a), b);

          insertRight(a, appended[0]);
          b = parentise(appended[1], appended[1].height + 1);
        } else {
          b = nodeCopy(b);
          var appended = append_(a, botLeft(b));

          var left = appended[0].table.length === 0 ? 0 : 1;
          var right = left === 0 ? 1 : 0;
          insertLeft(b, appended[left]);
          a = parentise(appended[right], appended[right].height + 1);
        }
      }

      // Check if balancing is needed and return based on that.
      if (a.table.length === 0 || b.table.length === 0) {
        return [a,b];
      }

      var toRemove = calcToRemove(a, b);
      if (toRemove <= E) {
        return [a,b];
      }
      return shuffle(a, b, toRemove);
    }

    // Helperfunctions for append_. Replaces a child node at the side of the parent.
    function insertRight(parent, node) {
      var index = parent.table.length - 1;
      parent.table[index] = node;
      parent.lengths[index] = length(node)
      parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
    }

    function insertLeft(parent, node) {
      if (node.table.length > 0) {
        parent.table[0] = node;
        parent.lengths[0] = length(node);

        var len = length(parent.table[0]);
        for (var i = 1; i < parent.lengths.length; i++) {
          len += length(parent.table[i]);
          parent.lengths[i] = len;
        }
      } else {
        parent.table.shift();
        for (var i = 1; i < parent.lengths.length; i++) {
          parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
        }
        parent.lengths.shift();
      }
    }

    // Returns the extra search steps for E. Refer to the paper.
    function calcToRemove(a, b) {
      var subLengths = 0;
      for (var i = 0; i < a.table.length; i++) {
        subLengths += a.table[i].table.length;
      }
      for (var i = 0; i < b.table.length; i++) {
        subLengths += b.table[i].table.length;
      }

      var toRemove = a.table.length + b.table.length
      return toRemove - (Math.floor((subLengths - 1) / M) + 1);
    }

    // get2, set2 and saveSlot are helpers for accessing elements over two arrays.
    function get2(a, b, index) {
      return index < a.length ? a[index] : b[index - a.length];
    }

    function set2(a, b, index, value) {
      if (index < a.length) {
        a[index] = value;
      } else {
        b[index - a.length] = value;
      }
    }

    function saveSlot(a, b, index, slot) {
      set2(a.table, b.table, index, slot);

      var l = (index == 0 || index == a.lengths.length) ?
                0 : get2(a.lengths, a.lengths, index - 1);
      set2(a.lengths, b.lengths, index, l + length(slot));
    }

    // Creates a node or leaf with a given length at their arrays for perfomance.
    // Is only used by shuffle.
    function createNode(h, length) {
      if (length < 0) { length = 0; }
      var a = { ctor:"_Array", height:h, table:new Array(length) };
      if (h > 0) {
        a.lengths = new Array(length);
      }
      return a;
    }

    // Returns an array of two balanced nodes.
    function shuffle(a, b, toRemove) {
      var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
      var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

      // Skip the slots with size M. More precise: copy the slot references
      // to the new node
      var read = 0;
      while (get2(a.table, b.table, read).table.length % M == 0) {
        set2(newA.table, newB.table, read, get2(a.table, b.table, read));
        set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
        read++;
      }

      // Pulling items from left to right, caching in a slot before writing
      // it into the new nodes.
      var write = read;
      var slot = new createNode(a.height - 1, 0);
      var from = 0;

      // If the current slot is still containing data, then there will be at
      // least one more write, so we do not break this loop yet.
      while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove) {
        // Find out the max possible items for copying.
        var source = get2(a.table, b.table, read);
        var to = Math.min(M - slot.table.length, source.table.length)

        // Copy and adjust size table.
        slot.table = slot.table.concat(source.table.slice(from, to));
        if (slot.height > 0) {
          var len = slot.lengths.length;
          for (var i = len; i < len + to - from; i++) {
            slot.lengths[i] = length(slot.table[i]);
            slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
          }
        }

        from += to;

        // Only proceed to next slots[i] if the current one was
        // fully copied.
        if (source.table.length <= to) {
          read++; from = 0;
        }

        // Only create a new slot if the current one is filled up.
        if (slot.table.length == M) {
          saveSlot(newA, newB, write, slot);
          slot = createNode(a.height - 1,0);
          write++;
        }
      }

      // Cleanup after the loop. Copy the last slot into the new nodes.
      if (slot.table.length > 0) {
        saveSlot(newA, newB, write, slot);
        write++;
      }

      // Shift the untouched slots to the left
      while (read < a.table.length + b.table.length ) {
        saveSlot(newA, newB, write, get2(a.table, b.table, read));
        read++; write++;
      }

      return [newA, newB];
    }

    // Navigation functions
    function botRight(a) { return a.table[a.table.length - 1]; }
    function botLeft(a)  { return a.table[0]; }

    // Copies a node for updating. Note that you should not use this if
    // only updating only one of "table" or "lengths" for performance reasons.
    function nodeCopy(a) {
      var newA = { ctor:"_Array", height:a.height
                                , table:a.table.slice() };
      if (a.height > 0) { newA.lengths = a.lengths.slice(); }
      return newA;
    }

    // Returns how many items are in the tree.
    function length(array) {
      if (array.height == 0) {
        return array.table.length;
      } else {
        return array.lengths[array.lengths.length - 1];
      }
    }

    // Calculates in which slot of "table" the item probably is, then
    // find the exact slot via forward searching in  "lengths". Returns the index.
    function getSlot(i, a) {
      var slot = i >> (5 * a.height);
      while (a.lengths[slot] <= i) {
        slot++;
      }
      return slot;
    }

    // Recursively creates a tree with a given height containing
    // only the given item.
    function create(item, h) {
      if (h == 0) {
        return { ctor:"_Array", height:0
                              , table:[item] };
      } else {
        return { ctor:"_Array", height:h
                              , table:[create(item, h - 1)]
                              , lengths:[1] };
      }
    }

    // Recursively creates a tree that contains the given tree.
    function parentise(tree, h) {
      if (h == tree.height) {
        return tree;
      } else {
        return { ctor:"_Array", height:h
                              , table:[parentise(tree, h - 1)]
                              , lengths:[length(tree)] };
      }
    }

    // Emphasizes blood brotherhood beneath two trees.
    function siblise(a, b) {
      return { ctor:"_Array", height:a.height + 1
                            , table:[a, b]
                            , lengths:[length(a), length(a) + length(b)] };
    }

    function toJSArray(a) {
      var jsArray = new Array(length(a));
      toJSArray_(jsArray, 0, a);
      return jsArray;
    }

    function toJSArray_(jsArray, i, a) {
      for (var t = 0; t < a.table.length; t++) {
        if (a.height == 0) {
          jsArray[i + t] = a.table[t];
        } else {
          var inc = t == 0 ? 0 : a.lengths[t - 1];
          toJSArray_(jsArray, i + inc, a.table[t]);
        }
      }
    }

    function fromJSArray(jsArray) {
      if (jsArray.length == 0) { return empty; }
      var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
      return fromJSArray_(jsArray, h, 0, jsArray.length);
    }

    function fromJSArray_(jsArray, h, from, to) {
      if (h == 0) {
        return { ctor:"_Array", height:0
                              , table:jsArray.slice(from, to) };
      }

      var step = Math.pow(M, h);
      var table = new Array(Math.ceil((to - from) / step));
      var lengths = new Array(table.length);
      for (var i = 0; i < table.length; i++) {
        table[i] = fromJSArray_( jsArray, h - 1, from + (i * step)
                               , Math.min(from + ((i + 1) * step), to));
        lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
      }
      return { ctor:"_Array", height:h, table:table, lengths:lengths };
    }

    Elm.Native.Array.values = {
      empty:empty,
      fromList:fromList,
      toList:toList,
      initialize:F2(initialize),
      append:F2(append),
      push:F2(push),
      slice:F3(slice),
      get:F2(get),
      set:F3(set),
      map:F2(map),
      indexedMap:F2(indexedMap),
      foldl:F3(foldl),
      foldr:F3(foldr),
      length:length,

      toJSArray:toJSArray,
      fromJSArray:fromJSArray
    };

    return elm.Native.Array.values = Elm.Native.Array.values;
}

Elm.Native.Basics = {};
Elm.Native.Basics.make = function(elm) {
  elm.Native = elm.Native || {};
  elm.Native.Basics = elm.Native.Basics || {};
  if (elm.Native.Basics.values) return elm.Native.Basics.values;

  var JS = Elm.Native.JavaScript.make(elm);
  var Utils = Elm.Native.Utils.make(elm);

  function div(a, b) {
      return (a/b)|0;
  }
  function rem(a, b) {
      return a % b;
  }
  var mod = Utils.mod;
  function logBase(base, n) {
      return Math.log(n) / Math.log(base);
  }
  function negate(n) {
      return -n;
  }
  function abs(n) {
      return n < 0 ? -n : n;
  }

  function min(a, b) {
      return Utils.cmp(a,b) < 0 ? a : b;
  }
  function max(a, b) {
      return Utils.cmp(a,b) > 0 ? a : b;
  }
  function clamp(lo, hi, n) {
      return Utils.cmp(n,lo) < 0 ? lo : Utils.cmp(n,hi) > 0 ? hi : n;
  }

  function xor(a, b) {
      return a !== b;
  }
  function not(b) {
      return !b;
  }
  function isInfinite(n) {
      return n === Infinity || n === -Infinity
  }

  function truncate(n) {
      return n|0;
  }

  function degrees(d) {
      return d * Math.PI / 180;
  }
  function turns(t) {
      return 2 * Math.PI * t;
  }
  function fromPolar(point) {
      var r = point._0;
      var t = point._1;
      return Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
  }
  function toPolar(point) {
      var x = point._0;
      var y = point._1;
      return Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y,x));
  }

  var basics = {
      div: F2(div),
      rem: F2(rem),
      mod: mod,

      pi: Math.PI,
      e: Math.E,
      cos: Math.cos,
      sin: Math.sin,
      tan: Math.tan,
      acos: Math.acos,
      asin: Math.asin,
      atan: Math.atan,
      atan2: F2(Math.atan2),

      degrees:  degrees,
      turns:  turns,
      fromPolar:  fromPolar,
      toPolar:  toPolar,

      sqrt: Math.sqrt,
      logBase: F2(logBase),
      negate: negate,
      abs: abs,
      min: F2(min),
      max: F2(max),
      clamp: F3(clamp),
      compare: Utils.compare,

      xor: F2(xor),
      not: not,

      truncate: truncate,
      ceiling: Math.ceil,
      floor: Math.floor,
      round: Math.round,
      toFloat: function(x) { return x; },
      isNaN: isNaN,
      isInfinite: isInfinite
  };

  return elm.Native.Basics.values = basics;
};
Elm.Native.Bitwise = {};
Elm.Native.Bitwise.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Bitwise = elm.Native.Bitwise || {};
    if (elm.Native.Bitwise.values) return elm.Native.Bitwise.values;

    function and(a,b) { return a & b; }
    function or (a,b) { return a | b; }
    function xor(a,b) { return a ^ b; }
    function not(a) { return ~a; }
    function sll(a,offset) { return a << offset; }
    function sra(a,offset) { return a >> offset; }
    function srl(a,offset) { return a >>> offset; }

    return elm.Native.Bitwise.values = {
        and: F2(and),
        or : F2(or ),
        xor: F2(xor),
        complement: not,
        shiftLeft           : F2(sll),
        shiftRightArithmatic: F2(sra),
        shiftRightLogical   : F2(srl)
    };
    
};
Elm.Native.Char = {};
Elm.Native.Char.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Char = elm.Native.Char || {};
    if (elm.Native.Char.values) return elm.Native.Char.values;

    var Utils = Elm.Native.Utils.make(elm);

    function isBetween(lo,hi) { return function(chr) {
	var c = chr.charCodeAt(0);
	return lo <= c && c <= hi;
    };
                              }
    var isDigit = isBetween('0'.charCodeAt(0),'9'.charCodeAt(0));
    var chk1 = isBetween('a'.charCodeAt(0),'f'.charCodeAt(0));
    var chk2 = isBetween('A'.charCodeAt(0),'F'.charCodeAt(0));

    return elm.Native.Char.values = {
        fromCode : function(c) { return String.fromCharCode(c); },
        toCode   : function(c) { return c.toUpperCase().charCodeAt(0); },
        toUpper  : function(c) { return Utils.chr(c.toUpperCase()); },
        toLower  : function(c) { return Utils.chr(c.toLowerCase()); },
        toLocaleUpper : function(c) { return Utils.chr(c.toLocaleUpperCase()); },
        toLocaleLower : function(c) { return Utils.chr(c.toLocaleLowerCase()); },
        isLower    : isBetween('a'.charCodeAt(0),'z'.charCodeAt(0)),
        isUpper    : isBetween('A'.charCodeAt(0),'Z'.charCodeAt(0)),
        isDigit    : isDigit,
        isOctDigit : isBetween('0'.charCodeAt(0),'7'.charCodeAt(0)),
        isHexDigit : function(c) { return isDigit(c) || chk1(c) || chk2(c); }
    };
};
Elm.Native.Color = {};
Elm.Native.Color.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Color = elm.Native.Color || {};
    if (elm.Native.Color.values) return elm.Native.Color.values;

    var Utils = Elm.Native.Utils.make(elm);

    function toCss(c) {
        var format = '';
        var colors = '';
        if (c.ctor === 'RGBA') {
            format = 'rgb';
            colors = c._0 + ', ' + c._1 + ', ' + c._2;
        } else {
            format = 'hsl';
            colors = (c._0 * 180 / Math.PI) + ', ' +
                     (c._1 * 100) + '%, ' +
                     (c._2 * 100) + '%';
        }
        if (c._3 === 1) {
            return format + '(' + colors + ')';
        } else {
            return format + 'a(' + colors + ', ' + c._3 + ')';
        }
    }

    return elm.Native.Color.values = {
        toCss:toCss
    };

};
Elm.Native.Date = {};
Elm.Native.Date.make = function(elm) {
 elm.Native = elm.Native || {};
 elm.Native.Date = elm.Native.Date || {};
 if (elm.Native.Date.values) return elm.Native.Date.values;

 var Maybe = Elm.Maybe.make(elm);

 function dateNow() { return new window.Date; }
 function readDate(str) {
     var d = new window.Date(str);
     if (isNaN(d.getTime())) return Maybe.Nothing;
     return Maybe.Just(d);
 }

 var dayTable = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
 var monthTable = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
		   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]; 

 return elm.Native.Date.values = {
     read    : readDate,
     year    : function(d) { return d.getFullYear(); },
     month   : function(d) { return { ctor:monthTable[d.getMonth()] }; },
     day     : function(d) { return d.getDate(); },
     hour    : function(d) { return d.getHours(); },
     minute  : function(d) { return d.getMinutes(); },
     second  : function(d) { return d.getSeconds(); },
     toTime  : function(d) { return d.getTime(); },
     fromTime: function(t) { return new window.Date(t); },
     dayOfWeek : function(d) { return { ctor:dayTable[d.getDay()] }; }
 };

};
Elm.Native.Debug = {};
Elm.Native.Debug.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Debug = elm.Native.Debug || {};
    if (elm.Native.Debug.values) return elm.Native.Debug.values;
    if ('values' in Elm.Native.Debug)
        return elm.Native.Debug.values = Elm.Native.Debug.values;

    var show = Elm.Native.Show.make(elm).show;
    var replace = Elm.Native.Utils.make(elm).replace;

    function log(tag, value) {
        var msg = tag + ': ' + show(value);
        var process = process || {};
        if (process.stdout) {
            process.stdout.write(msg);
        } else {
            console.log(msg);
        }
        return value;
    }

    function tracePath(debugId, form) {
        return replace([["debugTracePathId",debugId]], form);
    }

    function WatchTracker() {
        this.frames = [{}];
        this.clear = function() {
            this.watches = {};
        };
        this.pushFrame = function() {
            var lastFrame = this.frames[this.frames.length - 1];
            this.frames.push(lastFrame);
        }
        this.notify = function(tag, value) {
            this.frames[this.frames.length - 1][tag] = value;
        };
    }
    var watchTracker = new WatchTracker();

    function watch(tag, value) {
        watchTracker.notify(tag, value);
        return value;
    }

    function watchSummary(tag, f, value) {
        watchTracker.notify(tag, f(value));
        return value;
    }

    Elm.Native.Debug.values = {
        tracePath: F2(tracePath),
        log: F2(log),
        watch: F2(watch),
        watchSummary:F3(watchSummary),
        watchTracker: watchTracker
    };
    return elm.Native.Debug.values = Elm.Native.Debug.values;
};
Elm.Native.Error = {};
Elm.Native.Error.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Error = elm.Native.Error || {};
    if (elm.Native.Error.values) return elm.Native.Error.values;

    function indent(lines) {
        var msg = '';
        for (var i = 0; i < lines.length; ++i) {
            msg += '<br/>&nbsp; &nbsp; ' + lines[i];
        }
        return msg;
    }

    function Case(moduleName, span) { 
	var msg = indent(['Non-exhaustive pattern match in case-expression.',
                          'Make sure your patterns cover every case!']);
	throw new Error('Runtime error in module ' + moduleName + ' (' + span + '):' + msg);
    }

    function If(moduleName, span) { 
	var msg = indent(['Non-exhaustive pattern match in multi-way-if expression.',
                          'It is best to use \'otherwise\' as the last branch of multi-way-if.']);
	throw new Error('Runtime error in module ' + moduleName + ' (' + span + '):' + msg);
    }

    function raise(str) { throw new Error(str); }

    return elm.Native.Error.values = { Case: Case, If: If, raise: raise };
};

function F2(fun) {
  function wrapper(a) { return function(b) { return fun(a,b) } }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun) {
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a,b,c) }}
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a,b,c,d) }}}
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a,b,c,d,e) }}}}
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
      return fun(a,b,c,d,e,f) }}}}}
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
      return function(g) { return fun(a,b,c,d,e,f,g) }}}}}}
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
	return function(g) { return function(h) {return fun(a,b,c,d,e,f,g,h)}}}}}}}
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
	return function(g) { return function(h) { return function(i) {
        return fun(a,b,c,d,e,f,g,h,i) }}}}}}}}
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun,a,b) {
  return fun.arity === 2 ? fun.func(a,b) : fun(a)(b);
}
function A3(fun,a,b,c) {
  return fun.arity === 3 ? fun.func(a,b,c) : fun(a)(b)(c);
}
function A4(fun,a,b,c,d) {
  return fun.arity === 4 ? fun.func(a,b,c,d) : fun(a)(b)(c)(d);
}
function A5(fun,a,b,c,d,e) {
  return fun.arity === 5 ? fun.func(a,b,c,d,e) : fun(a)(b)(c)(d)(e);
}
function A6(fun,a,b,c,d,e,f) {
  return fun.arity === 6 ? fun.func(a,b,c,d,e,f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun,a,b,c,d,e,f,g) {
  return fun.arity === 7 ? fun.func(a,b,c,d,e,f,g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun,a,b,c,d,e,f,g,h) {
  return fun.arity === 8 ? fun.func(a,b,c,d,e,f,g,h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun,a,b,c,d,e,f,g,h,i) {
  return fun.arity === 9 ? fun.func(a,b,c,d,e,f,g,h,i)
                         : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}
Elm.Native.JavaScript = {};
Elm.Native.JavaScript.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.JavaScript = elm.Native.JavaScript || {};
    if (elm.Native.JavaScript.values) return elm.Native.JavaScript.values;

    var List = Elm.Native.List.make(elm);
    var Render = ElmRuntime.use(ElmRuntime.Render.Element);

    function fromJS(v) {
        var type = typeof v;
        if (type === 'number' ) return v;
        if (type === 'boolean') return v;
        if (type === 'string' ) return v;
        if (v instanceof Array) {
            var arr = [];
            var len = v.length;
            for (var i = 0; i < len; ++i) {
                var x = fromJS(v[i]);
                if (x !== null) arr.push(x);
            }
            return List.fromArray(arr);
        }
        if (type === 'object') {
            var rec = { _:{} };
            for (var f in v) {
                var x = fromJS(v[f]);
                if (x !== null) rec[f] = x;
            }
            return rec;
        }
        return null;
    }

    function toJS(v) {
        var type = typeof v;
        if (type === 'number' || type === 'boolean' || type === 'string') return v;
        if (type === 'object' && '_' in v) {
            var obj = {};
            for (var k in v) {
                var x = toJS(v[k]);
                if (x !== null) obj[k] = x;
            }
            return obj;
        }
        if (type === 'object' && (v.ctor === '::' || v.ctor === '[]')) {
            var array = List.toArray(v);
            for (var i = array.length; i--; ) {
                array[i] = toJS(array[i]);
            }
            return array;
        }
        return null;
    }

    function fromRecord(r) {
        if (typeof r === 'object' && '_' in r) {
            return toJS(r);
        }
        throw new Error("'fromRecord' must be called on a record.");
    }

    return elm.Native.JavaScript.values = {
        toRecord    : fromJS,
        fromRecord  : fromRecord
    };

};
Elm.Native.Json = {};
Elm.Native.Json.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Json = elm.Native.Json || {};
    if (elm.Native.Json.values) return elm.Native.Json.values;

    var Maybe = Elm.Maybe.make(elm);
    var Dict = Elm.Dict.make(elm);
    var List = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    function toJS(v) {
        switch (v.ctor) {
        case 'Null'   : return null;
        case 'String' : return v._0;
        case 'Number' : return v._0;
        case 'Boolean': return v._0;
        case 'Object' :
            var obj = {};
            var array = List.toArray(Dict.toList(v._0));
            for (var i = array.length; i--; ) {
                var entry = array[i];
                obj[entry._0] = toJS(entry._1);
            }
            return obj;
        case 'Array'  :
            var array = List.toArray(v._0);
            for (var i = array.length; i--; ) {
	        array[i] = toJS(array[i]);
            }
            return array;
        }
    }

    function toString(sep, value) {
        return JSON.stringify(toJS(value), null, sep);
    }

    function fromJS(v) {
        switch (typeof v) {
        case 'string' : return { ctor:"String" , _0: v };
        case 'number' : return { ctor:"Number" , _0: v };
        case 'boolean': return { ctor:"Boolean", _0: v };
        case 'object' :
            if (v === null) return { ctor:"Null" };
            if (v instanceof Array) {
                var array = new Array(v.length);
                for (var i = v.length; i--; ) {
                    array[i] = fromJS(v[i]);
                }
	        return {
                    ctor:"Array",
                    _0: List.fromArray(array)
                };
            }
            var array = [];
            for (var key in v) {
                var value = fromJS(v[key]);
                array.push(Utils.Tuple2(key, value));
            }
            var list = List.fromArray(array);
            return {
                ctor:"Object",
                _0: Dict.fromList(list)
            };
        }
    }

    function fromString(str) {
        try {
	    return Maybe.Just(fromJS(JSON.parse(str)));
        } catch (e) {
	    return Maybe.Nothing;
        }
    }

    return elm.Native.Json.values = {
        toString   : F2(toString),
        fromString : fromString,
        fromJS     : fromJS,
        toJS       : toJS
    };

};
Elm.Native.List = {};
Elm.Native.List.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.List = elm.Native.List || {};
    if (elm.Native.List.values) return elm.Native.List.values;
    if ('values' in Elm.Native.List)
        return elm.Native.List.values = Elm.Native.List.values;

    var Utils = Elm.Native.Utils.make(elm);

    // TODO: Improve Nil handling
    // We can change places like:  if (xs.ctor === '[]') ... to if (xs === Nil) ...
    // but only if we're confident Nil can only be defined once.
    // Currently (27Mar2013) each module can have different instantiations, so multiple Nil objects can exist
    // (and if they're used interchangeably then direct object comparison fails where ctor doesn't).
    // So, this can only be fixed when modules initialisation is also fixed.
    // The performance overhead of the .ctor calls is 5-10% according to jsperf (depending on fn + list size)
    // (on firefox 19)

    var Nil = { ctor:'[]' };

    // using freeze for every cons would be nice but is a huge (9x on firefox 19)
    // performance penalty
    function Cons(hd,tl) { return { ctor:"::", _0:hd, _1:tl }; }

    function throwError(f) {
        throw new Error("Function '" + f + "' expects a non-empty list!");
    }

    function toArray(xs) {
        var out = [];
        while (xs.ctor !== '[]') {
            out.push(xs._0);
            xs = xs._1;
        }
        return out;
    }

    function fromArray(arr) {
        var out = Nil;
        for (var i = arr.length; i--; ) {
            out = Cons(arr[i], out);
        }
        return out;
    }

    function range(lo,hi) {
        var lst = Nil;
        if (lo <= hi) {
            do { lst = Cons(hi,lst) } while (hi-->lo);
        }
        return lst
    }

    function append(xs,ys) {
        // append Text
        if (xs.text || ys.text) {
            return Utils.txt(Utils.makeText(xs) + Utils.makeText(ys));
        }

        // append Strings
        if (typeof xs === "string") return xs + ys;

        // append Lists
        if (xs.ctor === '[]') { return ys; }
        var root = Cons(xs._0, Nil);
        var curr = root;
        xs = xs._1;
        while (xs.ctor !== '[]') {
	    curr._1 = Cons(xs._0, Nil);
	    xs = xs._1;
	    curr = curr._1;
        }
        curr._1 = ys;
        return root;
    }

    function head(v) { return v.ctor === '[]' ? throwError('head') : v._0; }
    function tail(v) { return v.ctor === '[]' ? throwError('tail') : v._1; }

    function last(xs) {
        if (xs.ctor === '[]') { throwError('last'); }
        var out = xs._0;
        while (xs.ctor !== '[]') {
            out = xs._0;
            xs = xs._1;
        }
        return out;
    }

    function map(f, xs) {
        var arr = [];
        while (xs.ctor !== '[]') {
            arr.push(f(xs._0));
            xs = xs._1;
        }
        return fromArray(arr);
    }

    // f defined similarly for both foldl and foldr (NB: different from Haskell)
    // ie, foldl : (a -> b -> b) -> b -> [a] -> b
    function foldl(f, b, xs) {
        var acc = b;
        while (xs.ctor !== '[]') {
            acc = A2(f, xs._0, acc);
            xs = xs._1;
        }
        return acc;
    }

    function foldr(f, b, xs) {
        var arr = toArray(xs);
        var acc = b;
        for (var i = arr.length; i--; ) {
            acc = A2(f, arr[i], acc);
        }
        return acc;
    }

    function foldl1(f, xs) {
        return xs.ctor === '[]' ? throwError('foldl1') : foldl(f, xs._0, xs._1);
    }

    function foldr1(f, xs) {
        if (xs.ctor === '[]') { throwError('foldr1'); }
        var arr = toArray(xs);
        var acc = arr.pop();
        for (var i = arr.length; i--; ) {
            acc = A2(f, arr[i], acc);
        }
        return acc;
    }

    function scanl(f, b, xs) {
        var arr = toArray(xs);
        arr.unshift(b);
        var len = arr.length;
        for (var i = 1; i < len; ++i) {
            arr[i] = A2(f, arr[i], arr[i-1]);
        }
        return fromArray(arr);
    }

    function scanl1(f, xs) {
        return xs.ctor === '[]' ? throwError('scanl1') : scanl(f, xs._0, xs._1);
    }

    function filter(pred, xs) {
        var arr = [];
        while (xs.ctor !== '[]') {
            if (pred(xs._0)) { arr.push(xs._0); }
            xs = xs._1;
        }
        return fromArray(arr);
    }

    function length(xs) {
        var out = 0;
        while (xs.ctor !== '[]') {
            out += 1;
            xs = xs._1;
        }
        return out;
    }

    function member(x, xs) {
        while (xs.ctor !== '[]') {
            if (Utils.eq(x,xs._0)) return true;
            xs = xs._1;
        }
        return false;
    }

    function reverse(xs) { return fromArray(toArray(xs).reverse()); }

    function concat(xss) {
        if (xss.ctor === '[]') return xss;
        var arr = toArray(xss);
        var xs = arr[arr.length-1];
        for (var i = arr.length-1; i--; ) {
	    xs = append(arr[i], xs);
        }
        return xs;
    }

    function all(pred, xs) {
        while (xs.ctor !== '[]') {
            if (!pred(xs._0)) return false;
            xs = xs._1;
        }
        return true;
    }

    function any(pred, xs) {
        while (xs.ctor !== '[]') {
            if (pred(xs._0)) return true;
            xs = xs._1;
        }
        return false;
    }

    function zip(xs, ys) {
        var arr = [];
        while (xs.ctor !== '[]' && ys.ctor !== '[]') {
            arr.push(Utils.Tuple2(xs._0, ys._0));
            xs = xs._1;
            ys = ys._1;
        }
        return fromArray(arr);
    }

    function zipWith(f, xs, ys) {
        var arr = [];
        while (xs.ctor !== '[]' && ys.ctor !== '[]') {
            arr.push(A2(f, xs._0, ys._0));
            xs = xs._1;
            ys = ys._1;
        }
        return fromArray(arr);
    }

    function zipWith3(f, xs, ys, zs) {
        var arr = [];
        while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]') {
            arr.push(A3(f, xs._0, ys._0, zs._0));
            xs = xs._1;
            ys = ys._1;
            zs = zs._1;
        }
        return fromArray(arr);
    }

    function zipWith4(f, ws, xs, ys, zs) {
        var arr = [];
        while (   ws.ctor !== '[]'
               && xs.ctor !== '[]'
               && ys.ctor !== '[]'
               && zs.ctor !== '[]')
        {
            arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
            ws = ws._1;
            xs = xs._1;
            ys = ys._1;
            zs = zs._1;
        }
        return fromArray(arr);
    }

    function zipWith5(f, vs, ws, xs, ys, zs) {
        var arr = [];
        while (   vs.ctor !== '[]'
               && ws.ctor !== '[]'
               && xs.ctor !== '[]'
               && ys.ctor !== '[]'
               && zs.ctor !== '[]')
        {
            arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
            vs = vs._1;
            ws = ws._1;
            xs = xs._1;
            ys = ys._1;
            zs = zs._1;
        }
        return fromArray(arr);
    }

    function sort(xs) {
        return fromArray(toArray(xs).sort(Utils.cmp));
    }

    function sortBy(f, xs) {
        return fromArray(toArray(xs).sort(function(a,b){
            return Utils.cmp(f(a), f(b));
        }));
    }

    function sortWith(f, xs) {
        return fromArray(toArray(xs).sort(function(a,b){
            var ord = f(a)(b).ctor;
            return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
        }));
    }

    function nth(xs, n) {
        return toArray(xs)[n];
    }

    function take(n, xs) {
        var arr = [];
        while (xs.ctor !== '[]' && n > 0) {
            arr.push(xs._0);
            xs = xs._1;
            --n;
        }
        return fromArray(arr);
    }

    function drop(n, xs) {
        while (xs.ctor !== '[]' && n > 0) {
            xs = xs._1;
            --n;
        }
        return xs;
    }

    function repeat(n, x) {
        var arr = [];
        var pattern = [x];
        while (n > 0) {
            if (n & 1) arr = arr.concat(pattern);
            n >>= 1, pattern = pattern.concat(pattern);
        }
        return fromArray(arr);
    }

    function join(sep, xss) {
        if (sep.text) {
            sep = Utils.makeText(sep);
            xss = toArray(xss);
            for (var i = xss.length; i--; ) {
                xss[i] = Utils.makeText(xss[i]);
            }
            return Utils.txt(xss.join(sep));
        }
        if (typeof sep === 'string') return toArray(xss).join(sep);
        if (xss.ctor === '[]') return Nil;
        var s = toArray(sep);
        var out = toArray(xss._0);
        xss = xss._1;
        while (xss.ctor !== '[]') {
            out = out.concat(s, toArray(xss._0));
            xss = xss._1;
        }
        return fromArray(out);
    }

    /*
     * Only to be used internally; do some side effects for each elem
     */
    function each(action, xs) {
        while(xs.ctor !== '[]') {
            action(xs._0);
            xs = xs._1;
        }
    }

    Elm.Native.List.values = {
        Nil:Nil,
        Cons:Cons,
        cons:F2(Cons),
        toArray:toArray,
        fromArray:fromArray,
        range:range,
        append:append,

        head:head,
        tail:tail,
        last:last,

        map:F2(map),
        foldl:F3(foldl),
        foldr:F3(foldr),

        foldl1:F2(foldl1),
        foldr1:F2(foldr1),
        scanl:F3(scanl),
        scanl1:F2(scanl1),
        filter:F2(filter),
        length:length,
        member:F2(member),
        reverse:reverse,
        concat:concat,

        all:F2(all),
        any:F2(any),
        zipWith :F3(zipWith ),
        zipWith3:F4(zipWith3),
        zipWith4:F5(zipWith4),
        zipWith5:F6(zipWith5),
        zip:F2(zip),
        sort:sort,
        sortBy:F2(sortBy),
        sortWith:F2(sortWith),
        nth:F2(nth),
        take:F2(take),
        drop:F2(drop),
        repeat:F2(repeat),

        join:F2(join),

        each:each
    };
    return elm.Native.List.values = Elm.Native.List.values;

};
Elm.Native.Ports = {};
Elm.Native.Ports.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Ports = elm.Native.Ports || {};
    if (elm.Native.Ports.values) return elm.Native.Ports.values;

    var Signal = Elm.Signal.make(elm);

    function incomingSignal(converter) {
        converter.isSignal = true;
        return converter;
    }

    function outgoingSignal(converter) {
        return function(signal) {
            var subscribers = []
            function subscribe(handler) {
                subscribers.push(handler);
            }
            function unsubscribe(handler) {
                subscribers.pop(subscribers.indexOf(handler));
            }
            A2( Signal.lift, function(value) {
                var val = converter(value);
                var len = subscribers.length;
                for (var i = 0; i < len; ++i) {
                    subscribers[i](val);
                }
            }, signal);
            return { subscribe:subscribe, unsubscribe:unsubscribe };
        }
    }

    function portIn(name, converter) {
        var jsValue = elm.ports.incoming[name];
        if (jsValue === undefined) {
            throw new Error("Initialization Error: port '" + name +
                            "' was not given an input!");
        }
        elm.ports.uses[name] += 1;
        try {
            var elmValue = converter(jsValue);
        } catch(e) {
            throw new Error("Initialization Error on port '" + name + "': \n" + e.message);
        }

        // just return a static value if it is not a signal
        if (!converter.isSignal) {
            return elmValue;
        }

        // create a signal if necessary
        var signal = Signal.constant(elmValue);
        function send(jsValue) {
            try {
                var elmValue = converter(jsValue);
            } catch(e) {
                throw new Error("Error sending to port '" + name + "': \n" + e.message);
            }
            setTimeout(function() {
                elm.notify(signal.id, elmValue);
            }, 0);
        }
        elm.ports.outgoing[name] = { send:send };
        return signal;
    }

    function portOut(name, converter, value) {
        try {
            elm.ports.outgoing[name] = converter(value);
        } catch(e) {
            throw new Error("Initialization Error on port '" + name + "': \n" + e.message);
        }
        return value;
    }

    return elm.Native.Ports.values = {
        incomingSignal: incomingSignal,
        outgoingSignal: outgoingSignal,
        portOut: portOut,
        portIn: portIn
    };
};
Elm.Native.Regex = {};
Elm.Native.Regex.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Regex = elm.Native.Regex || {};
    if (elm.Native.Regex.values) return elm.Native.Regex.values;
    if ('values' in Elm.Native.Regex)
        return elm.Native.Regex.values = Elm.Native.Regex.values;

    var List = Elm.Native.List.make(elm);
    var Maybe = Elm.Maybe.make(elm);

    function escape(str) {
        return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
    }
    function caseInsensitive(re) {
        return new RegExp(re.source, 'gi');
    }
    function regex(raw) {
        return new RegExp(raw, 'g');
    }

    function contains(re, string) {
        return string.match(re) !== null;
    }

    function find(n, re, str) {
        n = n.ctor === "All" ? Infinity : n._0;
        var out = [];
        var number = 0;
        var string = str;
        var result;
        while (number++ < n && (result = re.exec(string))) {
            var i = result.length - 1;
            var subs = new Array(i);
            while (i > 0) {
                var submatch = result[i];
                subs[--i] = submatch === undefined
                    ? Maybe.Nothing
                    : Maybe.Just(submatch);
            }
            out.push({
                _:{},
                match: result[0],
                submatches: List.fromArray(subs),
                index: result.index,
                number: number
            });
        }
        return List.fromArray(out);
    }

    function replace(n, re, replacer, string) {
        n = n.ctor === "All" ? Infinity : n._0;
        var count = 0;
        function jsReplacer(match) {
            if (count++ > n) return match;
            var i = arguments.length-3;
            var submatches = new Array(i);
            while (i > 0) {
                var submatch = arguments[i];
                submatches[--i] = submatch === undefined
                    ? Maybe.Nothing
                    : Maybe.Just(submatch);
            }
            return replacer({
                _:{},
                match:match,
                submatches:List.fromArray(submatches),
                index:arguments[i-1],
                number:count
            });
        }
        return string.replace(re, jsReplacer);
    }

    function split(n, re, str) {
        if (n === Infinity) {
            return List.fromArray(string.split(re));
        }
        var string = str;
        var result;
        var out = [];
        var start = re.lastIndex;
        while (n--) {
            if (!(result = re.exec(string))) break;
            out.push(string.slice(start, result.index));
            start = re.lastIndex;
        }
        out.push(string.slice(start));
        return List.fromArray(out);
    }

    return Elm.Native.Regex.values = {
        regex: regex,
        caseInsensitive: caseInsensitive,
        escape: escape,

        contains: F2(contains),
        find: F3(find),
        replace: F4(replace),
        split: F3(split)
    };
};
Elm.Native.Show = {};
Elm.Native.Show.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Show = elm.Native.Show || {};
    if (elm.Native.Show.values) return elm.Native.Show.values;

    var NList = Elm.Native.List.make(elm);
    var Array = Elm.Array.make(elm);
    var List = Elm.List.make(elm);
    var Dict = Elm.Dict.make(elm);
    var Tuple2 = Elm.Native.Utils.make(elm).Tuple2;

    var toString = function(v) {
        var type = typeof v;
        if (type === "function") {
            var name = v.func ? v.func.name : v.name;
            return '<function' + (name === '' ? '' : ': ') + name + '>';
        }
        else if (type === "boolean") {
            return v ? "True" : "False";
        }
        else if (type === "number") {
            return v + "";
        }
        else if ((v instanceof String) && v.isChar) {
            return "'" + addSlashes(v) + "'";
        }
        else if (type === "string") {
            return '"' + addSlashes(v) + '"';
        }
        else if (type === "object" && '_' in v && probablyPublic(v)) {
            var output = [];
            for (var k in v._) {
                for (var i = v._[k].length; i--; ) {
                    output.push(k + " = " + toString(v._[k][i]));
                }
            }
            for (var k in v) {
                if (k === '_') continue;
                output.push(k + " = " + toString(v[k]));
            }
            if (output.length === 0) {
                return "{}";
            }
            return "{ " + output.join(", ") + " }";
        }
        else if (type === "object" && 'ctor' in v) {
            if (v.ctor.substring(0,6) === "_Tuple") {
                var output = [];
                for (var k in v) {
                    if (k === 'ctor') continue;
                    output.push(toString(v[k]));
                }
                return "(" + output.join(",") + ")";
            }
            else if (v.ctor === "_Array") {
                var list = Array.toList(v);
                return "Array.fromList " + toString(list);
            }
            else if (v.ctor === "::") {
                var output = '[' + toString(v._0);
                v = v._1;
                while (v.ctor === "::") {
                    output += "," + toString(v._0);
                    v = v._1;
                }
                return output + ']';
            }
            else if (v.ctor === "[]") {
                return "[]";
            }
            else if (v.ctor === "RBNode" || v.ctor === "RBEmpty") {
                var cons = F3(function(k,v,acc){return NList.Cons(Tuple2(k,v),acc)});
                var list = A3(Dict.foldr, cons, NList.Nil, v);
                var name = "Dict";
                if (list.ctor === "::" && list._0._1.ctor === "_Tuple0") {
                    name = "Set";
                    list = A2(List.map, function(x){return x._0}, list);
                }
                return name + ".fromList " + toString(list);
            }
            else {
                var output = "";
                for (var i in v) {
                    if (i === 'ctor') continue;
                    var str = toString(v[i]);
                    var parenless = str[0] === '{' || str[0] === '<' || str.indexOf(' ') < 0;
                    output += ' ' + (parenless ? str : '(' + str + ')');
                }
                return v.ctor + output;
            }
        }
        if (type === 'object' && 'recv' in v) {
            return '<signal>';
        }
        return "<internal structure>";
    };

    function addSlashes(str) {
        return str.replace(/\\/g, '\\\\')
                  .replace(/\n/g, '\\n')
                  .replace(/\t/g, '\\t')
                  .replace(/\r/g, '\\r')
                  .replace(/\v/g, '\\v')
                  .replace(/\0/g, '\\0')
                  .replace(/\'/g, "\\'")
                  .replace(/\"/g, '\\"');
    }

    function probablyPublic(v) {
        var keys = Object.keys(v);
        var len = keys.length;
        if (len === 3
            && 'props' in v
            && 'element' in v) return false;
        if (len === 5
            && 'horizontal' in v
            && 'vertical' in v
            && 'x' in v
            && 'y' in v) return false;
        if (len === 7
            && 'theta' in v
            && 'scale' in v
            && 'x' in v
            && 'y' in v
            && 'alpha' in v
            && 'form' in v) return false;
        return true;
    }

    return elm.Native.Show.values = { show:toString };
};
Elm.Native.String = {};
Elm.Native.String.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.String = elm.Native.String || {};
    if (elm.Native.String.values) return elm.Native.String.values;
    if ('values' in Elm.Native.String)
        return elm.Native.String.values = Elm.Native.String.values;

    var Char = Elm.Char.make(elm);
    var Maybe = Elm.Maybe.make(elm);
    var List = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    function isEmpty(str) {
        return str.length === 0;
    }
    function cons(chr,str) {
        return chr + str;
    }
    function uncons(str) {
        var hd;
        return (hd = str[0]) ? Maybe.Just(Utils.Tuple2(Utils.chr(hd), str.slice(1)))
                              : Maybe.Nothing;
    }
    function append(a,b) {
        return a + b;
    }
    function concat(strs) {
        return List.toArray(strs).join('');
    }
    function length(str) {
        return str.length;
    }
    function map(f,str) {
        var out = str.split('');
        for (var i = out.length; i--; ) {
            out[i] = f(Utils.chr(out[i]));
        }
        return out.join('');
    }
    function filter(pred,str) {
        return str.split('').map(Utils.chr).filter(pred).join('');
    }
    function reverse(str) {
        return str.split('').reverse().join('');
    }
    function foldl(f,b,str) {
        var len = str.length;
        for (var i = 0; i < len; ++i) {
            b = A2(f, Utils.chr(str[i]), b);
        }
        return b;
    }
    function foldr(f,b,str) {
        for (var i = str.length; i--; ) {
            b = A2(f, Utils.chr(str[i]), b);
        }
        return b;
    }

    function split(sep, str) {
        return List.fromArray(str.split(sep));
    }
    function join(sep, strs) {
        return List.toArray(strs).join(sep);
    }
    function repeat(n, str) {
        var result = '';
        while (n > 0) {
            if (n & 1) result += str;
            n >>= 1, str += str;
        }
        return result;
    }

    function slice(start, end, str) {
        return str.slice(start,end);
    }
    function left(n, str) {
        return n < 1 ? "" : str.slice(0,n);
    }
    function right(n, str) {
        return n < 1 ? "" : str.slice(-n);
    }
    function dropLeft(n, str) {
        return n < 1 ? str : str.slice(n);
    }
    function dropRight(n, str) {
        return n < 1 ? str : str.slice(0,-n);
    }

    function pad(n,chr,str) {
        var half = (n - str.length) / 2;
        return repeat(Math.ceil(half),chr) + str + repeat(half|0,chr);
    }
    function padRight(n,chr,str) {
        return str + repeat(n - str.length, chr);
    }
    function padLeft(n,chr,str) {
        return repeat(n - str.length, chr) + str;
    }

    function trim(str) {
        return str.trim();
    }
    function trimLeft(str) {
        return str.trimLeft();
    }
    function trimRight(str) {
        return str.trimRight();
    }

    function words(str) {
        return List.fromArray(str.trim().split(/\s+/g));
    }
    function lines(str) {
        return List.fromArray(str.split(/\r\n|\r|\n/g));
    }

    function toUpper(str) {
        return str.toUpperCase();
    }
    function toLower(str) {
        return str.toLowerCase();
    }

    function any(pred, str) {
        for (var i = str.length; i--; ) {
            if (pred(Utils.chr(str[i]))) return true;
        }
        return false;
    }
    function all(pred, str) {
        for (var i = str.length; i--; ) {
            if (!pred(Utils.chr(str[i]))) return false;
        }
        return true;
    }

    function contains(sub, str) {
        return str.indexOf(sub) > -1;
    }
    function startsWith(sub, str) {
        return str.indexOf(sub) === 0;
    }
    function endsWith(sub, str) {
        return str.length >= sub.length &&
               str.lastIndexOf(sub) === str.length - sub.length;
    }
    function indexes(sub, str) {
        var subLen = sub.length;
        var i = 0;
        var is = [];
        while ((i = str.indexOf(sub, i)) > -1) {
            is.push(i);
            i = i + subLen;
        }
        return List.fromArray(is);
    }

    function toInt(s) {
        var len = s.length;
        if (len === 0) { return Maybe.Nothing; }
        var start = 0;
        if (s[0] == '-') {
            if (len === 1) { return Maybe.Nothing; }
            start = 1;
        }
        for (var i = start; i < len; ++i) {
            if (!Char.isDigit(s[i])) { return Maybe.Nothing; }
        }
        return Maybe.Just(parseInt(s, 10));
    }

    function toFloat(s) {
        var len = s.length;
        if (len === 0) { return Maybe.Nothing; }
        var start = 0;
        if (s[0] == '-') {
            if (len === 1) { return Maybe.Nothing; }
            start = 1;
        }
        var dotCount = 0;
        for (var i = start; i < len; ++i) {
            if (Char.isDigit(s[i])) { continue; }
            if (s[i] === '.') {
                dotCount += 1;
                if (dotCount <= 1) { continue; }
            }
            return Maybe.Nothing;
        }
        return Maybe.Just(parseFloat(s));
    }

    function toList(str) {
        return List.fromArray(str.split('').map(Utils.chr));
    }
    function fromList(chars) {
        return List.toArray(chars).join('');
    }

    return Elm.Native.String.values = {
        isEmpty: isEmpty,
        cons: F2(cons),
        uncons: uncons,
        append: F2(append),
        concat: concat,
        length: length,
        map: F2(map),
        filter: F2(filter),
        reverse: reverse,
        foldl: F3(foldl),
        foldr: F3(foldr),

        split: F2(split),
        join: F2(join),
        repeat: F2(repeat),

        slice: F3(slice),
        left: F2(left),
        right: F2(right),
        dropLeft: F2(dropLeft),
        dropRight: F2(dropRight),

        pad: F3(pad),
        padLeft: F3(padLeft),
        padRight: F3(padRight),

        trim: trim,
        trimLeft: trimLeft,
        trimRight: trimRight,

        words: words,
        lines: lines,

        toUpper: toUpper,
        toLower: toLower,

        any: F2(any),
        all: F2(all),

        contains: F2(contains),
        startsWith: F2(startsWith),
        endsWith: F2(endsWith),
        indexes: F2(indexes),

        toInt: toInt,
        toFloat: toFloat,
        toList: toList,
        fromList: fromList
    };
};
Elm.Native.Text = {};
Elm.Native.Text.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Text = elm.Native.Text || {};
    if (elm.Native.Text.values) return elm.Native.Text.values;

    var toCss = Elm.Native.Color.make(elm).toCss;
    var Element = Elm.Graphics.Element.make(elm);
    var List = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    function makeSpaces(s) {
        if (s.length == 0) { return s; }
        var arr = s.split('');
        if (arr[0] == ' ') { arr[0] = "&nbsp;" }      
        for (var i = arr.length; --i; ) {
            if (arr[i][0] == ' ' && arr[i-1] == ' ') {
                arr[i-1] = arr[i-1] + arr[i];
                arr[i] = '';
            }
        }
        for (var i = arr.length; i--; ) {
            if (arr[i].length > 1 && arr[i][0] == ' ') {
                var spaces = arr[i].split('');
                for (var j = spaces.length - 2; j >= 0; j -= 2) {
                    spaces[j] = '&nbsp;';
                }
                arr[i] = spaces.join('');
            }
        }
        arr = arr.join('');
        if (arr[arr.length-1] === " ") {
	    return arr.slice(0,-1) + '&nbsp;';
        }
        return arr;
    }

    function properEscape(str) {
        if (str.length == 0) return str;
        str = str //.replace(/&/g,  "&#38;")
	    .replace(/"/g,  '&#34;')
	    .replace(/'/g,  "&#39;")
	    .replace(/</g,  "&#60;")
	    .replace(/>/g,  "&#62;")
	    .replace(/\n/g, "<br/>");
        var arr = str.split('<br/>');
        for (var i = arr.length; i--; ) {
	    arr[i] = makeSpaces(arr[i]);
        }
        return arr.join('<br/>');
    }

    function toText(str) { return Utils.txt(properEscape(str)); }

    // conversions from Elm values to CSS
    function toTypefaces(list) {
        var typefaces = List.toArray(list);
        for (var i = typefaces.length; i--; ) {
            var typeface = typefaces[i];
            if (typeface.indexOf(' ') > -1) {
                typefaces[i] = "'" + typeface + "'";
            }
        }
        return typefaces.join(',');
    }
    function toLine(line) {
        var ctor = line.ctor;
        return ctor === 'Under' ? 'underline' :
               ctor === 'Over'  ? 'overline'  : 'line-through';
    }

    // setting styles of Text
    function style(style, text) {
        var newText = '<span style="color:' + toCss(style.color) + ';'
        if (style.typeface.ctor !== '[]') {
            newText += 'font-family:' + toTypefaces(style.typeface) + ';'
        }
        if (style.height.ctor !== "Nothing") {
            newText += 'font-size:' + style.height._0 + 'px;';
        }
        if (style.bold) {
            newText += 'font-weight:bold;';
        }
        if (style.italic) {
            newText += 'font-style:italic;';
        }
        if (style.line.ctor !== 'Nothing') {
            newText += 'text-decoration:' + toLine(style.line._0) + ';';
        }
        newText += '">' + Utils.makeText(text) + '</span>'
        return Utils.txt(newText);
    }
    function height(px, text) {
        return { style: 'font-size:' + px + 'px;', text:text }
    }
    function typeface(names, text) {
        return { style: 'font-family:' + toTypefaces(names) + ';', text:text }
    }
    function monospace(text) {
        return { style: 'font-family:monospace;', text:text }
    }
    function italic(text) {
        return { style: 'font-style:italic;', text:text }
    }
    function bold(text) {
        return { style: 'font-weight:bold;', text:text }
    }
    function link(href, text) {
        return { href: toText(href), text:text };
    }
    function line(line, text) {
        return { style: 'text-decoration:' + toLine(line) + ';', text:text };
    }

    function color(color, text) {
        return { style: 'color:' + toCss(color) + ';', text:text };
    }

    function block(align) {
        return function(text) {
            var raw = {
                ctor :'RawHtml',
                html : Utils.makeText(text),
                align: align,
                guid : null,
                args : []
            };
            var pos = A2(Utils.htmlHeight, 0, raw);
            return A3(Element.newElement, pos._0, pos._1, raw);
        }
    }

    function markdown(text, guid) {
        var raw = {
            ctor:'RawHtml',
            html: text,
            align: null,
            guid: guid,
            args: []
        };
        var pos = A2(Utils.htmlHeight, 0, raw);
        return A3(Element.newElement, pos._0, pos._1, raw);
    }

    return elm.Native.Text.values = {
        toText: toText,

        height : F2(height),
        italic : italic,
        bold : bold,
        line : F2(line),
        monospace : monospace,
        typeface : F2(typeface),
        color : F2(color),
        link : F2(link),
        style : F2(style),

        leftAligned  : block('left'),
        rightAligned : block('right'),
        centered     : block('center'),
        justified    : block('justify'),
        markdown     : markdown,

        toTypefaces:toTypefaces,
        toLine:toLine
    };
};
Elm.Native.Trampoline = {};
Elm.Native.Trampoline.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Trampoline = elm.Native.Trampoline || {};
    if (elm.Native.Trampoline.values) return elm.Native.Trampoline.values;

    // trampoline : Trampoline a -> a
    function trampoline(t) {
        var tramp = t;
        while(true) {
            switch(tramp.ctor) {
            case "Done":
                return tramp._0;
            case "Continue":
                tramp = tramp._0({ctor: "_Tuple0"});
                continue;
            }
        }
    }

    return elm.Native.Trampoline.values = {
        trampoline:trampoline
    };
};
Elm.Native.Transform2D = {};
Elm.Native.Transform2D.make = function(elm) {

 elm.Native = elm.Native || {};
 elm.Native.Transform2D = elm.Native.Transform2D || {};
 if (elm.Native.Transform2D.values) return elm.Native.Transform2D.values;

 var A;
 if (typeof Float32Array === 'undefined') {
     A = function(arr) {
         this.length = arr.length;
         this[0] = arr[0];
         this[1] = arr[1];
         this[2] = arr[2];
         this[3] = arr[3];
         this[4] = arr[4];
         this[5] = arr[5];
     };
 } else {
     A = Float32Array;
 }

 // layout of matrix in an array is
 //
 //   | m11 m12 dx |
 //   | m21 m22 dy |
 //   |  0   0   1 |
 //
 //  new A([ m11, m12, dx, m21, m22, dy ])

 var identity = new A([1,0,0,0,1,0]);
 function matrix(m11, m12, m21, m22, dx, dy) {
     return new A([m11, m12, dx, m21, m22, dy]);
 }
 function rotation(t) {
     var c = Math.cos(t);
     var s = Math.sin(t);
     return new A([c, -s, 0, s, c, 0]);
 }
 function rotate(t,m) {
     var c = Math.cos(t);
     var s = Math.sin(t);
     var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
     return new A([m11*c + m12*s, -m11*s + m12*c, m[2],
                   m21*c + m22*s, -m21*s + m22*c, m[5]]);
 }
 /*
 function move(xy,m) {
     var x = xy._0;
     var y = xy._1;
     var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
     return new A([m11, m12, m11*x + m12*y + m[2],
                   m21, m22, m21*x + m22*y + m[5]]);
 }
 function scale(s,m) { return new A([m[0]*s, m[1]*s, m[2], m[3]*s, m[4]*s, m[5]]); }
 function scaleX(x,m) { return new A([m[0]*x, m[1], m[2], m[3]*x, m[4], m[5]]); }
 function scaleY(y,m) { return new A([m[0], m[1]*y, m[2], m[3], m[4]*y, m[5]]); }
 function reflectX(m) { return new A([-m[0], m[1], m[2], -m[3], m[4], m[5]]); }
 function reflectY(m) { return new A([m[0], -m[1], m[2], m[3], -m[4], m[5]]); }

 function transform(m11, m21, m12, m22, mdx, mdy, n) {
     var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
     return new A([m11*n11 + m12*n21,
                   m11*n12 + m12*n22,
                   m11*ndx + m12*ndy + mdx,
                   m21*n11 + m22*n21,
                   m21*n12 + m22*n22,
                   m21*ndx + m22*ndy + mdy]);
 }
 */
 function multiply(m, n) {
     var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4], mdx = m[2], mdy = m[5];
     var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
     return new A([m11*n11 + m12*n21,
                   m11*n12 + m12*n22,
                   m11*ndx + m12*ndy + mdx,
                   m21*n11 + m22*n21,
                   m21*n12 + m22*n22,
                   m21*ndx + m22*ndy + mdy]);
 }

 return elm.Native.Transform2D.values = {
     identity:identity,
     matrix:F6(matrix),
     rotation:rotation,
     multiply:F2(multiply)
     /*
     transform:F7(transform),
     rotate:F2(rotate),
     move:F2(move),
     scale:F2(scale),
     scaleX:F2(scaleX),
     scaleY:F2(scaleY),
     reflectX:reflectX,
     reflectY:reflectY
     */
 };

};
Elm.Native.Utils = {};
Elm.Native.Utils.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Utils = elm.Native.Utils || {};
    if (elm.Native.Utils.values) return elm.Native.Utils.values;

    function eq(l,r) {
        var stack = [{'x': l, 'y': r}]
        while (stack.length > 0) {
            var front = stack.pop();
            var x = front.x;
            var y = front.y;
            if (x === y) continue;
            if (typeof x === "object") {
                var c = 0;
                for (var i in x) {
                    ++c;
                    if (i in y) {
                        if (i !== 'ctor') {
                            stack.push({ 'x': x[i], 'y': y[i] });
                        }
                    } else {
                        return false;
                    }
                }
                if ('ctor' in x) {
                    stack.push({'x': x.ctor, 'y': y.ctor});
                }
                if (c !== Object.keys(y).length) {
                    return false;
                };
            } else if (typeof x === 'function') {
                throw new Error('Equality error: general function equality is ' +
                                'undecidable, and therefore, unsupported');
            } else {
                return false;
            }
        }
        return true;
    }

    // code in Generate/JavaScript.hs depends on the particular
    // integer values assigned to LT, EQ, and GT
    var LT = -1, EQ = 0, GT = 1, ord = ['LT','EQ','GT'];
    function compare(x,y) { return { ctor: ord[cmp(x,y)+1] } }
    function cmp(x,y) {
        var ord;
        if (typeof x !== 'object'){
            return x === y ? EQ : x < y ? LT : GT;
        }
        else if (x.isChar){
            var a = x.toString();
            var b = y.toString();
            return a === b ? EQ : a < b ? LT : GT;
        }
        else if (x.ctor === "::" || x.ctor === "[]") {
            while (true) {
                if (x.ctor === "[]" && y.ctor === "[]") return EQ;
                if (x.ctor !== y.ctor) return x.ctor === '[]' ? LT : GT;
                ord = cmp(x._0, y._0);
                if (ord !== EQ) return ord;
                x = x._1;
                y = y._1;
            }
        }
        else if (x.ctor.slice(0,6) === '_Tuple') {
            var n = x.ctor.slice(6) - 0;
            var err = 'cannot compare tuples with more than 6 elements.';
            if (n === 0) return EQ;
            if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
            if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
            if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
            if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
            if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
            if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
            if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
            return EQ;
        }
        else {
            throw new Error('Comparison error: comparison is only defined on ints, ' +
                            'floats, times, chars, strings, lists of comparable values, ' +
                            'and tuples of comparable values.');
        }
    }


    var Tuple0 = { ctor: "_Tuple0" };
    function Tuple2(x,y) { return { ctor:"_Tuple2", _0:x, _1:y } }

    function chr(c) {
        var x = new String(c);
        x.isChar = true;
        return x;
    }

    function txt(str) {
        var t = new String(str);
        t.text = true;
        return t;
    }

    function makeText(text) {
        var style = '';
        var href = '';
        while (true) {
            if (text.style) {
                style += text.style;
                text = text.text;
                continue;
            }
            if (text.href) {
                href = text.href;
                text = text.text;
                continue;
            }
            if (href) text = '<a href="' + href + '">' + text + '</a>';
            if (style) text = '<span style="' + style + '">' + text + '</span>';
            return text;
        }
    }

    var count = 0;
    function guid(_) { return count++ }

    function copy(oldRecord) {
        var newRecord = {};
        for (var key in oldRecord) {
            var value = key === '_'
                ? copy(oldRecord._)
                : oldRecord[key]
                ;
            newRecord[key] = value;
        }
        return newRecord;
    }

    function remove(key, oldRecord) {
        var record = copy(oldRecord);
        if (key in record._) {
            record[key] = record._[key][0];
            record._[key] = record._[key].slice(1);
            if (record._[key].length === 0) {
                delete record._[key];
            }
        } else {
            delete record[key];
        }
        return record;
    }

    function replace(keyValuePairs, oldRecord) {
        var record = copy(oldRecord);
        for (var i = keyValuePairs.length; i--; ) {
            var pair = keyValuePairs[i];
            record[pair[0]] = pair[1];
        }
        return record;
    }

    function insert(key, value, oldRecord) {
        var newRecord = copy(oldRecord);
        if (key in newRecord) {
            var values = newRecord._[key];
            var copiedValues = values ? values.slice(0) : [];
            newRecord._[key] = [newRecord[key]].concat(copiedValues);
        }
        newRecord[key] = value;
        return newRecord;
    }

    function max(a,b) { return a > b ? a : b }
    function min(a,b) { return a < b ? a : b }

    function mod(a,b) {
        if (b === 0) {
            throw new Error("Cannot perform mod 0. Division by zero error.");
        }
        var r = a % b;
        var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r+b) : -mod(-a,-b));

        return m === b ? 0 : m;
    }

    function htmlHeight(width, rawHtml) {
        // create dummy node
        var html = rawHtml.html;
        var t = document.createElement('div');
        t.innerHTML = html;
        if (width > 0) { t.style.width = width + "px"; }
        t.style.visibility = "hidden";
        t.style.styleFloat = "left";
        t.style.cssFloat   = "left";

        document.body.appendChild(t);

        // insert interpolated values
        var args = rawHtml.args;
        var guid = rawHtml.guid;
        for (var i = args.length; i--; ) {
            var arg = args[i];
            var span = document.getElementById('md-' + guid + '-' + i);
            if (arg.isElement) {
                span.style.width = arg.props.width + 'px';
                span.style.height = arg.props.height + 'px';
            } else {
                span.innerHTML = arg;
            }
        }

        // get dimensions
        var style = window.getComputedStyle(t, null);
        var w = Math.ceil(style.getPropertyValue("width").slice(0,-2) - 0);
        var h = Math.ceil(style.getPropertyValue("height").slice(0,-2) - 0);
        document.body.removeChild(t);
        return Tuple2(w,h);
    }

    function getXY(e) {
        var posx = 0;
        var posy = 0;
        if (e.pageX || e.pageY) {
            posx = e.pageX;
            posy = e.pageY;
        } else if (e.clientX || e.clientY) {
            posx = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
            posy = e.clientY + document.body.scrollTop + document.documentElement.scrollTop;
        }

        if (elm.display === ElmRuntime.Display.COMPONENT) {
            var rect = elm.node.getBoundingClientRect();
            var relx = rect.left + document.body.scrollLeft + document.documentElement.scrollLeft;
            var rely = rect.top + document.body.scrollTop + document.documentElement.scrollTop;
            // TODO: figure out if there is a way to avoid rounding here
            posx = posx - Math.round(relx) - elm.node.clientLeft;
            posy = posy - Math.round(rely) - elm.node.clientTop;
        }
        return Tuple2(posx, posy);
    }

    function isJSArray(a) {
        return a instanceof Array;
    }

    return elm.Native.Utils.values = {
        eq:eq,
        cmp:cmp,
        compare:F2(compare),
        Tuple0:Tuple0,
        Tuple2:Tuple2,
        chr:chr,
        txt:txt,
        makeText:makeText,
        copy: copy,
        remove: remove,
        replace: replace,
        insert: insert,
        guid: guid,
        max : F2(max),
        min : F2(min),
        mod : F2(mod),
        htmlHeight: F2(htmlHeight),
        getXY: getXY,
        isJSArray: isJSArray,
        toFloat: function(x) { return +x; }
    };
};
Elm.Native.Graphics.Collage = {};
Elm.Native.Graphics.Collage.make = function(elm) {

 elm.Native = elm.Native || {};
 elm.Native.Graphics = elm.Native.Graphics || {};
 elm.Native.Graphics.Collage = elm.Native.Graphics.Collage || {};
 if (elm.Native.Graphics.Collage.values) return elm.Native.Graphics.Collage.values;

 var newElement = Elm.Graphics.Element.make(elm).newElement;
 var C = ElmRuntime.use(ElmRuntime.Render.Collage);

 function collage(w,h,forms) {
     return A3(newElement, w, h, {
                 ctor: 'Custom',
		 type: 'Collage',
		 render: C.render,
		 update: C.update,
		 model: {w:w, h:h, forms:forms}
	 });
 }
 return elm.Native.Graphics.Collage.values = { collage:F3(collage) };

};Elm.Native.Graphics.Input = {};
Elm.Native.Graphics.Input.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Graphics = elm.Native.Graphics || {};
    elm.Native.Graphics.Input = elm.Native.Graphics.Input || {};
    if (elm.Native.Graphics.Input.values) return elm.Native.Graphics.Input.values;

    var Render = ElmRuntime.use(ElmRuntime.Render.Element);
    var newNode = ElmRuntime.use(ElmRuntime.Render.Utils).newElement;

    var toCss = Elm.Native.Color.make(elm).toCss;
    var Text = Elm.Native.Text.make(elm);
    var Signal = Elm.Signal.make(elm);
    var newElement = Elm.Graphics.Element.make(elm).newElement;
    var List = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);
    var Tuple2 = Utils.Tuple2;

    function input(initialValue) {
        var signal = Signal.constant(initialValue);
        return { _:{}, signal:signal, handle:signal };
    }

    function renderDropDown(model) {
        var drop = newNode('select');
        drop.style.border = '0 solid';
        drop.style.pointerEvents = 'auto';
        drop.style.display = 'block';

        drop.elm_signal = model.signal;
        drop.elm_values = List.toArray(model.values);
        var values = drop.elm_values;

        for (var i = 0; i < values.length; ++i) {
            var option = newNode('option');
            var name = values[i]._0;
            option.value = name;
            option.innerHTML = name;
            drop.appendChild(option);
        }
        drop.addEventListener('change', function() {
            elm.notify(drop.elm_signal.id, drop.elm_values[drop.selectedIndex]._1);
        });

        return drop;
    }

    function updateDropDown(node, oldModel, newModel) {
        node.elm_signal = newModel.signal;
        node.elm_values = List.toArray(newModel.values);

        var values = node.elm_values;
        var kids = node.childNodes;
        var kidsLength = kids.length;

        var i = 0;
        for (; i < kidsLength && i < values.length; ++i) {
            var option = kids[i];
            var name = values[i]._0;
            option.value = name;
            option.innerHTML = name;
        }
        for (; i < kidsLength; ++i) {
            node.removeChild(node.lastChild);
        }
        for (; i < values.length; ++i) {
            var option = newNode('option');
            var name = values[i]._0;
            option.value = name;
            option.innerHTML = name;
            node.appendChild(option);
        }
    }

    function dropDown(signal, values) {
        return A3(newElement, 100, 24, {
            ctor: 'Custom',
            type: 'DropDown',
            render: renderDropDown,
            update: updateDropDown,
            model: {
                signal: signal,
                values: values
            }
        });
    }

    function renderButton(model) {
        var node = newNode('button');
        node.style.display = 'block';
        node.style.pointerEvents = 'auto';
        node.elm_signal = model.signal;
        node.elm_value = model.value;
        function click() {
            elm.notify(node.elm_signal.id, node.elm_value);
        }
        node.addEventListener('click', click);
        node.innerHTML = model.text;
        return node;
    }

    function updateButton(node, oldModel, newModel) {
        node.elm_signal = newModel.signal;
        node.elm_value = newModel.value;
        var txt = newModel.text;
        if (oldModel.text !== txt) node.innerHTML = txt;
    }

    function button(signal, value, text) {
        return A3(newElement, 100, 40, {
            ctor: 'Custom',
            type: 'Button',
            render: renderButton,
            update: updateButton,
            model: { signal:signal, value:value, text:text }
        });
    }

    function renderCustomButton(model) {
        var btn = newNode('div');
        btn.style.pointerEvents = 'auto';
        btn.elm_signal = model.signal;
        btn.elm_value = model.value;

        btn.elm_up    = Render.render(model.up);
        btn.elm_hover = Render.render(model.hover);
        btn.elm_down  = Render.render(model.down);

        btn.elm_up.style.display = 'block';
        btn.elm_hover.style.display = 'none';
        btn.elm_down.style.display = 'none';
  
        btn.appendChild(btn.elm_up);
        btn.appendChild(btn.elm_hover);
        btn.appendChild(btn.elm_down);

        function swap(visibleNode, hiddenNode1, hiddenNode2) {
            visibleNode.style.display = 'block';
            hiddenNode1.style.display = 'none';
            hiddenNode2.style.display = 'none';
        }

        var overCount = 0;
        function over(e) {
            if (overCount++ > 0) return;
            swap(btn.elm_hover, btn.elm_down, btn.elm_up);
        }
        function out(e) {
            if (btn.contains(e.toElement || e.relatedTarget)) return;
            overCount = 0;
            swap(btn.elm_up, btn.elm_down, btn.elm_hover);
        }
        function up() {
            swap(btn.elm_hover, btn.elm_down, btn.elm_up);
            elm.notify(btn.elm_signal.id, btn.elm_value);
        }
        function down() {
            swap(btn.elm_down, btn.elm_hover, btn.elm_up);
        }

        btn.addEventListener('mouseover', over);
        btn.addEventListener('mouseout' , out);
        btn.addEventListener('mousedown', down);
        btn.addEventListener('mouseup'  , up);

        return btn;
    }

    function updateCustomButton(node, oldModel, newModel) {
        node.elm_signal = newModel.signal;
        node.elm_value = newModel.value;

        var kids = node.childNodes;
        var styleUp    = kids[0].style.display;
        var styleHover = kids[1].style.display;
        var styleDown  = kids[2].style.display;

        Render.update(kids[0], oldModel.up, newModel.up);
        Render.update(kids[1], oldModel.hover, newModel.hover);
        Render.update(kids[2], oldModel.down, newModel.down);

        var kids = node.childNodes;
        kids[0].style.display = styleUp;
        kids[1].style.display = styleHover;
        kids[2].style.display = styleDown;
    }

    function max3(a,b,c) {
        var ab = a > b ? a : b;
        return ab > c ? ab : c;
    }

    function customButton(signal, value, up, hover, down) {
        return A3(newElement,
                  max3(up.props.width, hover.props.width, down.props.width),
                  max3(up.props.height, hover.props.height, down.props.height),
                  { ctor: 'Custom',
                    type: 'CustomButton',
                    render: renderCustomButton,
                    update: updateCustomButton,
                    model: { signal:signal, value:value, up:up, hover:hover, down:down }
                  });
    }

    function renderCheckbox(model) {
        var node = newNode('input');
        node.type = 'checkbox';
        node.checked = model.checked;
        node.style.display = 'block';
        node.style.pointerEvents = 'auto';
        node.elm_signal = model.signal;
        node.elm_handler = model.handler;
        function change() {
            elm.notify(node.elm_signal.id, node.elm_handler(node.checked));
        }
        node.addEventListener('change', change);
        return node;
    }

    function updateCheckbox(node, oldModel, newModel) {
        node.elm_signal = newModel.signal;
        node.elm_handler = newModel.handler;
        node.checked = newModel.checked;
        return true;
    }

    function checkbox(signal, handler, checked) {
        return A3(newElement, 13, 13, {
            ctor: 'Custom',
            type: 'CheckBox',
            render: renderCheckbox,
            update: updateCheckbox,
            model: { signal:signal, handler:handler, checked:checked }
        });
    }

    function setRange(node, start, end, dir) {
        if (node.parentNode) {
            node.setSelectionRange(start, end, dir);
        } else {
            setTimeout(function(){node.setSelectionRange(start, end, dir);}, 0);
        }
    }

    function updateIfNeeded(css, attribute, latestAttribute) {
        if (css[attribute] !== latestAttribute) {
            css[attribute] = latestAttribute;
        }
    }
    function cssDimensions(dimensions) {
        return dimensions.top    + 'px ' +
               dimensions.right  + 'px ' +
               dimensions.bottom + 'px ' +
               dimensions.left   + 'px';
    }
    function updateFieldStyle(css, style) {
        updateIfNeeded(css, 'padding', cssDimensions(style.padding));

        var outline = style.outline;
        updateIfNeeded(css, 'border-width', cssDimensions(outline.width));
        updateIfNeeded(css, 'border-color', toCss(outline.color));
        updateIfNeeded(css, 'border-radius', outline.radius + 'px');

        var highlight = style.highlight;
        if (highlight.width === 0) {
            css.outline = 'none';
        } else {
            updateIfNeeded(css, 'outline-width', highlight.width + 'px');
            updateIfNeeded(css, 'outline-color', toCss(highlight.color));
        }

        var textStyle = style.style;
        updateIfNeeded(css, 'color', toCss(textStyle.color));
        if (textStyle.typeface.ctor !== '[]') {
            updateIfNeeded(css, 'font-family', Text.toTypefaces(textStyle.typeface));
        }
        if (textStyle.height.ctor !== "Nothing") {
            updateIfNeeded(css, 'font-size', textStyle.height._0 + 'px');
        }
        updateIfNeeded(css, 'font-weight', textStyle.bold ? 'bold' : 'normal');
        updateIfNeeded(css, 'font-style', textStyle.italic ? 'italic' : 'normal');
        if (textStyle.line.ctor !== 'Nothing') {
            updateIfNeeded(css, 'text-decoration', Text.toLine(textStyle.line._0));
        }
    }

    function renderField(model) {
        var field = newNode('input');
        updateFieldStyle(field.style, model.style);
        field.style.borderStyle = 'solid';
        field.style.pointerEvents = 'auto';

        field.type = model.type;
        field.placeholder = model.placeHolder;
        field.value = model.content.string;

        field.elm_signal = model.signal;
        field.elm_handler = model.handler;
        field.elm_old_value = field.value;

        function inputUpdate(event) {
            var curr = field.elm_old_value;
            var next = field.value;
            if (curr === next) {
                return;
            }

            var direction = field.selectionDirection === 'forward' ? 'Forward' : 'Backward';
            var start = field.selectionStart;
            var end = field.selectionEnd;
            field.value = field.elm_old_value;

            elm.notify(field.elm_signal.id, field.elm_handler({
                _:{},
                string: next,
                selection: {
                    _:{},
                    start: start,
                    end: end,
                    direction: { ctor: direction }
                }
            }));
        }

        field.addEventListener('input', inputUpdate);
        field.addEventListener('focus', function() {
            field.elm_hasFocus = true;
        });
        field.addEventListener('blur', function() {
            field.elm_hasFocus = false;
        });

        return field;
    }

    function updateField(field, oldModel, newModel) {
        if (oldModel.style !== newModel.style) {
            updateFieldStyle(field.style, newModel.style);
        }
        field.elm_signal = newModel.signal;
        field.elm_handler = newModel.handler;

        field.type = newModel.type;
        field.placeholder = newModel.placeHolder;
        var value = newModel.content.string;
        field.value = value;
        field.elm_old_value = value;
        if (field.elm_hasFocus) {
            var selection = newModel.content.selection;
            var direction = selection.direction.ctor === 'Forward' ? 'forward' : 'backward';
            setRange(field, selection.start, selection.end, direction);
        }
    }

    function mkField(type) {
        function field(style, signal, handler, placeHolder, content) {
            var padding = style.padding;
            var outline = style.outline.width;
            var adjustWidth = padding.left + padding.right + outline.left + outline.right;
            var adjustHeight = padding.top + padding.bottom + outline.top + outline.bottom;
            return A3(newElement, 200, 30, {
                ctor: 'Custom',
                type: type + 'Field',
                adjustWidth: adjustWidth,
                adjustHeight: adjustHeight,
                render: renderField,
                update: updateField,
                model: {
                    signal:signal,
                    handler:handler,
                    placeHolder:placeHolder,
                    content:content,
                    style:style,
                    type:type
                }
            });
        }
        return F5(field);
    }

    function hoverable(signal, handler, elem) {
        function onHover(bool) {
            elm.notify(signal.id, handler(bool));
        }
        var props = Utils.replace([['hover',onHover]], elem.props);
        return { props:props, element:elem.element };
    }

    function clickable(signal, value, elem) {
        function onClick(bool) {
            elm.notify(signal.id, value);
        }
        var props = Utils.replace([['click',onClick]], elem.props);
        return { props:props, element:elem.element };
    }

    return elm.Native.Graphics.Input.values = {
        input:input,
        button:F3(button),
        customButton:F5(customButton),
        checkbox:F3(checkbox),
        dropDown:F2(dropDown),
        field:mkField('text'),
        email:mkField('email'),
        password:mkField('password'),
        hoverable:F3(hoverable),
        clickable:F3(clickable)
    };

};
Elm.Native.Http = {};
Elm.Native.Http.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Http = elm.Native.Http || {};
    if (elm.Native.Http.values) return elm.Native.Http.values;

    var List = Elm.List.make(elm);
    var Signal = Elm.Signal.make(elm);

    function registerReq(queue,responses) {
        return function(req) {
            if (req.url.length > 0) {
                sendReq(queue,responses,req);
            }
        };
    }

    function updateQueue(queue,responses) {
        if (queue.length > 0) {
            elm.notify(responses.id, queue[0].value);
            if (queue[0].value.ctor !== 'Waiting') {
                queue.shift();
                setTimeout(function() { updateQueue(queue,responses); }, 0);
            }
        }
    }

    function sendReq(queue,responses,req) {
        var response = { value: { ctor:'Waiting' } };
        queue.push(response);

        var request = (window.ActiveXObject
                       ? new ActiveXObject("Microsoft.XMLHTTP")
                       : new XMLHttpRequest());

        request.onreadystatechange = function(e) {
            if (request.readyState === 4) {
                response.value = (request.status >= 200 && request.status < 300 ?
                                  { ctor:'Success', _0:request.responseText } :
                                  { ctor:'Failure', _0:request.status, _1:request.statusText });
                setTimeout(function() { updateQueue(queue,responses); }, 0);
            }
        };
        request.open(req.verb, req.url, true);
        function setHeader(pair) {
            request.setRequestHeader( pair._0, pair._1 );
        }
        A2( List.map, setHeader, req.headers );
        request.send(req.body);
    }

    function send(requests) {
        var responses = Signal.constant(elm.Http.values.Waiting);
        var sender = A2( Signal.lift, registerReq([],responses), requests );
        function f(x) { return function(y) { return x; } }
        return A3( Signal.lift2, f, responses, sender );
    }

    return elm.Native.Http.values = {
        send:send
    };
};
Elm.Native.Keyboard = {};
Elm.Native.Keyboard.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Keyboard = elm.Native.Keyboard || {};
    if (elm.Native.Keyboard.values) return elm.Native.Keyboard.values;

    // Duplicated from Native.Signal
    function send(node, timestep, changed) {
        var kids = node.kids;
        for (var i = kids.length; i--; ) {
            kids[i].recv(timestep, changed, node.id);
        }
    }

    var Signal = Elm.Signal.make(elm);
    var NList = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    var downEvents = Signal.constant(null);
    var upEvents = Signal.constant(null);
    var blurEvents = Signal.constant(null);

    elm.addListener([downEvents.id], document, 'keydown', function down(e) {
        elm.notify(downEvents.id, e);
    });

    elm.addListener([upEvents.id], document, 'keyup', function up(e) {
        elm.notify(upEvents.id, e);
    });

    elm.addListener([blurEvents.id], window, 'blur', function blur(e) {
        elm.notify(blurEvents.id, null);
    });

    function state(alt, meta, keyCodes) {
        return {
            alt: alt,
            meta: meta,
            keyCodes: keyCodes
        };
    }
    var emptyState = state(false, false, NList.Nil);

    function KeyMerge(down, up, blur) {
        var args = [down,up,blur];
        this.id = Utils.guid();
        // Ignore starting values here
        this.value = emptyState;
        this.kids = [];
        
        var n = args.length;
        var count = 0;
        var isChanged = false;

        this.recv = function(timestep, changed, parentID) {
            ++count;
            if (changed) { 
                // We know this a change must only be one of the following cases
                if (parentID === down.id && !A2(NList.member, down.value.keyCode, this.value.keyCodes)) {
                    isChanged = true;
                    var v = down.value;
                    var newCodes = NList.Cons(v.keyCode, this.value.keyCodes);
                    this.value = state(v.altKey, v.metaKey, newCodes);
                }
                else if (parentID === up.id) {
                    isChanged = true;
                    var v = up.value;
                    var notEq = function(kc) { return kc !== v.keyCode };
                    var newCodes = A2(NList.filter, notEq, this.value.keyCodes);
                    this.value = state(v.altKey, v.metaKey, newCodes);
                }
                else if (parentID === blur.id) {
                    isChanged = true;
                    this.value = emptyState;
                }
            }
            if (count == n) {
                send(this, timestep, isChanged);
                isChanged = false;
                count = 0;
            }
        };

        for (var i = n; i--; ) {
            args[i].kids.push(this);
            args[i].defaultNumberOfKids += 1;
        }
    }

    var keyMerge = new KeyMerge(downEvents,upEvents,blurEvents);

    // select a part of a keyMerge and dropRepeats the result
    function keySignal(f) {
        var signal = A2(Signal.lift, f, keyMerge);
        // must set the default number of kids to make it possible to filter
        // these signals if they are not actually used.
        keyMerge.defaultNumberOfKids += 1;
        signal.defaultNumberOfKids = 1;
        var filtered = Signal.dropRepeats(signal);
        filtered.defaultNumberOfKids = 0;
        return filtered;
    }

    // break keyMerge into parts
    var keysDown = keySignal(function getKeyCodes(v) {
        return v.keyCodes;
    });
    var alt = keySignal(function getKeyCodes(v) {
        return v.alt;
    });
    var meta = keySignal(function getKeyCodes(v) {
        return v.meta;
    });

    function dir(up, down, left, right) {
        function toDirections(state) {
            var keyCodes = state.keyCodes;
            var x = 0, y = 0;
            while (keyCodes.ctor === "::") {
                switch (keyCodes._0) {
                case left : --x; break;
                case right: ++x; break;
                case up   : ++y; break;
                case down : --y; break;
                }
                keyCodes = keyCodes._1;
            }
            return { _:{}, x:x, y:y };
        }
        return keySignal(toDirections);
    }

    function is(key) {
        return keySignal(function(v) {
            return A2( NList.member, key, v.keyCodes );
        });
    }

    var lastPressed = A2(Signal.lift, function(e) {
        return e ? e.keyCode : 0;
    }, downEvents);
    downEvents.defaultNumberOfKids += 1;

    return elm.Native.Keyboard.values = {
        isDown:is,
        alt: alt,
        meta: meta,
        directions:F4(dir),
        keysDown:keysDown,
        lastPressed:lastPressed
    };

};
Elm.Native.Mouse = {};
Elm.Native.Mouse.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Mouse = elm.Native.Mouse || {};
    if (elm.Native.Mouse.values) return elm.Native.Mouse.values;

    var Signal = Elm.Signal.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    var position  = Signal.constant(Utils.Tuple2(0,0));
    position.defaultNumberOfKids = 2;

    // do not move x and y into Elm. By setting their default number
    // of kids, it is possible to detatch the mouse listeners if
    // they are not needed.
    var x = A2( Signal.lift, function(p){return p._0}, position);
    x.defaultNumberOfKids = 0;
    var y = A2( Signal.lift, function(p){return p._1}, position);
    y.defaultNumberOfKids = 0;

    var isDown    = Signal.constant(false);
    var clicks = Signal.constant(Utils.Tuple0);

    var node = elm.display === ElmRuntime.Display.FULLSCREEN ? document : elm.node;

    elm.addListener([clicks.id], node, 'click', function click() {
        elm.notify(clicks.id, Utils.Tuple0);
    });
    elm.addListener([isDown.id], node, 'mousedown', function down() {
        elm.notify(isDown.id, true);
    });
    elm.addListener([isDown.id], node, 'mouseup', function up() {
        elm.notify(isDown.id, false);
    });
    elm.addListener([position.id], node, 'mousemove', function move(e) {
        elm.notify(position.id, Utils.getXY(e));
    });

    return elm.Native.Mouse.values = {
        position: position,
        x:x,
        y:y,
        isDown: isDown,
        clicks: clicks
    };
};
Elm.Native.Random = {};
Elm.Native.Random.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Random = elm.Native.Random || {};
    if (elm.Native.Random.values) return elm.Native.Random.values;

    var Signal = Elm.Signal.make(elm);
    var List = Elm.Native.List.make(elm);

    function range(min, max, signal) {
        function f(x) { return Math.floor(Math.random() * (max-min+1)) + min; }
        return A2( Signal.lift, f, signal );
    }

    function float_(signal) {
        function f(x) { return Math.random(); }
        return A2( Signal.lift, f, signal );
    }

    function floatList(signal) {
        function f(n) {
            if (n < 0) return List.Nil;
            var arr = new Array(n);
            for (var i = n; i--; ) {
                arr[i] = Math.random();
            }
            return List.fromArray(arr);
        }
        return A2( Signal.lift, f, signal );
    }

    return elm.Native.Random.values = {
        range: F3(range),
        float_: float_,
        floatList: floatList
    };

};

Elm.Native.Signal = {};
Elm.Native.Signal.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.Signal = elm.Native.Signal || {};
  if (elm.Native.Signal.values) return elm.Native.Signal.values;

  var Utils = Elm.Native.Utils.make(elm);
  var foldr1 = Elm.List.make(elm).foldr1;

  function send(node, timestep, changed) {
    var kids = node.kids;
    for (var i = kids.length; i--; ) {
      kids[i].recv(timestep, changed, node.id);
    }
  }

  function Input(base) {
    this.id = Utils.guid();
    this.value = base;
    this.kids = [];
    this.defaultNumberOfKids = 0;
    this.recv = function(timestep, eid, v) {
      var changed = eid === this.id;
      if (changed) { this.value = v; }
      send(this, timestep, changed);
      return changed;
    };
    elm.inputs.push(this);
  }

  function LiftN(update, args) {
    this.id = Utils.guid();
    this.value = update();
    this.kids = [];

    var n = args.length;
    var count = 0;
    var isChanged = false;

    this.recv = function(timestep, changed, parentID) {
      ++count;
      if (changed) { isChanged = true; }
      if (count == n) {
        if (isChanged) { this.value = update(); }
        send(this, timestep, isChanged);
        isChanged = false;
        count = 0;
      }
    };
    for (var i = n; i--; ) { args[i].kids.push(this); }
  }

  function lift(func, a) {
    function update() { return func(a.value); }
    return new LiftN(update, [a]);
  }
  function lift2(func, a, b) {
    function update() { return A2( func, a.value, b.value ); }
    return new LiftN(update, [a,b]);
  }
  function lift3(func, a, b, c) {
    function update() { return A3( func, a.value, b.value, c.value ); }
    return new LiftN(update, [a,b,c]);
  }
  function lift4(func, a, b, c, d) {
    function update() { return A4( func, a.value, b.value, c.value, d.value ); }
    return new LiftN(update, [a,b,c,d]);
  }
  function lift5(func, a, b, c, d, e) {
    function update() { return A5( func, a.value, b.value, c.value, d.value, e.value ); }
    return new LiftN(update, [a,b,c,d,e]);
  }
  function lift6(func, a, b, c, d, e, f) {
    function update() { return A6( func, a.value, b.value, c.value, d.value, e.value, f.value ); }
    return new LiftN(update, [a,b,c,d,e,f]);
  }
  function lift7(func, a, b, c, d, e, f, g) {
    function update() { return A7( func, a.value, b.value, c.value, d.value, e.value, f.value, g.value ); }
    return new LiftN(update, [a,b,c,d,e,f,g]);
  }
  function lift8(func, a, b, c, d, e, f, g, h) {
    function update() { return A8( func, a.value, b.value, c.value, d.value, e.value, f.value, g.value, h.value ); }
    return new LiftN(update, [a,b,c,d,e,f,g,h]);
  }

  function Foldp(step, state, input) {
    this.id = Utils.guid();
    this.value = state;
    this.kids = [];

    this.recv = function(timestep, changed, parentID) {
      if (changed) {
          this.value = A2( step, input.value, this.value );
      }
      send(this, timestep, changed);
    };
    input.kids.push(this);
  }

  function foldp(step, state, input) {
      return new Foldp(step, state, input);
  }

  function DropIf(pred,base,input) {
    this.id = Utils.guid();
    this.value = pred(input.value) ? base : input.value;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
      var chng = changed && !pred(input.value);
      if (chng) { this.value = input.value; }
      send(this, timestep, chng);
    };
    input.kids.push(this);
  }

  function DropRepeats(input) {
    this.id = Utils.guid();
    this.value = input.value;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
      var chng = changed && !Utils.eq(this.value,input.value);
      if (chng) { this.value = input.value; }
      send(this, timestep, chng);
    };
    input.kids.push(this);
  }

  function timestamp(a) {
    function update() { return Utils.Tuple2(elm.timer.now(), a.value); }
    return new LiftN(update, [a]);
  }

  function SampleOn(s1,s2) {
    this.id = Utils.guid();
    this.value = s2.value;
    this.kids = [];

    var count = 0;
    var isChanged = false;

    this.recv = function(timestep, changed, parentID) {
      if (parentID === s1.id) isChanged = changed;
      ++count;
      if (count == 2) {
        if (isChanged) { this.value = s2.value; }
        send(this, timestep, isChanged);
        count = 0;
        isChanged = false;
      }
    };
    s1.kids.push(this);
    s2.kids.push(this);
  }

  function sampleOn(s1,s2) { return new SampleOn(s1,s2); }

  function delay(t,s) {
      var delayed = new Input(s.value);
      var firstEvent = true;
      function update(v) {
        if (firstEvent) { firstEvent = false; return; }
        setTimeout(function() { elm.notify(delayed.id, v); }, t);
      }
      function first(a,b) { return a; }
      return new SampleOn(delayed, lift2(F2(first), delayed, lift(update,s)));
  }

  function Merge(s1,s2) {
      this.id = Utils.guid();
      this.value = s1.value;
      this.kids = [];

      var next = null;
      var count = 0;
      var isChanged = false;

      this.recv = function(timestep, changed, parentID) {
        ++count;
        if (changed) {
            isChanged = true;
            if (parentID == s2.id && next === null) { next = s2.value; }
            if (parentID == s1.id) { next = s1.value; }
        }

        if (count == 2) {
            if (isChanged) { this.value = next; next = null; }
            send(this, timestep, isChanged);
            isChanged = false;
            count = 0;
        }
      };
      s1.kids.push(this);
      s2.kids.push(this);
  }

  function merge(s1,s2) { return new Merge(s1,s2); }
  function merges(ss) { return A2(foldr1, F2(merge), ss); }

  return elm.Native.Signal.values = {
    input: function(v) { return new Input(v); },
    constant : function(v) { return new Input(v); },
    lift  : F2(lift ),
    lift2 : F3(lift2),
    lift3 : F4(lift3),
    lift4 : F5(lift4),
    lift5 : F6(lift5),
    lift6 : F7(lift6),
    lift7 : F8(lift7),
    lift8 : F9(lift8),
    foldp : F3(foldp),
    delay : F2(delay),
    merge : F2(merge),
    merges : merges,
    count : function(s) { return foldp(F2(function(_,c) { return c+1; }), 0, s); },
    countIf : F2(function(pred,s) {
      return foldp(F2(function(x,c){
        return pred(x) ? c+1 : c; }), 0, s)}),
    keepIf : F3(function(pred,base,sig) {
      return new DropIf(function(x) {return !pred(x);},base,sig); }),
    dropIf : F3(function(pred,base,sig) { return new DropIf(pred,base,sig); }),
    dropRepeats : function(s) { return new DropRepeats(s);},
    sampleOn : F2(sampleOn),
    timestamp : timestamp
  };
};
Elm.Native.Time = {};
Elm.Native.Time.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.Time = elm.Native.Time || {};
  if (elm.Native.Time.values) return elm.Native.Time.values;

  var Signal = Elm.Signal.make(elm);
  var NS = Elm.Native.Signal.make(elm);
  var Maybe = Elm.Maybe.make(elm);
  var Utils = Elm.Native.Utils.make(elm);

  function fpsWhen(desiredFPS, isOn) {
    var msPerFrame = 1000 / desiredFPS;
    var prev = elm.timer.now(), curr = prev, diff = 0, wasOn = true;
    var ticker = NS.input(diff);
    function tick(zero) {
      return function() {
        curr = elm.timer.now();
        diff = zero ? 0 : curr - prev;
        if (prev > curr) {
          diff = 0;
        }
        prev = curr;
        elm.notify(ticker.id, diff);
      };
    }
    var timeoutID = 0;
    function f(isOn, t) {
      if (isOn) {
        timeoutID = elm.setTimeout(tick(!wasOn && isOn), msPerFrame);
      } else if (wasOn) {
        clearTimeout(timeoutID);
      }
      wasOn = isOn;
      return t;
    }
    return A3( Signal.lift2, F2(f), isOn, ticker );
  }

  function every(t) {
    var clock = NS.input(elm.timer.now());
    function tellTime() {
        elm.notify(clock.id, elm.timer.now());
    }
    setInterval(tellTime, t);
    return clock;
  }

  function since(t, s) {
    function cmp(a,b) { return !Utils.eq(a,b); }
    var dcount = Signal.count(A2(NS.delay, t, s));
    return A3( Signal.lift2, F2(cmp), Signal.count(s), dcount );
  }
  function read(s) {
      var t = Date.parse(s);
      return isNaN(t) ? Maybe.Nothing : Maybe.Just(t);
  }
  return elm.Native.Time.values = {
      fpsWhen : F2(fpsWhen),
      fps : function(t) { return fpsWhen(t, Signal.constant(true)); },
      every : every,
      delay : NS.delay,
      timestamp : NS.timestamp,
      since : F2(since),
      toDate : function(t) { return new window.Date(t); },
      read   : read
  };

};
Elm.Native.Touch = {};
Elm.Native.Touch.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Touch = elm.Native.Touch || {};
    if (elm.Native.Touch.values) return elm.Native.Touch.values;

    var Signal = Elm.Signal.make(elm);
    var List = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    function Dict() {
        this.keys = [];
        this.values = [];

        this.insert = function(key,value) {
            this.keys.push(key);
            this.values.push(value);
        };
        this.lookup = function(key) {
            var i = this.keys.indexOf(key)
            return i >= 0 ? this.values[i] : {x:0,y:0,t:0};
        };
        this.remove = function(key) {
            var i = this.keys.indexOf(key);
            if (i < 0) return;
            var t = this.values[i];
            this.keys.splice(i,1);
            this.values.splice(i,1);
            return t;
        };
        this.clear = function() {
            this.keys = [];
            this.values = [];
        };
    }
    
    var root = Signal.constant([]),
    tapTime = 500,
    hasTap = false,
    tap = {_:{},x:0,y:0},
    dict = new Dict();

    function touch(t) {
        var r = dict.lookup(t.identifier);
        var point = Utils.getXY(t);
        return {_ : {},
	        id: t.identifier,
	        x : point._0,
	        y : point._1,
	        x0: r.x,
	        y0: r.y,
	        t0: r.t
	       };
    }

    var node = elm.display === ElmRuntime.Display.FULLSCREEN ? document : elm.node;

    function start(e) {
        var point = Utils.getXY(e);
        dict.insert(e.identifier,
                    {x: point._0,
                     y: point._1,
                     t: elm.timer.now()});
    }
    function end(e) {
        var t = dict.remove(e.identifier);
        if (elm.timer.now() - t.t < tapTime) {
            hasTap = true;
            tap = {_:{}, x:t.x, y:t.y};
        }
    }

    function listen(name, f) {
        function update(e) {
            for (var i = e.changedTouches.length; i--; ) { f(e.changedTouches[i]); }
            var ts = new Array(e.touches.length);
            for (var i = e.touches.length; i--; ) { ts[i] = touch(e.touches[i]); }
            elm.notify(root.id, ts);
            e.preventDefault();
        }
        elm.addListener([root.id], node, name, update);
    }

    listen("touchstart", start);
    listen("touchmove", function(_){});
    listen("touchend", end);
    listen("touchcancel", end);
    listen("touchleave", end);

    var mouseID = -1;
    function move(e) {
        var point = Utils.getXY(e);
        for (var i = root.value.length; i--; ) {
            if (root.value[i].id === mouseID) {
                root.value[i].x = point._0;
                root.value[i].y = point._1;
                elm.notify(root.id, root.value);
                break;
            }
        }
    }
    elm.addListener([root.id], node, "mousedown", function down(e) {
        node.addEventListener("mousemove", move);
        e.identifier = mouseID;
        start(e);
        root.value.push(touch(e));
        elm.notify(root.id, root.value);
    });
    elm.addListener([root.id], document, "mouseup", function up(e) {
        node.removeEventListener("mousemove", move);
        e.identifier = mouseID;
        end(e);
        for (var i = root.value.length; i--; ) {
            if (root.value[i].id === mouseID) {
                root.value.splice(i, 1);
                --mouseID;
                break;
            }
        }
        elm.notify(root.id, root.value);
    });
    elm.addListener([root.id], node, "blur", function blur(e) {
        node.removeEventListener("mousemove", move);
        if (root.value.length > 0) {
            elm.notify(root.id, []);
            --mouseID;
        }
        dict.clear();
    });

    function dependency(f) {
        var sig = A2( Signal.lift, f, root );
        root.defaultNumberOfKids += 1;
        sig.defaultNumberOfKids = 0;
        return sig;
    }

    var touches = dependency(List.fromArray);

    var taps = function() {
        var sig = dependency(function(_) { return tap; });
        sig.defaultNumberOfKids = 1;
        function pred(_) { var b = hasTap; hasTap = false; return b; }
        var sig2 = A3( Signal.keepIf, pred, {_:{},x:0,y:0}, sig);
        sig2.defaultNumberOfKids = 0;
        return sig2;
    }();

    return elm.Native.Touch.values = { touches: touches, taps: taps };

};
Elm.Native.WebSocket = {};
Elm.Native.WebSocket.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.WebSocket = elm.Native.WebSocket || {};
  if (elm.Native.WebSocket.values) return elm.Native.WebSocket.values;

  var Signal = Elm.Signal.make(elm);
  var List = Elm.Native.List.make(elm);

  function open(url, outgoing) {
    var incoming = Signal.constant("");
    var ws = new WebSocket(url);

    var pending = [];
    var ready = false;
    
    ws.onopen = function(e) {
      var len = pending.length;
      for (var i = 0; i < len; ++i) { ws.send(pending[i]); }
      ready = true;
    };
    ws.onmessage = function(event) {
      elm.notify(incoming.id, event.data);
    };
    
    function send(msg) {
      ready ? ws.send(msg) : pending.push(msg);
    }
    
    function take1(x,y) { return x }
    return A3(Signal.lift2, F2(take1), incoming, A2(Signal.lift, send, outgoing));
  }

  return elm.Native.WebSocket.values = { connect: F2(open) };
};
Elm.Native.Window = {};
Elm.Native.Window.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.Window = elm.Native.Window || {};
  if (elm.Native.Window.values) return elm.Native.Window.values;

  var Signal = Elm.Signal.make(elm);
  var NS = Elm.Native.Signal.make(elm);
  var Tuple2 = Elm.Native.Utils.make(elm).Tuple2;

  function getWidth() { return elm.node.clientWidth; }
  function getHeight() {
      if (elm.display === ElmRuntime.Display.FULLSCREEN) {
          return window.innerHeight;
      }
      return elm.node.clientHeight;
  }

  var dimensions = NS.input(Tuple2(getWidth(), getHeight()));
  dimensions.defaultNumberOfKids = 2;

  // Do not move width and height into Elm. By setting the default number of kids,
  // the resize listener can be detached.
  var width  = A2(Signal.lift, function(p){return p._0;}, dimensions);
  width.defaultNumberOfKids = 0;

  var height = A2(Signal.lift, function(p){return p._1;}, dimensions);
  height.defaultNumberOfKids = 0;

  function resizeIfNeeded() {
      // Do not trigger event if the dimensions have not changed.
      // This should be most of the time.
      var w = getWidth();
      var h = getHeight();
      if (dimensions.value._0 === w && dimensions.value._1 === h) return;

      setTimeout(function () {
          // Check again to see if the dimensions have changed.
          // It is conceivable that the dimensions have changed
          // again while some other event was being processed.
          var w = getWidth();
          var h = getHeight();
          if (dimensions.value._0 === w && dimensions.value._1 === h) return;
          elm.notify(dimensions.id, Tuple2(w,h));
      }, 0);
  }
  elm.addListener([dimensions.id], window, 'resize', resizeIfNeeded);

  return elm.Native.Window.values = {
      dimensions:dimensions,
      width:width,
      height:height,
      resizeIfNeeded:resizeIfNeeded
  };

};
Elm.Array = Elm.Array || {};
Elm.Array.make = function (_elm) {
   "use strict";
   _elm.Array = _elm.Array || {};
   if (_elm.Array.values)
   return _elm.Array.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Array",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Array = Elm.Native.Array.make(_elm);
   var append = $Native$Array.append;
   var length = $Native$Array.length;
   var slice = $Native$Array.slice;
   var set = $Native$Array.set;
   var getOrElse = F3(function ($default,
   i,
   array) {
      return _U.cmp(0,
      i) < 1 && _U.cmp(i,
      $Native$Array.length(array)) < 0 ? A2($Native$Array.get,
      i,
      array) : $default;
   });
   var get = F2(function (i,
   array) {
      return _U.cmp(0,
      i) < 1 && _U.cmp(i,
      $Native$Array.length(array)) < 0 ? $Maybe.Just(A2($Native$Array.get,
      i,
      array)) : $Maybe.Nothing;
   });
   var getOrFail = $Native$Array.get;
   var push = $Native$Array.push;
   var empty = $Native$Array.empty;
   var filter = F2(function (isOkay,
   arr) {
      return function () {
         var update = F2(function (x,
         xs) {
            return isOkay(x) ? A2($Native$Array.push,
            x,
            xs) : xs;
         });
         return A3($Native$Array.foldl,
         update,
         $Native$Array.empty,
         arr);
      }();
   });
   var foldr = $Native$Array.foldr;
   var foldl = $Native$Array.foldl;
   var indexedMap = $Native$Array.indexedMap;
   var map = $Native$Array.map;
   var toIndexedList = function (array) {
      return A2($List.zip,
      _L.range(0,
      $Native$Array.length(array) - 1),
      $Native$Array.toList(array));
   };
   var toList = $Native$Array.toList;
   var fromList = $Native$Array.fromList;
   var initialize = $Native$Array.initialize;
   var repeat = F2(function (n,e) {
      return A2(initialize,
      n,
      $Basics.always(e));
   });
   var Array = {ctor: "Array"};
   _elm.Array.values = {_op: _op
                       ,Array: Array
                       ,initialize: initialize
                       ,repeat: repeat
                       ,fromList: fromList
                       ,toList: toList
                       ,toIndexedList: toIndexedList
                       ,map: map
                       ,indexedMap: indexedMap
                       ,foldl: foldl
                       ,foldr: foldr
                       ,filter: filter
                       ,empty: empty
                       ,push: push
                       ,getOrFail: getOrFail
                       ,get: get
                       ,getOrElse: getOrElse
                       ,set: set
                       ,slice: slice
                       ,length: length
                       ,append: append};
   return _elm.Array.values;
};Elm.Basics = Elm.Basics || {};
Elm.Basics.make = function (_elm) {
   "use strict";
   _elm.Basics = _elm.Basics || {};
   if (_elm.Basics.values)
   return _elm.Basics.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Basics",
   $Native$Basics = Elm.Native.Basics.make(_elm);
   var uncurry = F2(function (f,
   _v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2": return A2(f,
              _v0._0,
              _v0._1);}
         _E.Case($moduleName,
         "on line 419, column 19 to 24");
      }();
   });
   var curry = F3(function (f,
   a,
   b) {
      return f({ctor: "_Tuple2"
               ,_0: a
               ,_1: b});
   });
   var flip = F3(function (f,b,a) {
      return A2(f,a,b);
   });
   var snd = function (_v4) {
      return function () {
         switch (_v4.ctor)
         {case "_Tuple2": return _v4._1;}
         _E.Case($moduleName,
         "on line 403, column 13 to 14");
      }();
   };
   var fst = function (_v8) {
      return function () {
         switch (_v8.ctor)
         {case "_Tuple2": return _v8._0;}
         _E.Case($moduleName,
         "on line 399, column 13 to 14");
      }();
   };
   var always = F2(function (a,
   _v12) {
      return function () {
         return a;
      }();
   });
   var identity = function (x) {
      return x;
   };
   _op["<|"] = F2(function (f,x) {
      return f(x);
   });
   _op["|>"] = F2(function (x,f) {
      return f(x);
   });
   _op[">>"] = F3(function (f,
   g,
   x) {
      return g(f(x));
   });
   _op["<<"] = F3(function (g,
   f,
   x) {
      return g(f(x));
   });
   var isInfinite = $Native$Basics.isInfinite;
   var isNaN = $Native$Basics.isNaN;
   var toFloat = $Native$Basics.toFloat;
   var ceiling = $Native$Basics.ceiling;
   var floor = $Native$Basics.floor;
   var truncate = $Native$Basics.truncate;
   var round = $Native$Basics.round;
   var otherwise = true;
   var not = $Native$Basics.not;
   var xor = $Native$Basics.xor;
   _op["||"] = $Native$Basics.or;
   _op["&&"] = $Native$Basics.and;
   var max = $Native$Basics.max;
   var min = $Native$Basics.min;
   var GT = {ctor: "GT"};
   var EQ = {ctor: "EQ"};
   var LT = {ctor: "LT"};
   var compare = $Native$Basics.compare;
   _op[">="] = $Native$Basics.ge;
   _op["<="] = $Native$Basics.le;
   _op[">"] = $Native$Basics.gt;
   _op["<"] = $Native$Basics.lt;
   _op["/="] = $Native$Basics.neq;
   _op["=="] = $Native$Basics.eq;
   var e = $Native$Basics.e;
   var pi = $Native$Basics.pi;
   var clamp = $Native$Basics.clamp;
   var logBase = $Native$Basics.logBase;
   var abs = $Native$Basics.abs;
   var negate = $Native$Basics.negate;
   var sqrt = $Native$Basics.sqrt;
   var atan2 = $Native$Basics.atan2;
   var atan = $Native$Basics.atan;
   var asin = $Native$Basics.asin;
   var acos = $Native$Basics.acos;
   var tan = $Native$Basics.tan;
   var sin = $Native$Basics.sin;
   var cos = $Native$Basics.cos;
   _op["^"] = $Native$Basics.exp;
   _op["%"] = $Native$Basics.mod;
   var rem = $Native$Basics.rem;
   _op["//"] = $Native$Basics.div;
   _op["/"] = $Native$Basics.floatDiv;
   _op["*"] = $Native$Basics.mul;
   _op["-"] = $Native$Basics.sub;
   _op["+"] = $Native$Basics.add;
   var toPolar = $Native$Basics.toPolar;
   var fromPolar = $Native$Basics.fromPolar;
   var turns = $Native$Basics.turns;
   var degrees = $Native$Basics.degrees;
   var radians = function (t) {
      return t;
   };
   _elm.Basics.values = {_op: _op
                        ,radians: radians
                        ,degrees: degrees
                        ,turns: turns
                        ,fromPolar: fromPolar
                        ,toPolar: toPolar
                        ,rem: rem
                        ,cos: cos
                        ,sin: sin
                        ,tan: tan
                        ,acos: acos
                        ,asin: asin
                        ,atan: atan
                        ,atan2: atan2
                        ,sqrt: sqrt
                        ,negate: negate
                        ,abs: abs
                        ,logBase: logBase
                        ,clamp: clamp
                        ,pi: pi
                        ,e: e
                        ,compare: compare
                        ,LT: LT
                        ,EQ: EQ
                        ,GT: GT
                        ,min: min
                        ,max: max
                        ,xor: xor
                        ,not: not
                        ,otherwise: otherwise
                        ,round: round
                        ,truncate: truncate
                        ,floor: floor
                        ,ceiling: ceiling
                        ,toFloat: toFloat
                        ,isNaN: isNaN
                        ,isInfinite: isInfinite
                        ,identity: identity
                        ,always: always
                        ,fst: fst
                        ,snd: snd
                        ,flip: flip
                        ,curry: curry
                        ,uncurry: uncurry};
   return _elm.Basics.values;
};Elm.Bitwise = Elm.Bitwise || {};
Elm.Bitwise.make = function (_elm) {
   "use strict";
   _elm.Bitwise = _elm.Bitwise || {};
   if (_elm.Bitwise.values)
   return _elm.Bitwise.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Bitwise",
   $Native$Bitwise = Elm.Native.Bitwise.make(_elm);
   var shiftRightLogical = $Native$Bitwise.shiftRightLogical;
   var shiftRight = $Native$Bitwise.shiftRightArithmatic;
   var shiftLeft = $Native$Bitwise.shiftLeft;
   var complement = $Native$Bitwise.complement;
   var xor = $Native$Bitwise.xor;
   var or = $Native$Bitwise.or;
   var and = $Native$Bitwise.and;
   _elm.Bitwise.values = {_op: _op
                         ,and: and
                         ,or: or
                         ,xor: xor
                         ,complement: complement
                         ,shiftLeft: shiftLeft
                         ,shiftRight: shiftRight
                         ,shiftRightLogical: shiftRightLogical};
   return _elm.Bitwise.values;
};Elm.Char = Elm.Char || {};
Elm.Char.make = function (_elm) {
   "use strict";
   _elm.Char = _elm.Char || {};
   if (_elm.Char.values)
   return _elm.Char.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Char",
   $Native$Char = Elm.Native.Char.make(_elm);
   var fromCode = $Native$Char.fromCode;
   var toCode = $Native$Char.toCode;
   var toLocaleLower = $Native$Char.toLocaleLower;
   var toLocaleUpper = $Native$Char.toLocaleUpper;
   var toLower = $Native$Char.toLower;
   var toUpper = $Native$Char.toUpper;
   var isHexDigit = $Native$Char.isHexDigit;
   var isOctDigit = $Native$Char.isOctDigit;
   var isDigit = $Native$Char.isDigit;
   var isLower = $Native$Char.isLower;
   var isUpper = $Native$Char.isUpper;
   _elm.Char.values = {_op: _op
                      ,isUpper: isUpper
                      ,isLower: isLower
                      ,isDigit: isDigit
                      ,isOctDigit: isOctDigit
                      ,isHexDigit: isHexDigit
                      ,toUpper: toUpper
                      ,toLower: toLower
                      ,toLocaleUpper: toLocaleUpper
                      ,toLocaleLower: toLocaleLower
                      ,toCode: toCode
                      ,fromCode: fromCode};
   return _elm.Char.values;
};Elm.Color = Elm.Color || {};
Elm.Color.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   if (_elm.Color.values)
   return _elm.Color.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Color",
   $Basics = Elm.Basics.make(_elm);
   var Radial = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "Radial"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var radial = Radial;
   var Linear = F3(function (a,
   b,
   c) {
      return {ctor: "Linear"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var linear = Linear;
   var fmod = F2(function (f,n) {
      return function () {
         var integer = $Basics.floor(f);
         return $Basics.toFloat(A2($Basics._op["%"],
         integer,
         n)) + f - $Basics.toFloat(integer);
      }();
   });
   var rgbToHsl = F3(function (red,
   green,
   blue) {
      return function () {
         var b = $Basics.toFloat(blue) / 255;
         var g = $Basics.toFloat(green) / 255;
         var r = $Basics.toFloat(red) / 255;
         var cMax = A2($Basics.max,
         A2($Basics.max,r,g),
         b);
         var cMin = A2($Basics.min,
         A2($Basics.min,r,g),
         b);
         var c = cMax - cMin;
         var lightness = (cMax + cMin) / 2;
         var saturation = _U.eq(lightness,
         0) ? 0 : c / (1 - $Basics.abs(2 * lightness - 1));
         var hue = $Basics.degrees(60) * (_U.eq(cMax,
         r) ? A2(fmod,
         (g - b) / c,
         6) : _U.eq(cMax,
         g) ? (b - r) / c + 2 : _U.eq(cMax,
         b) ? (r - g) / c + 4 : _E.If($moduleName,
         "between lines 140 and 142"));
         return {ctor: "_Tuple3"
                ,_0: hue
                ,_1: saturation
                ,_2: lightness};
      }();
   });
   var hslToRgb = F3(function (hue,
   saturation,
   lightness) {
      return function () {
         var hue$ = hue / $Basics.degrees(60);
         var chroma = (1 - $Basics.abs(2 * lightness - 1)) * saturation;
         var x = chroma * (1 - $Basics.abs(A2(fmod,
         hue$,
         2) - 1));
         var $ = _U.cmp(hue$,
         0) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: 0
                  ,_2: 0} : _U.cmp(hue$,
         1) < 0 ? {ctor: "_Tuple3"
                  ,_0: chroma
                  ,_1: x
                  ,_2: 0} : _U.cmp(hue$,
         2) < 0 ? {ctor: "_Tuple3"
                  ,_0: x
                  ,_1: chroma
                  ,_2: 0} : _U.cmp(hue$,
         3) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: chroma
                  ,_2: x} : _U.cmp(hue$,
         4) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: x
                  ,_2: chroma} : _U.cmp(hue$,
         5) < 0 ? {ctor: "_Tuple3"
                  ,_0: x
                  ,_1: 0
                  ,_2: chroma} : _U.cmp(hue$,
         6) < 0 ? {ctor: "_Tuple3"
                  ,_0: chroma
                  ,_1: 0
                  ,_2: x} : {ctor: "_Tuple3"
                            ,_0: 0
                            ,_1: 0
                            ,_2: 0},
         r = $._0,
         g = $._1,
         b = $._2;
         var m = lightness - chroma / 2;
         return {ctor: "_Tuple3"
                ,_0: r + m
                ,_1: g + m
                ,_2: b + m};
      }();
   });
   var toRgb = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA":
            return function () {
                 var $ = A3(hslToRgb,
                 color._0,
                 color._1,
                 color._2),
                 r = $._0,
                 g = $._1,
                 b = $._2;
                 return {_: {}
                        ,alpha: color._3
                        ,blue: $Basics.round(255 * b)
                        ,green: $Basics.round(255 * g)
                        ,red: $Basics.round(255 * r)};
              }();
            case "RGBA": return {_: {}
                                ,alpha: color._3
                                ,blue: color._2
                                ,green: color._1
                                ,red: color._0};}
         _E.Case($moduleName,
         "between lines 114 and 122");
      }();
   };
   var toHsl = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA": return {_: {}
                              ,alpha: color._3
                              ,hue: color._0
                              ,lightness: color._2
                              ,saturation: color._1};
            case "RGBA":
            return function () {
                 var $ = A3(rgbToHsl,
                 color._0,
                 color._1,
                 color._2),
                 h = $._0,
                 s = $._1,
                 l = $._2;
                 return {_: {}
                        ,alpha: color._3
                        ,hue: h
                        ,lightness: l
                        ,saturation: s};
              }();}
         _E.Case($moduleName,
         "between lines 104 and 111");
      }();
   };
   var HSLA = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "HSLA"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var hsla = F4(function (hue,
   saturation,
   lightness,
   alpha) {
      return A4(HSLA,
      hue - $Basics.turns($Basics.toFloat($Basics.floor(hue / (2 * $Basics.pi)))),
      saturation,
      lightness,
      alpha);
   });
   var hsl = F3(function (hue,
   saturation,
   lightness) {
      return A4(hsla,
      hue,
      saturation,
      lightness,
      1);
   });
   var complement = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA": return A4(hsla,
              color._0 + $Basics.degrees(180),
              color._1,
              color._2,
              color._3);
            case "RGBA":
            return function () {
                 var $ = A3(rgbToHsl,
                 color._0,
                 color._1,
                 color._2),
                 h = $._0,
                 s = $._1,
                 l = $._2;
                 return A4(hsla,
                 h + $Basics.degrees(180),
                 s,
                 l,
                 color._3);
              }();}
         _E.Case($moduleName,
         "between lines 95 and 101");
      }();
   };
   var grayscale = function (p) {
      return A4(HSLA,0,0,1 - p,1);
   };
   var greyscale = function (p) {
      return A4(HSLA,0,0,1 - p,1);
   };
   var RGBA = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "RGBA"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var rgba = RGBA;
   var rgb = F3(function (r,g,b) {
      return A4(RGBA,r,g,b,1);
   });
   var lightRed = A4(RGBA,
   239,
   41,
   41,
   1);
   var red = A4(RGBA,204,0,0,1);
   var darkRed = A4(RGBA,
   164,
   0,
   0,
   1);
   var lightOrange = A4(RGBA,
   252,
   175,
   62,
   1);
   var orange = A4(RGBA,
   245,
   121,
   0,
   1);
   var darkOrange = A4(RGBA,
   206,
   92,
   0,
   1);
   var lightYellow = A4(RGBA,
   255,
   233,
   79,
   1);
   var yellow = A4(RGBA,
   237,
   212,
   0,
   1);
   var darkYellow = A4(RGBA,
   196,
   160,
   0,
   1);
   var lightGreen = A4(RGBA,
   138,
   226,
   52,
   1);
   var green = A4(RGBA,
   115,
   210,
   22,
   1);
   var darkGreen = A4(RGBA,
   78,
   154,
   6,
   1);
   var lightBlue = A4(RGBA,
   114,
   159,
   207,
   1);
   var blue = A4(RGBA,
   52,
   101,
   164,
   1);
   var darkBlue = A4(RGBA,
   32,
   74,
   135,
   1);
   var lightPurple = A4(RGBA,
   173,
   127,
   168,
   1);
   var purple = A4(RGBA,
   117,
   80,
   123,
   1);
   var darkPurple = A4(RGBA,
   92,
   53,
   102,
   1);
   var lightBrown = A4(RGBA,
   233,
   185,
   110,
   1);
   var brown = A4(RGBA,
   193,
   125,
   17,
   1);
   var darkBrown = A4(RGBA,
   143,
   89,
   2,
   1);
   var black = A4(RGBA,0,0,0,1);
   var white = A4(RGBA,
   255,
   255,
   255,
   1);
   var lightGrey = A4(RGBA,
   238,
   238,
   236,
   1);
   var grey = A4(RGBA,
   211,
   215,
   207,
   1);
   var darkGrey = A4(RGBA,
   186,
   189,
   182,
   1);
   var lightGray = A4(RGBA,
   238,
   238,
   236,
   1);
   var gray = A4(RGBA,
   211,
   215,
   207,
   1);
   var darkGray = A4(RGBA,
   186,
   189,
   182,
   1);
   var lightCharcoal = A4(RGBA,
   136,
   138,
   133,
   1);
   var charcoal = A4(RGBA,
   85,
   87,
   83,
   1);
   var darkCharcoal = A4(RGBA,
   46,
   52,
   54,
   1);
   _elm.Color.values = {_op: _op
                       ,RGBA: RGBA
                       ,HSLA: HSLA
                       ,rgba: rgba
                       ,rgb: rgb
                       ,hsla: hsla
                       ,hsl: hsl
                       ,grayscale: grayscale
                       ,greyscale: greyscale
                       ,complement: complement
                       ,toHsl: toHsl
                       ,toRgb: toRgb
                       ,fmod: fmod
                       ,rgbToHsl: rgbToHsl
                       ,hslToRgb: hslToRgb
                       ,Linear: Linear
                       ,Radial: Radial
                       ,linear: linear
                       ,radial: radial
                       ,lightRed: lightRed
                       ,red: red
                       ,darkRed: darkRed
                       ,lightOrange: lightOrange
                       ,orange: orange
                       ,darkOrange: darkOrange
                       ,lightYellow: lightYellow
                       ,yellow: yellow
                       ,darkYellow: darkYellow
                       ,lightGreen: lightGreen
                       ,green: green
                       ,darkGreen: darkGreen
                       ,lightBlue: lightBlue
                       ,blue: blue
                       ,darkBlue: darkBlue
                       ,lightPurple: lightPurple
                       ,purple: purple
                       ,darkPurple: darkPurple
                       ,lightBrown: lightBrown
                       ,brown: brown
                       ,darkBrown: darkBrown
                       ,black: black
                       ,white: white
                       ,lightGrey: lightGrey
                       ,grey: grey
                       ,darkGrey: darkGrey
                       ,lightGray: lightGray
                       ,gray: gray
                       ,darkGray: darkGray
                       ,lightCharcoal: lightCharcoal
                       ,charcoal: charcoal
                       ,darkCharcoal: darkCharcoal};
   return _elm.Color.values;
};Elm.Date = Elm.Date || {};
Elm.Date.make = function (_elm) {
   "use strict";
   _elm.Date = _elm.Date || {};
   if (_elm.Date.values)
   return _elm.Date.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Date",
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Date = Elm.Native.Date.make(_elm),
   $Time = Elm.Time.make(_elm);
   var second = $Native$Date.second;
   var minute = $Native$Date.minute;
   var hour = $Native$Date.hour;
   var dayOfWeek = $Native$Date.dayOfWeek;
   var day = $Native$Date.day;
   var month = $Native$Date.month;
   var year = $Native$Date.year;
   var fromTime = $Native$Date.fromTime;
   var toTime = $Native$Date.toTime;
   var read = $Native$Date.read;
   var Dec = {ctor: "Dec"};
   var Nov = {ctor: "Nov"};
   var Oct = {ctor: "Oct"};
   var Sep = {ctor: "Sep"};
   var Aug = {ctor: "Aug"};
   var Jul = {ctor: "Jul"};
   var Jun = {ctor: "Jun"};
   var May = {ctor: "May"};
   var Apr = {ctor: "Apr"};
   var Mar = {ctor: "Mar"};
   var Feb = {ctor: "Feb"};
   var Jan = {ctor: "Jan"};
   var Sun = {ctor: "Sun"};
   var Sat = {ctor: "Sat"};
   var Fri = {ctor: "Fri"};
   var Thu = {ctor: "Thu"};
   var Wed = {ctor: "Wed"};
   var Tue = {ctor: "Tue"};
   var Mon = {ctor: "Mon"};
   var Date = {ctor: "Date"};
   _elm.Date.values = {_op: _op
                      ,Date: Date
                      ,Mon: Mon
                      ,Tue: Tue
                      ,Wed: Wed
                      ,Thu: Thu
                      ,Fri: Fri
                      ,Sat: Sat
                      ,Sun: Sun
                      ,Jan: Jan
                      ,Feb: Feb
                      ,Mar: Mar
                      ,Apr: Apr
                      ,May: May
                      ,Jun: Jun
                      ,Jul: Jul
                      ,Aug: Aug
                      ,Sep: Sep
                      ,Oct: Oct
                      ,Nov: Nov
                      ,Dec: Dec
                      ,read: read
                      ,toTime: toTime
                      ,fromTime: fromTime
                      ,year: year
                      ,month: month
                      ,day: day
                      ,dayOfWeek: dayOfWeek
                      ,hour: hour
                      ,minute: minute
                      ,second: second};
   return _elm.Date.values;
};Elm.Debug = Elm.Debug || {};
Elm.Debug.make = function (_elm) {
   "use strict";
   _elm.Debug = _elm.Debug || {};
   if (_elm.Debug.values)
   return _elm.Debug.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Debug",
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm),
   $Native$Error = Elm.Native.Error.make(_elm);
   var trace = $Native$Debug.tracePath;
   var watchSummary = $Native$Debug.watchSummary;
   var watch = $Native$Debug.watch;
   var crash = $Native$Error.raise;
   var log = $Native$Debug.log;
   _elm.Debug.values = {_op: _op
                       ,log: log
                       ,crash: crash
                       ,watch: watch
                       ,watchSummary: watchSummary
                       ,trace: trace};
   return _elm.Debug.values;
};Elm.Dict = Elm.Dict || {};
Elm.Dict.make = function (_elm) {
   "use strict";
   _elm.Dict = _elm.Dict || {};
   if (_elm.Dict.values)
   return _elm.Dict.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Dict",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Error = Elm.Native.Error.make(_elm),
   $Native$Utils = Elm.Native.Utils.make(_elm);
   var foldr = F3(function (f,
   acc,
   t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldr,
              f,
              A3(f,
              t._1,
              t._2,
              A3(foldr,f,acc,t._4)),
              t._3);}
         _E.Case($moduleName,
         "between lines 369 and 374");
      }();
   });
   var keys = function (t) {
      return A3(foldr,
      F3(function (k,v,acc) {
         return A2($List._op["::"],
         k,
         acc);
      }),
      _L.fromArray([]),
      t);
   };
   var values = function (t) {
      return A3(foldr,
      F3(function (k,v,acc) {
         return A2($List._op["::"],
         v,
         acc);
      }),
      _L.fromArray([]),
      t);
   };
   var toList = function (t) {
      return A3(foldr,
      F3(function (k,v,acc) {
         return A2($List._op["::"],
         {ctor: "_Tuple2",_0: k,_1: v},
         acc);
      }),
      _L.fromArray([]),
      t);
   };
   var foldl = F3(function (f,
   acc,
   t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldl,
              f,
              A3(f,
              t._1,
              t._2,
              A3(foldl,f,acc,t._3)),
              t._4);}
         _E.Case($moduleName,
         "between lines 361 and 366");
      }();
   });
   var isBBlack = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBBlack": return true;}
              break;
            case "RBNode":
            switch (t._0.ctor)
              {case "BBlack": return true;}
              break;}
         return false;
      }();
   };
   var showFlag = function (f) {
      return function () {
         switch (f.ctor)
         {case "Insert": return "Insert";
            case "Remove": return "Remove";
            case "Same": return "Same";}
         _E.Case($moduleName,
         "between lines 187 and 192");
      }();
   };
   var Same = {ctor: "Same"};
   var Remove = {ctor: "Remove"};
   var Insert = {ctor: "Insert"};
   var getOrFail = F2(function (k,
   t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack":
                 return $Native$Error.raise("key not found when using \'getOrFail\'");}
              break;
            case "RBNode":
            return function () {
                 var _v29 = A2($Native$Utils.compare,
                 k,
                 t._1);
                 switch (_v29.ctor)
                 {case "EQ": return t._2;
                    case "GT": return A2(getOrFail,
                      k,
                      t._4);
                    case "LT": return A2(getOrFail,
                      k,
                      t._3);}
                 _E.Case($moduleName,
                 "between lines 156 and 161");
              }();}
         _E.Case($moduleName,
         "between lines 153 and 161");
      }();
   });
   var getOrElse = F3(function (base,
   k,
   t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack": return base;}
              break;
            case "RBNode":
            return function () {
                 var _v37 = A2($Native$Utils.compare,
                 k,
                 t._1);
                 switch (_v37.ctor)
                 {case "EQ": return t._2;
                    case "GT": return A3(getOrElse,
                      base,
                      k,
                      t._4);
                    case "LT": return A3(getOrElse,
                      base,
                      k,
                      t._3);}
                 _E.Case($moduleName,
                 "between lines 134 and 150");
              }();}
         _E.Case($moduleName,
         "between lines 131 and 150");
      }();
   });
   var get = F2(function (k,t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack":
                 return $Maybe.Nothing;}
              break;
            case "RBNode":
            return function () {
                 var _v45 = A2($Native$Utils.compare,
                 k,
                 t._1);
                 switch (_v45.ctor)
                 {case "EQ":
                    return $Maybe.Just(t._2);
                    case "GT": return A2(get,
                      k,
                      t._4);
                    case "LT": return A2(get,
                      k,
                      t._3);}
                 _E.Case($moduleName,
                 "between lines 115 and 128");
              }();}
         _E.Case($moduleName,
         "between lines 112 and 128");
      }();
   });
   var member = F2(function (k,t) {
      return $Maybe.isJust(A2(get,
      k,
      t));
   });
   var max = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return $Native$Error.raise("(max Empty) is not defined");
            case "RBNode":
            switch (t._4.ctor)
              {case "RBEmpty":
                 return {ctor: "_Tuple2"
                        ,_0: t._1
                        ,_1: t._2};}
              return max(t._4);}
         _E.Case($moduleName,
         "between lines 92 and 109");
      }();
   };
   var min = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack":
                 return $Native$Error.raise("(min Empty) is not defined");}
              break;
            case "RBNode":
            switch (t._3.ctor)
              {case "RBEmpty":
                 switch (t._3._0.ctor)
                   {case "LBlack":
                      return {ctor: "_Tuple2"
                             ,_0: t._1
                             ,_1: t._2};}
                   break;}
              return min(t._3);}
         _E.Case($moduleName,
         "between lines 85 and 88");
      }();
   };
   var RBEmpty = function (a) {
      return {ctor: "RBEmpty"
             ,_0: a};
   };
   var RBNode = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "RBNode"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var showLColor = function (c) {
      return function () {
         switch (c.ctor)
         {case "LBBlack":
            return "LBBlack";
            case "LBlack": return "LBlack";}
         _E.Case($moduleName,
         "between lines 71 and 73");
      }();
   };
   var LBBlack = {ctor: "LBBlack"};
   var LBlack = {ctor: "LBlack"};
   var empty = RBEmpty(LBlack);
   var map = F2(function (f,t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              t._0,
              t._1,
              f(t._2),
              A2(map,f,t._3),
              A2(map,f,t._4));}
         _E.Case($moduleName,
         "between lines 353 and 358");
      }();
   });
   var showNColor = function (c) {
      return function () {
         switch (c.ctor)
         {case "BBlack": return "BBlack";
            case "Black": return "Black";
            case "NBlack": return "NBlack";
            case "Red": return "Red";}
         _E.Case($moduleName,
         "between lines 60 and 64");
      }();
   };
   var reportRemBug = F4(function (msg,
   c,
   lgot,
   rgot) {
      return $Native$Error.raise($List.concat(_L.fromArray(["Internal red-black tree invariant violated, expected "
                                                           ,msg
                                                           ,"and got"
                                                           ,showNColor(c)
                                                           ," "
                                                           ,lgot
                                                           ," "
                                                           ,rgot
                                                           ,"\nPlease report this bug to https://github.com/elm-lang/Elm/issues"])));
   });
   var NBlack = {ctor: "NBlack"};
   var BBlack = {ctor: "BBlack"};
   var Black = {ctor: "Black"};
   var ensureBlackRoot = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack": return t;}
              break;
            case "RBNode":
            switch (t._0.ctor)
              {case "Black": return t;
                 case "Red": return A5(RBNode,
                   Black,
                   t._1,
                   t._2,
                   t._3,
                   t._4);}
              break;}
         _E.Case($moduleName,
         "between lines 168 and 174");
      }();
   };
   var blackish = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty": return true;
            case "RBNode":
            return _U.eq(t._0,
              Black) || _U.eq(t._0,BBlack);}
         _E.Case($moduleName,
         "between lines 304 and 306");
      }();
   };
   var blacken = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return RBEmpty(LBlack);
            case "RBNode": return A5(RBNode,
              Black,
              t._1,
              t._2,
              t._3,
              t._4);}
         _E.Case($moduleName,
         "between lines 340 and 342");
      }();
   };
   var Red = {ctor: "Red"};
   var moreBlack = function (c) {
      return function () {
         switch (c.ctor)
         {case "BBlack":
            return $Native$Error.raise("Can\'t make a double black node more black!");
            case "Black": return BBlack;
            case "NBlack": return Red;
            case "Red": return Black;}
         _E.Case($moduleName,
         "between lines 230 and 234");
      }();
   };
   var lessBlack = function (c) {
      return function () {
         switch (c.ctor)
         {case "BBlack": return Black;
            case "Black": return Red;
            case "NBlack":
            return $Native$Error.raise("Can\'t make a negative black node less black!");
            case "Red": return NBlack;}
         _E.Case($moduleName,
         "between lines 237 and 241");
      }();
   };
   var lessBlackTree = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              lessBlack(t._0),
              t._1,
              t._2,
              t._3,
              t._4);}
         _E.Case($moduleName,
         "between lines 244 and 246");
      }();
   };
   var redden = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return $Native$Error.raise("can\'t make a Leaf red");
            case "RBNode": return A5(RBNode,
              Red,
              t._1,
              t._2,
              t._3,
              t._4);}
         _E.Case($moduleName,
         "between lines 346 and 350");
      }();
   };
   var balance_node = function (t) {
      return function () {
         var assemble = function (col) {
            return function (xk) {
               return function (xv) {
                  return function (yk) {
                     return function (yv) {
                        return function (zk) {
                           return function (zv) {
                              return function (a) {
                                 return function (b) {
                                    return function (c) {
                                       return function (d) {
                                          return A5(RBNode,
                                          lessBlack(col),
                                          yk,
                                          yv,
                                          A5(RBNode,Black,xk,xv,a,b),
                                          A5(RBNode,Black,zk,zv,c,d));
                                       };
                                    };
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
         return blackish(t) ? function () {
            switch (t.ctor)
            {case "RBNode":
               switch (t._3.ctor)
                 {case "RBNode":
                    switch (t._3._0.ctor)
                      {case "Red":
                         switch (t._3._3.ctor)
                           {case "RBNode":
                              switch (t._3._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._3._1)(t._3._3._2)(t._3._1)(t._3._2)(t._1)(t._2)(t._3._3._3)(t._3._3._4)(t._3._4)(t._4);}
                                break;}
                           switch (t._3._4.ctor)
                           {case "RBNode":
                              switch (t._3._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._1)(t._3._2)(t._3._4._1)(t._3._4._2)(t._1)(t._2)(t._3._3)(t._3._4._3)(t._3._4._4)(t._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._4.ctor)
                 {case "RBNode":
                    switch (t._4._0.ctor)
                      {case "Red":
                         switch (t._4._3.ctor)
                           {case "RBNode":
                              switch (t._4._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._3._1)(t._4._3._2)(t._4._1)(t._4._2)(t._3)(t._4._3._3)(t._4._3._4)(t._4._4);}
                                break;}
                           switch (t._4._4.ctor)
                           {case "RBNode":
                              switch (t._4._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._1)(t._4._2)(t._4._4._1)(t._4._4._2)(t._3)(t._4._3)(t._4._4._3)(t._4._4._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._0.ctor)
                 {case "BBlack":
                    switch (t._4.ctor)
                      {case "RBNode":
                         switch (t._4._0.ctor)
                           {case "NBlack":
                              switch (t._4._3.ctor)
                                {case "RBNode":
                                   switch (t._4._3._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._4._4.ctor)
                                             {case "RBNode":
                                                switch (t._4._4._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._4._3._1,
                                                       t._4._3._2,
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3,
                                                       t._4._3._3),
                                                       A5(balance,
                                                       Black,
                                                       t._4._1,
                                                       t._4._2,
                                                       t._4._3._4,
                                                       redden(t._4._4)));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      switch (t._3.ctor)
                      {case "RBNode":
                         switch (t._3._0.ctor)
                           {case "NBlack":
                              switch (t._3._4.ctor)
                                {case "RBNode":
                                   switch (t._3._4._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._3._3.ctor)
                                             {case "RBNode":
                                                switch (t._3._3._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._3._4._1,
                                                       t._3._4._2,
                                                       A5(balance,
                                                       Black,
                                                       t._3._1,
                                                       t._3._2,
                                                       redden(t._3._3),
                                                       t._3._4._3),
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3._4._4,
                                                       t._4));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      break;}
                 break;}
            return t;
         }() : t;
      }();
   };
   var balance = F5(function (c,
   k,
   v,
   l,
   r) {
      return balance_node(A5(RBNode,
      c,
      k,
      v,
      l,
      r));
   });
   var bubble = F5(function (c,
   k,
   v,
   l,
   r) {
      return isBBlack(l) || isBBlack(r) ? A5(balance,
      moreBlack(c),
      k,
      v,
      lessBlackTree(l),
      lessBlackTree(r)) : A5(RBNode,
      c,
      k,
      v,
      l,
      r);
   });
   var remove_max = F5(function (c,
   k,
   v,
   l,
   r) {
      return function () {
         switch (r.ctor)
         {case "RBEmpty": return A3(rem,
              c,
              l,
              r);
            case "RBNode": return A5(bubble,
              c,
              k,
              v,
              l,
              A5(remove_max,
              r._0,
              r._1,
              r._2,
              r._3,
              r._4));}
         _E.Case($moduleName,
         "between lines 292 and 295");
      }();
   });
   var rem = F3(function (c,l,r) {
      return function () {
         var _v183 = {ctor: "_Tuple2"
                     ,_0: l
                     ,_1: r};
         switch (_v183.ctor)
         {case "_Tuple2":
            switch (_v183._0.ctor)
              {case "RBEmpty":
                 switch (_v183._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           switch (c.ctor)
                           {case "Black":
                              return RBEmpty(LBBlack);
                              case "Red":
                              return RBEmpty(LBlack);}
                           _E.Case($moduleName,
                           "between lines 265 and 268");
                        }();
                      case "RBNode":
                      return function () {
                           var _v205 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v183._0._0
                                       ,_2: _v183._1._0};
                           switch (_v205.ctor)
                           {case "_Tuple3":
                              switch (_v205._0.ctor)
                                {case "Black":
                                   switch (_v205._1.ctor)
                                     {case "LBlack":
                                        switch (_v205._2.ctor)
                                          {case "Red": return A5(RBNode,
                                               Black,
                                               _v183._1._1,
                                               _v183._1._2,
                                               _v183._1._3,
                                               _v183._1._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black, LBlack, Red",
                           c,
                           showLColor(_v183._0._0),
                           showNColor(_v183._1._0));
                        }();}
                   break;
                 case "RBNode":
                 switch (_v183._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           var _v209 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v183._0._0
                                       ,_2: _v183._1._0};
                           switch (_v209.ctor)
                           {case "_Tuple3":
                              switch (_v209._0.ctor)
                                {case "Black":
                                   switch (_v209._1.ctor)
                                     {case "Red":
                                        switch (_v209._2.ctor)
                                          {case "LBlack":
                                             return A5(RBNode,
                                               Black,
                                               _v183._0._1,
                                               _v183._0._2,
                                               _v183._0._3,
                                               _v183._0._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black, Red, LBlack",
                           c,
                           showNColor(_v183._0._0),
                           showLColor(_v183._1._0));
                        }();
                      case "RBNode":
                      return function () {
                           var l$ = A5(remove_max,
                           _v183._0._0,
                           _v183._0._1,
                           _v183._0._2,
                           _v183._0._3,
                           _v183._0._4);
                           var r = A5(RBNode,
                           _v183._1._0,
                           _v183._1._1,
                           _v183._1._2,
                           _v183._1._3,
                           _v183._1._4);
                           var l = A5(RBNode,
                           _v183._0._0,
                           _v183._0._1,
                           _v183._0._2,
                           _v183._0._3,
                           _v183._0._4);
                           var $ = max(l),
                           k = $._0,
                           v = $._1;
                           return A5(bubble,c,k,v,l$,r);
                        }();}
                   break;}
              break;}
         _E.Case($moduleName,
         "between lines 264 and 282");
      }();
   });
   var update = F3(function (k,
   u,
   t) {
      return function () {
         var up = function (t) {
            return function () {
               switch (t.ctor)
               {case "RBEmpty":
                  switch (t._0.ctor)
                    {case "LBlack":
                       return function () {
                            var _v220 = u($Maybe.Nothing);
                            switch (_v220.ctor)
                            {case "Just":
                               return {ctor: "_Tuple2"
                                      ,_0: Insert
                                      ,_1: A5(RBNode,
                                      Red,
                                      k,
                                      _v220._0,
                                      empty,
                                      empty)};
                               case "Nothing":
                               return {ctor: "_Tuple2"
                                      ,_0: Same
                                      ,_1: empty};}
                            _E.Case($moduleName,
                            "between lines 196 and 199");
                         }();}
                    break;
                  case "RBNode":
                  return function () {
                       var _v222 = A2($Native$Utils.compare,
                       k,
                       t._1);
                       switch (_v222.ctor)
                       {case "EQ": return function () {
                               var _v223 = u($Maybe.Just(t._2));
                               switch (_v223.ctor)
                               {case "Just":
                                  return {ctor: "_Tuple2"
                                         ,_0: Same
                                         ,_1: A5(RBNode,
                                         t._0,
                                         t._1,
                                         _v223._0,
                                         t._3,
                                         t._4)};
                                  case "Nothing":
                                  return {ctor: "_Tuple2"
                                         ,_0: Remove
                                         ,_1: A3(rem,t._0,t._3,t._4)};}
                               _E.Case($moduleName,
                               "between lines 200 and 203");
                            }();
                          case "GT": return function () {
                               var $ = up(t._4),
                               fl = $._0,
                               r$ = $._1;
                               return function () {
                                  switch (fl.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            t._0,
                                            t._1,
                                            t._2,
                                            t._3,
                                            r$)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            t._0,
                                            t._1,
                                            t._2,
                                            t._3,
                                            r$)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            t._0,
                                            t._1,
                                            t._2,
                                            t._3,
                                            r$)};}
                                  _E.Case($moduleName,
                                  "between lines 209 and 213");
                               }();
                            }();
                          case "LT": return function () {
                               var $ = up(t._3),
                               fl = $._0,
                               l$ = $._1;
                               return function () {
                                  switch (fl.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            t._0,
                                            t._1,
                                            t._2,
                                            l$,
                                            t._4)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            t._0,
                                            t._1,
                                            t._2,
                                            l$,
                                            t._4)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            t._0,
                                            t._1,
                                            t._2,
                                            l$,
                                            t._4)};}
                                  _E.Case($moduleName,
                                  "between lines 204 and 208");
                               }();
                            }();}
                       _E.Case($moduleName,
                       "between lines 199 and 213");
                    }();}
               _E.Case($moduleName,
               "between lines 195 and 213");
            }();
         };
         var $ = up(t),
         fl = $._0,
         t$ = $._1;
         return function () {
            switch (fl.ctor)
            {case "Insert":
               return ensureBlackRoot(t$);
               case "Remove":
               return blacken(t$);
               case "Same": return t$;}
            _E.Case($moduleName,
            "between lines 214 and 219");
         }();
      }();
   });
   var insert = F3(function (k,
   v,
   t) {
      return function () {
         var u = function (_v228) {
            return function () {
               return $Maybe.Just(v);
            }();
         };
         return A3(update,k,u,t);
      }();
   });
   var singleton = F2(function (k,
   v) {
      return A3(insert,
      k,
      v,
      RBEmpty(LBlack));
   });
   var union = F2(function (t1,
   t2) {
      return A3(foldl,
      insert,
      t2,
      t1);
   });
   var fromList = function (assocs) {
      return A3($List.foldl,
      F2(function (_v230,d) {
         return function () {
            switch (_v230.ctor)
            {case "_Tuple2":
               return A3(insert,
                 _v230._0,
                 _v230._1,
                 d);}
            _E.Case($moduleName,
            "on line 402, column 43 to 55");
         }();
      }),
      empty,
      assocs);
   };
   var filter = F2(function (p,
   dict) {
      return function () {
         var add = F3(function (k,
         v,
         t) {
            return A2(p,k,v) ? A3(insert,
            k,
            v,
            t) : t;
         });
         return A3(foldl,add,empty,dict);
      }();
   });
   var intersect = F2(function (t1,
   t2) {
      return A2(filter,
      F2(function (k,_v234) {
         return function () {
            return A2(member,k,t2);
         }();
      }),
      t1);
   });
   var partition = F2(function (p,
   dict) {
      return function () {
         var add = F3(function (k,
         v,
         _v236) {
            return function () {
               switch (_v236.ctor)
               {case "_Tuple2": return A2(p,
                    k,
                    v) ? {ctor: "_Tuple2"
                         ,_0: A3(insert,k,v,_v236._0)
                         ,_1: _v236._1} : {ctor: "_Tuple2"
                                          ,_0: _v236._0
                                          ,_1: A3(insert,k,v,_v236._1)};}
               _E.Case($moduleName,
               "between lines 416 and 418");
            }();
         });
         return A3(foldl,
         add,
         {ctor: "_Tuple2"
         ,_0: empty
         ,_1: empty},
         dict);
      }();
   });
   var remove = F2(function (k,t) {
      return function () {
         var u = function (_v240) {
            return function () {
               return $Maybe.Nothing;
            }();
         };
         return A3(update,k,u,t);
      }();
   });
   var diff = F2(function (t1,t2) {
      return A3(foldl,
      F3(function (k,v,t) {
         return A2(remove,k,t);
      }),
      t1,
      t2);
   });
   _elm.Dict.values = {_op: _op
                      ,empty: empty
                      ,singleton: singleton
                      ,insert: insert
                      ,update: update
                      ,get: get
                      ,getOrElse: getOrElse
                      ,getOrFail: getOrFail
                      ,remove: remove
                      ,member: member
                      ,filter: filter
                      ,partition: partition
                      ,foldl: foldl
                      ,foldr: foldr
                      ,map: map
                      ,union: union
                      ,intersect: intersect
                      ,diff: diff
                      ,keys: keys
                      ,values: values
                      ,toList: toList
                      ,fromList: fromList};
   return _elm.Dict.values;
};Elm.Either = Elm.Either || {};
Elm.Either.make = function (_elm) {
   "use strict";
   _elm.Either = _elm.Either || {};
   if (_elm.Either.values)
   return _elm.Either.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Either",
   $List = Elm.List.make(_elm);
   var consEither = F2(function (e,
   _v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return function () {
                 switch (e.ctor)
                 {case "Left":
                    return {ctor: "_Tuple2"
                           ,_0: A2($List._op["::"],
                           e._0,
                           _v0._0)
                           ,_1: _v0._1};
                    case "Right":
                    return {ctor: "_Tuple2"
                           ,_0: _v0._0
                           ,_1: A2($List._op["::"],
                           e._0,
                           _v0._1)};}
                 _E.Case($moduleName,
                 "between lines 89 and 91");
              }();}
         _E.Case($moduleName,
         "between lines 89 and 91");
      }();
   });
   var consRight = F2(function (e,
   vs) {
      return function () {
         switch (e.ctor)
         {case "Left": return vs;
            case "Right":
            return A2($List._op["::"],
              e._0,
              vs);}
         _E.Case($moduleName,
         "between lines 84 and 86");
      }();
   });
   var consLeft = F2(function (e,
   vs) {
      return function () {
         switch (e.ctor)
         {case "Left":
            return A2($List._op["::"],
              e._0,
              vs);
            case "Right": return vs;}
         _E.Case($moduleName,
         "between lines 79 and 81");
      }();
   });
   var partition = function (es) {
      return A3($List.foldr,
      consEither,
      {ctor: "_Tuple2"
      ,_0: _L.fromArray([])
      ,_1: _L.fromArray([])},
      es);
   };
   var rights = function (es) {
      return A3($List.foldr,
      consRight,
      _L.fromArray([]),
      es);
   };
   var lefts = function (es) {
      return A3($List.foldr,
      consLeft,
      _L.fromArray([]),
      es);
   };
   var isRight = function (e) {
      return function () {
         switch (e.ctor)
         {case "Right": return true;}
         return false;
      }();
   };
   var isLeft = function (e) {
      return function () {
         switch (e.ctor)
         {case "Left": return true;}
         return false;
      }();
   };
   var either = F3(function (f,
   g,
   e) {
      return function () {
         switch (e.ctor)
         {case "Left": return f(e._0);
            case "Right": return g(e._0);}
         _E.Case($moduleName,
         "on line 38, column 16 to 60");
      }();
   });
   var Right = function (a) {
      return {ctor: "Right",_0: a};
   };
   var Left = function (a) {
      return {ctor: "Left",_0: a};
   };
   _elm.Either.values = {_op: _op
                        ,Left: Left
                        ,Right: Right
                        ,either: either
                        ,isLeft: isLeft
                        ,isRight: isRight
                        ,lefts: lefts
                        ,rights: rights
                        ,partition: partition
                        ,consLeft: consLeft
                        ,consRight: consRight
                        ,consEither: consEither};
   return _elm.Either.values;
};Elm.Http = Elm.Http || {};
Elm.Http.make = function (_elm) {
   "use strict";
   _elm.Http = _elm.Http || {};
   if (_elm.Http.values)
   return _elm.Http.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Http",
   $Native$Http = Elm.Native.Http.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var send = $Native$Http.send;
   var Request = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,body: c
             ,headers: d
             ,url: b
             ,verb: a};
   });
   var request = Request;
   var get = function (url) {
      return A4(Request,
      "GET",
      url,
      "",
      _L.fromArray([]));
   };
   var sendGet = function (reqs) {
      return send(A2($Signal.lift,
      get,
      reqs));
   };
   var post = F2(function (url,
   body) {
      return A4(Request,
      "POST",
      url,
      body,
      _L.fromArray([]));
   });
   var Failure = F2(function (a,
   b) {
      return {ctor: "Failure"
             ,_0: a
             ,_1: b};
   });
   var Waiting = {ctor: "Waiting"};
   var Success = function (a) {
      return {ctor: "Success"
             ,_0: a};
   };
   _elm.Http.values = {_op: _op
                      ,Success: Success
                      ,Waiting: Waiting
                      ,Failure: Failure
                      ,Request: Request
                      ,request: request
                      ,get: get
                      ,post: post
                      ,send: send
                      ,sendGet: sendGet};
   return _elm.Http.values;
};Elm.Json = Elm.Json || {};
Elm.Json.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   if (_elm.Json.values)
   return _elm.Json.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Json",
   $Dict = Elm.Dict.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm);
   var fromString = function (str) {
      return $Native$Json.fromString(str);
   };
   var toString = F2(function (sep,
   value) {
      return A2($Native$Json.toString,
      sep,
      value);
   });
   var Object = function (a) {
      return {ctor: "Object"
             ,_0: a};
   };
   var Array = function (a) {
      return {ctor: "Array",_0: a};
   };
   var Null = {ctor: "Null"};
   var Boolean = function (a) {
      return {ctor: "Boolean"
             ,_0: a};
   };
   var Number = function (a) {
      return {ctor: "Number"
             ,_0: a};
   };
   var String = function (a) {
      return {ctor: "String"
             ,_0: a};
   };
   _elm.Json.values = {_op: _op
                      ,String: String
                      ,Number: Number
                      ,Boolean: Boolean
                      ,Null: Null
                      ,Array: Array
                      ,Object: Object
                      ,toString: toString
                      ,fromString: fromString};
   return _elm.Json.values;
};Elm.Keyboard = Elm.Keyboard || {};
Elm.Keyboard.make = function (_elm) {
   "use strict";
   _elm.Keyboard = _elm.Keyboard || {};
   if (_elm.Keyboard.values)
   return _elm.Keyboard.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Keyboard",
   $Native$Keyboard = Elm.Native.Keyboard.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var lastPressed = $Native$Keyboard.lastPressed;
   var keysDown = $Native$Keyboard.keysDown;
   var meta = $Native$Keyboard.meta;
   var alt = $Native$Keyboard.alt;
   var isDown = $Native$Keyboard.isDown;
   var ctrl = isDown(17);
   var shift = isDown(16);
   var space = isDown(32);
   var enter = isDown(13);
   var directions = $Native$Keyboard.directions;
   var arrows = A4(directions,
   38,
   40,
   37,
   39);
   var wasd = A4(directions,
   87,
   83,
   65,
   68);
   _elm.Keyboard.values = {_op: _op
                          ,directions: directions
                          ,arrows: arrows
                          ,wasd: wasd
                          ,isDown: isDown
                          ,alt: alt
                          ,ctrl: ctrl
                          ,meta: meta
                          ,shift: shift
                          ,space: space
                          ,enter: enter
                          ,keysDown: keysDown
                          ,lastPressed: lastPressed};
   return _elm.Keyboard.values;
};Elm.List = Elm.List || {};
Elm.List.make = function (_elm) {
   "use strict";
   _elm.List = _elm.List || {};
   if (_elm.List.values)
   return _elm.List.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "List",
   $Basics = Elm.Basics.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$List = Elm.Native.List.make(_elm);
   var sortWith = $Native$List.sortWith;
   var sortBy = $Native$List.sortBy;
   var sort = $Native$List.sort;
   var repeat = $Native$List.repeat;
   var drop = $Native$List.drop;
   var take = $Native$List.take;
   var join = $Native$List.join;
   var zipWith5 = $Native$List.zipWith5;
   var zipWith4 = $Native$List.zipWith4;
   var zipWith3 = $Native$List.zipWith3;
   var zipWith = $Native$List.zipWith;
   var zip5 = $Native$List.zipWith5(F5(function (v0,
   v1,
   v2,
   v3,
   v4) {
      return {ctor: "_Tuple5"
             ,_0: v0
             ,_1: v1
             ,_2: v2
             ,_3: v3
             ,_4: v4};
   }));
   var zip4 = $Native$List.zipWith4(F4(function (v0,
   v1,
   v2,
   v3) {
      return {ctor: "_Tuple4"
             ,_0: v0
             ,_1: v1
             ,_2: v2
             ,_3: v3};
   }));
   var zip3 = $Native$List.zipWith3(F3(function (v0,
   v1,
   v2) {
      return {ctor: "_Tuple3"
             ,_0: v0
             ,_1: v1
             ,_2: v2};
   }));
   var zip = $Native$List.zip;
   var concat = $Native$List.concat;
   var any = $Native$List.any;
   var all = $Native$List.all;
   var reverse = $Native$List.reverse;
   var length = $Native$List.length;
   var filter = $Native$List.filter;
   var scanl1 = $Native$List.scanl1;
   var scanl = $Native$List.scanl;
   var foldr1 = $Native$List.foldr1;
   var foldl1 = $Native$List.foldl1;
   var maximum = foldl1($Basics.max);
   var minimum = foldl1($Basics.min);
   var foldr = $Native$List.foldr;
   var foldl = $Native$List.foldl;
   var sum = A2(foldl,
   F2(function (x,y) {
      return x + y;
   }),
   0);
   var product = A2(foldl,
   F2(function (x,y) {
      return x * y;
   }),
   1);
   var indexedMap = F2(function (f,
   xs) {
      return A3(zipWith,
      f,
      _L.range(0,length(xs) - 1),
      xs);
   });
   var map = $Native$List.map;
   var concatMap = F2(function (f,
   list) {
      return concat(A2(map,
      f,
      list));
   });
   var isEmpty = function (xs) {
      return function () {
         switch (xs.ctor)
         {case "[]": return true;}
         return false;
      }();
   };
   var last = $Native$List.last;
   var tail = $Native$List.tail;
   var head = $Native$List.head;
   _op["++"] = $Native$List.append;
   _op["::"] = $Native$List.cons;
   var maybeCons = F3(function (f,
   mx,
   xs) {
      return function () {
         var _v1 = f(mx);
         switch (_v1.ctor)
         {case "Just":
            return A2(_op["::"],_v1._0,xs);
            case "Nothing": return xs;}
         _E.Case($moduleName,
         "between lines 145 and 149");
      }();
   });
   var filterMap = F2(function (f,
   xs) {
      return A3(foldr,
      maybeCons(f),
      _L.fromArray([]),
      xs);
   });
   var partition = function (pred) {
      return function () {
         var step = F2(function (x,
         _v3) {
            return function () {
               switch (_v3.ctor)
               {case "_Tuple2":
                  return pred(x) ? {ctor: "_Tuple2"
                                   ,_0: A2(_op["::"],x,_v3._0)
                                   ,_1: _v3._1} : {ctor: "_Tuple2"
                                                  ,_0: _v3._0
                                                  ,_1: A2(_op["::"],x,_v3._1)};}
               _E.Case($moduleName,
               "between lines 205 and 207");
            }();
         });
         return A2(foldr,
         step,
         {ctor: "_Tuple2"
         ,_0: _L.fromArray([])
         ,_1: _L.fromArray([])});
      }();
   };
   var unzip = function () {
      var step = F2(function (_v7,
      _v8) {
         return function () {
            switch (_v8.ctor)
            {case "_Tuple2":
               return function () {
                    switch (_v7.ctor)
                    {case "_Tuple2":
                       return {ctor: "_Tuple2"
                              ,_0: A2(_op["::"],_v7._0,_v8._0)
                              ,_1: A2(_op["::"],
                              _v7._1,
                              _v8._1)};}
                    _E.Case($moduleName,
                    "on line 251, column 32 to 44");
                 }();}
            _E.Case($moduleName,
            "on line 251, column 32 to 44");
         }();
      });
      return A2(foldr,
      step,
      {ctor: "_Tuple2"
      ,_0: _L.fromArray([])
      ,_1: _L.fromArray([])});
   }();
   var intersperse = F2(function (sep,
   xs) {
      return function () {
         switch (xs.ctor)
         {case "::": return function () {
                 var step = F2(function (x,
                 rest) {
                    return A2(_op["::"],
                    sep,
                    A2(_op["::"],x,rest));
                 });
                 var spersed = A3(foldr,
                 step,
                 _L.fromArray([]),
                 xs._1);
                 return A2(_op["::"],
                 xs._0,
                 spersed);
              }();
            case "[]":
            return _L.fromArray([]);}
         _E.Case($moduleName,
         "between lines 268 and 275");
      }();
   });
   _elm.List.values = {_op: _op
                      ,head: head
                      ,tail: tail
                      ,last: last
                      ,isEmpty: isEmpty
                      ,map: map
                      ,indexedMap: indexedMap
                      ,foldl: foldl
                      ,foldr: foldr
                      ,foldl1: foldl1
                      ,foldr1: foldr1
                      ,scanl: scanl
                      ,scanl1: scanl1
                      ,filter: filter
                      ,filterMap: filterMap
                      ,maybeCons: maybeCons
                      ,length: length
                      ,reverse: reverse
                      ,all: all
                      ,any: any
                      ,concat: concat
                      ,concatMap: concatMap
                      ,sum: sum
                      ,product: product
                      ,maximum: maximum
                      ,minimum: minimum
                      ,partition: partition
                      ,zip: zip
                      ,zip3: zip3
                      ,zip4: zip4
                      ,zip5: zip5
                      ,zipWith: zipWith
                      ,zipWith3: zipWith3
                      ,zipWith4: zipWith4
                      ,zipWith5: zipWith5
                      ,unzip: unzip
                      ,join: join
                      ,intersperse: intersperse
                      ,take: take
                      ,drop: drop
                      ,repeat: repeat
                      ,sort: sort
                      ,sortBy: sortBy
                      ,sortWith: sortWith};
   return _elm.List.values;
};Elm.Maybe = Elm.Maybe || {};
Elm.Maybe.make = function (_elm) {
   "use strict";
   _elm.Maybe = _elm.Maybe || {};
   if (_elm.Maybe.values)
   return _elm.Maybe.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Maybe";
   var maybe = F3(function (b,
   f,
   m) {
      return function () {
         switch (m.ctor)
         {case "Just": return f(m._0);
            case "Nothing": return b;}
         _E.Case($moduleName,
         "between lines 30 and 39");
      }();
   });
   var isJust = A2(maybe,
   false,
   function (_v2) {
      return function () {
         return true;
      }();
   });
   var isNothing = A2(maybe,
   true,
   function (_v4) {
      return function () {
         return false;
      }();
   });
   var Nothing = {ctor: "Nothing"};
   var Just = function (a) {
      return {ctor: "Just",_0: a};
   };
   var map = F2(function (f,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return Just(f(maybe._0));
            case "Nothing": return Nothing;}
         _E.Case($moduleName,
         "between lines 59 and 61");
      }();
   });
   _elm.Maybe.values = {_op: _op
                       ,maybe: maybe
                       ,isJust: isJust
                       ,isNothing: isNothing
                       ,map: map
                       ,Just: Just
                       ,Nothing: Nothing};
   return _elm.Maybe.values;
};Elm.Mouse = Elm.Mouse || {};
Elm.Mouse.make = function (_elm) {
   "use strict";
   _elm.Mouse = _elm.Mouse || {};
   if (_elm.Mouse.values)
   return _elm.Mouse.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Mouse",
   $Native$Mouse = Elm.Native.Mouse.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var clicks = $Native$Mouse.clicks;
   var isDown = $Native$Mouse.isDown;
   var y = $Native$Mouse.y;
   var x = $Native$Mouse.x;
   var position = $Native$Mouse.position;
   _elm.Mouse.values = {_op: _op
                       ,position: position
                       ,x: x
                       ,y: y
                       ,isDown: isDown
                       ,clicks: clicks};
   return _elm.Mouse.values;
};Elm.Random = Elm.Random || {};
Elm.Random.make = function (_elm) {
   "use strict";
   _elm.Random = _elm.Random || {};
   if (_elm.Random.values)
   return _elm.Random.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Random",
   $Native$Random = Elm.Native.Random.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var floatList = $Native$Random.floatList;
   var $float = $Native$Random.float_;
   var range = $Native$Random.range;
   _elm.Random.values = {_op: _op
                        ,range: range
                        ,$float: $float
                        ,floatList: floatList};
   return _elm.Random.values;
};Elm.Regex = Elm.Regex || {};
Elm.Regex.make = function (_elm) {
   "use strict";
   _elm.Regex = _elm.Regex || {};
   if (_elm.Regex.values)
   return _elm.Regex.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Regex",
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Regex = Elm.Native.Regex.make(_elm);
   var split = $Native$Regex.split;
   var replace = $Native$Regex.replace;
   var find = $Native$Regex.find;
   var AtMost = function (a) {
      return {ctor: "AtMost"
             ,_0: a};
   };
   var All = {ctor: "All"};
   var Match = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,index: c
             ,match: a
             ,number: d
             ,submatches: b};
   });
   var contains = $Native$Regex.contains;
   var caseInsensitive = $Native$Regex.caseInsensitive;
   var regex = $Native$Regex.regex;
   var escape = $Native$Regex.escape;
   var Regex = {ctor: "Regex"};
   _elm.Regex.values = {_op: _op
                       ,Regex: Regex
                       ,escape: escape
                       ,regex: regex
                       ,caseInsensitive: caseInsensitive
                       ,contains: contains
                       ,Match: Match
                       ,All: All
                       ,AtMost: AtMost
                       ,find: find
                       ,replace: replace
                       ,split: split};
   return _elm.Regex.values;
};Elm.Set = Elm.Set || {};
Elm.Set.make = function (_elm) {
   "use strict";
   _elm.Set = _elm.Set || {};
   if (_elm.Set.values)
   return _elm.Set.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Set",
   $Dict = Elm.Dict.make(_elm),
   $List = Elm.List.make(_elm);
   var partition = F2(function (p,
   set) {
      return A2($Dict.partition,
      F2(function (k,_v0) {
         return function () {
            return p(k);
         }();
      }),
      set);
   });
   var filter = F2(function (p,
   set) {
      return A2($Dict.filter,
      F2(function (k,_v2) {
         return function () {
            return p(k);
         }();
      }),
      set);
   });
   var foldr = F3(function (f,
   b,
   s) {
      return A3($Dict.foldr,
      F3(function (k,_v4,b) {
         return function () {
            return A2(f,k,b);
         }();
      }),
      b,
      s);
   });
   var foldl = F3(function (f,
   b,
   s) {
      return A3($Dict.foldl,
      F3(function (k,_v6,b) {
         return function () {
            return A2(f,k,b);
         }();
      }),
      b,
      s);
   });
   var toList = $Dict.keys;
   var diff = $Dict.diff;
   var intersect = $Dict.intersect;
   var union = $Dict.union;
   var member = $Dict.member;
   var remove = $Dict.remove;
   var insert = function (k) {
      return A2($Dict.insert,
      k,
      {ctor: "_Tuple0"});
   };
   var singleton = function (k) {
      return A2($Dict.singleton,
      k,
      {ctor: "_Tuple0"});
   };
   var empty = $Dict.empty;
   var fromList = function (xs) {
      return A3($List.foldl,
      insert,
      empty,
      xs);
   };
   var map = F2(function (f,s) {
      return fromList(A2($List.map,
      f,
      toList(s)));
   });
   _elm.Set.values = {_op: _op
                     ,empty: empty
                     ,singleton: singleton
                     ,insert: insert
                     ,remove: remove
                     ,member: member
                     ,foldl: foldl
                     ,foldr: foldr
                     ,map: map
                     ,filter: filter
                     ,partition: partition
                     ,union: union
                     ,intersect: intersect
                     ,diff: diff
                     ,toList: toList
                     ,fromList: fromList};
   return _elm.Set.values;
};Elm.Signal = Elm.Signal || {};
Elm.Signal.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   if (_elm.Signal.values)
   return _elm.Signal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Signal",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm);
   _op["~"] = F2(function (sf,s) {
      return A3($Native$Signal.lift2,
      F2(function (f,x) {
         return f(x);
      }),
      sf,
      s);
   });
   _op["<~"] = F2(function (f,s) {
      return A2($Native$Signal.lift,
      f,
      s);
   });
   var sampleOn = $Native$Signal.sampleOn;
   var dropRepeats = $Native$Signal.dropRepeats;
   var dropIf = $Native$Signal.dropIf;
   var keepIf = $Native$Signal.keepIf;
   var keepWhen = F3(function (bs,
   def,
   sig) {
      return A2(_op["<~"],
      $Basics.snd,
      A3(keepIf,
      $Basics.fst,
      {ctor: "_Tuple2"
      ,_0: false
      ,_1: def},
      A2(_op["~"],
      A2(_op["<~"],
      F2(function (v0,v1) {
         return {ctor: "_Tuple2"
                ,_0: v0
                ,_1: v1};
      }),
      A2(sampleOn,sig,bs)),
      sig)));
   });
   var dropWhen = function (bs) {
      return keepWhen(A2(_op["<~"],
      $Basics.not,
      bs));
   };
   var countIf = $Native$Signal.countIf;
   var count = $Native$Signal.count;
   var combine = A2($List.foldr,
   $Native$Signal.lift2(F2(function (x,
   y) {
      return A2($List._op["::"],
      x,
      y);
   })),
   $Native$Signal.constant(_L.fromArray([])));
   var merges = $Native$Signal.merges;
   var merge = $Native$Signal.merge;
   var foldp = $Native$Signal.foldp;
   var lift8 = $Native$Signal.lift8;
   var lift7 = $Native$Signal.lift7;
   var lift6 = $Native$Signal.lift6;
   var lift5 = $Native$Signal.lift5;
   var lift4 = $Native$Signal.lift4;
   var lift3 = $Native$Signal.lift3;
   var lift2 = $Native$Signal.lift2;
   var lift = $Native$Signal.lift;
   var constant = $Native$Signal.constant;
   var Signal = {ctor: "Signal"};
   _elm.Signal.values = {_op: _op
                        ,Signal: Signal
                        ,constant: constant
                        ,lift: lift
                        ,lift2: lift2
                        ,lift3: lift3
                        ,lift4: lift4
                        ,lift5: lift5
                        ,lift6: lift6
                        ,lift7: lift7
                        ,lift8: lift8
                        ,foldp: foldp
                        ,merge: merge
                        ,merges: merges
                        ,combine: combine
                        ,count: count
                        ,countIf: countIf
                        ,keepIf: keepIf
                        ,dropIf: dropIf
                        ,keepWhen: keepWhen
                        ,dropWhen: dropWhen
                        ,dropRepeats: dropRepeats
                        ,sampleOn: sampleOn};
   return _elm.Signal.values;
};Elm.String = Elm.String || {};
Elm.String.make = function (_elm) {
   "use strict";
   _elm.String = _elm.String || {};
   if (_elm.String.values)
   return _elm.String.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "String",
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Show = Elm.Native.Show.make(_elm),
   $Native$String = Elm.Native.String.make(_elm);
   var fromList = $Native$String.fromList;
   var toList = $Native$String.toList;
   var toFloat = $Native$String.toFloat;
   var toInt = $Native$String.toInt;
   var show = $Native$Show.show;
   var indices = $Native$String.indexes;
   var indexes = $Native$String.indexes;
   var endsWith = $Native$String.endsWith;
   var startsWith = $Native$String.startsWith;
   var contains = $Native$String.contains;
   var all = $Native$String.all;
   var any = $Native$String.any;
   var toLower = $Native$String.toLower;
   var toUpper = $Native$String.toUpper;
   var lines = $Native$String.lines;
   var words = $Native$String.words;
   var trimRight = $Native$String.trimRight;
   var trimLeft = $Native$String.trimLeft;
   var trim = $Native$String.trim;
   var padRight = $Native$String.padRight;
   var padLeft = $Native$String.padLeft;
   var pad = $Native$String.pad;
   var dropRight = $Native$String.dropRight;
   var dropLeft = $Native$String.dropLeft;
   var right = $Native$String.right;
   var left = $Native$String.left;
   var slice = $Native$String.slice;
   var repeat = $Native$String.repeat;
   var join = $Native$String.join;
   var split = $Native$String.split;
   var foldr = $Native$String.foldr;
   var foldl = $Native$String.foldl;
   var reverse = $Native$String.reverse;
   var filter = $Native$String.filter;
   var map = $Native$String.map;
   var length = $Native$String.length;
   var concat = $Native$String.concat;
   var append = $Native$String.append;
   var uncons = $Native$String.uncons;
   var cons = $Native$String.cons;
   var isEmpty = $Native$String.isEmpty;
   _elm.String.values = {_op: _op
                        ,isEmpty: isEmpty
                        ,cons: cons
                        ,uncons: uncons
                        ,append: append
                        ,concat: concat
                        ,length: length
                        ,map: map
                        ,filter: filter
                        ,reverse: reverse
                        ,foldl: foldl
                        ,foldr: foldr
                        ,split: split
                        ,join: join
                        ,repeat: repeat
                        ,slice: slice
                        ,left: left
                        ,right: right
                        ,dropLeft: dropLeft
                        ,dropRight: dropRight
                        ,pad: pad
                        ,padLeft: padLeft
                        ,padRight: padRight
                        ,trim: trim
                        ,trimLeft: trimLeft
                        ,trimRight: trimRight
                        ,words: words
                        ,lines: lines
                        ,toUpper: toUpper
                        ,toLower: toLower
                        ,any: any
                        ,all: all
                        ,contains: contains
                        ,startsWith: startsWith
                        ,endsWith: endsWith
                        ,indexes: indexes
                        ,indices: indices
                        ,show: show
                        ,toInt: toInt
                        ,toFloat: toFloat
                        ,toList: toList
                        ,fromList: fromList};
   return _elm.String.values;
};Elm.Text = Elm.Text || {};
Elm.Text.make = function (_elm) {
   "use strict";
   _elm.Text = _elm.Text || {};
   if (_elm.Text.values)
   return _elm.Text.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Text",
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Show = Elm.Native.Show.make(_elm),
   $Native$Text = Elm.Native.Text.make(_elm);
   var markdown = $Native$Text.markdown;
   var justified = $Native$Text.justified;
   var centered = $Native$Text.centered;
   var rightAligned = $Native$Text.rightAligned;
   var leftAligned = $Native$Text.leftAligned;
   var line = $Native$Text.line;
   var italic = $Native$Text.italic;
   var bold = $Native$Text.bold;
   var color = $Native$Text.color;
   var height = $Native$Text.height;
   var link = $Native$Text.link;
   var monospace = $Native$Text.monospace;
   var typeface = $Native$Text.typeface;
   var style = $Native$Text.style;
   var toText = $Native$Text.toText;
   var plainText = function (str) {
      return leftAligned(toText(str));
   };
   var asText = function (value) {
      return leftAligned(monospace(toText($Native$Show.show(value))));
   };
   var defaultStyle = {_: {}
                      ,bold: false
                      ,color: $Color.black
                      ,height: $Maybe.Nothing
                      ,italic: false
                      ,line: $Maybe.Nothing
                      ,typeface: _L.fromArray([])};
   var Style = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,bold: d
             ,color: c
             ,height: b
             ,italic: e
             ,line: f
             ,typeface: a};
   });
   var Through = {ctor: "Through"};
   var Over = {ctor: "Over"};
   var Under = {ctor: "Under"};
   var Text = {ctor: "Text"};
   _elm.Text.values = {_op: _op
                      ,Text: Text
                      ,Under: Under
                      ,Over: Over
                      ,Through: Through
                      ,Style: Style
                      ,defaultStyle: defaultStyle
                      ,toText: toText
                      ,style: style
                      ,typeface: typeface
                      ,monospace: monospace
                      ,link: link
                      ,height: height
                      ,color: color
                      ,bold: bold
                      ,italic: italic
                      ,line: line
                      ,leftAligned: leftAligned
                      ,rightAligned: rightAligned
                      ,centered: centered
                      ,justified: justified
                      ,plainText: plainText
                      ,markdown: markdown
                      ,asText: asText};
   return _elm.Text.values;
};Elm.Time = Elm.Time || {};
Elm.Time.make = function (_elm) {
   "use strict";
   _elm.Time = _elm.Time || {};
   if (_elm.Time.values)
   return _elm.Time.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Time",
   $Basics = Elm.Basics.make(_elm),
   $Native$Time = Elm.Native.Time.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var delay = $Native$Time.delay;
   var timestamp = $Native$Time.timestamp;
   var since = $Native$Time.since;
   var every = $Native$Time.every;
   var fpsWhen = $Native$Time.fpsWhen;
   var fps = $Native$Time.fps;
   var inMilliseconds = function (t) {
      return t;
   };
   var millisecond = 1;
   var second = 1000 * millisecond;
   var minute = 60 * second;
   var hour = 60 * minute;
   var inHours = function (t) {
      return t / hour;
   };
   var inMinutes = function (t) {
      return t / minute;
   };
   var inSeconds = function (t) {
      return t / second;
   };
   _elm.Time.values = {_op: _op
                      ,millisecond: millisecond
                      ,second: second
                      ,minute: minute
                      ,hour: hour
                      ,inMilliseconds: inMilliseconds
                      ,inSeconds: inSeconds
                      ,inMinutes: inMinutes
                      ,inHours: inHours
                      ,fps: fps
                      ,fpsWhen: fpsWhen
                      ,every: every
                      ,since: since
                      ,timestamp: timestamp
                      ,delay: delay};
   return _elm.Time.values;
};Elm.Touch = Elm.Touch || {};
Elm.Touch.make = function (_elm) {
   "use strict";
   _elm.Touch = _elm.Touch || {};
   if (_elm.Touch.values)
   return _elm.Touch.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Touch",
   $Native$Touch = Elm.Native.Touch.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Time = Elm.Time.make(_elm);
   var taps = $Native$Touch.taps;
   var touches = $Native$Touch.touches;
   var Touch = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,id: c
             ,t0: f
             ,x: a
             ,x0: d
             ,y: b
             ,y0: e};
   });
   _elm.Touch.values = {_op: _op
                       ,Touch: Touch
                       ,touches: touches
                       ,taps: taps};
   return _elm.Touch.values;
};Elm.Trampoline = Elm.Trampoline || {};
Elm.Trampoline.make = function (_elm) {
   "use strict";
   _elm.Trampoline = _elm.Trampoline || {};
   if (_elm.Trampoline.values)
   return _elm.Trampoline.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Trampoline",
   $Native$Trampoline = Elm.Native.Trampoline.make(_elm);
   var trampoline = $Native$Trampoline.trampoline;
   var Continue = function (a) {
      return {ctor: "Continue"
             ,_0: a};
   };
   var Done = function (a) {
      return {ctor: "Done",_0: a};
   };
   _elm.Trampoline.values = {_op: _op
                            ,Done: Done
                            ,Continue: Continue
                            ,trampoline: trampoline};
   return _elm.Trampoline.values;
};Elm.Transform2D = Elm.Transform2D || {};
Elm.Transform2D.make = function (_elm) {
   "use strict";
   _elm.Transform2D = _elm.Transform2D || {};
   if (_elm.Transform2D.values)
   return _elm.Transform2D.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Transform2D",
   $Native$Transform2D = Elm.Native.Transform2D.make(_elm);
   var multiply = $Native$Transform2D.multiply;
   var rotation = $Native$Transform2D.rotation;
   var matrix = $Native$Transform2D.matrix;
   var translation = F2(function (x,
   y) {
      return A6(matrix,
      1,
      0,
      0,
      1,
      x,
      y);
   });
   var scale = function (s) {
      return A6(matrix,
      s,
      0,
      0,
      s,
      0,
      0);
   };
   var scaleX = function (x) {
      return A6(matrix,
      x,
      0,
      0,
      1,
      0,
      0);
   };
   var scaleY = function (y) {
      return A6(matrix,
      1,
      0,
      0,
      y,
      0,
      0);
   };
   var identity = $Native$Transform2D.identity;
   var Transform2D = {ctor: "Transform2D"};
   _elm.Transform2D.values = {_op: _op
                             ,identity: identity
                             ,matrix: matrix
                             ,multiply: multiply
                             ,rotation: rotation
                             ,translation: translation
                             ,scale: scale
                             ,scaleX: scaleX
                             ,scaleY: scaleY};
   return _elm.Transform2D.values;
};Elm.WebSocket = Elm.WebSocket || {};
Elm.WebSocket.make = function (_elm) {
   "use strict";
   _elm.WebSocket = _elm.WebSocket || {};
   if (_elm.WebSocket.values)
   return _elm.WebSocket.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "WebSocket",
   $Native$WebSocket = Elm.Native.WebSocket.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var connect = $Native$WebSocket.connect;
   _elm.WebSocket.values = {_op: _op
                           ,connect: connect};
   return _elm.WebSocket.values;
};Elm.Window = Elm.Window || {};
Elm.Window.make = function (_elm) {
   "use strict";
   _elm.Window = _elm.Window || {};
   if (_elm.Window.values)
   return _elm.Window.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Window",
   $Native$Window = Elm.Native.Window.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var height = $Native$Window.height;
   var width = $Native$Window.width;
   var dimensions = $Native$Window.dimensions;
   _elm.Window.values = {_op: _op
                        ,dimensions: dimensions
                        ,width: width
                        ,height: height};
   return _elm.Window.values;
};Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Collage = Elm.Graphics.Collage || {};
Elm.Graphics.Collage.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Collage = _elm.Graphics.Collage || {};
   if (_elm.Graphics.Collage.values)
   return _elm.Graphics.Collage.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Graphics.Collage",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Either = Elm.Either.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Native$Graphics$Collage = Elm.Native.Graphics.Collage.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm);
   var ngon = F2(function (n,r) {
      return function () {
         var m = $Basics.toFloat(n);
         var t = 2 * $Basics.pi / m;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: r * $Basics.cos(t * i)
                   ,_1: r * $Basics.sin(t * i)};
         };
         return A2($List.map,
         f,
         _L.range(0,m - 1));
      }();
   });
   var oval = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         var n = 50;
         var t = 2 * $Basics.pi / n;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: hw * $Basics.cos(t * i)
                   ,_1: hh * $Basics.sin(t * i)};
         };
         return A2($List.map,
         f,
         _L.range(0,n - 1));
      }();
   });
   var circle = function (r) {
      return A2(oval,2 * r,2 * r);
   };
   var rect = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         return _L.fromArray([{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: 0 - hh}
                             ,{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: hh}
                             ,{ctor: "_Tuple2",_0: hw,_1: hh}
                             ,{ctor: "_Tuple2"
                              ,_0: hw
                              ,_1: 0 - hh}]);
      }();
   });
   var square = function (n) {
      return A2(rect,n,n);
   };
   var polygon = function (points) {
      return points;
   };
   var segment = F2(function (p1,
   p2) {
      return _L.fromArray([p1,p2]);
   });
   var path = function (ps) {
      return ps;
   };
   var collage = $Native$Graphics$Collage.collage;
   var alpha = F2(function (a,f) {
      return _U.replace([["alpha"
                         ,a]],
      f);
   });
   var rotate = F2(function (t,f) {
      return _U.replace([["theta"
                         ,f.theta + t]],
      f);
   });
   var scale = F2(function (s,f) {
      return _U.replace([["scale"
                         ,f.scale * s]],
      f);
   });
   var moveY = F2(function (y,f) {
      return _U.replace([["y"
                         ,f.y + y]],
      f);
   });
   var moveX = F2(function (x,f) {
      return _U.replace([["x"
                         ,f.x + x]],
      f);
   });
   var move = F2(function (_v0,f) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return _U.replace([["x"
                               ,f.x + _v0._0]
                              ,["y",f.y + _v0._1]],
              f);}
         _E.Case($moduleName,
         "on line 170, column 20 to 48");
      }();
   });
   var form = function (f) {
      return {_: {}
             ,alpha: 1
             ,form: f
             ,scale: 1
             ,theta: 0
             ,x: 0
             ,y: 0};
   };
   var FGroup = F2(function (a,b) {
      return {ctor: "FGroup"
             ,_0: a
             ,_1: b};
   });
   var group = function (fs) {
      return form(A2(FGroup,
      $Transform2D.identity,
      fs));
   };
   var groupTransform = F2(function (matrix,
   fs) {
      return form(A2(FGroup,
      matrix,
      fs));
   });
   var FElement = function (a) {
      return {ctor: "FElement"
             ,_0: a};
   };
   var toForm = function (e) {
      return form(FElement(e));
   };
   var FImage = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "FImage"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var sprite = F4(function (w,
   h,
   pos,
   src) {
      return form(A4(FImage,
      w,
      h,
      pos,
      src));
   });
   var FShape = F2(function (a,b) {
      return {ctor: "FShape"
             ,_0: a
             ,_1: b};
   });
   var fill = F2(function (style,
   shape) {
      return form(A2(FShape,
      $Either.Right(style),
      shape));
   });
   var outlined = F2(function (style,
   shape) {
      return form(A2(FShape,
      $Either.Left(style),
      shape));
   });
   var FPath = F2(function (a,b) {
      return {ctor: "FPath"
             ,_0: a
             ,_1: b};
   });
   var traced = F2(function (style,
   path) {
      return form(A2(FPath,
      style,
      path));
   });
   var LineStyle = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,cap: c
             ,color: a
             ,dashOffset: f
             ,dashing: e
             ,join: d
             ,width: b};
   });
   var Clipped = {ctor: "Clipped"};
   var Sharp = function (a) {
      return {ctor: "Sharp",_0: a};
   };
   var Smooth = {ctor: "Smooth"};
   var Padded = {ctor: "Padded"};
   var Round = {ctor: "Round"};
   var Flat = {ctor: "Flat"};
   var defaultLine = {_: {}
                     ,cap: Flat
                     ,color: $Color.black
                     ,dashOffset: 0
                     ,dashing: _L.fromArray([])
                     ,join: Sharp(10)
                     ,width: 1};
   var solid = function (clr) {
      return _U.replace([["color"
                         ,clr]],
      defaultLine);
   };
   var dashed = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([8,4])]],
      defaultLine);
   };
   var dotted = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([3,3])]],
      defaultLine);
   };
   var Grad = function (a) {
      return {ctor: "Grad",_0: a};
   };
   var gradient = F2(function (grad,
   shape) {
      return A2(fill,
      Grad(grad),
      shape);
   });
   var Texture = function (a) {
      return {ctor: "Texture"
             ,_0: a};
   };
   var textured = F2(function (src,
   shape) {
      return A2(fill,
      Texture(src),
      shape);
   });
   var Solid = function (a) {
      return {ctor: "Solid",_0: a};
   };
   var filled = F2(function (color,
   shape) {
      return A2(fill,
      Solid(color),
      shape);
   });
   var Form = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,alpha: e
             ,form: f
             ,scale: b
             ,theta: a
             ,x: c
             ,y: d};
   });
   _elm.Graphics.Collage.values = {_op: _op
                                  ,Form: Form
                                  ,Solid: Solid
                                  ,Texture: Texture
                                  ,Grad: Grad
                                  ,Flat: Flat
                                  ,Round: Round
                                  ,Padded: Padded
                                  ,Smooth: Smooth
                                  ,Sharp: Sharp
                                  ,Clipped: Clipped
                                  ,LineStyle: LineStyle
                                  ,defaultLine: defaultLine
                                  ,solid: solid
                                  ,dashed: dashed
                                  ,dotted: dotted
                                  ,FPath: FPath
                                  ,FShape: FShape
                                  ,FImage: FImage
                                  ,FElement: FElement
                                  ,FGroup: FGroup
                                  ,form: form
                                  ,fill: fill
                                  ,filled: filled
                                  ,textured: textured
                                  ,gradient: gradient
                                  ,outlined: outlined
                                  ,traced: traced
                                  ,sprite: sprite
                                  ,toForm: toForm
                                  ,group: group
                                  ,groupTransform: groupTransform
                                  ,move: move
                                  ,moveX: moveX
                                  ,moveY: moveY
                                  ,scale: scale
                                  ,rotate: rotate
                                  ,alpha: alpha
                                  ,collage: collage
                                  ,path: path
                                  ,segment: segment
                                  ,polygon: polygon
                                  ,rect: rect
                                  ,square: square
                                  ,oval: oval
                                  ,circle: circle
                                  ,ngon: ngon};
   return _elm.Graphics.Collage.values;
};Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Element = Elm.Graphics.Element || {};
Elm.Graphics.Element.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Element = _elm.Graphics.Element || {};
   if (_elm.Graphics.Element.values)
   return _elm.Graphics.Element.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Graphics.Element",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Utils = Elm.Native.Utils.make(_elm);
   var DOut = {ctor: "DOut"};
   var outward = DOut;
   var DIn = {ctor: "DIn"};
   var inward = DIn;
   var DRight = {ctor: "DRight"};
   var right = DRight;
   var DLeft = {ctor: "DLeft"};
   var left = DLeft;
   var DDown = {ctor: "DDown"};
   var down = DDown;
   var DUp = {ctor: "DUp"};
   var up = DUp;
   var Position = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,horizontal: a
             ,vertical: b
             ,x: c
             ,y: d};
   });
   var Relative = function (a) {
      return {ctor: "Relative"
             ,_0: a};
   };
   var relative = Relative;
   var Absolute = function (a) {
      return {ctor: "Absolute"
             ,_0: a};
   };
   var absolute = Absolute;
   var N = {ctor: "N"};
   var bottomLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var Z = {ctor: "Z"};
   var middle = {_: {}
                ,horizontal: Z
                ,vertical: Z
                ,x: Relative(0.5)
                ,y: Relative(0.5)};
   var midLeft = _U.replace([["horizontal"
                             ,N]
                            ,["x",Absolute(0)]],
   middle);
   var middleAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midBottomAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var P = {ctor: "P"};
   var topLeft = {_: {}
                 ,horizontal: N
                 ,vertical: P
                 ,x: Absolute(0)
                 ,y: Absolute(0)};
   var bottomLeft = _U.replace([["vertical"
                                ,N]],
   topLeft);
   var topRight = _U.replace([["horizontal"
                              ,P]],
   topLeft);
   var bottomRight = _U.replace([["horizontal"
                                 ,P]],
   bottomLeft);
   var midRight = _U.replace([["horizontal"
                              ,P]],
   midLeft);
   var midTop = _U.replace([["vertical"
                            ,P]
                           ,["y",Absolute(0)]],
   middle);
   var midBottom = _U.replace([["vertical"
                               ,N]],
   midTop);
   var topLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var topRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var bottomRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var midRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midTopAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var Tiled = {ctor: "Tiled"};
   var Cropped = function (a) {
      return {ctor: "Cropped"
             ,_0: a};
   };
   var Fitted = {ctor: "Fitted"};
   var Plain = {ctor: "Plain"};
   var Custom = {ctor: "Custom"};
   var RawHtml = {ctor: "RawHtml"};
   var Spacer = {ctor: "Spacer"};
   var Flow = F2(function (a,b) {
      return {ctor: "Flow"
             ,_0: a
             ,_1: b};
   });
   var Container = F2(function (a,
   b) {
      return {ctor: "Container"
             ,_0: a
             ,_1: b};
   });
   var Image = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "Image"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var link = F2(function (href,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["href"
                                    ,href]],
                p)};
      }();
   });
   var tag = F2(function (name,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["tag"
                                    ,name]],
                p)};
      }();
   });
   var color = F2(function (c,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["color"
                                    ,$Maybe.Just(c)]],
                p)};
      }();
   });
   var opacity = F2(function (o,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["opacity"
                                    ,o]],
                p)};
      }();
   });
   var height = F2(function (nh,
   e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v0 = e.element;
            switch (_v0.ctor)
            {case "Image":
               return _U.replace([["width"
                                  ,$Basics.round($Basics.toFloat(_v0._1) / $Basics.toFloat(_v0._2) * $Basics.toFloat(nh))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["height"
                                    ,nh]],
                p)};
      }();
   });
   var width = F2(function (nw,e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v5 = e.element;
            switch (_v5.ctor)
            {case "Image":
               return _U.replace([["height"
                                  ,$Basics.round($Basics.toFloat(_v5._2) / $Basics.toFloat(_v5._1) * $Basics.toFloat(nw))]],
                 p);
               case "RawHtml":
               return _U.replace([["height"
                                  ,$Basics.snd(A2($Native$Utils.htmlHeight,
                                  nw,
                                  e.element))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["width"
                                    ,nw]],
                props)};
      }();
   });
   var size = F3(function (w,h,e) {
      return A2(height,
      h,
      A2(width,w,e));
   });
   var sizeOf = function (e) {
      return {ctor: "_Tuple2"
             ,_0: e.props.width
             ,_1: e.props.height};
   };
   var heightOf = function (e) {
      return e.props.height;
   };
   var widthOf = function (e) {
      return e.props.width;
   };
   var Element = F2(function (a,
   b) {
      return {_: {}
             ,element: b
             ,props: a};
   });
   var Properties = F9(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h,
   i) {
      return {_: {}
             ,click: i
             ,color: e
             ,height: c
             ,hover: h
             ,href: f
             ,id: a
             ,opacity: d
             ,tag: g
             ,width: b};
   });
   var newElement = F3(function (w,
   h,
   e) {
      return {_: {}
             ,element: e
             ,props: A9(Properties,
             $Native$Utils.guid({ctor: "_Tuple0"}),
             w,
             h,
             1,
             $Maybe.Nothing,
             "",
             "",
             {ctor: "_Tuple0"},
             {ctor: "_Tuple0"})};
   });
   var image = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Plain,w,h,src));
   });
   var fittedImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Fitted,w,h,src));
   });
   var croppedImage = F4(function (pos,
   w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Cropped(pos),w,h,src));
   });
   var tiledImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Tiled,w,h,src));
   });
   var container = F4(function (w,
   h,
   pos,
   e) {
      return A3(newElement,
      w,
      h,
      A2(Container,pos,e));
   });
   var spacer = F2(function (w,h) {
      return A3(newElement,
      w,
      h,
      Spacer);
   });
   var empty = A2(spacer,0,0);
   var flow = F2(function (dir,
   es) {
      return function () {
         var newFlow = F2(function (w,
         h) {
            return A3(newElement,
            w,
            h,
            A2(Flow,dir,es));
         });
         var hs = A2($List.map,
         heightOf,
         es);
         var ws = A2($List.map,
         widthOf,
         es);
         return _U.eq(es,
         _L.fromArray([])) ? empty : function () {
            switch (dir.ctor)
            {case "DDown":
               return A2(newFlow,
                 $List.maximum(ws),
                 $List.sum(hs));
               case "DIn": return A2(newFlow,
                 $List.maximum(ws),
                 $List.maximum(hs));
               case "DLeft": return A2(newFlow,
                 $List.sum(ws),
                 $List.maximum(hs));
               case "DOut": return A2(newFlow,
                 $List.maximum(ws),
                 $List.maximum(hs));
               case "DRight":
               return A2(newFlow,
                 $List.sum(ws),
                 $List.maximum(hs));
               case "DUp": return A2(newFlow,
                 $List.maximum(ws),
                 $List.sum(hs));}
            _E.Case($moduleName,
            "between lines 206 and 216");
         }();
      }();
   });
   var above = F2(function (hi,
   lo) {
      return A3(newElement,
      A2($Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var below = F2(function (lo,
   hi) {
      return A3(newElement,
      A2($Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var beside = F2(function (lft,
   rht) {
      return A3(newElement,
      widthOf(lft) + widthOf(rht),
      A2($Basics.max,
      heightOf(lft),
      heightOf(rht)),
      A2(Flow,
      right,
      _L.fromArray([lft,rht])));
   });
   var layers = function (es) {
      return function () {
         var hs = A2($List.map,
         heightOf,
         es);
         var ws = A2($List.map,
         widthOf,
         es);
         return A3(newElement,
         $List.maximum(ws),
         $List.maximum(hs),
         A2(Flow,DOut,es));
      }();
   };
   _elm.Graphics.Element.values = {_op: _op
                                  ,Properties: Properties
                                  ,Element: Element
                                  ,empty: empty
                                  ,widthOf: widthOf
                                  ,heightOf: heightOf
                                  ,sizeOf: sizeOf
                                  ,width: width
                                  ,height: height
                                  ,size: size
                                  ,opacity: opacity
                                  ,color: color
                                  ,tag: tag
                                  ,link: link
                                  ,newElement: newElement
                                  ,Image: Image
                                  ,Container: Container
                                  ,Flow: Flow
                                  ,Spacer: Spacer
                                  ,RawHtml: RawHtml
                                  ,Custom: Custom
                                  ,Plain: Plain
                                  ,Fitted: Fitted
                                  ,Cropped: Cropped
                                  ,Tiled: Tiled
                                  ,image: image
                                  ,fittedImage: fittedImage
                                  ,croppedImage: croppedImage
                                  ,tiledImage: tiledImage
                                  ,P: P
                                  ,Z: Z
                                  ,N: N
                                  ,Absolute: Absolute
                                  ,Relative: Relative
                                  ,Position: Position
                                  ,container: container
                                  ,spacer: spacer
                                  ,DUp: DUp
                                  ,DDown: DDown
                                  ,DLeft: DLeft
                                  ,DRight: DRight
                                  ,DIn: DIn
                                  ,DOut: DOut
                                  ,flow: flow
                                  ,above: above
                                  ,below: below
                                  ,beside: beside
                                  ,layers: layers
                                  ,absolute: absolute
                                  ,relative: relative
                                  ,middle: middle
                                  ,topLeft: topLeft
                                  ,topRight: topRight
                                  ,bottomLeft: bottomLeft
                                  ,bottomRight: bottomRight
                                  ,midLeft: midLeft
                                  ,midRight: midRight
                                  ,midTop: midTop
                                  ,midBottom: midBottom
                                  ,middleAt: middleAt
                                  ,topLeftAt: topLeftAt
                                  ,topRightAt: topRightAt
                                  ,bottomLeftAt: bottomLeftAt
                                  ,bottomRightAt: bottomRightAt
                                  ,midLeftAt: midLeftAt
                                  ,midRightAt: midRightAt
                                  ,midTopAt: midTopAt
                                  ,midBottomAt: midBottomAt
                                  ,up: up
                                  ,down: down
                                  ,left: left
                                  ,right: right
                                  ,inward: inward
                                  ,outward: outward};
   return _elm.Graphics.Element.values;
};Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Input = Elm.Graphics.Input || {};
Elm.Graphics.Input.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Input = _elm.Graphics.Input || {};
   if (_elm.Graphics.Input.values)
   return _elm.Graphics.Input.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Graphics.Input",
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Native$Graphics$Input = Elm.Native.Graphics.Input.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var clickable = $Native$Graphics$Input.clickable;
   var hoverable = $Native$Graphics$Input.hoverable;
   var dropDown = $Native$Graphics$Input.dropDown;
   var checkbox = $Native$Graphics$Input.checkbox;
   var customButton = $Native$Graphics$Input.customButton;
   var button = $Native$Graphics$Input.button;
   var input = $Native$Graphics$Input.input;
   var Handle = {ctor: "Handle"};
   var Input = F2(function (a,b) {
      return {_: {}
             ,handle: b
             ,signal: a};
   });
   _elm.Graphics.Input.values = {_op: _op
                                ,Input: Input
                                ,Handle: Handle
                                ,input: input
                                ,button: button
                                ,customButton: customButton
                                ,checkbox: checkbox
                                ,dropDown: dropDown
                                ,hoverable: hoverable
                                ,clickable: clickable};
   return _elm.Graphics.Input.values;
};Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Input = Elm.Graphics.Input || {};
Elm.Graphics.Input.Field = Elm.Graphics.Input.Field || {};
Elm.Graphics.Input.Field.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Input = _elm.Graphics.Input || {};
   _elm.Graphics.Input.Field = _elm.Graphics.Input.Field || {};
   if (_elm.Graphics.Input.Field.values)
   return _elm.Graphics.Input.Field.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Graphics.Input.Field",
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Graphics$Input = Elm.Graphics.Input.make(_elm),
   $Native$Graphics$Input = Elm.Native.Graphics.Input.make(_elm),
   $Text = Elm.Text.make(_elm);
   var email = $Native$Graphics$Input.email;
   var password = $Native$Graphics$Input.password;
   var field = $Native$Graphics$Input.field;
   var Backward = {ctor: "Backward"};
   var Forward = {ctor: "Forward"};
   var Selection = F3(function (a,
   b,
   c) {
      return {_: {}
             ,direction: c
             ,end: b
             ,start: a};
   });
   var Content = F2(function (a,
   b) {
      return {_: {}
             ,selection: b
             ,string: a};
   });
   var noContent = A2(Content,
   "",
   A3(Selection,0,0,Forward));
   var Style = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,highlight: c
             ,outline: b
             ,padding: a
             ,style: d};
   });
   var Highlight = F2(function (a,
   b) {
      return {_: {}
             ,color: a
             ,width: b};
   });
   var noHighlight = A2(Highlight,
   $Color.blue,
   0);
   var Outline = F3(function (a,
   b,
   c) {
      return {_: {}
             ,color: a
             ,radius: c
             ,width: b};
   });
   var Dimensions = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,bottom: d
             ,left: a
             ,right: b
             ,top: c};
   });
   var uniformly = function (n) {
      return A4(Dimensions,
      n,
      n,
      n,
      n);
   };
   var noOutline = A3(Outline,
   $Color.grey,
   uniformly(0),
   0);
   var defaultStyle = {_: {}
                      ,highlight: A2(Highlight,
                      $Color.blue,
                      1)
                      ,outline: A3(Outline,
                      $Color.grey,
                      uniformly(1),
                      2)
                      ,padding: uniformly(4)
                      ,style: $Text.defaultStyle};
   _elm.Graphics.Input.Field.values = {_op: _op
                                      ,uniformly: uniformly
                                      ,Dimensions: Dimensions
                                      ,Outline: Outline
                                      ,noOutline: noOutline
                                      ,Highlight: Highlight
                                      ,noHighlight: noHighlight
                                      ,Style: Style
                                      ,defaultStyle: defaultStyle
                                      ,Content: Content
                                      ,Selection: Selection
                                      ,Forward: Forward
                                      ,Backward: Backward
                                      ,noContent: noContent
                                      ,field: field
                                      ,password: password
                                      ,email: email};
   return _elm.Graphics.Input.Field.values;
};Elm.JavaScript = Elm.JavaScript || {};
Elm.JavaScript.Experimental = Elm.JavaScript.Experimental || {};
Elm.JavaScript.Experimental.make = function (_elm) {
   "use strict";
   _elm.JavaScript = _elm.JavaScript || {};
   _elm.JavaScript.Experimental = _elm.JavaScript.Experimental || {};
   if (_elm.JavaScript.Experimental.values)
   return _elm.JavaScript.Experimental.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "JavaScript.Experimental",
   $Json = Elm.Json.make(_elm),
   $Native$JavaScript = Elm.Native.JavaScript.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm);
   var fromJson = $Native$Json.toJS;
   var toJson = $Native$Json.fromJS;
   var fromRecord = $Native$JavaScript.fromRecord;
   var toRecord = $Native$JavaScript.toRecord;
   var RawObject = {ctor: "RawObject"};
   _elm.JavaScript.Experimental.values = {_op: _op
                                         ,RawObject: RawObject
                                         ,toRecord: toRecord
                                         ,fromRecord: fromRecord
                                         ,toJson: toJson
                                         ,fromJson: fromJson};
   return _elm.JavaScript.Experimental.values;
};(function() {
'use strict';

if (typeof window != 'undefined' && !window.location.origin) {
  window.location.origin =
      window.location.protocol + "//" +
      window.location.hostname +
      (window.location.port ? (':' + window.location.port) : '');
}

Elm.fullscreenDebugHooks = function(module, debuggerHistory /* =undefined */) {
  var exposedDebugger = {};
  function debuggerAttach(module, debuggerHistory) {
    return {
      make: function(runtime) {
        var wrappedModule = debugModule(module, runtime);
        exposedDebugger = debuggerInit(wrappedModule, runtime, debuggerHistory);
        return wrappedModule.debuggedModule;
      }
    }
  }
  var mainHandle = Elm.fullscreen(debuggerAttach(module, debuggerHistory));
  mainHandle.debugger = exposedDebugger;
  return mainHandle;
};

var EVENTS_PER_SAVE = 100;

function debugModule(module, runtime) {
  var programPaused = false;
  var recordedEvents = [];
  var asyncCallbacks = [];
  var snapshots = [];
  var watchTracker = Elm.Native.Debug.make(runtime).watchTracker;
  var pauseTime = 0;
  var eventsUntilSnapshot = EVENTS_PER_SAVE;
  runtime.debuggerStatus = runtime.debuggerStatus || {};
  runtime.debuggerStatus.eventCounter = runtime.debuggerStatus.eventCounter || 0;

  // runtime is the prototype of wrappedRuntime
  // so we can access all runtime properties too
  var wrappedRuntime = Object.create(runtime);
  wrappedRuntime.notify = notifyWrapper;
  wrappedRuntime.setTimeout = setTimeoutWrapper;

  // make a copy of the wrappedRuntime
  var assignedPropTracker = Object.create(wrappedRuntime);
  var debuggedModule = module.make(assignedPropTracker);

  // make sure the signal graph is actually a signal & extract the visual model
  if ( !('recv' in debuggedModule.main) ) {
    debuggedModule.main = Elm.Signal.make(runtime).constant(debuggedModule.main);
  }

  // The main module stores imported modules onto the runtime.
  // To ensure only one instance of each module is created,
  // we assign them back on the original runtime object.
  Object.keys(assignedPropTracker).forEach(function(key) {
    runtime[key] = assignedPropTracker[key];
  });

  var signalGraphNodes = flattenSignalGraph(wrappedRuntime.inputs);
  var tracePath = tracePathInit(runtime, debuggedModule.main);

  snapshots.push(snapshotSignalGraph(signalGraphNodes));

  function notifyWrapper(id, v) {
    var timestep = runtime.timer.now();

    if (programPaused) {
      // ignore async events generated while playing back
      // or user events while program is paused
      return false;
    }
    else {
      recordEvent(id, v, timestep);
      var changed = runtime.notify(id, v, timestep);
      snapshotOnCheckpoint();
      if (parent.window) {
        parent.window.postMessage("elmNotify", window.location.origin);
      }
      return changed;
    }
  };

  function setTimeoutWrapper(func, delayMs) {
    if (programPaused) {
      // Don't push timers and such to the callback stack while we're paused.
      // It causes too many callbacks to be fired during unpausing.
      return 0;
    }
    var cbObj = { func:func, delayMs:delayMs, timerId:0, executed:false };
    var timerId = setTimeout(function() {
        cbObj.executed = true;
        func();
      }, delayMs);
    cbObj.timerId = timerId;
    asyncCallbacks.push(cbObj);
    return timerId;
  }

  function recordEvent(id, v, timestep) {
    watchTracker.pushFrame();
    recordedEvents.push({ id:id, value:v, timestep:timestep });
    runtime.debuggerStatus.eventCounter += 1;
  }

  function clearAsyncCallbacks() {
    asyncCallbacks.forEach(function(timer) {
      if (!timer.executed) {
        clearTimeout(timer.timerId);
      }
    });
  }

  function clearRecordedEvents() {
    recordedEvents = [];
    runtime.debuggerStatus.eventCounter = 0;
  }

  function getRecordedEventsLength() {
    return recordedEvents.length;
  }

  function getRecordedEventAt(i) {
    return recordedEvents[i];
  }

  function copyRecordedEvents() {
    return recordedEvents.slice();
  }

  function loadRecordedEvents(events) {
    recordedEvents = events.slice();
  }

  function clearSnapshots() {
    snapshots = [snapshotSignalGraph(signalGraphNodes)];
  }

  function getSnapshotAt(i) {
    var snapshotEvent = Math.floor(i / EVENTS_PER_SAVE);
    assert(snapshotEvent < snapshots.length && snapshotEvent >= 0,
           "Out of bounds index: " + snapshotEvent);
    return snapshots[snapshotEvent];
  }

  function snapshotOnCheckpoint() {
    if (eventsUntilSnapshot === 1) {
      snapshots.push(snapshotSignalGraph(signalGraphNodes));
      eventsUntilSnapshot = EVENTS_PER_SAVE;
    } else {
      eventsUntilSnapshot -= 1;
    }
  }

  function setPaused() {
    programPaused = true;
    clearAsyncCallbacks();
    pauseTime = Date.now();
    tracePath.stopRecording();
    preventInputEvents();
  }

  function setContinue(position) {
    var pauseDelay = Date.now() - pauseTime;
    runtime.timer.addDelay(pauseDelay);
    programPaused = false;

    // we need to dump the events that are ahead of where we're continuing.
    var lastSnapshotPosition = Math.floor(position / EVENTS_PER_SAVE);
    eventsUntilSnapshot = EVENTS_PER_SAVE - (position % EVENTS_PER_SAVE);
    snapshots = snapshots.slice(0, lastSnapshotPosition + 1);

    if (position < getRecordedEventsLength()) {
      var lastEventTime = recordedEvents[position].timestep;
      var scrubTime = runtime.timer.now() - lastEventTime;
      runtime.timer.addDelay(scrubTime);
    }

    recordedEvents = recordedEvents.slice(0, position);
    tracePath.clearTracesAfter(position);
    runtime.debuggerStatus.eventCounter = position;
    executeCallbacks(asyncCallbacks);
    permitInputEvents();

    tracePath.startRecording();
  }

  function getPaused() {
    return programPaused;
  }

  function preventInputEvents(){
    var events =
      [ "click", "mousemove", "mouseup", "mousedown", "mouseclick"
      , "keydown", "keypress", "keyup", "touchstart", "touchend"
      , "touchcancel", "touchleave", "touchmove", "pointermove"
      , "pointerdown", "pointerup", "pointerover", "pointerout"
      , "pointerenter", "pointerleave", "pointercancel"
      ];

    var ignore = function(e) {
      var evt = e ? e : window.event;
      if (evt.stopPropagation) {
        evt.stopPropagation();
      }
      if (evt.cancelBubble !== null) {
        evt.cancelBubble = true;
      }
      if (evt.preventDefault) {
        evt.preventDefault();
      }
      return false;
    };

    var ignoringDiv = document.getElementById("elmEventIgnorer");
    if (!ignoringDiv) {
      ignoringDiv = document.createElement("div");
      ignoringDiv.id = "elmEventIgnorer";
      ignoringDiv.style.position = "absolute";
      ignoringDiv.style.top = "0px";
      ignoringDiv.style.left = "0px";
      ignoringDiv.style.width = "100%";
      ignoringDiv.style.height = "100%";

      for (var i = events.length; i-- ;) {
        ignoringDiv.addEventListener(events[i], ignore, true);
      }
      runtime.node.appendChild(ignoringDiv);
    }
  }

  function permitInputEvents(){
    var ignoringDiv = document.getElementById("elmEventIgnorer");
    ignoringDiv.parentNode.removeChild(ignoringDiv);
  }

  return {
    debuggedModule: debuggedModule,
    signalGraphNodes: signalGraphNodes,
    initialSnapshot: snapshotSignalGraph(signalGraphNodes),
    initialAsyncCallbacks: asyncCallbacks.slice(),
    // API functions
    clearAsyncCallbacks: clearAsyncCallbacks,
    clearRecordedEvents: clearRecordedEvents,
    getRecordedEventsLength: getRecordedEventsLength,
    getRecordedEventAt: getRecordedEventAt,
    copyRecordedEvents: copyRecordedEvents,
    loadRecordedEvents: loadRecordedEvents,
    clearSnapshots: clearSnapshots,
    getSnapshotAt: getSnapshotAt,
    snapshotOnCheckpoint: snapshotOnCheckpoint,
    getPaused: getPaused,
    setPaused: setPaused,
    setContinue: setContinue,
    tracePath: tracePath,
    watchTracker: watchTracker
  };
}

// The debuggerHistory variable is passed in on swap. It represents
// the a state of the debugger for it to assume during init. It contains
// the paused state of the debugger, the recorded events, and the current
// event being processed.
function debuggerInit(debugModule, runtime, debuggerHistory /* =undefined */) {
  var currentEventIndex = 0;

  function resetProgram(position) {
    var closestSnapshot = debugModule.getSnapshotAt(position);
    debugModule.clearAsyncCallbacks();
    restoreSnapshot(debugModule.signalGraphNodes, closestSnapshot);
    redrawGraphics();
  }

  function restartProgram() {
    pauseProgram();
    resetProgram(0);
    debugModule.watchTracker.clear();
    debugModule.tracePath.clearTraces();
    debugModule.setContinue(0);
    debugModule.clearRecordedEvents();
    debugModule.clearSnapshots();
    executeCallbacks(debugModule.initialAsyncCallbacks);
  }

  function pauseProgram() {
    debugModule.setPaused();
    currentEventIndex = debugModule.getRecordedEventsLength();
  }

  function continueProgram() {
    if (debugModule.getPaused())
    {
      var closestSnapshotIndex =
          Math.floor(currentEventIndex / EVENTS_PER_SAVE) * EVENTS_PER_SAVE;
      resetProgram(currentEventIndex);
      var continueIndex = currentEventIndex;
      currentEventIndex = closestSnapshotIndex;
      stepTo(continueIndex);
      debugModule.setContinue(currentEventIndex);
    }
  }

  function stepTo(index) {
    if (!debugModule.getPaused()) {
      debugModule.setPaused();
      resetProgram();
    }

    if (index < 0 || index > getMaxSteps()) {
      throw "Index out of bounds: " + index;
    }

    if (index < currentEventIndex) {
      var closestSnapshotIndex = Math.floor(index / EVENTS_PER_SAVE) * EVENTS_PER_SAVE;
      resetProgram(index);
      currentEventIndex = closestSnapshotIndex;
    }

    while (currentEventIndex < index) {
      var nextEvent = debugModule.getRecordedEventAt(currentEventIndex);
      runtime.notify(nextEvent.id, nextEvent.value, nextEvent.timestep);

      currentEventIndex += 1;
    }
  }

  function getMaxSteps() {
    return debugModule.getRecordedEventsLength();
  }

  function redrawGraphics() {
    var main = debugModule.debuggedModule.main
    for (var i = main.kids.length ; i-- ; ) {
      main.kids[i].recv(runtime.timer.now(), true, main.id);
    }
  }

  function getSwapState() {
    var continueIndex = currentEventIndex;
    if (!debugModule.getPaused()) {
      continueIndex = getMaxSteps();
    }
    return {
      paused: debugModule.getPaused(),
      recordedEvents: debugModule.copyRecordedEvents(),
      currentEventIndex: continueIndex
    };
  }

  function dispose() {
    var parentNode = runtime.node.parentNode;
    parentNode.removeChild(debugModule.tracePath.canvas);
    parentNode.removeChild(runtime.node);
  }

  if (debuggerHistory) {
    // The problem is that we want to previous paused state. But
    // by the time JS reaches here, the old code has been swapped out
    // and the new modules are being generated. So we can ask the
    // debugging console what it thinks the pause state is and go
    // from there.
    var paused = debuggerHistory.paused;
    debugModule.setPaused();
    debugModule.loadRecordedEvents(debuggerHistory.recordedEvents);
    var index = getMaxSteps();
    runtime.debuggerStatus.eventCounter = 0;
    debugModule.tracePath.clearTraces();

    // draw new trace path
    debugModule.tracePath.startRecording();
    while(currentEventIndex < index) {
      var nextEvent = debugModule.getRecordedEventAt(currentEventIndex);
      runtime.debuggerStatus.eventCounter += 1;
      runtime.notify(nextEvent.id, nextEvent.value, nextEvent.timestep);
      debugModule.snapshotOnCheckpoint();
      currentEventIndex += 1;
    }
    debugModule.tracePath.stopRecording();

    stepTo(debuggerHistory.currentEventIndex);
    if (!paused) {
      debugModule.setContinue(debuggerHistory.currentEventIndex);
    }
  }

  runtime.node.parentNode.appendChild(debugModule.tracePath.canvas);

  var elmDebugger = {
      restart: restartProgram,
      pause: pauseProgram,
      kontinue: continueProgram,
      getMaxSteps: getMaxSteps,
      stepTo: stepTo,
      getPaused: debugModule.getPaused,
      getSwapState: getSwapState,
      dispose: dispose,
      allNodes: debugModule.signalGraphNodes,
      watchTracker: debugModule.watchTracker
  };

  return elmDebugger;
}

function Point(x, y) {
  this.x = x;
  this.y = y;

  this.translate = function(x, y) {
    return new Point(this.x + x, this.y + y);
  }

  this.equals = function(p) {
    return this.x == p.x && this.y == p.y;
  }
}

function tracePathInit(runtime, signalGraphMain) {
  var List = Elm.List.make(runtime);
  var Signal = Elm.Signal.make(runtime);
  var tracePathNode = A2(Signal.lift, graphicsUpdate, signalGraphMain);
  var tracePathCanvas = createCanvas();
  var tracePositions = {};
  var recordingTraces = true;

  function findPositions(currentScene) {
    var positions = {};
    function processElement(elem, offset) {
      if (elem.element.ctor == "Custom" && elem.element.type == "Collage")
      {
        List.map(F2(processForm)(offset))(elem.element.model.forms);
      }
    }

    function processForm(offset, form) {
      if (form.form.ctor == "FElement")
      {
        processElement(form.form._0, offset.translate(form.x, -form.y));
      }
      if (form.form.ctor == "FGroup")
      {
        var newOffset = offset.translate(form.x, -form.y);
        List.map(F2(processForm)(newOffset))(form.form._1);
      }
      if (form.debugTracePathId)
      {
        positions[form.debugTracePathId] = new Point(form.x + offset.x, -form.y + offset.y);
      }
    }

    processElement(currentScene, new Point(0, 0));
    return positions;
  }

  function appendPositions(positions) {
    for (var id in positions) {
      var pos = positions[id];
      if (tracePositions.hasOwnProperty(id)) {
        tracePositions[id].push(pos);
      }
      else {
        tracePositions[id] = [pos];
      }
      if (tracePositions[id].length < runtime.debuggerStatus.eventCounter) {
        var padCount = runtime.debuggerStatus.eventCounter - tracePositions[id].length;
        var lastTracePosition = tracePositions[id][tracePositions[id].length - 1];
        for (var i = padCount; i--;) {
          tracePositions[id].push(lastTracePosition)
        }
      }
      assert(tracePositions[id].length === runtime.debuggerStatus.eventCounter,
             "We don't have a 1-1 mapping of trace positions to events");
    }
  }

  function graphicsUpdate(currentScene) {
    if (!recordingTraces) {
      return;
    }

    var ctx = tracePathCanvas.getContext('2d');
    ctx.clearRect(0, 0, tracePathCanvas.width, tracePathCanvas.height);

    ctx.save();
    ctx.translate(ctx.canvas.width/2, ctx.canvas.height/2);
    appendPositions(findPositions(currentScene));
    for (var id in tracePositions)
    {
      ctx.beginPath();
      var points = tracePositions[id];
      for (var i=0; i < points.length; i++)
      {
        var p = points[i];
        if (i == 0) {
          ctx.moveTo(p.x, p.y);
        }
        else {
          ctx.lineTo(p.x, p.y);
        }
      }
      ctx.lineWidth = 1;
      ctx.strokeStyle = "rgba(50, 50, 50, 0.4)";
      ctx.stroke();
    }

    ctx.restore();
  }

  function clearTraces() {
    tracePositions = {};
  }

  function stopRecording() {
    recordingTraces = false;
  }

  function startRecording() {
    recordingTraces = true;
  }

  function clearTracesAfter(position) {
    var newTraces = {};
    for (var id in tracePositions) {
      newTraces[id] = tracePositions[id].slice(0,position);
    }
    tracePositions = newTraces;
  }

  return {
    graphicsUpdate: graphicsUpdate,
    canvas: tracePathCanvas,
    clearTraces: clearTraces,
    clearTracesAfter: clearTracesAfter,
    stopRecording: stopRecording,
    startRecording: startRecording
  }
}

function executeCallbacks(callbacks) {
  callbacks.forEach(function(timer) {
    if (!timer.executed) {
      var func = timer.func;
      timer.executed = true;
      func();
    }
  });
}

function createCanvas() {
  var c = document.createElement('canvas');
  c.width = window.innerWidth;
  c.height = window.innerHeight;
  c.style.position = "absolute";
  c.style.top = "0";
  c.style.left = "0";
  c.style.pointerEvents = "none";
  return c;
}

function assert(bool, msg) {
  if (!bool) {
    throw "Assertion error: " + msg;
  }
}

function snapshotSignalGraph(signalGraphNodes) {
  var nodeValues = [];

  signalGraphNodes.forEach(function(node) {
    nodeValues.push({ value: node.value, id: node.id });
  });

  return nodeValues;
};

function restoreSnapshot(signalGraphNodes, snapshot) {
  assert(signalGraphNodes.length == snapshot.length,
         "saved program state has wrong length");
  for (var i=0; i < signalGraphNodes.length; i++) {
    var node = signalGraphNodes[i];
    var state = snapshot[i];
    assert(node.id == state.id, "the nodes moved position");

    node.value = state.value;
  }
}

function flattenSignalGraph(nodes) {
  var nodesById = {};

  function addAllToDict(node) {
    nodesById[node.id] = node;
    node.kids.forEach(addAllToDict);
  }
  nodes.forEach(addAllToDict);

  var allNodes = Object.keys(nodesById).sort().map(function(key) {
    return nodesById[key];
  });
  return allNodes;
};

}());
(function() {

// Returns boolean indicating if the swap was successful.
// Requires that the two signal graphs have exactly the same
// structure.
ElmRuntime.swap = function(from, to) {
    function similar(nodeOld,nodeNew) {
        var idOkay = nodeOld.id === nodeNew.id;
        var lengthOkay = nodeOld.kids.length === nodeNew.kids.length;
        return idOkay && lengthOkay;
    }
    function swap(nodeOld,nodeNew) {
        nodeNew.value = nodeOld.value;
        return true;
    }
    var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
    if (canSwap) { depthFirstTraversals(swap, from.inputs, to.inputs); }
    from.node.parentNode.replaceChild(to.node, from.node);
    return canSwap;
}

// Returns false if the node operation f ever fails.
function depthFirstTraversals(f, queueOld, queueNew) {
    if (queueOld.length !== queueNew.length) return false;
    queueOld = queueOld.slice(0);
    queueNew = queueNew.slice(0);

    var seen = [];
    while (queueOld.length > 0 && queueNew.length > 0) {
        var nodeOld = queueOld.pop();
        var nodeNew = queueNew.pop();
        if (seen.indexOf(nodeOld.id) < 0) {
            if (!f(nodeOld, nodeNew)) return false;
            queueOld = queueOld.concat(nodeOld.kids);
            queueNew = queueNew.concat(nodeNew.kids);
            seen.push(nodeOld.id);
        }
    }
    return true;
}

}());

(function() {
'use strict';

Elm.fullscreen = function(module, ports) {
    var style = document.createElement('style');
    style.type = 'text/css';
    style.innerHTML = "html,head,body { padding:0; margin:0; }" +
        "body { font-family: calibri, helvetica, arial, sans-serif; }";
    document.head.appendChild(style);
    var container = document.createElement('div');
    document.body.appendChild(container);
    return init(ElmRuntime.Display.FULLSCREEN, container, module, ports || {});
};

Elm.embed = function(module, container, ports) {
    var tag = container.tagName;
    if (tag !== 'DIV') {
        throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
    } else if (container.hasChildNodes()) {
        throw new Error('Elm.node must be given an empty DIV. No children allowed!');
    }
    return init(ElmRuntime.Display.COMPONENT, container, module, ports || {});
};

Elm.worker = function(module, ports) {
    return init(ElmRuntime.Display.NONE, {}, module, ports || {});
};

function init(display, container, module, ports, moduleToReplace) {
  // defining state needed for an instance of the Elm RTS
  var inputs = [];

  /* OFFSET
   * Elm's time traveling debugger lets you interrupt the smooth flow of time
   * by pausing and continuing program execution. To ensure the user sees a
   * program that moves smoothly through the pause/continue time gap,
   * we need to adjsut the value of Date.now().
   */
  var timer = function() {
    var inducedDelay = 0;

    var now = function() {
      return Date.now() - inducedDelay;
    };

    var addDelay = function(d) {
      inducedDelay += d;
      return inducedDelay;
    };

    return { now : now
           , addDelay : addDelay
           }
  }();

  var updateInProgress = false;
  function notify(id, v) {
      if (updateInProgress) {
          throw new Error(
              'The notify function has been called synchronously!\n' +
              'This can lead to frames being dropped.\n' +
              'Definitely report this to <https://github.com/elm-lang/Elm/issues>\n');
      }
      updateInProgress = true;
      var timestep = timer.now();
      for (var i = inputs.length; i--; ) {
          inputs[i].recv(timestep, id, v);
      }
      updateInProgress = false;
  }
  function setTimeout(func, delay) {
    window.setTimeout(func, delay);
  }

  var listeners = [];
  function addListener(relevantInputs, domNode, eventName, func) {
      domNode.addEventListener(eventName, func);
      var listener = {
          relevantInputs: relevantInputs,
          domNode: domNode,
          eventName: eventName,
          func: func
      };
      listeners.push(listener);
  }

  var portUses = {}
  for (var key in ports) {
      portUses[key] = 0;
  }
  // create the actual RTS. Any impure modules will attach themselves to this
  // object. This permits many Elm programs to be embedded per document.
  var elm = {
      notify:notify,
      setTimeout:setTimeout,
      node:container,
      display:display,
      id:ElmRuntime.guid(),
      addListener:addListener,
      inputs:inputs,
      timer:timer,
      ports: { incoming:ports, outgoing:{}, uses:portUses }
  };

  function swap(newModule) {
      removeListeners(listeners);
      var div = document.createElement('div');
      var newElm = init(display, div, newModule, ports, elm);
      inputs = [];
      // elm.swap = newElm.swap;
      return newElm;
  }

  function dispose() {
    removeListeners(listeners);
    inputs = [];
  }

  var Module = {};
  var reportAnyErrors = function() {};
  try {
      Module = module.make(elm);
      checkPorts(elm);
  } catch(e) {
      var directions = "<br/>&nbsp; &nbsp; Open the developer console for more details."
      Module.main = Elm.Text.make(elm).leftAligned('<code>' + e.message + directions + '</code>');
      reportAnyErrors = function() { throw e; }
  }
  inputs = ElmRuntime.filterDeadInputs(inputs);
  filterListeners(inputs, listeners);
  addReceivers(elm.ports.outgoing);
  if (display !== ElmRuntime.Display.NONE) {
      var graphicsNode = initGraphics(elm, Module);
  }
  if (typeof moduleToReplace !== 'undefined') {
      ElmRuntime.swap(moduleToReplace, elm);

      // rerender scene if graphics are enabled.
      if (typeof graphicsNode !== 'undefined') {
          graphicsNode.recv(0, true, 0);
      }
  }

  reportAnyErrors();
  return {
    swap:swap,
    ports:elm.ports.outgoing,
    dispose:dispose
  };
};

function checkPorts(elm) {
    var portUses = elm.ports.uses;
    for (var key in portUses) {
        var uses = portUses[key]
        if (uses === 0) {
            throw new Error(
                "Initialization Error: provided port '" + key +
                "' to a module that does not take it as in input.\n" +
                "Remove '" + key + "' from the module initialization code.");
        } else if (uses > 1) {
            throw new Error(
                "Initialization Error: port '" + key +
                "' has been declared multiple times in the Elm code.\n" +
                "Remove declarations until there is exactly one.");
        }
    }
}
    
function filterListeners(inputs, listeners) {
    loop:
    for (var i = listeners.length; i--; ) {
        var listener = listeners[i];
        for (var j = inputs.length; j--; ) {
            if (listener.relevantInputs.indexOf(inputs[j].id) >= 0) {
                continue loop;
            }
        }
        listener.domNode.removeEventListener(listener.eventName, listener.func);
    }
}

function removeListeners(listeners) {
    for (var i = listeners.length; i--; ) {
        var listener = listeners[i];
        listener.domNode.removeEventListener(listener.eventName, listener.func);
    }
}

// add receivers for built-in ports if they are defined
function addReceivers(ports) {
    if ('log' in ports) {
        ports.log.subscribe(function(v) { console.log(v) });
    }
    if ('stdout' in ports) {
        var process = process || {};
        var handler = process.stdout
            ? function(v) { process.stdout.write(v); }
            : function(v) { console.log(v); };
        ports.stdout.subscribe(handler);
    }
    if ('stderr' in ports) {
        var process = process || {};
        var handler = process.stderr
            ? function(v) { process.stderr.write(v); }
            : function(v) { console.log('Error:' + v); };
        ports.stderr.subscribe(handler);
    }
    if ('title' in ports) {
        if (typeof ports.title === 'string') {
            document.title = ports.title;
        } else {
            ports.title.subscribe(function(v) { document.title = v; });
        }
    }
    if ('redirect' in ports) {
        ports.redirect.subscribe(function(v) {
            if (v.length > 0) window.location = v;
        });
    }
    if ('favicon' in ports) {
        if (typeof ports.favicon === 'string') {
            changeFavicon(ports.favicon);
        } else {
            ports.favicon.subscribe(changeFavicon);
        }
    }
    function changeFavicon(src) {
        var link = document.createElement('link');
        var oldLink = document.getElementById('elm-favicon');
        link.id = 'elm-favicon';
        link.rel = 'shortcut icon';
        link.href = src;
        if (oldLink) {
            document.head.removeChild(oldLink);
        }
        document.head.appendChild(link);
    }
}

function initGraphics(elm, Module) {
  if (!('main' in Module)) {
      throw new Error("'main' is missing! What do I display?!");
  }

  var signalGraph = Module.main;

  // make sure the signal graph is actually a signal & extract the visual model
  var Signal = Elm.Signal.make(elm);
  if (!('recv' in signalGraph)) {
      signalGraph = Signal.constant(signalGraph);
  }
  var currentScene = signalGraph.value;

 // Add the currentScene to the DOM
  var Render = ElmRuntime.use(ElmRuntime.Render.Element);
  elm.node.appendChild(Render.render(currentScene));

  // set up updates so that the DOM is adjusted as necessary.
  var savedScene = currentScene;
  var previousDrawId = 0;
  function domUpdate(newScene) {
      previousDrawId = ElmRuntime.draw(previousDrawId, function(_) {
          Render.update(elm.node.firstChild, savedScene, newScene);
          if (elm.Native.Window) elm.Native.Window.values.resizeIfNeeded();
          savedScene = newScene;
      });
  }
  var renderer = A2(Signal.lift, domUpdate, signalGraph);

  // must check for resize after 'renderer' is created so
  // that changes show up.
  if (elm.Native.Window) elm.Native.Window.values.resizeIfNeeded();

  return renderer;
}

}());

(function() {
'use strict';

ElmRuntime.Display = { FULLSCREEN: 0, COMPONENT: 1, NONE: 2 };

ElmRuntime.counter = 0;
ElmRuntime.guid = function() { return ElmRuntime.counter++; }

ElmRuntime.use = function(M) {
    if (typeof M === 'function') M = M();
    return M;
};

function isAlive(input) {
    if (!('defaultNumberOfKids' in input)) return true;
    var len = input.kids.length;
    if (len === 0) return false;
    if (len > input.defaultNumberOfKids) return true;
    var alive = false;
    for (var i = len; i--; ) {
        alive = alive || isAlive(input.kids[i]);
    }
    return alive;
}

ElmRuntime.filterDeadInputs = function(inputs) {
    var temp = [];
    for (var i = inputs.length; i--; ) {
        if (isAlive(inputs[i])) temp.push(inputs[i]);
    }
    return temp;
};

// define function for drawing efficiently
//
//   draw : RequestID -> (() -> ()) -> RequestID
//
// Takes a "RequestID" allowing you to cancel old requests if possible.
// Returns a "RequestID" so you can refer to past requests.
//
var vendors = ['ms', 'moz', 'webkit', 'o'];
var win = typeof window !== 'undefined' ? window : {};
for (var i = 0; i < vendors.length && !win.requestAnimationFrame; ++i) {
    win.requestAnimationFrame = win[vendors[i]+'RequestAnimationFrame'];
    win.cancelAnimationFrame  = win[vendors[i]+'CancelAnimationFrame'] ||
                                win[vendors[i]+'CancelRequestAnimationFrame'];
}

if (win.requestAnimationFrame && win.cancelAnimationFrame) {
    ElmRuntime.draw = function(previousRequestID, callback) {
        win.cancelAnimationFrame(previousRequestID);
        return win.requestAnimationFrame(callback);
    };
} else {
    ElmRuntime.draw = function(previousRequestID, callback) {
        callback();
        return previousRequestID;
    };
}

}());

ElmRuntime.Render.Collage = function() {

var Render = ElmRuntime.use(ElmRuntime.Render.Element);
var Transform = Elm.Transform2D.make({});
var Utils = ElmRuntime.use(ElmRuntime.Render.Utils);
var newElement = Utils.newElement,
    colorToCss = Utils.colorToCss, fromList = Utils.fromList,
    fromString = Utils.fromString, addTransform = Utils.addTransform;

function trace(ctx, path) {
    var points = fromList(path);
    var i = points.length - 1;
    if (i <= 0) return;
    ctx.moveTo(points[i]._0, points[i]._1);
    while (i--) { ctx.lineTo(points[i]._0, points[i]._1); }
    if (path.closed) {
        i = points.length - 1;
        ctx.lineTo(points[i]._0, points[i]._1);
    }
}

function line(ctx,style,path) {
    style.dashing.ctor === '[]' ? trace(ctx, path) : customLineHelp(ctx, style, path);
    ctx.scale(1,-1);
    ctx.stroke();
}

function customLineHelp(ctx, style, path) {
    var points = fromList(path);
    if (path.closed) points.push(points[0]);
    var pattern = fromList(style.dashing);
    var i = points.length - 1;
    if (i <= 0) return;
    var x0 = points[i]._0, y0 = points[i]._1;
    var x1=0, y1=0, dx=0, dy=0, remaining=0, nx=0, ny=0;
    var pindex = 0, plen = pattern.length;
    var draw = true, segmentLength = pattern[0];
    ctx.moveTo(x0,y0);
    while (i--) {
        x1 = points[i]._0; y1 = points[i]._1;
        dx = x1 - x0; dy = y1 - y0;
        remaining = Math.sqrt(dx * dx + dy * dy);
        while (segmentLength <= remaining) {
            x0 += dx * segmentLength / remaining;
            y0 += dy * segmentLength / remaining;
            ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
            // update starting position
            dx = x1 - x0; dy = y1 - y0;
            remaining = Math.sqrt(dx * dx + dy * dy);
            // update pattern
            draw = !draw;
            pindex = (pindex + 1) % plen;
            segmentLength = pattern[pindex];
        }
        if (remaining > 0) {
            ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
            segmentLength -= remaining;
        }
        x0 = x1; y0 = y1;
    }
}

function drawLine(ctx, style, path) {
    ctx.lineWidth = style.width;
    var cap = style.cap.ctor;
    ctx.lineCap = cap === 'Flat' ? 'butt' :
                  cap === 'Round' ? 'round' : 'square';
    var join = style.join.ctor;
    ctx.lineJoin = join === 'Smooth' ? 'round' :
                   join === 'Sharp' ? 'miter' : 'bevel';
    ctx.miterLimit = style.join._0 || 10;
    ctx.strokeStyle = colorToCss(style.color);
    return line(ctx, style, path);
}

function texture(redo, ctx, src) {
    var img = new Image();
    img.src = fromString(src);
    img.onload = redo;
    return ctx.createPattern(img, 'repeat');
}

function gradient(ctx, grad) {
  var g;
  var stops = [];
  if (grad.ctor === 'Linear') {
    var p0 = grad._0, p1 = grad._1;
    g = ctx.createLinearGradient(p0._0, -p0._1, p1._0, -p1._1);
    stops = fromList(grad._2);
  } else {
    var p0 = grad._0, p2 = grad._2;
    g = ctx.createRadialGradient(p0._0, -p0._1, grad._1, p2._0, -p2._1, grad._3);
    stops = fromList(grad._4);
  }
  var len = stops.length;
  for (var i = 0; i < len; ++i) {
    var stop = stops[i];
    g.addColorStop(stop._0, colorToCss(stop._1));
  }
  return g;
}

function drawShape(redo, ctx, style, path) {
    trace(ctx, path);
    var sty = style.ctor;
    ctx.fillStyle =
        sty === 'Solid' ? colorToCss(style._0) :
        sty === 'Texture' ? texture(redo, ctx, style._0) : gradient(ctx, style._0);
    ctx.scale(1,-1);
    ctx.fill();
}

function drawImage(redo, ctx, form) {
    var img = new Image();
    img.onload = redo;
    img.src = fromString(form._3);
    var w = form._0,
        h = form._1,
        pos = form._2,
        srcX = pos._0,
        srcY = pos._1,
        srcW = w,
        srcH = h,
        destX = -w/2,
        destY = -h/2,
        destW = w,
        destH = h;

    ctx.scale(1,-1);
    ctx.drawImage(img, srcX, srcY, srcW, srcH, destX, destY, destW, destH);
}

function renderForm(redo, ctx, form) {
    ctx.save();
    var x = form.x, y = form.y, theta = form.theta, scale = form.scale;
    if (x !== 0 || y !== 0) ctx.translate(x, y);
    if (theta !== 0) ctx.rotate(theta);
    if (scale !== 1) ctx.scale(scale,scale);
    if (form.alpha !== 1) ctx.globalAlpha = ctx.globalAlpha * form.alpha;
    ctx.beginPath();
    var f = form.form;
    switch(f.ctor) {
    case 'FPath' : drawLine(ctx, f._0, f._1); break;
    case 'FImage': drawImage(redo, ctx, f); break;
    case 'FShape':
      if (f._0.ctor === 'Left') {
        f._1.closed = true;
        drawLine(ctx, f._0._0, f._1);
      } else {
        drawShape(redo, ctx, f._0._0, f._1);
      }
    break;
    }
    ctx.restore();
}

function formToMatrix(form) {
   var scale = form.scale;
   var matrix = A6( Transform.matrix, scale, 0, 0, scale, form.x, form.y );

   var theta = form.theta
   if (theta !== 0)
       matrix = A2( Transform.multiply, matrix, Transform.rotation(theta) );

   return matrix;
}

function str(n) {
    if (n < 0.00001 && n > -0.00001) return 0;
    return n;
}

function makeTransform(w, h, form, matrices) {
    var props = form.form._0.props;
    var m = A6( Transform.matrix, 1, 0, 0, -1,
                (w - props.width ) / 2,
                (h - props.height) / 2 );
    var len = matrices.length;
    for (var i = 0; i < len; ++i) { m = A2( Transform.multiply, m, matrices[i] ); }
    m = A2( Transform.multiply, m, formToMatrix(form) );

    return 'matrix(' + str( m[0]) + ', ' + str( m[3]) + ', ' +
                       str(-m[1]) + ', ' + str(-m[4]) + ', ' +
                       str( m[2]) + ', ' + str( m[5]) + ')';
}

function stepperHelp(list) {
    var arr = fromList(list);
    var i = 0;
    function peekNext() {
        return i < arr.length ? arr[i].form.ctor : '';
    }
    // assumes that there is a next element
    function next() {
        var out = arr[i];
        ++i;
        return out;
    }
    return { peekNext:peekNext, next:next };
}

function formStepper(forms) {
    var ps = [stepperHelp(forms)];
    var matrices = [];
    var alphas = [];
    function peekNext() {
        var len = ps.length;
        var formType = '';
        for (var i = 0; i < len; ++i ) {
            if (formType = ps[i].peekNext()) return formType;
        }
        return '';
    }
    // assumes that there is a next element
    function next(ctx) {
        while (!ps[0].peekNext()) {
            ps.shift();
            matrices.pop();
            alphas.shift();
            if (ctx) { ctx.restore(); }
        }
        var out = ps[0].next();
        var f = out.form;
        if (f.ctor === 'FGroup') {
            ps.unshift(stepperHelp(f._1));
            var m = A2(Transform.multiply, f._0, formToMatrix(out));
            ctx.save();
            ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
            matrices.push(m);

            var alpha = (alphas[0] || 1) * out.alpha;
            alphas.unshift(alpha);
            ctx.globalAlpha = alpha;
        }
        return out;
    }
    function transforms() { return matrices; }
    function alpha() { return alphas[0] || 1; }
    return { peekNext:peekNext, next:next, transforms:transforms, alpha:alpha };
}

function makeCanvas(w,h) {
    var canvas = newElement('canvas');
    canvas.style.width  = w + 'px';
    canvas.style.height = h + 'px';
    canvas.style.display = "block";
    canvas.style.position = "absolute";
    canvas.width  = w;
    canvas.height = h;
    return canvas;
}

function render(model) {
    var div = newElement('div');
    div.style.overflow = 'hidden';
    div.style.position = 'relative';
    update(div, model, model);
    return div;
}

function nodeStepper(w,h,div) {
    var kids = div.childNodes;
    var i = 0;
    function transform(transforms, ctx) {
        ctx.translate(w/2, h/2);
        ctx.scale(1,-1);
        var len = transforms.length;
        for (var i = 0; i < len; ++i) {
            var m = transforms[i];
            ctx.save();
            ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
        }
        return ctx;
    }
    function nextContext(transforms) {
        while (i < kids.length) {
            var node = kids[i];
            if (node.getContext) {
                node.width = w;
                node.height = h;
                node.style.width = w + 'px';
                node.style.height = h + 'px';
                ++i;
                return transform(transforms, node.getContext('2d'));
            }
            div.removeChild(node);
        }
        var canvas = makeCanvas(w,h);
        div.appendChild(canvas);
        // we have added a new node, so we must step our position
        ++i;
        return transform(transforms, canvas.getContext('2d'));
    }
    function addElement(matrices, alpha, form) {
        var kid = kids[i];
        var elem = form.form._0;

        var node = (!kid || kid.getContext)
            ? Render.render(elem)
            : (Render.update(kid, kid.oldElement, elem), kids[i]);

        node.style.position = 'absolute';
        node.style.opacity = alpha * form.alpha * elem.props.opacity;
        addTransform(node.style, makeTransform(w, h, form, matrices));
        node.oldElement = elem;
        ++i;
        if (!kid) {
            div.appendChild(node);
        } else if (kid.getContext) {
            div.insertBefore(node, kid);
        }
    }
    function clearRest() {
        while (i < kids.length) {
            div.removeChild(kids[i]);
        }
    }
    return { nextContext:nextContext, addElement:addElement, clearRest:clearRest };
}


function update(div, _, model) {
    var w = model.w;
    var h = model.h;

    var forms = formStepper(model.forms);
    var nodes = nodeStepper(w,h,div);
    var ctx = null;
    var formType = '';

    while (formType = forms.peekNext()) {
        // make sure we have context if we need it
        if (ctx === null && formType !== 'FElement') {
            ctx = nodes.nextContext(forms.transforms());
            ctx.globalAlpha = forms.alpha();
        }

        var form = forms.next(ctx);
        // if it is FGroup, all updates are made within formStepper when next is called.
        if (formType === 'FElement') {
            // update or insert an element, get a new context
            nodes.addElement(forms.transforms(), forms.alpha(), form);
            ctx = null;
        } else if (formType !== 'FGroup') {
            renderForm(function() { update(div, model, model); }, ctx, form);
        }
    }
    nodes.clearRest();
    return false;
}

return { render:render, update:update };

};

ElmRuntime.Render.Element = function() {
'use strict';

var Utils = ElmRuntime.use(ElmRuntime.Render.Utils);
var newElement = Utils.newElement;
var colorToCss = Utils.colorToCss;
var addTransform = Utils.addTransform;
var removeTransform = Utils.removeTransform;
var fromList = Utils.fromList;
var eq = Utils.eq;

function setProps(elem, node) {
    var props = elem.props;

    var element = elem.element;
    var width = props.width - (element.adjustWidth || 0);
    var height = props.height - (element.adjustHeight || 0);
    node.style.width  = (width |0) + 'px';
    node.style.height = (height|0) + 'px';

    if (props.opacity !== 1) {
        node.style.opacity = props.opacity;
    }

    if (props.color.ctor === 'Just') {
        node.style.backgroundColor = colorToCss(props.color._0);
    }

    if (props.tag !== '') {
        node.id = props.tag;
    }

    if (props.hover.ctor !== '_Tuple0') {
        addHover(node, props.hover);
    }

    if (props.click.ctor !== '_Tuple0') {
        addClick(node, props.click);
    }

    if (props.href !== '') {
        var anchor = newElement('a');
        anchor.href = props.href;
        anchor.style.display = 'block';
        anchor.style.pointerEvents = 'auto';
        anchor.appendChild(node);
        node = anchor;
    }

    return node;
}

function addClick(e, handler) {
    e.style.pointerEvents = 'auto';
    e.elm_click_handler = handler;
    function trigger(ev) {
        e.elm_click_handler(Utils.Tuple0);
        ev.stopPropagation();
    }
    e.elm_click_trigger = trigger;
    e.addEventListener('click', trigger);
}

function removeClick(e, handler) {
    if (e.elm_click_trigger) {
        e.removeEventListener('click', e.elm_click_trigger);
        e.elm_click_trigger = null;
        e.elm_click_handler = null;
    }
}

function addHover(e, handler) {
    e.style.pointerEvents = 'auto';
    e.elm_hover_handler = handler;
    e.elm_hover_count = 0;

    function over(evt) {
        if (e.elm_hover_count++ > 0) return;
        e.elm_hover_handler(true);
        evt.stopPropagation();
    }
    function out(evt) {
        if (e.contains(evt.toElement || evt.relatedTarget)) return;
        e.elm_hover_count = 0;
        e.elm_hover_handler(false);
        evt.stopPropagation();
    }
    e.elm_hover_over = over;
    e.elm_hover_out = out;
    e.addEventListener('mouseover', over);
    e.addEventListener('mouseout', out);
}

function removeHover(e) {
    e.elm_hover_handler = null;
    if (e.elm_hover_over) {
        e.removeEventListener('mouseover', e.elm_hover_over);
        e.elm_hover_over = null;
    }
    if (e.elm_hover_out) {
        e.removeEventListener('mouseout', e.elm_hover_out);
        e.elm_hover_out = null;
    }
}

function image(props, img) {
    switch (img._0.ctor) {
    case 'Plain':   return plainImage(img._3);
    case 'Fitted':  return fittedImage(props.width, props.height, img._3);
    case 'Cropped': return croppedImage(img,props.width,props.height,img._3);
    case 'Tiled':   return tiledImage(img._3);
    }
}

function plainImage(src) {
    var img = newElement('img');
    img.src = src;
    img.name = src;
    img.style.display = "block";
    return img;
}

function tiledImage(src) {
    var div = newElement('div');
    div.style.backgroundImage = 'url(' + src + ')';
    return div;
}

function fittedImage(w, h, src) {
    var div = newElement('div');
    div.style.background = 'url(' + src + ') no-repeat center';
    div.style.webkitBackgroundSize = 'cover';
    div.style.MozBackgroundSize = 'cover';
    div.style.OBackgroundSize = 'cover';
    div.style.backgroundSize = 'cover';
    return div;
}

function croppedImage(elem, w, h, src) {
    var pos = elem._0._0;
    var e = newElement('div');
    e.style.overflow = "hidden";

    var img = newElement('img');
    img.onload = function() {
        var sw = w / elem._1, sh = h / elem._2;
        img.style.width = ((this.width * sw)|0) + 'px';
        img.style.height = ((this.height * sh)|0) + 'px';
        img.style.marginLeft = ((- pos._0 * sw)|0) + 'px';
        img.style.marginTop = ((- pos._1 * sh)|0) + 'px';
    };
    img.src = src;
    img.name = src;
    e.appendChild(img);
    return e;
}

function goOut(e) { e.style.position = 'absolute'; return e; }
function goDown(e) { return e }
function goRight(e) { e.style.styleFloat = e.style.cssFloat = "left"; return e; }

var directionTable = {
    DUp    : goDown,
    DDown  : goDown,
    DLeft  : goRight,
    DRight : goRight,
    DIn    : goOut,
    DOut   : goOut
};
function needsReversal(dir) {
    return dir == 'DUp' || dir == 'DLeft' || dir == 'DIn';
}

function flow(dir,elist) {
    var array = fromList(elist);
    var container = newElement('div');
    var goDir = directionTable[dir];
    if (goDir == goOut) {
        container.style.pointerEvents = 'none';
    }
    if (needsReversal(dir)) {
        array.reverse();
    }
    var len = array.length;
    for (var i = 0; i < len; ++i) {
        container.appendChild(goDir(render(array[i])));
    }
    return container;
}

function toPos(pos) {
    switch(pos.ctor) {
    case "Absolute": return  pos._0 + "px";
    case "Relative": return (pos._0 * 100) + "%";
    }
}

// must clear right, left, top, bottom, and transform
// before calling this function
function setPos(pos,elem,e) {
    var element = elem.element;
    var props = elem.props;
    var w = props.width + (element.adjustWidth ? element.adjustWidth : 0);
    var h = props.height + (element.adjustHeight ? element.adjustHeight : 0);

    e.style.position = 'absolute';
    e.style.margin = 'auto';
    var transform = '';
    switch(pos.horizontal.ctor) {
    case 'P': e.style.right = toPos(pos.x); e.style.removeProperty('left'); break;
    case 'Z': transform = 'translateX(' + ((-w/2)|0) + 'px) ';
    case 'N': e.style.left = toPos(pos.x); e.style.removeProperty('right'); break;
    }
    switch(pos.vertical.ctor) {
    case 'N': e.style.bottom = toPos(pos.y); e.style.removeProperty('top'); break;
    case 'Z': transform += 'translateY(' + ((-h/2)|0) + 'px)';
    case 'P': e.style.top = toPos(pos.y); e.style.removeProperty('bottom'); break;
    }
    if (transform !== '') addTransform(e.style, transform);
    return e;
}

function container(pos,elem) {
    var e = render(elem);
    setPos(pos, elem, e);
    var div = newElement('div');
    div.style.position = 'relative';
    div.style.overflow = 'hidden';
    div.appendChild(e);
    return div;
}

function rawHtml(elem) {
    var html = elem.html;
    var args = elem.args;
    var guid = elem.guid;
    var align = elem.align;

    var div = newElement('div');
    div.innerHTML = html;
    div.style.visibility = "hidden";
    if (align) div.style.textAlign = align;
    document.body.appendChild(div);

    for (var i = args.length; i--; ) {
        var arg = args[i];
        var span = document.getElementById('md-' + guid + '-' + i);
        if (arg.isText) {
            span.innerHTML = arg;
        } else {
            span.style.display = 'block';
            span.style.width = arg.props.width + 'px';
            span.style.height = arg.props.height + 'px';
            span.appendChild(render(arg));
        }
    }
    document.body.removeChild(div);
    div.style.visibility = 'visible';
    div.style.pointerEvents = 'auto';
    return div;
}

function render(elem) {
    return setProps(elem, makeElement(elem));
}
function makeElement(e) {
    var elem = e.element;
    switch(elem.ctor) {
    case 'Image':     return image(e.props, elem);
    case 'Flow':      return flow(elem._0.ctor, elem._1);
    case 'Container': return container(elem._0, elem._1);
    case 'Spacer':    return newElement('div');
    case 'RawHtml':   return rawHtml(elem);
    case 'Custom':    return elem.render(elem.model);
    }
}

function update(node, curr, next) {
    if (node.tagName === 'A') {
        node = node.firstChild;
    }
    if (curr.props.id === next.props.id) {
        return updateProps(node, curr, next);
    }
    if (curr.element.ctor !== next.element.ctor) {
        node.parentNode.replaceChild(render(next),node);
        return true;
    }
    var nextE = next.element, currE = curr.element;
    switch(nextE.ctor) {
    case "Spacer": break;
    case "RawHtml":
        // only markdown blocks have guids, so this must be a text block
        if (nextE.guid === null) {
            if(currE.html.valueOf() !== nextE.html.valueOf()) {
                node.innerHTML = nextE.html;
            }
            break;
        }
        if (nextE.guid !== currE.guid) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        var nargs = nextE.args;
        var cargs = currE.args;
        for (var i = nargs.length; i--; ) {
            var narg = nargs[i];
            var carg = cargs[i]
            if (narg == carg) continue;
            var span = document.getElementById('md-' + currE.guid + '-' + i);
            if (narg.isElement) {
                if (carg.isElement) {
                    update(span, carg, narg);
                } else {
                    span.style.display = 'block';
                    var e = render(narg);
                    span.innerHTML = '';
                    span.appendChild(e);
                }
            } else {
                span.style.display = 'inline';
                span.innerHTML = narg;
            }
        }
        break;
    case "Image":
        if (nextE._0.ctor === 'Plain') {
            if (nextE._3 !== currE._3) node.src = nextE._3;
        } else if (!eq(nextE,currE) ||
                   next.props.width !== curr.props.width ||
                   next.props.height !== curr.props.height) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        break;
    case "Flow":
        var arr = fromList(nextE._1);
        for (var i = arr.length; i--; ) { arr[i] = arr[i].element.ctor; }
        if (nextE._0.ctor !== currE._0.ctor) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        var nexts = fromList(nextE._1);
        var kids = node.childNodes;
        if (nexts.length !== kids.length) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        var currs = fromList(currE._1);
        var dir = nextE._0.ctor;
        var goDir = directionTable[dir];
        var toReverse = needsReversal(dir);
        var len = kids.length;
        for (var i = len; i-- ;) {
            update(kids[toReverse ? len - i - 1 : i],currs[i],nexts[i]);
            goDir(kids[i]);
        }
        break;
    case "Container":
        update(node.firstChild, currE._1, nextE._1);
        setPos(nextE._0, nextE._1, node.firstChild);
        break;
    case "Custom":
        if (currE.type === nextE.type) {
            var done = nextE.update(node, currE.model, nextE.model);
            if (done) return;
        } else {
            return node.parentNode.replaceChild(render(next), node);
        }
    }
    updateProps(node, curr, next);
}

function updateProps(node, curr, next) {
    var nextProps = next.props;
    var currProps = curr.props;

    var element = next.element;
    var width = nextProps.width - (element.adjustWidth || 0);
    var height = nextProps.height - (element.adjustHeight || 0);
    if (width !== currProps.width) {
        node.style.width = (width|0) + 'px';
    }
    if (height !== currProps.height) {
        node.style.height = (height|0) + 'px';
    }

    if (nextProps.opacity !== currProps.opacity) {
        node.style.opacity = nextProps.opacity;
    }

    var nextColor = nextProps.color.ctor === 'Just'
        ? colorToCss(nextProps.color._0)
        : 'transparent';
    if (node.style.backgroundColor !== nextColor) {
        node.style.backgroundColor = nextColor;
    }

    if (nextProps.tag !== currProps.tag) {
        node.id = nextProps.tag;
    }

    if (nextProps.href !== currProps.href) {
        if (currProps.href === '') {
            // add a surrounding href
            var anchor = newElement('a');
            anchor.href = nextProps.href;
            anchor.style.display = 'block';
            anchor.style.pointerEvents = 'auto';

            node.parentNode.replaceChild(anchor, node);
            anchor.appendChild(node);
        } else if (nextProps.href === '') {
            // remove the surrounding href
            var anchor = node.parentNode;
            anchor.parentNode.replaceChild(node, anchor);
        } else {
            // just update the link
            node.parentNode.href = nextProps.href;
        }
    }

    // update click and hover handlers
    var removed = false;

    // update hover handlers
    if (currProps.hover.ctor === '_Tuple0') {
        if (nextProps.hover.ctor !== '_Tuple0') {
            addHover(node, nextProps.hover);
        }
    }
    else {
        if (nextProps.hover.ctor === '_Tuple0') {
            removed = true;
            removeHover(node);
        }
        else {
            node.elm_hover_handler = nextProps.hover;
        }
    }

    // update click handlers
    if (currProps.click.ctor === '_Tuple0') {
        if (nextProps.click.ctor !== '_Tuple0') {
            addClick(node, nextProps.click);
        }
    }
    else {
        if (nextProps.click.ctor === '_Tuple0') {
            removed = true;
            removeClick(node);
        } else {
            node.elm_click_handler = nextProps.click;
        }
    }

    // stop capturing clicks if 
    if (removed
        && nextProps.hover.ctor === '_Tuple0'
        && nextProps.click.ctor === '_Tuple0')
    {
        node.style.pointerEvents = 'none';
    }
}

return { render:render, update:update };

};
ElmRuntime.Render.Utils = function() {
  function newElement(elementType) {
      var e = document.createElement(elementType);
      e.style.padding = "0";
      e.style.margin = "0";
      return e;
  }

  function addTo(container, elem) {
      container.appendChild(elem);
  }

  function addTransform(style, trans) {
    style.transform       = trans;
    style.msTransform     = trans;
    style.MozTransform    = trans;
    style.webkitTransform = trans;
    style.OTransform      = trans;
  }

  function removeTransform(style) {
    style.transform       = 'none';
    style.msTransform     = 'none';
    style.MozTransform    = 'none';
    style.webkitTransform = 'none';
    style.OTransform      = 'none';
  }

  var List = Elm.Native.List.make({});

  return {addTo:addTo,
          newElement: newElement,
          colorToCss: Elm.Native.Color.make({}).toCss,
          fromList: List.toArray,
          fromString: function(s) { return s; },
          toString: function(s) { return s; },
          eq: Elm.Native.Utils.make({}).eq,
          addTransform: addTransform,
          removeTransform: removeTransform};
};
Elm.Native.Dropbox = {};
Elm.Native.Dropbox.make = function(elm) {
  elm.Native = elm.Native || {};
  elm.Native.Dropbox = elm.Native.Dropbox || {};
  if (elm.Native.Dropbox.values) return elm.Native.Dropbox.values;

  // Imports
  var Signal = Elm.Native.Signal.make(elm);

  function client(apiKey) {
    var client = new Dropbox.Client({ key: apiKey });

    client.authenticate(function(error, client) {
      if (error) {
        return alert(error);
      }
      console.log("elm-dropbox: Dropbox is authorized");
    });

    function read(filename) {
      var output = Signal.constant("");

      client.readFile(filename, function(error, data) {
        if (error) {
          return alert(error);
        }
        console.log("elm-dropbox: " + filename + ": Read from Dropbox");
        elm.notify(output.id, data);
      });

      return output;
    }

    function write(filename, dataSignal) {
      var isFirst = true;
      var writeToken = null;
      var handler = function(data) {
        if (isFirst) return isFirst = false;
        console.log("elm-dropbox: " + filename + ": Writing...", data.slice(0, 100));
        if (writeToken) clearTimeout(writeToken);
        writeToken = setTimeout(function() {
          client.writeFile(filename, data, function(error, stat) {
            if (error) {
              return alert(error);
            }
            console.log("elm-dropbox: " + filename + ": Wrote revision " + stat.versionTag);
          });
        }, 5000);
      }
      Signal.lift(handler)(dataSignal);
    }

    return { read: read, write: F2(write) };
  }

  return elm.Native.Dropbox.values = {
    client: client
  };
};
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
Elm.Native.Keys = {};
Elm.Native.Keys.make = function(elm) {
  elm.Native = elm.Native || {};
  elm.Native.Keys = elm.Native.Keys || {};
  if (elm.Native.Keys.values) return elm.Native.Keys.values;

  // Imports
  var Signal = Elm.Native.Signal.make(elm);

  var pressesIn = Signal.constant("");
  var downsIn = Signal.constant(0);
  var metaIn = Signal.constant(0);

  var specialKeys = {
    '8': 'backspace',
    '37': 'left',
    '38': 'up',
    '39': 'right',
    '40': 'down'
  };

  var modKeys = {
    '91': 'meta'
  };

  var downMods = {};

  document.onkeydown = function(e) {
    var mod;
    if (mod = modKeys[e.keyCode.toString()]) {
      downMods[mod] = true;
      e.preventDefault();
    } else if (downMods.meta) {
      elm.notify(metaIn.id, e.keyCode);
      e.preventDefault();
    } else if (specialKeys[e.keyCode.toString()]) {
      elm.notify(downsIn.id, e.keyCode)
      e.preventDefault();
    }
  }

  document.onkeypress = function(e) {
    var evt = evt || window.event;
    var charCode = evt.which || evt.keyCode;
    var charTyped = String.fromCharCode(charCode);
    elm.notify(pressesIn.id, charTyped);
    e.preventDefault();
  }

  document.onkeyup = function(e) {
    var mod;
    if (mod = modKeys[e.keyCode.toString()]) {
      downMods[mod] = false;
      e.preventDefault();
    }
  }

  return elm.Native.Keys.values = {
    pressesIn: pressesIn,
    downsIn: downsIn,
    metaIn: metaIn
  };
};
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
   $Dropbox = Elm.Dropbox.make(_elm),
   $Keys = Elm.Keys.make(_elm),
   $Outline$Entry = Elm.Outline.Entry.make(_elm),
   $SampleData = Elm.SampleData.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Window = Elm.Window.make(_elm);
   var initialDocument = $Outline$Entry.textZipper($SampleData.template);
   var dropbox = $Dropbox.client("sy8pzlg66rwnv7n");
   var commands = $Signal.merges(_L.fromArray([A2($Signal._op["<~"],
                                              $App.Key,
                                              $Keys.lastPressed)
                                              ,A2($Signal._op["<~"],
                                              $App.Loaded,
                                              dropbox.read("outlin.json"))]));
   var state = A3($Signal.foldp,
   $App.step,
   initialDocument,
   commands);
   var main = A2($Signal._op["~"],
   A2($Signal._op["<~"],
   $App.render,
   $Window.dimensions),
   state);
   var jsonOutput = $Signal.dropRepeats(A2($Signal._op["<~"],
   function (x) {
      return $Outline$Entry.toJson($Outline$Entry.toValue(x));
   },
   state));
   var toDropbox = A2(dropbox.write,
   "outlin.json",
   jsonOutput);
   _elm.Main.values = {_op: _op
                      ,dropbox: dropbox
                      ,commands: commands
                      ,initialDocument: initialDocument
                      ,state: state
                      ,main: main
                      ,jsonOutput: jsonOutput
                      ,toDropbox: toDropbox};
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
   $Color = Elm.Color.make(_elm),
   $Core$Action = Elm.Core.Action.make(_elm),
   $Core$Array = Elm.Core.Array.make(_elm),
   $Core$String = Elm.Core.String.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Json$Decoder = Elm.Json.Decoder.make(_elm),
   $Json$Output = Elm.Json.Output.make(_elm),
   $Json$Process = Elm.Json.Process.make(_elm),
   $Keys = Elm.Keys.make(_elm),
   $List = Elm.List.make(_elm),
   $Outline$Document = Elm.Outline.Document.make(_elm),
   $Outline$Entry = Elm.Outline.Entry.make(_elm),
   $String = Elm.String.make(_elm),
   $Text = Elm.Text.make(_elm);
   var findFocus = function (z) {
      return function () {
         switch (z.ctor)
         {case "InChild":
            return findFocus($Core$Array.active(z._0.children));}
         return z;
      }();
   };
   var footer = function (_v2) {
      return function () {
         switch (_v2.ctor)
         {case "_Tuple2":
            return $Graphics$Element.color(A3($Color.hsl,
              0,
              0,
              0.8))(A3($Graphics$Element.container,
              _v2._0,
              40,
              $Graphics$Element.midLeft)(A2($Graphics$Element.flow,
              $Graphics$Element.right,
              A2($List.map,
              function (x) {
                 return $Text.asText(x);
              },
              _L.fromArray(["⌘D: delete"
                           ,"⌘M: Missorted"
                           ,"⌘Up/Down: move up/down"])))));}
         _E.Case($moduleName,
         "between lines 138 and 143");
      }();
   };
   var title = F2(function (_v6,
   s) {
      return function () {
         switch (_v6.ctor)
         {case "_Tuple2":
            return $Graphics$Element.color($Color.red)(A3($Graphics$Element.container,
              _v6._0,
              30,
              $Graphics$Element.midLeft)($Text.plainText(s)));}
         _E.Case($moduleName,
         "on line 136, column 17 to 70");
      }();
   });
   var inboxItemV = function (v) {
      return function () {
         switch (v.ctor)
         {case "Entry":
            return $Text.plainText(v._0.text);}
         _E.Case($moduleName,
         "on line 86, column 16 to 61");
      }();
   };
   var hintText = function (s) {
      return $Text.leftAligned($Text.color(A3($Color.hsl,
      0,
      0,
      0.7))($Text.italic($Text.toText(s))));
   };
   var leftPanel$ = F4(function (_v12,
   textElement,
   descriptionElement,
   inboxElements) {
      return function () {
         switch (_v12.ctor)
         {case "_Tuple2":
            return A3($Graphics$Element.container,
              _v12._0,
              _v12._1,
              $Graphics$Element.topLeft)(A2($Graphics$Element.flow,
              $Graphics$Element.down,
              _L.append(_L.fromArray([$Graphics$Element.color($Color.green)(A3($Graphics$Element.container,
                                     _v12._0,
                                     30,
                                     $Graphics$Element.midLeft)(textElement))
                                     ,$Graphics$Element.color($Color.yellow)(A3($Graphics$Element.container,
                                     _v12._0,
                                     30,
                                     $Graphics$Element.midLeft)(descriptionElement))
                                     ,hintText("⌘A: add to inbox")]),
              inboxElements)));}
         _E.Case($moduleName,
         "between lines 89 and 94");
      }();
   });
   var child = F2(function (i,e) {
      return A2($Graphics$Element.flow,
      $Graphics$Element.right,
      _L.fromArray([hintText(_L.append("⌘",
                   _L.append($String.show(i + 1),
                   " ")))
                   ,e]));
   });
   var rightPanel$ = F2(function (_v16,
   childElements) {
      return function () {
         switch (_v16.ctor)
         {case "_Tuple2":
            return A3($Graphics$Element.container,
              _v16._0,
              _v16._1,
              $Graphics$Element.topLeft)(A2($Graphics$Element.flow,
              $Graphics$Element.down,
              _L.append(_L.fromArray([hintText("⌘P: promote from inbox")]),
              A2($List.indexedMap,
              child,
              childElements))));}
         _E.Case($moduleName,
         "between lines 124 and 127");
      }();
   });
   var rightPanel = F2(function (size,
   z) {
      return function () {
         return function () {
            var _v21 = $Outline$Entry.toValue(z);
            switch (_v21.ctor)
            {case "Entry":
               return A2(rightPanel$,
                 size,
                 $List.map($Text.plainText)($List.map(function (en) {
                    return function () {
                       switch (en.ctor)
                       {case "Entry":
                          return en._0.text;}
                       _E.Case($moduleName,
                       "on line 133, column 34 to 68");
                    }();
                 })(_v21._0.children)));}
            _E.Case($moduleName,
            "between lines 131 and 133");
         }();
      }();
   });
   var textCursor = F2(function (fn,
   z) {
      return function () {
         var _v25 = $Core$String.toTuple(z);
         switch (_v25.ctor)
         {case "_Tuple2":
            return A2($Graphics$Element.flow,
              $Graphics$Element.right,
              _L.fromArray([fn(_v25._0)
                           ,$Text.plainText("^")
                           ,fn(_v25._1)]));}
         _E.Case($moduleName,
         "between lines 70 and 75");
      }();
   });
   var inboxItem = function (z) {
      return function () {
         switch (z.ctor)
         {case "InText":
            return A2(textCursor,
              $Text.plainText,
              z._0.text);}
         return $Text.plainText($Outline$Entry.textValue(z));
      }();
   };
   var leftPanel = F2(function (size,
   z) {
      return function () {
         switch (z.ctor)
         {case "InDescription":
            return A4(leftPanel$,
              size,
              $Text.plainText(z._0.text),
              textCursor($Text.plainText)(z._0.description),
              A2($List.map,
              inboxItemV,
              z._0.inbox));
            case "InInbox":
            return A4(leftPanel$,
              size,
              $Text.plainText(z._0.text),
              $Text.plainText(z._0.description),
              A3($Core$Array.map,
              inboxItemV,
              inboxItem,
              z._0.inbox));
            case "InText":
            return A4(leftPanel$,
              size,
              textCursor($Text.plainText)(z._0.text),
              $Text.plainText(z._0.description),
              A2($List.map,
              inboxItemV,
              z._0.inbox));}
         return function () {
            var _v34 = $Outline$Entry.toValue(z);
            switch (_v34.ctor)
            {case "Entry":
               return A4(leftPanel$,
                 size,
                 $Text.plainText(_v34._0.text),
                 $Text.plainText(_v34._0.description),
                 A2($List.map,
                 inboxItemV,
                 _v34._0.inbox));}
            _E.Case($moduleName,
            "between lines 111 and 115");
         }();
      }();
   });
   var render = F2(function (_v36,
   z) {
      return function () {
         switch (_v36.ctor)
         {case "_Tuple2":
            return function () {
                 var focus = findFocus(z);
                 var header = A2(title,
                 {ctor: "_Tuple2"
                 ,_0: _v36._0
                 ,_1: _v36._1},
                 $Outline$Entry.textValue(z));
                 var f = footer({ctor: "_Tuple2"
                                ,_0: _v36._0
                                ,_1: _v36._1});
                 var mh = _v36._1 - $Graphics$Element.heightOf(f) - $Graphics$Element.heightOf(header);
                 return A2($Graphics$Element.flow,
                 $Graphics$Element.down,
                 _L.fromArray([header
                              ,A2($Graphics$Element.flow,
                              $Graphics$Element.right,
                              _L.fromArray([A2(leftPanel,
                                           {ctor: "_Tuple2"
                                           ,_0: $Basics.floor($Basics.toFloat(_v36._0) / 2)
                                           ,_1: mh},
                                           focus)
                                           ,A2(rightPanel,
                                           {ctor: "_Tuple2"
                                           ,_0: $Basics.floor($Basics.toFloat(_v36._0) / 2)
                                           ,_1: mh},
                                           focus)]))
                              ,f]));
              }();}
         _E.Case($moduleName,
         "between lines 152 and 163");
      }();
   });
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
   z) {
      return function () {
         var _v40 = action(z);
         switch (_v40.ctor)
         {case "Delete": return z;
            case "EnterNext": return z;
            case "EnterPrev": return z;
            case "NoChange": return z;
            case "Split": return z;
            case "Update": return _v40._0;}
         _E.Case($moduleName,
         "between lines 24 and 31");
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
                   {case "1":
                      return A2(updateModel,
                        $Outline$Entry.moveInto(0),
                        m);
                      case "2": return A2(updateModel,
                        $Outline$Entry.moveInto(1),
                        m);
                      case "3": return A2(updateModel,
                        $Outline$Entry.moveInto(2),
                        m);
                      case "4": return A2(updateModel,
                        $Outline$Entry.moveInto(3),
                        m);
                      case "5": return A2(updateModel,
                        $Outline$Entry.moveInto(4),
                        m);
                      case "6": return A2(updateModel,
                        $Outline$Entry.moveInto(5),
                        m);
                      case "7": return A2(updateModel,
                        $Outline$Entry.moveInto(6),
                        m);
                      case "Down":
                      return A2(updateModel,
                        $Outline$Entry.moveChildDown,
                        m);
                      case "Up":
                      return A2(updateModel,
                        $Outline$Entry.moveChildUp,
                        m);
                      case "a": return A2(updateModel,
                        $Outline$Entry.addInboxItem,
                        m);
                      case "d": return A2(updateModel,
                        $Outline$Entry.$delete,
                        m);
                      case "m": return A2(updateModel,
                        $Outline$Entry.missort,
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
                 var _v50 = A2($Json$Process.into,
                 $Json$Decoder.fromString(c._0),
                 $Outline$Entry.decoder);
                 switch (_v50.ctor)
                 {case "Success":
                    return $Outline$Entry.textZipper(_v50._0);}
                 return $Basics.fst({ctor: "_Tuple2"
                                    ,_0: m
                                    ,_1: A2($Debug.log,
                                    "Load failed",
                                    _v50)});
              }();}
         return $Basics.fst({ctor: "_Tuple2"
                            ,_0: m
                            ,_1: A2($Debug.log,
                            "Unhandled command",
                            c)});
      }();
   });
   _elm.App.values = {_op: _op
                     ,updateModel: updateModel
                     ,Key: Key
                     ,KeyMeta: KeyMeta
                     ,Loaded: Loaded
                     ,step: step
                     ,textCursor: textCursor
                     ,hintText: hintText
                     ,inboxItem: inboxItem
                     ,inboxItemV: inboxItemV
                     ,leftPanel$: leftPanel$
                     ,leftPanel: leftPanel
                     ,child: child
                     ,rightPanel$: rightPanel$
                     ,rightPanel: rightPanel
                     ,title: title
                     ,footer: footer
                     ,findFocus: findFocus
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
                                       ,inbox: _L.fromArray([$Outline$Entry.Entry({_: {}
                                                                                  ,children: _L.fromArray([])
                                                                                  ,description: ""
                                                                                  ,inbox: _L.fromArray([])
                                                                                  ,text: "sdfs"})
                                                            ,$Outline$Entry.Entry({_: {}
                                                                                  ,children: _L.fromArray([])
                                                                                  ,description: ""
                                                                                  ,inbox: _L.fromArray([])
                                                                                  ,text: "Bacon"})
                                                            ,$Outline$Entry.Entry({_: {}
                                                                                  ,children: _L.fromArray([])
                                                                                  ,description: ""
                                                                                  ,inbox: _L.fromArray([])
                                                                                  ,text: "Freddy"})
                                                            ,$Outline$Entry.Entry({_: {}
                                                                                  ,children: _L.fromArray([])
                                                                                  ,description: ""
                                                                                  ,inbox: _L.fromArray([])
                                                                                  ,text: "Jo-jo"})
                                                            ,$Outline$Entry.Entry({_: {}
                                                                                  ,children: _L.fromArray([])
                                                                                  ,description: ""
                                                                                  ,inbox: _L.fromArray([])
                                                                                  ,text: "Fish"})
                                                            ,$Outline$Entry.Entry({_: {}
                                                                                  ,children: _L.fromArray([])
                                                                                  ,description: ""
                                                                                  ,inbox: _L.fromArray([])
                                                                                  ,text: "Fries"})
                                                            ,$Outline$Entry.Entry({_: {}
                                                                                  ,children: _L.fromArray([])
                                                                                  ,description: ""
                                                                                  ,inbox: _L.fromArray([])
                                                                                  ,text: "Pork"})
                                                            ,$Outline$Entry.Entry({_: {}
                                                                                  ,children: _L.fromArray([])
                                                                                  ,description: ""
                                                                                  ,inbox: _L.fromArray([])
                                                                                  ,text: "Lacey"})
                                                            ,$Outline$Entry.Entry({_: {}
                                                                                  ,children: _L.fromArray([])
                                                                                  ,description: ""
                                                                                  ,inbox: _L.fromArray([])
                                                                                  ,text: "quux"})
                                                            ,$Outline$Entry.Entry({_: {}
                                                                                  ,children: _L.fromArray([])
                                                                                  ,description: ""
                                                                                  ,inbox: _L.fromArray([])
                                                                                  ,text: "sdfsd"})
                                                            ,$Outline$Entry.Entry({_: {}
                                                                                  ,children: _L.fromArray([])
                                                                                  ,description: ""
                                                                                  ,inbox: _L.fromArray([])
                                                                                  ,text: "Billy"})
                                                            ,$Outline$Entry.Entry({_: {}
                                                                                  ,children: _L.fromArray([])
                                                                                  ,description: ""
                                                                                  ,inbox: _L.fromArray([])
                                                                                  ,text: "Camilla"})
                                                            ,$Outline$Entry.Entry({_: {}
                                                                                  ,children: _L.fromArray([])
                                                                                  ,description: ""
                                                                                  ,inbox: _L.fromArray([])
                                                                                  ,text: "baz"})])
                                       ,text: "Tasks (LOADING...)"});
   _elm.SampleData.values = {_op: _op
                            ,template: template};
   return _elm.SampleData.values;
};Elm.Outline = Elm.Outline || {};
Elm.Outline.Document = Elm.Outline.Document || {};
Elm.Outline.Document.make = function (_elm) {
   "use strict";
   _elm.Outline = _elm.Outline || {};
   _elm.Outline.Document = _elm.Outline.Document || {};
   if (_elm.Outline.Document.values)
   return _elm.Outline.Document.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Outline.Document",
   $Outline$Entry = Elm.Outline.Entry.make(_elm);
   _elm.Outline.Document.values = {_op: _op};
   return _elm.Outline.Document.values;
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
   $Json$Decoder = Elm.Json.Decoder.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm);
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
              toJson,
              entry._0.inbox),
              _L.append(",\"children\":",
              _L.append(A2($Core$Array.toJson,
              toJson,
              entry._0.children),
              "}"))))))));}
         _E.Case($moduleName,
         "between lines 307 and 312");
      }();
   };
   var swap = F5(function (ai,
   a,
   bi,
   b,
   list) {
      return $List.indexedMap(F2(function (i,
      x) {
         return _U.eq(i,
         ai) ? b : _U.eq(i,bi) ? a : x;
      }))(list);
   });
   var tryMap2 = F3(function (fn,
   list,
   $default) {
      return function () {
         switch (list.ctor)
         {case "::": return function () {
                 var _v5 = fn(list._0);
                 switch (_v5.ctor)
                 {case "Just":
                    return $Maybe.Just(_v5._0);
                    case "Nothing":
                    return A3(tryMap2,
                      fn,
                      list._1,
                      $default);}
                 _E.Case($moduleName,
                 "between lines 143 and 146");
              }();
            case "[]": return $default;}
         _E.Case($moduleName,
         "between lines 142 and 146");
      }();
   });
   var tryMap = F3(function (fn,
   list,
   $default) {
      return function () {
         switch (list.ctor)
         {case "::": return function () {
                 var _v10 = fn(list._0);
                 switch (_v10.ctor)
                 {case "Just": return _v10._0;
                    case "Nothing":
                    return A3(tryMap,
                      fn,
                      list._1,
                      $default);}
                 _E.Case($moduleName,
                 "between lines 136 and 139");
              }();
            case "[]": return $default;}
         _E.Case($moduleName,
         "between lines 135 and 139");
      }();
   });
   var $try = tryMap($Basics.identity);
   var unwrap = function (en) {
      return function () {
         switch (en.ctor)
         {case "Entry": return en._0;}
         _E.Case($moduleName,
         "on line 104, column 13 to 36");
      }();
   };
   var getInbox = function (en) {
      return function (_) {
         return _.inbox;
      }(unwrap(en));
   };
   var at = F2(function (i,list) {
      return $List.head(A2($List.drop,
      i,
      list));
   });
   var changeAt = F3(function (fn,
   i,
   list) {
      return A2($List.indexedMap,
      F2(function (n,x) {
         return _U.eq(n,
         i) ? fn(x) : x;
      }),
      list);
   });
   var dropAt = F2(function (i,
   list) {
      return _L.append(A2($List.take,
      i,
      list),
      A2($List.drop,i + 1,list));
   });
   var InChild = function (a) {
      return {ctor: "InChild"
             ,_0: a};
   };
   var childZipper = F2(function (fn,
   v) {
      return function () {
         switch (v.ctor)
         {case "Entry":
            return InChild(_U.replace([["children"
                                       ,fn(v._0.children)]],
              v._0));}
         _E.Case($moduleName,
         "on line 50, column 20 to 82");
      }();
   });
   var childZipperAt = F3(function (i,
   fn,
   v) {
      return function () {
         switch (v.ctor)
         {case "Entry":
            return InChild(_U.replace([["children"
                                       ,A3($Core$Array.zipperAt,
                                       i,
                                       fn,
                                       v._0.children)]],
              v._0));}
         _E.Case($moduleName,
         "on line 53, column 24 to 108");
      }();
   });
   var swapChildren = F2(function (fn,
   z) {
      return function () {
         switch (z.ctor)
         {case "InChild":
            return function () {
                 var _v20 = $Core$Array.active(z._0.children);
                 switch (_v20.ctor)
                 {case "InChild":
                    return $Core$Action.NoChange;
                    case "InInbox":
                    return $Core$Action.NoChange;}
                 return function () {
                    var _v23 = fn(z._0.children);
                    switch (_v23.ctor)
                    {case "Just":
                       return $Core$Action.Update(InChild(_U.replace([["children"
                                                                      ,_v23._0]],
                         z._0)));
                       case "Nothing":
                       return $Core$Action.Update(InChild(z._0));}
                    _E.Case($moduleName,
                    "between lines 202 and 205");
                 }();
              }();}
         return $Core$Action.NoChange;
      }();
   });
   var InInbox = function (a) {
      return {ctor: "InInbox"
             ,_0: a};
   };
   var inboxZipper = F2(function (fn,
   v) {
      return function () {
         switch (v.ctor)
         {case "Entry":
            return InInbox(_U.replace([["inbox"
                                       ,fn(v._0.inbox)]],
              v._0));}
         _E.Case($moduleName,
         "on line 56, column 20 to 76");
      }();
   });
   var inboxZipperAt = F3(function (i,
   fn,
   v) {
      return function () {
         switch (v.ctor)
         {case "Entry":
            return InInbox(_U.replace([["inbox"
                                       ,A3($Core$Array.zipperAt,
                                       i,
                                       fn,
                                       v._0.inbox)]],
              v._0));}
         _E.Case($moduleName,
         "on line 59, column 24 to 102");
      }();
   });
   var InDescription = function (a) {
      return {ctor: "InDescription"
             ,_0: a};
   };
   var InText = function (a) {
      return {ctor: "InText"
             ,_0: a};
   };
   var textZipper = function (en) {
      return function () {
         switch (en.ctor)
         {case "Entry":
            return InText(_U.replace([["text"
                                      ,$Core$String.endZipper(en._0.text)]],
              en._0));}
         _E.Case($moduleName,
         "on line 44, column 17 to 90");
      }();
   };
   var firstInboxZipper = function (v) {
      return function () {
         switch (v.ctor)
         {case "Entry":
            return function () {
                 var _v33 = v._0.inbox;
                 switch (_v33.ctor)
                 {case "::":
                    return $Maybe.Just(InInbox(_U.replace([["inbox"
                                                           ,A2($Core$Array.firstZipper,
                                                           textZipper,
                                                           v._0.inbox)]],
                      v._0)));}
                 return $Maybe.Nothing;
              }();}
         _E.Case($moduleName,
         "between lines 108 and 111");
      }();
   };
   var firstChildInboxZipper = function (v) {
      return function () {
         switch (v.ctor)
         {case "Entry":
            return $Maybe.map(function (x) {
                 return InChild(_U.replace([["children"
                                            ,x]],
                 v._0));
              })(A2($Core$Array.firstZipperThat,
              firstInboxZipper,
              v._0.children));}
         _E.Case($moduleName,
         "between lines 130 and 132");
      }();
   };
   var moveToInboxOfFirstChildOrNext = function (en) {
      return function () {
         switch (en.ctor)
         {case "Entry":
            return A3($Maybe.maybe,
              $Core$Action.EnterNext,
              $Core$Action.Update,
              firstChildInboxZipper(en));}
         _E.Case($moduleName,
         "between lines 152 and 153");
      }();
   };
   var lastInboxZipper = function (v) {
      return function () {
         switch (v.ctor)
         {case "Entry":
            return function () {
                 var _v42 = v._0.inbox;
                 switch (_v42.ctor)
                 {case "::":
                    return $Maybe.Just(InInbox(_U.replace([["inbox"
                                                           ,A2($Core$Array.lastZipper,
                                                           textZipper,
                                                           v._0.inbox)]],
                      v._0)));}
                 return $Maybe.Nothing;
              }();}
         _E.Case($moduleName,
         "between lines 114 and 117");
      }();
   };
   var firstChildZipper = function (v) {
      return function () {
         switch (v.ctor)
         {case "Entry":
            return $Maybe.map(function (x) {
                 return InChild(_U.replace([["children"
                                            ,x]],
                 v._0));
              })(A2($Core$Array.firstZipperThat,
              function (x) {
                 return $Maybe.Just(textZipper(x));
              },
              v._0.children));}
         _E.Case($moduleName,
         "between lines 120 and 122");
      }();
   };
   var lastChildZipper = function (v) {
      return function () {
         switch (v.ctor)
         {case "Entry":
            return $Maybe.map(function (x) {
                 return InChild(_U.replace([["children"
                                            ,x]],
                 v._0));
              })(A2($Core$Array.lastZipperThat,
              function (x) {
                 return $Maybe.Just(textZipper(x));
              },
              v._0.children));}
         _E.Case($moduleName,
         "between lines 125 and 127");
      }();
   };
   var textZipperAt = F2(function (i,
   en) {
      return function () {
         switch (en.ctor)
         {case "Entry":
            return InText(_U.replace([["text"
                                      ,A2($Core$String.zipperAt,
                                      i,
                                      en._0.text)]],
              en._0));}
         _E.Case($moduleName,
         "on line 47, column 21 to 95");
      }();
   });
   var findLastCursor = function (en) {
      return function () {
         switch (en.ctor)
         {case "Entry":
            return _U.cmp($List.length(en._0.children),
              0) > 0 ? InChild(_U.replace([["children"
                                           ,A2($Core$Array.lastZipper,
                                           findLastCursor,
                                           en._0.children)]],
              en._0)) : _U.cmp($List.length(en._0.inbox),
              0) > 0 ? InInbox(_U.replace([["inbox"
                                           ,A2($Core$Array.lastZipper,
                                           findLastCursor,
                                           en._0.inbox)]],
              en._0)) : InText(_U.replace([["text"
                                           ,$Core$String.endZipper(en._0.text)]],
              en._0));}
         _E.Case($moduleName,
         "between lines 62 and 66");
      }();
   };
   var Entry = function (a) {
      return {ctor: "Entry",_0: a};
   };
   var emptyEntry = Entry({_: {}
                          ,children: _L.fromArray([])
                          ,description: ""
                          ,inbox: _L.fromArray([])
                          ,text: ""});
   var textEntry = function (t) {
      return Entry({_: {}
                   ,children: _L.fromArray([])
                   ,description: ""
                   ,inbox: _L.fromArray([])
                   ,text: t});
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
   var toValue = function (z) {
      return function () {
         switch (z.ctor)
         {case "InChild":
            return Entry(_U.replace([["children"
                                     ,A2($Core$Array.toValue,
                                     toValue,
                                     z._0.children)]],
              z._0));
            case "InDescription":
            return Entry(_U.replace([["description"
                                     ,$Core$String.toValue(z._0.description)]],
              z._0));
            case "InInbox":
            return Entry(_U.replace([["inbox"
                                     ,A2($Core$Array.toValue,
                                     toValue,
                                     z._0.inbox)]],
              z._0));
            case "InText":
            return Entry(_U.replace([["text"
                                     ,$Core$String.toValue(z._0.text)]],
              z._0));}
         _E.Case($moduleName,
         "between lines 33 and 37");
      }();
   };
   var textValue = function (z) {
      return function () {
         var _v58 = toValue(z);
         switch (_v58.ctor)
         {case "Entry":
            return _v58._0.text;}
         _E.Case($moduleName,
         "between lines 40 and 41");
      }();
   };
   var addInboxItem_ = F2(function (newEntry,
   z) {
      return function () {
         switch (z.ctor)
         {case "InInbox":
            return $Core$Action.Update(InInbox(_U.replace([["inbox"
                                                           ,A2($Core$Array.firstZipper,
                                                           textZipper,
                                                           A2($List._op["::"],
                                                           newEntry,
                                                           A2($Core$Array.toValue,
                                                           toValue,
                                                           z._0.inbox)))]],
              z._0)));
            case "InText":
            return $Core$Action.Update(InInbox(_U.replace([["text"
                                                           ,$Core$String.toValue(z._0.text)]
                                                          ,["inbox"
                                                           ,A2($Core$Array.firstZipper,
                                                           textZipper,
                                                           A2($List._op["::"],
                                                           newEntry,
                                                           z._0.inbox))]],
              z._0)));}
         return $Core$Action.NoChange;
      }();
   });
   var promote_ = function (z) {
      return function () {
         switch (z.ctor)
         {case "InInbox":
            return function () {
                 var i = $Core$Array.active(z._0.inbox);
                 return function () {
                    var _v65 = A2($Core$Array.remove,
                    textZipper,
                    z._0.inbox);
                    switch (_v65.ctor)
                    {case "Just":
                       return $Core$Action.Update(InInbox(_U.replace([["inbox"
                                                                      ,_v65._0]
                                                                     ,["children"
                                                                      ,A2($List._op["::"],
                                                                      toValue(i),
                                                                      z._0.children)]],
                         z._0)));
                       case "Nothing":
                       return $Core$Action.Update(InChild(_U.replace([["inbox"
                                                                      ,_L.fromArray([])]
                                                                     ,["children"
                                                                      ,A3($Core$Array.zipper,
                                                                      _L.fromArray([]),
                                                                      i,
                                                                      z._0.children)]],
                         z._0)));}
                    _E.Case($moduleName,
                    "between lines 91 and 100");
                 }();
              }();}
         return $Core$Action.NoChange;
      }();
   };
   var doEntry = F2(function (action,
   z) {
      return function () {
         switch (z.ctor)
         {case "InChild":
            return function () {
                 var _v69 = action(z);
                 switch (_v69.ctor)
                 {case "NoChange":
                    return function () {
                         var _v70 = A5($Core$Array.$do,
                         toValue,
                         textZipper,
                         findLastCursor,
                         doEntry(action),
                         z._0.children);
                         switch (_v70.ctor)
                         {case "NoChange":
                            return $Core$Action.NoChange;
                            case "Update":
                            return $Core$Action.Update(InChild(_U.replace([["children"
                                                                           ,_v70._0]],
                              z._0)));}
                         _E.Case($moduleName,
                         "between lines 213 and 216");
                      }();}
                 return _v69;
              }();}
         return action(z);
      }();
   });
   var addInboxItem = doEntry(addInboxItem_(emptyEntry));
   var promote = doEntry(promote_);
   var moveChildUp = doEntry(swapChildren($Core$Array.moveUp));
   var moveChildDown = doEntry(swapChildren($Core$Array.moveDown));
   var $do = F2(function (stringAction,
   z) {
      return function () {
         switch (z.ctor)
         {case "InChild":
            return function () {
                 var _v77 = A5($Core$Array.$do,
                 toValue,
                 textZipper,
                 findLastCursor,
                 $do(stringAction),
                 z._0.children);
                 switch (_v77.ctor)
                 {case "Delete":
                    return $Core$Action.Update(InText(_U.replace([["children"
                                                                  ,_L.fromArray([])]
                                                                 ,["text"
                                                                  ,$Core$String.endZipper(z._0.text)]],
                      z._0)));
                    case "EnterNext":
                    return $Core$Action.EnterNext;
                    case "EnterPrev":
                    return A2($try,
                      _L.fromArray([$Maybe.map($Core$Action.Update)(lastInboxZipper(toValue(z)))]),
                      $Core$Action.Update(textZipper(toValue(z))));
                    case "NoChange":
                    return $Core$Action.NoChange;
                    case "Update":
                    return $Core$Action.Update(InChild(_U.replace([["children"
                                                                   ,_v77._0]],
                      z._0)));}
                 _E.Case($moduleName,
                 "between lines 243 and 250");
              }();
            case "InDescription":
            return function () {
                 var _v79 = stringAction(z._0.description);
                 switch (_v79.ctor)
                 {case "Delete":
                    return $Core$Action.Delete;
                    case "NoChange":
                    return $Core$Action.NoChange;
                    case "Update":
                    return $Core$Action.Update(InDescription(_U.replace([["description"
                                                                         ,_v79._0]],
                      z._0)));}
                 _E.Case($moduleName,
                 "between lines 231 and 235");
              }();
            case "InInbox":
            return function () {
                 var _v81 = A5($Core$Array.$do,
                 toValue,
                 textZipper,
                 textZipper,
                 $do(stringAction),
                 z._0.inbox);
                 switch (_v81.ctor)
                 {case "Delete":
                    return $Core$Action.Update(InText(_U.replace([["inbox"
                                                                  ,_L.fromArray([])]
                                                                 ,["text"
                                                                  ,$Core$String.endZipper(z._0.text)]],
                      z._0)));
                    case "EnterNext":
                    return A2($try,
                      _L.fromArray([$Maybe.map($Core$Action.Update)(firstChildZipper(toValue(z)))]),
                      $Core$Action.EnterNext);
                    case "EnterPrev":
                    return $Core$Action.Update(textZipper(toValue(z)));
                    case "NoChange":
                    return $Core$Action.NoChange;
                    case "Update":
                    return $Core$Action.Update(InInbox(_U.replace([["inbox"
                                                                   ,_v81._0]],
                      z._0)));}
                 _E.Case($moduleName,
                 "between lines 235 and 243");
              }();
            case "InText":
            return function () {
                 var _v83 = stringAction(z._0.text);
                 switch (_v83.ctor)
                 {case "Delete":
                    return $Core$Action.Delete;
                    case "EnterNext":
                    return A2($try,
                      _L.fromArray([$Maybe.map($Core$Action.Update)(firstInboxZipper(toValue(z)))
                                   ,$Maybe.map($Core$Action.Update)(firstChildZipper(toValue(z)))]),
                      $Core$Action.EnterNext);
                    case "EnterPrev":
                    return $Core$Action.EnterPrev;
                    case "NoChange":
                    return $Core$Action.NoChange;
                    case "Split":
                    return A3($Core$Action.Split,
                      A2($List.map,textEntry,_v83._0),
                      InText(_U.replace([["text"
                                         ,_v83._1]],
                      z._0)),
                      A2($List.map,
                      textEntry,
                      _v83._2));
                    case "Update":
                    return $Core$Action.Update(InText(_U.replace([["text"
                                                                  ,_v83._0]],
                      z._0)));}
                 _E.Case($moduleName,
                 "between lines 221 and 231");
              }();}
         _E.Case($moduleName,
         "between lines 220 and 250");
      }();
   });
   var goLeft = $do($Core$String.goLeft);
   var goRight = $do($Core$String.goRight);
   var backspace = $do($Core$String.backspace);
   var $delete = $do($Core$String.$delete);
   var insert = function (s) {
      return $do($Core$String.insert(s));
   };
   var enter = $do($Core$String.split);
   var goNext = $do($Core$Action.always($Core$Action.EnterNext));
   var goPrev = $do($Core$Action.always($Core$Action.EnterPrev));
   var addInboxItem__ = F2(function (s,
   en) {
      return function () {
         switch (en.ctor)
         {case "Entry":
            return Entry(_U.replace([["inbox"
                                     ,A2($List._op["::"],
                                     s,
                                     en._0.inbox)]],
              en._0));}
         _E.Case($moduleName,
         "on line 69, column 23 to 80");
      }();
   });
   var appendToInboxOfChild = F3(function (n,
   v,
   children) {
      return A3($Core$Array.mapAt,
      n,
      function (_v90) {
         return function () {
            switch (_v90.ctor)
            {case "Entry":
               return Entry(_U.replace([["inbox"
                                        ,A2($Core$Array.prepend,
                                        v,
                                        _v90._0.inbox)]],
                 _v90._0));}
            _E.Case($moduleName,
            "on line 156, column 71 to 120");
         }();
      },
      children);
   });
   var moveInto_ = F2(function (n,
   z) {
      return function () {
         switch (z.ctor)
         {case "InInbox":
            return _U.cmp(n,
              $List.length(z._0.children)) > -1 ? $Core$Action.NoChange : function () {
                 var _v95 = A2($Core$Array.remove,
                 textZipper,
                 z._0.inbox);
                 switch (_v95.ctor)
                 {case "Just":
                    return $Core$Action.Update(InInbox(_U.replace([["inbox"
                                                                   ,_v95._0]
                                                                  ,["children"
                                                                   ,A3(appendToInboxOfChild,
                                                                   n,
                                                                   toValue($Core$Array.active(z._0.inbox)),
                                                                   z._0.children)]],
                      z._0)));
                    case "Nothing":
                    return moveToInboxOfFirstChildOrNext(Entry(_U.replace([["inbox"
                                                                           ,_L.fromArray([])]
                                                                          ,["children"
                                                                           ,A3(appendToInboxOfChild,
                                                                           n,
                                                                           toValue($Core$Array.active(z._0.inbox)),
                                                                           z._0.children)]],
                      z._0)));}
                 _E.Case($moduleName,
                 "between lines 162 and 165");
              }();}
         return $Core$Action.NoChange;
      }();
   });
   var moveInto = function (n) {
      return doEntry(moveInto_(n));
   };
   var missort_ = function (z) {
      return function () {
         switch (z.ctor)
         {case "InChild":
            return function () {
                 var _v99 = $Core$Array.active(z._0.children);
                 switch (_v99.ctor)
                 {case "InChild":
                    return function () {
                         var _v102 = $Core$Array.active(_v99._0.children);
                         switch (_v102.ctor)
                         {case "InChild":
                            return $Core$Action.NoChange;
                            case "InInbox":
                            return $Core$Action.NoChange;}
                         return function () {
                            var item = toValue($Core$Array.active(_v99._0.children));
                            var withNewInbox = _U.replace([["inbox"
                                                           ,A2($Core$Array.prepend,
                                                           item,
                                                           z._0.inbox)]],
                            z._0);
                            return function () {
                               var _v105 = A2($Core$Array.remove,
                               textZipper,
                               _v99._0.children);
                               switch (_v105.ctor)
                               {case "Just":
                                  return $Core$Action.Update(InChild(_U.replace([["children"
                                                                                 ,A2($Core$Array.update,
                                                                                 InChild(_U.replace([["children"
                                                                                                     ,_v105._0]],
                                                                                 _v99._0)),
                                                                                 z._0.children)]],
                                    withNewInbox)));
                                  case "Nothing":
                                  return $Core$Action.Update(A2(inboxZipper,
                                    $Core$Array.firstZipper(textZipper),
                                    Entry(_U.replace([["children"
                                                      ,A2($Core$Array.toValue,
                                                      function (z) {
                                                         return function () {
                                                            switch (z.ctor)
                                                            {case "InChild":
                                                               return Entry(_U.replace([["children"
                                                                                        ,_L.fromArray([])]],
                                                                 z._0));}
                                                            _E.Case($moduleName,
                                                            "on line 184, column 149 to 204");
                                                         }();
                                                      },
                                                      z._0.children)]],
                                    withNewInbox))));}
                               _E.Case($moduleName,
                               "between lines 182 and 185");
                            }();
                         }();
                      }();
                    case "InInbox":
                    return function () {
                         var item = toValue($Core$Array.active(_v99._0.inbox));
                         var withNewInbox = _U.replace([["inbox"
                                                        ,A2($Core$Array.prepend,
                                                        item,
                                                        z._0.inbox)]],
                         z._0);
                         return function () {
                            var _v109 = A2($Core$Array.remove,
                            textZipper,
                            _v99._0.inbox);
                            switch (_v109.ctor)
                            {case "Just":
                               return $Core$Action.Update(InChild(_U.replace([["children"
                                                                              ,A2($Core$Array.update,
                                                                              InInbox(_U.replace([["inbox"
                                                                                                  ,_v109._0]],
                                                                              _v99._0)),
                                                                              z._0.children)]],
                                 withNewInbox)));
                               case "Nothing":
                               return $Core$Action.Update(A2(inboxZipper,
                                 $Core$Array.firstZipper(textZipper),
                                 Entry(_U.replace([["children"
                                                   ,A2($Core$Array.toValue,
                                                   function (z) {
                                                      return function () {
                                                         switch (z.ctor)
                                                         {case "InInbox":
                                                            return Entry(_U.replace([["inbox"
                                                                                     ,_L.fromArray([])]],
                                                              z._0));}
                                                         _E.Case($moduleName,
                                                         "on line 176, column 147 to 199");
                                                      }();
                                                   },
                                                   z._0.children)]],
                                 withNewInbox))));}
                            _E.Case($moduleName,
                            "between lines 174 and 177");
                         }();
                      }();}
                 return $Core$Action.NoChange;
              }();}
         return $Core$Action.NoChange;
      }();
   };
   var missort = doEntry(missort_);
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
      $Json$Decoder.listOf(decoder)),
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
                               ,insert: insert
                               ,backspace: backspace
                               ,enter: enter
                               ,addInboxItem: addInboxItem
                               ,promote: promote
                               ,moveInto: moveInto
                               ,missort: missort
                               ,moveChildUp: moveChildUp
                               ,moveChildDown: moveChildDown
                               ,$delete: $delete
                               ,goLeft: goLeft
                               ,goRight: goRight
                               ,goNext: goNext
                               ,goPrev: goPrev
                               ,decoder: decoder
                               ,toJson: toJson
                               ,emptyEntry: emptyEntry
                               ,entry: entry
                               ,childZipper: childZipper
                               ,textZipper: textZipper
                               ,inboxZipper: inboxZipper
                               ,toValue: toValue
                               ,textZipperAt: textZipperAt
                               ,childZipperAt: childZipperAt
                               ,inboxZipperAt: inboxZipperAt
                               ,textValue: textValue
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
   var walk = function (fn) {
      return fn;
   };
   var toJson = walk(function (s) {
      return _L.append("\"",
      _L.append(quote(s),"\""));
   });
   var renderZipper = function (_v4) {
      return function () {
         switch (_v4.ctor)
         {case "_Tuple2":
            return A3($Html.node,
              "span",
              _L.fromArray([]),
              _L.fromArray([$Html.text(A2($String.left,
                           _v4._1,
                           _v4._0))
                           ,A3($Html.node,
                           "span",
                           _L.fromArray([$Html$Attributes.$class("cursor")]),
                           _L.fromArray([$Html.text("^")]))
                           ,$Html.text(A2($String.dropLeft,
                           _v4._1,
                           _v4._0))]));}
         _E.Case($moduleName,
         "between lines 62 and 65");
      }();
   };
   var renderValue = function (value) {
      return $Html.text(value);
   };
   var split = function (_v8) {
      return function () {
         switch (_v8.ctor)
         {case "_Tuple2":
            return A3($Core$Action.Split,
              _L.fromArray([A2($String.left,
              _v8._1,
              _v8._0)]),
              {ctor: "_Tuple2"
              ,_0: A2($String.dropLeft,
              _v8._1,
              _v8._0)
              ,_1: 0},
              _L.fromArray([]));}
         _E.Case($moduleName,
         "on line 56, column 15 to 73");
      }();
   };
   var $delete = $Core$Action.always($Core$Action.Delete);
   var goRight = $Core$Action.nav(F2(function (v,
   c) {
      return A2($Basics.min,
      $String.length(v),
      c + 1);
   }));
   var goLeft = $Core$Action.nav(F2(function (_v12,
   c) {
      return function () {
         return _U.cmp(c,
         0) > 0 ? c - 1 : c;
      }();
   }));
   var backspace = function (_v14) {
      return function () {
         switch (_v14.ctor)
         {case "_Tuple2":
            return function () {
                 var _v18 = {ctor: "_Tuple2"
                            ,_0: _v14._0
                            ,_1: _v14._1};
                 switch (_v18.ctor)
                 {case "_Tuple2":
                    switch (_v18._1)
                      {case 0:
                         return $Core$Action.NoChange;}
                      break;}
                 return $Core$Action.Update({ctor: "_Tuple2"
                                            ,_0: _L.append(A2($String.left,
                                            _v14._1 - 1,
                                            _v14._0),
                                            A2($String.dropLeft,
                                            _v14._1,
                                            _v14._0))
                                            ,_1: _v14._1 - 1});
              }();}
         _E.Case($moduleName,
         "between lines 46 and 48");
      }();
   };
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
   var insert = F2(function (s,
   _v21) {
      return function () {
         switch (_v21.ctor)
         {case "_Tuple2":
            return $Core$Action.Update({ctor: "_Tuple2"
                                       ,_0: A3(update,
                                       s,
                                       _v21._0,
                                       _v21._1)
                                       ,_1: A3(move,
                                       s,
                                       _v21._0,
                                       _v21._1)});}
         _E.Case($moduleName,
         "on line 43, column 18 to 60");
      }();
   });
   var toTuple = function (_v25) {
      return function () {
         switch (_v25.ctor)
         {case "_Tuple2":
            return {ctor: "_Tuple2"
                   ,_0: A2($String.left,
                   _v25._1,
                   _v25._0)
                   ,_1: A2($String.dropLeft,
                   _v25._1,
                   _v25._0)};}
         _E.Case($moduleName,
         "on line 29, column 18 to 54");
      }();
   };
   var toValue = function (_v29) {
      return function () {
         switch (_v29.ctor)
         {case "_Tuple2":
            return _v29._0;}
         _E.Case($moduleName,
         "on line 26, column 17 to 18");
      }();
   };
   var zipperAt = F2(function (i,
   s) {
      return {ctor: "_Tuple2"
             ,_0: s
             ,_1: i};
   });
   var zipper = F2(function (left,
   right) {
      return {ctor: "_Tuple2"
             ,_0: _L.append(left,right)
             ,_1: $String.length(left)};
   });
   var endZipper = function (v) {
      return {ctor: "_Tuple2"
             ,_0: v
             ,_1: $String.length(v)};
   };
   var startZipper = function (v) {
      return {ctor: "_Tuple2"
             ,_0: v
             ,_1: 0};
   };
   var split_ = function (_v33) {
      return function () {
         switch (_v33.ctor)
         {case "_Tuple2":
            return {ctor: "_Tuple2"
                   ,_0: A2($String.left,
                   _v33._1,
                   _v33._0)
                   ,_1: startZipper(A2($String.dropLeft,
                   _v33._1,
                   _v33._0))};}
         _E.Case($moduleName,
         "on line 32, column 17 to 67");
      }();
   };
   _elm.Core.String.values = {_op: _op
                             ,insert: insert
                             ,backspace: backspace
                             ,goLeft: goLeft
                             ,goRight: goRight
                             ,$delete: $delete
                             ,split: split
                             ,renderValue: renderValue
                             ,renderZipper: renderZipper
                             ,toJson: toJson
                             ,startZipper: startZipper
                             ,endZipper: endZipper
                             ,toValue: toValue
                             ,split_: split_
                             ,zipper: zipper
                             ,zipperAt: zipperAt
                             ,toTuple: toTuple};
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
   child,
   list) {
      return wrapFn(A2($List.map,
      child,
      list));
   });
   var toJson = function (fn) {
      return A2(walk,
      function (vs) {
         return _L.append("[",
         _L.append(A2($List.join,",",vs),
         "]"));
      },
      fn);
   };
   var map = F3(function (valueFn,
   zipperFn,
   _v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple3":
            return _L.append(A2($List.map,
              valueFn,
              $List.reverse(_v0._0)),
              _L.append(_L.fromArray([zipperFn(_v0._1)]),
              A2($List.map,valueFn,_v0._2)));}
         _E.Case($moduleName,
         "between lines 111 and 113");
      }();
   });
   var update = F2(function ($new,
   _v5) {
      return function () {
         switch (_v5.ctor)
         {case "_Tuple3":
            return {ctor: "_Tuple3"
                   ,_0: _v5._0
                   ,_1: $new
                   ,_2: _v5._2};}
         _E.Case($moduleName,
         "on line 107, column 30 to 44");
      }();
   });
   var moveDown = function (_v10) {
      return function () {
         switch (_v10.ctor)
         {case "_Tuple3":
            return function () {
                 switch (_v10._2.ctor)
                 {case "::":
                    return $Maybe.Just({ctor: "_Tuple3"
                                       ,_0: A2($List._op["::"],
                                       _v10._2._0,
                                       _v10._0)
                                       ,_1: _v10._1
                                       ,_2: _v10._2._1});
                    case "[]":
                    return $Maybe.Nothing;}
                 _E.Case($moduleName,
                 "between lines 102 and 104");
              }();}
         _E.Case($moduleName,
         "between lines 102 and 104");
      }();
   };
   var moveUp = function (_v18) {
      return function () {
         switch (_v18.ctor)
         {case "_Tuple3":
            return function () {
                 switch (_v18._0.ctor)
                 {case "::":
                    return $Maybe.Just({ctor: "_Tuple3"
                                       ,_0: _v18._0._1
                                       ,_1: _v18._1
                                       ,_2: A2($List._op["::"],
                                       _v18._0._0,
                                       _v18._2)});
                    case "[]":
                    return $Maybe.Nothing;}
                 _E.Case($moduleName,
                 "between lines 97 and 99");
              }();}
         _E.Case($moduleName,
         "between lines 97 and 99");
      }();
   };
   var split_ = F2(function (fn,
   z) {
      return function () {
         var _v26 = fn(z);
         switch (_v26.ctor)
         {case "_Tuple2":
            return A3($Core$Action.Split,
              _L.fromArray([_v26._0]),
              _v26._1,
              _L.fromArray([]));}
         _E.Case($moduleName,
         "between lines 90 and 91");
      }();
   });
   var $do = F5(function (toVal,
   nextFn,
   prevFn,
   action,
   _v29) {
      return function () {
         switch (_v29.ctor)
         {case "_Tuple3":
            return function () {
                 var _v34 = action(_v29._1);
                 switch (_v34.ctor)
                 {case "Delete":
                    return function () {
                         switch (_v29._2.ctor)
                         {case "::":
                            return $Core$Action.Update({ctor: "_Tuple3"
                                                       ,_0: _v29._0
                                                       ,_1: nextFn(_v29._2._0)
                                                       ,_2: _v29._2._1});
                            case "[]": return function () {
                                 switch (_v29._0.ctor)
                                 {case "::":
                                    return $Core$Action.Update({ctor: "_Tuple3"
                                                               ,_0: _v29._0._1
                                                               ,_1: prevFn(_v29._0._0)
                                                               ,_2: _v29._2});
                                    case "[]":
                                    return $Core$Action.Delete;}
                                 _E.Case($moduleName,
                                 "between lines 78 and 81");
                              }();}
                         _E.Case($moduleName,
                         "between lines 76 and 81");
                      }();
                    case "EnterNext":
                    return function () {
                         switch (_v29._2.ctor)
                         {case "::":
                            return $Core$Action.Update({ctor: "_Tuple3"
                                                       ,_0: A2($List._op["::"],
                                                       toVal(_v29._1),
                                                       _v29._0)
                                                       ,_1: nextFn(_v29._2._0)
                                                       ,_2: _v29._2._1});
                            case "[]":
                            return $Core$Action.EnterNext;}
                         _E.Case($moduleName,
                         "between lines 81 and 84");
                      }();
                    case "EnterPrev":
                    return function () {
                         switch (_v29._0.ctor)
                         {case "::":
                            return $Core$Action.Update({ctor: "_Tuple3"
                                                       ,_0: _v29._0._1
                                                       ,_1: prevFn(_v29._0._0)
                                                       ,_2: A2($List._op["::"],
                                                       toVal(_v29._1),
                                                       _v29._2)});
                            case "[]":
                            return $Core$Action.EnterPrev;}
                         _E.Case($moduleName,
                         "between lines 84 and 87");
                      }();
                    case "NoChange":
                    return $Core$Action.NoChange;
                    case "Split":
                    return $Core$Action.Update({ctor: "_Tuple3"
                                               ,_0: _L.append($List.reverse(_v34._0),
                                               _v29._0)
                                               ,_1: _v34._1
                                               ,_2: _L.append(_v34._2,
                                               _v29._2)});
                    case "Update":
                    return $Core$Action.Update({ctor: "_Tuple3"
                                               ,_0: _v29._0
                                               ,_1: _v34._0
                                               ,_2: _v29._2});}
                 _E.Case($moduleName,
                 "between lines 73 and 87");
              }();}
         _E.Case($moduleName,
         "between lines 73 and 87");
      }();
   });
   var split = F4(function (toVal,
   nextCursor,
   prevCursor,
   fn) {
      return A4($do,
      toVal,
      nextCursor,
      prevCursor,
      split_(fn));
   });
   var remove = F2(function (fn,
   _v51) {
      return function () {
         switch (_v51.ctor)
         {case "_Tuple3":
            return function () {
                 switch (_v51._2.ctor)
                 {case "::":
                    return $Maybe.Just({ctor: "_Tuple3"
                                       ,_0: _v51._0
                                       ,_1: fn(_v51._2._0)
                                       ,_2: _v51._2._1});
                    case "[]": return function () {
                         switch (_v51._0.ctor)
                         {case "::":
                            return $Maybe.Just({ctor: "_Tuple3"
                                               ,_0: _v51._0._1
                                               ,_1: fn(_v51._0._0)
                                               ,_2: _v51._2});
                            case "[]":
                            return $Maybe.Nothing;}
                         _E.Case($moduleName,
                         "between lines 67 and 69");
                      }();}
                 _E.Case($moduleName,
                 "between lines 65 and 69");
              }();}
         _E.Case($moduleName,
         "between lines 65 and 69");
      }();
   });
   var lastZipper = F2(function (fn,
   list) {
      return function () {
         var _raw = $List.reverse(list),
         $ = _raw.ctor === "::" ? _raw : _E.Case($moduleName,
         "on line 56, column 42 to 54"),
         cur = $._0,
         tail = $._1;
         return {ctor: "_Tuple3"
                ,_0: tail
                ,_1: fn(cur)
                ,_2: _L.fromArray([])};
      }();
   });
   var firstZipperThat = F2(function (fn,
   vs) {
      return function () {
         switch (vs.ctor)
         {case "::": return function () {
                 var _v65 = fn(vs._0);
                 switch (_v65.ctor)
                 {case "Just":
                    return $Maybe.Just({ctor: "_Tuple3"
                                       ,_0: _L.fromArray([])
                                       ,_1: _v65._0
                                       ,_2: vs._1});
                    case "Nothing":
                    return function () {
                         var _v67 = A2(firstZipperThat,
                         fn,
                         vs._1);
                         switch (_v67.ctor)
                         {case "Just":
                            switch (_v67._0.ctor)
                              {case "_Tuple3":
                                 return $Maybe.Just({ctor: "_Tuple3"
                                                    ,_0: A2($List._op["::"],
                                                    vs._0,
                                                    _v67._0._0)
                                                    ,_1: _v67._0._1
                                                    ,_2: _v67._0._2});}
                              break;
                            case "Nothing":
                            return $Maybe.Nothing;}
                         _E.Case($moduleName,
                         "between lines 50 and 53");
                      }();}
                 _E.Case($moduleName,
                 "between lines 48 and 53");
              }();
            case "[]":
            return $Maybe.Nothing;}
         _E.Case($moduleName,
         "between lines 47 and 53");
      }();
   });
   var lastZipperThat = F2(function (fn,
   vs) {
      return function () {
         var _v72 = A2(firstZipperThat,
         fn,
         $List.reverse(vs));
         switch (_v72.ctor)
         {case "Just":
            switch (_v72._0.ctor)
              {case "_Tuple3":
                 return $Maybe.Just({ctor: "_Tuple3"
                                    ,_0: _v72._0._2
                                    ,_1: _v72._0._1
                                    ,_2: _v72._0._0});}
              break;
            case "Nothing":
            return $Maybe.Nothing;}
         _E.Case($moduleName,
         "between lines 59 and 61");
      }();
   });
   var firstZipper = F2(function (fn,
   _v77) {
      return function () {
         switch (_v77.ctor)
         {case "::":
            return {ctor: "_Tuple3"
                   ,_0: _L.fromArray([])
                   ,_1: fn(_v77._0)
                   ,_2: _v77._1};}
         _E.Case($moduleName,
         "on line 44, column 33 to 47");
      }();
   });
   var zipperAt = F3(function (i,
   fn,
   vs) {
      return {ctor: "_Tuple3"
             ,_0: $List.reverse($List.take(i)(vs))
             ,_1: fn($List.head($List.drop(i)(vs)))
             ,_2: $List.drop(i + 1)(vs)};
   });
   var zipper = F3(function (left,
   cur,
   right) {
      return {ctor: "_Tuple3"
             ,_0: left
             ,_1: cur
             ,_2: right};
   });
   var active = function (_v81) {
      return function () {
         switch (_v81.ctor)
         {case "_Tuple3":
            return _v81._1;}
         _E.Case($moduleName,
         "on line 30, column 18 to 19");
      }();
   };
   var prepend = F2(function (v,
   vs) {
      return A2($List._op["::"],
      v,
      vs);
   });
   var append = F2(function (v,
   vs) {
      return _L.append(vs,
      _L.fromArray([v]));
   });
   var mapAt = F3(function (n,
   fn,
   vs) {
      return A2($List.indexedMap,
      F2(function (i,v) {
         return _U.eq(i,
         n) ? fn(v) : v;
      }),
      vs);
   });
   var toValue = F2(function (fn,
   _v86) {
      return function () {
         switch (_v86.ctor)
         {case "_Tuple3":
            return _L.append($List.reverse(_v86._0),
              _L.append(_L.fromArray([fn(_v86._1)]),
              _v86._2));}
         _E.Case($moduleName,
         "on line 18, column 31 to 64");
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
   _elm.Core.Array.values = {_op: _op
                            ,toValue: toValue
                            ,$do: $do
                            ,split: split
                            ,toJson: toJson
                            ,firstZipper: firstZipper
                            ,lastZipper: lastZipper
                            ,remove: remove
                            ,map: map
                            ,active: active
                            ,zipper: zipper
                            ,append: append
                            ,prepend: prepend
                            ,mapAt: mapAt
                            ,firstZipperThat: firstZipperThat
                            ,lastZipperThat: lastZipperThat
                            ,zipperAt: zipperAt
                            ,moveUp: moveUp
                            ,moveDown: moveDown
                            ,update: update};
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
   var always = F2(function (r,
   _v0) {
      return function () {
         return r;
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
   var Update = function (a) {
      return {ctor: "Update"
             ,_0: a};
   };
   var nav = F2(function (fn,_v2) {
      return function () {
         switch (_v2.ctor)
         {case "_Tuple2":
            return Update({ctor: "_Tuple2"
                          ,_0: _v2._0
                          ,_1: A2(fn,_v2._0,_v2._1)});}
         _E.Case($moduleName,
         "on line 11, column 20 to 39");
      }();
   });
   var change = F2(function (fn,
   _v6) {
      return function () {
         switch (_v6.ctor)
         {case "_Tuple2":
            return Update({ctor: "_Tuple2"
                          ,_0: A2(fn,_v6._0,_v6._1)
                          ,_1: _v6._1});}
         _E.Case($moduleName,
         "on line 14, column 23 to 42");
      }();
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
};Elm.Dropbox = Elm.Dropbox || {};
Elm.Dropbox.make = function (_elm) {
   "use strict";
   _elm.Dropbox = _elm.Dropbox || {};
   if (_elm.Dropbox.values)
   return _elm.Dropbox.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Dropbox",
   $Native$Dropbox = Elm.Native.Dropbox.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var client = $Native$Dropbox.client;
   var Client = F2(function (a,b) {
      return {_: {}
             ,read: a
             ,write: b};
   });
   _elm.Dropbox.values = {_op: _op
                         ,client: client};
   return _elm.Dropbox.values;
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
   $Native$Keys = Elm.Native.Keys.make(_elm),
   $Signal = Elm.Signal.make(_elm),
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
         {case 37:
            return Command("Left");
            case 38: return Command("Up");
            case 39:
            return Command("Right");
            case 40: return Command("Down");
            case 49: return Command("1");
            case 50: return Command("2");
            case 51: return Command("3");
            case 52: return Command("4");
            case 53: return Command("5");
            case 54: return Command("6");
            case 55: return Command("7");
            case 65: return Command("a");
            case 68: return Command("d");
            case 77: return Command("m");
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
   var lastPressed = $Signal.merges(_L.fromArray([A2($Signal._op["<~"],
                                                 fromPresses,
                                                 $Native$Keys.pressesIn)
                                                 ,A2($Signal._op["<~"],
                                                 fromDowns,
                                                 $Native$Keys.downsIn)
                                                 ,A2($Signal._op["<~"],
                                                 fromMeta,
                                                 $Native$Keys.metaIn)]));
   _elm.Keys.values = {_op: _op
                      ,lastPressed: lastPressed
                      ,Enter: Enter
                      ,Backspace: Backspace
                      ,Left: Left
                      ,Right: Right
                      ,Up: Up
                      ,Down: Down
                      ,Character: Character
                      ,Command: Command
                      ,Unrecognized: Unrecognized};
   return _elm.Keys.values;
};