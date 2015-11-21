// module Concelo.Tree

"use strict";

var crypto = require("crypto");
var List = require("Data.List");
var Maybe = require("Data.Maybe");

exports.hashStrings = function (strings) {
    var hash = crypto.createHash("sha256");
    for (; ! List.null(strings);
         strings = Maybe.fromMaybe(List.Nil.value)(List.tail(strings)))
    {
        hash.update(Maybe.fromMaybe("")(List.head(strings)), "utf8");
    }
    return hash.digest("hex");
}

exports.hexToInt = function (string) {
    return parseInt(string, 16);
}

exports.intToHex = function (n) {
    return n.toString(16);
}
