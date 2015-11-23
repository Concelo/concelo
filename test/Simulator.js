"use strict";

// module Test.Simulator

exports.trace = function (x) {
    return function (f) {
        Error.stackTraceLimit = Infinity;
        console.trace(x);
        return f({});
    };
};

exports.log = function (x) {
    return function (f) {
        console.log(x);
        return f({});
    };
};
