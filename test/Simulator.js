"use strict";

// module Test.Simulator

exports.trace = function (x) {
    return function (f) {
        Error.stackTraceLimit = Infinity;
        console.trace(f);
        return f({});
    };
};
