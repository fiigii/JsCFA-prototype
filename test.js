function f(x) {
    if(x > 0) {
        return function(y) {return x * y};
    } else {
        return function(z) {return z - x};
    }
}

var a = f(2)(0);
var b = f(f(a)(a));

function g(x, y) {
    if (y == 0) {
        return x;
    } else {
        return g({x: x}, y - 1);
    }
}

var a0 = g("a0", 0);
var a1 = g("a1", 1);
var a2 = g(1, 10);