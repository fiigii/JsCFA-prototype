/*
function fib(x) {
	if(x < 3) return 1;
	else return fib(x -1) + fib(x - 2);
}

function fib1(x) {
    var r1 = x < 3;
    if(r1) return 1;
    else {
        var r2 = x - 1;
        var r3 = fib1(r2)
        var r4 = x - 2
        var r5 = fib1(r4)
        var r6 = r3 + r2
        return r6
    }
}
 */

function f(x) {
    if(x == 0) return function(y) {return y + x};
    else return function(y) {return x + 22 - y}
}


var b = f(1)(17)
var a = f(0)
var ff = f(a(10))

var d = a("s")
var e = a(1)
