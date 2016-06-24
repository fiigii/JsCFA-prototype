
function fib(n) {
	if(n < 3) return 1;
	else return fib(n - 1) + fib(n - 2);
}

var a = fib(10)
//var b = fib(122)

var b = a + 1 * 19

/*
function tak(x,y,z) {
	if (y >= x) return z;
	return tak(tak(x-1,y,z), tak(y-1,z,x), tak(z-1,x,y));
}

var a = tak(12,2,1)
var b = tak(32,16,5)
*/