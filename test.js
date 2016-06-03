function f(x, y) {
	if(y==0) return x
	else return f([x], y - 1);
}

var obj = f("1", 10)
var a = obj[0]
var b = a[1]
var c = obj[0][0][0][0]
