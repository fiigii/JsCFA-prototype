function f(x, y) {
    if(y == 0) return x;
    else return f({x:x}, y - 1);
}

var a = f("a", 10)
