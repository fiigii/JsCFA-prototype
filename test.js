function f(x) {
    if(x > 0) {
        return function(y) {return x * y};
    } else {
        return function(z) {return z - x};
    }
}

