# JsCFA
## An Abstract Interpreter for JavaScript with Perfect Call/Return Matching.

This a control flow analyzer for JavaScript based on [h-CFA](http://dc.uwm.edu/etd/1300/).
JsCFA can match call/return flow perfectly, and it filters local data flow (stack variables) via abstract garbage collection.
Therefore, JsCFA provides very precise result of value-flow analysis. 

### Applications:
1. Static Type Inference.
2. Call Graph Based Interprocedure Analysis and Optimization. 
3. Devirtualization Optimization.

## Example  
```javascript
function f(x) {
    if(x > 0) {
        return function(y) {return x * y};
    } else {
        return function(z) {return z - x};
    }
}

var a = f(2)(0);
var b = f(f(a)(a));
```

The analysis result from JsCFA, which has exact control flows and types:
```
z -> 
    JSNumber(VariableNumber)
x -> 
    JSNumber(VariableNumber)
y -> 
    JSNumber(VariableNumber)
b -> 
    FunctionObject : 
function(z) {
  return z - x;
}
    FunctionObject : 
function(y) {
  return x * y;
}
f -> 
    FunctionObject : 
function f(x) {
  if (x > 0) 
  {
    return function(y) {
  return x * y;
};
  } else {
    return function(z) {
  return z - x;
};
  }
}

a -> 
    JSNumber(VariableNumber)
360 msecs
Miss Match: 0
```

A more complex example for showing JsCFA type inference:
 ```javascript
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
 ```
 
 There is a loop pointer that represents the recursive data type in the analysis result:
 ```
 a0 -> 
     JSString(ConstantString(a0))
 a2 -> 
     JSNumber(ConstantNumber(1.0))
     Object : 
         JSString(ConstantString(x)) -> JSReference(21,-1)
         JSString(ConstantString(__proto__)) -> JSReference(23,-1)
         JSString(ConstantString(constructor)) -> JSReference(24,-1)
 
 a1 -> 
     JSString(ConstantString(a1))
     Object : 
         JSString(ConstantString(x)) -> JSReference(21,-1)
         JSString(ConstantString(__proto__)) -> JSReference(23,-1)
         JSString(ConstantString(constructor)) -> JSReference(24,-1)
 
 x -> 
     JSString(VariableString)
     JSNumber(ConstantNumber(1.0))
     Object : 
         JSString(ConstantString(x)) -> JSReference(21,-1)
         JSString(ConstantString(__proto__)) -> JSReference(23,-1)
         JSString(ConstantString(constructor)) -> JSReference(24,-1)
 
 g -> 
     FunctionObject : 
 function g(x, y) {
   if (y == 0) 
   {
     return x;
   } else {
     return g({x: x}, y - 1);
   }
 }
 
 y -> 
     JSNumber(VariableNumber)
 546 msecs
 Miss Match: 0
 ```
 
 Value map of above references:
  ```
  JSReference(21,-1) ->
      JSString(ConstantString(a1))
      JSNumber(ConstantNumber(1.0))
      Object : 
          JSString(ConstantString(x)) -> JSReference(21,-1)
          JSString(ConstantString(__proto__)) -> JSReference(23,-1)
          JSString(ConstantString(constructor)) -> JSReference(24,-1)
          
  JSReference(23,-1) ->
      Object : 
          JSString(ConstantString(toString)) -> JSReference(-22,-1)
          JSString(ConstantString(__proto__)) -> JSReference(-2,-1)
          JSString(ConstantString(constructor)) -> JSReference(-7,-1)
  ```
