module Examples.Signatures where

import DataType

signature1 = Signature [a1,b1,t1,u1,v1,w1,y1,z1]
a1 = FunctionSymbol "a" [] (Type "A")
b1 = FunctionSymbol "b" [] (Type "B")
t1 = FunctionSymbol "t" [Type "B" ,Type "E"] (Type "D")
u1 = FunctionSymbol "u" [Type "C" ,Type "C"] (Type "E")
v1 = FunctionSymbol "v" [Type "B"] (Type "C")
w1 = FunctionSymbol "w" [Type "C" ,Type "A"] (Type "D")
x1 = FunctionSymbol "x" [Type "A" ,Type "B"] (Type "C")
y1 = FunctionSymbol "y" [Type "A"] (Type "D")
z1 = FunctionSymbol "z" [Type "A" ,Type "C"] (Type "D")

signature2 = Signature [a2,b2,c2,d2,e2,f2,g2,h2,i2]
a2 = FunctionSymbol "a" [] (Type "A")
b2 = FunctionSymbol "b" [Type "E"] (Type "F")
c2 = FunctionSymbol "c" [Type "A"] (Type "C")
d2 = FunctionSymbol "d" [Type "C"] (Type "B")
e2 = FunctionSymbol "e" [] (Type "C")
f2 = FunctionSymbol "f" [Type "C"] (Type "D")
g2 = FunctionSymbol "g" [Type "D" ,Type "A"] (Type "E")
h2 = FunctionSymbol "h" [Type "E"] (Type "C")
i2 = FunctionSymbol "i" [Type "G"] (Type "A")

signature3 = Signature [x3,y3,z3,f3,g3,h3]
x3  = FunctionSymbol "x" [] (Type "A")
y3  = FunctionSymbol "y" [] (Type "B")
z3  = FunctionSymbol "z" [] (Type "C")
f3  = FunctionSymbol "f" [Type "A",Type "A"] (Type "B")
g3  = FunctionSymbol "g" [Type "A",Type "B"] (Type "C")
h3  = FunctionSymbol "h" [Type "A",Type "B",Type "C"] (Type "D")