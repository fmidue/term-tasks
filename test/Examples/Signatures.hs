module Examples.Signatures where

import DataType

signature1 = Signature [a1,b1,t1,u1,v1,w1,y1,z1]
a1 = Symbol "a" [] (Type "A")
b1 = Symbol "b" [] (Type "B")
t1 = Symbol "t" [Type "B" ,Type "E"] (Type "D")
u1 = Symbol "u" [Type "C" ,Type "C"] (Type "E")
v1 = Symbol "v" [Type "B"] (Type "C")
w1 = Symbol "w" [Type "C" ,Type "A"] (Type "D")
x1 = Symbol "x" [Type "A" ,Type "B"] (Type "C")
y1 = Symbol "y" [Type "A"] (Type "D")
z1 = Symbol "z" [Type "A" ,Type "C"] (Type "D")

signature2 = Signature [a2,b2,c2,d2,e2,f2,g2,h2,i2]
a2 = Symbol "a" [] (Type "A")
b2 = Symbol "b" [Type "E"] (Type "F")
c2 = Symbol "c" [Type "A"] (Type "C")
d2 = Symbol "d" [Type "C"] (Type "B")
e2 = Symbol "e" [] (Type "C")
f2 = Symbol "f" [Type "C"] (Type "D")
g2 = Symbol "g" [Type "D" ,Type "A"] (Type "E")
h2 = Symbol "h" [Type "E"] (Type "C")
i2 = Symbol "i" [Type "G"] (Type "A")

signature3 = Signature [x3,y3,z3,f3,g3,h3]
x3  = Symbol "x" [] (Type "A")
y3  = Symbol "y" [] (Type "B")
z3  = Symbol "z" [] (Type "C")
f3  = Symbol "f" [Type "A",Type "A"] (Type "B")
g3  = Symbol "g" [Type "A",Type "B"] (Type "C")
h3  = Symbol "h" [Type "A",Type "B",Type "C"] (Type "D")

signature4 = Signature [a4,b4,c4,d4,x4,y4,z4,f4,g4,h4]
a4  = Symbol "a" [] (Type "A")
b4  = Symbol "b" [] (Type "B")
c4  = Symbol "c" [] (Type "C")
d4  = Symbol "d" [] (Type "D")
x4  = Symbol "x" [Type "A",Type "B" ,Type "C"] (Type "A")
y4  = Symbol "y" [Type "B",Type "D"] (Type "E")
z4  = Symbol "z" [Type "D",Type "C" ,Type "E"] (Type "F")
f4  = Symbol "f" [Type "A",Type "B"] (Type "G")
g4  = Symbol "g" [Type "F",Type "G"] (Type "C")
h4  = Symbol "h" [Type "A",Type "G"] (Type "F")

signature5 = Signature [a5,b5,c5,d5,e5]
a5  = Symbol "a" [] (Type "A")
b5  = Symbol "b" [Type "A"] (Type "B")
c5  = Symbol "c" [Type "B"] (Type "C")
d5  = Symbol "d" [Type "C"] (Type "D")
e5  = Symbol "e" [Type "D"] (Type "A")

signature6 = Signature [a6,b6,c6,d6,e6,f6]
a6  = Symbol "a" [] (Type "A")
b6  = Symbol "b" [] (Type "A")
c6  = Symbol "c" [Type "A",Type "A",Type "C"] (Type "B")
d6  = Symbol "d" [] (Type "C")
e6  = Symbol "e" [] (Type "C")
f6  = Symbol "f" [] (Type "C")

