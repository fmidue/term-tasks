module Examples.Signatures where

import DataType

signature1 = Signature [a1,b1,t1,u1,v1,w1,y1,z1]
a1 = FunctionSymbol "a" [] A
b1 = FunctionSymbol "b" [] B
t1 = FunctionSymbol "t" [B ,E] D
u1 = FunctionSymbol "u" [C ,C] E
v1 = FunctionSymbol "v" [B] C
w1 = FunctionSymbol "w" [C ,A] D
x1 = FunctionSymbol "x" [A ,B] C
y1 = FunctionSymbol "y" [A] D
z1 = FunctionSymbol "z" [A ,C] D

-- signature2 = Signature [a2,b2,c2,d2,e2,f2,g2,h2,i2]
-- a2 = FunctionSymbol "a" [] A
-- b2 = FunctionSymbol "b" [E] F
-- c2 = FunctionSymbol "c" [A] C
-- d2 = FunctionSymbol "d" [C] B
-- e2 = FunctionSymbol "e" [C]
-- f2 = FunctionSymbol "f" [C] D
-- g2 = FunctionSymbol "g" [D ,A] E
-- h2 = FunctionSymbol "h" [E] C
-- i2 = FunctionSymbol "i" [G] A

signature3 = Signature [x3,y3,z3,f3,g3,h3]
x3  = FunctionSymbol "x" [] A
y3  = FunctionSymbol "y" [] B
z3  = FunctionSymbol "z" [] C
f3  = FunctionSymbol "f" [A,A] B
g3  = FunctionSymbol "g" [A,B] C
h3  = FunctionSymbol "h" [A,B,C] D
