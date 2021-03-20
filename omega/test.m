/* test.f */ 

if (true) then (succ 10) else (succ 11);

true;
if false then true else false; 
(\x:Bool->Bool. x false) (\x:Bool.x) ;
(\x:Bool.x) true;
/* iszero (pred (succ (succ 0))); */ 
iszero 0; 

BB = Bool -> Bool; 
NN = Nat -> Nat ;

/* (\x:((\X::*.X) Nat). x) 0 ; */

I       = \X.X; 
Id X    = X ;
N       = Id Nat; 

n : N ;

(\x:Nat.x) 0;
(\x:N. x)  0; 


