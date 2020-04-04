
true;
if false then true else false;

(\x:Bool->Bool.x false) (\x:Bool.x) ;
(\x:Bool.x) true;
iszero (pred (succ 2));


let g = (\x:Nat.succ x) in g 1;

X = Nat; 
(\x:X.succ x) 1;
Y = X -> X; 
s = (\x:X.succ x);
double = \f:Y.\x:X.f(f x);
double s 1;



