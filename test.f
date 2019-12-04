
true;
if false then true else false;

(\x:Bool->Bool.x false) (\x:Bool.x) ;
(\x:Bool.x) true;
iszero (pred (succ 2));

let projx = (\r:{x:Nat,y:Nat,z:Nat}.r.x) in projx {x=2,y=4,z=1} ;
(\x:String.x) "hoge";

case <some=1> as <some:Nat,none:Unit> of <some=a> => succ a | <none=y> => 0; 
let g = (\x:Nat.succ x) in g 1;
let f = (\x : <some:Nat,none:Unit>. case x of <some=a> => succ a | <none=y> => 0) in 
f (<some=1> as <some:Nat,none:Unit>) ; 

X = Nat; 
(\x:X.succ x) 1;
Y = X -> X; 
s = (\x:X.succ x);
double = \f:Y.\x:X.f(f x);
double s 1;

