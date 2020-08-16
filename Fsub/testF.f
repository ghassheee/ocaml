let double = \X.\f:X->X.\x:X.f (f x) in double [Nat] (\x:Nat.succ x) 10;
let double = \X.\f:X->X.\x:X.f (f x) in double [Nat] (\x:Nat.succ x) 10;

let {X,d} = {*Nat,{f=\x:Nat.succ x, a=1} } as Exists Y. {f:Y->Nat, a:Y} in d.f d.a ;
let {X,e} = {*Nat,{d=\f:Nat->Nat.\x:Nat.f(f x), f=\x:Nat.succ x, a=1} } as Exists Y. {d:(Y->Y)->Y->Nat, f:Y->Y, a:Y} in e.d e.f e.a ;


counterADT = {*Nat, {new=1, get=\i:Nat.i, inc=\i:Nat.succ(i)}} 
        as Exists Counter. {new:Counter,get:Counter->Nat,inc:Counter->Counter} ;

let {Counter,counter} = counterADT in 
    counter.get (counter.inc counter.new) ;


