let double = \X.\f:X->X.\x:X.f (f x) in double [Nat] (\x:Nat.succ x) 10;
let double = \X.\f:X->X.\x:X.f (f x) in double [Nat] (\x:Nat.succ x) 10;

let {X,d} = {*Nat,{f=\x:Nat.succ x, a=1} } as Exists Y. {f:Y->Nat, a:Y} in d.f d.a ;
let {X,e} = {*Nat,{d=\f:Nat->Nat.\x:Nat.f(f x), f=\x:Nat.succ x, a=1} } as Exists Y. {d:(Y->Y)->Y->Nat, f:Y->Y, a:Y} in e.d e.f e.a ;
