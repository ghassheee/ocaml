(\x:Bool.if x then 1 else true ) ;
(\y:(Pi Bool (\x:Bool. if x then Bool else Nat)).y) (\x:Bool. if x then true else 1) ;
