(\x:Bool.if x then 1 else true ) ;
(\y:(Pi Bool (\x:Bool. if x then Bool else Nat)).y) ;
(\y:(Pi Bool (\x:Bool. if x then Bool else Nat)).y) (\x:Bool. if x then true else 1) ;

/* g : a->b->c is constraint that we define the returning type as c */
swap = \a:Univ. \b:Univ. \c:Univ. \g:(a->(b->c)). \y:b. \x:a. (g x) y ;




