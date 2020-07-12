/* test.f */ 

if (true) then (succ 10) else (succ 11); 

true;
if false then true else false; 

x/;
y/;
(\x.x) 1; 
(\z.z) 10; 
0; 
succ (pred 0);
iszero (pred (succ (succ 0))); 

(\a.\b.\c.\x.a x); 
(\a.\b.\c.\x.b a x); 




