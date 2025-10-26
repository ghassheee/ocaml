
/* g : a->b->c is constraint that we define the returning type as c */
swap = \a:Univ. \b:Univ. \c:Univ. \g:(a->(b->c)). \y:b. \x:a. (g x) y ;




