{ 
    open Support
    open Parser

    let reserved = [
        ("Variables",   fun i -> VARIABLES i);
        ("Theorem",     fun i -> THEOREM i); 
        ("                                                                                                                                                                                                                                                                                                                                                                                                                                Lemma",       fun i -> LEMMA i); 
        ("pepe
