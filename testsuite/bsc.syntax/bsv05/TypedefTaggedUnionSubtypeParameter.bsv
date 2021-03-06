typedef union tagged {
    void   Nil;
    struct {
        t          hd;
        FList #(t) tl;
    } Cons;
} FList #(type t);


function FList#(t) cons (t x, FList#(t) xs);
    return tagged Cons {hd:x, tl:xs};
endfunction: cons

function FList#(t) nil ();
    return tagged Nil;
endfunction
