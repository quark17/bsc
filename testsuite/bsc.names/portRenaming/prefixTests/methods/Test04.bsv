interface IFC#(type anyType);
  (* prefix = "_variable" *)
 method Action start(anyType a, anyType b);
  (* prefix = "_variable" *)
 method anyType result(anyType c);
  (* prefix = "_variable" *)
 method ActionValue#(anyType) check(anyType d);
endinterface

typedef Bit#(8) Type;

(* synthesize *)
module mkDesign_04 (IFC#(Type));

  Reg#(Type) val <- mkReg(0);
  Reg#(Type) res <- mkReg(0);


  method Action start(a,b);
    val <= a;
    res <= b;
  endmethod

  method Type result(c);
     return res+c;
  endmethod

  method ActionValue#(Type) check(d);
    val <= val + 1;
    res <= res + 2;
	return res+d;
  endmethod

endmodule
