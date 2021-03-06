package Sscanr;

import Vector :: *;

function Action displayabc (a abc) provisos (Bits #(a, sa));
    action
      $display ("%d", abc);
    endaction
endfunction



function Action display_list (Vector #(n,a) my_list) provisos (Bits # (a,sa));
     action
       joinActions ( map (displayabc, my_list));
     endaction
endfunction

function Bit #(16) my_mult (Bit #(8) a, Bit #(16) b);
   return (zeroExtend (a) * b);
endfunction


module mkTestbench_Sscanr();
   Vector #(7,Bit #(8)) my_list1 = cons (8, cons (7, cons (6, cons (5, cons (4, cons (3, cons (2, nil)))))));
   Vector #(7,Bit #(16)) my_list2 = cons (2, cons (6, cons (24, cons (120, cons (720, cons (5040, cons (40320, nil)))))));

   Vector #(7,Bit #(16)) my_list3 = sscanr (my_mult, 16'd1, my_list1);

   rule fire_once (True);
      $display("Original Vector:");
      display_list(my_list1);
      $display("Scanned Vector (Factorial):");
      display_list(my_list3);
      if (reverse (my_list2) != my_list3)
        $display ("Simulation Fails");
      else
        $display ("Simulation Passes");
	  $finish(2'b00);
   endrule

endmodule : mkTestbench_Sscanr
endpackage : Sscanr
