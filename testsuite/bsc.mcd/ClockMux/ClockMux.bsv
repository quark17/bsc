import Clocks::*;

(* synthesize *)
module sysClockMux(Empty);

  Reg#(Bit#(3)) count5 <- mkReg(0);

  Reg#(Bit#(4)) count11 <- mkReg(0);

  Reg#(Bit#(32)) flipcount <- mkReg(0);

  Reg#(Bit#(32)) fliptime <- mkReg(16);

  Reg#(Bool) clkflag <- mkReg(True);

  MakeClockIfc#(Bool) mc5 <- mkClock(False, True);
  Clock clk5 = mc5.new_clk;

  MakeClockIfc#(Bool) mc11 <- mkClock(False, True);
  Clock clk11 = mc11.new_clk;

  MuxClkIfc cm <- mkClockMux(clk11, clk5);
  Clock clkM = cm.clock_out;

  // make a reset for the muxed clock
  Reset rstM <- mkAsyncResetFromCR(2, clkM);

  Reg#(Bit#(14)) real_counter <- mkReg(0, clocked_by clkM, reset_by rstM);

  rule count;
     let new_count5 = (count5 < 5) ? (count5 + 1) : 0;
     mc5.setClockValue(new_count5 == 5);
     count5 <= new_count5;

     let new_count11 = (count11 < 11) ? (count11 + 1) : 0;
     mc11.setClockValue(new_count11 == 11);
     count11 <= new_count11;

     flipcount <= flipcount + 1;
  endrule

  (* descending_urgency="select, count" *)
  rule select (flipcount == fliptime);
     cm.select(clkflag);
     clkflag <= !clkflag;
     fliptime <= fliptime << 1;
     flipcount <= 0;

     if(clkflag)
       $display ("Clock Select A");
     else
       $display ("Clock Select B");

  endrule

  rule real_count;
     real_counter <= real_counter + 1;
  endrule

  rule test;
    $display("Real counter: %0d Time %t", real_counter, $time);
    if (real_counter == 9)
      $finish(0);
  endrule

endmodule



