
module sysDescendingUrgencyAttributeSubModule1 (Empty);

  Reg#(Bit#(8)) count();
  mkReg#(0) count_r(count);

  Empty e();
  mkM#(count) m(e);

  (* descending_urgency="test_rule_1, m.test_rule_2" *)
  rule test_rule_1;
    count <= count + 2;
  endrule

endmodule

module mkM#(Reg#(Bit#(8)) count) (Empty);
  rule test_rule_2;
    count <= count + 3;
  endrule
endmodule

