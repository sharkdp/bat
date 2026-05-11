`timescale 1ns/1ps

// Design Code
module ADDER(
    input clk,
    input [7:0]	a,
    input [7:0]	b,
    input bIsPos,
    output reg [8:0] result
);

    always @ (posedge clk) begin
        if (bIsPos) begin	
            result <= a + b;
        end else begin
            result <= a - b;
        end
    end

endmodule: ADDER

interface adder_if(
    input bit clk,
    input [7:0] a,
    input [7:0] b,
    input bIsPos,
    input [8:0] result
);

    clocking cb @(posedge clk);
        output a;
        output b;
        output bIsPos;
        input result;
    endclocking : cb

endinterface: adder_if


bind ADDER adder_if my_adder_if(
    .clk(clk),
    .a(a),
    .b(b),
    .bIsPos(bIsPos),
    .result(result)
);


// Testbench Code
import uvm_pkg::*;
`include "uvm_macros.svh"

class testbench_env extends uvm_env;

    virtual adder_if m_if;

    function new(string name, uvm_component parent = null);
        super.new(name, parent);
    endfunction
    
    function void connect_phase(uvm_phase phase);
        assert(uvm_resource_db#(virtual adder_if)::read_by_name(get_full_name(), "adder_if", m_if));
    endfunction: connect_phase

    task run_phase(uvm_phase phase);
        phase.raise_objection(this);
        `uvm_info(get_name(), "Starting test!", UVM_HIGH);
        begin
            int a = 8'h4, b = 8'h5;
            @(m_if.cb);
            m_if.cb.a <= a;
            m_if.cb.b <= b;
            m_if.cb.bIsPos <= 1'b1;
            repeat(2) @(m_if.cb);
            `uvm_info(get_name(), $sformatf("%0d + %0d = %0d", a, b, m_if.cb.result), UVM_LOW);
        end
        `uvm_info(get_name(), "Ending test!", UVM_HIGH);
        phase.drop_objection(this);
    endtask: run_phase
endclass


module top;

    bit clk;
    env environment;
    ADDER dut(.clk (clk));

    initial begin
        environment = new("testbench_env");
        uvm_resource_db#(virtual adder_if)::set("env", "adder_if", dut.my_adder_if);
        clk = 0;
        run_test();
    end

    // Clock generation	
    initial begin
        forever begin
            #(1) clk = ~clk;
        end
    end
    
endmodule
