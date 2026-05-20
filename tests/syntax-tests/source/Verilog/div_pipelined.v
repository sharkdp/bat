// Copyright 2018 Schuyler Eldridge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Implements a fixed-point parameterized pipelined division
// operation. Outputs are expected to be on range [-1,1), techincally
// [-1,2^(BITS-1)-1/2^(BITS-1)]. There is no convergent rounding.
//
// [TODO] Implement optional convergent rounding and some form of
// variable output binary point placement. There are arguments in
// these changes that make sense (specifically, adding an additional
// bit results in a gain of one value when all the other 2^6 values
// greater than 1 aren't used). Other improvements that are needed: 1)
// quotient_gen is getting smaller by one bit in every stage, it would
// make more sense to generate this as such, 2) there's some weird
// initial behavior after rst_n is deasserted, you get weird output on
// the quotient line for a number of cycles.
//
// [TODO] This doesn't exactly behave as expected if you specify
// different BITS and STAGES parameters (which for a functional
// module, should be implemented). Note, that this technically works,
// but needs more investigation to fully understand its properties.

`timescale 1ns / 1ps
module div_pipelined
  (
   input                 clk,
   input                 rst_n,
   input                 start,
   input [BITS-1:0]      dividend,
   input [BITS-1:0]      divisor,
   output reg            data_valid,
   output reg            div_by_zero,
   output reg [STAGES-1:0] quotient
   //   output reg [7:0]      quotient_correct
   );

  // WARNING!!! THESE PARAMETERS ARE INTENDED TO BE MODIFIED IN A TOP
  // LEVEL MODULE. LOCAL CHANGES HERE WILL, MOST LIKELY, BE
  // OVERWRITTEN!
  parameter
    BITS  = 8,
    STAGES  = BITS;

  // y = a/bQ

  reg [STAGES-1:0]       start_gen, negative_quotient_gen, div_by_zero_gen;
  reg [BITS*2*(STAGES-1)-1:0] dividend_gen, divisor_gen, quotient_gen;
  wire [BITS-1:0]             pad_dividend;
  wire [BITS-2:0]             pad_divisor;

  assign pad_dividend  = 0;
  assign pad_divisor  = 0;

  // sign conversion stage
  always @ (posedge clk or negedge rst_n)
    begin
      if (!rst_n) begin
        div_by_zero_gen[0] <= 0;
        start_gen[0] <=0;
        negative_quotient_gen[0] <= 0;
        dividend_gen[BITS*2-1:0] <= 0;
        divisor_gen[BITS*2-1:0] <= 0; end
      else begin
        div_by_zero_gen[0]       <= (divisor == 0);
        start_gen[0]             <= start;
        negative_quotient_gen[0] <= dividend[BITS-1] ^ divisor[BITS-1];
        dividend_gen[BITS*2-1:0] <= (dividend[BITS-1]) ? ~{dividend,pad_dividend} + 1 : {dividend,pad_dividend};
        divisor_gen[BITS*2-1:0]  <= (divisor [BITS-1]) ? ~{1'b1,divisor, pad_divisor} + 1 : {1'b0,divisor, pad_divisor};
      end
    end

  // first computation stage
  always @ (posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      div_by_zero_gen[1]              <= 0;
      start_gen[1]                    <= 0;
      negative_quotient_gen[1]        <= 0;
      divisor_gen[BITS*2*2-1:BITS*2]  <= 0;
      quotient_gen[BITS*2-1:0]        <= 0;
      dividend_gen[BITS*2*2-1:BITS*2] <= 0;
    end
    else begin
      div_by_zero_gen[1]             <= div_by_zero_gen[0];
      start_gen[1]                   <= start_gen[0];
      negative_quotient_gen[1]       <= negative_quotient_gen[0];
      divisor_gen[BITS*2*2-1:BITS*2] <= divisor_gen[BITS*2-1:0] >> 1;
      if ( dividend_gen[BITS*2-1:0] >= divisor_gen[BITS*2-1:0]) begin
        quotient_gen[BITS*2-1:0] <= 1 << STAGES - 2;
        dividend_gen[BITS*2*2-1:BITS*2] <= dividend_gen[BITS*2-1:0] - divisor_gen[BITS*2-1:0];
      end
      else begin
        quotient_gen[BITS*2-1:0] <= 0;
        dividend_gen[BITS*2*2-1:BITS*2] <= dividend_gen[BITS*2-1:0];
      end
    end // else: !if(!rst_n)
  end // always @ (posedge clk)

  generate
    genvar            i;
    for (i = 1; i < STAGES - 2; i = i + 1) begin : pipeline
      always @ (posedge clk or negedge rst_n) begin
        if (!rst_n) begin
          div_by_zero_gen[i+1]                     <= 0;
          start_gen[i+1]                           <= 0;
          negative_quotient_gen[i+1]               <= 0;
          divisor_gen[BITS*2*(i+2)-1:BITS*2*(i+1)] <= 0;
          quotient_gen[BITS*2*(i+1)-1:BITS*2*i]     <= 0;
          dividend_gen[BITS*2*(i+2)-1:BITS*2*(i+1)] <= 0;
        end
        else begin
          div_by_zero_gen[i+1]                     <= div_by_zero_gen[i];
          start_gen[i+1]                           <= start_gen[i];
          negative_quotient_gen[i+1]               <= negative_quotient_gen[i];
          divisor_gen[BITS*2*(i+2)-1:BITS*2*(i+1)] <= divisor_gen[BITS*2*(i+1)-1:BITS*2*i] >> 1;
          if (dividend_gen[BITS*2*(i+1)-1:BITS*2*i] >= divisor_gen[BITS*2*(i+1)-1:BITS*2*i]) begin
            quotient_gen[BITS*2*(i+1)-1:BITS*2*i] <= quotient_gen[BITS*2*i-1:BITS*2*(i-1)] | (1 << (STAGES-2-i));
            dividend_gen[BITS*2*(i+2)-1:BITS*2*(i+1)] <= dividend_gen[BITS*2*(i+1)-1:BITS*2*i] - divisor_gen[BITS*2*(i+1)-1:BITS*2*i];
          end
          else begin
            quotient_gen[BITS*2*(i+1)-1:BITS*2*i]     <= quotient_gen[BITS*2*i-1:BITS*2*(i-1)];
            dividend_gen[BITS*2*(i+2)-1:BITS*2*(i+1)] <= dividend_gen[BITS*2*(i+1)-1:BITS*2*i];
          end
        end // else: !if(!rst_n)
      end // always @ (posedge clk or negedge rst_n)
    end // block: pipeline
  endgenerate

  // last computation stage
  always @ (posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      div_by_zero_gen[STAGES-1]                           <= 0;
      start_gen[STAGES-1]                                 <= 0;
      negative_quotient_gen[STAGES-1]                     <= 0;
      quotient_gen[BITS*2*(STAGES-1)-1:BITS*2*(STAGES-2)] <= 0;
    end
    else begin
      div_by_zero_gen[STAGES-1]       <= div_by_zero_gen[STAGES-2];
      start_gen[STAGES-1]             <= start_gen[STAGES-2];
      negative_quotient_gen[STAGES-1] <= negative_quotient_gen[STAGES-2];
      if ( dividend_gen[BITS*2*(STAGES-1)-1:BITS*2*(STAGES-2)] >= divisor_gen[BITS*2*(STAGES-1)-1:BITS*2*(STAGES-2)] )
        quotient_gen[BITS*2*(STAGES-1)-1:BITS*2*(STAGES-2)] <= quotient_gen[BITS*2*(STAGES-2)-1:BITS*2*(STAGES-3)] | 1;
      else
        quotient_gen[BITS*2*(STAGES-1)-1:BITS*2*(STAGES-2)] <= quotient_gen[BITS*2*(STAGES-2)-1:BITS*2*(STAGES-3)];
    end // else: !if(!rst_n)
  end // always @ (posedge clk)

  // sign conversion stage
  always @ (posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      div_by_zero <= 0;
      data_valid  <= 0;
      quotient    <= 0;
    end
    else begin
      div_by_zero <= div_by_zero_gen[STAGES-1];
      data_valid  <= start_gen[STAGES-1];
      quotient    <= (negative_quotient_gen[STAGES-1]) ? ~quotient_gen[BITS*2*(STAGES-1)-1:BITS*2*(STAGES-2)] + 1 : quotient_gen[BITS*2*(STAGES-1)-1:BITS*2*(STAGES-2)];
    end
  end

endmodule
