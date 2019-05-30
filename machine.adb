with Instruction;
use Instruction;
with Debug; use Debug;

-- used so we can print TAB character
with Ada.Characters.Latin_1;

package body Machine with SPARK_Mode => On is
   -- data values are 32-bit integers
   -- this is the type of words used in the virtual machine
   type DataVal is range -(2**31) .. +(2**31 - 1);
      
   -- the registers
   Regs : array (Reg) of DataVal := (others => 0);
   
   -- the memory
   Memory : array (Addr) of DataVal := (others => 0);
   
   -- the program counter
   PC : ProgramCounter := ProgramCounter'First;
      
   procedure IncPC(Ret : out ReturnCode; Offs : in Offset) is
   begin
      PC := ProgramCounter(Integer(PC) + Integer(Offs));
      Ret := Success;
   end IncPC;
   
   procedure DoAdd(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := Regs(Rs1) + Regs(Rs2);
      Ret := Success;
   end DoAdd;
   
   procedure DoSub(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := Regs(Rs1) - Regs(Rs2);
      Ret := Success;
   end DoSub;
   
   procedure DoMul(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := Regs(Rs1) * Regs(Rs2);
      Ret := Success;
   end DoMul;
   
   procedure DoDiv(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := Regs(Rs1) / Regs(Rs2);
      Ret := Success;
   end DoDiv;
   
   procedure DoLdr(Rd : in Reg; 
                   Rs : in Reg; 
                   Offs : in Offset;
                   Ret : out ReturnCode) is
      A : Addr := Addr(Regs(Rs) + DataVal(Offs));
   begin
      Regs(Rd) := Memory(A);
      Ret := Success;
   end DoLdr;
   
   procedure DoStr(Ra : in Reg;
                   Offs : in Offset;
                   Rb : in Reg;
                   Ret : out ReturnCode) is
      A : Addr := Addr(Regs(Ra) + DataVal(Offs));   
   begin
      Memory(A) := Regs(Rb);
      Ret := Success;
   end DoStr;
   
   procedure DoMov(Rd : in Reg;
                   Offs : in Offset;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := DataVal(Offs);
      Ret := Success;
   end DoMov;
   
   procedure ExecuteProgram(Prog : in Program;
                            Cycles : in Integer;
                            Ret : out ReturnCode;
                            Result : out Integer) 
   is
      CycleCount : Integer := 0;
      Inst : Instr;
   begin
      Ret := Success;
      PC := ProgramCounter'First;
      Result := 0;
      while (CycleCount < Cycles and Ret = Success) loop
         Inst := Prog(PC);
         
         -- debug print pc and current instruction
         Put(Integer(PC)); Put(':'); Put(Ada.Characters.Latin_1.HT);
         DebugPrintInstr(Inst);
         New_Line;
         
         case Inst.Op is
            when ADD =>
               DoAdd(Inst.AddRd,Inst.AddRs1,Inst.AddRs2,Ret);
               IncPC(Ret,1);
            when SUB =>
               DoSub(Inst.SubRd,Inst.SubRs1,Inst.SubRs2,Ret);
               IncPC(Ret,1);
            when MUL =>
               DoMul(Inst.MulRd,Inst.MulRs1,Inst.MulRs2,Ret);
               IncPC(Ret,1);
            when DIV =>
               DoDiv(Inst.DivRd,Inst.DivRs1,Inst.DivRs2,Ret);
               IncPC(Ret,1);
            when LDR =>
               DoLdr(Inst.LdrRd,Inst.LdrRs,Inst.LdrOffs,Ret);
               IncPC(Ret,1);
            when STR =>
               DoStr(Inst.StrRa,Inst.StrOffs,Inst.StrRb,Ret);
               IncPC(Ret,1);
            when MOV =>
               DoMov(Inst.MovRd,Inst.MovOffs,Ret);
               IncPC(Ret,1);
            when Instruction.RET =>
               Result := Integer(Regs(Inst.RetRs));
               Ret := Success;
               return;
            when JMP =>
               IncPC(Ret,Inst.JmpOffs);
            when JZ =>
               if Regs(Inst.JzRa) = 0 then
                  IncPC(Ret,Inst.JzOffs);
               else
                  IncPc(Ret,1);
               end if;
            when NOP =>
               IncPC(Ret,1);
         end case;
         CycleCount := CycleCount + 1;
      end loop;
      if Ret = Success then
         -- Cycles instructions executed without a RET or invalid behaviour
         Ret := CyclesExhausted;
      end if;
      
   end ExecuteProgram;
   
   -- A static analysis that goes throught the generated assembley instructions
   -- line by line and if a value has been placed in a register, if a offset is
   -- legal and that known register value instructions do not cause over/under
   -- flow
   function DetectInvalidBehaviour(Prog : in Program;
                                   Cycles : in Integer) return Boolean is
      
      -- Integer subtype in the range of ProgramCounter for easy incrementation,
      -- comparison and indexing Program instructions 
      subtype InstrCount is Integer range 1..MEMORY_SIZE;
      Count : InstrCount := InstrCount'First;
      
      Inst : Instr;
      
      -- Array that tracks values MOV'ed into registers adn the manipulation
      -- on these known register values.
      -- It assumes both worst cases where increment or decrement by 1 would
      -- cause overflow and decrement by 1 would cause underflow
      RegTracker : array (Reg) of DataVal := (others => 0);
      
      RegKnown : array (Reg) of Boolean := (others => False);
      
   begin
      
      while(Count < Cycles) loop
         
         inst := Prog(ProgramCounter(Count));
         
         case inst.Op is
            
            when MOV =>
               if 
                 -- Makes sure no invalid values are entered into a register 
                 Inst.MovOffs > Offset'Last or 
                 Inst.MovOffs < Offset'First
               then
                  return True;   
               end if;
               RegTracker(Inst.MovRd) := DataVal(Inst.MovOffs);
               RegKnown(Inst.MovRd) := True;
               Count := Count + 1;
               
            
            when JMP =>
               if
                 -- Jumps outside of the line limit or below 1
                 Integer(Inst.JmpOffs) + Count > Cycles or 
                 Integer(Inst.JmpOffs) + Count < 1 or 
                 -- An invalid offset is entered
                 Inst.JmpOffs < Offset'First or 
                 Inst.JmpOffs > Offset'Last
               then
                  return True;
               end if;
               Count := Count + Integer(Inst.JmpOffs);
            

            when JZ =>
               if 
                 -- Jumps outside of the line limit or below 1
                 Integer(Inst.JzOffs) + Count > Cycles or 
                 Integer(Inst.JzOffs) + Count < 1 or 
                 -- An invalid offset is entered
                 Inst.JzOffs < Offset'First or 
                 Inst.JzOffs > Offset'Last or
                 -- The register value is unknown
                 RegTracker(Inst.JzRa) = 0
               then
                  return True; 
               end if;
               Count := Count + Integer(Inst.JzOffs);
                 
            -- if a return statement is reached and no other condition
            -- has been breached then the program is valid 
            when RET =>
               return False;
               
            when ADD =>
               if not
                 -- Reject if a register is unknown, or 2 known registers lead to 
                 -- overflow or ...
                 (RegKnown(Inst.AddRs1) and RegKnown(Inst.AddRs2) 
                 and (RegTracker(Inst.AddRs1) + RegTracker(Inst.AddRs2) <= DataVal'Last)
                 and (RegTracker(Inst.AddRs1) + RegTracker(Inst.AddRs2) >= DataVal'First))
                 -- if a register is unknown while the other register known to be non 0
                 or ((RegKnown(Inst.AddRs1) and not(RegKnown(Inst.AddRs2)) and 
                        RegTracker(Inst.AddRs1) = 0) or
                    (RegKnown(Inst.AddRs2) and not(RegKnown(Inst.AddRs1)) and 
                        RegTracker(Inst.AddRs2) = 0)
                 )
               then
                  return True;
               end if;
               RegTracker(Inst.AddRd) := RegTracker(Inst.AddRs1) 
                 + RegTracker(Inst.AddRs2);
               RegKnown(Inst.AddRd) := True;
               Count := Count + 1;
            
            -- makes sure the product of two valid values does not overflow 
            -- or underflow
            when MUL =>  
               if not
                 -- A value in the register is unknown or ...
                 -- RegTracker(Inst.MulRs1) = 0  or RegTracker(Inst.MulRs2) = 0 or
                 -- both register values are known and their product is 
                 -- outside of legal data values
                 --(RegTrackerMax(Inst.MulRs1) * RegTrackerMax(Inst.MulRs2) > DataVal'Last)
                 --and RegTrackerMin(Inst.MulRs1) * RegTrackerMin(Inst.MulRs2) 
                 --< DataVal'First)
                 -- Reject if a register is unknown, or 2 known registers lead to 
                 -- overflow or ...
                 (RegKnown(Inst.MulRs1) and RegKnown(Inst.MulRs2) 
                 and (RegTracker(Inst.MulRs1) * RegTracker(Inst.MulRs2) <= DataVal'Last)
                 and (RegTracker(Inst.MulRs1) * RegTracker(Inst.MulRs2) >= DataVal'First))
                 -- if a register is unknown while the other register known to be non 0
                 or ((RegKnown(Inst.MulRs1) and not(RegKnown(Inst.MulRs2)) and 
                        RegTracker(Inst.MulRs1) = 0) or
                    (RegKnown(Inst.MulRs2) and not(RegKnown(Inst.MulRs1)) and 
                        RegTracker(Inst.MulRs2) = 0)
                 )
               then
                  return True;
               end if;
               RegTracker(Inst.MulRd) := RegTracker(Inst.MulRs1) 
                 * RegTracker(Inst.MulRs2);
               RegKnown(Inst.MulRd) := True;
               Count := Count + 1;
            
            when DIV =>
               -- The denominator is set to zero (either assumed 0 if unknown
               -- or checked if entered by MOV)
               if Integer(RegTracker(Inst.DivRs2)) = 0
               then
                  return True;
               end if;
               RegTracker(Inst.DivRd) := RegTracker(Inst.DivRs1) 
                 * RegTracker(Inst.DivRs2);
               RegKnown(Inst.DivRd) := True;
               Count := Count + 1;
            
            -- makes sure that the difference of two valid values does not
            -- overflow nor underflow
            when SUB =>
               if not
                 -- both register values are known and their subtraction is 
                 -- outside of legal data values or ...
                 (RegKnown(Inst.SubRs1) and RegKnown(Inst.SubRs2) 
                 and (RegTracker(Inst.SubRs1) - RegTracker(Inst.SubRs2) <= DataVal'Last)
                 and (RegTracker(Inst.SubRs1) - RegTracker(Inst.SubRs2) >= DataVal'First))
                 -- if a register is unknown while the other register known to be non 0
                 or ((RegKnown(Inst.SubRs1)  and RegTracker(Inst.SubRs1) = 0 and 
                        not RegKnown(Inst.SubRs2)) or
                     (RegKnown(Inst.SubRs2) and RegTracker(Inst.SubRs2) = 0 and 
                        not RegKnown(Inst.SubRs1))
                 )
                 -- a value in the register is unknown
                 --RegTracker(Inst.SubRs1) = 0 or RegTracker(Inst.SubRs2) = 0
               then
                  return True;
               end if;
               RegTracker(Inst.SubRd) := RegTracker(Inst.SubRs1) 
                 - RegTracker(Inst.SubRs2);
               RegKnown(Inst.SubRd) := True;
               Count := Count + 1;
            
            when LDR =>
               if 
                 -- The register value is known and the sum of the value and
                 -- the offset are outside of legal addresses or ...
                 Integer(DataVal(Inst.LdrOffs) + RegTracker(Inst.LdrRs))  
                 > Integer(Addr'Last) or
                 Integer(DataVal(Inst.LdrOffs) + RegTracker(Inst.LdrRs)) 
                 < Integer(Addr'First) or
                 Inst.LdrOffs > Offset'Last or Inst.LdrOffs < Offset'First or 
                 -- The register value is unknown
                 RegTracker(Inst.LdrRs) = 0
               then
                  return True;
               end if;
               Count := Count + 1;
            
            when STR => 
               if 
                 -- The register value is known and the sum of the value and
                 -- the offset are outside of legal addresses or ...
                 Integer(DataVal(Inst.StrOffs) + RegTracker(Inst.StrRa)) 
                 > Integer(Addr'Last) or
                 Integer(DataVal(Inst.StrOffs) + RegTracker(Inst.StrRa)) 
                   < Integer(Addr'First) or
                 Inst.StrOffs > Offset'Last or Inst.StrOffs < Offset'First or
                 -- The register value is unknown
                 RegTracker(Inst.StrRa) = 0
               then
                  return True;
               end if;
               Count := Count + 1;
            
            -- increment counter
            when NOP =>
               Count := Count + 1;
         
            end case;
              
       
      end loop;
      return True;
   end DetectInvalidBehaviour;
   
end Machine;
