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
   -- line by line and checks the legality of offsets, addresses, data 
   -- in registers and divide by zero
   function DetectInvalidBehaviour(Prog : in Program;
                                   Cycles : in Integer) return Boolean is
      
      -- Integer subtype in the range of ProgramCounter for easy incrementation,
      -- comparison and indexing Prog
      subtype InstrCount is Integer range 1..MEMORY_SIZE;
      Count : InstrCount := InstrCount'First;
      Inst : Instr;
      
   begin
      
      while(Count < Cycles) loop
         
         inst := Prog(ProgramCounter(Count));
         
         case inst.Op is
            
            -- Makes sure no invalid values are entered into a register 
            when MOV =>
               if Integer(Inst.MovOffs) >= MEMORY_SIZE or 
                 Integer(Inst.MovOffs) <= -(MEMORY_SIZE) 
               then
                  return True;   
               end if;
               Count := Count + 1;

            -- Makes sure counter does not drop below 1 or jump past line limit 
            -- limit
            when JMP =>
               if Integer(Inst.JmpOffs) + Count > Cycles or 
                 Integer(Inst.JmpOffs) + Count < 1 or 
                 Integer(Inst.JmpOffs) <= -(MEMORY_SIZE) or 
                 Integer(Inst.JmpOffs) >= MEMORY_SIZE
               then
                  return True;
               end if;
               Count := Count + Integer(Inst.JmpOffs);
            
            -- Makes sure counter does not drop below 1 or jump past line limit
            -- limit
            when JZ =>
               if Integer(Inst.JzOffs) + Count > Cycles or 
                 Integer(Inst.JzOffs) + Count < 1 or 
                 Integer(Inst.JzOffs) <= -(MEMORY_SIZE) or 
                 Integer(Inst.JzOffs) >= MEMORY_SIZE
               then
                  return True; 
               end if;
               Count := Count + Integer(Inst.JzOffs);
                 
            -- if a return statement is reached and no other condition
            -- has been breached then the program is valid 
            when RET =>
               return False;
               
            -- makes sure that the sum of two valid values does not overflow
            -- or underflow
            when ADD =>
               if Regs(Inst.AddRs1) + Regs(Inst.AddRs2) > DataVal'Last or
                 Regs(Inst.AddRs1) + Regs(Inst.AddRs1) < DataVal'First
               then
                  return True;
               end if;
               Count := Count + 1;
            
            -- makes sure the product of two valid values does not overflow 
            -- or underflow
            when MUL =>
               if Regs(Inst.MulRs1) * Regs(Inst.MulRs2) > DataVal'Last or
                 Regs(Inst.MulRs1) * Regs(Inst.MulRs1) < DataVal'First 
               then
                  return True;
               end if;
               Count := Count + 1;
            
            -- Mitigates divide by zero
            when DIV =>
               if Integer(Regs(Inst.DivRs2)) = 0 
               then
                  return True;
               end if;
               Count := Count + 1;
            
            -- makes sure that the difference of two valid values does not
            -- overflow nor underflow
            when SUB =>
               if Regs(Inst.SubRs1) - Regs(Inst.SubRs2) > DataVal'Last or
                 Regs(Inst.SubRs1) - Regs(Inst.SubRs2) < DataVal'First 
               then
                  return True;
               end if;
               Count := Count + 1;
            
            -- Checks that loading address is a valid
            when LDR =>
               if Integer(DataVal(Inst.LdrOffs) + Regs(Inst.LdrRs))  > Integer(Addr'Last) or
                 Integer(DataVal(Inst.LdrOffs) + Regs(Inst.LdrRs)) < Integer(Addr'First) or
                 Inst.LdrOffs > Offset'Last or Inst.LdrOffs < Offset'First
               then
                  return True;
               end if;
               Count := Count + 1;
                
            -- Checks that storing address is valid
            when STR => 
                if Integer(DataVal(Inst.StrOffs) + Regs(Inst.StrRa)) > Integer(Addr'Last) or
                 Integer(DataVal(Inst.StrOffs) + Regs(Inst.StrRa)) < Integer(Addr'First)
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
