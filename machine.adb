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
-- Mitigates the following
--machine.adb:24:40: medium: range check might fail, in call inlined at machine.adb:146 (e.g. when PC = 65536)
--machine.adb:24:40: medium: range check might fail, in call inlined at machine.adb:139 (e.g. when PC = 1)
--machine.adb:24:40: medium: range check might fail, in call inlined at machine.adb:132 (e.g. when PC = 65536)
--machine.adb:24:40: medium: range check might fail, in call inlined at machine.adb:123 (e.g. when PC = 65536)
--machine.adb:24:40: medium: range check might fail, in call inlined at machine.adb:149 (e.g. when PC = 65536)
--machine.adb:24:40: medium: range check might fail, in call inlined at machine.adb:120 (e.g. when PC = 65536)
--machine.adb:24:40: medium: range check might fail, in call inlined at machine.adb:117 (e.g. when PC = 65536)
--machine.adb:24:40: medium: range check might fail, in call inlined at machine.adb:144 (e.g. when PC = 1)
--machine.adb:24:40: medium: range check might fail, in call inlined at machine.adb:129 (e.g. when PC = 65536)
--machine.adb:24:40: medium: range check might fail, in call inlined at machine.adb:114 (e.g. when PC = 65536)
--machine.adb:24:40: medium: range check might fail, in call inlined at machine.adb:126 (e.g. when PC = 65536)
--------------------------------------------------------------------------------
-- This is caused by the Program counter going outside of its range, as JZ and JMP
-- Could go over the program's MAX_PROGRAM_LENGTH
      if Integer(PC) + Integer(Offs) > Integer(ProgramCounter'Last) or
        Integer(PC) + Integer(Offs) < Integer(ProgramCounter'First) then
         Ret := IllegalProgram;
      else
         PC := ProgramCounter(Integer(PC) + Integer(Offs));
         Ret := Success;
      end if;
   end IncPC;

   procedure DoAdd(Rd : in Reg;
                   Rs1 : in Reg;
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
      Val1 : Integer := Integer(Regs(Rs1));
      Val2 : Integer := Integer(Regs(Rs2));
      Min : Integer := Integer(DataVal'First);
      Max : Integer := Integer(DataVal'Last);

   begin

      Ret := Success;
      if -- If two positive numbers ...
        Val1 > 0 and Val2 > 0 then
        -- sum to a numeber greater than DataVal/Integer
        -- ILLEGALPROGRAM
         if Max - Val1 < Val2 then
            Ret := IllegalProgram;
         end if;
      end if;

      -- Mitigates the following:
      -- machine.adb:33:29: medium: overflow check might fail, in call inlined at
      --  machine.adb:113 (e.g. when Regs = (others => -1073741825))
      -------------------------------------------------------------------------
      -- This is caused by two negative numbers summing and overlowing not only
      -- data values but ada's 32 bit integers
      if -- If two negative numbers ...
         Val1 < 0 and Val2 < 0 then
         -- sum to a number smaller  than DataVal/Integer
         -- ILLEGALPROGRAM
         if Min - Val1 > Val2 then
            Ret := IllegalProgram;
         end if;

      end if;
      -- machine.adb:34:11: warning: unused assignment, in call inlined at
      --   machine.adb:113
      -------------------------------------------------------------------------
      -- Make use of Ret which was just assigned in this proceedure, and not used
      -- within this proceedure
      if -- No chance of overflow
        Ret = Success then
               Regs(Rd) := Regs(Rs1) + Regs(Rs2);
      end if;




   end DoAdd;

   procedure DoSub(Rd : in Reg;
                   Rs1 : in Reg;
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
      Val1 : Integer := Integer(Regs(Rs1));
      Val2 : Integer := Integer(Regs(Rs2));
      Min : Integer := Integer(DataVal'First);
      Max : Integer := Integer(DataVal'Last);
   begin
      -- machine.adb:42:29: medium: overflow check might fail, in call inlined
      --   at machine.adb:116 (e.g. when Regs = (1 => 1, others => -2147483648))
      -------------------------------------------------------------------------
      -- If too large of a number is subtracted from too small a number it
      -- will go below DataVal's range.
      Ret := Success;
      if -- If a positive number is subtracted from a negative number ...
        Val1 < 0 and Val2 > 0 then
         if Min + Val2 > Val1 then
        -- and that positive number will go lower than DataVal/Integer
        -- ILLEGALPROGRAM
            Ret := IllegalProgram;
         end if;
      end if;

      if -- If a negative number is subtracted from a positive number ...
        Val1 > 0 and Val2 < 0 then
         if Max + Val2 < Val1 then
        -- and that positive number will go lower than DataVal/Integer
        -- ILLEGALPROGRAM
            Ret := IllegalProgram;
         end if;
      end if;

      if -- If two negative numbers cause overflow => ILLEGALPROGRAM
         -- 0 included as [(0 - Min => overflow) => ILLEGALPROGRAM]
        Val1 = 0 and Val2 < 0 then
        if Max + Val2 < Val1 then
         Ret := IllegalProgram;
         end if;
      end if;

      -- machine.adb:43:11: warning: unused assignment, in call inlined at
      --   machine.adb:116
      --------------------------------------------------------------------
      -- Removes percieved data flow anomaly
      if -- otherwise the subtraction will be SUCCESSful
      Ret = Success then
         Regs(Rd) := Regs(Rs1) - Regs(Rs2);

      end if;


   end DoSub;

   procedure DoMul(Rd : in Reg;
                   Rs1 : in Reg;
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
      Val1 : Integer := Integer(Regs(Rs1));
      Val2 : Integer := Integer(Regs(Rs2));
      Min : Integer := Integer(DataVal'First);
      Max : Integer := Integer(DataVal'Last);
   begin
      Ret := Success;


      if -- two positive numbers cause overflow
        Val1 > 0 and Val2 > 0 then
         if Max / Val1 < Val2 then
            Ret := IllegalProgram;
         end if;
      end if;


      if Val1 < 0 and Val2 > 0 then
         if Min / Val2 > Val1 then
            Ret := IllegalProgram;
         end if;
      end if;


      if Val1 < 0 and Val2 < 0 then
         if Max / Val1 > Val2 then
            Ret := IllegalProgram;
         end if;

      end if;

      if Val1 > 0 and Val2 < 0 then
         if Min / Val1 > Val2 then
            Ret := IllegalProgram;
         end if;
      end if;

      -- machine.adb:52:11: warning: unused assignment, in call inlined at
      --  machine.adb:119
      --------------------------------------------------------------------
      -- Removes percieved data flow anomaly
      if Ret = Success then
         Regs(Rd) := Regs(Rs1) * Regs(Rs2);

      end if;


   end DoMul;

   procedure DoDiv(Rd : in Reg;
                   Rs1 : in Reg;
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
      Val1 : Integer := Integer(Regs(Rs1));
      Val2 : Integer := Integer(Regs(Rs2));
      Min : Integer := Integer(DataVal'First);
   begin
      Ret := Success;
      -- machine.adb:60:29: medium: divide by zero might fail, in call inlined
      --   at machine.adb:122 (e.g. when Regs = (others => 0))
      -- machine.adb:60:29: medium: overflow check might fail, in call inlined
      --   at machine.adb:122 (e.g. when Regs = (0 => -1, others => 0))
      ------------------------------------------------------------------------
      -- Do not allow 0 in the denominator
      if Val2 = 0 then
         Ret := IllegalProgram;
      end if;

      if (Val1 = Min and Val2 = -1) then
         Ret := IllegalProgram;
      end if;

      -- machine.adb:61:11: warning: unused assignment, in call inlined at
      -- machine.adb:122
      --------------------------------------------------------------------
      -- Removes percieved data flow anomaly
      if Ret = Success then
      Regs(Rd) := Regs(Rs1)/Regs(Rs2);
      end if;
   end DoDiv;

   procedure DoLdr(Rd : in Reg;
                   Rs : in Reg;
                   Offs : in Offset;
                   Ret : out ReturnCode) is
      A : Addr;
      Val1 : Integer := Integer(Regs(Rs));
      Val2 : Integer := Integer(Offs);
   begin

      Ret := Success;
      if Val1 < 0 and Val2 > 0 then
         if -Val2 > Val1 then
            Ret := IllegalProgram;
         end if;
      end if;

      if Val1 > 0 and Val2 < 0 then
         if Val2  < -Val1 then
            Ret := IllegalProgram;
         end if;
      end if;

      -- Despite 0 being a valid Address, SPARK kept returning this
      -- machine.adb:195:33: medium: range check might fail, in call inlined at
      --   machine.adb:303 (e.g. when A = 0)
      -- machine.adb:213:33: medium: range check might fail, in call inlined at
      --   machine.adb:312 (e.g. when A = 0)

      if Ret = Success then
        -- e.m.
          if Regs(Rs) + DataVal(Offs) <= 65535 and Regs(Rs) + DataVal(Offs) >= 0 then
            if Addr(Regs(Rs) + DataVal(Offs)) < Addr'Last and Addr(Regs(Rs) + DataVal(Offs)) > Addr'First then
              A := Addr(Regs(Rs) + DataVal(Offs));
              Regs(Rd) := Memory(A);
            end if;
          -- end if;
        end if;
      end if;


   end DoLdr;

   procedure DoStr(Ra : in Reg;
                   Offs : in Offset;
                   Rb : in Reg;
                   Ret : out ReturnCode) is
      A : Addr;
   begin
      Ret := Success;


      if Ret = Success then
        -- e.m
        if Regs(Ra) + DataVal(Offs) <= 65535 and Regs(Ra) + DataVal(Offs) >= 0 then
          if Addr(Regs(Ra) + DataVal(Offs)) < Addr'Last and Addr(Regs(Ra) + DataVal(Offs)) > Addr'First then
              A := Addr(Regs(Ra) + DataVal(Offs));
              Memory(A) := Regs(Rb);
          end if;
        end if;
      end if;

   end DoStr;

   procedure DoMov(Rd : in Reg;
                   Offs : in Offset;
                   Ret : out ReturnCode) is
   begin
      Ret := Success;
      -- Redundancy to eliminate:
      -- machine.adb:236:11: warning: unused assignment, in call inlined
      --   at machine.adb:332
      if Ret = Success then
         Regs(Rd) := DataVal(Offs);
      end if;
   end DoMov;

   procedure ValidMemAccess(Rs : in Reg;
                            Offs : in Offset;
                           MemAccessAllowed : out Boolean) is
      -- MinVal : Integer := Integer(DataVal'First);
      MaxVal : Integer := Integer(DataVal'Last);
      MaxOffs : Integer := Integer(Offset'Last);
      --MinOffs : Integer := Integer(Offset'First);
      Val1 : Integer := Integer(Regs(Rs));
      Val2 : Integer := Integer(Offs);
   begin
      MemAccessAllowed := False;
      if -- If two positive numbers ...
        Val1 > 0 and Val2 > 0 then
        -- sum to a numeber greater than DataVal/Integer
        -- ILLEGALPROGRAM
         if MaxVal - Val1 < Val2 or MaxOffs - Val1 < Val2 then
            MemAccessAllowed := False;
         end if;
      elsif -- If two negative numbers ...
         Val1 <= 0 and Val2 <= 0 then
            MemAccessAllowed := False;
      elsif val1 < 0 and val2 > 0 then
         if -val2 >= val1 then
           MemAccessAllowed := False;
         end if;
      elsif val1 > 0 and val2 < 0 then
         if val2 <= -val1 then
           MemAccessAllowed := False;
         end if;
      else
         MemAccessAllowed := True;
      end if;




   end ValidMemAccess;


   procedure ExecuteProgram(Prog : in Program;
                            Cycles : in Integer;
                            Ret : out ReturnCode;
                            Result : out Integer)
   is
      CycleCount : Integer := 0;
      Inst : Instr;
      MemAccessAllowed : Boolean;
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
               ValidMemAccess(Inst.LdrRs, Inst.LdrOffs, MemAccessAllowed);
               if  MemAccessAllowed then
                  DoLdr(Inst.LdrRd,Inst.LdrRs,Inst.LdrOffs,Ret);
                  IncPC(Ret,1);
               else
                  Ret := IllegalProgram;
               end if;

            when STR =>
               ValidMemAccess(Inst.StrRa, Inst.StrOffs, MemAccessAllowed);
               if  MemAccessAllowed then
                  DoStr(Inst.StrRa,Inst.StrOffs,Inst.StrRb,Ret);
                  IncPC(Ret,1);
               else
                   Ret := IllegalProgram;
               end if;


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
                                   Cycles : in Integer) return Boolean
   is

      -- Integer subtype in the range of ProgramCounter for easy incrementation,
      -- comparison and indexing Program instructions
      subtype InstrCount is Integer range 1..Cycles;
      Count : Integer := Integer(InstrCount'First); --e.m.
      Inst : Instr;
      MinVal : Integer := Integer(DataVal'First);
      MaxVal : Integer := Integer(DataVal'Last);
      MaxOffs : Integer := Integer(Offset'Last);
      -- Array that tracks values MOV'ed into registers adn the manipulation
      -- on these known register values.
      RegTracker : array (Reg) of DataVal := (others => 0);

      MemTracker : array (Addr) of DataVal := (others => 0);
      -- Array that tracks if a register value is known or not, useful if
      -- user inputs 0, or 1 register is unknown and 1 register is known
      -- to be 0. This will not lead to overflow in any case, except if
      -- 0 is the known denoMinator.
      RegKnown : array (Reg) of Boolean := (others => False);
      Val1 : Integer;
      Val2 : Integer;

   begin


      while(Count < Cycles) loop

        if Count > 0 and Count < 65537 then --e.m.
         Inst := Prog(ProgramCounter(Count));

         -- Pragma Loop_Invariant (Count < Cycles and Count > 0);
         case Inst.Op is
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
               Val1 := Integer(Inst.JmpOffs);
               if Val1 > 0 then
                  if MaxVal - Val1 < Count then
                     Return True;
                  end if;
               end if;
               if
                 -- Jumps outside of the line limit or below 1
                 Integer(Inst.JmpOffs) + Count >= Cycles or
                 Integer(Inst.JmpOffs) + Count < 1
               then
                  Return True;
               else
                  Count := Count + Integer(Inst.JmpOffs);
               end if;



            when JZ =>
               if Integer(Inst.JzOffs) > 0 then
                  if MaxVal - Integer(Inst.JzOffs) < Count then
                     return True;
                  end if;
               elsif
                 -- Jumps outside of the line limit or below 1
                 Integer(Inst.JzOffs) + Count >= Cycles or
                 Integer(Inst.JzOffs) + Count < 1 or
                 -- The register value is unknown
                 not RegKnown(Inst.JzRa)
               then
                  return True;
               else
                  Count := Count + Integer(Inst.JzOffs);
               end if;


            -- if a return statement is reached and no other condition
            -- has been breached then the program is valid
            when RET =>
               return False;

            when ADD =>

               if -- If both registers are known then see if add is succeessful
                 (RegKnown(Inst.AddRs1) and RegKnown(Inst.AddRs2))

               then
                  Val1 := Integer(RegTracker(Inst.AddRs1));
                  Val2 := Integer(RegTracker(Inst.AddRs2));
                  if -- If two positive numbers ...
                    Val1 > 0 and Val2 > 0 then
                     -- sum to a numeber greater than DataVal/Integer
                     -- ILLEGALPROGRAM
                     if MaxVal - Val1 < Val2 then
                        Return True;
                     end if;
                  end if;



                  if -- If two negative numbers ...
                    Val1 < 0 and Val2 < 0 then
                     -- sum to a number smaller  than DataVal/Integer
                     -- ILLEGALPROGRAM
                     if MinVal - Val1 > Val2 then
                        Return True;
                     end if;
                  end if;

                  RegTracker(Inst.AddRd) := RegTracker(Inst.AddRs1)
                    + RegTracker(Inst.AddRs2);
                  Count := Count + 1;

               elsif -- Any n + 0 = n -> Valid
                 ((not RegKnown(Inst.AddRs1)) and
                    Integer(RegTracker(Inst.AddRs2)) = 0 and
                      RegKnown(Inst.AddRs2)) or
                 ((not RegKnown(Inst.AddRs2)) and
                    Integer(RegTracker(Inst.AddRs1)) = 0 and
                      RegKnown(Inst.AddRs1))
               then
                  RegTracker(Inst.AddRd) := RegTracker(Inst.AddRs1)
                    + RegTracker(Inst.AddRs2);
                  Count := Count + 1;
               else
                 Return True;
               end if;



            when MUL =>
               Val1 := Integer(RegTracker(Inst.MulRs1));
               Val2 := Integer(RegTracker(Inst.MulRs2));
               if
                 -- Reject if a register is unknown, or product of 2 known
                 -- registers lead to overflow or ...
                 RegKnown(Inst.MulRs1) and RegKnown(Inst.MulRs2)
               then
                   if Val1 > 0 and Val2 > 0 then
                     if MaxVal / Val1 < Val2 then
                        Return True;
                     end if;
                  end if;
                  if Val1 < 0 and Val2 > 0 then

                     if MinVal / Val2 > Val1 then
                        Return True;
                     end if;
                  end if;

                  if Val1 < 0 and Val2 < 0 then
                     if MaxVal / Val1 > Val2 then
                        Return True;
                     end if;
                  end if;

                  if Val1 > 0 and Val2 < 0 then
                     if MinVal / Val1 > Val2 then
                        Return True;
                     end if;
                  end if;

                  RegTracker(Inst.MulRd) := RegTracker(Inst.MulRs1)
                    * RegTracker(Inst.MulRs2);
                  Count := Count + 1;

               elsif (RegKnown(Inst.MulRs1) and Val1 = 0 and
                        not RegKnown(Inst.MulRs2)) or
                 (RegKnown(Inst.MulRs2) and Val2 = 0 and
                        not RegKnown(Inst.MulRs1))
               then
                  RegTracker(Inst.MulRd) := RegTracker(Inst.MulRs1)
                    * RegTracker(Inst.MulRs2);
                  Count := Count + 1;
               else
                  Return True;
               end if;




            when DIV =>
               -- The denoMinator is set to zero (either assumed 0 if unknown
               -- or checked if entered by MOV)
               Val1 := Integer(RegTracker(Inst.DivRs1));
               Val2 := Integer(RegTracker(Inst.DivRs2));
               if
                 RegKnown(Inst.DivRs2) and RegKnown(Inst.DivRs1)
                 and ((Val2 = -1 and Val1 = MinVal) or
                         (Val1 = -1 and Val2 = MinVal))
               then
                  Return True;

               elsif RegKnown(Inst.DivRs1) and RegKnown(Inst.DivRs2) and
                 RegTracker(Inst.DivRs2) /= 0
               then
                  RegTracker(Inst.DivRd) := RegTracker(Inst.DivRs1)
                    / RegTracker(Inst.DivRs2);
                  Count := Count + 1;
               else
                  Return True;
               end if;

            -- makes sure that the difference of two valid values does not
            -- overflow nor underflow
            when SUB =>
               if
                 -- reject if both register values are known, or their
                 -- subtraction leads to overflow or ...
                 RegKnown(Inst.SubRs1) and RegKnown(Inst.SubRs2)
               then
                  Val1 := Integer(RegTracker(Inst.SubRs1));
                  Val2 := Integer(RegTracker(Inst.SubRs2));
                  if -- If a positive number is subtracted from a negative number ...
                    Val1 < 0 and Val2 > 0 then
                     if MinVal + Val2 > Val1 then
                        -- and that positive number will go lower than DataVal/Integer
                        -- ILLEGALPROGRAM
                        Return True;
                     end if;

                  elsif -- If a negative number is subtracted from a positive number ...
                    Val1 > 0 and Val2 < 0 then
                     if -(MaxVal + Val2) <= - Val1 then
                        -- and that positive number will go lower than DataVal/Integer
                        -- ILLEGALPROGRAM
                        Return True;
                     end if;



                  elsif -- If two negative numbers cause overflow => ILLEGALPROGRAM
                     -- 0 included as [(0 - Min => overflow) => ILLEGALPROGRAM]
                    Val1 = 0 and Val2 < 0 then
                     if MaxVal + Val2 < Val1 then
                        Return True;
                     end if;
                  else

                  RegTracker(Inst.SubRd) := RegTracker(Inst.SubRs1)
                    - RegTracker(Inst.SubRs2);
                     Count := Count + 1;
                  end if;


               elsif RegKnown(Inst.SubRs2) and
                 Integer(RegTracker(Inst.SubRs2)) = 0
                 and not RegKnown(Inst.SubRs1)
               then
                  RegTracker(Inst.SubRd) := RegTracker(Inst.SubRs1)
                    - RegTracker(Inst.SubRs2);
                  Count := Count + 1;
               else Return True;
               end if;



            when LDR =>

               if RegKnown(Inst.LdrRs)
                 -- The register value is known and the sum of the value and
                 -- the offset are outside of legal addresses or ...
               then
                  Val1 := Integer(RegTracker(Inst.LdrRs));
                  Val2 := Integer(Inst.LdrOffs);
                  if -- If two positive numbers ...
                    Val1 > 0 and Val2 > 0 then
                     -- sum to a numeber greater than DataVal/Integer
                     -- ILLEGALPROGRAM
                     if MaxVal - Val1 < Val2 or MaxOffs - Val1 < Val2 then

                        Return True;

                     end if;

                  elsif -- If two negative numbers ...

                    Val1 < 0 and Val2 < 0 then

                      Return True;

                  elsif val1 < 0 and val2 > 0 then

                     if -val2 > val1 then

                         Return True;

                     end if;

                  elsif val1 > 0 and val2 < 0 then

                     if val2 < -val1 then

                         Return True;

                     end if;

                  else
                    -- e.m
                    if RegTracker(Inst.LdrRs) + DataVal(Inst.LdrOffs) <= 65535 and RegTracker(Inst.LdrRs) + DataVal(Inst.LdrOffs) >= 0 then
                      if Addr(RegTracker(Inst.LdrRs) + DataVal(Inst.LdrOffs)) < Addr'Last and Addr(RegTracker(Inst.LdrRs) + DataVal(Inst.LdrOffs)) > Addr'First then
                          RegTracker(Inst.LdrRd) :=
                            MemTracker(Addr(RegTracker(Inst.LdrRs)
                              + DataVal(Inst.LdrOffs)));
                          Count := Count + 1;
                      end if;
                    end if;
                  end if;
               else
                  Return True;
               end if;



            when STR =>

               if RegKnown(Inst.StrRb)
                 -- The register value is known and the sum of the value and
                 -- the offset are outside of legal addresses or ...
               then
                  Val1 := Integer(RegTracker(Inst.StrRa));
                  Val2 := Integer(Inst.StrOffs);
                  if -- If two positive numbers ...
                    Val1 > 0 and Val2 > 0 then
                     -- sum to a numeber greater than DataVal/Integer
                     -- ILLEGALPROGRAM
                     if MaxVal - Val1 < Val2 or MaxOffs - Val1 < Val2 then

                        Return True;

                     end if;

                  elsif -- If two negative numbers ...

                    Val1 < 0 and Val2 < 0 then

                      Return True;

                  elsif val1 < 0 and val2 > 0 then

                     if -val2 > val1 then

                         Return True;

                     end if;

                  elsif val1 > 0 and val2 < 0 then

                     if val2 < -val1 then

                         Return True;

                     end if;
                  elsif val1 = 0 and val2 = 0 then
                        return True;


                  else
                    --e.m.
                    if RegTracker(Inst.StrRa) + DataVal(Inst.StrOffs) <= 65535 and RegTracker(Inst.StrRa) + DataVal(Inst.StrOffs) >= 0 then
                      if Addr(RegTracker(Inst.StrRa) + DataVal(Inst.StrOffs)) < Addr'Last and Addr(RegTracker(Inst.StrRa) + DataVal(Inst.StrOffs)) > Addr'First then
                          MemTracker(Addr(RegTracker(Inst.StrRa) + DataVal(Inst.StrOffs)))
                              := RegTracker(Inst.StrRb);
                          Count := Count + 1;
                      end if;
                    end if;
                  end if;
               end if;

            -- increment counter
            when NOP =>
               Count := Count + 1;
            end case;
        end if; --e.m.
      end loop;
      return True;
   end DetectInvalidBehaviour;

end Machine;
