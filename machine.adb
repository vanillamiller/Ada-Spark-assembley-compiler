with Instruction;
use Instruction;
with Debug; use Debug;

with Ada.Characters.Latin_1;

package body Machine with SPARK_Mode => On is

   type DataVal is range -(2**31) .. +(2**31 - 1);

   Regs : array (Reg) of DataVal := (others => 0);

   Memory : array (Addr) of DataVal := (others => 0);

   PC : ProgramCounter := ProgramCounter'First;

   procedure IncPC(Ret : out ReturnCode; Offs : in Offset) is
   begin

-- This check prevents the PC from being assigned a value that is outside of
-- its allowable range, i.e. outside the set number of lines in the program,
-- which is [1, 65536] aka [1, MAX_PROGRAM_LENGTH]
      if Integer(PC) + Integer(Offs) > Integer(ProgramCounter'Last) or
        Integer(PC) + Integer(Offs) < Integer(ProgramCounter'First) then
         Ret := IllegalProgram;
      else
         PC := ProgramCounter(Integer(PC) + Integer(Offs));
         Ret := Success;
      end if;
   end IncPC;

   -- If the sum of numbers held in R1 and R2 falls outside the range of
   -- ada's 32 bit integers, then return ILLEGALPROGRAM
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
        -- return ILLEGALPROGRAM
         if Max - Val1 < Val2 then
            Ret := IllegalProgram;
         end if;
      end if;


      if -- If two negative numbers ...
         Val1 < 0 and Val2 < 0 then
         -- sum to a number smaller than DataVal/Integer
         -- then return ILLEGALPROGRAM
         if Min - Val1 > Val2 then
            Ret := IllegalProgram;
            -- This also makes use of Ret, which was assigned but unused in then
            -- provided implementation
         end if;

      end if;
      -- Check whether Ret = 'Success' (i.e. no chance of overflow detected),
      -- if so then proceed with addition.
      if
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
      -- If the result of subtracting the value stored in R2 from the value
      -- stored in R1 will go below DataVal's range, then return ILLEGALPROGRAM.
      Ret := Success;
      if -- If a positive number is subtracted from a negative number ...
        Val1 < 0 and Val2 > 0 then
         if Min + Val2 > Val1 then
        -- and that positive number will go lower than DataVal/Integer,
        -- then return ILLEGALPROGRAM
            Ret := IllegalProgram;
         end if;
      end if;

      if -- If a negative number is subtracted from a positive number ...
        Val1 > 0 and Val2 < 0 then
         if Max + Val2 < Val1 then
        -- and that positive number will go lower than DataVal/Integer,
        -- then return ILLEGALPROGRAM
            Ret := IllegalProgram;
         end if;
      end if;

      if -- If two negative numbers cause overflow => ILLEGALPROGRAM
         -- 0 included as [(0 - Min => overflow) => ILLEGALPROGRAM]
        Val1 = 0 and Val2 < 0 then
        if Max + Val2 < Val1 then
         Ret := IllegalProgram;
         -- This also makes use of Ret, which was assigned but unused in then
        -- provided implementation
         end if;
      end if;

      -- Check whether Ret = 'Success' (i.e. no chance of overflow detected),
      -- if so then proceed with subtraction.
      if Ret = Success then
         Regs(Rd) := Regs(Rs1) - Regs(Rs2);

      end if;

   end DoSub;

   -- If the multiplication of numbers held in R1 and R2 falls outside the range of
    -- ada's 32 bit integers, then return ILLEGALPROGRAM
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
      -- The below code makes use of Ret, which was assigned but unused in then
      -- provided implementation

      -- if the product of two positive numbers cause overflow, return ILLEGALPROGRAM
      if Val1 > 0 and Val2 > 0 then
         if Max / Val1 < Val2 then
            Ret := IllegalProgram;
         end if;
      end if;

      -- if the product of one negative and one positive number causes overflow,
      -- return ILLEGALPROGRAM
      if Val1 < 0 and Val2 > 0 then
         if Min / Val2 > Val1 then
            Ret := IllegalProgram;
         end if;
      end if;

      -- if the product of two negative numbers cause overflow, return ILLEGALPROGRAM
      if Val1 < 0 and Val2 < 0 then
         if Max / Val1 > Val2 then
            Ret := IllegalProgram;
         end if;
      end if;

      -- if the product of one positive and one nagative number causes overflow,
      -- return ILLEGALPROGRAM
      if Val1 > 0 and Val2 < 0 then
         if Min / Val1 > Val2 then
            Ret := IllegalProgram;
         end if;
      end if;

      -- Check whether Ret = 'Success' (i.e. no chance of overflow detected),
      -- if so then proceed with addition.
      if Ret = Success then
         Regs(Rd) := Regs(Rs1) * Regs(Rs2);
      end if;


   end DoMul;

   -- If an instruction attempts to divide any number by zero, return
   -- 'ILLEGALPROGRAM' to indicate invalid behaviour
   procedure DoDiv(Rd : in Reg;
                   Rs1 : in Reg;
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
      Val1 : Integer := Integer(Regs(Rs1));
      Val2 : Integer := Integer(Regs(Rs2));
      Min : Integer := Integer(DataVal'First);
   begin
      Ret := Success;
      -- the below make use of Ret, which was assigned but unused in
      -- the provided implementation
      if Val2 = 0 then
         Ret := IllegalProgram;
      end if;

      if (Val1 = Min and Val2 = -1) then
         Ret := IllegalProgram;
      end if;

      -- if no zero in denominator, indicated by ret = success, then proceed
      -- with division
      if Ret = Success then
      Regs(Rd) := Regs(Rs1)/Regs(Rs2);
      end if;
   end DoDiv;

   -- check that Val1 + Val2 will compute to a valid value, and then that those
   -- values are withiin the range of a valid mem address
   procedure DoLdr(Rd : in Reg;
                   Rs : in Reg;
                   Offs : in Offset;
                   Ret : out ReturnCode) is
      A : Addr;
      Val1 : Integer := Integer(Regs(Rs));
      Val2 : Integer := Integer(Offs);
   begin
      Ret := Success;
      --below also makes use of Ret,which was assigned and unused in provided VM
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

      -- if valid mem value, confirm valid mem address
      if Ret = Success then
          if Regs(Rs) + DataVal(Offs) <= 65535 and Regs(Rs) + DataVal(Offs) >= 0 then
            if Addr(Regs(Rs) + DataVal(Offs)) < Addr'Last and Addr(Regs(Rs) + DataVal(Offs)) > Addr'First then
              A := Addr(Regs(Rs) + DataVal(Offs));
              Regs(Rd) := Memory(A);
            end if;
        end if;
      end if;
   end DoLdr;

   -- confritm that value in Rb + Offset will compute to a valid mem address
   procedure DoStr(Ra : in Reg;
                   Offs : in Offset;
                   Rb : in Reg;
                   Ret : out ReturnCode) is
      A : Addr;

   begin
      Ret := Success;
      if Ret = Success then
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
      if Ret = Success then
         Regs(Rd) := DataVal(Offs);
      end if;
   end DoMov;

   -- procedure to indicate whether or not the result of adding the value in
   -- a register Rs, to a value Offs, will be a valid memory address
   procedure ValidMemAccess(Rs : in Reg;
                            Offs : in Offset;
                           MemAccessAllowed : out Boolean) is
      MaxVal : Integer := Integer(DataVal'Last);
      MaxOffs : Integer := Integer(Offset'Last);
      Val1 : Integer := Integer(Regs(Rs));
      Val2 : Integer := Integer(Offs);
   begin
      MemAccessAllowed := False;
      if -- If two positive numbers ...
        Val1 > 0 and Val2 > 0 then
        -- sum to a numeber greater than DataVal/Integer range
        -- then return ILLEGALPROGRAM
         if MaxVal - Val1 < Val2 or MaxOffs - Val1 < Val2 then
            MemAccessAllowed := False;
         end if;
      elsif -- If two negative numbers ...
         Val1 <= 0 and Val2 <= 0 then
           -- sum to a numeber less than DataVal/Integer range
           -- then return ILLEGALPROGRAM
            MemAccessAllowed := False;

      --if two numbers, of different signs, sum to a number outside the Range
      -- of DataVal/Integer, then return ILEGALPROGRAM
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
                -- Call function to check whether the inputs will compute to
                -- a valid memory address
               ValidMemAccess(Inst.LdrRs, Inst.LdrOffs, MemAccessAllowed);
               -- if they do, perform LDR procedure
               if  MemAccessAllowed then
                  DoLdr(Inst.LdrRd,Inst.LdrRs,Inst.LdrOffs,Ret);
                  IncPC(Ret,1);
               else
                  Ret := IllegalProgram;
               end if;

            when STR =>
            -- Call function to check whether the inputs will compute to
            -- a valid memory address
               ValidMemAccess(Inst.StrRa, Inst.StrOffs, MemAccessAllowed);
              -- if they do, perform STR procedure
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
         Ret := CyclesExhausted;
      end if;

   end ExecuteProgram;

   -- ****************** ANALYSIS FUNCTION: Overall Design ******************
   -- Our analysis function implements a static analysis on the program. It
   -- keeps track of what registers have known values, what these values are,
   -- and any values that have been written to memory. This is important,
   -- because the analysis function must work on any initial state - and this
   -- initial state is unknown to the analysis function. Therefore, any attempt
   -- to perform operations on an unknown register (with a few noted exceptions,
   --  discussed below) must be flagged as invalid behaviour.
   -- Key data structures of our function include:
   --   1. ‘RegTracker’: an array that represents all valid registers (i.e.
   --       regs 0 … 31, inclusive), and tracks any values assigned to there
   --       registers, and any manipulation on registers with known values.
   --   2. ‘MemTracker’: an array of all valid memory addresses (e.g. address
   --       0 … 65535, inclusive), and holds any values that are written to
   --       memory- which can also be used to check whether a memory address is
   --       known and can be read from.
   --   3. ‘RegKnown’: an array that represents all valid registers (i.e. regs
   --       0 … 31, inclusive), where each register is assigned a boolean value
   --       indicating whether or its value is known.
   --   4. ‘Count’: a variable whose type is an integer within the same range as
   --       ProgramCounter. ‘Count’ keeps track of the number of statements
   --       executed by the program and is useful for easy incrementation,
   --       comparison and indexing of Program instructions
   -- Key features of our analysis include:
   --   1. The analysis function checks whether a register is known, and if not
   --      it returns ‘true’ for illegal behaviour. An exception to this, is if
   --      there is a zero in the equation (because 0 multiplied by any other
   --      number will equal 0, which is valid.
   --   2. The analysis function also checks whether or not any array reference
   --      is out of bounds (for example, the array of Regs or mem addresses).


   function DetectInvalidBehaviour(Prog : in Program;
                                   Cycles : in Integer) return Boolean
   is

      -- Integer subtype in the range of ProgramCounter for easy incrementation,
      -- comparison and indexing Program instructions
      subtype InstrCount is Integer range 1..Cycles;
      Count : Integer := Integer(InstrCount'First);
      Inst : Instr;
      MinVal : Integer := Integer(DataVal'First);
      MaxVal : Integer := Integer(DataVal'Last);
      MaxOffs : Integer := Integer(Offset'Last);
      -- Array that tracks values MOV'ed into registers and the manipulation
      -- on these known register values.
      RegTracker : array (Reg) of DataVal := (others => 0);
      -- Array that tracks values assigned to addresses in memory
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

        if Count > 0 and Count < 65537 then
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
                    -- check valid mem address
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
                    -- check valid mem address
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
        end if;
      end loop;
      return True;
   end DetectInvalidBehaviour;

end Machine;
