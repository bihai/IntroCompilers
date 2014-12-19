package cs536.codegen.mips;

import cs536.codegen.AsmChunk;

public class MipsChunk extends AsmChunk
{
    private Mips mips = new Mips();

    //! Pop the top word from the stack into @reg
    /*!

      This handles the double action required in a RISC arch of reading
      the value and then messing with the stack pointer.

      \param reg The register into which the word should be stored
    */
    public void popToReg(Regs reg)
    {
        append(mips.lw(reg, 0, Regs.sp));
        popBytes(4);
    }

    //! Pop n bytes off of the stack (discard)
    /*!

      \param byteCount Number of byte to discard
    */
    public void popBytes(int byteCount)
    {
        append(mips.addi(Regs.sp, Regs.sp, byteCount));
    }

    //! Push the value in @reg onto the stack
    /*!

      Handles the modification of the stack pointer and then the copy.
      The register name should include the $ sigil.

      \param reg The register whose contents should be pushed onto the
      stack.
    */
    public void pushReg(Regs reg)
    {
        // Reserve space
        reserveStackSpace(4);

        // Now write
        append(mips.sw(reg, 0, Regs.sp));
    }

    public void pushReg(Regs reg, String comment)
    {
        append(mips.comment(comment));
        pushReg(reg);
    }

    //! Reserve a number of bytes on top of the stack
    /*!

      The initial value of this space is undefined

      \param byteCount Number of bytes to reserve
    */
    public void reserveStackSpace(int byteCount)
    {
        // We add (-byteCount) instead of subtracting byteCount
        // because the Mips class doesn't have the subi
        // pseudoinstruction.
        append(mips.addi(Regs.sp, Regs.sp, -byteCount));
    }
    
    // Mike: put the given callCode into $v0, then do a syscall.
    public void syscall(int callCode)
    {
        this.append(mips.li(Regs.v0, callCode));
        this.append(mips.syscall());
    }
    
    // Mike: like popToReg, but pops an 8-byte double.
    public void popToFpReg(FpRegs reg)
    {
        append(mips.l_d(reg, 0, Regs.sp));
        popBytes(8);
    }
    
    // Mike: like pushReg, but pushes an 8-byte double.
    public void pushFpReg(FpRegs reg)
    {
        // Reserve space
        reserveStackSpace(8);

        // Now write
        append(mips.s_d(reg, 0, Regs.sp));
    }
    
    // Mike: push the FPU condition flag 0 onto the stack.
    // This uses t0 and t1.
    public void pushConditionFlag0()
    {
        append(mips.li(Regs.t0, 1));
        append(mips.li(Regs.t1, 0));
        append(mips.movt(Regs.t1, Regs.t0));
        pushReg(Regs.t1);
    }
}
