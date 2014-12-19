package cs536.codegen.mips;

/** Constants for floating point registers */
public enum FpRegs
{
        // Floating point registers (*only even numbers are available for doubles*)

        f0, // used for float/double returns (or use the stack)
        f1,
        f2,
        f3,
        f4,
        f5,
        f6,
        f7,
        f8,
        f9,
        f10,
        f11,
        f12, // $f12 and $f14 used for float/double arguments
        f13,
        f14,
        f15,
        f16,
        f17,
        f18,
        f19,
        f20,
        f21,
        f22,
        f23,
        f24,
        f25,
        f26,
        f27,
        f28,
        f29,
        f30,
        f31
        ;
}