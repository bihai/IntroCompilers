package cs536.codegen.mips;

import java.util.List;
import java.util.Iterator;
import java.util.ListIterator;

import cs536.ast.SourceLocation;
import cs536.ast.AstVisitor;
import static cs536.ast.AbstractSyntaxTree.*;
import cs536.ast.AbstractSyntaxTree;

import cs536.codegen.*;

import cs536.staticSemantics.types.*;
import cs536.staticSemantics.symbolTable.*;

public class CodeGenerator extends AstVisitor<MipsChunk>
{
    Mips mips = new Mips();
    
    public MipsChunk visitAst(AbstractSyntaxTree ast, List<MipsChunk> decls)
    {
        MipsChunk chunk = new MipsChunk();
        for (MipsChunk c : decls) {
            chunk.appendChunk(c);
        }
        return chunk;
    }
    
    // done. optional: initializers
    public MipsChunk visitGlobalVarDecl(GlobalVarDecl globalVar,
                                      MipsChunk typeResult, MipsChunk exprResult)
    {
        //           .data
        //           .align 2  # align on a word boundary
        //   _v:     .space N
        
        MipsChunk chunk = new MipsChunk();
        
        chunk.append(mips.comment("global variable"));
        chunk.append(mips._data());
        chunk.append(mips._align(2));
        chunk.append(mips.label("_" + globalVar.getName()));
        chunk.append(mips._space(globalVar.getGlobalSymbol().getType().getSize()));
        // works with doubles.
        
        return chunk;
    }
    
    private String mainError = "0:0:no main function";
    
    public String getMainError() {
        return mainError;
    }

    // Done, I think
    public MipsChunk visitFuncDecl(FuncDecl funcDecl, MipsChunk typeResult,
                                 List<MipsChunk> formalResults, List<MipsChunk> statementResults)
    {
        // For every function you will generate code for:
        //   the function "preamble"
        //   the function entry (to set up the function’s Activation Record)
        //   the function body (its statements)
        //   function exit (restoring the stack, and returning to the caller).
        
        MipsChunk chunk = new MipsChunk();
        
        int localsSize = funcDecl.getFunctionSymbol().getLocalsSize();
        int argsSize = 0;
        for (FormalArg f : funcDecl.getFormalArguments()) {
            argsSize += f.getLocalSymbol().getType().getSize();
        }
        
        // Preamble: for the main function, generate
        //     .text
        //     .globl main
        //   main:
        // for all other functions:
        //     .text
        //   _functionname:
        
        chunk.append(mips.comment("function preamble"));
        chunk.append(mips._text());
        if (funcDecl.getName().equals("main")) {
            chunk.append(mips._globl("main"));
            chunk.append(mips.label("main"));
            
            // Because this is the main function, this is where we make sure the
            // main function is "void main()".
            if (funcDecl.getFunctionSymbol().getType().getArgumentTypes().size() != 0) {
                mainError = funcDecl.getLocation() + ":main must take no arguments";
            }
            else if (funcDecl.getFunctionSymbol().getType().getReturnType() != null) {
                mainError = funcDecl.getLocation() + ":main must return void";
            }
            else {
                // main function is good, no error.
                mainError = null;
            }
        }
        else {
            chunk.append(mips.label("_" + funcDecl.getName()));
        }
        
        // function entry: Here’s the code you need to generate:
        //   # (1) push return addr
        //     subu $sp, $sp, 4
        //     sw   $ra, 0($sp)
        //   # (2) "push" space for locals
        //     subu $sp, $sp, <total size of locals in bytes>
        //   # (3) push control link
        //     subu $sp, $sp, 4
        //     sw   $fp, 0($sp)
        //   # (4) set the FP
        //   # note: the following sets the FP to point to first local.
        //   #       The reason for "+ 4" is 4 bytes for the saved FP
        //     addu $fp, $sp, 4
        
        chunk.append(mips.comment("function entry"));
        chunk.pushReg(Regs.ra);
        chunk.reserveStackSpace(localsSize);
        chunk.pushReg(Regs.fp);
        chunk.append(mips.addu(Regs.fp, Regs.sp, 4));
        
        // function body: just append each of the statements' chunks
        
        chunk.append(mips.comment("function body"));
        for (MipsChunk c : statementResults) {
            chunk.appendChunk(c);
        }
        
        // function exit:
        //  lw   $ra, <size of locals>($fp) # load return address
        //  move $t0, $fp                   # save control link
        //  lw   $fp, -4($fp)               # restore FP
        //  add  $sp, $sp, <size of locals + size of params + 8> # restore SP to just below the parameters
        //  jr   $ra                        # return
        
        chunk.append(mips.label("exit_" + fname));
        chunk.append(mips.comment("function exit"));
        chunk.append(mips.lw(Regs.ra, localsSize, Regs.fp));
        chunk.append(mips.move(Regs.t0, Regs.fp));
        chunk.append(mips.lw(Regs.fp, -4, Regs.fp));
        chunk.append(mips.add(Regs.sp, Regs.sp, localsSize + argsSize + 8));
        chunk.append(mips.jr(Regs.ra));
        
        return chunk;
    }

    public MipsChunk visitFormalArg(FormalArg formalArg, MipsChunk typeResult)
    {
        // nothing
        return null;
    }

    // Done. Handles doubles, and does int->double promotions.
    public MipsChunk visitVarDeclStmt(VarDeclStmt localVar,
                                    MipsChunk typeResult, MipsChunk initResult)
    {
        MipsChunk chunk = new MipsChunk();
        LocalSymbol sym = localVar.getLocalSymbol();
        
        // Only do anything if there's an initializer
        if (initResult != null) {
            // run the initializer
            chunk.appendChunk(initResult);
            // then pop the result, and store it at the appropriate offset
            if (sym.getType() == DoubleType.make()) {
                // If the result is an int, convert to double.
                if (localVar.getInitializer().getType() == IntType.make()) {
                    chunk.popToReg(Regs.t0);
                    chunk.append(mips.mtc1(Regs.t0, FpRegs.f0));
                    chunk.append(mips.cvt_d_w(FpRegs.f0, FpRegs.f0));
                }
                // otherwise just pop the double.
                else {
                    chunk.popToFpReg(FpRegs.f0);
                }
                chunk.append(mips.sdc1(FpRegs.f0, sym.getOffset(), Regs.fp));
            }
            else /* type is int, bool, or string */ {
                chunk.popToReg(Regs.t0);
                chunk.append(mips.sw(Regs.t0, sym.getOffset(), Regs.fp));
            }
        }
        
        return chunk;
    }
    
    private String fname;
    private Type ftype;
    
    // Used to store the current function name, so return statements work.
    // Also stores the function return type, to convert int->double correctly.
    public void preVisit(FuncDecl decl) {
        fname = decl.getName();
        ftype = decl.getFunctionSymbol().getType().getReturnType();
    }
    
    // Done. works with doubles, and also promotes int->double.
    public MipsChunk visitReturnStmt(ReturnStmt retStmt, MipsChunk valResult)
    {
        MipsChunk chunk = new MipsChunk();
        // First, put the return value, if there is one, in $v0 or $f0.
        if (retStmt.hasExpression()) {
            chunk.appendChunk(valResult);
            if (ftype == DoubleType.make()) {
                // if return value is already double, just pop to $f0.
                if (retStmt.getExpression().getType() == DoubleType.make()) {
                    chunk.popToFpReg(FpRegs.f0);
                }
                // otherwise, convert it.
                else {
                    chunk.popToReg(Regs.t0);
                    chunk.append(mips.mtc1(Regs.t0, FpRegs.f0));
                    chunk.append(mips.cvt_d_w(FpRegs.f0, FpRegs.f0));
                }
            }
            else {
                chunk.popToReg(Regs.v0);
            }
        }
        
        // Then, just jump to the function exit.
        chunk.append(mips.b("exit_" + fname));
        
        return chunk;
    }
    
    LabelGenerator elseGen = new LabelGenerator("Else");
    LabelGenerator afterElseGen = new LabelGenerator("AfterElse");

    // Done.
    public MipsChunk visitIfStmt(IfStmt ifStmt, MipsChunk conditionResult,
                               List<MipsChunk> thenResults, List<MipsChunk> elseResults)
    {
        MipsChunk chunk = new MipsChunk();
        
        String elseL = elseGen.genLabel();
        String afterElseL = afterElseGen.genLabel();
        
        chunk.append(mips.comment("line " + ifStmt.getLocation().getRow() + ": if statement"));
        
        // First, evaluate the condition and pop the bool (0 or 1) to $t0.
        chunk.appendChunk(conditionResult);
        chunk.popToReg(Regs.t0);
        // If the bool is 0 (false), branch to the "else" label.
        chunk.append(mips.beqz(Regs.t0, elseL));
        // Otherwise evaluate the "then" statements, and branch to "after else".
        for (MipsChunk c : thenResults) {
            chunk.appendChunk(c);
        }
        chunk.append(mips.b(afterElseL));
        // At the "else" label, evaluate the "else" statements and continue.
        chunk.append(mips.label(elseL));
        for (MipsChunk c : elseResults) {
            chunk.appendChunk(c);
        }
        chunk.append(mips.label(afterElseL));
        
        return chunk;
    }
    
    LabelGenerator beginGen = new LabelGenerator("BeginLoop");
    LabelGenerator endGen = new LabelGenerator("EndLoop");

    // Done.
    public MipsChunk visitWhileStmt(WhileStmt whileStmt, MipsChunk conditionResult,
                                  List<MipsChunk> bodyResults)
    {
        MipsChunk chunk = new MipsChunk();
        
        chunk.append(mips.comment("line " + whileStmt.getLocation().getRow() + ": while statement"));
        
        String beginL = beginGen.genLabel();
        String endL = endGen.genLabel();
        
        // At the beginning of the loop, evaluate the condition.
        chunk.append(mips.label(beginL));
        chunk.appendChunk(conditionResult);
        // Pop it to $t0, and if it's 0 (false), jump to the "end" label.
        chunk.popToReg(Regs.t0);
        chunk.append(mips.beqz(Regs.t0, endL));
        // Otherwise, evaluate the loop body, and then jump back to "begin".
        for (MipsChunk c : bodyResults) {
            chunk.appendChunk(c);
        }
        chunk.append(mips.b(beginL));
        chunk.append(mips.label(endL));
        
        return chunk;
    }

    // Done, for all types.
    public MipsChunk visitExpressionStmt(ExpressionStmt exprStmt, MipsChunk expResult)
    {
        // Just evaluate the expression, and then pop its value off.
        MipsChunk chunk = new MipsChunk();
        chunk.appendChunk(expResult);
        // Make sure to pop off the right number of bytes.
        chunk.popBytes(exprStmt.getExpression().getType().getSize());
        return chunk;
    }

    // Done, for ints, strings, and doubles. Optional: bools
    public MipsChunk visitWriteStmt(WriteStmt writeStmt, MipsChunk valResult)
    {
        MipsChunk chunk = new MipsChunk();
        
        chunk.append(mips.comment("line " + writeStmt.getLocation().getRow() + ": write statement"));
        
        chunk.appendChunk(valResult);
        
        Type t = writeStmt.getExpression().getType();
        
        if (t == IntType.make()) {
            // writing an int
            chunk.popToReg(Regs.a0);
            chunk.syscall(1);
        }
        else if (t == StringType.make()) {
            // writing a string
            chunk.popToReg(Regs.a0);
            chunk.syscall(4);
        }
        else if (t == DoubleType.make()) {
            // writing a double
            chunk.popToFpReg(FpRegs.f12);
            chunk.syscall(3);
        }
        
        return chunk;
    }

    // Done for ints, bools (0 and 1), and doubles.
    public MipsChunk visitReadStmt(ReadStmt readStmt)
    {    
        MipsChunk chunk = new MipsChunk();
        
        VariableSymbol sym = readStmt.getDestinationSymbol();
        if (sym.getType() == IntType.make() || sym.getType() == BoolType.make()) {
            chunk.syscall(5);
            // int is now in $v0
            if (sym instanceof LocalSymbol) {
                int offset = ((LocalSymbol) sym).getOffset();
                chunk.append(mips.sw(Regs.v0, offset, Regs.fp));
            }
            else if (sym instanceof GlobalSymbol) {
                chunk.append(mips.sw(Regs.v0, "_" + sym.getName()));
            }
        }
        else if (sym.getType() == DoubleType.make()) {
            chunk.syscall(7);
            // double is now in $f0
            if (sym instanceof LocalSymbol) {
                int offset = ((LocalSymbol) sym).getOffset();
                chunk.append(mips.s_d(FpRegs.f0, offset, Regs.fp));
            }
            else if (sym instanceof GlobalSymbol) {
                chunk.append(mips.s_d(FpRegs.f0, "_" + sym.getName()));
            }
        }
        
        return chunk;
    }

    // Done. Works for double return values, and promotions.
    public MipsChunk visitFunctionCallExp(FunctionCallExp fcallExp,
                                        List<MipsChunk> argResults)
    {
        MipsChunk chunk = new MipsChunk();
        chunk.append(mips.comment("line " + fcallExp.getLocation().getRow() + ": function call"));
        
        // using $v0 and $f0 for return values.
        
        // Step 1: nothing, since we're using registers for return values.
        
        // Step 2: evaluate each parameter.
        // we also have to do int->double promotions.
        
        Iterator<MipsChunk> itChunks = argResults.iterator();
        Iterator<Type> itExpected = fcallExp.getFunctionSymbol().getType().getArgumentTypes().iterator();
        Iterator<Expression> itActual = fcallExp.getActualArguments().iterator();
        
        while (itChunks.hasNext()) {
            MipsChunk c = itChunks.next();
            Type exp = itExpected.next();
            Type act = itActual.next().getType();
            
            chunk.appendChunk(c);
            if (exp == DoubleType.make() && act == IntType.make()) {
                // pop int off stack, convert to double, push onto stack.
                chunk.popToReg(Regs.t0);
                chunk.append(mips.mtc1(Regs.t0, FpRegs.f0));
                chunk.append(mips.cvt_d_w(FpRegs.f0, FpRegs.f0));
                chunk.pushFpReg(FpRegs.f0);
            }
        }
        
        // Step 3: jump-and-link to the function.
        String fnName = fcallExp.getFunctionSymbol().getName();
        chunk.append(mips.jal(fnName.equals("main") ? "main" : ("_" + fnName)));
        
        // Step 4: push return value onto the stack.
        if (fcallExp.getFunctionSymbol().getType().getReturnType() == DoubleType.make())
            chunk.pushFpReg(FpRegs.f0);
        else
            chunk.pushReg(Regs.v0);

        return chunk;
    }

    public MipsChunk visitDerefOp(DerefOp derefOp, MipsChunk expResult)
    {
        // Not fundamental
        return null;
    }

    // done. works with ints and doubles.
    public MipsChunk visitNegationOp(NegationOp negationOp, MipsChunk expResult)
    {
        MipsChunk chunk = new MipsChunk();
        chunk.appendChunk(expResult);
        
        Type t = negationOp.getOperand().getType();
        if (t == IntType.make()) {
            chunk.popToReg(Regs.t0);
            chunk.append(mips.neg(Regs.t0, Regs.t0));
            chunk.pushReg(Regs.t0);
        }
        else if (t == DoubleType.make()) {
            chunk.popToFpReg(FpRegs.f0);
            chunk.append(mips.neg_d(FpRegs.f0, FpRegs.f0));
            chunk.pushFpReg(FpRegs.f0);
        }
        return chunk;
    }

    // TODO: variables of type int or double. also note pre/post op
    public MipsChunk visitIncrementOp(IncrementOp incrOp, MipsChunk expResult)
    {
        MipsChunk chunk = new MipsChunk();
        return chunk;
    }

    public MipsChunk visitDecrementOp(DecrementOp decrOp, MipsChunk expResult)
    {
        // Not fundamental
        return null;
    }

    // Done.
    public MipsChunk visitNotOp(NotOp logNotOp, MipsChunk expResult)
    {
        MipsChunk chunk = new MipsChunk();
        chunk.appendChunk(expResult);
        chunk.popToReg(Regs.t0);
        // if $t0 == 0, $t0 = 1. else $t0 = 0.
        chunk.append(mips.seq(Regs.t0, Regs.t0, 0));
        chunk.pushReg(Regs.t0);
        return chunk;
    }

    public MipsChunk visitAddrOfOp(AddrOfOp addrOfOp, MipsChunk expResult)
    {
        // Not fundamental
        return null;
    }
    
    LabelGenerator shortCircuitGen = new LabelGenerator("ShortCircuit");

    // Done
    public MipsChunk visitAndOp(AndOp logAndOp, MipsChunk lhsResult, MipsChunk rhsResult)
    {
        String shortCL = shortCircuitGen.genLabel();
    
        MipsChunk chunk = new MipsChunk();
        // First evaluate the lhs, and *copy* it to $t0
        chunk.appendChunk(lhsResult);
        chunk.append(mips.lw(Regs.t0, Regs.sp));
        // Then, if $t0 is false, branch to shortCL.
        chunk.append(mips.beqz(Regs.t0, shortCL));
        // Otherwise eval the rhs onto the stack.
        chunk.appendChunk(rhsResult);
        // If we branch to shortCL, the 0 just gets left on the stack.
        chunk.append(mips.label(shortCL));
        
        return chunk;
    }

    // Done
    public MipsChunk visitOrOp(OrOp logOrOp, MipsChunk lhsResult, MipsChunk rhsResult)
    {
        String shortCL = shortCircuitGen.genLabel();
    
        MipsChunk chunk = new MipsChunk();
        // First evaluate the lhs, and *copy* it to $t0
        chunk.appendChunk(lhsResult);
        chunk.append(mips.lw(Regs.t0, Regs.sp));
        // Then, if $t0 is true, branch to shortCL.
        chunk.append(mips.bnez(Regs.t0, shortCL));
        // Otherwise eval the rhs onto the stack.
        chunk.appendChunk(rhsResult);
        // If we branch to shortCL, the 1 just gets left on the stack.
        chunk.append(mips.label(shortCL));
        
        return chunk;
    }

    // Done, for variable lvalues. Works with globals+locals, and doubles, and promotes.
    public MipsChunk visitAssignOp(AssignOp assignOp, MipsChunk lhsResult, MipsChunk rhsResult)
    {
        MipsChunk chunk = new MipsChunk();
        chunk.append(mips.comment("line " + assignOp.getLocation().getRow() + ": assignment"));
        // First evaluate the expression.
        chunk.appendChunk(rhsResult);
        // Now, check the kind of lvalue.
        Expression lhs = assignOp.getLhsOperand();
        if (lhs instanceof VarRefExp) {
            VariableSymbol sym = ((VarRefExp) lhs).getVariableSymbol();
            if (sym.getType() == DoubleType.make()) {
                // Get the double into $f0, possibly converting from int.
                if (assignOp.getRhsOperand().getType() == IntType.make()) {
                    chunk.popToReg(Regs.t0);
                    chunk.append(mips.mtc1(Regs.t0, FpRegs.f0));
                    chunk.append(mips.cvt_d_w(FpRegs.f0, FpRegs.f0));
                }
                else {
                    chunk.popToFpReg(FpRegs.f0);
                }
                // Write it to the appropriate memory location.
                if (sym instanceof GlobalSymbol) {
                    chunk.append(mips.s_d(FpRegs.f0, "_" + sym.getName()));
                }
                else if (sym instanceof LocalSymbol) {
                    int offset = ((LocalSymbol) sym).getOffset();
                    chunk.append(mips.s_d(FpRegs.f0, offset, Regs.fp));
                }
                // Push the double back onto the stack.
                chunk.pushFpReg(FpRegs.f0);
            }
            else {
                // Pop word off the stack.
                chunk.popToReg(Regs.t0);
                // Write it to the appropriate memory location.
                if (sym instanceof GlobalSymbol) {
                    chunk.append(mips.sw(Regs.t0, "_" + sym.getName()));
                }
                else if (sym instanceof LocalSymbol) {
                    int offset = ((LocalSymbol) sym).getOffset();
                    chunk.append(mips.sw(Regs.t0, offset, Regs.fp));
                }
                // Push the word back onto the stack.
                chunk.pushReg(Regs.t0);
            }
        }
        else {
            chunk.append(mips.comment("Unsupported lvalue"));
        }
        return chunk;
    }

    // done. for ints, bools, and doubles.
    // also converts ints to doubles if needed.
    public MipsChunk visitEqualsOp(EqualsOp equalsOp, MipsChunk lhsResult, MipsChunk rhsResult)
    {
        MipsChunk chunk = new MipsChunk();
        
        chunk.appendChunk(lhsResult);
        chunk.appendChunk(rhsResult);
        
        boolean lhsDouble = equalsOp.getLhsOperand().getType() == DoubleType.make();
        boolean rhsDouble = equalsOp.getRhsOperand().getType() == DoubleType.make();
        
        // pop rhs into either $t1 or $f2
        if (rhsDouble)
            chunk.popToFpReg(FpRegs.f2);
        else {
            chunk.popToReg(Regs.t1);
            // If "double == int", convert int to double.
            if (lhsDouble) {
                // t1 -> f2, then convert f2 word to double.
                chunk.append(mips.mtc1(Regs.t1, FpRegs.f2));
                chunk.append(mips.cvt_d_w(FpRegs.f2, FpRegs.f2));
            }
        }
        // pop lhs into either $t0 or $f0
        if (lhsDouble)
            chunk.popToFpReg(FpRegs.f0);
        else {
            chunk.popToReg(Regs.t0);
            // If "int == double", convert int to double.
            if (rhsDouble) {
                // t0 -> f0, then convert f0 word to double.
                chunk.append(mips.mtc1(Regs.t0, FpRegs.f0));
                chunk.append(mips.cvt_d_w(FpRegs.f0, FpRegs.f0));
            }
        }
        
        // if any doubles, compare $f0 and $f2.
        if (lhsDouble || rhsDouble) {
            chunk.append(mips.c_eq_d(FpRegs.f0, FpRegs.f2));
            // push FPU condition flag to stack
            String lbl = conFlagGen.genLabel();
            chunk.append(mips.li(Regs.t0, 0));
            chunk.append(mips.bc1f(lbl));
            chunk.append(mips.li(Regs.t0, 1));
            chunk.append(mips.label(lbl));
            chunk.pushReg(Regs.t0);
        }
        // otherwise compare $t0 and $t1.
        else {
            chunk.append(mips.seq(Regs.t2, Regs.t0, Regs.t1));
            chunk.pushReg(Regs.t2);
        }
        
        return chunk;
    }
    
    LabelGenerator conFlagGen = new LabelGenerator("ConFlag");
    
    // done. for ints, bools, and doubles.
    // also converts ints to doubles if needed.
    public MipsChunk visitNotEqualsOp(NotEqualsOp notEqualsOp,
                                    MipsChunk lhsResult, MipsChunk rhsResult)
    {
        MipsChunk chunk = new MipsChunk();
        
        chunk.appendChunk(lhsResult);
        chunk.appendChunk(rhsResult);
        
        boolean lhsDouble = notEqualsOp.getLhsOperand().getType() == DoubleType.make();
        boolean rhsDouble = notEqualsOp.getRhsOperand().getType() == DoubleType.make();
        
        // pop rhs into either $t1 or $f2
        if (rhsDouble)
            chunk.popToFpReg(FpRegs.f2);
        else {
            chunk.popToReg(Regs.t1);
            // If "double != int", convert int to double.
            if (lhsDouble) {
                // t1 -> f2, then convert f2 word to double.
                chunk.append(mips.mtc1(Regs.t1, FpRegs.f2));
                chunk.append(mips.cvt_d_w(FpRegs.f2, FpRegs.f2));
            }
        }
        // pop lhs into either $t0 or $f0
        if (lhsDouble)
            chunk.popToFpReg(FpRegs.f0);
        else {
            chunk.popToReg(Regs.t0);
            // If "int != double", convert int to double.
            if (rhsDouble) {
                // t0 -> f0, then convert f0 word to double.
                chunk.append(mips.mtc1(Regs.t0, FpRegs.f0));
                chunk.append(mips.cvt_d_w(FpRegs.f0, FpRegs.f0));
            }
        }
        
        // if any doubles, compare $f0 and $f2.
        if (lhsDouble || rhsDouble) {
            chunk.append(mips.c_eq_d(FpRegs.f0, FpRegs.f2));
            // push NOT of FPU condition flag to stack
            String lbl = conFlagGen.genLabel();
            chunk.append(mips.li(Regs.t0, 1));
            chunk.append(mips.bc1f(lbl));
            chunk.append(mips.li(Regs.t0, 0));
            chunk.append(mips.label(lbl));
            chunk.pushReg(Regs.t0);
        }
        // otherwise compare $t0 and $t1.
        else {
            chunk.append(mips.sne(Regs.t2, Regs.t0, Regs.t1));
            chunk.pushReg(Regs.t2);
        }
        
        return chunk;
    }

    // done, for ints and doubles.
    public MipsChunk visitLessThanOp(LessThanOp lessThanOp, MipsChunk lhsResult, MipsChunk rhsResult)
    {
        MipsChunk chunk = new MipsChunk();
        
        chunk.appendChunk(lhsResult);
        chunk.appendChunk(rhsResult);
        
        boolean lhsDouble = lessThanOp.getLhsOperand().getType() == DoubleType.make();
        boolean rhsDouble = lessThanOp.getRhsOperand().getType() == DoubleType.make();
        
        // pop rhs into either $t1 or $f2
        if (rhsDouble)
            chunk.popToFpReg(FpRegs.f2);
        else {
            chunk.popToReg(Regs.t1);
            // If "double < int", convert int to double.
            if (lhsDouble) {
                // t1 -> f2, then convert f2 word to double.
                chunk.append(mips.mtc1(Regs.t1, FpRegs.f2));
                chunk.append(mips.cvt_d_w(FpRegs.f2, FpRegs.f2));
            }
        }
        // pop lhs into either $t0 or $f0
        if (lhsDouble)
            chunk.popToFpReg(FpRegs.f0);
        else {
            chunk.popToReg(Regs.t0);
            // If "int < double", convert int to double.
            if (rhsDouble) {
                // t0 -> f0, then convert f0 word to double.
                chunk.append(mips.mtc1(Regs.t0, FpRegs.f0));
                chunk.append(mips.cvt_d_w(FpRegs.f0, FpRegs.f0));
            }
        }
        
        // if any doubles, compare $f0 and $f2.
        if (lhsDouble || rhsDouble) {
            chunk.append(mips.c_lt_d(FpRegs.f0, FpRegs.f2));
            // push FPU condition flag to stack
            String lbl = conFlagGen.genLabel();
            chunk.append(mips.li(Regs.t0, 0));
            chunk.append(mips.bc1f(lbl));
            chunk.append(mips.li(Regs.t0, 1));
            chunk.append(mips.label(lbl));
            chunk.pushReg(Regs.t0);
        }
        // otherwise compare $t0 and $t1.
        else {
            chunk.append(mips.slt(Regs.t2, Regs.t0, Regs.t1));
            chunk.pushReg(Regs.t2);
        }
        
        return chunk;
    }

    // done, for ints and doubles. also promotes
    public MipsChunk visitLessOrEqualOp(LessOrEqualOp lessOrEqualOp,
                                      MipsChunk lhsResult, MipsChunk rhsResult)
    {
        MipsChunk chunk = new MipsChunk();
        
        chunk.appendChunk(lhsResult);
        chunk.appendChunk(rhsResult);
        
        boolean lhsDouble = lessOrEqualOp.getLhsOperand().getType() == DoubleType.make();
        boolean rhsDouble = lessOrEqualOp.getRhsOperand().getType() == DoubleType.make();
        
        // pop rhs into either $t1 or $f2
        if (rhsDouble)
            chunk.popToFpReg(FpRegs.f2);
        else {
            chunk.popToReg(Regs.t1);
            // If "double <= int", convert int to double.
            if (lhsDouble) {
                // t1 -> f2, then convert f2 word to double.
                chunk.append(mips.mtc1(Regs.t1, FpRegs.f2));
                chunk.append(mips.cvt_d_w(FpRegs.f2, FpRegs.f2));
            }
        }
        // pop lhs into either $t0 or $f0
        if (lhsDouble)
            chunk.popToFpReg(FpRegs.f0);
        else {
            chunk.popToReg(Regs.t0);
            // If "int <= double", convert int to double.
            if (rhsDouble) {
                // t0 -> f0, then convert f0 word to double.
                chunk.append(mips.mtc1(Regs.t0, FpRegs.f0));
                chunk.append(mips.cvt_d_w(FpRegs.f0, FpRegs.f0));
            }
        }
        
        // if any doubles, compare $f0 and $f2.
        if (lhsDouble || rhsDouble) {
            chunk.append(mips.c_le_d(FpRegs.f0, FpRegs.f2));
            // push FPU condition flag to stack
            String lbl = conFlagGen.genLabel();
            chunk.append(mips.li(Regs.t0, 0));
            chunk.append(mips.bc1f(lbl));
            chunk.append(mips.li(Regs.t0, 1));
            chunk.append(mips.label(lbl));
            chunk.pushReg(Regs.t0);
        }
        // otherwise compare $t0 and $t1.
        else {
            chunk.append(mips.sle(Regs.t2, Regs.t0, Regs.t1));
            chunk.pushReg(Regs.t2);
        }
        
        return chunk;
    }

    // done for ints and doubles. also promotes
    public MipsChunk visitGreaterThanOp(GreaterThanOp greaterThanOp,
                                      MipsChunk lhsResult, MipsChunk rhsResult)
    {
        MipsChunk chunk = new MipsChunk();
        
        chunk.appendChunk(lhsResult);
        chunk.appendChunk(rhsResult);
        
        boolean lhsDouble = greaterThanOp.getLhsOperand().getType() == DoubleType.make();
        boolean rhsDouble = greaterThanOp.getRhsOperand().getType() == DoubleType.make();
        
        // pop rhs into either $t1 or $f2
        if (rhsDouble)
            chunk.popToFpReg(FpRegs.f2);
        else {
            chunk.popToReg(Regs.t1);
            // If "double > int", convert int to double.
            if (lhsDouble) {
                // t1 -> f2, then convert f2 word to double.
                chunk.append(mips.mtc1(Regs.t1, FpRegs.f2));
                chunk.append(mips.cvt_d_w(FpRegs.f2, FpRegs.f2));
            }
        }
        // pop lhs into either $t0 or $f0
        if (lhsDouble)
            chunk.popToFpReg(FpRegs.f0);
        else {
            chunk.popToReg(Regs.t0);
            // If "int > double", convert int to double.
            if (rhsDouble) {
                // t0 -> f0, then convert f0 word to double.
                chunk.append(mips.mtc1(Regs.t0, FpRegs.f0));
                chunk.append(mips.cvt_d_w(FpRegs.f0, FpRegs.f0));
            }
        }
        
        // if any doubles, compare $f0 and $f2.
        if (lhsDouble || rhsDouble) {
            chunk.append(mips.c_le_d(FpRegs.f0, FpRegs.f2));
            // push NOT of FPU condition flag to stack
            String lbl = conFlagGen.genLabel();
            chunk.append(mips.li(Regs.t0, 1));
            chunk.append(mips.bc1f(lbl));
            chunk.append(mips.li(Regs.t0, 0));
            chunk.append(mips.label(lbl));
            chunk.pushReg(Regs.t0);
        }
        // otherwise compare $t0 and $t1.
        else {
            chunk.append(mips.sgt(Regs.t2, Regs.t0, Regs.t1));
            chunk.pushReg(Regs.t2);
        }
        
        return chunk;
    }

    // done for ints. optional: doubles
    public MipsChunk visitGreaterOrEqualOp(GreaterOrEqualOp greaterOrEqualOp,
                                         MipsChunk lhsResult, MipsChunk rhsResult)
    {
        MipsChunk chunk = new MipsChunk();
        
        chunk.appendChunk(lhsResult);
        chunk.appendChunk(rhsResult);
        
        boolean lhsDouble = greaterOrEqualOp.getLhsOperand().getType() == DoubleType.make();
        boolean rhsDouble = greaterOrEqualOp.getRhsOperand().getType() == DoubleType.make();
        
        // pop rhs into either $t1 or $f2
        if (rhsDouble)
            chunk.popToFpReg(FpRegs.f2);
        else {
            chunk.popToReg(Regs.t1);
            // If "double >= int", convert int to double.
            if (lhsDouble) {
                // t1 -> f2, then convert f2 word to double.
                chunk.append(mips.mtc1(Regs.t1, FpRegs.f2));
                chunk.append(mips.cvt_d_w(FpRegs.f2, FpRegs.f2));
            }
        }
        // pop lhs into either $t0 or $f0
        if (lhsDouble)
            chunk.popToFpReg(FpRegs.f0);
        else {
            chunk.popToReg(Regs.t0);
            // If "int >= double", convert int to double.
            if (rhsDouble) {
                // t0 -> f0, then convert f0 word to double.
                chunk.append(mips.mtc1(Regs.t0, FpRegs.f0));
                chunk.append(mips.cvt_d_w(FpRegs.f0, FpRegs.f0));
            }
        }
        
        // if any doubles, compare $f0 and $f2.
        if (lhsDouble || rhsDouble) {
            chunk.append(mips.c_lt_d(FpRegs.f0, FpRegs.f2));
            // push NOT of FPU condition flag to stack
            String lbl = conFlagGen.genLabel();
            chunk.append(mips.li(Regs.t0, 1));
            chunk.append(mips.bc1f(lbl));
            chunk.append(mips.li(Regs.t0, 0));
            chunk.append(mips.label(lbl));
            chunk.pushReg(Regs.t0);
        }
        // otherwise compare $t0 and $t1.
        else {
            chunk.append(mips.slt(Regs.t2, Regs.t0, Regs.t1));
            chunk.pushReg(Regs.t2);
        }
        
        return chunk;
    }

    // Done, for ints and doubles. Also converts ints to doubles.
    public MipsChunk visitAddOp(AddOp addOp, MipsChunk lhsResult, MipsChunk rhsResult)
    {
        MipsChunk chunk = new MipsChunk();
        
        chunk.appendChunk(lhsResult);
        chunk.appendChunk(rhsResult);
        
        boolean lhsDouble = addOp.getLhsOperand().getType() == DoubleType.make();
        boolean rhsDouble = addOp.getRhsOperand().getType() == DoubleType.make();
        
        // pop rhs into either $t1 or $f2
        if (rhsDouble)
            chunk.popToFpReg(FpRegs.f2);
        else {
            chunk.popToReg(Regs.t1);
            // If "double + int", convert int to double.
            if (lhsDouble) {
                // t1 -> f2, then convert f2 word to double.
                chunk.append(mips.mtc1(Regs.t1, FpRegs.f2));
                chunk.append(mips.cvt_d_w(FpRegs.f2, FpRegs.f2));
            }
        }
        // pop lhs into either $t0 or $f0
        if (lhsDouble)
            chunk.popToFpReg(FpRegs.f0);
        else {
            chunk.popToReg(Regs.t0);
            // If "int + double", convert int to double.
            if (rhsDouble) {
                // t0 -> f0, then convert f0 word to double.
                chunk.append(mips.mtc1(Regs.t0, FpRegs.f0));
                chunk.append(mips.cvt_d_w(FpRegs.f0, FpRegs.f0));
            }
        }
        
        // if any doubles, add $f0 and $f2.
        if (lhsDouble || rhsDouble) {
            chunk.append(mips.add_d(FpRegs.f4, FpRegs.f0, FpRegs.f2));
            chunk.pushFpReg(FpRegs.f4);
        }
        // otherwise add $t0 and $t1.
        else {
            chunk.append(mips.add(Regs.t2, Regs.t0, Regs.t1));
            chunk.pushReg(Regs.t2);
        }
        
        return chunk;
    }

    // Done, for ints and doubles. Also converts int -> double.
    public MipsChunk visitSubtractOp(SubtractOp subtractOp, MipsChunk lhsResult, MipsChunk rhsResult)
    {
        MipsChunk chunk = new MipsChunk();
        
        chunk.appendChunk(lhsResult);
        chunk.appendChunk(rhsResult);
        
        boolean lhsDouble = subtractOp.getLhsOperand().getType() == DoubleType.make();
        boolean rhsDouble = subtractOp.getRhsOperand().getType() == DoubleType.make();
        
        // pop rhs into either $t1 or $f2
        if (rhsDouble)
            chunk.popToFpReg(FpRegs.f2);
        else {
            chunk.popToReg(Regs.t1);
            // If "double - int", convert int to double.
            if (lhsDouble) {
                // t1 -> f2, then convert f2 word to double.
                chunk.append(mips.mtc1(Regs.t1, FpRegs.f2));
                chunk.append(mips.cvt_d_w(FpRegs.f2, FpRegs.f2));
            }
        }
        // pop lhs into either $t0 or $f0
        if (lhsDouble)
            chunk.popToFpReg(FpRegs.f0);
        else {
            chunk.popToReg(Regs.t0);
            // If "int - double", convert int to double.
            if (rhsDouble) {
                // t0 -> f0, then convert f0 word to double.
                chunk.append(mips.mtc1(Regs.t0, FpRegs.f0));
                chunk.append(mips.cvt_d_w(FpRegs.f0, FpRegs.f0));
            }
        }
        
        // if any doubles, take $f0 minus $f2.
        if (lhsDouble || rhsDouble) {
            chunk.append(mips.sub_d(FpRegs.f4, FpRegs.f0, FpRegs.f2));
            chunk.pushFpReg(FpRegs.f4);
        }
        // otherwise take $t0 minus $t1.
        else {
            chunk.append(mips.sub(Regs.t2, Regs.t0, Regs.t1));
            chunk.pushReg(Regs.t2);
        }
        
        return chunk;
    }

    // Done, for ints and doubles. Also promotes.
    public MipsChunk visitMultiplyOp(MultiplyOp multiplyOp, MipsChunk lhsResult, MipsChunk rhsResult)
    {
        MipsChunk chunk = new MipsChunk();
        
        chunk.appendChunk(lhsResult);
        chunk.appendChunk(rhsResult);
        
        boolean lhsDouble = multiplyOp.getLhsOperand().getType() == DoubleType.make();
        boolean rhsDouble = multiplyOp.getRhsOperand().getType() == DoubleType.make();
        
        // pop rhs into either $t1 or $f2
        if (rhsDouble)
            chunk.popToFpReg(FpRegs.f2);
        else {
            chunk.popToReg(Regs.t1);
            // If "double * int", convert int to double.
            if (lhsDouble) {
                // t1 -> f2, then convert f2 word to double.
                chunk.append(mips.mtc1(Regs.t1, FpRegs.f2));
                chunk.append(mips.cvt_d_w(FpRegs.f2, FpRegs.f2));
            }
        }
        // pop lhs into either $t0 or $f0
        if (lhsDouble)
            chunk.popToFpReg(FpRegs.f0);
        else {
            chunk.popToReg(Regs.t0);
            // If "int * double", convert int to double.
            if (rhsDouble) {
                // t0 -> f0, then convert f0 word to double.
                chunk.append(mips.mtc1(Regs.t0, FpRegs.f0));
                chunk.append(mips.cvt_d_w(FpRegs.f0, FpRegs.f0));
            }
        }
        
        // if any doubles, $f0 * $f2.
        if (lhsDouble || rhsDouble) {
            chunk.append(mips.mul_d(FpRegs.f4, FpRegs.f0, FpRegs.f2));
            chunk.pushFpReg(FpRegs.f4);
        }
        // otherwise, $t0 * $t1.
        else {
            chunk.append(mips.mulo(Regs.t2, Regs.t0, Regs.t1));
            chunk.pushReg(Regs.t2);
        }
        
        return chunk;
    }

    // Done, for ints and doubles. Also promotes.
    public MipsChunk visitDivideOp(DivideOp divideOp, MipsChunk lhsResult, MipsChunk rhsResult)
    {
        MipsChunk chunk = new MipsChunk();
        
        chunk.appendChunk(lhsResult);
        chunk.appendChunk(rhsResult);
        
        boolean lhsDouble = divideOp.getLhsOperand().getType() == DoubleType.make();
        boolean rhsDouble = divideOp.getRhsOperand().getType() == DoubleType.make();
        
        // pop rhs into either $t1 or $f2
        if (rhsDouble)
            chunk.popToFpReg(FpRegs.f2);
        else {
            chunk.popToReg(Regs.t1);
            // If "double / int", convert int to double.
            if (lhsDouble) {
                // t1 -> f2, then convert f2 word to double.
                chunk.append(mips.mtc1(Regs.t1, FpRegs.f2));
                chunk.append(mips.cvt_d_w(FpRegs.f2, FpRegs.f2));
            }
        }
        // pop lhs into either $t0 or $f0
        if (lhsDouble)
            chunk.popToFpReg(FpRegs.f0);
        else {
            chunk.popToReg(Regs.t0);
            // If "int / double", convert int to double.
            if (rhsDouble) {
                // t0 -> f0, then convert f0 word to double.
                chunk.append(mips.mtc1(Regs.t0, FpRegs.f0));
                chunk.append(mips.cvt_d_w(FpRegs.f0, FpRegs.f0));
            }
        }
        
        // if any doubles, $f0 / $f2.
        if (lhsDouble || rhsDouble) {
            chunk.append(mips.div_d(FpRegs.f4, FpRegs.f0, FpRegs.f2));
            chunk.pushFpReg(FpRegs.f4);
        }
        // otherwise, $t0 / $t1.
        else {
            chunk.append(mips.div(Regs.t2, Regs.t0, Regs.t1));
            chunk.pushReg(Regs.t2);
        }
        
        return chunk;
    }

    // Done. Only deals with ints, of course.
    public MipsChunk visitModOp(ModOp modOp, MipsChunk lhsResult, MipsChunk rhsResult)
    {
        MipsChunk chunk = new MipsChunk();
        
        chunk.appendChunk(lhsResult);
        chunk.appendChunk(rhsResult);
        
        chunk.popToReg(Regs.t1);
        chunk.popToReg(Regs.t0);
        
        // div moves the remainder into the HI register.
        chunk.append(mips.div(Regs.t0, Regs.t1));
        // so we use mfhi to access it
        chunk.append(mips.mfhi(Regs.t2));
        chunk.pushReg(Regs.t2);
        
        return chunk;
    }

    public MipsChunk visitArrayAccessOp(ArrayAccessOp arrayAccess, MipsChunk array, MipsChunk index)
    {
        // not fundamental
        return null;
    }

    public MipsChunk visitNullLit(NullLit nullLit)
    {
        // not fundamental
        return null;
    }

    // Done.
    public MipsChunk visitBooleanLit(BooleanLit booleanLit)
    {
        // Just push the value 1 or 0 onto the stack.
        MipsChunk chunk = new MipsChunk();
        chunk.append(mips.li(Regs.t0, booleanLit.getValue() ? 1 : 0));
        chunk.pushReg(Regs.t0);
        return chunk;
    }

    public MipsChunk visitStringLit(StringLit stringLit)
    {
        // not fundamental
        return null;
    }

    // I think this is done, but what about a negated INT_MIN?
    public MipsChunk visitIntLit(IntLit intLit)
    {
        MipsChunk chunk = new MipsChunk();
        chunk.append(mips.li(Regs.t0, (int) intLit.getValue()));
        chunk.pushReg(Regs.t0);
        return chunk;
    }

    // Done. Uses "li.d" instruction, not supported in MARS.
    public MipsChunk visitDoubleLit(DoubleLit doubleLit)
    {
        MipsChunk chunk = new MipsChunk();
        chunk.append(mips.li_d(FpRegs.f0, (double) doubleLit.getValue()));
        chunk.pushFpReg(FpRegs.f0);
        return chunk;
    }

    public MipsChunk visitVarRefExp(VarRefExp varRefExp)
    {
        // By default, returns a var-read chunk.
        return readVariable(varRefExp);
    }
    
    // Done. Supports globals + locals, and all types.
    public MipsChunk readVariable(VarRefExp varRefExp)
    {
        // This will return code for a variable read.
        MipsChunk chunk = new MipsChunk();
        VariableSymbol sym = varRefExp.getVariableSymbol();
        if (sym.getType() == DoubleType.make()) {
            if (sym instanceof GlobalSymbol) {
                chunk.append(mips.ldc1(FpRegs.f0, "_" + sym.getName()));
            }
            else if (sym instanceof LocalSymbol) {
                chunk.append(mips.ldc1(FpRegs.f0, ((LocalSymbol) sym).getOffset(), Regs.fp));
            }
            chunk.pushFpReg(FpRegs.f0);
        }
        else {
            if (sym instanceof GlobalSymbol) {
                chunk.append(mips.lw(Regs.t0, "_" + sym.getName()));
            }
            else if (sym instanceof LocalSymbol) {
                chunk.append(mips.lw(Regs.t0, ((LocalSymbol) sym).getOffset(), Regs.fp));
            }
            chunk.pushReg(Regs.t0);
        }
        
        return chunk;
    }

    public MipsChunk visitBooleanTypeNode(BooleanTypeNode boolType)
    {
        // Nothing
        return null;
    }

    public MipsChunk visitIntTypeNode(IntTypeNode intType)
    {
        // Nothing
        return null;
    }

    public MipsChunk visitDoubleTypeNode(DoubleTypeNode doubleType)
    {
        // Nothing
        return null;
    }

    public MipsChunk visitStringTypeNode(StringTypeNode stringType)
    {
        // Nothing
        return null;
    }

    public MipsChunk visitPointerTypeNode(PointerTypeNode pointerType, MipsChunk innerResult)
    {
        // Nothing
        return null;
    }

    public MipsChunk visitArrayTypeNode(ArrayTypeNode pointerType, MipsChunk innerResult)
    {
        // Nothing
        return null;
    }
}
