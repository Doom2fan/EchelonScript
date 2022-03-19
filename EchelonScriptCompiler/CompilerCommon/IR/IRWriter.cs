/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using ChronosLib.Pooled;
using EchelonScriptCommon.Data;
using EchelonScriptCommon.Utilities;
using static EchelonScriptCompiler.CompilerCommon.IR.ESIR_Factory;

namespace EchelonScriptCompiler.CompilerCommon.IR;

public unsafe sealed class ESIR_Writer : IDisposable {
    private struct LocalValueRegister {
        public ESIR_TypeNode Type;
        public bool Used;
    }

    private struct RegisterAllocator : IDisposable {
        private const int minHeadroom = 5;

        private PooledArray<LocalValueRegister> registers;
        private int regCount;

        public static RegisterAllocator Create () {
            return new RegisterAllocator {
                registers = PooledArray<LocalValueRegister>.Empty (),
                regCount = 0
            };
        }

        public int RentRegister (ESIR_TypeNode type) {
            var regType = type;

            var regsSpan = registers.Span;
            for (var i = 0; i < regCount; i++) {
                ref var reg = ref regsSpan [i];
                if (reg.Used || reg.Type.Pointer != regType.Pointer)
                    continue;

                reg.Used = true;
                return i;
            }

            if (regCount == registers.RealLength) {
                using var oldRegs = registers;
                registers = PooledArray<LocalValueRegister>.GetArray (regCount + minHeadroom);
                oldRegs.Span.CopyTo (registers);
            }

            var idx = regCount++;

            registers.Span [idx] = new LocalValueRegister {
                Type = regType,
                Used = true,
            };

            return idx;
        }

        public void ReturnRegister (int regIdx) {
            Debug.Assert (regIdx >= 0);
            Debug.Assert (regIdx < regCount);

            ref var reg = ref registers.Span [regIdx];

            Debug.Assert (reg.Used);
            reg.Used = false;
        }

        public void ReturnRegisters (ReadOnlySpan<int> regIndices) {
            var regsSpan = registers.Span [..regCount];

            foreach (var regIdx in regIndices) {
                Debug.Assert (regIdx >= 0);
                Debug.Assert (regIdx < regCount);

                ref var reg = ref regsSpan [regIdx];

                Debug.Assert (reg.Used);
                reg.Used = false;
            }
        }

        public ESIR_List<ESIR_TypeNode> GetLocalValues () {
            using var arr = PooledArray<ESIR_TypeNode>.GetArray (regCount);
            var arrSpan = arr.Span;

            var regsSpan = registers.Span [..regCount];
            for (var i = 0; i < regCount; i++)
                arrSpan [i] = regsSpan [i].Type;

            return List (arrSpan);
        }

        public void Clear () => regCount = 0;

        public void Dispose () {
            registers.Dispose ();
            registers = PooledArray<LocalValueRegister>.Empty ();
        }
    }

    private struct StoredScope {
        public PooledArray<ESIR_Statement> Statements { get; init; }
        public PooledArray<int> Registers { get; init; }

        public void Dispose () {
            Statements.Dispose ();
            Registers.Dispose ();
        }
    }

    private struct StoredPartialFunction {
        public ES_Identifier Name { get; init; }

        public ESIR_TypeNode ReturnType { get; init; }
        public PooledArray<ESIR_ArgumentDefinition> Arguments { get; init; }
        public RegisterAllocator RegisterAllocator { get; init; }
        public PooledArray<StoredScope> Scopes { get; init; }

        public PooledArray<ESIR_Statement> Statements { get; init; }
        public PooledArray<int> ScopeRegisters { get; init; }

        public void Dispose () {
            Arguments.Dispose ();
            RegisterAllocator.Dispose ();
            Scopes.Dispose ();

            Statements.Dispose ();
            ScopeRegisters.Dispose ();
        }
    }

    #region ================== Instance fields

    private Stack<StoredPartialFunction> funcStack;

    private StructPooledList<ESIR_StaticVariable> staticVars;
    private StructPooledList<ESIR_Function> functions;
    private StructPooledList<ESIR_Struct> structs;

    private bool funcStarted;
    private ES_Identifier funcName;

    private ESIR_TypeNode? funcRetType;
    private StructPooledList<ESIR_ArgumentDefinition> funcArgs;
    private RegisterAllocator funcRegs;
    private StructPooledList<StoredScope> funcScopes;

    private StructPooledList<ESIR_Statement> funcStmts;
    private StructPooledList<int> funcScopeRegs;

    #endregion

    public ESIR_Writer () {
        IsDisposed = false;

        funcStack = new (15);

        staticVars = new StructPooledList<ESIR_StaticVariable> (CL_ClearMode.Auto);
        functions = new StructPooledList<ESIR_Function> (CL_ClearMode.Auto);
        structs = new StructPooledList<ESIR_Struct> (CL_ClearMode.Auto);
    }

    #region ================== Instance methods

    #region Checking functions

    private void CheckDisposed () {
        if (IsDisposed)
            throw new ObjectDisposedException (nameof (ESIR_Writer));
    }

    private void CheckFunctionStarted () {
        Debug.Assert (funcStarted);
        if (!funcStarted)
            throw new CompilationException ("IRWriter error: Not in a function.");
    }

    private void CheckFunctionNotStarted (string message) {
        Debug.Assert (!funcStarted);
        if (funcStarted)
            throw new CompilationException (message);
    }

    private void CheckNoScopes (string message) {
        Debug.Assert (funcScopes.Count < 1);
        if (funcScopes.Count > 0)
            throw new CompilationException (message);
    }

    private void CheckNoStoredFunctions (string message) {
        Debug.Assert (funcScopes.Count < 1);
        if (funcScopes.Count > 0)
            throw new CompilationException (message);
    }

    #endregion

    #region Misc management

    public void AddStaticVar (ESIR_StaticVariable staticVar) {
        CheckDisposed ();
        staticVars.Add (staticVar);
    }

    public void AddStruct (ESIR_Struct structDef) {
        CheckDisposed ();
        structs.Add (structDef);
    }

    public ESIR_Tree FinishTree () {
        CheckDisposed ();
        CheckFunctionNotStarted ("IRWriter error: Cannot finish the tree while writing a function.");
        CheckNoStoredFunctions ("IRWriter error: Cannot finish the tree with functions still stored.");

        var ret = new ESIR_Tree (
            List (staticVars.Span),
            List (functions.Span),
            List (structs.Span)
        );

        staticVars.Clear ();
        functions.Clear ();
        structs.Clear ();

        return ret;
    }

    #endregion

    #region Function management

    private void ClearFunction () {
        Debug.Assert (funcStarted);

        funcStarted = false;
        funcName = ES_Identifier.Empty;

        funcRetType = null;
        funcArgs.Dispose ();
        funcRegs.Dispose ();
        funcScopes.Dispose ();

        funcStmts.Dispose ();
        funcScopeRegs.Dispose ();
    }

    private StoredPartialFunction StoreFunction () {
        Debug.Assert (funcStarted);

        var ret = new StoredPartialFunction {
            Name = funcName,

            ReturnType = funcRetType!,
            Arguments = funcArgs.MoveToArray (),
            RegisterAllocator = funcRegs,
            Scopes = funcScopes.MoveToArray (),
            Statements = funcStmts.MoveToArray (),
        };

        ClearFunction ();

        return ret;
    }
    private void RestoreFunction (StoredPartialFunction func) {
        Debug.Assert (!funcStarted);

        funcStarted = true;
        funcName = func.Name;

        funcArgs.Dispose ();
        funcRegs.Dispose ();
        funcScopes.Dispose ();

        funcStmts.Dispose ();
        funcScopeRegs.Dispose ();

        funcRetType = func.ReturnType;
        funcArgs = func.Arguments.MoveToStructPooledList (CL_ClearMode.Auto);
        funcRegs = func.RegisterAllocator;
        funcScopes = func.Scopes.MoveToStructPooledList (CL_ClearMode.Auto);

        funcStmts = func.Statements.MoveToStructPooledList (CL_ClearMode.Auto);
        funcScopeRegs = func.ScopeRegisters.MoveToStructPooledList (CL_ClearMode.Auto);
    }

    public void StartFunction (ES_Identifier name, ESIR_TypeNode retType) {
        CheckDisposed ();
        CheckFunctionNotStarted ("IRWriter error: Cannot start a function; Already in a function.");

        funcStarted = true;
        funcName = name;

        funcRetType = retType;
        funcArgs = new StructPooledList<ESIR_ArgumentDefinition> (CL_ClearMode.Auto);
        funcRegs = RegisterAllocator.Create ();
        funcScopes = new StructPooledList<StoredScope> (CL_ClearMode.Auto);

        funcStmts = new StructPooledList<ESIR_Statement> (CL_ClearMode.Auto);
        funcScopeRegs = new StructPooledList<int> (CL_ClearMode.Auto);
    }
    public void EndFunction (ESIR_List<ESIR_Attribute>? attributes) {
        CheckDisposed ();
        CheckFunctionStarted ();
        CheckNoScopes ("IRWriter error: Cannot end function with scopes still stored.");

        functions.Add (Function (
            funcName,
            attributes ?? List<ESIR_Attribute> (),
            funcRetType!,
            List (funcArgs.Span),
            funcRegs.GetLocalValues (),
            List (funcStmts.Span)
        ));

        ClearFunction ();
    }

    public void PushFunction () {
        CheckDisposed ();
        CheckFunctionStarted ();

        funcStack.Push (StoreFunction ());
    }
    public void PopFunction () {
        CheckDisposed ();
        CheckFunctionNotStarted ("IRWriter error: Cannot pop a function while writing to a function.");

        var storedFunc = funcStack.Pop ();
        RestoreFunction (storedFunc);
    }

    #endregion

    #region Register allocation

    public int RentRegister (ESIR_TypeNode type) {
        CheckDisposed ();
        CheckFunctionStarted ();

        return funcRegs.RentRegister (type);
    }

    public void ReturnRegister (int? idx) {
        CheckDisposed ();
        CheckFunctionStarted ();

        if (idx.HasValue)
            funcRegs.ReturnRegister (idx.Value);
    }
    public void ReturnRegister (int idx) {
        CheckDisposed ();
        CheckFunctionStarted ();

        funcRegs.ReturnRegister (idx);
    }
    public void ReturnRegisters (ReadOnlySpan<int> indices) {
        CheckDisposed ();
        CheckFunctionStarted ();

        funcRegs.ReturnRegisters (indices);
    }

    #endregion

    #region Function writing

    public int AddArgument (ESIR_ArgumentDefinition arg) {
        CheckDisposed ();
        CheckFunctionStarted ();

        funcArgs.Add (arg);
        return funcArgs.Count - 1;
    }

    public void AddStatement (ESIR_Statement stmt) {
        CheckDisposed ();
        CheckFunctionStarted ();

        funcStmts.Add (stmt);
    }
    public void AddStatements (ReadOnlySpan<ESIR_Statement> stmts) {
        CheckDisposed ();
        CheckFunctionStarted ();

        funcStmts.AddRange (stmts);
    }

    public void AddScopeRegister (int? reg) {
        CheckDisposed ();
        CheckFunctionStarted ();

        if (reg is not null)
            funcScopeRegs.Add (reg.Value);
    }
    public void AddScopeRegister (int reg) {
        CheckDisposed ();
        CheckFunctionStarted ();

        funcScopeRegs.Add (reg);
    }
    public void AddScopeRegisters (ReadOnlySpan<int> regs) {
        CheckDisposed ();
        CheckFunctionStarted ();

        funcScopeRegs.AddRange (regs);
    }

    public void PushScope () {
        CheckDisposed ();
        CheckFunctionStarted ();

        var storedScope = new StoredScope {
            Statements = funcStmts.MoveToArray (),
            Registers = funcScopeRegs.MoveToArray (),
        };

        funcScopes.Add (storedScope);

        funcStmts = new StructPooledList<ESIR_Statement> (CL_ClearMode.Auto);
        funcScopeRegs = new StructPooledList<int> (CL_ClearMode.Auto);
    }
    public PooledArray<ESIR_Statement> PopScope () {
        CheckDisposed ();
        CheckFunctionStarted ();

        if (funcScopes.Count < 1)
            throw new CompilationException ("IRWriter error: No scopes to push.");

        var ret = funcStmts.MoveToArray ();

        ReturnRegisters (funcScopeRegs.Span);
        funcScopeRegs.Dispose ();

        var scope = funcScopes [^1];
        funcScopes.RemoveEnd (1);

        funcStmts = scope.Statements.MoveToStructPooledList (CL_ClearMode.Auto);
        funcScopeRegs = scope.Registers.MoveToStructPooledList (CL_ClearMode.Auto);

        return ret;
    }

    #endregion

    #endregion

    #region ================== IDisposable support

    public bool IsDisposed { get; private set; }

    ~ESIR_Writer () {
        if (!IsDisposed)
            DoDispose ();
    }

    private void DoDispose () {
        if (IsDisposed)
            return;

        foreach (var value in funcScopes)
            value.Dispose ();

        foreach (var value in funcStack)
            value.Dispose ();

        if (funcStarted) {
            funcArgs.Dispose ();
            funcRegs.Dispose ();
            funcScopes.Dispose ();

            funcStmts.Dispose ();
            funcScopeRegs.Dispose ();
        }

        staticVars.Dispose ();
        functions.Dispose ();
        structs.Dispose ();

        IsDisposed = true;
    }

    public void Dispose () {
        DoDispose ();
        GC.SuppressFinalize (this);
    }

    #endregion
}
