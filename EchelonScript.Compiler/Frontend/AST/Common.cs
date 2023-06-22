/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

namespace EchelonScript.Compiler.Frontend.AST;

#if false
public struct ES_AstNodeBounds {
    public int StartPos;
    public int EndPos;

    public ES_AstNodeBounds (int pos) => StartPos = EndPos = pos;

    public ES_AstNodeBounds (int start, int end) {
        StartPos = start;
        EndPos = end;
    }
}

public abstract class ES_AstNode {
    public abstract ES_AstNodeBounds NodeBounds { get; }

    public ES_AstNode (int nothing) { }
}

//public struct ES_AstNodeId<T> where T : unmanaged, ES_AstNode { }
#endif
