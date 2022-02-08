/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

namespace EchelonScriptCompiler.CompilerCommon.IR {
    public class ESIR_Tree {
        private readonly ESIR_List<ESIR_StaticVariable> staticVarsNode;
        private readonly ESIR_List<ESIR_Function> functionsNode;
        private readonly ESIR_List<ESIR_Struct> structsNode;

        public ESIR_List<ESIR_StaticVariable> StaticVariables => staticVarsNode;
        public ESIR_List<ESIR_Function> Functions => functionsNode;
        public ESIR_List<ESIR_Struct> Structs => structsNode;

        public ESIR_Tree (
            ESIR_List<ESIR_StaticVariable> staticVars,
            ESIR_List<ESIR_Function> functions,
            ESIR_List<ESIR_Struct> structs
        ) {
            staticVarsNode = staticVars;
            functionsNode = functions;
            structsNode = structs;
        }
    }
}
