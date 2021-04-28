/*
 * EchelonScript
 * Copyright (C) 2020-2021 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Linq;
using EchelonScriptCompiler.Frontend.Parser;

namespace EchelonScriptCompiler.Data.Types {
    public abstract class ES_ExportAttributeBase : Attribute {
        public enum AggregateType {
            Struct,
            Class,
        }

        public bool IsValidIdentifier (string id) {
            if (id is null || id.Length < 1)
                return false;

            if (!EchelonScriptTokenizer.IsLatinLetter (id [0]) && id [0] != '_')
                return false;

            return id.All (c => (
                EchelonScriptTokenizer.IsLatinLetter (c) ||
                EchelonScriptTokenizer.IsIntegerDigit (c) ||
                c == '_'
            ));
        }
    }

    [AttributeUsage (AttributeTargets.Struct)]
    public class ES_ExportAggregateAttribute : ES_ExportAttributeBase {
        protected string []? exportNamespace;
        protected string exportName;
        protected AggregateType aggregateType;

        public ES_ExportAggregateAttribute (string []? nm, string name, AggregateType type) {
            if (nm != null && !nm.All (str => IsValidIdentifier (str)))
                throw new ArgumentException ("Invalid namespace.", nameof (nm));
            else if (!IsValidIdentifier (name))
                throw new ArgumentException ("Invalid name.", nameof (name));

            exportNamespace = nm;
            exportName = name;
            aggregateType = type;
        }
    }

    [AttributeUsage (AttributeTargets.Field)]
    public class ES_ExportFieldAttribute : ES_ExportAttributeBase {
        protected string exportName;

        public ES_ExportFieldAttribute (string type, string name) {
            if (!IsValidIdentifier (name))
                throw new ArgumentException ("Invalid name.", nameof (name));

            exportName = name;
            throw new NotImplementedException ();
        }
    }
}
