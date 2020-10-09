/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

using System;
using System.Collections.Generic;
using System.Text;

namespace EchelonScriptCompiler.Parser {
    public class ES_AstTreeNode {

    }

    public class ES_AstTree : ES_AstTreeNode {
        public ES_AstImportStatement [] ImportStatements;
        public ES_AstTypeAlias [] TypeAliases;
        public ES_AstNamespace [] Namespaces;
    }

    public class ES_AstNamespace {
        public ES_AstDottableIdentifier NamespaceName;

        public ES_AstTreeNode [] Contents;
    }

    public class ES_AstDottableIdentifier : ES_AstTreeNode {
        public ReadOnlyMemory<char> [] Parts;
    }

    public class ES_AstImportStatement : ES_AstTreeNode {
        public ES_AstDottableIdentifier NamespaceName;
        public ReadOnlyMemory<char> [] ImportedNames;
    }

    public class ES_AstTypeAlias : ES_AstTreeNode {
        public ReadOnlyMemory<char> AliasName;
        public ES_AstDottableIdentifier OriginalName;
    }

}
