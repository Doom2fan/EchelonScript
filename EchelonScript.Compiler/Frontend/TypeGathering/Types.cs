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
using CommunityToolkit.HighPerformance;
using EchelonScript.Common.Data;
using EchelonScript.Common.Data.Types;
using EchelonScript.Common.Utilities;

namespace EchelonScript.Compiler.Frontend;

internal static partial class Compiler_TypeGathering {
    public static void GatherTypes_Types (ref CompileData compileData) { }
#if false
    public static void GatherTypes_Types (ref CompileData compileData) {
        Debug.Assert (compileData.Symbols.ScopesCount == 0);
        compileData.Symbols.Push ();

        compileData.GatherGlobalImports ();

        foreach (ref var transUnit in compileData.TranslationUnits) {
            foreach (ref var astUnit in transUnit.AstUnits.Span) {
                var passData = new PassData {
                    TransUnitName = transUnit.Name,
                    Source = astUnit.SourceData,
                };

                GatherAstUnit_Types (ref compileData, ref passData, astUnit.Ast);
            }
        }

        compileData.Symbols.Pop ();
        Debug.Assert (compileData.Symbols.ScopesCount == 0);
    }

    private static void GatherAstUnit_Types (ref CompileData compileData, ref PassData passData, ES_AbstractSyntaxTree ast) {
        GatherASTImports (ref compileData, ref passData, ast);

        foreach (var nsDef in ast.Namespaces) {
            var namespaceName = nsDef.NamespaceName.ToIdentifier (compileData.IdPool);
            var namespaceData = compileData.GetOrCreateNamespace (namespaceName);
            Debug.Assert (namespaceData is not null);

            compileData.Symbols.Push ();
            ImportNamespaceSymbols (ref compileData, namespaceData);

            foreach (var type in nsDef.Contents) {
                switch (type) {
                    case ES_AstClassDefinition classDef: {
                        var typeName = compileData.IdPool.GetIdentifier (classDef.Name.Text.Span);
                        var classBuilder = namespaceData.GetClass (typeName);
                        Debug.Assert (classBuilder is not null);

                        GatherTypes_Class (ref compileData, ref passData, classDef, classBuilder);
                        break;
                    }

                    case ES_AstStructDefinition structDef: {
                        var typeName = compileData.IdPool.GetIdentifier (structDef.Name.Text.Span);
                        var structBuilder = namespaceData.GetStruct (typeName);
                        Debug.Assert (structBuilder is not null);

                        GatherTypes_Struct (ref compileData, ref passData, structDef, structBuilder);
                        break;
                    }

                    case ES_AstEnumDefinition enumDef: {
                        var typeName = compileData.IdPool.GetIdentifier (enumDef.Name.Text.Span);
                        var enumBuilder = namespaceData.GetEnum (typeName);
                        Debug.Assert (enumBuilder is not null);

                        GatherTypes_Enum (ref compileData, ref passData, enumDef, enumBuilder);
                        break;
                    }

                    case ES_AstFunctionDefinition funcDef:
                        GatherTypes_Function (ref compileData, ref passData, funcDef);
                        break;

                    default:
                        throw new NotImplementedException ("Node type not implemented.");
                }
            }

            compileData.Symbols.Pop ();
        }
    }

    private static void GatherTypes_InheritanceList (
        ref CompileData compileData, ref PassData passData,
        ReadOnlySpan<ES_AstTypeDeclaration_TypeName> inheritanceList, bool isClass,
        out ESC_TypeInterface [] interfaces, out ESC_TypeClass? baseClass
    ) {
        baseClass = null;

        var source = passData.Source;
        var errorList = compileData.ErrorList;

        var interfacesList = new StructPooledList<ESC_TypeInterface> (CL_ClearMode.Auto);

        foreach (var inheritId in inheritanceList) {
            var type = ResolveTypeDeclaration (ref compileData, ref passData, inheritId);

            if (type.Type == null)
                continue;

            if (isClass && type.Type is ESC_TypeClass a) {
                if (baseClass is not null) {
                    errorList.Add (new (source, inheritId.NodeBounds, ES_FrontendErrors.MultipleBaseClasses));
                    continue;
                }

                baseClass = (ESC_TypeClass) type.Type;
            } else if (type.Type is ESC_TypeInterface interfaceData) {
                var interfaceInList = false;
                foreach (var other in interfacesList) {
                    if (other == type.Type) {
                        interfaceInList = true;
                        break;
                    }
                }

                if (interfaceInList) {
                    var interfaceFqnStr = compileData.GetNiceNameString (type, true);
                    errorList.Add (ES_FrontendErrors.GenRepeatedInterfaceInList (interfaceFqnStr, source, inheritId.NodeBounds));
                    continue;
                }

                interfacesList.Add (interfaceData);
            } else {
                using var charsPool = PooledArray<char>.GetArray (inheritId.GetStringLength ());
                inheritId.ToString (charsPool.Span);

                var symbolStr = charsPool.Span.GetPooledString ();
                errorList.Add (ES_FrontendErrors.GenInvalidInheritance (symbolStr, source, inheritId.NodeBounds));
                continue;
            }
        }

        interfaces = interfacesList.ToArray ();
    }

    private static unsafe void ResolveAggregate (
        ref CompileData compileData, ref PassData passData,
        ESC_TypeAggregate type, ES_AstAggregateDefinition typeDef
    ) {
        static bool IsNameUsed (ES_Identifier id, List<ESC_TypeMember> members) {
            foreach (var member in members) {
                if (member.Name.Equals (id))
                    return true;
            }

            return false;
        }

        foreach (var content in typeDef.Contents) {
            switch (content) {
                case ES_AstMemberVarDefinition varDef: {
                    Debug.Assert (varDef.ValueType is not null);

                    varDef.ValueType = GenerateASTTypeRef (ref compileData, ref passData, varDef.ValueType);
                    if (varDef.InitializationExpression is not null)
                        GatherTypes_Expression (ref compileData, ref passData, varDef.InitializationExpression);

                    var varId = compileData.IdPool.GetIdentifier (varDef.Name.Text.Span);
                    var varType = GetTypeRef (varDef.ValueType);
                    var flags = (ESC_MemberFlags) 0;

                    if (varDef.Static)
                        flags |= ESC_MemberFlags.Static;

                    if (IsNameUsed (varId, type.Members)) {
                        compileData.ErrorList.Add (ES_FrontendErrors.GenDuplicateSymbolDef (
                            varDef.Name.Text.Span.GetPooledString (), varDef.Name
                        ));
                    }

                    type.Members.Add (new ESC_TypeMember_Field {
                        Name = varId,
                        AccessModifier = varDef.AccessModifier,
                        Flags = flags,
                        FieldType = varType,
                        Offset = -1,
                    });

                    break;
                }

                case ES_AstFunctionDefinition funcDef:
                    throw new NotImplementedException ("[TODO] Member functions not implemented yet.");

                default:
                    throw new NotImplementedException ("Node type not implemented.");
            }
        }
    }

    private static void GatherTypes_Class (
        ref CompileData compileData, ref PassData passData,
        ES_AstClassDefinition classDef, ESC_TypeClass classData
    ) {
        if (classData.RuntimeSize > -1)
            return;

        GatherTypes_InheritanceList (
            ref compileData, ref passData,
            classDef.InheritanceList, true,
            out var interfaces, out var baseClass
        );
        classData.BaseClass = baseClass;
        classData.Interfaces = interfaces;

        throw new NotImplementedException ("[TODO] Classes not implemented yet.");
    }

    private static void GatherTypes_Struct (ref CompileData compileData, ref PassData passData, ES_AstStructDefinition structDef, ESC_TypeStruct structData) {
        GatherTypes_InheritanceList (
            ref compileData, ref passData,
            structDef.InterfacesList, false,
            out var interfaces, out _
        );
        structData.Interfaces = interfaces;

        ResolveAggregate (ref compileData, ref passData, structData, structDef);
    }

    private static void GatherTypes_Enum (ref CompileData compileData, ref PassData passData, ES_AstEnumDefinition enumDef, ESC_TypeEnum enumData) {
        var baseTypeName = ES_PrimitiveTypes.Int32;
        if (enumDef.BaseType.HasValue)
            baseTypeName = enumDef.BaseType.Value.Text.Span.GetPooledString ();

        var baseType = baseTypeName switch {
            ES_PrimitiveTypes.Bool => compileData.GetUnknownType (ESC_Constness.Mutable),
            ES_PrimitiveTypes.Int8 => compileData.GetIntType (ES_IntSize.Int8, false),
            ES_PrimitiveTypes.Int16 => compileData.GetIntType (ES_IntSize.Int16, false),
            ES_PrimitiveTypes.Int32 => compileData.GetIntType (ES_IntSize.Int32, false),
            ES_PrimitiveTypes.Int64 => compileData.GetIntType (ES_IntSize.Int64, false),
            ES_PrimitiveTypes.UInt8 => compileData.GetIntType (ES_IntSize.Int8, true),
            ES_PrimitiveTypes.UInt16 => compileData.GetIntType (ES_IntSize.Int16, true),
            ES_PrimitiveTypes.UInt32 => compileData.GetIntType (ES_IntSize.Int32, true),
            ES_PrimitiveTypes.UInt64 => compileData.GetIntType (ES_IntSize.Int64, true),
            ES_PrimitiveTypes.Float32 => compileData.GetFloat32Type (ESC_Constness.Mutable),
            ES_PrimitiveTypes.Float64 => compileData.GetFloat64Type (ESC_Constness.Mutable),

            ES_PrimitiveTypes.String or
            ES_PrimitiveTypes.Char => throw new NotImplementedException ("[TODO] Types not implemented yet."),

            _ => compileData.GetUnknownType (ESC_Constness.Mutable),
        };
        enumData.BaseType = baseType.Type;

        if (baseType.Type is ESC_TypeUnknown) {
            Debug.Assert (enumDef.BaseType is not null);
            compileData.ErrorList.Add (
                ES_FrontendErrors.GenInvalidEnumBaseType (baseTypeName, enumDef.BaseType.Value)
            );
        }

        foreach (ref var pair in enumDef.MembersList.AsSpan ()) {
            if (pair.Item2 is null)
                continue;

            GatherTypes_Expression (ref compileData, ref passData, pair.Item2);
        }

        throw new NotImplementedException ("[TODO] Enum type gathering not implemented yet.");
    }

    private static void GatherTypes_Function (ref CompileData compileData, ref PassData passData, ES_AstFunctionDefinition funcDef) {
        // Guarantee some conditions.
        // These should have been validated and resolved by the function creation pass.
        Debug.Assert (funcDef.ReturnType is ES_AstTypeDeclaration_TypeReference);
        Debug.Assert (funcDef.ArgumentsList is not null);

        Debug.Assert (funcDef.Statement is not null);
        Debug.Assert (funcDef.Statement.Endpoint is null);
        // Expression-body functions must contain exactly a single expression statement.
        Debug.Assert (
            (funcDef.ExpressionBody && funcDef.Statement is ES_AstExpressionStatement) ||
            !funcDef.ExpressionBody
        );

        compileData.Symbols.Push ();

        foreach (var arg in funcDef.ArgumentsList) {
            if (arg.DefaultExpression is null)
                continue;

            GatherTypes_Expression (ref compileData, ref passData, arg.DefaultExpression);
        }

        GatherTypes_Statement (ref compileData, ref passData, funcDef.Statement);

        compileData.Symbols.Pop ();
    }

    private static void GatherTypes_Statement (ref CompileData compileData, ref PassData passData, ES_AstStatement stmt) {
        Debug.Assert (stmt is not null);

        switch (stmt) {
            case ES_AstEmptyStatement:
            case ES_AstLabeledStatement:
                break;

            case ES_AstBlockStatement blockStmt: {
                compileData.Symbols.Push ();

                var curStatement = blockStmt.Statement;
                while (curStatement is not null) {
                    GatherTypes_Statement (ref compileData, ref passData, curStatement);
                    curStatement = curStatement.Endpoint;
                }

                compileData.Symbols.Pop ();
                break;
            }

    #region Symbol definition

            case ES_AstImportStatement importStmt:
                AST_HandleImport (ref compileData, ref passData, importStmt);
                break;

            case ES_AstTypeAlias aliasStmt:
                AST_HandleAlias (ref compileData, ref passData, aliasStmt);
                break;

            case ES_AstLocalVarDefinition varDef: {
                if (varDef.ValueType is not null)
                    varDef.ValueType = GenerateASTTypeRef (ref compileData, ref passData, varDef.ValueType);

                foreach (var variable in varDef.Variables) {
                    if (variable.InitializationExpression is not null)
                        GatherTypes_Expression (ref compileData, ref passData, variable.InitializationExpression);
                }

                break;
            }

    #endregion

    #region Jumps

            case ES_AstConditionalStatement condStmt:
                GatherTypes_Expression (ref compileData, ref passData, condStmt.ConditionExpression);
                GatherTypes_Statement (ref compileData, ref passData, condStmt.ThenStatement);
                if (condStmt.ElseStatement is not null)
                    GatherTypes_Statement (ref compileData, ref passData, condStmt.ElseStatement);
                break;

            case ES_AstSwitchStatement switchStmt: {
                GatherTypes_Expression (ref compileData, ref passData, switchStmt.ValueExpression);

                foreach (var section in switchStmt.Sections) {
                    foreach (var expr in section.Expressions) {
                        if (expr is not null)
                            GatherTypes_Expression (ref compileData, ref passData, expr);
                    }

                    var curStatement = section.StatementsBlock;
                    while (curStatement is not null) {
                        GatherTypes_Statement (ref compileData, ref passData, curStatement);
                        curStatement = curStatement.Endpoint;
                    }
                }

                break;
            }

            case ES_AstBreakStatement:
                break;

            case ES_AstContinueStatement:
                break;

            case ES_AstGotoLabelStatement:
                break;

            case ES_AstGotoCaseStatement gotoCaseStmt:
                if (gotoCaseStmt.CaseExpression is not null)
                    GatherTypes_Expression (ref compileData, ref passData, gotoCaseStmt.CaseExpression);
                break;

            case ES_AstReturnStatement retStmt:
                if (retStmt.ReturnExpression is not null)
                    GatherTypes_Expression (ref compileData, ref passData, retStmt.ReturnExpression);
                break;

    #endregion

    #region Loops

            case ES_AstLoopStatement loopStmt: {
                compileData.Symbols.Push ();

                if (loopStmt.InitializationStatement is not null)
                    GatherTypes_Statement (ref compileData, ref passData, loopStmt.InitializationStatement);

                if (loopStmt.ConditionExpression is not null)
                    GatherTypes_Expression (ref compileData, ref passData, loopStmt.ConditionExpression);

                if (loopStmt.IterationExpressions is not null) {
                    foreach (var expr in loopStmt.IterationExpressions) {
                        if (expr is not null)
                            GatherTypes_Expression (ref compileData, ref passData, expr);
                    }
                }

                Debug.Assert (loopStmt.LoopBody is not null);
                Debug.Assert (loopStmt.LoopBody.Endpoint is null);

                GatherTypes_Statement (ref compileData, ref passData, loopStmt.LoopBody);

                compileData.Symbols.Pop ();

                break;
            }

    #endregion

            case ES_AstExpressionStatement exprStmt:
                GatherTypes_Expression (ref compileData, ref passData, exprStmt.Expression);
                break;

            case ES_AstExpressionListStatement exprListStmt: {
                foreach (var expr in exprListStmt.Expressions)
                    GatherTypes_Expression (ref compileData, ref passData, expr);
                break;
            }

            default:
                throw new NotImplementedException ("Statement type not implemented.");
        }
    }

    private static void GatherTypes_Expression (ref CompileData compileData, ref PassData passData, ES_AstExpression expr) {
        Debug.Assert (expr is not null);

        switch (expr) {
            case ES_AstParenthesisExpression parenExpr:
                GatherTypes_Expression (ref compileData, ref passData, parenExpr.Inner);
                break;

    #region Primary expressions

            case ES_AstFunctionCallExpression funcCallExpr: {
                GatherTypes_Expression (ref compileData, ref passData, funcCallExpr.FunctionExpression);
                foreach (var args in funcCallExpr.Arguments)
                    GatherTypes_Expression (ref compileData, ref passData, args.ValueExpression);
                break;
            }

            case ES_AstIndexingExpression indexExpr: {
                GatherTypes_Expression (ref compileData, ref passData, indexExpr.IndexedExpression);
                foreach (var dim in indexExpr.DimensionExpressions) {
                    if (dim is not null)
                        GatherTypes_Expression (ref compileData, ref passData, dim);
                }
                break;
            }

            case ES_AstNewObjectExpression newObjExpr: {
                if (newObjExpr.TypeDeclaration is not null)
                    newObjExpr.TypeDeclaration = GenerateASTTypeRef (ref compileData, ref passData, newObjExpr.TypeDeclaration);

                foreach (var args in newObjExpr.Arguments)
                    GatherTypes_Expression (ref compileData, ref passData, args.ValueExpression);

                break;
            }

            case ES_AstNewArrayExpression newArrayExpr: {
                if (newArrayExpr.ElementType is not null)
                    newArrayExpr.ElementType = GenerateASTTypeRef (ref compileData, ref passData, newArrayExpr.ElementType);

                foreach (var dim in newArrayExpr.Dimensions) {
                    Debug.Assert (dim is not null);
                    GatherTypes_Expression (ref compileData, ref passData, dim);
                }

                break;
            }

            // We don't need to do anything for these
            case ES_AstIntegerLiteralExpression:
            case ES_AstBooleanLiteralExpression:
            case ES_AstFloatLiteralExpression:
            case ES_AstStringLiteralExpression:
            case ES_AstCharLiteralExpression:
            case ES_AstNullLiteralExpression:
            case ES_AstNameExpression:
                break;

            case ES_AstMemberAccessExpression memberAccessExpr:
                GatherTypes_Expression (ref compileData, ref passData, memberAccessExpr.Parent);
                break;

    #endregion

            case ES_AstIncDecExpression incDecExpr:
                GatherTypes_Expression (ref compileData, ref passData, incDecExpr.Inner);
                break;

    #region Unary expressions

            case ES_AstSimpleUnaryExpression unaryExpr:
                GatherTypes_Expression (ref compileData, ref passData, unaryExpr.Inner);
                break;

            case ES_AstCastExpression castExpr:
                if (castExpr.DestinationType is not null)
                    castExpr.DestinationType = GenerateASTTypeRef (ref compileData, ref passData, castExpr.DestinationType);
                GatherTypes_Expression (ref compileData, ref passData, castExpr.InnerExpression);
                break;

    #endregion

            case ES_AstSimpleBinaryExpression simpleBinaryExpr:
                GatherTypes_Expression (ref compileData, ref passData, simpleBinaryExpr.Left);
                GatherTypes_Expression (ref compileData, ref passData, simpleBinaryExpr.Right);
                break;

            case ES_AstConditionalExpression condExpr:
                GatherTypes_Expression (ref compileData, ref passData, condExpr.Condition);
                GatherTypes_Expression (ref compileData, ref passData, condExpr.Then);
                GatherTypes_Expression (ref compileData, ref passData, condExpr.Else);
                break;

            default:
                throw new NotImplementedException ("Expression type not implemented.");
        }
    }
#endif
}
