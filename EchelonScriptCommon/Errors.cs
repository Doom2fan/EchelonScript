/*
 * EchelonScript
 * Copyright (C) 2020- Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Diagnostics;
using System.Reflection;
using System.Text;
using ChronosLib.Pooled;

namespace EchelonScriptCommon;

[AttributeUsage (AttributeTargets.Method, AllowMultiple = false)]
public class ES_ExcludeFromStackTraceAttribute : Attribute {
    public ES_ExcludeFromStackTraceAttribute () { }
}

[AttributeUsage (AttributeTargets.Method, AllowMultiple = false)]
public class ES_MethodTraceDataAttribute : Attribute {
    public string Namespace { get; private init; }
    public string Name { get; private init; }
    public string? ParentType { get; init; }

    public string? FileName { get; init; }

    public ES_MethodTraceDataAttribute (string namespaceName, string name) {
        Namespace = namespaceName;
        Name = name;

        ParentType = null;
    }
}

public abstract class EchelonScriptException : Exception {
    public override string? StackTrace {
        get {
            GetESStackTrace (true);
            return string.Join ("\n", stackTrace_IncludeNative!);
        }
    }

    private string []? stackTrace_NoNative;
    private string []? stackTrace_IncludeNative;

    public EchelonScriptException (string message)
        : base (message) {
    }

    public EchelonScriptException (string message, Exception innerException)
        : base (message, innerException) {
    }

    public ReadOnlySpan<string> GetESStackTrace (bool includeNative) {
        ref var stackTrace = ref stackTrace_NoNative;

        if (includeNative)
            stackTrace = ref stackTrace_IncludeNative;

        if (stackTrace is not null)
            return stackTrace;

        var trace = new StackTrace (this, includeNative);
        var frames = trace.GetFrames ();

        using var lines = new StructPooledList<string> (CL_ClearMode.Auto);
        var sb = new StringBuilder ();
        foreach (var frame in frames) {
            if (!frame.HasMethod ())
                continue;

            var frameMethod = frame.GetMethod ()!;

            if (frameMethod.GetCustomAttribute<ES_ExcludeFromStackTraceAttribute> () != null)
                continue;

            var traceData = frameMethod.GetCustomAttribute<ES_MethodTraceDataAttribute> ();

            sb.Clear ();

            if (traceData is not null) {
                sb.Append ("At ");
                sb.Append (traceData.Namespace);
                sb.Append ("::");

                if (traceData.ParentType is not null) {
                    sb.Append (traceData.ParentType);
                    sb.Append ('.');
                }

                sb.Append (traceData.Name);

                if (traceData.FileName is not null) {
                    sb.Append (", in file ");
                    sb.Append (traceData.FileName);
                }

                sb.Append ('.');
            } else if (includeNative) {
                sb.Append ("At ");

                if (frameMethod.DeclaringType is not null) {
                    sb.Append (frameMethod.DeclaringType.Name);
                    sb.Append ('.');
                }

                sb.Append (frameMethod.Name);

                var fileName = frame.GetFileName ();
                if (fileName is not null) {
                    sb.Append (", in file ");
                    sb.Append (fileName);
                }

                var lineNum = frame.GetFileLineNumber ();
                if (lineNum > 0) {
                    sb.Append (", line ");
                    sb.Append (lineNum);
                }
            }

            if (sb.Length > 0)
                lines.Add (sb.ToString ());
        }

        stackTrace = lines.ToArray ();

        return stackTrace;
    }
}

public class EchelonScriptNullAccessException : EchelonScriptException {
    public EchelonScriptNullAccessException ()
        : base ("Tried to access a null reference.") { }
}

public class EchelonScriptIntegerDivisionByZeroException : EchelonScriptException {
    public EchelonScriptIntegerDivisionByZeroException ()
        : base ("Integer division by zero.") { }
}

public class EchelonScriptOutOfBoundsException : EchelonScriptException {
    public EchelonScriptOutOfBoundsException (string message)
        : base (message) { }
}
