/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;
using Collections.Pooled;
using EchelonScriptCompiler.Utilities;
using Microsoft.Toolkit.HighPerformance.Buffers;

namespace EchelonScriptCompiler.Data {
    public unsafe class ES_NamespaceData : IDisposable {
        public class Builder : IDisposable {
            #region ================== Instance fields

            private ES_NamespaceData namespaceData;

            #endregion

            #region ================== Instance properties

            public PooledDictionary<ArrayPointer<byte>, ES_ClassData.Builder> ClassBuilders { get; protected set; }

            #endregion

            #region ================== Constructors

            public Builder ([DisallowNull] ES_NamespaceData nm) {
                namespaceData = nm;

                ClassBuilders = new PooledDictionary<ArrayPointer<byte>, ES_ClassData.Builder> ();
            }

            #endregion

            #region ================== Instance methods

            public ES_ClassData.Builder CreateClass (string name) {
                CheckDisposed ();

                var namePtr = namespaceData.environment.IdPool.GetIdentifier (name);

                if (ClassBuilders.ContainsKey (namePtr))
                    throw new ArgumentNullException (nameof (name), "A class with the specified name already exists.");

                var classDataPtr = (ES_ClassData*) Marshal.AllocHGlobal (sizeof (ES_ClassData));
                namespaceData.classesDict.Add (namePtr, classDataPtr);

                var builder = new ES_ClassData.Builder (classDataPtr, namePtr);
                ClassBuilders.Add (namePtr, builder);

                return builder;
            }

            protected void CheckDisposed () {
                if (disposedValue)
                    throw new ObjectDisposedException (nameof (ES_NamespaceData.Builder));
            }

            #endregion

            #region ================== IDisposable support

            private bool disposedValue = false;

            ~Builder () {
                if (!disposedValue)
                    DoDispose ();
            }

            protected virtual void DoDispose () {
                if (!disposedValue) {
                    ClassBuilders?.Dispose ();

                    disposedValue = true;
                }
            }

            public void Dispose () {
                DoDispose ();
                GC.SuppressFinalize (this);
            }

            #endregion
        }

        #region ================== Instance fields

        protected EchelonScriptEnvironment environment;
        protected ArrayPointer<byte> namespaceName;

        protected Dictionary<ArrayPointer<byte>, Pointer<ES_ClassData>> classesDict;

        #endregion

        #region ================== Instance properties

        public string NamespaceName {
            get {
                return StringPool.Shared.GetOrAdd (namespaceName.Span, Encoding.ASCII);
            }
        }

        public IReadOnlyDictionary<ArrayPointer<byte>, Pointer<ES_ClassData>> Classes => classesDict;

        #endregion

        #region ================== Constructors

        public ES_NamespaceData (EchelonScriptEnvironment env, ArrayPointer<byte> name) {
            environment = env;
            namespaceName = name;

            classesDict = new Dictionary<ArrayPointer<byte>, Pointer<ES_ClassData>> ();
        }

        #endregion

        #region ================== IDisposable support

        private bool disposedValue = false;

        ~ES_NamespaceData () {
            if (!disposedValue)
                DoDispose ();
        }

        protected virtual void DoDispose () {
            if (!disposedValue) {
                foreach (var kvp in classesDict) {
                    Marshal.FreeHGlobal (new IntPtr (kvp.Value));
                }
                classesDict.Clear ();

                disposedValue = true;
            }
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }

    public unsafe class EchelonScriptEnvironment : IDisposable {
        public class Builder : IDisposable {
            #region ================== Instance fields

            private EchelonScriptEnvironment environment;

            #endregion

            #region ================== Instance properties

            public PooledDictionary<ArrayPointer<byte>, ES_NamespaceData.Builder> NamespaceBuilders { get; protected set; }

            #endregion

            #region ================== Constructors

            internal Builder (EchelonScriptEnvironment env) {
                environment = env;

                NamespaceBuilders = new PooledDictionary<ArrayPointer<byte>, ES_NamespaceData.Builder> ();
            }

            #endregion

            #region ================== Instance methods

            public ES_NamespaceData.Builder CreateNamespace (ReadOnlySpan<char> name) {
                CheckDisposed ();

                var namePtr = environment.IdPool.GetIdentifier (name);
                if (NamespaceBuilders.ContainsKey (namePtr))
                    throw new ArgumentNullException (nameof (name), "A class with the specified name already exists.");

                var namespaceData = new ES_NamespaceData (environment, namePtr);
                environment.namespacesDict.Add (namePtr, namespaceData);

                var builder = new ES_NamespaceData.Builder (namespaceData);
                NamespaceBuilders.Add (namePtr, builder);

                return builder;
            }

            protected void CheckDisposed () {
                if (disposedValue)
                    throw new ObjectDisposedException (nameof (EchelonScriptEnvironment));
            }

            #endregion

            #region ================== IDisposable support

            private bool disposedValue = false;

            ~Builder () {
                if (!disposedValue)
                    DoDispose ();
            }

            protected virtual void DoDispose () {
                if (!disposedValue) {
                    NamespaceBuilders?.Dispose ();

                    disposedValue = true;
                }
            }

            public void Dispose () {
                DoDispose ();
                GC.SuppressFinalize (this);
            }

            #endregion
        }

        #region ================== Instance fields

        protected Dictionary<ArrayPointer<byte>, ES_NamespaceData> namespacesDict;

        #endregion

        #region ================== Instance properties

        public UnmanagedIdentifierPool IdPool { get; protected set; }
        public IReadOnlyDictionary<ArrayPointer<byte>, ES_NamespaceData> Namespaces => namespacesDict;

        #endregion

        #region ================== Constructors

        protected EchelonScriptEnvironment () {
            namespacesDict = new Dictionary<ArrayPointer<byte>, ES_NamespaceData> ();
            IdPool = new UnmanagedIdentifierPool ();
        }

        #endregion

        #region ================== Static methods

        public static EchelonScriptEnvironment CreateEnvironment (out Builder builder) {
            var ret = new EchelonScriptEnvironment ();

            builder = new Builder (ret);

            return ret;
        }

        #endregion

        #region ================== IDisposable support

        private bool disposedValue = false;

        ~EchelonScriptEnvironment () {
            if (!disposedValue)
                DoDispose ();
        }

        protected virtual void DoDispose () {
            if (!disposedValue) {
                foreach (var kvp in namespacesDict) {
                    kvp.Value.Dispose ();
                }
                namespacesDict.Clear ();
                IdPool?.Dispose ();

                disposedValue = true;
            }
        }

        public void Dispose () {
            DoDispose ();
            GC.SuppressFinalize (this);
        }

        #endregion
    }
}
