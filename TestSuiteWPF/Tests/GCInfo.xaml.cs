/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Text;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Media;
using System.Windows.Threading;
using EchelonScriptCommon.Immix_GC;

namespace TestSuiteWPF.Tests {
    /// <summary>
    /// Interaction logic for GCInfo.xaml
    /// </summary>
    public partial class GCInfo : UserControl {
        #region ================== Instance fields

        DispatcherTimer updateTimer;
        StringBuilder stringBuilder;

        #endregion

        #region ================== Constructors

        public GCInfo () {
            InitializeComponent ();

            updateTimer = new DispatcherTimer (DispatcherPriority.ContextIdle, Dispatcher);
            updateTimer.Tick += UpdateTimer_Tick;
            updateTimer.Interval = TimeSpan.FromMilliseconds (1d / 60);

            updateTimer.Start ();
            stringBuilder = new StringBuilder ();
        }

        #endregion

        #region ================== Event handlers

        private void Update () {
            ImmixGC.GetInfo (out var allocInfo, out var overflowInfo);

            PrintInfo (gcInfoTextBlock, ref allocInfo);
            PrintInfo (gcOverflowInfoTextBlock, ref overflowInfo);
        }

        private void PrintInfo (TextBlock block, ref ImmixDebugInfo info) {
            using var blocks = info.Blocks;

            block.Inlines.Clear ();

            block.Inlines.Add ($"Line size / block size: {ImmixGC.LineSize} / {ImmixGC.BlockSize}\nLines count: {ImmixGC.LinesCount}\n\n");

            block.Inlines.Add ($"Total blocks: {info.BlockCount}\nEmpty blocks: {info.EmptyBlocks}\nFull blocks: {info.FullBlocks}\nRecyclable blocks: {info.RecyclableBlocks}\n\n");

            block.Inlines.Add ($"Current block id: {info.CurrentBlockIndex + 1}\nBlocks list:\n");

            var blocksSpan = blocks.Span;
            for (int blockIdx = 0; blockIdx < info.BlockCount; blockIdx++) {
                var blockLines = blocksSpan.Slice (blockIdx * info.BlockStride, info.BlockStride);

                var markedLinesCount = 0;
                var prevLineMarked = false;
                for (int chunkIdx = 0; chunkIdx < info.BlockStride; chunkIdx++) {
                    var chunk = blockLines [chunkIdx];

                    for (int lineIdx = 0; lineIdx < 8; lineIdx++) {
                        var curLineMarked = (chunk & (1 << lineIdx)) != 0;

                        if (curLineMarked || prevLineMarked)
                            markedLinesCount++;

                        prevLineMarked = curLineMarked;
                    }
                }

                var run = new Run ($"  Block {blockIdx + 1}:\n    Lines marked: {markedLinesCount} / {ImmixGC.LinesCount}\n");

                if (blockIdx == info.CurrentBlockIndex)
                    run.Foreground = Brushes.Red;

                block.Inlines.Add (run);
            }
        }

        private void UpdateTimer_Tick (object sender, EventArgs e) => Update ();

        #endregion
    }
}
