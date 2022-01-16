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
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Media;
using System.Windows.Threading;
using EchelonScriptCommon.GarbageCollection;
using EchelonScriptCommon.GarbageCollection.Immix;

namespace TestSuiteWPF.Tests {
    /// <summary>
    /// Interaction logic for GCInfo.xaml
    /// </summary>
    public partial class GCInfo : UserControl {
        #region ================== Instance fields

        DispatcherTimer updateTimer;

        #endregion

        #region ================== Constructors

        public GCInfo () {
            InitializeComponent ();

            updateTimer = new DispatcherTimer (DispatcherPriority.ContextIdle, Dispatcher);
            updateTimer.Tick += UpdateTimer_Tick;
            updateTimer.Interval = TimeSpan.FromMilliseconds (1d / 60);

            updateTimer.Start ();
        }

        #endregion

        #region ================== Event handlers

        private void Update () {
            ES_GarbageCollector.GetImmixInfo (out var allocInfo, out var overflowInfo);

            PrintInfo (gcInfoTextBlock, ref allocInfo);
            PrintInfo (gcOverflowInfoTextBlock, ref overflowInfo);
        }

        private void PrintInfo (TextBlock block, ref ImmixDebugInfo info) {
            using var blocks = info.Blocks;

            block.Inlines.Clear ();

            block.Inlines.Add ($"Line size / block size: {ImmixConstants.LineSize} / {ImmixConstants.TrueBlockSize}\nLines count: {ImmixConstants.LinesCount}\n\n");

            block.Inlines.Add ($"Total blocks: {info.BlockCount}\nEmpty blocks: {info.EmptyBlocks}\nFull blocks: {info.FullBlocks}\nRecyclable blocks: {info.RecyclableBlocks}\n\n");

            block.Inlines.Add ($"Current block id: {info.CurrentBlockIndex + 1}\nBlocks list:\n");

            var blocksSpan = blocks.Span;
            for (int blockIdx = 0; blockIdx < info.BlockCount; blockIdx++) {
                var blockLines = blocksSpan.Slice (blockIdx * info.BlockStride, info.BlockStride);

                var markedLinesCount = 0;
                var prevLineMarked = false;
                var linesCount = 0;
                for (int chunkIdx = 0; chunkIdx < info.BlockStride; chunkIdx++) {
                    var chunk = blockLines [chunkIdx];

                    var chunkLen = Math.Min (8, ImmixConstants.LinesCount - linesCount);
                    for (int lineIdx = 0; lineIdx < chunkLen; lineIdx++) {
                        var curLineMarked = (chunk & (1 << lineIdx)) != 0;

                        if (curLineMarked || prevLineMarked)
                            markedLinesCount++;

                        prevLineMarked = curLineMarked;
                        linesCount++;
                    }
                }

                var run = new Run ($"  Block {blockIdx + 1}:\n    Lines marked: {markedLinesCount} / {ImmixConstants.LinesCount}\n");

                if (blockIdx == info.CurrentBlockIndex)
                    run.Foreground = Brushes.Red;

                block.Inlines.Add (run);
            }
        }

        private void UpdateTimer_Tick (object sender, EventArgs e) => Update ();

        private void CollectButton_Click (object sender, RoutedEventArgs e) {
            static void ShowError (string message) {
                MessageBox.Show (message, "Internal error", MessageBoxButton.OK, MessageBoxImage.Error);
            }

            var genTag = (string) ((ComboBoxItem) gcCollectGenDropDown.SelectedItem)?.Tag;
            if (genTag is null) {
                ShowError ($"Generation tag is null.");
                return;
            }
            if (!int.TryParse (genTag, out var collectionGen)) {
                ShowError ($"Could not parse generation tag. (\"{genTag}\")");
                return;
            }
            if (collectionGen < -1) {
                ShowError ($"Generation tag < -1. ({collectionGen})");
                return;
            }
            if (collectionGen > 2) {
                ShowError ($"Generation tag > 2. ({collectionGen})");
                return;
            }

            var modeTag = (string) ((ComboBoxItem) gcCollectModeDropDown.SelectedItem)?.Tag;
            if (modeTag is null) {
                ShowError ($"Mode tag is null.");
                return;
            }
            if (!int.TryParse (modeTag, out var collectionMode)) {
                ShowError ($"Could not parse generation tag. (\"{modeTag}\")");
                return;
            }
            if (collectionMode < 0) {
                ShowError ($"Mode tag < 0. ({collectionMode})");
                return;
            }
            if (collectionMode >= (int) ES_GarbageCollector.CollectionMode.__Count) {
                ShowError ($"Mode tag > max mode number. ({collectionMode})");
                return;
            }

            ES_GarbageCollector.PerformCollection (collectionGen, (ES_GarbageCollector.CollectionMode) collectionMode);

            Update ();
        }

        #endregion
    }
}
