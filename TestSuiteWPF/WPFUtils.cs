﻿/*
 * EchelonScript
 * Copyright (C) 2020 Chronos "phantombeta" Ouroboros
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Media;
using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.Document;
using ICSharpCode.AvalonEdit.Rendering;

namespace TestSuiteWPF {
    public class TextMarkerService : IBackgroundRenderer, IVisualLineTransformer {
        public sealed class TextMarker : TextSegment {
            public Color? BackgroundColor { get; set; }
            public Color MarkerColor { get; set; }
            public string ToolTip { get; set; }

            public TextMarker (int startOffset, int length) {
                StartOffset = startOffset;
                Length = length;
            }
        }

        #region ================== Instance fields

        protected readonly TextEditor textEditor;
        protected readonly TextSegmentCollection<TextMarker> markers;

        #endregion

        #region ================== Instance properties

        public KnownLayer Layer => KnownLayer.Selection;

        #endregion

        #region ================== Constructors

        public TextMarkerService (TextEditor textEd) {
            textEditor = textEd;
            markers = new TextSegmentCollection<TextMarker> (textEd.Document);

            TextView textView = textEd.TextArea.TextView;
            textView.BackgroundRenderers.Add (this);
            textView.LineTransformers.Add (this);
            textView.Services.AddService (typeof (TextMarkerService), this);
        }

        #endregion

        #region ================== Instance methods

        #region Public methods

        public void Clear () {
            while (markers.Count > 0) {
                var marker = markers.First ();
                markers.Remove (marker);
                Redraw (marker);
            }
        }

        public void Create (int offset, int length, Action<TextMarker> func) {
            var m = new TextMarker (offset, length);
            func?.Invoke (m);

            markers.Add (m);

            Redraw (m);
        }

        public IEnumerable<TextMarker> GetMarkersAtOffset (int offset) {
            return markers == null ? Enumerable.Empty<TextMarker> () : markers.FindSegmentsContaining (offset);
        }

        #endregion

        #region Interface methods

        void IBackgroundRenderer.Draw (TextView textView, DrawingContext drawingContext) {
            if (markers == null || !textView.VisualLinesValid)
                return;
            var visualLines = textView.VisualLines;
            if (visualLines.Count == 0)
                return;

            int viewStart = visualLines.First ().FirstDocumentLine.Offset;
            int viewEnd = visualLines.Last ().LastDocumentLine.EndOffset;
            foreach (TextMarker marker in markers.FindOverlappingSegments (viewStart, viewEnd - viewStart)) {
                if (marker.BackgroundColor != null) {
                    var geoBuilder = new BackgroundGeometryBuilder {AlignToWholePixels = true, CornerRadius = 3};
                    geoBuilder.AddSegment (textView, marker);

                    Geometry geometry = geoBuilder.CreateGeometry();
                    if (geometry != null) {
                        Color color = marker.BackgroundColor.Value;
                        var brush = new SolidColorBrush(color);
                        brush.Freeze ();
                        drawingContext.DrawGeometry (brush, null, geometry);
                    }
                }

                foreach (Rect r in BackgroundGeometryBuilder.GetRectsForSegment (textView, marker)) {
                    Point startPoint = r.BottomLeft;
                    Point endPoint = r.BottomRight;

                    var usedPen = new Pen (new SolidColorBrush (marker.MarkerColor), 1);
                    usedPen.Freeze ();
                    const double offset = 2.5;

                    int count = Math.Max ((int) ((endPoint.X - startPoint.X) / offset) + 1, 4);

                    var geometry = new StreamGeometry();

                    using (StreamGeometryContext ctx = geometry.Open ()) {
                        ctx.BeginFigure (startPoint, false, false);
                        ctx.PolyLineTo (CreatePoints (startPoint, endPoint, offset, count).ToArray (), true, false);
                    }

                    geometry.Freeze ();

                    drawingContext.DrawGeometry (Brushes.Transparent, usedPen, geometry);
                    break;
                }
            }
        }

        void IVisualLineTransformer.Transform (ITextRunConstructionContext context, IList<VisualLineElement> elements) { }

        #endregion

        #region Private methods

        private IEnumerable<Point> CreatePoints (Point start, Point end, double offset, int count) {
            for (int i = 0; i < count; i++) {
                double x = start.X + (i * offset);
                double y = start.Y - ((i + 1) % 2 == 0 ? offset : 0);
                yield return new Point (x, y);
            }
        }

        private void Remove (TextMarker marker) {
            if (markers.Remove (marker))
                Redraw (marker);
        }

        private void Redraw (ISegment segment) {
            textEditor.TextArea.TextView.Redraw (segment);
        }

        #endregion

        #endregion
    }
}
