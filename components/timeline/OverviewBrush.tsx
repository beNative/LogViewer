
import React from 'react';
import { LogDensityPointByLevel, LogDensityPoint, OverallTimeRange } from '../../types';
import { Theme, getLevelColor, LOG_LEVEL_ORDER } from '../../utils/timelineUtils';
import { ValueToPositionFn } from './Bar';

export type PositionToValueFn = (pos: number) => number;

interface OverviewBrushProps {
    minTime: number;
    maxTime: number;
    viewMin: number;
    viewMax: number;
    selectedMin: number | null;
    selectedMax: number | null;
    density: (LogDensityPointByLevel[] | LogDensityPoint[]);
    theme: Theme;
    valueToPosition: ValueToPositionFn;
    positionToValue: PositionToValueFn;
    onViewRangeChange: (range: OverallTimeRange | null) => void;
    onRangeChange: (startTime: number, endTime: number) => void;
    uiScale: number;
}

export const OverviewBrush: React.FC<OverviewBrushProps> = ({
    minTime, maxTime, viewMin, viewMax, selectedMin, selectedMax,
    density, theme, valueToPosition, positionToValue,
    onViewRangeChange, onRangeChange, uiScale
}) => {
    const containerRef = React.useRef<HTMLDivElement>(null);

    type OverviewDragState =
        | { type: 'brush'; startX: number; initialMin: number; initialMax: number }
        | { type: 'brush_left' | 'brush_right'; startX: number; initialMin: number; initialMax: number }
        | { type: 'new_overview_selection'; startX: number; startTime: number };

    const [dragState, setDragState] = React.useState<OverviewDragState | null>(null);
    const [tempSelection, setTempSelection] = React.useState<{ start: number, end: number } | null>(null);

    const handleBrushMouseDown = (e: React.MouseEvent, type: 'brush' | 'brush_left' | 'brush_right') => {
        e.stopPropagation();
        setDragState({ type, startX: e.clientX, initialMin: viewMin, initialMax: viewMax });
    };

    const handleContainerMouseDown = (e: React.MouseEvent) => {
        if (e.target !== e.currentTarget) return;
        e.stopPropagation();

        const rect = containerRef.current!.getBoundingClientRect();
        const clickPos = (e.clientX - rect.left) / uiScale;
        const clickTime = positionToValue(clickPos);
        setDragState({ type: 'new_overview_selection', startX: e.clientX, startTime: clickTime });
    };

    const handleMouseMove = React.useCallback((e: MouseEvent) => {
        if (!dragState || !containerRef.current) return;
        e.preventDefault();

        // const rect = containerRef.current.getBoundingClientRect(); // Unused here?
        // const currentPos = (e.clientX - rect.left) / uiScale; // Unused depending on logic logic below uses delta

        // Re-calculating current time for new selection
        let currentTime = 0;
        if (dragState.type === 'new_overview_selection') {
            const rect = containerRef.current.getBoundingClientRect();
            const currentPos = (e.clientX - rect.left) / uiScale;
            currentTime = positionToValue(currentPos);
        }

        const deltaX = e.clientX - dragState.startX;
        const unscaledDeltaX = deltaX / uiScale;
        const deltaTime = positionToValue(unscaledDeltaX) - positionToValue(0);

        if (dragState.type === 'new_overview_selection') {
            const start = Math.min(dragState.startTime, currentTime);
            const end = Math.max(dragState.startTime, currentTime);
            setTempSelection({ start, end });
        } else {
            let newMin = dragState.initialMin;
            let newMax = dragState.initialMax;

            if (dragState.type === 'brush') {
                newMin += deltaTime;
                newMax += deltaTime;
                if (newMin < minTime) {
                    const diff = minTime - newMin;
                    newMin = minTime;
                    newMax += diff;
                }
                if (newMax > maxTime) {
                    const diff = newMax - maxTime;
                    newMax = maxTime;
                    newMin -= diff;
                }
            } else if (dragState.type === 'brush_left') {
                newMin += deltaTime;
                if (newMin < minTime) newMin = minTime;
                if (newMin >= newMax) newMin = newMax - 1;
            } else if (dragState.type === 'brush_right') {
                newMax += deltaTime;
                if (newMax > maxTime) newMax = maxTime;
                if (newMax <= newMin) newMax = newMin + 1;
            }

            if (newMin < newMax) {
                onViewRangeChange({ min: newMin, max: newMax });
            }
        }
    }, [dragState, minTime, maxTime, positionToValue, onViewRangeChange, uiScale]);

    const handleMouseUp = React.useCallback((e: MouseEvent) => {
        if (dragState?.type === 'new_overview_selection') {
            if (Math.abs(e.clientX - dragState.startX) >= 5 && tempSelection) {
                onRangeChange(tempSelection.start, tempSelection.end);
            }
        }
        setDragState(null);
        setTempSelection(null);
    }, [dragState, onRangeChange, tempSelection]);

    React.useEffect(() => {
        const cursor = dragState?.type.startsWith('brush') ? 'col-resize' :
            dragState?.type === 'new_overview_selection' ? 'crosshair' : 'default';

        if (dragState) {
            document.body.style.cursor = cursor;
            document.addEventListener('mousemove', handleMouseMove);
            document.addEventListener('mouseup', handleMouseUp);
        }
        return () => {
            document.body.style.cursor = 'default';
            document.removeEventListener('mousemove', handleMouseMove);
            document.removeEventListener('mouseup', handleMouseUp);
        };
    }, [dragState, handleMouseMove, handleMouseUp]);

    const viewLeft = valueToPosition(viewMin);
    const viewWidth = valueToPosition(viewMax) - viewLeft;

    const currentSelection = tempSelection || (selectedMin !== null && selectedMax !== null ? { start: selectedMin, end: selectedMax } : null);

    const selectionLeft = currentSelection ? valueToPosition(currentSelection.start) : -1;
    const selectionWidth = currentSelection ? valueToPosition(currentSelection.end) - selectionLeft : 0;

    const maxTotalCount = React.useMemo(() => {
        return density.reduce((max: number, bucket: LogDensityPointByLevel | LogDensityPoint) => {
            const total = 'counts' in bucket ? Object.values(bucket.counts).reduce((sum: number, count: number) => sum + count, 0) : (bucket as LogDensityPoint).count;
            return Math.max(max, total);
        }, 1);
    }, [density]);

    return (
        <div
            ref={containerRef}
            onMouseDown={handleContainerMouseDown}
            className="relative w-full h-5 cursor-crosshair"
        >
            <div className="w-full h-full flex items-end pointer-events-none">
                {density.map((bucket: LogDensityPointByLevel | LogDensityPoint, i) => {
                    const total: number = 'counts' in bucket ? Object.values(bucket.counts).reduce((s: number, c: number) => s + c, 0) : (bucket as LogDensityPoint).count;
                    const heightPercent = total > 0 ? (total / maxTotalCount) * 100 : 0;

                    if ('counts' in bucket && total > 0) {
                        return (
                            <div key={i} style={{ flex: '1 1 0%', height: `${Math.max(5, heightPercent)}%` }} className="flex flex-col w-full relative group">
                                {Object.entries(bucket.counts)
                                    .sort((a, b) => (LOG_LEVEL_ORDER[a[0].toUpperCase()] ?? 99) - (LOG_LEVEL_ORDER[b[0].toUpperCase()] ?? 99))
                                    .map(([level, count]: [string, number]) => (
                                        <div key={level} style={{ height: `${(count / total) * 100}%`, backgroundColor: getLevelColor(level, theme) }} />
                                    ))}
                            </div>
                        )
                    }
                    const opacity = total > 0 ? 0.3 + (total / maxTotalCount) * 0.7 : 0;
                    return (
                        <div key={`density-${i}`} style={{ flex: '1 1 0%', height: '100%', backgroundColor: `rgba(147, 197, 253, ${opacity})` }} />
                    );
                })}
            </div>


            {/* Only show temp selection during drag, not persisted selection */}
            {tempSelection && selectionLeft >= 0 && selectionWidth > 0 && (
                <div
                    className="absolute top-0 h-full bg-sky-500/20 dark:bg-sky-400/20 pointer-events-none"
                    style={{ left: `${selectionLeft}px`, width: `${selectionWidth}px` }}
                />
            )}

            <div
                onMouseDown={(e) => handleBrushMouseDown(e, 'brush')}
                className="absolute top-0 h-full bg-black/20 dark:bg-white/20 cursor-move border-x-2 border-sky-600 dark:border-sky-400"
                style={{ left: `${viewLeft}px`, width: `${viewWidth}px` }}
            >
                <div onMouseDown={(e) => handleBrushMouseDown(e, 'brush_left')} className="absolute left-0 top-0 w-2 h-full cursor-col-resize bg-sky-600/50 hover:bg-sky-600" />
                <div onMouseDown={(e) => handleBrushMouseDown(e, 'brush_right')} className="absolute right-0 top-0 w-2 h-full cursor-col-resize bg-sky-600/50 hover:bg-sky-600" />
            </div>
        </div>
    );
};
