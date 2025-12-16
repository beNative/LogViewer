import React from 'react';
import { PageTimestampRange, FileTimeRange, LogDensityPointByLevel, OverallTimeRange, IconSet, LogDensityPoint, TimelineBarVisibility } from '../types.ts';
import { Icon } from './icons/index.tsx';
import { Tooltip } from './Tooltip.tsx';
import { TimelineBarVisibilityMenu } from './TimelineBarVisibilityMenu.tsx';
import { Bar, DensityBar, ValueToPositionFn } from './timeline/Bar.tsx';
import {
    Theme,
    TIMELINE_PALETTE as PALETTE,
    LOG_LEVEL_COLORS,
    LOG_LEVEL_ORDER,
    getLevelColor,
    formatTimestamp,
    formatTickLabel
} from '../utils/timelineUtils.ts';
import { useTimeline } from '../contexts/TimelineContext';
import { useResizeObserver } from '../hooks/useResizeObserver';

type PositionToValueFn = (pos: number) => number;

// New union type for clarity
type BucketData = LogDensityPointByLevel | LogDensityPoint;

interface TimeRangeSelectorProps {
    minTime: number;
    maxTime: number;
    selectedStartTime: number | null;
    selectedEndTime: number | null;
    onRangeChange: (startTime: number, endTime: number) => void;
    onClear: () => void;
    theme: Theme;
    pageTimestampRanges: PageTimestampRange[];
    fileTimeRanges: FileTimeRange[];
    logDensity: (LogDensityPointByLevel[] | LogDensityPoint[]);
    overallLogDensity: (LogDensityPointByLevel[] | LogDensityPoint[]);
    datesWithLogs: string[];
    onGoToPage?: (page: number) => void;
    onCursorChange: (time: number) => void;
    onFileSelect: (fileName: string) => void;
    onDateSelect: (date: string) => void;
    cursorTime?: number | null;
    activeFileName?: string | null;
    activeDate?: string | null;
    currentPage?: number | null;
    zoomToSelectionEnabled: boolean;
    iconSet: IconSet;
    uiScale: number;
    timelineBarVisibility: TimelineBarVisibility;
    onTimelineBarVisibilityChange: (newVisibility: TimelineBarVisibility) => void;
}

type DragState =
    | { type: 'select_left'; startX: number; initialStart: number; initialEnd: number }
    | { type: 'select_right'; startX: number; initialStart: number; initialEnd: number }
    | { type: 'cursor'; startX: number }
    | { type: 'new_selection'; startX: number; startTime: number };



const formatTooltip = (time: number | null): React.ReactNode => {
    if (typeof time !== 'number' || isNaN(time)) return 'N/A';
    const d = new Date(time);
    const isoString = d.toISOString(); // YYYY-MM-DDTHH:mm:ss.sssZ
    const datePart = isoString.substring(0, 10);
    const timePart = isoString.substring(11, 23); // Includes milliseconds

    return (
        <div className="text-center leading-tight">
            <div>{datePart}</div>
            <div>{timePart}</div>
        </div>
    );
};

// Generic Bar component for rendering timeline segments


// ... existing OverviewBrush component ...

export const TimeRangeSelector: React.FC<TimeRangeSelectorProps> = ({
    minTime, maxTime, selectedStartTime, selectedEndTime,
    onRangeChange, onClear, theme,
    pageTimestampRanges, fileTimeRanges, logDensity, overallLogDensity, datesWithLogs, onGoToPage,
    onCursorChange, onFileSelect, onDateSelect,
    cursorTime = null, activeFileName = null, activeDate = null, currentPage = null,
    zoomToSelectionEnabled, iconSet, uiScale,
    timelineBarVisibility, onTimelineBarVisibilityChange
}) => {
    const { viewRange, setViewRange, zoomToSelection, resetZoom, setSelection, selection } = useTimeline();
    const mainContainerRef = React.useRef<HTMLDivElement>(null);
    const overviewContainerRef = React.useRef<HTMLDivElement>(null);
    const [dragState, setDragState] = React.useState<DragState | null>(null);
    const [tempSelection, setTempSelection] = React.useState<{ start: number, end: number } | null>(null);
    const [tempCursorTime, setTempCursorTime] = React.useState<number | null>(null);
    const [densityTooltip, setDensityTooltip] = React.useState<{ x: number, y: number, bucketData: BucketData } | null>(null);
    const [contextMenu, setContextMenu] = React.useState<{ x: number, y: number } | null>(null);

    const [optimisticSelection, setOptimisticSelection] = React.useState<{ start: number, end: number } | null>(null);
    const [optimisticViewRange, setOptimisticViewRange] = React.useState<{ min: number, max: number } | null>(null);

    // Clear optimistic state when props update to match (or close enough)
    React.useEffect(() => {
        if (optimisticSelection && selectedStartTime !== null && selectedEndTime !== null) {
            // If props have caught up to optimistic state (within 100ms tolerance), clear optimistic
            if (Math.abs(selectedStartTime - optimisticSelection.start) < 100 &&
                Math.abs(selectedEndTime - optimisticSelection.end) < 100) {
                setOptimisticSelection(null);
            }
        }
        // Sync props to Context Selection so zoomToSelection works
        if (selectedStartTime !== null && selectedEndTime !== null) {
            // We use a timeout or direct set? Direct set is fine but need to avoid loops.
            // Context.setSelection does not trigger props update directly (props come from App filters).
            // But App filters change triggers selectedStartTime change.
            // So this is safe.
            // Check if context needs update to avoid infinite loop if context updates cause prop updates (unlikely here as flow is unidir).
            // Actually, we can just call setSelection.
            // But we need to use 'useTimeline' setSelection.
            setSelection(selectedStartTime, selectedEndTime);
            // Wait, I need to destruct setSelection first!
        }
    }, [selectedStartTime, selectedEndTime, optimisticSelection, setSelection]);

    React.useEffect(() => {
        if (optimisticViewRange && viewRange) {
            if (Math.abs(viewRange.min - optimisticViewRange.min) < 100 &&
                Math.abs(viewRange.max - optimisticViewRange.max) < 100) {
                setOptimisticViewRange(null);
            }
        }
    }, [viewRange, optimisticViewRange]);

    // Fallback: Clear optimistic state after a timeout to prevent getting stuck if props never update
    React.useEffect(() => {
        if (optimisticSelection || optimisticViewRange) {
            const timer = setTimeout(() => {
                setOptimisticSelection(null);
                setOptimisticViewRange(null);
            }, 1000);
            return () => clearTimeout(timer);
        }
    }, [optimisticSelection, optimisticViewRange]);


    const handleOverviewRangeChange = React.useCallback((range: OverallTimeRange | null) => {
        if (range) {
            setOptimisticViewRange(range);
        }
        setViewRange(range);
    }, [setViewRange]);

    const displayMinTime = optimisticViewRange?.min ?? viewRange?.min ?? minTime;
    const displayMaxTime = optimisticViewRange?.max ?? viewRange?.max ?? maxTime;

    // Local implementation replaced by shared hook
    /*
    const useResizeObserver = (ref: React.RefObject<HTMLElement>, isVisible: boolean = true) => {
       ...
    };
    */

    // Use debounced observers (50ms) to prevent excessive recalculations during rapid resize
    const mainContainerWidth = useResizeObserver(mainContainerRef, true, 50);
    const overviewContainerWidth = useResizeObserver(overviewContainerRef, timelineBarVisibility.overview, 50);

    const mainPosToValue = React.useCallback((p: number) => {
        if (mainContainerWidth === 0 || displayMaxTime === displayMinTime) return displayMinTime;
        return displayMinTime + (p / mainContainerWidth) * (displayMaxTime - displayMinTime);
    }, [displayMinTime, displayMaxTime, mainContainerWidth]);

    const mainValueToPos = React.useCallback((v: number) => {
        if (displayMaxTime === displayMinTime) return 0;
        return ((v - displayMinTime) / (displayMaxTime - displayMinTime)) * mainContainerWidth;
    }, [displayMinTime, displayMaxTime, mainContainerWidth]);

    const overviewValueToPos = React.useCallback((v: number) => ((v - minTime) / (maxTime - minTime)) * overviewContainerWidth, [minTime, maxTime, overviewContainerWidth]);
    const overviewPosToValue = React.useCallback((p: number) => minTime + (p / overviewContainerWidth) * (maxTime - minTime), [minTime, maxTime, overviewContainerWidth]);

    React.useEffect(() => {
        if (tempCursorTime !== null && cursorTime !== null && Math.abs(tempCursorTime - cursorTime) < 1000) {
            setTempCursorTime(null);
        }
    }, [cursorTime, tempCursorTime]);


    const handleMouseDown = (
        e: React.MouseEvent,
        type: 'select_left' | 'select_right' | 'cursor' | 'new_selection'
    ) => {
        e.stopPropagation();
        e.preventDefault();

        const rect = mainContainerRef.current!.getBoundingClientRect();
        const clickPos = (e.clientX - rect.left) / uiScale;
        const clickTime = mainPosToValue(clickPos);

        if (type === 'cursor') {
            setDragState({ type: 'cursor', startX: e.clientX });
        } else if (type === 'new_selection') {
            setDragState({ type: 'new_selection', startX: e.clientX, startTime: clickTime });
        } else {
            setDragState({
                type: type,
                startX: e.clientX,
                initialStart: selectedStartTime || displayMinTime,
                initialEnd: selectedEndTime || displayMaxTime,
            });
        }
    };

    React.useEffect(() => {
        const handleMouseMove = (e: MouseEvent) => {
            if (!dragState || !mainContainerRef.current) return;
            e.preventDefault();
            const rect = mainContainerRef.current.getBoundingClientRect();
            const currentUnscaledPos = (e.clientX - rect.left) / uiScale;
            const currentTime = mainPosToValue(currentUnscaledPos);

            switch (dragState.type) {
                case 'new_selection': {
                    const start = Math.min(dragState.startTime, currentTime);
                    const end = Math.max(dragState.startTime, currentTime);
                    setTempSelection({ start, end });
                    break;
                }
                case 'select_left': {
                    const newStart = Math.min(dragState.initialEnd, Math.max(displayMinTime, currentTime));
                    setTempSelection({ start: newStart, end: dragState.initialEnd });
                    break;
                }
                case 'select_right': {
                    const newEnd = Math.max(dragState.initialStart, Math.min(displayMaxTime, currentTime));
                    setTempSelection({ start: dragState.initialStart, end: newEnd });
                    break;
                }
                case 'cursor': {
                    const newCursorTime = Math.max(displayMinTime, Math.min(displayMaxTime, currentTime));
                    setTempCursorTime(newCursorTime);
                    break;
                }
            }
        };

        const handleMouseUp = (e: MouseEvent) => {
            if (!dragState) return;

            if (dragState.type === 'new_selection') {
                if (Math.abs(e.clientX - dragState.startX) < 5) {
                    onCursorChange(dragState.startTime);
                } else if (tempSelection) {
                    // Optimistic update
                    setOptimisticSelection(tempSelection);
                    // If zoom enabled, optimistic view update
                    if (zoomToSelectionEnabled) {
                        setOptimisticViewRange({ min: tempSelection.start, max: tempSelection.end });
                        // DIRECT AUTO-ZOOM (User Request)
                        setViewRange({ min: tempSelection.start, max: tempSelection.end });
                    }
                    onRangeChange(tempSelection.start, tempSelection.end);
                }
            } else if (dragState.type === 'select_left' || dragState.type === 'select_right') {
                if (tempSelection) {
                    setOptimisticSelection(tempSelection);
                    if (zoomToSelectionEnabled) {
                        setOptimisticViewRange({ min: tempSelection.start, max: tempSelection.end });
                        // DIRECT AUTO-ZOOM
                        setViewRange({ min: tempSelection.start, max: tempSelection.end });
                    }
                    onRangeChange(tempSelection.start, tempSelection.end);
                }
            } else if (dragState.type === 'cursor' && tempCursorTime !== null) {
                onCursorChange(tempCursorTime);
            }

            setDragState(null);
            setTempSelection(null);
        };

        if (dragState) {
            document.body.style.cursor = 'col-resize';
            document.addEventListener('mousemove', handleMouseMove);
            document.addEventListener('mouseup', handleMouseUp);
        }
        return () => {
            document.body.style.cursor = 'default';
            document.removeEventListener('mousemove', handleMouseMove);
            document.removeEventListener('mouseup', handleMouseUp);
        };
    }, [dragState, mainPosToValue, displayMinTime, displayMaxTime, onRangeChange, onCursorChange, tempSelection, tempCursorTime, uiScale, zoomToSelectionEnabled, setViewRange]);

    const handleDensityHover = (e: React.MouseEvent<HTMLDivElement>) => {
        const rect = e.currentTarget.getBoundingClientRect();
        const x = e.clientX - rect.left;
        const hoverTime = mainPosToValue(x);

        if (logDensity.length < 2) {
            setDensityTooltip(null);
            return;
        }

        const bucketDuration = logDensity[1].time - logDensity[0].time;
        if (bucketDuration <= 0) {
            setDensityTooltip(null);
            return;
        }

        // Calculate bucket index with bounds validation
        const rawIndex = Math.floor((hoverTime - logDensity[0].time) / bucketDuration);
        const bucketIndex = Math.max(0, Math.min(rawIndex, logDensity.length - 1));
        const bucketData = logDensity[bucketIndex];

        if (!bucketData) {
            setDensityTooltip(null);
            return;
        }

        const isLevelData = 'counts' in bucketData;
        const hasData = isLevelData
            ? Object.keys((bucketData as LogDensityPointByLevel).counts).length > 0
            : (bucketData as LogDensityPoint).count > 0;

        if (hasData) {
            setDensityTooltip({ x: e.clientX, y: rect.top, bucketData: bucketData });
        } else {
            setDensityTooltip(null);
        }
    };

    const handleDensityLeave = () => setDensityTooltip(null);

    const handleLabelsContextMenu = (e: React.MouseEvent) => {
        e.preventDefault();
        setContextMenu({ x: e.clientX, y: e.clientY });
    };


    const barComponents = React.useMemo(() => {
        const components = [];
        const barProps = { displayMinTime: displayMinTime, displayMaxTime: displayMaxTime };

        if (timelineBarVisibility.pages && pageTimestampRanges.length > 0 && onGoToPage) components.push({ key: 'page', label: 'Pages', Comp: Bar, props: { ...barProps, items: pageTimestampRanges, isActive: (item: PageTimestampRange) => item.page === currentPage, onSelect: (item: PageTimestampRange) => onGoToPage(item.page), getLabel: (item: PageTimestampRange) => String(item.page), getTitle: (item: PageTimestampRange) => `Page ${item.page}`, getStart: (item: PageTimestampRange) => new Date(item.startTime + 'Z').getTime(), getEnd: (item: PageTimestampRange) => new Date(item.endTime + 'Z').getTime(), getColor: (i: number) => PALETTE[i % PALETTE.length] } });
        if (timelineBarVisibility.files && fileTimeRanges.length > 0) components.push({ key: 'file', label: 'Files', Comp: Bar, props: { ...barProps, items: fileTimeRanges, isActive: (item: FileTimeRange) => item.name === activeFileName, onSelect: (item: FileTimeRange) => onFileSelect(item.name), getLabel: (item: FileTimeRange) => item.name.split('/').pop() || '', getTitle: (item: FileTimeRange) => item.name, getStart: (item: FileTimeRange) => new Date(item.startTime + 'Z').getTime(), getEnd: (item: FileTimeRange) => new Date(item.endTime + 'Z').getTime(), getColor: (i: number) => PALETTE[i % PALETTE.length] } });
        if (timelineBarVisibility.dates && datesWithLogs.length > 0) components.push({ key: 'date', label: 'Date', Comp: Bar, props: { ...barProps, items: datesWithLogs, isActive: (item: string) => item === activeDate, onSelect: (item: string) => onDateSelect(item), getLabel: (item: string) => item, getTitle: (item: string) => item, getStart: (item: string) => new Date(`${item}T00:00:00.000Z`).getTime(), getEnd: (item: string) => new Date(`${item}T23:59:59.999Z`).getTime(), getColor: (i: number) => PALETTE[i % PALETTE.length] } });
        if (timelineBarVisibility.density && logDensity.length > 0) components.push({
            key: 'density',
            label: 'Density',
            Comp: DensityBar,
            props: {
                items: logDensity,
                theme: theme,
                ...barProps,
                onMouseMove: handleDensityHover,
                onMouseLeave: handleDensityLeave
            }
        });
        return components;
    }, [displayMinTime, displayMaxTime, timelineBarVisibility, pageTimestampRanges, fileTimeRanges, datesWithLogs, logDensity, currentPage, activeFileName, activeDate, theme, onGoToPage, onFileSelect, onDateSelect]);

    const currentStart = tempSelection?.start ?? optimisticSelection?.start ?? selectedStartTime;
    const currentEnd = tempSelection?.end ?? optimisticSelection?.end ?? selectedEndTime;

    const visibleSelectionStart = currentStart !== null ? Math.max(currentStart, displayMinTime) : null;
    const visibleSelectionEnd = currentEnd !== null ? Math.min(currentEnd, displayMaxTime) : null;

    const startMatches = currentStart !== null && Math.abs(currentStart - displayMinTime) < (displayMaxTime - displayMinTime) * 0.001;
    const endMatches = currentEnd !== null && Math.abs(currentEnd - displayMaxTime) < (displayMaxTime - displayMinTime) * 0.001;
    // Show selection if dragging (tempSelection) OR if selection doesn't match the current view
    const showSelection = !!tempSelection || !(startMatches && endMatches);

    const startPos = visibleSelectionStart !== null ? mainValueToPos(visibleSelectionStart) : -1;
    const endPos = visibleSelectionEnd !== null ? mainValueToPos(visibleSelectionEnd) : -1;

    // formatTickLabel imported from timelineUtils

    const ticks = React.useMemo(() => {
        if (!mainContainerWidth || mainContainerWidth < 100) return [];

        const tickCount = Math.max(2, Math.floor(mainContainerWidth / 120));
        const duration = displayMaxTime - displayMinTime;
        if (duration <= 0) return [];

        const niceIntervals = [
            1000, 5000, 15000, 30000, // seconds
            60 * 1000, 5 * 60 * 1000, 15 * 60 * 1000, 30 * 60 * 1000, // minutes
            60 * 60 * 1000, 2 * 60 * 60 * 1000, 6 * 60 * 60 * 1000, 12 * 60 * 60 * 1000, // hours
            24 * 60 * 60 * 1000, 2 * 24 * 60 * 60 * 1000, 7 * 24 * 60 * 60 * 1000 // days
        ];
        const targetStep = duration / tickCount;
        const step = niceIntervals.find(i => i > targetStep) || niceIntervals[niceIntervals.length - 1];

        const tickValues: number[] = [];
        const startTick = Math.ceil(displayMinTime / step) * step;

        for (let t = startTick; t <= displayMaxTime; t += step) {
            tickValues.push(t);
        }
        return tickValues;
    }, [displayMinTime, displayMaxTime, mainContainerWidth]);

    const finalCursorTime = tempCursorTime ?? cursorTime;


    return (
        <div className="w-full">
            <div className="flex items-start gap-3 w-full">
                <div
                    className="w-16 flex-shrink-0 text-right"
                    onContextMenu={handleLabelsContextMenu}
                >
                    {/* This div is a spacer to align with the timeline content */}
                    <div className="h-4" />
                    <div className="space-y-2">
                        {barComponents.map(bar => (
                            <div key={bar.key} className="h-5 text-xs font-semibold text-gray-600 dark:text-gray-400 flex items-center justify-end pr-2">
                                {bar.label}
                            </div>
                        ))}
                    </div>
                </div>

                <div className="flex-grow flex flex-col relative">
                    {/* Tooltips Container */}
                    <div className="absolute top-0 w-full h-5 pointer-events-none z-40">
                        {showSelection && startPos >= 0 && <div className="absolute top-0 text-xs font-mono text-gray-700 dark:text-gray-200 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm px-1.5 py-0.5 rounded shadow" style={{ left: `${startPos}px`, transform: 'translateX(-50%)' }}>{formatTooltip(currentStart)}</div>}
                        {showSelection && endPos >= 0 && (endPos - startPos > 60) && <div className="absolute top-0 text-xs font-mono text-gray-700 dark:text-gray-200 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm px-1.5 py-0.5 rounded shadow" style={{ left: `${endPos}px`, transform: 'translateX(-50%)' }}>{formatTooltip(currentEnd)}</div>}
                        {finalCursorTime !== null && finalCursorTime >= displayMinTime && finalCursorTime <= displayMaxTime && <div className="absolute top-0 text-xs font-mono bg-red-500/90 backdrop-blur-sm text-white rounded px-1.5 py-0.5" style={{ left: `${mainValueToPos(finalCursorTime)}px`, transform: 'translateX(-50%)' }}>{formatTooltip(finalCursorTime)}</div>}
                    </div>

                    {/* Main Timeline Area */}
                    <div
                        onMouseDown={(e) => handleMouseDown(e, 'new_selection')}
                        className="w-full cursor-crosshair bg-gray-200 dark:bg-gray-700/50 rounded p-1 overflow-hidden mt-4"
                    >
                        <div ref={mainContainerRef} className="relative">
                            <div className="space-y-2">
                                {barComponents.map(bar => (
                                    <bar.Comp key={bar.key} valueToPosition={mainValueToPos} {...bar.props} />
                                ))}
                            </div>
                            {startPos >= 0 && endPos >= 0 && (endPos > startPos) && showSelection && (
                                <div
                                    className="absolute top-0 bottom-0 bg-sky-500/20 dark:bg-sky-400/20 z-10 pointer-events-none"
                                    style={{ left: `${startPos}px`, width: `${Math.max(0, endPos - startPos)}px` }}
                                >
                                    <div
                                        data-handle="left"
                                        onMouseDown={(e) => handleMouseDown(e, 'select_left')}
                                        className="absolute top-0 -left-2 w-4 h-full cursor-col-resize group pointer-events-auto"
                                    >
                                        <div className="absolute left-1/2 -translate-x-1/2 top-1/2 -translate-y-1/2 h-8 w-1.5 bg-sky-600 dark:bg-sky-400 rounded-full opacity-0 group-hover:opacity-100 transition-opacity duration-150" />
                                    </div>
                                    <div
                                        data-handle="right"
                                        onMouseDown={(e) => handleMouseDown(e, 'select_right')}
                                        className="absolute top-0 -right-2 w-4 h-full cursor-col-resize group pointer-events-auto"
                                    >
                                        <div className="absolute left-1/2 -translate-x-1/2 top-1/2 -translate-y-1/2 h-8 w-1.5 bg-sky-600 dark:bg-sky-400 rounded-full opacity-0 group-hover:opacity-100 transition-opacity duration-150" />
                                    </div>
                                </div>
                            )}

                            {finalCursorTime !== null && mainContainerWidth > 0 && finalCursorTime >= displayMinTime && finalCursorTime <= displayMaxTime && (
                                <div
                                    data-handle="cursor"
                                    onMouseDown={(e) => handleMouseDown(e, 'cursor')}
                                    className="absolute top-0 bottom-0 bg-red-500 z-30 cursor-col-resize"
                                    style={{ left: `${mainValueToPos(finalCursorTime)}px`, width: '4pt', transform: 'translateX(-2pt)' }}
                                />
                            )}
                        </div>
                    </div>
                    {/* Time Axis Ticks */}
                    <div className="relative w-full h-5 mt-1 pointer-events-none">
                        {ticks.map(tick => {
                            const pos = mainValueToPos(tick);
                            if (pos < 20 || pos > mainContainerWidth - 20) return null;
                            return (
                                <div key={tick} className="absolute top-0 text-center" style={{ left: `${pos}px`, transform: 'translateX(-50%)' }}>
                                    <div className="w-px h-1.5 bg-gray-400 dark:bg-gray-500 mx-auto" />
                                    <span className="block mt-0.5 text-xs text-gray-500 dark:text-gray-400 font-mono whitespace-nowrap">
                                        {formatTickLabel(tick, displayMaxTime - displayMinTime)}
                                    </span>
                                </div>
                            );
                        })}
                    </div>
                </div>

                <div className="w-auto flex-shrink-0 self-start pt-4 flex flex-col items-center gap-1">
                    <Tooltip content="Zoom to Selection">
                        <button onClick={zoomToSelection} disabled={!zoomToSelectionEnabled || !selection} className="p-1.5 text-gray-500 dark:text-gray-400 rounded-full hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors disabled:opacity-50 disabled:cursor-not-allowed"><Icon name="ArrowsPointingIn" iconSet={iconSet} className="w-4 h-4" /></button>
                    </Tooltip>
                    <Tooltip content="Zoom to Extent">
                        <button onClick={resetZoom} disabled={!viewRange} className="p-1.5 text-gray-500 dark:text-gray-400 rounded-full hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors disabled:opacity-50 disabled:cursor-not-allowed"><Icon name="ArrowsPointingOut" iconSet={iconSet} className="w-4 h-4" /></button>
                    </Tooltip>
                    <Tooltip content="Clear time selection">
                        <button onClick={onClear} className="p-1.5 text-gray-500 dark:text-gray-400 rounded-full hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors"><Icon name="XMark" iconSet={iconSet} className="w-4 h-4" /></button>
                    </Tooltip>
                </div>
            </div>
            {timelineBarVisibility.overview && (
                <div className="flex items-start gap-3 w-full mt-2">
                    <div className="w-16 flex-shrink-0 text-right pr-1 text-xs font-medium text-gray-500 dark:text-gray-400 pt-3">
                        Overview
                    </div>
                    <div className="flex-grow" ref={overviewContainerRef}>
                        {overviewContainerWidth > 0 && (
                            <OverviewBrush
                                minTime={minTime} maxTime={maxTime}
                                viewMin={displayMinTime} viewMax={displayMaxTime}
                                selectedMin={selectedStartTime}
                                selectedMax={selectedEndTime}
                                density={overallLogDensity} theme={theme}
                                valueToPosition={overviewValueToPos} positionToValue={overviewPosToValue}
                                onViewRangeChange={handleOverviewRangeChange}
                                onRangeChange={onRangeChange}
                                uiScale={uiScale}
                            />
                        )}
                    </div>
                    <div className="w-[60px] flex-shrink-0" />
                </div>
            )}
            {densityTooltip && <DensityTooltip {...densityTooltip} theme={theme} />}
            {contextMenu && (
                <TimelineBarVisibilityMenu
                    x={contextMenu.x}
                    y={contextMenu.y}
                    onClose={() => setContextMenu(null)}
                    visibility={timelineBarVisibility}
                    onVisibilityChange={onTimelineBarVisibilityChange}
                />
            )}
        </div>
    );
};

const DensityTooltip: React.FC<{ x: number, y: number, bucketData: BucketData, theme: Theme }> = ({ x, y, bucketData, theme }) => {
    const ref = React.useRef<HTMLDivElement>(null);
    const [style, setStyle] = React.useState<React.CSSProperties>({
        position: 'fixed',
        // Start off-screen and invisible to prevent flicker while calculating position
        top: -9999,
        left: -9999,
        opacity: 0,
    });

    React.useLayoutEffect(() => {
        if (ref.current) {
            const tooltip = ref.current;
            const { width, height } = tooltip.getBoundingClientRect();
            const GAP = 10; // 10px gap from viewport edges

            // Default position: centered above the cursor
            let newTop = y - height - GAP;
            let newLeft = x - (width / 2);

            // Check for vertical boundary collision
            if (newTop < GAP) {
                // Not enough space above, flip to below
                newTop = y + 20; // Some gap below the cursor
            }

            // Check for left boundary collision
            if (newLeft < GAP) {
                newLeft = GAP;
            }

            // Check for right boundary collision
            if (newLeft + width > window.innerWidth - GAP) {
                newLeft = window.innerWidth - width - GAP;
            }

            setStyle({
                position: 'fixed',
                top: newTop,
                left: newLeft,
                // The animate-fadeIn class will handle the opacity transition
            });
        }
    }, [x, y, bucketData]);

    const isLevelData = 'counts' in bucketData;
    const total = isLevelData ? Object.values(bucketData.counts).reduce((sum, count) => sum + count, 0) : bucketData.count;
    const sortedLevels = isLevelData ? Object.entries(bucketData.counts).sort((a, b) => b[1] - a[1]) : [];
    const unitLabel = isLevelData ? 'logs' : 'density';

    return (
        <div ref={ref} style={style} className="z-50 p-2.5 bg-gray-800/90 dark:bg-black/80 text-white rounded-lg shadow-2xl pointer-events-none w-56 backdrop-blur-sm animate-fadeIn">
            <div className="text-center mb-2">
                <p className="text-xs text-gray-400">Time Bucket</p>
                <p className="font-mono text-sm">{new Date(bucketData.time).toLocaleString()}</p>
                <p className="font-bold text-lg">{total.toLocaleString()} <span className="text-sm font-normal text-gray-300">{unitLabel}</span></p>
            </div>
            {isLevelData && (
                <div className="space-y-1">
                    {sortedLevels.map(([level, count]) => {
                        const percentage = total > 0 ? (count / total) * 100 : 0;
                        return (
                            <div key={level} className="text-xs">
                                <div className="flex justify-between items-center mb-0.5">
                                    <span className="font-semibold" style={{ color: getLevelColor(level, theme) }}>{level}</span>
                                    <span className="font-mono">{count.toLocaleString()} ({percentage.toFixed(1)}%)</span>
                                </div>
                                <div className="w-full bg-gray-600/50 rounded-full h-1.5">
                                    <div className="h-1.5 rounded-full" style={{ width: `${percentage}%`, backgroundColor: getLevelColor(level, theme) }} />
                                </div>
                            </div>
                        );
                    })}
                </div>
            )}
        </div>
    );
};


// Internal OverviewBrush component - handles the mini-map visualization and interaction.
// TODO: Extract to separate file in future refactoring phase.
const OverviewBrush: React.FC<{
    minTime: number;
    maxTime: number;
    viewMin: number;
    viewMax: number;
    selectedMin: number | null;
    selectedMax: number | null;
    density: (LogDensityPointByLevel[] | LogDensityPoint[]); // Accept both types
    theme: Theme;
    valueToPosition: ValueToPositionFn;
    positionToValue: PositionToValueFn;
    onViewRangeChange: (range: OverallTimeRange | null) => void;
    onRangeChange: (startTime: number, endTime: number) => void;
    uiScale: number;
}> = ({ minTime, maxTime, viewMin, viewMax, selectedMin, selectedMax, density, theme, valueToPosition, positionToValue, onViewRangeChange, onRangeChange, uiScale }) => {
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

        const rect = containerRef.current.getBoundingClientRect();
        const currentPos = (e.clientX - rect.left) / uiScale;
        const currentTime = positionToValue(currentPos);

        const deltaX = e.clientX - dragState.startX;
        const unscaledDeltaX = deltaX / uiScale;
        const deltaTime = positionToValue(unscaledDeltaX) - positionToValue(0);

        if (dragState.type === 'new_overview_selection') {
            const start = Math.min(dragState.startTime, currentTime);
            const end = Math.max(dragState.startTime, currentTime);
            setTempSelection({ start, end });
        } else {
            // After narrowing, TypeScript knows dragState has initialMin/initialMax
            // since 'new_overview_selection' is excluded from the union
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
            className="relative w-full h-10 bg-gray-200 dark:bg-gray-700/50 rounded p-0.5 cursor-crosshair"
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

            {selectionLeft >= 0 && selectionWidth > 0 && (
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
