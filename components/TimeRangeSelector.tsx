import React from 'react';
import { PageTimestampRange, FileTimeRange, LogDensityPointByLevel, OverallTimeRange, IconSet, LogDensityPoint, TimelineBarVisibility } from '../types.ts';
import { Icon } from './icons/index.tsx';
import { Tooltip } from './Tooltip.tsx';
import { TimelineBarVisibilityMenu } from './TimelineBarVisibilityMenu.tsx';
import { Bar, DensityBar, ValueToPositionFn } from './timeline/Bar.tsx';
import { OverviewBrush } from './timeline/OverviewBrush.tsx';
import { DensityTooltip } from './timeline/DensityTooltip.tsx';
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
import { useOptimisticState } from '../hooks/useOptimisticState';
import { useTimelineDrag } from '../hooks/useTimelineDrag';



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

    const [densityTooltip, setDensityTooltip] = React.useState<{ x: number, y: number, bucketData: BucketData } | null>(null);
    const [contextMenu, setContextMenu] = React.useState<{ x: number, y: number } | null>(null);

    const {
        optimisticSelection,
        setOptimisticSelection,
        optimisticViewRange,
        setOptimisticViewRange
    } = useOptimisticState(selectedStartTime, selectedEndTime, viewRange, setSelection);


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

    const {
        dragState,
        tempSelection,
        tempCursorTime,
        handleMouseDown
    } = useTimelineDrag({
        mainContainerRef,
        uiScale,
        displayMinTime,
        displayMaxTime,
        selectedStartTime,
        selectedEndTime,
        cursorTime,
        mainPosToValue,
        onRangeChange,
        onCursorChange,
        zoomToSelectionEnabled,
        setOptimisticSelection,
        setOptimisticViewRange,
        setViewRange
    });

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
                                {barComponents.map(bar => {
                                    const Component = bar.Comp as any;
                                    return <Component key={bar.key} valueToPosition={mainValueToPos} {...bar.props} />;
                                })}
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




// Internal OverviewBrush component - handles the mini-map visualization and interaction.

