import React from 'react';
import { PageTimestampRange, FileTimeRange, LogDensityPointByLevel, ViewMode, OverallTimeRange, IconSet, LogDensityPoint, TimelineBarVisibility } from '../types.ts';
import { Icon } from './icons/index.tsx';
import { Tooltip } from './Tooltip.tsx';
import { TimelineBarVisibilityMenu } from './TimelineBarVisibilityMenu.tsx';

type Theme = 'light' | 'dark';
type ValueToPositionFn = (value: number) => number;
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
    viewMode: ViewMode;
    onGoToPage?: (page: number) => void;
    onCursorChange: (time: number) => void;
    onFileSelect: (fileName: string) => void;
    onDateSelect: (date: string) => void;
    cursorTime?: number | null;
    activeFileName?: string | null;
    activeDate?: string | null;
    currentPage?: number | null;
    viewRange: OverallTimeRange | null;
    onViewRangeChange: (range: OverallTimeRange | null) => void;
    onZoomToSelection: () => void;
    onZoomToExtent: () => void;
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

const PALETTE = [
  '#3b82f6', '#ef4444', '#10b981', '#f59e0b', '#8b5cf6', '#ec4899', 
  '#6366f1', '#06b6d4', '#d946ef', '#f43f5e', '#14b8a6', '#eab308'
];

const LOG_LEVEL_COLORS: Record<string, { light: string, dark: string }> = {
    'ERROR': { light: '#ef4444', dark: '#f87171' },
    'FATAL': { light: '#b91c1c', dark: '#ef4444' },
    'WARNING': { light: '#f59e0b', dark: '#facc15' },
    'WARN': { light: '#f59e0b', dark: '#facc15' },
    'INFO': { light: '#3b82f6', dark: '#60a5fa' },
    'DEBUG': { light: '#6b7280', dark: '#9ca3af' },
    'TRACE': { light: '#a1a1aa', dark: '#71717a' },
    'DEFAULT': { light: '#d1d5db', dark: '#4b5563' },
};

const getLevelColor = (level: string, theme: Theme) => {
    const upperLevel = level?.toUpperCase() || 'DEFAULT';
    return (LOG_LEVEL_COLORS[upperLevel] || LOG_LEVEL_COLORS.DEFAULT)[theme];
};

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

const Bar: React.FC<{
    items: any[];
    valueToPosition: ValueToPositionFn;
    isActive: (item: any) => boolean;
    onSelect: (item: any) => void;
    getLabel: (item: any) => string;
    getTitle: (item: any) => string;
    getStart: (item: any) => number;
    getEnd: (item: any) => number;
    getColor: (index: number) => string;
    displayMinTime: number;
    displayMaxTime: number;
}> = ({ items, valueToPosition, isActive, onSelect, getLabel, getTitle, getStart, getEnd, getColor, displayMinTime, displayMaxTime }) => (
    <div className="relative w-full h-5">
        {items.map((item, i) => {
            const start = getStart(item);
            const end = getEnd(item);
            if (end < displayMinTime || start > displayMaxTime) {
                return null;
            }

            const visibleStart = Math.max(start, displayMinTime);
            const visibleEnd = Math.min(end, displayMaxTime);

            const leftPx = valueToPosition(visibleStart);
            const rightPx = valueToPosition(visibleEnd);
            let widthPx = Math.max(1, rightPx - leftPx);
            
            return (
                <Tooltip key={getTitle(item)} content={getTitle(item)}>
                    <div
                        onClick={(e) => { e.stopPropagation(); onSelect(item); }}
                        className={`absolute h-5 flex items-center justify-center overflow-hidden transition-all duration-150 hover:scale-y-105 origin-bottom ${isActive(item) ? 'outline outline-4 outline-offset-2 outline-black dark:outline-white' : ''}`}
                        style={{
                            left: `${leftPx}px`,
                            width: `${widthPx}px`,
                            backgroundColor: getColor(i) + '99',
                            cursor: 'pointer'
                        }}
                    >
                        {widthPx > 30 && <span className="text-xs font-semibold text-white whitespace-nowrap text-center mix-blend-luminosity pointer-events-none">{getLabel(item)}</span>}
                    </div>
                </Tooltip>
            );
        })}
    </div>
);

const DensityBar: React.FC<{
    items: LogDensityPointByLevel[] | LogDensityPoint[];
    valueToPosition: ValueToPositionFn;
    theme: Theme;
    displayMinTime: number;
    displayMaxTime: number;
    onMouseMove?: (e: React.MouseEvent<HTMLDivElement>) => void;
    onMouseLeave?: (e: React.MouseEvent<HTMLDivElement>) => void;
}> = ({ items, valueToPosition, theme, displayMinTime, displayMaxTime, onMouseMove, onMouseLeave }) => {
    if (items.length === 0) {
        return <div className="relative w-full h-5" onMouseMove={onMouseMove} onMouseLeave={onMouseLeave} />;
    }

    const isLevelDensity = 'counts' in items[0];

    if (isLevelDensity) {
        const levelItems = items as LogDensityPointByLevel[];
        const maxTotalCount = React.useMemo(() => {
            return levelItems.reduce((max, bucket) => {
                const total = Object.values(bucket.counts).reduce((sum, count) => sum + count, 0);
                return Math.max(max, total);
            }, 1); // Use 1 to avoid division by zero
        }, [levelItems]);

        const bucketDuration = levelItems.length > 1 ? levelItems[1].time - levelItems[0].time : 0;
        if (bucketDuration === 0) return <div className="relative w-full h-5" onMouseMove={onMouseMove} onMouseLeave={onMouseLeave} />;

        return (
            <div className="relative w-full h-5" onMouseMove={onMouseMove} onMouseLeave={onMouseLeave}>
                {levelItems.map((bucket, i) => {
                    const start = bucket.time;
                    const end = bucket.time + bucketDuration;
                    if (end < displayMinTime || start > displayMaxTime) return null;

                    const bucketTotal = Object.values(bucket.counts).reduce((s, c) => s + c, 0);
                    if (bucketTotal === 0) return null;
                    
                    const barHeightPercent = (bucketTotal / maxTotalCount) * 100;
                    const leftPx = valueToPosition(Math.max(start, displayMinTime));
                    const rightPx = valueToPosition(Math.min(end, displayMaxTime));
                    const widthPx = Math.max(0, rightPx - leftPx);

                    if (widthPx === 0) return null;

                    const sortedLevels = Object.entries(bucket.counts).sort((a,b) => {
                        const order: Record<string, number> = { ERROR: 0, FATAL: 1, WARNING: 2, WARN: 3, INFO: 4 };
                        return (order[a[0].toUpperCase()] ?? 99) - (order[b[0].toUpperCase()] ?? 99);
                    });

                    return (
                        <div
                            key={`density-stack-${i}`}
                            style={{ left: `${leftPx}px`, width: `${widthPx}px`, height: `${barHeightPercent}%` }}
                            className="absolute bottom-0 flex flex-col"
                        >
                            {sortedLevels.map(([level, count]) => (
                                <div
                                    key={level}
                                    style={{
                                        height: `${(count / bucketTotal) * 100}%`,
                                        backgroundColor: getLevelColor(level, theme),
                                    }}
                                />
                            ))}
                        </div>
                    );
                })}
            </div>
        );
    } else {
        const simpleItems = items as LogDensityPoint[];
        const maxCount = React.useMemo(() => {
            return simpleItems.reduce((max, bucket) => Math.max(max, bucket.count), 1);
        }, [simpleItems]);

        const bucketDuration = simpleItems.length > 1 ? simpleItems[1].time - simpleItems[0].time : 0;
        if (bucketDuration === 0) return <div className="relative w-full h-5" onMouseMove={onMouseMove} onMouseLeave={onMouseLeave} />;

        return (
            <div className="relative w-full h-5" onMouseMove={onMouseMove} onMouseLeave={onMouseLeave}>
                {simpleItems.map((bucket, i) => {
                    const start = bucket.time;
                    const end = bucket.time + bucketDuration;
                    if (end < displayMinTime || start > displayMaxTime) return null;
                    if (bucket.count === 0) return null;

                    const barHeightPercent = (bucket.count / maxCount) * 100;
                    const leftPx = valueToPosition(Math.max(start, displayMinTime));
                    const rightPx = valueToPosition(Math.min(end, displayMaxTime));
                    const widthPx = Math.max(0, rightPx - leftPx);

                    if (widthPx === 0) return null;

                    return (
                        <div
                            key={`density-bar-${i}`}
                            style={{ left: `${leftPx}px`, width: `${widthPx}px`, height: `${barHeightPercent}%`, backgroundColor: theme === 'dark' ? '#60a5fa' : '#3b82f6' }}
                            className="absolute bottom-0"
                        />
                    );
                })}
            </div>
        );
    }
};

// ... existing OverviewBrush component ...

export const TimeRangeSelector: React.FC<TimeRangeSelectorProps> = ({
    minTime, maxTime, selectedStartTime, selectedEndTime,
    onRangeChange, onClear, theme,
    pageTimestampRanges, fileTimeRanges, logDensity, overallLogDensity, datesWithLogs, viewMode, onGoToPage,
    onCursorChange, onFileSelect, onDateSelect,
    cursorTime = null, activeFileName = null, activeDate = null, currentPage = null,
    viewRange, onViewRangeChange, onZoomToSelection, onZoomToExtent, zoomToSelectionEnabled, iconSet, uiScale,
    timelineBarVisibility, onTimelineBarVisibilityChange
}) => {
    const mainContainerRef = React.useRef<HTMLDivElement>(null);
    const overviewContainerRef = React.useRef<HTMLDivElement>(null);
    const [dragState, setDragState] = React.useState<DragState | null>(null);
    const [tempSelection, setTempSelection] = React.useState<{ start: number, end: number} | null>(null);
    const [tempCursorTime, setTempCursorTime] = React.useState<number | null>(null);
    const [densityTooltip, setDensityTooltip] = React.useState<{ x: number, y: number, bucketData: BucketData } | null>(null);
    const [contextMenu, setContextMenu] = React.useState<{ x: number, y: number } | null>(null);

    const displayMinTime = viewRange?.min ?? minTime;
    const displayMaxTime = viewRange?.max ?? maxTime;

    const useResizeObserver = (ref: React.RefObject<HTMLElement>) => {
        const [width, setWidth] = React.useState(0);
        React.useEffect(() => {
            const element = ref.current;
            if (!element) {
                // When element is not present (e.g., hidden), reset width.
                setWidth(0);
                return;
            }
    
            const resizeObserver = new ResizeObserver(entries => {
                if (entries[0]) {
                    setWidth(entries[0].contentRect.width);
                }
            });
    
            resizeObserver.observe(element);
    
            // Cleanup: Disconnect observer when component unmounts or ref changes.
            return () => {
                resizeObserver.disconnect();
            };
        }, [ref.current]); // Dependency on the actual DOM element is crucial.
        return width;
    };
    
    const mainContainerWidth = useResizeObserver(mainContainerRef);
    const overviewContainerWidth = useResizeObserver(overviewContainerRef);

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
                    onRangeChange(tempSelection.start, tempSelection.end);
                }
            } else if (dragState.type === 'select_left' || dragState.type === 'select_right') {
                if (tempSelection) {
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
    }, [dragState, mainPosToValue, displayMinTime, displayMaxTime, onRangeChange, onCursorChange, tempSelection, tempCursorTime, uiScale]);

    const handleDensityHover = (e: React.MouseEvent<HTMLDivElement>) => {
        const rect = e.currentTarget.getBoundingClientRect();
        const x = e.clientX - rect.left;
        const hoverTime = mainPosToValue(x);

        if (logDensity.length < 2) {
            setDensityTooltip(null);
            return;
        }

        const bucketDuration = logDensity[1].time - logDensity[0].time;
        const bucketIndex = Math.floor((hoverTime - logDensity[0].time) / bucketDuration);
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


    const barComponents = [];
    const barProps = { displayMinTime: displayMinTime, displayMaxTime: displayMaxTime };
    if (timelineBarVisibility.pages && viewMode === 'pagination' && pageTimestampRanges.length > 0 && onGoToPage) barComponents.push({key: 'page', label: 'Pages', Comp: Bar, props: { ...barProps, items: pageTimestampRanges, isActive: (item: any) => item.page === currentPage, onSelect: (item: any) => onGoToPage(item.page), getLabel: (item: any) => item.page, getTitle: (item: any) => `Page ${item.page}`, getStart: (item: any) => new Date(item.startTime + 'Z').getTime(), getEnd: (item: any) => new Date(item.endTime + 'Z').getTime(), getColor: (i: number) => PALETTE[i % PALETTE.length] }});
    if (timelineBarVisibility.files && fileTimeRanges.length > 0) barComponents.push({key: 'file', label: 'Files', Comp: Bar, props: { ...barProps, items: fileTimeRanges, isActive: (item: any) => item.name === activeFileName, onSelect: (item: any) => onFileSelect(item.name), getLabel: (item: any) => item.name.split('/').pop(), getTitle: (item: any) => item.name, getStart: (item: any) => new Date(item.startTime + 'Z').getTime(), getEnd: (item: any) => new Date(item.endTime + 'Z').getTime(), getColor: (i: number) => PALETTE[i % PALETTE.length] }});
    if (timelineBarVisibility.dates && datesWithLogs.length > 0) barComponents.push({key: 'date', label: 'Date', Comp: Bar, props: { ...barProps, items: datesWithLogs, isActive: (item: any) => item === activeDate, onSelect: (item: any) => onDateSelect(item), getLabel: (item: any) => item, getTitle: (item: any) => item, getStart: (item: any) => new Date(`${item}T00:00:00.000Z`).getTime(), getEnd: (item: any) => new Date(`${item}T23:59:59.999Z`).getTime(), getColor: (i: number) => PALETTE[i % PALETTE.length] }});
    if (timelineBarVisibility.density && logDensity.length > 0) barComponents.push({
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

    const currentStart = tempSelection?.start ?? selectedStartTime;
    const currentEnd = tempSelection?.end ?? selectedEndTime;

    const visibleSelectionStart = currentStart !== null ? Math.max(currentStart, displayMinTime) : null;
    const visibleSelectionEnd = currentEnd !== null ? Math.min(currentEnd, displayMaxTime) : null;

    const startPos = visibleSelectionStart !== null ? mainValueToPos(visibleSelectionStart) : -1;
    const endPos = visibleSelectionEnd !== null ? mainValueToPos(visibleSelectionEnd) : -1;

     const formatTickLabel = (time: number, duration: number): string => {
        const d = new Date(time);
        if (duration <= 2 * 60 * 1000) { // < 2 minutes
            return d.toLocaleTimeString('en-US', { hour12: false, minute: '2-digit', second: '2-digit' });
        }
        if (duration <= 2 * 60 * 60 * 1000) { // < 2 hours
            return d.toLocaleTimeString('en-US', { hour12: false, hour: '2-digit', minute: '2-digit' });
        }
        if (duration <= 2 * 24 * 60 * 60 * 1000) { // < 2 days
            return `${d.toLocaleDateString('en-CA', { month: '2-digit', day: '2-digit' })} ${d.toLocaleTimeString('en-US', { hour12: false, hour: '2-digit', minute: '2-digit' })}`;
        }
        return d.toLocaleDateString('en-CA'); // YYYY-MM-DD
    };

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
                         {startPos >= 0 && <div className="absolute top-0 text-xs font-mono text-gray-700 dark:text-gray-200 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm px-1.5 py-0.5 rounded shadow" style={{ left: `${startPos}px`, transform: 'translateX(-50%)' }}>{formatTooltip(currentStart)}</div>}
                         {endPos >= 0 && (endPos - startPos > 60) && <div className="absolute top-0 text-xs font-mono text-gray-700 dark:text-gray-200 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm px-1.5 py-0.5 rounded shadow" style={{ left: `${endPos}px`, transform: 'translateX(-50%)' }}>{formatTooltip(currentEnd)}</div>}
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
                             {startPos >= 0 && endPos >= 0 && (endPos > startPos) && (
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
                        <button onClick={onZoomToSelection} disabled={!zoomToSelectionEnabled} className="p-1.5 text-gray-500 dark:text-gray-400 rounded-full hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors disabled:opacity-50 disabled:cursor-not-allowed"><Icon name="ArrowsPointingIn" iconSet={iconSet} className="w-4 h-4"/></button>
                     </Tooltip>
                     <Tooltip content="Zoom to Extent">
                        <button onClick={onZoomToExtent} disabled={!viewRange} className="p-1.5 text-gray-500 dark:text-gray-400 rounded-full hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors disabled:opacity-50 disabled:cursor-not-allowed"><Icon name="ArrowsPointingOut" iconSet={iconSet} className="w-4 h-4"/></button>
                     </Tooltip>
                     <Tooltip content="Clear time selection">
                        <button onClick={onClear} className="p-1.5 text-gray-500 dark:text-gray-400 rounded-full hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors"><Icon name="XMark" iconSet={iconSet} className="w-4 h-4"/></button>
                    </Tooltip>
                </div>
            </div>
            {timelineBarVisibility.overview && (
                 <div className="flex items-start gap-3 w-full mt-2">
                    <div className="w-16 flex-shrink-0"></div>
                    <div className="flex-grow" ref={overviewContainerRef}>
                        {overviewContainerWidth > 0 && (
                            <OverviewBrush
                                minTime={minTime} maxTime={maxTime}
                                viewMin={displayMinTime} viewMax={displayMaxTime}
                                selectedMin={selectedStartTime}
                                selectedMax={selectedEndTime}
                                density={overallLogDensity} theme={theme}
                                valueToPosition={overviewValueToPos} positionToValue={overviewPosToValue}
                                onViewRangeChange={onViewRangeChange}
                                onRangeChange={onRangeChange}
                                uiScale={uiScale}
                            />
                        )}
                    </div>
                    <div className="w-[60px] flex-shrink-0"/>
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
                    viewMode={viewMode}
                />
            )}
        </div>
    );
};

const DensityTooltip: React.FC<{ x: number, y: number, bucketData: BucketData, theme: Theme }> = ({ x, y, bucketData, theme }) => {
    const ref = React.useRef<HTMLDivElement>(null);
    const [style, setStyle] = React.useState<React.CSSProperties>({
        position: 'fixed',
        top: y,
        left: x,
        transform: 'translate(-50%, -110%)',
    });

    React.useLayoutEffect(() => {
        if (ref.current) {
            const rect = ref.current.getBoundingClientRect();
            let newX = x;
            let newY = y;
            let transform = 'translate(-50%, -110%)';

            if (rect.right > window.innerWidth) {
                newX = window.innerWidth - rect.width / 2 - 10;
            }
            if (rect.left < 0) {
                newX = rect.width / 2 + 10;
            }
            if (rect.top < 0) {
                newY = y + rect.height + 20; // Flip below
                transform = 'translate(-50%, 20px)';
            }
            
            setStyle({
                position: 'fixed',
                top: newY,
                left: newX,
                transform: transform,
            });
        }
    }, [x, y]);

    const isLevelData = 'counts' in bucketData;
    const total = isLevelData ? Object.values(bucketData.counts).reduce((sum, count) => sum + count, 0) : bucketData.count;
    const sortedLevels = isLevelData ? Object.entries(bucketData.counts).sort((a,b) => b[1] - a[1]) : [];
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


// Note: This is a placeholder for the OverviewBrush to satisfy type-checking since it's a large component.
// The new implementation for overview density is below.
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
            return;
        }

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
        } else { // brush_right
            newMax += deltaTime;
            if (newMax > maxTime) newMax = maxTime;
            if (newMax <= newMin) newMax = newMin + 1;
        }

        if (newMin < newMax) {
            onViewRangeChange({ min: newMin, max: newMax });
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
            <div className="w-full h-full flex pointer-events-none">
                {density.map((bucket: LogDensityPointByLevel | LogDensityPoint, i) => {
                    const total: number = 'counts' in bucket ? Object.values(bucket.counts).reduce((s: number, c: number) => s + c, 0) : (bucket as LogDensityPoint).count;
                    const opacity = total > 0 ? 0.3 + (total / maxTotalCount) * 0.7 : 0;
                     if ('counts' in bucket && total > 0) {
                        return (
                             <div key={i} style={{ flex: '1 1 0%'}} className="flex flex-col">
                                 {Object.entries(bucket.counts).map(([level, count]: [string, number]) => (
                                    <div key={level} style={{ height: `${(count/total)*100}%`, backgroundColor: getLevelColor(level, theme) }}/>
                                 ))}
                             </div>
                        )
                    }
                    return (
                        <div key={`density-${i}`} style={{ flex: '1 1 0%', backgroundColor: `rgba(147, 197, 253, ${opacity})`}} />
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