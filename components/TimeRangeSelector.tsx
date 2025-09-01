import React from 'react';
import { XMarkIcon } from './icons/XMarkIcon';
import { PageTimestampRange, FileTimeRange, LogDensityPoint, ViewMode, OverallTimeRange } from '../types';
import { ArrowsPointingInIcon } from './icons/ArrowsPointingInIcon';
import { ArrowsPointingOutIcon } from './icons/ArrowsPointingOutIcon';

type Theme = 'light' | 'dark';
type ValueToPositionFn = (value: number) => number;

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
    logDensity: LogDensityPoint[];
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
    onZoomToSelection: () => void;
    onZoomToExtent: () => void;
    zoomToSelectionEnabled: boolean;
}

type DragState =
  | { type: 'left' | 'right'; startX: number; initialStart: number; initialEnd: number }
  | { type: 'cursor'; startX: number; initialCursor: number };

const BAR_HEIGHT = 20; // px
const PALETTE = [
  '#3b82f6', '#ef4444', '#10b981', '#f59e0b', '#8b5cf6', '#ec4899', 
  '#6366f1', '#06b6d4', '#d946ef', '#f43f5e', '#14b8a6', '#eab308'
];

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


const TimeAxis = React.memo(({ minTime, maxTime, valueToPosition }: { minTime: number, maxTime: number, valueToPosition: ValueToPositionFn }) => {
    const ticks = React.useMemo(() => {
        if (maxTime <= minTime) return [];

        const duration = maxTime - minTime;
        const ONE_SECOND = 1000;
        const ONE_MINUTE = 60 * ONE_SECOND;
        const ONE_HOUR = 60 * ONE_MINUTE;
        const ONE_DAY = 24 * ONE_HOUR;
        
        const checkSpansMultipleDays = (min: number, max: number): boolean => {
            const d1 = new Date(min);
            const d2 = new Date(max);
            return d1.getUTCFullYear() !== d2.getUTCFullYear() ||
                   d1.getUTCMonth() !== d2.getUTCMonth() ||
                   d1.getUTCDate() !== d2.getUTCDate();
        };
        const spansMultipleDays = checkSpansMultipleDays(minTime, maxTime);

        const formatters = {
            timeWithSecs: (d: Date) => d.toLocaleTimeString('en-GB', { timeZone: 'UTC', hour: '2-digit', minute: '2-digit', second: '2-digit' }),
            time: (d: Date) => d.toLocaleTimeString('en-GB', { timeZone: 'UTC', hour: '2-digit', minute: '2-digit' }),
            dateTime: (d: Date) => `${d.toLocaleDateString('en-CA', { timeZone: 'UTC' })} ${d.toLocaleTimeString('en-GB', { timeZone: 'UTC', hour: '2-digit', minute: '2-digit' })}`,
            monthDay: (d: Date) => d.toLocaleDateString('en-US', { timeZone: 'UTC', month: 'short', day: 'numeric' }),
            date: (d: Date) => d.toLocaleDateString('en-CA', { timeZone: 'UTC' }),
        };

        let majorInterval: number;
        let minorInterval: number;
        let majorLabelFormatter: (d: Date) => string;
        let getMinorLabel: ((d: Date) => string) | null = null;
        
        if (duration <= ONE_MINUTE * 2) {
            majorInterval = 15 * ONE_SECOND;
            minorInterval = 5 * ONE_SECOND;
            majorLabelFormatter = formatters.timeWithSecs;
            getMinorLabel = (d: Date) => String(d.getUTCSeconds()).padStart(2, '0');
        } else if (duration <= ONE_MINUTE * 10) {
            majorInterval = ONE_MINUTE;
            minorInterval = 15 * ONE_SECOND;
            majorLabelFormatter = formatters.timeWithSecs;
            getMinorLabel = (d: Date) => String(d.getUTCSeconds()).padStart(2, '0');
        } else if (duration <= ONE_HOUR) {
            majorInterval = 10 * ONE_MINUTE;
            minorInterval = ONE_MINUTE;
            majorLabelFormatter = formatters.time;
            getMinorLabel = (d: Date) => String(d.getUTCMinutes()).padStart(2, '0');
        } else if (duration <= ONE_DAY * 3) {
            if (duration <= ONE_HOUR * 6) {
                majorInterval = ONE_HOUR;
                minorInterval = 15 * ONE_MINUTE;
                getMinorLabel = (d: Date) => String(d.getUTCMinutes()).padStart(2, '0');
            } else {
                majorInterval = 6 * ONE_HOUR;
                minorInterval = ONE_HOUR;
                getMinorLabel = (d: Date) => String(d.getUTCHours());
            }
            majorLabelFormatter = spansMultipleDays ? formatters.dateTime : formatters.time;
        } else if (duration <= ONE_DAY * 10) {
            majorInterval = ONE_DAY;
            minorInterval = 6 * ONE_HOUR;
            majorLabelFormatter = formatters.monthDay;
            getMinorLabel = null;
        } else {
            majorInterval = 7 * ONE_DAY;
            minorInterval = ONE_DAY;
            majorLabelFormatter = formatters.date;
            getMinorLabel = null;
        }
        
        const generatedTicks: { time: number; majorLabel: string; minorLabel: string; isMajor: boolean }[] = [];
        const addedTimes = new Set<number>();
        
        const addTick = (time: number, isMajor: boolean) => {
            const roundedTime = Math.round(time);
            if (roundedTime >= minTime && roundedTime < maxTime && !addedTimes.has(roundedTime)) {
                const date = new Date(roundedTime);
                const majorLabel = isMajor ? majorLabelFormatter(date) : '';
                const minorLabel = !isMajor && getMinorLabel ? getMinorLabel(date) : '';
                generatedTicks.push({ time: roundedTime, majorLabel, minorLabel, isMajor });
                addedTimes.add(roundedTime);
            }
        };

        let tickTime = Math.ceil(minTime / majorInterval) * majorInterval;
        while (tickTime < maxTime) {
            addTick(tickTime, true);
            tickTime += majorInterval;
        }
        tickTime = Math.ceil(minTime / minorInterval) * minorInterval;
        while (tickTime < maxTime) {
            addTick(tickTime, false);
            tickTime += minorInterval;
        }

        return generatedTicks.sort((a,b) => a.time - b.time);
    }, [minTime, maxTime]);

    return (
        <div className="relative h-8 w-full mt-1">
            {ticks.map(({ time, majorLabel, minorLabel, isMajor }) => (
                <div
                    key={time}
                    className="absolute top-0 h-full"
                    style={{ left: `${valueToPosition(time)}px` }}
                >
                    <div className={`w-px ${isMajor ? 'h-3 bg-gray-500 dark:bg-gray-300' : 'h-1.5 bg-gray-400 dark:bg-gray-500'}`} />
                    {majorLabel && <span className="absolute -translate-x-1/2 top-3 mt-1 text-xs text-gray-500 dark:text-gray-400 font-semibold whitespace-nowrap">{majorLabel}</span>}
                    {minorLabel && (
                         <span className="absolute top-[2px] -translate-x-1/2 text-[10px] text-gray-400 dark:text-gray-500">
                            {minorLabel}
                        </span>
                    )}
                </div>
            ))}
        </div>
    );
});


const PageBar: React.FC<{
    pageRanges: PageTimestampRange[],
    valueToPosition: ValueToPositionFn,
    onPageSelect: (page: number) => void,
    currentPage: number | null,
}> = React.memo(({ pageRanges, valueToPosition, onPageSelect, currentPage }) => {
    return (
        <React.Fragment>
            {pageRanges.map((range) => {
                if (!range.startTime || !range.endTime) return null;
                const start = new Date(range.startTime + 'Z').getTime();
                const end = new Date(range.endTime + 'Z').getTime();
                const leftPx = valueToPosition(start);
                const rightPx = valueToPosition(end);
                const widthPx = rightPx - leftPx;
                
                const showText = widthPx > 18;
                const isActive = range.page === currentPage;

                return (
                    <div
                        key={`page-${range.page}`}
                        onClick={(e) => { e.stopPropagation(); onPageSelect(range.page); }}
                        className={`absolute flex items-center justify-center overflow-hidden transition-all duration-150 hover:scale-y-105 origin-bottom ${isActive ? 'outline outline-4 outline-offset-2 outline-black dark:outline-white' : ''}`}
                        style={{
                            left: `${leftPx}px`,
                            width: `${widthPx}px`,
                            height: `${BAR_HEIGHT}px`,
                            backgroundColor: PALETTE[range.page % PALETTE.length] + '99',
                            cursor: 'pointer'
                        }}
                        title={`Page ${range.page}`}
                    >
                         {showText && <span className="text-xs font-semibold text-white mix-blend-luminosity pointer-events-none">{range.page}</span>}
                    </div>
                );
            })}
        </React.Fragment>
    );
});

const FileBar: React.FC<{
    fileRanges: FileTimeRange[],
    valueToPosition: ValueToPositionFn,
    onFileSelect: (fileName: string) => void;
    activeFileName: string | null,
}> = React.memo(({ fileRanges, valueToPosition, onFileSelect, activeFileName }) => {
    return (
        <React.Fragment>
            {fileRanges.map((range, i) => {
                const start = new Date(range.startTime + 'Z').getTime();
                const end = new Date(range.endTime + 'Z').getTime();
                const leftPx = valueToPosition(start);
                const rightPx = valueToPosition(end);
                let widthPx = rightPx - leftPx;

                if (widthPx < 1 && widthPx > 0) {
                    widthPx = 1;
                }
                
                const showText = widthPx > 30;
                const isActive = range.name === activeFileName;

                return (
                    <div
                        key={`file-${range.name}`}
                        onClick={(e) => { e.stopPropagation(); onFileSelect(range.name); }}
                        className={`absolute flex items-center justify-center overflow-hidden transition-all duration-150 hover:scale-y-105 origin-bottom ${isActive ? 'outline outline-4 outline-offset-2 outline-black dark:outline-white' : ''}`}
                        style={{
                            left: `${leftPx}px`,
                            width: `${widthPx}px`,
                            height: `${BAR_HEIGHT}px`,
                            backgroundColor: PALETTE[i % PALETTE.length] + '99',
                            cursor: 'pointer'
                        }}
                        title={range.name}
                    >
                         {showText && <span className="text-xs font-semibold text-white whitespace-nowrap text-center mix-blend-luminosity pointer-events-none">{range.name.split('/').pop()}</span>}
                    </div>
                );
            })}
        </React.Fragment>
    );
});

const DateBar: React.FC<{
    datesWithLogs: string[],
    valueToPosition: ValueToPositionFn,
    onDateSelect: (date: string) => void;
    activeDate: string | null,
}> = React.memo(({ datesWithLogs, valueToPosition, onDateSelect, activeDate }) => {
    const datesToRender = React.useMemo(() => {
        return datesWithLogs.map(dateStr => {
            const start = new Date(`${dateStr}T00:00:00.000Z`).getTime();
            const end = new Date(`${dateStr}T23:59:59.999Z`).getTime();
            return { dateStr, start, end };
        });
    }, [datesWithLogs]);

    return (
         <React.Fragment>
            {datesToRender.map(({ dateStr, start, end }, i) => {
                const leftPx = valueToPosition(start);
                const rightPx = valueToPosition(end);
                const widthPx = Math.max(1, rightPx - leftPx); // Ensure at least 1px for visibility
                const isActive = dateStr === activeDate;

                return (
                    <div
                        key={dateStr}
                        onClick={(e) => { e.stopPropagation(); onDateSelect(dateStr); }}
                        className={`absolute flex items-center justify-center overflow-hidden transition-all duration-150 hover:scale-y-105 origin-bottom ${isActive ? 'outline outline-4 outline-offset-2 outline-black dark:outline-white' : ''}`}
                        style={{
                            left: `${leftPx}px`,
                            width: `${widthPx}px`,
                            height: `${BAR_HEIGHT}px`,
                            backgroundColor: PALETTE[i % PALETTE.length] + '80',
                            cursor: 'pointer'
                        }}
                        title={dateStr}
                    >
                        {widthPx > 50 && <span className="text-xs font-semibold text-white mix-blend-luminosity pointer-events-none">{dateStr}</span>}
                    </div>
                );
            })}
        </React.Fragment>
    );
});


const getHeatmapColor = (densityValue: number, theme: Theme): string => {
    if (densityValue <= 0) return 'transparent';
    let hue: number;
    if (densityValue < 50) {
        hue = 120 - (densityValue / 50) * 60;
    } else {
        hue = 60 - ((densityValue - 50) / 50) * 60;
    }
    const saturation = theme === 'dark' ? 80 : 90;
    const lightness = theme === 'dark' ? 50 : 55;
    const alpha = 0.85;
    return `hsla(${hue}, ${saturation}%, ${lightness}%, ${alpha})`;
};

const DensityHeatmapBar: React.FC<{
    density: LogDensityPoint[],
    theme: Theme,
}> = React.memo(({ density, theme }) => {
    if (density.length === 0) return null;

    return (
         <div className="w-full h-full flex" style={{ height: `${BAR_HEIGHT}px`}}>
            {density.map((bucket, i) => (
                <div
                    key={`density-${i}`}
                    style={{
                        flex: '1 1 0%',
                        backgroundColor: getHeatmapColor(bucket.count, theme)
                    }}
                />
            ))}
        </div>
    );
});


export const TimeRangeSelector: React.FC<TimeRangeSelectorProps> = ({
    minTime, maxTime, selectedStartTime, selectedEndTime,
    onRangeChange, onClear, theme,
    pageTimestampRanges, fileTimeRanges, logDensity, datesWithLogs, viewMode, onGoToPage,
    onCursorChange, onFileSelect, onDateSelect,
    cursorTime = null, activeFileName = null, activeDate = null, currentPage = null,
    viewRange, onZoomToSelection, onZoomToExtent, zoomToSelectionEnabled
}) => {
    const contentContainerRef = React.useRef<HTMLDivElement>(null);
    const [containerWidth, setContainerWidth] = React.useState(0);
    const [dragState, setDragState] = React.useState<DragState | null>(null);
    const [localSelection, setLocalSelection] = React.useState({ start: selectedStartTime, end: selectedEndTime });
    const [localCursorTime, setLocalCursorTime] = React.useState<number | null>(null);
    const [densityHoverIndex, setDensityHoverIndex] = React.useState<number | null>(null);
    
    const displayMinTime = viewRange?.min ?? minTime;
    const displayMaxTime = viewRange?.max ?? maxTime;

    React.useEffect(() => {
        setLocalSelection({ start: selectedStartTime, end: selectedEndTime });
    }, [selectedStartTime, selectedEndTime]);

    React.useEffect(() => {
        const el = contentContainerRef.current;
        if (!el) return;
        const resizeObserver = new ResizeObserver(entries => {
            if (entries[0]) {
                setContainerWidth(entries[0].contentRect.width);
            }
        });
        resizeObserver.observe(el);
        setContainerWidth(el.clientWidth);
        return () => resizeObserver.disconnect();
    }, []);

    const valueToPosition = React.useCallback((value: number) => {
        if (displayMaxTime === displayMinTime || !containerWidth) return 0;
        const position = ((value - displayMinTime) / (displayMaxTime - displayMinTime)) * containerWidth;
        return Math.max(0, Math.min(position, containerWidth));
    }, [displayMinTime, displayMaxTime, containerWidth]);

    const positionToValue = React.useCallback((pos: number) => {
        if (!containerWidth) return displayMinTime;
        const value = displayMinTime + (pos / containerWidth) * (displayMaxTime - displayMinTime);
        return Math.max(displayMinTime, Math.min(value, displayMaxTime));
    }, [displayMinTime, displayMaxTime, containerWidth]);

    const handleMouseDown = (e: React.MouseEvent<HTMLDivElement>, type: 'left' | 'right') => {
        e.preventDefault();
        e.stopPropagation();
        setDragState({
            type,
            startX: e.clientX,
            initialStart: localSelection.start ?? displayMinTime,
            initialEnd: localSelection.end ?? displayMaxTime,
        });
    };
    
    const handleCursorMouseDown = (e: React.MouseEvent<HTMLDivElement>) => {
        e.preventDefault();
        e.stopPropagation();
        const initialTime = displayCursorTime;
        if (initialTime === null) return;
        setDragState({
            type: 'cursor',
            startX: e.clientX,
            initialCursor: initialTime,
        });
        setLocalCursorTime(initialTime);
    };

    const handleContainerMouseDown = (e: React.MouseEvent<HTMLDivElement>) => {
        if ((e.target as HTMLElement).closest('[data-handle]')) {
            return;
        }

        if (!contentContainerRef.current) return;
        const rect = contentContainerRef.current.getBoundingClientRect();
        const clickPos = e.clientX - rect.left;
        const timeAtClick = positionToValue(clickPos);
        onCursorChange(timeAtClick);
    };

    const handleMouseMove = React.useCallback((e: MouseEvent) => {
        if (!dragState || !contentContainerRef.current) return;
        e.preventDefault();

        const deltaX = e.clientX - dragState.startX;

        if (dragState.type === 'cursor') {
            const newPos = valueToPosition(dragState.initialCursor) + deltaX;
            const newTime = positionToValue(newPos);
            setLocalCursorTime(newTime);
            return;
        }

        let newStart = localSelection.start;
        let newEnd = localSelection.end;

        if (dragState.type === 'left') {
            const newPos = valueToPosition(dragState.initialStart) + deltaX;
            newStart = positionToValue(newPos);
            if (newEnd !== null && newStart > newEnd) newStart = newEnd;
        } else if (dragState.type === 'right') {
            const newPos = valueToPosition(dragState.initialEnd) + deltaX;
            newEnd = positionToValue(newPos);
            if (newStart !== null && newEnd < newStart) newEnd = newStart;
        }

        setLocalSelection({ start: newStart, end: newEnd });

    }, [dragState, positionToValue, valueToPosition, localSelection]);

    const handleMouseUp = React.useCallback(() => {
        if (dragState) {
            if (dragState.type === 'cursor') {
                if (localCursorTime !== null) {
                    onCursorChange(localCursorTime);
                }
            } else if (localSelection.start !== null && localSelection.end !== null && localSelection.start < localSelection.end) {
                onRangeChange(localSelection.start, localSelection.end);
            }
        }
        setDragState(null);
        setLocalCursorTime(null);
    }, [dragState, localSelection, onRangeChange, localCursorTime, onCursorChange]);
    
    const handleDensityMouseMove = (e: React.MouseEvent<HTMLDivElement>) => {
        if (logDensity.length === 0) return;
        const rect = e.currentTarget.getBoundingClientRect();
        const x = e.clientX - rect.left;
        const index = Math.floor((x / rect.width) * logDensity.length);

        if (index >= 0 && index < logDensity.length) {
            setDensityHoverIndex(index);
        }
    };
    
    const handleDensityMouseLeave = () => {
        setDensityHoverIndex(null);
    };


    React.useEffect(() => {
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
    }, [dragState, handleMouseMove, handleMouseUp]);

    const displayCursorTime = localCursorTime ?? cursorTime;
    const startPos = localSelection.start !== null ? valueToPosition(localSelection.start) : -1;
    const endPos = localSelection.end !== null ? valueToPosition(localSelection.end) : -1;

    const barComponents = [];
    if (viewMode === 'pagination' && pageTimestampRanges.length > 0 && onGoToPage) barComponents.push({key: 'page', label: 'Pages', Comp: PageBar, props: { pageRanges: pageTimestampRanges, valueToPosition, onPageSelect: onGoToPage, currentPage }});
    if (fileTimeRanges.length > 0) barComponents.push({key: 'file', label: 'Files', Comp: FileBar, props: { fileRanges: fileTimeRanges, valueToPosition, onFileSelect, activeFileName }});
    if (datesWithLogs.length > 0) barComponents.push({key: 'date', label: 'Date', Comp: DateBar, props: { datesWithLogs, valueToPosition, onDateSelect, activeDate }});
    if (logDensity.length > 0) barComponents.push({key: 'density', label: 'Density', Comp: DensityHeatmapBar, props: { density: logDensity, theme }});

    return (
        <div className="flex items-start gap-3 w-full">
            <div className="w-16 flex-shrink-0 text-right pt-6">
                <div className="space-y-3">
                    {barComponents.map(bar => (
                        <div key={bar.key} style={{ height: `${BAR_HEIGHT}px`}} className="text-xs font-semibold text-gray-600 dark:text-gray-400 flex items-center justify-end pr-2">
                            {bar.label}
                        </div>
                    ))}
                    <div className="h-8" /> {/* Spacer for axis */}
                </div>
            </div>
            
            <div className="flex-grow flex flex-col relative pt-6"> {/* pt-6 for tooltips */}
                <div 
                    onMouseDown={handleContainerMouseDown}
                    className="w-full cursor-crosshair bg-gray-200 dark:bg-gray-700/50 rounded p-1"
                >
                    <div ref={contentContainerRef} className="relative">
                        <div className="space-y-3">
                            {barComponents.map(bar => (
                                <div 
                                    key={bar.key}
                                    className="relative w-full" 
                                    style={{ height: `${BAR_HEIGHT}px`}}
                                    onMouseMove={bar.key === 'density' ? handleDensityMouseMove : undefined}
                                    onMouseLeave={bar.key === 'density' ? handleDensityMouseLeave : undefined}
                                >
                                    <bar.Comp {...bar.props} />
                                </div>
                            ))}
                        </div>
                        
                        {startPos >= 0 && endPos >= 0 && (
                            <div 
                                className="absolute top-0 bottom-0 bg-sky-500/20 dark:bg-sky-400/20 z-10"
                                style={{ left: `${startPos}px`, width: `${endPos - startPos}px`, pointerEvents: 'none' }}
                            />
                        )}

                        {startPos >= 0 && (
                             <div
                                data-handle="left"
                                onMouseDown={(e) => handleMouseDown(e, 'left')}
                                className="absolute top-0 h-full w-1 bg-sky-600 dark:bg-sky-500 cursor-col-resize z-20 group"
                                style={{ left: `${startPos}px`, transform: 'translateX(-50%)', pointerEvents: 'auto' }}
                             >
                                <div className="absolute top-1/2 -translate-y-1/2 -left-full -translate-x-0.5 w-1.5 h-10 bg-sky-600 dark:bg-sky-500 rounded-full opacity-0 group-hover:opacity-100 transition-opacity" />
                             </div>
                        )}
                        {endPos >= 0 && (
                             <div
                                data-handle="right"
                                onMouseDown={(e) => handleMouseDown(e, 'right')}
                                className="absolute top-0 h-full w-1 bg-sky-600 dark:bg-sky-500 cursor-col-resize z-20 group"
                                style={{ left: `${endPos}px`, transform: 'translateX(-50%)', pointerEvents: 'auto' }}
                            >
                                <div className="absolute top-1/2 -translate-y-1/2 -right-full translate-x-0.5 w-1.5 h-10 bg-sky-600 dark:bg-sky-500 rounded-full opacity-0 group-hover:opacity-100 transition-opacity" />
                            </div>
                        )}
                        
                        {displayCursorTime !== null && containerWidth > 0 && (
                             <div
                                onMouseDown={handleCursorMouseDown}
                                onDoubleClick={() => { if (displayCursorTime !== null) onCursorChange(displayCursorTime); }}
                                className="absolute top-0 bottom-0 w-px bg-red-500 z-30 cursor-col-resize group"
                                style={{
                                    left: `${valueToPosition(displayCursorTime)}px`,
                                    pointerEvents: 'auto',
                                    paddingLeft: '2px',
                                    paddingRight: '2px',
                                    marginLeft: '-2px'
                                }}
                            >
                                <div className="absolute top-1/2 -translate-y-1/2 w-2 h-2 bg-red-500 rounded-full -left-1/2 -translate-x-px opacity-0 group-hover:opacity-100 transition-opacity" />
                            </div>
                        )}

                        {densityHoverIndex !== null && logDensity[densityHoverIndex] && (
                            <div
                                className="absolute text-xs font-mono text-gray-700 dark:text-gray-200 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm px-1.5 py-0.5 rounded shadow z-40 pointer-events-none"
                                style={{
                                    left: `${(densityHoverIndex / logDensity.length) * 100}%`,
                                    top: `${barComponents.findIndex(b => b.key === 'density') * (BAR_HEIGHT + 12)}px`, // 12px for space-y-3
                                    transform: 'translateY(calc(-100% - 4px))'
                                }}
                            >
                                {formatTooltip(logDensity[densityHoverIndex].time)}
                            </div>
                        )}
                    </div>
                </div>
                <TimeAxis minTime={displayMinTime} maxTime={displayMaxTime} valueToPosition={valueToPosition} />
                
                {localSelection.start !== null && startPos >= 0 && containerWidth > 0 && (
                    <div
                        className="absolute top-0 text-xs font-mono text-gray-700 dark:text-gray-200 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm px-1.5 py-0.5 rounded shadow z-40"
                        style={{
                            left: `${startPos}px`,
                            transform: 'translateX(-50%)',
                            pointerEvents: 'none',
                        }}
                    >
                        {formatTooltip(localSelection.start)}
                    </div>
                )}
                {localSelection.end !== null && endPos >= 0 && containerWidth > 0 && (localSelection.end - (localSelection.start ?? 0) > 1000) && (
                    <div
                        className="absolute top-0 text-xs font-mono text-gray-700 dark:text-gray-200 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm px-1.5 py-0.5 rounded shadow z-40"
                        style={{
                            left: `${endPos}px`,
                            transform: 'translateX(-50%)',
                            pointerEvents: 'none',
                        }}
                    >
                        {formatTooltip(localSelection.end)}
                    </div>
                )}

                 {displayCursorTime !== null && containerWidth > 0 && (
                    <div
                        className="absolute top-0 text-xs font-mono bg-red-500/90 backdrop-blur-sm text-white rounded px-1.5 py-0.5 z-40"
                        style={{
                            left: `${valueToPosition(displayCursorTime)}px`,
                            transform: 'translateX(-50%)',
                            pointerEvents: 'none',
                        }}
                    >
                        {formatTooltip(displayCursorTime)}
                    </div>
                 )}

            </div>
            
            <div className="w-auto flex-shrink-0 self-start pt-6 flex flex-col items-center gap-1">
                 <button
                    onClick={onZoomToSelection}
                    disabled={!zoomToSelectionEnabled}
                    className="p-2 text-gray-500 dark:text-gray-400 rounded-full hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
                    title="Zoom to Selection"
                >
                    <ArrowsPointingInIcon className="w-5 h-5"/>
                </button>
                 <button
                    onClick={onZoomToExtent}
                    disabled={!viewRange}
                    className="p-2 text-gray-500 dark:text-gray-400 rounded-full hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
                    title="Zoom to Extent"
                >
                    <ArrowsPointingOutIcon className="w-5 h-5"/>
                </button>
                 <button
                    onClick={onClear}
                    className="p-2 text-gray-500 dark:text-gray-400 rounded-full hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors"
                    title="Clear time selection"
                >
                    <XMarkIcon className="w-6 h-6"/>
                </button>
            </div>
        </div>
    );
};