import React from 'react';
import { PageTimestampRange, FileTimeRange, LogDensityPoint, ViewMode, OverallTimeRange, IconSet } from '../types.ts';
import { Icon } from './icons/index.tsx';

type Theme = 'light' | 'dark';
type ValueToPositionFn = (value: number) => number;
type PositionToValueFn = (pos: number) => number;

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
    overallLogDensity: LogDensityPoint[];
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
}

type DragState =
  | { type: 'select_left'; startX: number; initialStart: number; initialEnd: number }
  | { type: 'select_right'; startX: number; initialStart: number; initialEnd: number }
  | { type: 'cursor'; startX: number; initialCursor: number }
  | { type: 'new_selection'; startX: number; startTime: number };

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
                <div
                    key={getTitle(item)}
                    onClick={(e) => { e.stopPropagation(); onSelect(item); }}
                    className={`absolute h-5 flex items-center justify-center overflow-hidden transition-all duration-150 hover:scale-y-105 origin-bottom ${isActive(item) ? 'outline outline-4 outline-offset-2 outline-black dark:outline-white' : ''}`}
                    style={{
                        left: `${leftPx}px`,
                        width: `${widthPx}px`,
                        backgroundColor: getColor(i) + '99',
                        cursor: 'pointer'
                    }}
                    title={getTitle(item)}
                >
                    {widthPx > 30 && <span className="text-xs font-semibold text-white whitespace-nowrap text-center mix-blend-luminosity pointer-events-none">{getLabel(item)}</span>}
                </div>
            );
        })}
    </div>
);

const DensityBar: React.FC<{
    items: LogDensityPoint[];
    valueToPosition: ValueToPositionFn;
    theme: Theme;
    displayMinTime: number;
    displayMaxTime: number;
}> = ({ items, valueToPosition, theme, displayMinTime, displayMaxTime }) => {
    if (items.length < 2) return <div className="relative w-full h-5" />;
    const bucketDuration = items[1].time - items[0].time;
    
    return (
        <div className="relative w-full h-5">
            {items.map((bucket, i) => {
                const start = bucket.time;
                const end = bucket.time + bucketDuration;
                
                if (end < displayMinTime || start > displayMaxTime) {
                    return null;
                }

                const visibleStart = Math.max(start, displayMinTime);
                const visibleEnd = Math.min(end, displayMaxTime);

                const leftPx = valueToPosition(visibleStart);
                const rightPx = valueToPosition(visibleEnd);
                const widthPx = Math.max(0, rightPx - leftPx);

                if (widthPx === 0) return null;
                
                return (
                    <div
                        key={`density-bucket-${i}`}
                        className="absolute top-0 bottom-0"
                        style={{
                            left: `${leftPx}px`,
                            width: `${widthPx}px`,
                            backgroundColor: getHeatmapColor(bucket.count, theme),
                        }}
                    />
                );
            })}
        </div>
    );
};

const OverviewBrush: React.FC<{
    minTime: number;
    maxTime: number;
    viewMin: number;
    viewMax: number;
    selectedMin: number | null;
    selectedMax: number | null;
    density: LogDensityPoint[];
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

    return (
        <div
            ref={containerRef}
            onMouseDown={handleContainerMouseDown}
            className="relative w-full h-10 bg-gray-200 dark:bg-gray-700/50 rounded p-0.5 cursor-crosshair"
        >
            <div className="w-full h-full flex pointer-events-none">
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


export const TimeRangeSelector: React.FC<TimeRangeSelectorProps> = ({
    minTime, maxTime, selectedStartTime, selectedEndTime,
    onRangeChange, onClear, theme,
    pageTimestampRanges, fileTimeRanges, logDensity, overallLogDensity, datesWithLogs, viewMode, onGoToPage,
    onCursorChange, onFileSelect, onDateSelect,
    cursorTime = null, activeFileName = null, activeDate = null, currentPage = null,
    viewRange, onViewRangeChange, onZoomToSelection, onZoomToExtent, zoomToSelectionEnabled, iconSet, uiScale
}) => {
    const mainContainerRef = React.useRef<HTMLDivElement>(null);
    const overviewContainerRef = React.useRef<HTMLDivElement>(null);
    const [dragState, setDragState] = React.useState<DragState | null>(null);
    const [tempSelection, setTempSelection] = React.useState<{ start: number, end: number} | null>(null);
    
    const displayMinTime = viewRange?.min ?? minTime;
    const displayMaxTime = viewRange?.max ?? maxTime;

    const useResizeObserver = (ref: React.RefObject<HTMLElement>) => {
        const [width, setWidth] = React.useState(0);
        React.useEffect(() => {
            const el = ref.current;
            if (!el) return;
            const resizeObserver = new ResizeObserver(entries => {
                if (entries[0]) setWidth(entries[0].contentRect.width);
            });
            resizeObserver.observe(el);
            setWidth(el.clientWidth);
            return () => resizeObserver.disconnect();
        }, [ref]);
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


    const handleMouseDown = (
        e: React.MouseEvent,
        type: 'select_left' | 'select_right' | 'cursor' | 'new_selection'
    ) => {
        e.stopPropagation();
        e.preventDefault();
        
        const rect = mainContainerRef.current!.getBoundingClientRect();
        const clickPos = (e.clientX - rect.left) / uiScale;
        const clickTime = mainPosToValue(clickPos);

        setDragState({
            type,
            startX: e.clientX,
            startTime: clickTime,
            initialStart: selectedStartTime || displayMinTime,
            initialEnd: selectedEndTime || displayMaxTime,
            initialCursor: cursorTime || clickTime,
        });
    };
    
    React.useEffect(() => {
        const handleMouseMove = (e: MouseEvent) => {
            if (!dragState || !mainContainerRef.current) return;
            e.preventDefault();
            const rect = mainContainerRef.current.getBoundingClientRect();
            
            const deltaX = e.clientX - dragState.startX;
            const unscaledDeltaX = deltaX / uiScale;
            const deltaTime = mainPosToValue(unscaledDeltaX) - mainPosToValue(0);

            switch (dragState.type) {
                case 'new_selection': {
                    const currentUnscaledPos = (e.clientX - rect.left) / uiScale;
                    const currentTime = mainPosToValue(currentUnscaledPos);
                    const start = Math.min(dragState.startTime, currentTime);
                    const end = Math.max(dragState.startTime, currentTime);
                    setTempSelection({ start, end });
                    break;
                }
                case 'select_left': {
                    const newStart = Math.min(dragState.initialEnd, Math.max(displayMinTime, dragState.initialStart + deltaTime));
                    setTempSelection({ start: newStart, end: dragState.initialEnd });
                    break;
                }
                case 'select_right': {
                    const newEnd = Math.max(dragState.initialStart, Math.min(displayMaxTime, dragState.initialEnd + deltaTime));
                    setTempSelection({ start: dragState.initialStart, end: newEnd });
                    break;
                }
                case 'cursor': {
                     const newCursorTime = Math.max(displayMinTime, Math.min(displayMaxTime, dragState.initialCursor + deltaTime));
                     onCursorChange(newCursorTime);
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
    }, [dragState, mainPosToValue, displayMinTime, displayMaxTime, onRangeChange, onCursorChange, tempSelection, uiScale]);

    const barComponents = [];
    const barProps = { displayMinTime: displayMinTime, displayMaxTime: displayMaxTime };
    if (viewMode === 'pagination' && pageTimestampRanges.length > 0 && onGoToPage) barComponents.push({key: 'page', label: 'Pages', Comp: Bar, props: { ...barProps, items: pageTimestampRanges, isActive: (item: any) => item.page === currentPage, onSelect: (item: any) => onGoToPage(item.page), getLabel: (item: any) => item.page, getTitle: (item: any) => `Page ${item.page}`, getStart: (item: any) => new Date(item.startTime + 'Z').getTime(), getEnd: (item: any) => new Date(item.endTime + 'Z').getTime(), getColor: (i: number) => PALETTE[i % PALETTE.length] }});
    if (fileTimeRanges.length > 0) barComponents.push({key: 'file', label: 'Files', Comp: Bar, props: { ...barProps, items: fileTimeRanges, isActive: (item: any) => item.name === activeFileName, onSelect: (item: any) => onFileSelect(item.name), getLabel: (item: any) => item.name.split('/').pop(), getTitle: (item: any) => item.name, getStart: (item: any) => new Date(item.startTime + 'Z').getTime(), getEnd: (item: any) => new Date(item.endTime + 'Z').getTime(), getColor: (i: number) => PALETTE[i % PALETTE.length] }});
    if (datesWithLogs.length > 0) barComponents.push({key: 'date', label: 'Date', Comp: Bar, props: { ...barProps, items: datesWithLogs, isActive: (item: any) => item === activeDate, onSelect: (item: any) => onDateSelect(item), getLabel: (item: any) => item, getTitle: (item: any) => item, getStart: (item: any) => new Date(`${item}T00:00:00.000Z`).getTime(), getEnd: (item: any) => new Date(`${item}T23:59:59.999Z`).getTime(), getColor: (i: number) => PALETTE[i % PALETTE.length] }});
    if (logDensity.length > 0) barComponents.push({key: 'density', label: 'Density', Comp: DensityBar, props: { items: logDensity, theme: theme, displayMinTime: displayMinTime, displayMaxTime: displayMaxTime }});

    const currentStart = tempSelection?.start ?? selectedStartTime;
    const currentEnd = tempSelection?.end ?? selectedEndTime;

    const visibleSelectionStart = currentStart !== null ? Math.max(currentStart, displayMinTime) : null;
    const visibleSelectionEnd = currentEnd !== null ? Math.min(currentEnd, displayMaxTime) : null;

    const startPos = visibleSelectionStart !== null ? mainValueToPos(visibleSelectionStart) : -1;
    const endPos = visibleSelectionEnd !== null ? mainValueToPos(visibleSelectionEnd) : -1;

    return (
        <div className="w-full">
            <div className="flex items-start gap-3 w-full">
                <div className="w-16 flex-shrink-0 text-right pt-6">
                    <div className="space-y-3">
                        {barComponents.map(bar => (
                            <div key={bar.key} className="h-5 text-xs font-semibold text-gray-600 dark:text-gray-400 flex items-center justify-end pr-2">
                                {bar.label}
                            </div>
                        ))}
                    </div>
                </div>
                
                <div className="flex-grow flex flex-col relative pt-6">
                    <div 
                        onMouseDown={(e) => handleMouseDown(e, 'new_selection')}
                        className="w-full cursor-crosshair bg-gray-200 dark:bg-gray-700/50 rounded p-1 overflow-hidden"
                    >
                        <div ref={mainContainerRef} className="relative">
                            <div className="space-y-3">
                                {barComponents.map(bar => (
                                    <bar.Comp key={bar.key} valueToPosition={mainValueToPos} {...bar.props} />
                                ))}
                            </div>
                             {startPos >= 0 && endPos >= 0 && (endPos > startPos) && (
                                <div
                                    className="absolute top-0 bottom-0 bg-sky-500/20 dark:bg-sky-400/20 z-10 border-x border-sky-600 dark:border-sky-400"
                                    style={{ left: `${startPos}px`, width: `${Math.max(0, endPos - startPos)}px` }}
                                >
                                    <div
                                        data-handle="left"
                                        onMouseDown={(e) => handleMouseDown(e, 'select_left')}
                                        className="absolute top-0 -left-2 w-4 h-full cursor-col-resize group"
                                    >
                                        <div className="absolute left-1/2 -translate-x-1/2 top-1/2 -translate-y-1/2 h-8 w-1.5 bg-sky-600 dark:bg-sky-400 rounded-full opacity-0 group-hover:opacity-100 transition-opacity duration-150" />
                                    </div>
                                    <div
                                        data-handle="right"
                                        onMouseDown={(e) => handleMouseDown(e, 'select_right')}
                                        className="absolute top-0 -right-2 w-4 h-full cursor-col-resize group"
                                    >
                                         <div className="absolute left-1/2 -translate-x-1/2 top-1/2 -translate-y-1/2 h-8 w-1.5 bg-sky-600 dark:bg-sky-400 rounded-full opacity-0 group-hover:opacity-100 transition-opacity duration-150" />
                                    </div>
                                </div>
                            )}
                            {cursorTime !== null && mainContainerWidth > 0 && cursorTime >= displayMinTime && cursorTime <= displayMaxTime && (
                                <div 
                                    data-handle="cursor"
                                    onMouseDown={(e) => handleMouseDown(e, 'cursor')}
                                    className="absolute top-0 bottom-0 bg-red-500 z-30 cursor-col-resize w-0.5" 
                                    style={{ left: `${mainValueToPos(cursorTime)}px` }} 
                                />
                            )}
                        </div>
                    </div>
                    {startPos >= 0 && <div className="absolute top-0 text-xs font-mono text-gray-700 dark:text-gray-200 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm px-1.5 py-0.5 rounded shadow z-40" style={{ left: `${startPos}px`, transform: 'translateX(-50%)' }}>{formatTooltip(currentStart)}</div>}
                    {endPos >= 0 && (endPos - startPos > 60) && <div className="absolute top-0 text-xs font-mono text-gray-700 dark:text-gray-200 bg-white/80 dark:bg-gray-800/80 backdrop-blur-sm px-1.5 py-0.5 rounded shadow z-40" style={{ left: `${endPos}px`, transform: 'translateX(-50%)' }}>{formatTooltip(currentEnd)}</div>}
                    {cursorTime !== null && cursorTime >= displayMinTime && cursorTime <= displayMaxTime && <div className="absolute top-0 text-xs font-mono bg-red-500/90 backdrop-blur-sm text-white rounded px-1.5 py-0.5 z-40" style={{ left: `${mainValueToPos(cursorTime)}px`, transform: 'translateX(-50%)' }}>{formatTooltip(cursorTime)}</div>}
                </div>
                
                <div className="w-auto flex-shrink-0 self-start pt-6 flex flex-col items-center gap-1">
                     <button onClick={onZoomToSelection} disabled={!zoomToSelectionEnabled} className="p-2 text-gray-500 dark:text-gray-400 rounded-full hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors disabled:opacity-50 disabled:cursor-not-allowed" title="Zoom to Selection"><Icon name="ArrowsPointingIn" iconSet={iconSet} className="w-5 h-5"/></button>
                     <button onClick={onZoomToExtent} disabled={!viewRange} className="p-2 text-gray-500 dark:text-gray-400 rounded-full hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors disabled:opacity-50 disabled:cursor-not-allowed" title="Zoom to Extent"><Icon name="ArrowsPointingOut" iconSet={iconSet} className="w-5 h-5"/></button>
                     <button onClick={onClear} className="p-2 text-gray-500 dark:text-gray-400 rounded-full hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors" title="Clear time selection"><Icon name="XMark" iconSet={iconSet} className="w-6 h-6"/></button>
                </div>
            </div>
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
        </div>
    );
};