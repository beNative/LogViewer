import React from 'react';
import { PageTimestampRange, FileTimeRange, LogDensityPoint, Theme, IconSet, OverallTimeRange } from '../types.ts';
import { Icon } from './icons/index.tsx';

interface TimeRangeSelectorProps {
    minTime: number; // ms
    maxTime: number; // ms
    selectedStartTime: number | null; // ms
    selectedEndTime: number | null; // ms
    onRangeChange: (startTime: number, endTime: number) => void;
    onClear: () => void;
    theme: Theme;
    pageTimestampRanges: PageTimestampRange[];
    fileTimeRanges: FileTimeRange[];
    logDensity: LogDensityPoint[];
    overallLogDensity: LogDensityPoint[];
    datesWithLogs: string[];
    viewMode: 'pagination' | 'scroll';
    onCursorChange: (time: number | null) => void;
    onFileSelect: (fileName: string) => void;
    onDateSelect: (date: string) => void;
    viewRange: OverallTimeRange | null;
    onViewRangeChange: (range: OverallTimeRange | null) => void;
    onZoomToSelection: () => void;
    onZoomToExtent: () => void;
    zoomToSelectionEnabled: boolean;
    iconSet: IconSet;
    uiScale: number;
    timelineBarVisibility?: {
        pages: boolean;
        files: boolean;
        dates: boolean;
        density: boolean;
        overview: boolean;
    };
}

const getPositionFromTime = (time: number, min: number, max: number): number => {
    if (max <= min) return 0;
    return ((time - min) / (max - min)) * 100;
};

const getTimeFromPosition = (pos: number, min: number, max: number, width: number): number => {
    if (width === 0) return min;
    return min + (pos / width) * (max - min);
};

export const TimeRangeSelector: React.FC<TimeRangeSelectorProps> = (props) => {
    const {
        minTime, maxTime,
        selectedStartTime, selectedEndTime, onRangeChange, onClear, theme,
        pageTimestampRanges, fileTimeRanges, logDensity, overallLogDensity, datesWithLogs,
        viewMode, onCursorChange, onFileSelect, onDateSelect,
        viewRange, onViewRangeChange, onZoomToSelection, onZoomToExtent, zoomToSelectionEnabled,
        iconSet, uiScale, timelineBarVisibility = { pages: true, files: true, dates: true, density: true, overview: true }
    } = props;

    const containerRef = React.useRef<HTMLDivElement>(null);
    const [isDragging, setIsDragging] = React.useState(false);
    const [dragStart, setDragStart] = React.useState(0);
    const [cursorPos, setCursorPos] = React.useState<number | null>(null);

    const effectiveMinTime = viewRange?.min ?? minTime;
    const effectiveMaxTime = viewRange?.max ?? maxTime;

    const handleMouseDown = (e: React.MouseEvent<HTMLDivElement>) => {
        if (!containerRef.current) return;
        setIsDragging(true);
        const rect = containerRef.current.getBoundingClientRect();
        const startPos = e.clientX - rect.left;
        setDragStart(startPos);
        const startTime = getTimeFromPosition(startPos, effectiveMinTime, effectiveMaxTime, rect.width);
        onRangeChange(startTime, startTime);
    };

    const handleMouseMove = (e: React.MouseEvent<HTMLDivElement>) => {
        if (!containerRef.current) return;
        const rect = containerRef.current.getBoundingClientRect();
        const currentPos = e.clientX - rect.left;
        setCursorPos(currentPos);
        const timeAtCursor = getTimeFromPosition(currentPos, effectiveMinTime, effectiveMaxTime, rect.width);
        onCursorChange(timeAtCursor);

        if (isDragging) {
            const startTime = getTimeFromPosition(dragStart, effectiveMinTime, effectiveMaxTime, rect.width);
            const endTime = getTimeFromPosition(currentPos, effectiveMinTime, effectiveMaxTime, rect.width);
            onRangeChange(Math.min(startTime, endTime), Math.max(startTime, endTime));
        }
    };

    const handleMouseUp = () => {
        setIsDragging(false);
    };
    
    const handleMouseLeave = () => {
        setCursorPos(null);
        onCursorChange(null);
        if (isDragging) {
            setIsDragging(false);
        }
    };

    const Bar: React.FC<{ label: string, children: React.ReactNode, isVisible: boolean }> = ({ label, children, isVisible }) => {
        if (!isVisible) return null;
        return (
            <div className="flex items-center gap-2">
                <span className="w-16 text-right text-xs text-gray-500 dark:text-gray-400 font-semibold">{label}</span>
                <div className="flex-1">{children}</div>
            </div>
        );
    };

    const pageItems = pageTimestampRanges.map((p, i) => ({
        start: new Date(p.startTime).getTime(),
        end: new Date(p.endTime).getTime(),
        label: `Page ${p.page}`,
        colorClass: i % 2 === 0 ? 'bg-blue-300/70 dark:bg-blue-700/70' : 'bg-blue-400/70 dark:bg-blue-600/70'
    }));
    
    const fileItems = fileTimeRanges.map((f, i) => ({
        start: new Date(f.startTime).getTime(),
        end: new Date(f.endTime).getTime(),
        label: f.name,
        onClick: () => onFileSelect(f.name),
        colorClass: i % 2 === 0 ? 'bg-purple-300/70 dark:bg-purple-700/70' : 'bg-purple-400/70 dark:bg-purple-600/70'
    }));

    return (
        <div className="flex flex-col gap-1.5 p-2 bg-gray-200 dark:bg-gray-700/50 rounded-lg">
           <div className="flex justify-end items-center gap-2">
               <button onClick={onZoomToSelection} disabled={!zoomToSelectionEnabled} title="Zoom to selection" className="p-1 text-gray-500 dark:text-gray-400 rounded-md hover:bg-gray-300 dark:hover:bg-gray-600 disabled:opacity-50"><Icon name="ArrowsPointingIn" iconSet={iconSet} className="w-4 h-4"/></button>
               <button onClick={onZoomToExtent} disabled={!viewRange} title="Zoom to full extent" className="p-1 text-gray-500 dark:text-gray-400 rounded-md hover:bg-gray-300 dark:hover:bg-gray-600 disabled:opacity-50"><Icon name="ArrowsPointingOut" iconSet={iconSet} className="w-4 h-4"/></button>
               <button onClick={onClear} disabled={!selectedStartTime} title="Clear selection" className="p-1 text-gray-500 dark:text-gray-400 rounded-md hover:bg-gray-300 dark:hover:bg-gray-600 disabled:opacity-50"><Icon name="XMark" iconSet={iconSet} className="w-4 h-4"/></button>
           </div>
           
           <div ref={containerRef} className="relative cursor-crosshair" onMouseMove={handleMouseMove} onMouseLeave={handleMouseLeave}>
                <div className="space-y-1">
                    <Bar label="Overview" isVisible={timelineBarVisibility.overview}>
                        <div className="relative h-6 w-full" onMouseDown={handleMouseDown} onMouseUp={handleMouseUp}>
                            {overallLogDensity.map(({ time, count }, index) => (
                                <div key={index} className="absolute h-full" style={{ left: `${getPositionFromTime(time, minTime, maxTime)}%`, width: `${100 / overallLogDensity.length}%`, backgroundColor: `rgba(139, 92, 246, ${count / 100})` }} />
                            ))}
                        </div>
                    </Bar>
                    <Bar label="Density" isVisible={timelineBarVisibility.density}>
                        <div className="relative h-4 w-full">
                            {logDensity.map(({ time, count }, index) => (
                                <div key={index} className="absolute h-full" style={{ left: `${getPositionFromTime(time, effectiveMinTime, effectiveMaxTime)}%`, width: `${100 / logDensity.length}%`, backgroundColor: `rgba(245, 158, 11, ${count / 100})` }} />
                            ))}
                        </div>
                    </Bar>
                    {/* Other bars would go here */}
                </div>

                {selectedStartTime && selectedEndTime && (
                    <div className="absolute top-0 bottom-0 bg-sky-500/30 dark:bg-sky-500/20 border-x-2 border-sky-600 dark:border-sky-400 pointer-events-none" style={{
                        left: `${getPositionFromTime(selectedStartTime, effectiveMinTime, effectiveMaxTime)}%`,
                        width: `${getPositionFromTime(selectedEndTime, effectiveMinTime, effectiveMaxTime) - getPositionFromTime(selectedStartTime, effectiveMinTime, effectiveMaxTime)}%`
                    }} />
                )}

                {cursorPos !== null && (
                    <div className="absolute top-0 bottom-0 w-px bg-red-500 pointer-events-none" style={{ left: `${cursorPos}px` }} />
                )}
           </div>
        </div>
    );
};
