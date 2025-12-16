import React from 'react';
import { LogDensityPointByLevel, LogDensityPoint } from '../../types.ts';
import { Tooltip } from '../Tooltip.tsx';
import {
    Theme,
    LOG_LEVEL_ORDER,
    getLevelColor,
} from '../../utils/timelineUtils.ts';

/** Function type for converting time value to pixel position */
export type ValueToPositionFn = (value: number) => number;

/**
 * Generic Bar component for rendering timeline segments.
 * Renders a horizontal bar with clickable segments representing time ranges.
 * 
 * @template T - The type of items being rendered
 * @param items - Array of items to render as bar segments
 * @param valueToPosition - Function to convert time value to pixel position
 * @param isActive - Function to determine if an item is currently active
 * @param onSelect - Callback when a segment is clicked
 * @param getLabel - Function to get display label for a segment
 * @param getTitle - Function to get tooltip title for a segment
 * @param getStart - Function to get start timestamp of a segment
 * @param getEnd - Function to get end timestamp of a segment
 * @param getColor - Function to get color for a segment by index
 * @param displayMinTime - Minimum visible time in the current view
 * @param displayMaxTime - Maximum visible time in the current view
 */
export function Bar<T>({ items, valueToPosition, isActive, onSelect, getLabel, getTitle, getStart, getEnd, getColor, displayMinTime, displayMaxTime }: {
    items: T[];
    valueToPosition: ValueToPositionFn;
    isActive: (item: T) => boolean;
    onSelect: (item: T) => void;
    getLabel: (item: T) => string;
    getTitle: (item: T) => string;
    getStart: (item: T) => number;
    getEnd: (item: T) => number;
    getColor: (index: number) => string;
    displayMinTime: number;
    displayMaxTime: number;
}): React.ReactElement {
    return (
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
}

/**
 * DensityBar component for rendering log density visualization.
 * Displays stacked bars showing log count distribution by level or simple density bars.
 * 
 * @param items - Array of density data points (with level counts or simple counts)
 * @param valueToPosition - Function to convert time value to pixel position
 * @param theme - Current color theme ('light' or 'dark')
 * @param displayMinTime - Minimum visible time in the current view
 * @param displayMaxTime - Maximum visible time in the current view
 * @param onMouseMove - Optional mouse move handler for tooltips
 * @param onMouseLeave - Optional mouse leave handler for tooltips
 */
export const DensityBar: React.FC<{
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
            }, 1);
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
                    const widthPx = Math.max(1, rightPx - leftPx + 0.5); // Add 0.5px overlap to prevent sub-pixel gaps

                    // if (widthPx === 0) return null; // Logic removed as min width is 1

                    const sortedLevels = Object.entries(bucket.counts).sort((a, b) => {
                        return (LOG_LEVEL_ORDER[a[0].toUpperCase()] ?? 99) - (LOG_LEVEL_ORDER[b[0].toUpperCase()] ?? 99);
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
                                        minHeight: count > 0 ? '1px' : '0',
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
                    const widthPx = Math.max(1, rightPx - leftPx + 0.5); // Add 0.5px overlap for simple bars too

                    // if (widthPx === 0) return null;

                    return (
                        <div
                            key={`density-bar-${i}`}
                            style={{ left: `${leftPx}px`, width: `${widthPx}px`, height: `${barHeightPercent}%`, minHeight: '1px', backgroundColor: theme === 'dark' ? '#60a5fa' : '#3b82f6' }}
                            className="absolute bottom-0"
                        />
                    );
                })}
            </div>
        );
    }
};
