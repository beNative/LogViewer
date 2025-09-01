



import React from 'react';
import { LogEntry, ColumnVisibilityState, ColumnStyles, ColumnKey } from '../types.ts';
import { highlightText } from '../utils.ts';
import { getLevelColor } from './LogTable.tsx';

const ROW_HEIGHT = 56; // px
const OVERSCAN_COUNT = 8;
// Set a max height for the scrollable element to avoid browser limitations (around 33.5M pixels in Chrome).
// 10M pixels is safe and allows for ~178k rows before scaling kicks in.
const MAX_SCROLL_HEIGHT = 10_000_000;

type Theme = 'light' | 'dark';

interface VirtualizedLogViewProps {
  entries: LogEntry[];
  entriesOffset: number;
  totalFilteredCount: number;
  columnVisibility: ColumnVisibilityState;
  columnStyles: ColumnStyles;
  getTdStyle: (key: ColumnKey) => React.CSSProperties;
  onVirtualScroll: (scrollTop: number) => void;
  hasMore: boolean;
  isBusy: boolean;
  onEntrySelect: (entry: LogEntry) => void;
  selectedEntryId: number | null;
  keyboardSelectedId: number | null;
  highlightTerms: string[];
  theme: Theme;
  onViewportChange: (range: { start: number; end: number }) => void;
}

const columnConfig: Record<ColumnKey, { flex: string, minWidth?: string, className?: string, header: string }> = {
    time: { flex: '0 0 190px', header: 'Timestamp', className: 'pl-4 pr-3 sm:pl-6' },
    level: { flex: '0 0 120px', header: 'Level', className: 'px-3' },
    sndrtype: { flex: '0 0 150px', header: 'Sender Type', className: 'px-3' },
    sndrname: { flex: '0 0 180px', header: 'Sender Name', className: 'px-3' },
    fileName: { flex: '0 0 220px', header: 'Filename', className: 'px-3 truncate' },
    msg: { flex: '1 1 0px', minWidth: '200px', header: 'Message', className: 'px-3' },
};

const VirtualizedRow: React.FC<{
  entry: LogEntry;
  style: React.CSSProperties;
  isKeyboardSelected: boolean;
  columnVisibility: ColumnVisibilityState;
  getTdStyle: (key: ColumnKey) => React.CSSProperties;
  highlightTerms: string[];
  theme: Theme;
  onSelect: (entry: LogEntry) => void;
}> = React.memo(({ entry, style, isKeyboardSelected, columnVisibility, getTdStyle, highlightTerms, theme, onSelect }) => {
    
    return (
        <div
            style={style}
            onClick={() => onSelect(entry)}
            data-row-id={entry.id}
            className={`absolute w-full flex items-center border-b border-gray-200/50 dark:border-gray-700/50 cursor-pointer transition-colors duration-150 overflow-hidden
                ${isKeyboardSelected ? 'bg-sky-50 dark:bg-sky-900/60 ring-2 ring-inset ring-sky-500' : 'hover:bg-gray-100 dark:hover:bg-gray-800/70'}
            `}
        >
            {columnVisibility.time && (
                <div style={{ flex: columnConfig.time.flex, ...getTdStyle('time') }} className={`whitespace-nowrap ${columnConfig.time.className}`}>
                    {entry.time}
                </div>
            )}
            {columnVisibility.level && (
                <div style={{ flex: columnConfig.level.flex }} className={`whitespace-nowrap ${columnConfig.level.className}`}>
                    <span style={getTdStyle('level')} className={`px-2 py-1 rounded-full font-medium ${getLevelColor(entry.level)}`}>
                        {entry.level}
                    </span>
                </div>
            )}
            {columnVisibility.sndrtype && (
                <div style={{ flex: columnConfig.sndrtype.flex, ...getTdStyle('sndrtype') }} className={`whitespace-nowrap truncate ${columnConfig.sndrtype.className}`}>
                    {entry.sndrtype}
                </div>
            )}
            {columnVisibility.sndrname && (
                <div style={{ flex: columnConfig.sndrname.flex, ...getTdStyle('sndrname') }} className={`whitespace-nowrap truncate ${columnConfig.sndrname.className}`}>
                    {entry.sndrname}
                </div>
            )}
            {columnVisibility.fileName && (
                <div style={{ flex: columnConfig.fileName.flex, ...getTdStyle('fileName') }} className={`whitespace-nowrap truncate ${columnConfig.fileName.className}`} title={entry.fileName}>
                    {entry.fileName}
                </div>
            )}
            {columnVisibility.msg && (
                 <div style={{ flex: columnConfig.msg.flex, minWidth: columnConfig.msg.minWidth, ...getTdStyle('msg') }} className={`whitespace-pre-wrap break-words ${columnConfig.msg.className}`} dangerouslySetInnerHTML={{ __html: highlightText(entry.msg, highlightTerms, theme) }} />
            )}
        </div>
    );
});


export const VirtualizedLogView = React.forwardRef<HTMLDivElement, VirtualizedLogViewProps>(
  ({ entries, entriesOffset, totalFilteredCount, onViewportChange, ...props }, ref) => {
    const { isBusy, onVirtualScroll, getTdStyle, columnVisibility, highlightTerms, theme, onEntrySelect } = props;
    const scrollContainerRef = React.useRef<HTMLDivElement>(null);
    const [scrollTop, setScrollTop] = React.useState(0);
    const debounceTimeout = React.useRef<number | null>(null);

    const realHeight = totalFilteredCount * ROW_HEIGHT;
    const displayHeight = realHeight > 0 ? Math.min(realHeight, MAX_SCROLL_HEIGHT) : 0;
    
    const getMappedScrollTop = React.useCallback((displayScrollTop: number): number => {
        if (realHeight <= displayHeight) {
            return displayScrollTop;
        }
        const container = scrollContainerRef.current;
        if (!container) return displayScrollTop;
        
        const displayScrollableDist = displayHeight - container.clientHeight;
        if (displayScrollableDist <= 0) return 0; // Avoid division by zero
    
        const realScrollableDist = realHeight - container.clientHeight;
        if (realScrollableDist <= 0) return 0;

        const scrollPercent = displayScrollTop / displayScrollableDist;
        return scrollPercent * realScrollableDist;
    }, [realHeight, displayHeight]);

    const getDisplayScrollTop = React.useCallback((realScrollTop: number): number => {
        if (realHeight <= displayHeight) {
            return realScrollTop;
        }
        const container = scrollContainerRef.current;
        if (!container) return realScrollTop;
        
        const displayScrollableDist = displayHeight - container.clientHeight;
        if (displayScrollableDist <= 0) return 0;

        const realScrollableDist = realHeight - container.clientHeight;
        if (realScrollableDist <= 0) return 0;
        
        const scrollPercent = realScrollTop / realScrollableDist;
        return scrollPercent * displayScrollableDist;
    }, [realHeight, displayHeight]);
    
    const calculateAndUpdateViewport = React.useCallback(() => {
        const container = scrollContainerRef.current;
        if (!container) return;
        
        const mappedScrollTop = getMappedScrollTop(container.scrollTop);
        const start = Math.floor(mappedScrollTop / ROW_HEIGHT);
        const visibleRows = Math.floor(container.clientHeight / ROW_HEIGHT);
        const end = Math.min(totalFilteredCount - 1, start + visibleRows);
        
        onViewportChange({ start, end: Math.max(start, end) });
    }, [onViewportChange, totalFilteredCount, getMappedScrollTop]);

    const handleScroll = React.useCallback((e: React.UIEvent<HTMLDivElement>) => {
        const newScrollTop = e.currentTarget.scrollTop;
        setScrollTop(newScrollTop); // Store the actual browser scroll top
        calculateAndUpdateViewport();

        if (debounceTimeout.current) clearTimeout(debounceTimeout.current);
        debounceTimeout.current = window.setTimeout(() => {
            onVirtualScroll(getMappedScrollTop(newScrollTop));
        }, 150);
    }, [calculateAndUpdateViewport, onVirtualScroll, getMappedScrollTop]);

    React.useLayoutEffect(() => {
        const container = scrollContainerRef.current;
        if (!container) return;
        
        const resizeObserver = new ResizeObserver(() => {
            calculateAndUpdateViewport();
        });
        
        resizeObserver.observe(container);
        calculateAndUpdateViewport();
        
        return () => resizeObserver.disconnect();
    }, [calculateAndUpdateViewport]);

    React.useEffect(() => {
        const container = scrollContainerRef.current;
        if (!container || props.keyboardSelectedId === null) return;

        const selectedIndex = entries.findIndex(e => e.id === props.keyboardSelectedId);
        if (selectedIndex === -1) return;

        const targetRealRowTop = (entriesOffset + selectedIndex) * ROW_HEIGHT;
        const mappedViewTop = getMappedScrollTop(container.scrollTop);
        const viewHeight = container.clientHeight;

        if (targetRealRowTop < mappedViewTop) {
            container.scrollTop = getDisplayScrollTop(targetRealRowTop);
        } else if (targetRealRowTop + ROW_HEIGHT > mappedViewTop + viewHeight) {
            container.scrollTop = getDisplayScrollTop(targetRealRowTop - viewHeight + ROW_HEIGHT);
        }
    }, [props.keyboardSelectedId, entries, entriesOffset, getMappedScrollTop, getDisplayScrollTop]);


    const mappedScrollTop = getMappedScrollTop(scrollTop);
    const startIndex = Math.max(0, Math.floor(mappedScrollTop / ROW_HEIGHT) - OVERSCAN_COUNT);
    const containerHeight = scrollContainerRef.current?.clientHeight || 0;
    const endIndex = Math.min(
      totalFilteredCount, 
      startIndex + Math.ceil(containerHeight / ROW_HEIGHT) + (2 * OVERSCAN_COUNT)
    );

    const visibleEntries = React.useMemo(() => {
        const visible: {entry: LogEntry, indexInFullList: number}[] = [];
        for (let i = 0; i < entries.length; i++) {
            const entry = entries[i];
            const actualIndex = entriesOffset + i;
            if (actualIndex >= startIndex && actualIndex <= endIndex) {
                visible.push({ entry, indexInFullList: actualIndex });
            }
        }
        return visible;
    }, [entries, entriesOffset, startIndex, endIndex]);

    return (
        <div ref={ref} className="absolute inset-0 grid grid-rows-[auto_1fr] bg-white dark:bg-gray-900 overflow-hidden">
            <div className="flex-shrink-0 bg-gray-100 dark:bg-gray-800 sticky top-0 z-10 border-b border-gray-200 dark:border-gray-700">
                <div className="flex min-w-full h-12 items-center">
                    {Object.entries(columnConfig).map(([key, config]) => 
                        props.columnVisibility[key as ColumnKey] && (
                            <div key={key} style={{ flex: config.flex, minWidth: config.minWidth }} className={`py-3.5 text-left text-sm font-semibold text-gray-900 dark:text-gray-300 ${config.className}`}>
                                {config.header}
                            </div>
                        )
                    )}
                </div>
            </div>

            <div
                ref={scrollContainerRef}
                onScroll={handleScroll}
                className="overflow-y-auto relative"
            >
                <div className="relative" style={{ height: displayHeight }}>
                    {visibleEntries.map(({ entry, indexInFullList }) => {
                        const realTop = indexInFullList * ROW_HEIGHT;
                        const displayTop = getDisplayScrollTop(realTop);

                        return (
                            <VirtualizedRow
                                key={entry.id}
                                entry={entry}
                                style={{
                                    height: `${ROW_HEIGHT}px`,
                                    transform: `translateY(${displayTop}px)`
                                }}
                                isKeyboardSelected={props.keyboardSelectedId === entry.id}
                                onSelect={onEntrySelect}
                                getTdStyle={getTdStyle}
                                columnVisibility={columnVisibility}
                                highlightTerms={highlightTerms}
                                theme={theme}
                            />
                        );
                    })}
                </div>
                
                {isBusy && (
                    <div className="sticky w-full flex justify-center top-1/2 -translate-y-1/2 z-20 pointer-events-none">
                         <div className="p-3 bg-gray-200/80 dark:bg-gray-800/80 rounded-lg shadow-lg backdrop-blur-sm">
                            <p className="text-gray-800 dark:text-gray-200 font-semibold animate-pulse">
                                Loading...
                            </p>
                        </div>
                    </div>
                )}

                {totalFilteredCount === 0 && !isBusy && (
                     <div className="flex items-center justify-center h-full">
                        <p className="text-center py-12 text-gray-500 dark:text-gray-400">
                            No log items found matching the filters.
                        </p>
                    </div>
                )}
            </div>
        </div>
    );
});