import React from 'react';
import { LogEntry, FilterState, PageTimestampRange, ColumnVisibilityState, ColumnStyles, ColumnKey, PanelWidths, ViewMode, ConsoleMessage, OverallTimeRange, FileTimeRange, LogDensityPointByLevel, IconSet, LogTableDensity, Theme, TimelineBarVisibility } from '../types';
import { Icon } from './icons';
import { FilterBar } from './FilterBar';
import { LogDetailPanel } from './LogDetailPanel';
import { highlightText } from '../utils';
import { TimeRangeSelector } from './TimeRangeSelector';
import { ActiveFilters } from './ActiveFilters';
import { ContextMenu } from './ContextMenu';
import { Splitter } from './Splitter';
import { ColumnVisibilityMenu } from './ColumnVisibilityMenu';

type ContextMenuState = { x: number; y: number; entry: LogEntry; value: string; key: ColumnKey } | null;

export const getLevelColor = (level: string) => {
    switch (level?.toUpperCase()) {
        case 'ERROR':
        case 'FATAL':
            return 'bg-red-100 text-red-800 dark:bg-red-900/50 dark:text-red-300';
        case 'WARNING':
        case 'WARN':
            return 'bg-amber-100 text-amber-800 dark:bg-amber-900/50 dark:text-amber-300';
        case 'INFO':
            return 'bg-sky-100 text-sky-800 dark:bg-sky-900/50 dark:text-sky-300';
        case 'DEBUG':
        case 'TRACE':
            return 'bg-gray-200 text-gray-800 dark:bg-gray-700 dark:text-gray-300';
        default:
            return 'bg-gray-100 text-gray-700 dark:bg-gray-800 dark:text-gray-400';
    }
};

interface LogTableProps {
    entries: LogEntry[];
    entriesOffset: number;
    totalFilteredCount: number;
    loadedFileNames: string[];
    pageTimestampRanges: PageTimestampRange[];
    onViewModeChange: (newMode: ViewMode) => void;
    filters: FilterState;
    appliedFilters: FilterState;
    onFiltersChange: (newFilters: FilterState) => void;
    onApplyFilters: () => void;
    onResetFilters: () => void;
    onClearTimeRange: () => void;
    uniqueValues: {
        level: string[];
        sndrtype: string[];
        sndrname: string[];
        fileName: string[];
    };
    theme: Theme;
    viewMode: ViewMode;
    columnVisibility: ColumnVisibilityState;
    onColumnVisibilityChange: (newState: ColumnVisibilityState) => void;
    columnStyles: ColumnStyles;
    panelWidths: PanelWidths;
    onPanelWidthsChange: (newWidths: PanelWidths) => void;
    isDetailPanelVisible: boolean;
    onDetailPanelVisibilityChange: (isVisible: boolean) => void;
    onApplyFilter: (key: 'level' | 'sndrtype' | 'sndrname' | 'fileName', value: string) => void;
    onContextMenuFilter: (key: 'level' | 'sndrtype' | 'sndrname' | 'fileName', value: string, exclude: boolean) => void;
    customFilterPresets: Record<string, FilterState>;
    onSavePreset: (name: string) => void;
    onDeletePreset: (name: string) => void;
    onLoadPreset: (name: string) => void;
    onLoadMore: () => void;
    onJumpToOffset: (offset: number) => void;
    hasMore: boolean;
    isBusy: boolean;
    logToConsole: (message: string, type: ConsoleMessage['type']) => void;
    overallTimeRange: OverallTimeRange | null;
    onTimeRangeSelectorChange: (startTime: number, endTime: number) => void;
    isTimeRangeSelectorVisible: boolean;
    onTimeRangeSelectorVisibilityChange: (isVisible: boolean) => void;
    fileTimeRanges: FileTimeRange[];
    logDensity: LogDensityPointByLevel[];
    overallLogDensity: LogDensityPointByLevel[];
    datesWithLogs: string[];
    onCursorChange: (time: number) => void;
    onFileSelect: (fileName: string) => void;
    onDateSelect: (date: string) => void;
    keyboardSelectedId: number | null;
    setKeyboardSelectedId: (id: number | null) => void;
    jumpToEntryId: number | null;
    timelineViewRange: OverallTimeRange | null;
    onTimelineViewRangeChange: (range: OverallTimeRange | null) => void;
    onTimelineZoomToSelection: () => void;
    onTimelineZoomReset: () => void;
    isInitialLoad: boolean;
    iconSet: IconSet;
    onRemoveAppliedFilter: (key: keyof FilterState, value?: string) => void;
    logTableDensity: LogTableDensity;
    onLogTableDensityChange: (density: LogTableDensity) => void;
    uiScale: number;
    cursorTime: number | null;
    timelineBarVisibility: TimelineBarVisibility;
    onTimelineBarVisibilityChange: (newVisibility: TimelineBarVisibility) => void;
}

const INFINITE_SCROLL_CHUNK_SIZE = 200;

export const LogTable: React.FC<LogTableProps> = (props) => {
    const [selectedEntry, setSelectedEntry] = React.useState<LogEntry | null>(null);
    const [contextMenuState, setContextMenuState] = React.useState<ContextMenuState>(null);
    const [headerContextMenu, setHeaderContextMenu] = React.useState<{ x: number, y: number } | null>(null);
    const viewportRef = React.useRef<HTMLDivElement>(null);
    const rowRefs = React.useRef<Map<number, HTMLTableRowElement | null>>(new Map());
    
    // Refs for performant scroll handling
    const scrollTopRef = React.useRef(0);
    const tickingRef = React.useRef(false);
    const [, forceUpdate] = React.useReducer(x => x + 1, 0);
    const userScrollingRef = React.useRef<number | null>(null);

    const panelWidthsRef = React.useRef(props.panelWidths);
    panelWidthsRef.current = props.panelWidths;

    const {
        entries,
        entriesOffset,
        totalFilteredCount,
        viewMode,
        onLoadMore,
        onJumpToOffset,
        hasMore,
        isBusy,
        columnVisibility,
        columnStyles,
        logTableDensity,
        keyboardSelectedId,
        setKeyboardSelectedId,
        appliedFilters,
        theme,
        jumpToEntryId
    } = props;
    
    const getRowHeight = React.useCallback((density: LogTableDensity): number => {
        switch (density) {
            case 'compact': return 24;
            case 'normal': return 28;
            case 'comfortable': return 36;
        }
    }, []);
    
    // This effect ensures the details panel stays in sync with keyboard navigation.
    React.useEffect(() => {
        if (keyboardSelectedId === null) {
            setSelectedEntry(null);
            return;
        }
        const entry = entries.find(e => e.id === keyboardSelectedId);
        if (entry) {
            setSelectedEntry(entry);
        }
    }, [keyboardSelectedId, entries]);

    const getRowClass = (density: LogTableDensity) => {
        switch (density) {
            case 'compact': return 'py-0.5';
            case 'normal': return 'py-1';
            case 'comfortable': return 'py-2';
        }
    };
    const getCellClass = (density: LogTableDensity) => {
        switch (density) {
            case 'compact': return 'px-2';
            case 'normal': return 'px-3';
            case 'comfortable': return 'px-4';
        }
    }

    const handleRowClick = (entry: LogEntry) => {
        setSelectedEntry(entry);
        setKeyboardSelectedId(entry.id);
        if (!props.isDetailPanelVisible) {
            props.onDetailPanelVisibilityChange(true);
        }
        viewportRef.current?.focus({ preventScroll: true });
    };

    // Keyboard navigation effect
    React.useEffect(() => {
        const container = viewportRef.current;
        if (!container) return;

        const handleKeyDown = (e: KeyboardEvent) => {
            if (!['ArrowUp', 'ArrowDown'].includes(e.key)) {
                return;
            }
            e.preventDefault();

            const currentIndex = entries.findIndex(entry => entry.id === keyboardSelectedId);

            if (currentIndex === -1) {
                if (entries.length > 0) {
                    setKeyboardSelectedId(entries[0].id);
                }
                return;
            }

            let nextIndex = currentIndex;
            if (e.key === 'ArrowDown') {
                nextIndex = currentIndex + 1;
            } else if (e.key === 'ArrowUp') {
                nextIndex = currentIndex - 1;
            }

            if (nextIndex >= 0 && nextIndex < entries.length) {
                setKeyboardSelectedId(entries[nextIndex].id);
            } else if (viewMode === 'scroll' && e.key === 'ArrowDown' && nextIndex >= entries.length && hasMore) {
                onLoadMore();
            }
        };
        
        container.addEventListener('keydown', handleKeyDown);
        return () => container.removeEventListener('keydown', handleKeyDown);

    }, [entries, keyboardSelectedId, setKeyboardSelectedId, hasMore, onLoadMore, viewMode]);

    // Scroll into view effect
    React.useEffect(() => {
        if (keyboardSelectedId === null || userScrollingRef.current) return;
    
        if (viewMode === 'scroll' && viewportRef.current) {
            // Calculate the absolute index of the selected item
            const absoluteIndex = entries.findIndex(e => e.id === keyboardSelectedId);
            if (absoluteIndex > -1) {
                const globalIndex = entriesOffset + absoluteIndex;
                const rowHeight = getRowHeight(logTableDensity);
                const targetScrollTop = globalIndex * rowHeight;
    
                const { scrollTop: currentScrollTop, clientHeight: viewportHeight } = viewportRef.current;
    
                // Check if the target row is outside the visible area
                if (targetScrollTop < currentScrollTop || targetScrollTop + rowHeight > currentScrollTop + viewportHeight) {
                    viewportRef.current.scrollTo({
                        top: targetScrollTop - (viewportHeight / 2) + (rowHeight / 2), // Center it
                        behavior: 'auto',
                    });
                }
            }
        } else { // pagination mode
            const rowEl = rowRefs.current.get(keyboardSelectedId);
            if (rowEl) {
                rowEl.scrollIntoView({ block: 'nearest', behavior: 'smooth' });
            }
        }
    }, [keyboardSelectedId, entries, viewMode, logTableDensity, getRowHeight, entriesOffset]);


    const handleContextMenu = (e: React.MouseEvent, entry: LogEntry, key: ColumnKey, value: string) => {
        e.preventDefault();
        setContextMenuState({ x: e.clientX, y: e.clientY, entry, key, value });
    };

    const getStyle = (key: ColumnKey): React.CSSProperties => {
        const styleConf = columnStyles[key];
        if (!styleConf) return {};
        const properties: React.CSSProperties = {
            fontFamily: styleConf.font || 'inherit',
            fontSize: `${styleConf.fontSize}px`,
            fontWeight: styleConf.isBold ? 'bold' : 'normal',
            fontStyle: styleConf.isItalic ? 'italic' : 'normal',
        };
        const color = theme === 'dark' ? styleConf.darkColor : styleConf.color;
        if (color) {
            properties.color = color;
        }
        return properties;
    };
    
    const handleFilterResize = (deltaX: number) => {
        const newWidth = Math.max(240, Math.min(800, panelWidthsRef.current.filters + deltaX));
        props.onPanelWidthsChange({ ...panelWidthsRef.current, filters: newWidth });
    };

    const handleDetailsResize = (deltaX: number) => {
        // Delta is positive when moving right, which should decrease the details panel width
        const newWidth = Math.max(300, Math.min(1200, panelWidthsRef.current.details - deltaX));
        props.onPanelWidthsChange({ ...panelWidthsRef.current, details: newWidth });
    };

    const handleScroll = (e: React.UIEvent<HTMLDivElement>) => {
        if (userScrollingRef.current) {
            clearTimeout(userScrollingRef.current);
        }
        userScrollingRef.current = window.setTimeout(() => {
            userScrollingRef.current = null;
        }, 150);

        const currentScrollTop = e.currentTarget.scrollTop;
        scrollTopRef.current = currentScrollTop;

        if (!tickingRef.current) {
            window.requestAnimationFrame(() => {
                forceUpdate();
                tickingRef.current = false;
            });
            tickingRef.current = true;
        }
    };
    
    const visibilityKey = Object.values(props.columnVisibility).join('-');
    const visibleColumnCount = React.useMemo(() => Object.values(props.columnVisibility).filter(Boolean).length, [props.columnVisibility]);

    // Virtualization calculations for scroll mode
    let virtualEntries: LogEntry[] = entries;
    let topSpacerHeight = 0;
    let bottomSpacerHeight = 0;
    
    if (viewMode === 'scroll') {
        // --- Smart Data Fetching Logic ---
        if (viewportRef.current && !isBusy) {
            const scrollTop = scrollTopRef.current;
            const rowHeight = getRowHeight(logTableDensity);
            
            const requiredTopIndex = Math.floor(scrollTop / rowHeight);

            const loadedRange = {
                start: entriesOffset,
                end: entriesOffset + entries.length
            };

            const isNearEndOfData = requiredTopIndex > loadedRange.end - (INFINITE_SCROLL_CHUNK_SIZE * 2);

            // A "jump" is when the required view is completely outside our loaded data window.
            const isJump = requiredTopIndex < loadedRange.start || requiredTopIndex > loadedRange.end;

            if (isJump) {
                // We've jumped to a completely different, unloaded area.
                // Fetch a new window of data centered around the required index.
                onJumpToOffset(requiredTopIndex);
            } else if (isNearEndOfData && hasMore) {
                // We are near the end of the loaded data, perform a normal incremental load.
                onLoadMore();
            }
        }

        // --- Virtualization Rendering Logic ---
        if (viewportRef.current) {
            const scrollTop = scrollTopRef.current;
            const rowHeight = getRowHeight(logTableDensity);
            const overscan = 20;

            // Calculate the start index of the items to render, relative to the *full dataset*.
            const absoluteStartIndex = Math.max(0, Math.floor(scrollTop / rowHeight) - overscan);
            
            // Convert the absolute start index to an index relative to our current `entries` window.
            const startIndexInWindow = absoluteStartIndex - entriesOffset;
            
            const visibleItemCount = Math.ceil(viewportRef.current.clientHeight / rowHeight);
            const endIndexInWindow = Math.min(entries.length, startIndexInWindow + visibleItemCount + (overscan * 2));
            
            const clampedStartIndex = Math.max(0, startIndexInWindow);

            virtualEntries = entries.slice(clampedStartIndex, endIndexInWindow);
            
            // The spacers are calculated based on the absolute positions in the full dataset.
            topSpacerHeight = (entriesOffset + clampedStartIndex) * rowHeight;
            const absoluteEndIndex = entriesOffset + endIndexInWindow;
            bottomSpacerHeight = (totalFilteredCount - absoluteEndIndex) * rowHeight;
        }
    }


    return (
        <div className="flex flex-col flex-grow min-h-0 bg-gray-100 dark:bg-gray-900">
            {props.isTimeRangeSelectorVisible && props.overallTimeRange && (
                <div className="flex-shrink-0 p-2 border-b border-gray-200 dark:border-gray-700 bg-white dark:bg-gray-800/50">
                   <TimeRangeSelector 
                    minTime={props.overallTimeRange.min}
                    maxTime={props.overallTimeRange.max}
                    selectedStartTime={new Date(props.appliedFilters.dateFrom + 'T' + props.appliedFilters.timeFrom + 'Z').getTime()}
                    selectedEndTime={new Date(props.appliedFilters.dateTo + 'T' + props.appliedFilters.timeTo + 'Z').getTime()}
                    onRangeChange={props.onTimeRangeSelectorChange}
                    onClear={props.onClearTimeRange}
                    theme={props.theme}
                    pageTimestampRanges={props.pageTimestampRanges}
                    fileTimeRanges={props.fileTimeRanges}
                    logDensity={props.logDensity}
                    overallLogDensity={props.overallLogDensity}
                    datesWithLogs={props.datesWithLogs}
                    viewMode={props.viewMode}
                    onGoToPage={() => {}}
                    onCursorChange={props.onCursorChange}
                    onFileSelect={props.onFileSelect}
                    onDateSelect={props.onDateSelect}
                    viewRange={props.timelineViewRange}
                    onViewRangeChange={props.onTimelineViewRangeChange}
                    onZoomToSelection={props.onTimelineZoomToSelection}
                    onZoomToExtent={props.onTimelineZoomReset}
                    zoomToSelectionEnabled={!!(props.appliedFilters.dateFrom && props.appliedFilters.dateTo)}
                    iconSet={props.iconSet}
                    uiScale={props.uiScale}
                    cursorTime={props.cursorTime}
                    timelineBarVisibility={props.timelineBarVisibility}
                    onTimelineBarVisibilityChange={props.onTimelineBarVisibilityChange}
                   />
                </div>
            )}
            <ActiveFilters appliedFilters={props.appliedFilters} onRemoveFilter={props.onRemoveAppliedFilter} iconSet={props.iconSet} />
            <div className="flex flex-grow min-h-0">
                <aside style={{ width: `${props.panelWidths.filters}px` }} className="flex-shrink-0 bg-gray-50 dark:bg-gray-800 border-r border-gray-200 dark:border-gray-700">
                    <FilterBar {...props} />
                </aside>
                <Splitter onDrag={handleFilterResize} />
                <main className="flex-grow min-h-0 flex flex-col min-w-0">
                    <div className="relative flex-grow">
                        <div className="absolute inset-0 overflow-auto outline-none" ref={viewportRef} tabIndex={-1} onScroll={handleScroll}>
                            <table key={visibilityKey} className="min-w-full table-fixed font-sans">
                                <thead className="sticky top-0 bg-gray-100 dark:bg-gray-800 z-10 shadow-sm">
                                    <tr onContextMenu={(e) => {
                                        e.preventDefault();
                                        setHeaderContextMenu({ x: e.clientX, y: e.clientY });
                                    }}>
                                        {columnVisibility.time && <th style={{width: '12%', minWidth: '170px'}} className={`py-2 ${getCellClass(logTableDensity)} text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider`}>Time</th>}
                                        {columnVisibility.level && <th style={{width: '8%', minWidth: '90px'}} className={`py-2 ${getCellClass(logTableDensity)} text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider`}>Level</th>}
                                        {columnVisibility.sndrtype && <th style={{width: '10%', minWidth: '120px'}} className={`py-2 ${getCellClass(logTableDensity)} text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider`}>Sender Type</th>}
                                        {columnVisibility.sndrname && <th style={{width: '10%', minWidth: '120px'}} className={`py-2 ${getCellClass(logTableDensity)} text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider`}>Sender Name</th>}
                                        {columnVisibility.fileName && <th style={{width: '15%', minWidth: '150px'}} className={`py-2 ${getCellClass(logTableDensity)} text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider`}>Filename</th>}
                                        {columnVisibility.msg && <th style={{width: '45%'}} className={`py-2 ${getCellClass(logTableDensity)} text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider`}>Message</th>}
                                    </tr>
                                </thead>
                                 <tbody className="bg-white dark:bg-gray-900 divide-y divide-gray-200 dark:divide-gray-700">
                                    {topSpacerHeight > 0 && (
                                        <tr><td style={{ height: topSpacerHeight }} colSpan={visibleColumnCount}></td></tr>
                                    )}
                                    {virtualEntries.map(entry => (
                                        <tr key={entry.id}
                                            ref={el => { rowRefs.current.set(entry.id, el); }}
                                            onClick={() => handleRowClick(entry)}
                                            className={`transition-colors duration-100 cursor-pointer ${keyboardSelectedId === entry.id ? 'bg-sky-100 dark:bg-sky-900/50' : 'hover:bg-gray-50 dark:hover:bg-gray-800/50'}`}
                                        >
                                            {columnVisibility.time && <td onContextMenu={(e) => handleContextMenu(e, entry, 'time', entry.time)} style={getStyle('time')} className={`${getRowClass(logTableDensity)} ${getCellClass(logTableDensity)} whitespace-nowrap`}>{entry.time}</td>}
                                            {columnVisibility.level && <td onContextMenu={(e) => handleContextMenu(e, entry, 'level', entry.level)} className={`${getRowClass(logTableDensity)} ${getCellClass(logTableDensity)} whitespace-nowrap`}><span style={getStyle('level')} className={`px-2 py-0.5 rounded-full text-xs font-medium ${getLevelColor(entry.level)}`}>{entry.level}</span></td>}
                                            {columnVisibility.sndrtype && <td onContextMenu={(e) => handleContextMenu(e, entry, 'sndrtype', entry.sndrtype)} style={getStyle('sndrtype')} className={`${getRowClass(logTableDensity)} ${getCellClass(logTableDensity)} whitespace-nowrap truncate`}>{entry.sndrtype}</td>}
                                            {columnVisibility.sndrname && <td onContextMenu={(e) => handleContextMenu(e, entry, 'sndrname', entry.sndrname)} style={getStyle('sndrname')} className={`${getRowClass(logTableDensity)} ${getCellClass(logTableDensity)} whitespace-nowrap truncate`}>{entry.sndrname}</td>}
                                            {columnVisibility.fileName && <td onContextMenu={(e) => handleContextMenu(e, entry, 'fileName', entry.fileName)} style={getStyle('fileName')} className={`${getRowClass(logTableDensity)} ${getCellClass(logTableDensity)} whitespace-nowrap truncate`}>{entry.fileName}</td>}
                                            {columnVisibility.msg && <td onContextMenu={(e) => handleContextMenu(e, entry, 'msg', entry.msg)} style={getStyle('msg')} className={`${getRowClass(logTableDensity)} ${getCellClass(logTableDensity)} whitespace-nowrap truncate`} dangerouslySetInnerHTML={{ __html: highlightText(entry.msg, [appliedFilters.includeMsg], theme) }}></td>}
                                        </tr>
                                    ))}
                                    {bottomSpacerHeight > 0 && (
                                        <tr><td style={{ height: bottomSpacerHeight }} colSpan={visibleColumnCount}></td></tr>
                                    )}
                                 </tbody>
                            </table>
                            {isBusy && <div className="p-4 text-center">Loading...</div>}
                        </div>
                    </div>
                </main>
                {props.isDetailPanelVisible && (
                    <>
                        <Splitter onDrag={handleDetailsResize} />
                        <LogDetailPanel
                            entry={selectedEntry}
                            onClose={() => props.onDetailPanelVisibilityChange(false)}
                            width={props.panelWidths.details}
                            highlightTerms={[appliedFilters.includeMsg]}
                            theme={props.theme}
                            onApplyFilter={props.onApplyFilter}
                            columnStyles={props.columnStyles}
                            iconSet={props.iconSet}
                        />
                    </>
                )}
            </div>
             {contextMenuState && <ContextMenu {...contextMenuState} onClose={() => setContextMenuState(null)} onFilter={props.onContextMenuFilter} iconSet={props.iconSet} contextKey={contextMenuState.key} contextValue={contextMenuState.value} />}
             {headerContextMenu && (
                <ColumnVisibilityMenu
                    x={headerContextMenu.x}
                    y={headerContextMenu.y}
                    visibility={props.columnVisibility}
                    onChange={props.onColumnVisibilityChange}
                    onClose={() => setHeaderContextMenu(null)}
                />
             )}
        </div>
    );
};