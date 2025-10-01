import React from 'react';
import { useVirtualizer } from '@tanstack/react-virtual';
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
import { useData } from '../contexts/DataContext';

type ContextMenuState = { x: number; y: number; entry: LogEntry; value: string; key: ColumnKey } | null;

type ColumnDefinition = {
    key: ColumnKey;
    label: string;
    minWidth: number;
    flex: number;
};

const COLUMN_DEFINITIONS: ColumnDefinition[] = [
    { key: 'time', label: 'Time', minWidth: 170, flex: 1.3 },
    { key: 'level', label: 'Level', minWidth: 90, flex: 0.8 },
    { key: 'sndrtype', label: 'Sender Type', minWidth: 120, flex: 1.1 },
    { key: 'sndrname', label: 'Sender Name', minWidth: 120, flex: 1.2 },
    { key: 'fileName', label: 'Filename', minWidth: 150, flex: 1.4 },
    { key: 'msg', label: 'Message', minWidth: 240, flex: 3.0 },
];

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

export const LogTable: React.FC<LogTableProps> = (props) => {
    const [selectedEntry, setSelectedEntry] = React.useState<LogEntry | null>(null);
    const [contextMenuState, setContextMenuState] = React.useState<ContextMenuState>(null);
    const [headerContextMenu, setHeaderContextMenu] = React.useState<{ x: number, y: number } | null>(null);
    const tableContainerRef = React.useRef<HTMLDivElement>(null);
    const pendingLoadRef = React.useRef(false);
    
    const panelWidthsRef = React.useRef(props.panelWidths);
    panelWidthsRef.current = props.panelWidths;

    const {
        entries,
        viewMode,
        onLoadMore,
        hasMore,
        isBusy,
        columnVisibility,
        columnStyles,
        logTableDensity,
        keyboardSelectedId,
        setKeyboardSelectedId,
        appliedFilters,
        theme,
        jumpToEntryId,
        uiScale
    } = props;

    const { setLogTableViewportHeight } = useData();

    const columns = React.useMemo(
        () => COLUMN_DEFINITIONS.filter(column => columnVisibility[column.key]),
        [columnVisibility]
    );

    const gridTemplateColumns = React.useMemo(
        () => columns.length
            ? columns.map(column => `minmax(${column.minWidth}px, ${column.flex}fr)`).join(' ')
            : '1fr',
        [columns]
    );

    const visibleColumnCount = columns.length || 1;
    
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
    };

    const rowHeight = React.useMemo(() => {
        switch (logTableDensity) {
            case 'compact':
                return 28;
            case 'normal':
                return 36;
            case 'comfortable':
                return 44;
            default:
                return 36;
        }
    }, [logTableDensity]);

    const estimateRowHeight = React.useCallback(() => rowHeight, [rowHeight]);

    const rowVirtualizer = useVirtualizer({
        count: entries.length,
        getScrollElement: () => tableContainerRef.current,
        estimateSize: estimateRowHeight,
        overscan: 6,
    });

    const virtualRows = rowVirtualizer.getVirtualItems();

    React.useEffect(() => {
        if (viewMode !== 'scroll') {
            setLogTableViewportHeight(null);
            return;
        }

        const container = tableContainerRef.current;
        if (!container) {
            setLogTableViewportHeight(null);
            return;
        }

        const updateHeight = () => setLogTableViewportHeight(container.clientHeight);
        updateHeight();

        let resizeObserver: ResizeObserver | null = null;
        let resizeListener: (() => void) | null = null;

        if (typeof ResizeObserver !== 'undefined') {
            resizeObserver = new ResizeObserver(entries => {
                for (const entry of entries) {
                    if (entry.target === container) {
                        setLogTableViewportHeight(entry.contentRect.height);
                    }
                }
            });
            resizeObserver.observe(container);
        } else {
            resizeListener = () => updateHeight();
            window.addEventListener('resize', resizeListener);
        }

        return () => {
            if (resizeObserver) {
                resizeObserver.disconnect();
            }
            if (resizeListener) {
                window.removeEventListener('resize', resizeListener);
            }
            setLogTableViewportHeight(null);
        };
    }, [setLogTableViewportHeight, viewMode]);

    React.useEffect(() => {
        if (viewMode !== 'scroll') {
            return;
        }

        const container = tableContainerRef.current;
        if (!container) {
            return;
        }

        setLogTableViewportHeight(container.clientHeight);
    }, [logTableDensity, uiScale, setLogTableViewportHeight, viewMode]);

    React.useEffect(() => {
        if (!isBusy) {
            pendingLoadRef.current = false;
        }
    }, [isBusy]);

    React.useEffect(() => {
        const container = tableContainerRef.current;
        if (!container) {
            return;
        }

        const threshold = Math.max(estimateRowHeight(), 200);

        const maybeLoadMore = () => {
            if (pendingLoadRef.current || isBusy || !hasMore) {
                return;
            }

            const { scrollTop, clientHeight, scrollHeight } = container;
            if (scrollHeight - (scrollTop + clientHeight) <= threshold) {
                pendingLoadRef.current = true;
                onLoadMore();
            }
        };

        container.addEventListener('scroll', maybeLoadMore);
        maybeLoadMore();

        return () => {
            container.removeEventListener('scroll', maybeLoadMore);
        };
    }, [estimateRowHeight, hasMore, isBusy, onLoadMore]);

    // This effect ensures the details panel stays in sync with keyboard navigation.
    React.useEffect(() => {
        if (keyboardSelectedId === null) {
            setSelectedEntry(null);
            return;
        }

        const index = entries.findIndex(e => e.id === keyboardSelectedId);
        if (index !== -1) {
            const entry = entries[index];
            setSelectedEntry(entry);
            rowVirtualizer.scrollToIndex(index, { align: 'auto' });
        } else {
            setSelectedEntry(null);
        }
    }, [keyboardSelectedId, entries, rowVirtualizer]);

    const handleRowClick = (entry: LogEntry) => {
        setSelectedEntry(entry);
        setKeyboardSelectedId(entry.id);
        if (!props.isDetailPanelVisible) {
            props.onDetailPanelVisibilityChange(true);
        }
        tableContainerRef.current?.focus({ preventScroll: true });
    };

    // Keyboard navigation effect
    React.useEffect(() => {
        const container = tableContainerRef.current;
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

    const handleContextMenu = (e: React.MouseEvent, entry: LogEntry, key: ColumnKey, value: string) => {
        e.preventDefault();
        setContextMenuState({ x: e.clientX, y: e.clientY, entry, key, value });
    };

    const renderCell = (column: ColumnDefinition, entry: LogEntry) => {
        const baseClasses = `${getRowClass(logTableDensity)} ${getCellClass(logTableDensity)}`;

        switch (column.key) {
            case 'time':
                return (
                    <div
                        key="time"
                        onContextMenu={(e) => handleContextMenu(e, entry, 'time', entry.time)}
                        style={getStyle('time')}
                        className={`${baseClasses} whitespace-nowrap`}
                    >
                        {entry.time}
                    </div>
                );
            case 'level':
                return (
                    <div
                        key="level"
                        onContextMenu={(e) => handleContextMenu(e, entry, 'level', entry.level)}
                        className={`${baseClasses} whitespace-nowrap`}
                    >
                        <span
                            style={getStyle('level')}
                            className={`px-2 py-0.5 rounded-full text-xs font-medium ${getLevelColor(entry.level)}`}
                        >
                            {entry.level}
                        </span>
                    </div>
                );
            case 'sndrtype':
                return (
                    <div
                        key="sndrtype"
                        onContextMenu={(e) => handleContextMenu(e, entry, 'sndrtype', entry.sndrtype)}
                        style={getStyle('sndrtype')}
                        className={`${baseClasses} whitespace-nowrap truncate`}
                    >
                        {entry.sndrtype}
                    </div>
                );
            case 'sndrname':
                return (
                    <div
                        key="sndrname"
                        onContextMenu={(e) => handleContextMenu(e, entry, 'sndrname', entry.sndrname)}
                        style={getStyle('sndrname')}
                        className={`${baseClasses} whitespace-nowrap truncate`}
                    >
                        {entry.sndrname}
                    </div>
                );
            case 'fileName':
                return (
                    <div
                        key="fileName"
                        onContextMenu={(e) => handleContextMenu(e, entry, 'fileName', entry.fileName)}
                        style={getStyle('fileName')}
                        className={`${baseClasses} whitespace-nowrap truncate`}
                    >
                        {entry.fileName}
                    </div>
                );
            case 'msg':
            default:
                return (
                    <div
                        key="msg"
                        onContextMenu={(e) => handleContextMenu(e, entry, 'msg', entry.msg)}
                        style={getStyle('msg')}
                        className={`${baseClasses} whitespace-nowrap truncate`}
                        dangerouslySetInnerHTML={{ __html: highlightText(entry.msg, [appliedFilters.includeMsg], theme) }}
                    />
                );
        }
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
                    uiScale={uiScale}
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
                    <div className="overflow-auto outline-none flex-grow" ref={tableContainerRef} tabIndex={-1}>
                        <div className="relative min-w-full">
                            <div
                                className="sticky top-0 z-10 shadow-sm"
                                onContextMenu={(e) => {
                                    e.preventDefault();
                                    setHeaderContextMenu({ x: e.clientX, y: e.clientY });
                                }}
                            >
                                <div
                                    className="grid font-sans bg-gray-100 dark:bg-gray-800 border-b border-gray-200 dark:border-gray-700 items-center"
                                    style={{ gridTemplateColumns }}
                                >
                                    {columns.length > 0 ? (
                                        columns.map((column) => (
                                            <div
                                                key={column.key}
                                                className={`py-2 ${getCellClass(logTableDensity)} text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider`}
                                            >
                                                {column.label}
                                            </div>
                                        ))
                                    ) : (
                                        <div className={`py-2 ${getCellClass(logTableDensity)} text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider`}>
                                            No columns selected
                                        </div>
                                    )}
                                </div>
                            </div>
                            <div
                                className="relative bg-white dark:bg-gray-900"
                                style={{ height: `${rowVirtualizer.getTotalSize() + (isBusy ? rowHeight : 0)}px` }}
                            >
                                {virtualRows.map((virtualRow) => {
                                    const entry = entries[virtualRow.index];
                                    if (!entry) return null;

                                    return (
                                        <div
                                            key={entry.id}
                                            onClick={() => handleRowClick(entry)}
                                            className={`grid font-sans items-center transition-colors duration-100 cursor-pointer border-b border-gray-200 dark:border-gray-700 ${keyboardSelectedId === entry.id ? 'bg-sky-100 dark:bg-sky-900/50' : 'hover:bg-gray-50 dark:hover:bg-gray-800/50'}`}
                                            style={{
                                                position: 'absolute',
                                                top: 0,
                                                left: 0,
                                                right: 0,
                                                transform: `translateY(${virtualRow.start}px)`,
                                                height: `${virtualRow.size}px`,
                                                gridTemplateColumns,
                                            }}
                                        >
                                            {columns.map((column) => renderCell(column, entry))}
                                        </div>
                                    );
                                })}
                                {isBusy && (
                                    <div
                                        key="loading"
                                        className="grid font-sans items-center border-t border-gray-200 dark:border-gray-700"
                                        style={{
                                            position: 'absolute',
                                            top: `${rowVirtualizer.getTotalSize()}px`,
                                            left: 0,
                                            right: 0,
                                            height: `${rowHeight}px`,
                                            gridTemplateColumns,
                                        }}
                                    >
                                        <div
                                            className={`${getRowClass(logTableDensity)} ${getCellClass(logTableDensity)} text-center text-sm text-gray-500 dark:text-gray-400`}
                                            style={{ gridColumn: '1 / -1' }}
                                        >
                                            Loading...
                                        </div>
                                    </div>
                                )}
                            </div>
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