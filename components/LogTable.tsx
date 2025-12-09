import React from 'react';
import { useVirtualizer } from '@tanstack/react-virtual';
import { LogEntry, FilterState, PageTimestampRange, ColumnVisibilityState, ColumnStyles, ColumnKey, PanelWidths, ViewMode, ConsoleMessage, OverallTimeRange, FileTimeRange, LogDensityPointByLevel, IconSet, LogTableDensity, Theme, TimelineBarVisibility } from '../types';
import { FilterBar } from './FilterBar';
import { LogDetailPanel } from './LogDetailPanel';
import { TimeRangeSelector } from './TimeRangeSelector';
import { ActiveFilters } from './ActiveFilters';
import { ContextMenu } from './ContextMenu';
import { Splitter } from './Splitter';
import { ColumnVisibilityMenu } from './ColumnVisibilityMenu';
import { useData } from '../contexts/DataContext';
import { ROW_HEIGHT_COMPACT, ROW_HEIGHT_NORMAL, ROW_HEIGHT_COMFORTABLE, COLUMN_DEFINITIONS } from '../constants';
import { LogTableHeader } from './LogTableHeader';
import { LogTableRow } from './LogTableRow';
import { EmptyState } from './EmptyState';

// Used in constants now, but kept here if dynamic density logic remains local or moved to utils
// Actually density heights are simple constants, using what I defined in tableUtils or defining locally if needed.
// Wait, I didn't export row heights in tableUtils earlier. I should assign them here or update tableUtils.
// Checked tableUtils: I didn't export ROW_HEIGHT_*. I used getRowClass etc.
// I'll define heights locally for now to match exactly or create them.
// Let's stick to defining rowHeight memo logic as is.

type ContextMenuState = { x: number; y: number; entry: LogEntry; value: string; key: ColumnKey } | null;

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
    onLoadPrev: () => void;
    hasPrevLogs: boolean;
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
    const previousScrollHeightRef = React.useRef(0);

    const panelWidthsRef = React.useRef(props.panelWidths);
    panelWidthsRef.current = props.panelWidths;

    const {
        entries,
        viewMode,
        onLoadMore,
        hasMore,
        onLoadPrev,
        hasPrevLogs,
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

    const rowHeight = React.useMemo(() => {
        switch (logTableDensity) {
            case 'compact': return 28;
            case 'normal': return 36;
            case 'comfortable': return 44;
            default: return 36;
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

    // Force remeasure all items when row height changes (density change)
    // Use useLayoutEffect to measure synchronously before paint
    React.useLayoutEffect(() => {
        rowVirtualizer.measure();

        // Handle scroll position restoration for bidirectional scrolling
        let restoredScroll = false;
        if (previousScrollHeightRef.current > 0 && tableContainerRef.current) {
            const container = tableContainerRef.current;
            const newScrollHeight = container.scrollHeight;
            const diff = newScrollHeight - previousScrollHeightRef.current;

            if (diff > 0) {
                container.scrollTop += diff;
                restoredScroll = true;
            }
            previousScrollHeightRef.current = 0;
        }

        // After remeasure, scroll to keep selected entry in view
        // Only auto-scroll if we didn't just restore position, to avoid fighting
        if (keyboardSelectedId !== null && !restoredScroll) {
            const index = entries.findIndex(e => e.id === keyboardSelectedId);
            if (index !== -1) {
                rowVirtualizer.scrollToIndex(index, { align: 'auto' });
            }
        }
    }, [rowHeight, entries]); // Added entries dependency to trigger recalculation on data change

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

            if (hasMore && scrollHeight - (scrollTop + clientHeight) <= threshold) {
                pendingLoadRef.current = true;
                onLoadMore();
            } else if (hasPrevLogs && scrollTop <= 50) { // Small threshold for top to detect intention
                pendingLoadRef.current = true;
                previousScrollHeightRef.current = scrollHeight;
                onLoadPrev();
            }
        };

        container.addEventListener('scroll', maybeLoadMore);
        maybeLoadMore();

        return () => {
            container.removeEventListener('scroll', maybeLoadMore);
        };
    }, [estimateRowHeight, hasMore, hasPrevLogs, isBusy, onLoadMore, onLoadPrev]);

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

    const handleRowClick = React.useCallback((entry: LogEntry) => {
        setSelectedEntry(entry);
        setKeyboardSelectedId(entry.id);
        if (!props.isDetailPanelVisible) {
            props.onDetailPanelVisibilityChange(true);
        }
        tableContainerRef.current?.focus({ preventScroll: true });
    }, [props.isDetailPanelVisible, props.onDetailPanelVisibilityChange, setKeyboardSelectedId]);

    // Keyboard navigation effect
    React.useEffect(() => {
        const container = tableContainerRef.current;
        if (!container) return;

        const handleKeyDown = (e: KeyboardEvent) => {
            const navKeys = ['ArrowUp', 'ArrowDown', 'PageUp', 'PageDown', 'Home', 'End'];
            if (!navKeys.includes(e.key)) {
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

            // Calculate visible rows per page based on container height
            const visibleRows = Math.max(1, Math.floor(container.clientHeight / rowHeight) - 1);

            let nextIndex = currentIndex;
            switch (e.key) {
                case 'ArrowDown':
                    nextIndex = currentIndex + 1;
                    break;
                case 'ArrowUp':
                    nextIndex = currentIndex - 1;
                    break;
                case 'PageDown':
                    nextIndex = Math.min(currentIndex + visibleRows, entries.length - 1);
                    break;
                case 'PageUp':
                    nextIndex = Math.max(currentIndex - visibleRows, 0);
                    break;
                case 'Home':
                    nextIndex = 0;
                    break;
                case 'End':
                    nextIndex = entries.length - 1;
                    // If we're at the end and there's more data, load it
                    if (viewMode === 'scroll' && hasMore && !isBusy) {
                        onLoadMore();
                    }
                    break;
            }

            if (nextIndex >= 0 && nextIndex < entries.length) {
                setKeyboardSelectedId(entries[nextIndex].id);

                // Check if we're near the end and should trigger autoload
                if (viewMode === 'scroll' && hasMore && !isBusy) {
                    const remainingEntries = entries.length - nextIndex;
                    if (remainingEntries <= visibleRows) {
                        onLoadMore();
                    }
                }
            } else if (viewMode === 'scroll' && e.key === 'ArrowDown' && nextIndex >= entries.length && hasMore) {
                onLoadMore();
            }
        };

        container.addEventListener('keydown', handleKeyDown);
        return () => container.removeEventListener('keydown', handleKeyDown);

    }, [entries, keyboardSelectedId, setKeyboardSelectedId, hasMore, onLoadMore, viewMode, rowHeight, isBusy]);

    const handleContextMenu = React.useCallback((e: React.MouseEvent, entry: LogEntry, key: ColumnKey, value: string) => {
        e.preventDefault();
        setContextMenuState({ x: e.clientX, y: e.clientY, entry, key, value });
    }, []);

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
                        onGoToPage={() => { }}
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
                            <LogTableHeader
                                columns={columns}
                                logTableDensity={logTableDensity}
                                gridTemplateColumns={gridTemplateColumns}
                                onContextMenu={(e) => {
                                    e.preventDefault();
                                    setHeaderContextMenu({ x: e.clientX, y: e.clientY });
                                }}
                            />
                            <div
                                className="relative bg-white dark:bg-gray-900"
                                style={{ height: `${rowVirtualizer.getTotalSize()}px` }}
                            >
                                {virtualRows.map((virtualRow) => {
                                    const entry = entries[virtualRow.index];
                                    if (!entry) return null;

                                    return (
                                        <LogTableRow
                                            key={entry.id}
                                            entry={entry}
                                            columns={columns}
                                            logTableDensity={logTableDensity}
                                            theme={theme}
                                            isSelected={keyboardSelectedId === entry.id}
                                            style={{
                                                position: 'absolute',
                                                top: 0,
                                                left: 0,
                                                right: 0,
                                                transform: `translateY(${virtualRow.start}px)`,
                                                height: `${virtualRow.size}px`,
                                                gridTemplateColumns,
                                            }}
                                            onRowClick={handleRowClick}
                                            onContextMenu={handleContextMenu}
                                            appliedFilters={appliedFilters}
                                            columnStyles={columnStyles}
                                        />
                                    );
                                })}
                                <EmptyState
                                    isBusy={isBusy}
                                    rowVirtualizerTotalSize={rowVirtualizer.getTotalSize()}
                                    rowHeight={rowHeight}
                                    gridTemplateColumns={gridTemplateColumns}
                                    logTableDensity={logTableDensity}
                                    entriesCount={entries.length}
                                />
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
