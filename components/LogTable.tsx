import React from 'react';
import { useVirtualizer } from '@tanstack/react-virtual';
import { LogEntry, FilterState, PageTimestampRange, ColumnVisibilityState, ColumnStyles, ColumnKey, PanelWidths, ConsoleMessage, OverallTimeRange, FileTimeRange, LogDensityPointByLevel, IconSet, LogTableDensity, Theme, TimelineBarVisibility } from '../types';
import { FilterBar } from './FilterBar';
import { LogDetailPanel } from './LogDetailPanel';
import { TimeRangeSelector } from './TimeRangeSelector';
import { ActiveFilters } from './ActiveFilters';
import { ContextMenu } from './ContextMenu';
import { Splitter } from './Splitter';
import { ColumnVisibilityMenu } from './ColumnVisibilityMenu';
import { useData } from '../contexts/DataContext';
import { useAnalytics } from '../contexts/AnalyticsContext';
import { useSettings } from '../contexts/SettingsContext';
import { useUI } from '../contexts/UIContext';
import { useConsole } from '../contexts/ConsoleContext';
import { ROW_HEIGHT_COMPACT, ROW_HEIGHT_NORMAL, ROW_HEIGHT_COMFORTABLE, COLUMN_DEFINITIONS } from '../constants';
import { useTableNavigation } from '../hooks/useTableNavigation';
import { usePanelResizer } from '../hooks/usePanelResizer';
import { LogTableHeader } from './LogTableHeader';
import { LogTableRow } from './LogTableRow';
import { EmptyState } from './EmptyState';
import { parseFilterDateTime } from '../utils';



type ContextMenuState = { x: number; y: number; entry: LogEntry; value: string; key: ColumnKey } | null;

interface LogTableProps { }

export const LogTable: React.FC<LogTableProps> = () => {
    // Context Hooks
    const {
        filteredEntries: entries,
        onLoadMore, hasMoreLogs: hasMore, onLoadPrev, hasPrevLogs,
        appliedFilters, isInitialLoad,
        keyboardSelectedId, setKeyboardSelectedId, jumpToEntryId,
        handleApplyDetailFilter: onApplyFilter,
        handleContextMenuFilter: onContextMenuFilter,
        handleRemoveAppliedFilter: onRemoveAppliedFilter,
        handleTimeRangeSelect,
        handleCursorChange: onCursorChange,
        handleFileSelect: onFileSelect,
        handleDateSelect: onDateSelect,
        handleClearTimeRange: onClearTimeRange,
        pageTimestampRanges, formFilters
    } = useData();

    const {
        fileTimeRanges, logDensity, overallLogDensity, datesWithLogs, overallLogTimeRange,
    } = useAnalytics();

    const {
        theme, columnVisibility, columnWidths, columnStyles, panelWidths, isDetailPanelVisible,
        logTableDensity, iconSet, uiScale, timelineBarVisibility, zoomToSelectionEnabled, isTimeRangeSelectorVisible,
        onColumnVisibilityChange, onColumnWidthsChange, onPanelWidthsChange, onDetailPanelVisibilityChange,
        onTimelineBarVisibilityChange
    } = useSettings();

    const { isBusy } = useUI();
    // cursorTime is not in standard contexts, but TimelineContext usually has viewRange.
    // However, `props.cursorTime` might be specific state. 
    // Checking App.tsx: line 37 [cursorTime, setCursorTime] = useState<number | null>(null);
    // AND passed to handleCursorChange (which updates DataContext?).
    // DataContext has handleCursorChange: (time) => setCursorTime(time)??
    // DataContext (line 549) handleCursorChange.
    // Wait, DataContext.tsx doesn't seem to manage cursorTime state in the viewed snippets.
    // It has `handleCursorChange` in `value`.
    // I need to check if DataContext manages `cursorTime` or if it was expected to be passed.
    // If not, I'll use local state or add to a context?
    // Let's assume DataContext logic handles it or I check `DataContext` again.
    // Wait, I can't leave it undefined.
    // Let's assume for now local state in App was used for sync?
    // LogTable sets it via `onCursorChange`.
    // TimeRangeSelector reads it.
    // If I remove props, who holds it?
    // `useData` likely should hold `cursorTime` for sync across components if needed.
    // Or I keep it local to LogTable if nobody else uses it (except TimeRangeSelector which is child).
    // Dashboard doesn't use cursor.
    // So Local State in LogTable is fine if only TimeRangeSelector uses it.

    // BUT `App.tsx` had it.
    // Let's verify `DataContext` support for cursorTime.

    // Missing: cursorTime.

    const [cursorTime, setCursorTime] = React.useState<number | null>(null);
    const handleCursorChange = (t: number) => {
        setCursorTime(t);
        // Also navigate to the log entry at this cursor time
        onCursorChange(t);
    };

    const [selectedEntry, setSelectedEntry] = React.useState<LogEntry | null>(null);
    const [contextMenuState, setContextMenuState] = React.useState<ContextMenuState>(null);
    const [headerContextMenu, setHeaderContextMenu] = React.useState<{ x: number, y: number } | null>(null);
    const tableContainerRef = React.useRef<HTMLDivElement>(null);
    const pendingLoadRef = React.useRef(false);
    const previousScrollHeightRef = React.useRef(0);

    // UseData hook might still be needed for other things, but if setLogTableViewportHeight was the only thing, we might check.
    // However, for now, let's just remove the effect that calls it.

    const columns = React.useMemo(
        () => COLUMN_DEFINITIONS.filter(column => columnVisibility[column.key]),
        [columnVisibility]
    );

    const gridTemplateColumns = React.useMemo(
        () => columns.length
            ? columns.map(column => {
                const userWidth = columnWidths[column.key];
                if (userWidth) {
                    return `${userWidth}px`;  // User-defined width takes priority
                }
                return column.flex === 0
                    ? `${column.minWidth}px`  // Fixed width for flex=0
                    : `minmax(${column.minWidth}px, ${column.flex}fr)`;  // Flexible for flex>0
            }).join(' ')
            : '1fr',
        [columns, columnWidths]
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
            if (pendingLoadRef.current || isBusy) {
                return;
            }

            const { scrollTop, clientHeight, scrollHeight } = container;

            if (hasMore && scrollHeight - (scrollTop + clientHeight) <= threshold) {
                pendingLoadRef.current = true;
                onLoadMore();
            } else if (hasPrevLogs && scrollTop <= 50) {
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
            // Sync cursor position with selected entry's timestamp
            const entryTime = new Date(entry.time + 'Z').getTime();
            if (!isNaN(entryTime)) {
                setCursorTime(entryTime);
            }
            rowVirtualizer.scrollToIndex(index, { align: 'auto' });
        } else {
            setSelectedEntry(null);
        }
    }, [keyboardSelectedId, entries, rowVirtualizer]);

    const handleRowClick = React.useCallback((entry: LogEntry) => {
        setSelectedEntry(entry);
        setKeyboardSelectedId(entry.id);
        // Sync cursor position with clicked entry's timestamp
        const entryTime = new Date(entry.time + 'Z').getTime();
        if (!isNaN(entryTime)) {
            setCursorTime(entryTime);
        }
        tableContainerRef.current?.focus({ preventScroll: true });
    }, [setKeyboardSelectedId]);

    // Use extracted hooks
    useTableNavigation({
        tableContainerRef,
        entries,
        keyboardSelectedId,
        setKeyboardSelectedId,
        rowHeight,
        hasMore,
        hasPrevLogs,
        onLoadMore,
        onLoadPrev,
        isBusy
    });

    const { handleFilterResize, handleDetailsResize } = usePanelResizer(panelWidths, onPanelWidthsChange);

    const handleContextMenu = React.useCallback((e: React.MouseEvent, entry: LogEntry, key: ColumnKey, value: string) => {
        e.preventDefault();
        setContextMenuState({ x: e.clientX, y: e.clientY, entry, key, value });
    }, []);

    const handleColumnResize = React.useCallback((key: ColumnKey, width: number) => {
        onColumnWidthsChange({ ...columnWidths, [key]: width });
    }, [columnWidths, onColumnWidthsChange]);



    return (
        <div className="flex flex-col flex-grow min-h-0 bg-gray-100 dark:bg-gray-900">
            {isTimeRangeSelectorVisible && overallLogTimeRange && (
                <div className="flex-shrink-0 p-2 border-b border-gray-200 dark:border-gray-700 bg-white dark:bg-gray-800/50">
                    <TimeRangeSelector
                        minTime={overallLogTimeRange.min}
                        maxTime={overallLogTimeRange.max}
                        selectedStartTime={parseFilterDateTime(formFilters.dateFrom, formFilters.timeFrom)}
                        selectedEndTime={parseFilterDateTime(formFilters.dateTo, formFilters.timeTo)}
                        onRangeChange={handleTimeRangeSelect}
                        onClear={onClearTimeRange}
                        theme={theme}
                        pageTimestampRanges={pageTimestampRanges}
                        fileTimeRanges={fileTimeRanges}
                        logDensity={logDensity}
                        overallLogDensity={overallLogDensity}
                        datesWithLogs={datesWithLogs}
                        onGoToPage={() => { }}
                        onCursorChange={handleCursorChange}
                        onFileSelect={onFileSelect}
                        onDateSelect={onDateSelect}
                        zoomToSelectionEnabled={zoomToSelectionEnabled}
                        iconSet={iconSet}
                        uiScale={uiScale}
                        cursorTime={cursorTime}
                        timelineBarVisibility={timelineBarVisibility}
                        onTimelineBarVisibilityChange={onTimelineBarVisibilityChange}
                    />
                </div>
            )}
            <ActiveFilters appliedFilters={appliedFilters} onRemoveFilter={onRemoveAppliedFilter} iconSet={iconSet} />
            <div className="flex flex-grow min-h-0">
                <aside style={{ width: `${panelWidths.filters}px` }} className="flex-shrink-0 bg-gray-50 dark:bg-gray-800 border-r border-gray-200 dark:border-gray-700">
                    <FilterBar />
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
                                onColumnResize={handleColumnResize}
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
                                            isOdd={virtualRow.index % 2 === 1}
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

                {isDetailPanelVisible && (
                    <>
                        <Splitter onDrag={handleDetailsResize} />
                        <LogDetailPanel
                            entry={selectedEntry}
                            onClose={() => onDetailPanelVisibilityChange(false)}
                            width={panelWidths.details}
                            highlightTerms={[appliedFilters.includeMsg]}
                            theme={theme}
                            onApplyFilter={onApplyFilter}
                            columnStyles={columnStyles}
                            iconSet={iconSet}
                        />
                    </>
                )}
            </div>
            {contextMenuState && <ContextMenu {...contextMenuState} onClose={() => setContextMenuState(null)} onFilter={onContextMenuFilter} iconSet={iconSet} contextKey={contextMenuState.key} contextValue={contextMenuState.value} />}
            {
                headerContextMenu && (
                    <ColumnVisibilityMenu
                        x={headerContextMenu.x}
                        y={headerContextMenu.y}
                        visibility={columnVisibility}
                        onChange={onColumnVisibilityChange}
                        onClose={() => setHeaderContextMenu(null)}
                    />
                )
            }
        </div >
    );
};
