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
        pageTimestampRanges, formFilters, setFormFilters
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

    // const [selectedEntry, setSelectedEntry] = React.useState<LogEntry | null>(null); // Replaced by multi-select
    const [selectedIds, setSelectedIds] = React.useState<Set<number>>(new Set());
    // Keep track of the last clicked ID for range selection
    const lastClickedIdRef = React.useRef<number | null>(null);

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

    const handleRowClick = React.useCallback((entry: LogEntry, event: React.MouseEvent) => {
        const id = entry.id;
        let newSelectedIds = new Set(selectedIds);

        if (event.ctrlKey || event.metaKey) {
            // Toggle selection
            if (newSelectedIds.has(id)) {
                newSelectedIds.delete(id);
            } else {
                newSelectedIds.add(id);
            }
            lastClickedIdRef.current = id;
        } else if (event.shiftKey && lastClickedIdRef.current !== null) {
            // Range selection
            const startId = lastClickedIdRef.current;
            const endId = id;

            const startIndex = entries.findIndex(e => e.id === startId);
            const endIndex = entries.findIndex(e => e.id === endId);

            if (startIndex !== -1 && endIndex !== -1) {
                const min = Math.min(startIndex, endIndex);
                const max = Math.max(startIndex, endIndex);

                // Add range to existing selection (standard behavior) or replace? 
                // Standard file explorer replaces unless Ctrl held too, but pure shift usually extends from anchor.
                // We'll reset and select range for simplicity or just add?
                // Standard behavior: Clear others, select range from anchor to current.
                newSelectedIds.clear();
                for (let i = min; i <= max; i++) {
                    newSelectedIds.add(entries[i].id);
                }
            }
        } else {
            // Single selection
            newSelectedIds.clear();
            newSelectedIds.add(id);
            lastClickedIdRef.current = id;
        }

        setSelectedIds(newSelectedIds);
        setKeyboardSelectedId(id);

        // Sync cursor position with clicked entry's timestamp (only if single log selected effectively)
        const entryTime = new Date(entry.time + 'Z').getTime();
        if (!isNaN(entryTime)) {
            setCursorTime(entryTime);
        }
        tableContainerRef.current?.focus({ preventScroll: true });
    }, [setKeyboardSelectedId, selectedIds, entries]);

    // Update selection when keyboard navigation changes


    // Use extracted hooks
    const handleNavigate = React.useCallback((id: number, event: KeyboardEvent) => {
        setKeyboardSelectedId(id);

        if (event.shiftKey && lastClickedIdRef.current !== null) {
            // Keyboard range selection
            const startId = lastClickedIdRef.current;
            const endId = id;

            const startIndex = entries.findIndex(e => e.id === startId);
            const endIndex = entries.findIndex(e => e.id === endId);

            if (startIndex !== -1 && endIndex !== -1) {
                const min = Math.min(startIndex, endIndex);
                const max = Math.max(startIndex, endIndex);

                const newSelectedIds = new Set<number>();
                for (let i = min; i <= max; i++) {
                    newSelectedIds.add(entries[i].id);
                }
                setSelectedIds(newSelectedIds);
            }
        } else {
            // Keyboard single selection
            setSelectedIds(new Set([id]));
            lastClickedIdRef.current = id;

            // Sync cursor time (if useful for single select navigation)
            const entry = entries.find(e => e.id === id);
            if (entry) {
                const entryTime = new Date(entry.time + 'Z').getTime();
                if (!isNaN(entryTime)) {
                    setCursorTime(entryTime);
                }
            }
        }
    }, [entries, lastClickedIdRef]);

    // Use extracted hooks
    useTableNavigation({
        tableContainerRef,
        entries,
        keyboardSelectedId,
        onNavigate: handleNavigate,
        rowHeight,
        hasMore,
        hasPrevLogs,
        onLoadMore,
        onLoadPrev,
        isBusy
    });

    const { handleFilterResize, handleDetailsResize } = usePanelResizer(panelWidths, onPanelWidthsChange);

    const handleSetTimeStart = React.useCallback(() => {
        if (!contextMenuState?.entry) return;
        const entryTime = new Date(contextMenuState.entry.time + 'Z');
        if (isNaN(entryTime.getTime())) return;

        const dateStr = entryTime.toISOString().split('T')[0];
        // Includes milliseconds from ISO string (12 chars: HH:MM:SS.mmm)
        const timeStr = entryTime.toISOString().split('T')[1].substring(0, 12);

        setFormFilters((prev: FilterState) => ({
            ...prev,
            dateFrom: dateStr,
            timeFrom: timeStr
        }));
    }, [contextMenuState, setFormFilters]);

    const handleSetTimeEnd = React.useCallback(() => {
        if (!contextMenuState?.entry) return;
        const entryTime = new Date(contextMenuState.entry.time + 'Z');
        if (isNaN(entryTime.getTime())) return;

        const dateStr = entryTime.toISOString().split('T')[0];
        const timeStr = entryTime.toISOString().split('T')[1].substring(0, 12);

        setFormFilters((prev: FilterState) => ({
            ...prev,
            dateTo: dateStr,
            timeTo: timeStr
        }));
    }, [contextMenuState, setFormFilters]);

    const handleSetTimeRange = React.useCallback(() => {
        if (selectedEntries.length < 1) return;

        const times = selectedEntries.map(e => new Date(e.time).getTime()).filter(t => !isNaN(t)).sort((a, b) => a - b);
        if (times.length === 0) return;

        const startTime = new Date(times[0]);
        const endTime = new Date(times[times.length - 1]);

        const startDateStr = startTime.toISOString().split('T')[0];
        const startTimeStr = startTime.toISOString().split('T')[1].slice(0, 12);
        const endDateStr = endTime.toISOString().split('T')[0];
        const endTimeStr = endTime.toISOString().split('T')[1].slice(0, 12);

        setFormFilters(prev => ({
            ...prev,
            dateFrom: startDateStr,
            timeFrom: startTimeStr,
            dateTo: endDateStr,
            timeTo: endTimeStr
        }));
    }, [selectedEntries, setFormFilters]);

    const handleContextMenu = React.useCallback((e: React.MouseEvent, entry: LogEntry, key: ColumnKey, value: string) => {
        e.preventDefault();

        // If the right-clicked entry is not in the selection, select it (and clear others)
        // This is standard behavior (right-click selects the item if not already selected)
        if (!selectedIds.has(entry.id)) {
            setSelectedIds(new Set([entry.id]));
            setKeyboardSelectedId(entry.id);
            lastClickedIdRef.current = entry.id;
        }

        setContextMenuState({ x: e.clientX, y: e.clientY, entry, key, value });
    }, [selectedIds, setKeyboardSelectedId]);

    const handleColumnResize = React.useCallback((key: ColumnKey, width: number) => {
        onColumnWidthsChange({ ...columnWidths, [key]: width });
    }, [columnWidths, onColumnWidthsChange]);



    const primarySelectedEntry = selectedIds.size === 1 ? entries.find(e => e.id === Array.from(selectedIds)[0]) || null : null;


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
                                            isSelected={selectedIds.has(entry.id)}
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
                            entry={primarySelectedEntry}
                            onClose={() => onDetailPanelVisibilityChange(false)}
                            width={panelWidths.details}
                            highlightTerms={[appliedFilters.includeMsg]}
                            theme={theme}
                            onApplyFilter={onApplyFilter}
                            columnStyles={columnStyles}
                            iconSet={iconSet}
                            selectedEntries={selectedEntries}
                        />
                    </>
                )}
            </div>
            {contextMenuState && <ContextMenu {...contextMenuState} selectedEntries={selectedEntries} onClose={() => setContextMenuState(null)} onFilter={onContextMenuFilter} iconSet={iconSet} contextKey={contextMenuState.key} contextValue={contextMenuState.value} onSetTimeStart={handleSetTimeStart} onSetTimeEnd={handleSetTimeEnd} onSetTimeRange={handleSetTimeRange} />}
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
