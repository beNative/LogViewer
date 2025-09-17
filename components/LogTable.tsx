import React from 'react';
import { LogEntry, FilterState, PageTimestampRange, ColumnVisibilityState, ColumnStyles, ColumnKey, PanelWidths, ViewMode, ConsoleMessage, OverallTimeRange, FileTimeRange, LogDensityPointByLevel, IconSet, LogTableDensity, Theme, TimelineBarVisibility } from '../types.ts';
import { Icon } from './icons/index.tsx';
import { FilterBar } from './FilterBar.tsx';
import { LogDetailPanel } from './LogDetailPanel.tsx';
import { highlightText } from '../utils.ts';
import { TimeRangeSelector } from './TimeRangeSelector.tsx';
import { ActiveFilters } from './ActiveFilters.tsx';
import { ContextMenu } from './ContextMenu.tsx';
import { Splitter } from './Splitter.tsx';
import { ColumnVisibilityMenu } from './ColumnVisibilityMenu.tsx';

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
    const rowRefs = React.useRef<Map<number, HTMLTableRowElement | null>>(new Map());
    
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
        jumpToEntryId
    } = props;
    
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

    // Scroll into view effect
    React.useEffect(() => {
        if (keyboardSelectedId !== null) {
            const rowEl = rowRefs.current.get(keyboardSelectedId);
            if (rowEl) {
                rowEl.scrollIntoView({ block: 'nearest', behavior: 'smooth' });
            }
        }
    }, [keyboardSelectedId]);


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

    const visibilityKey = Object.values(props.columnVisibility).join('-');

    return (
        <div className="flex flex-col flex-grow min-h-0 bg-gray-100 dark:bg-gray-900">
            {props.isTimeRangeSelectorVisible && props.overallTimeRange && (
                <div className="flex-shrink-0 p-4 border-b border-gray-200 dark:border-gray-700 bg-white dark:bg-gray-800/50">
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
                <main className="flex-grow min-h-0 flex flex-col">
                    <div className="overflow-auto outline-none flex-grow min-w-0" ref={tableContainerRef} tabIndex={-1}>
                        <table key={visibilityKey} className="min-w-full table-auto font-sans">
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
                                {entries.map(entry => (
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
                             </tbody>
                        </table>
                        {isBusy && <div className="p-4 text-center">Loading...</div>}
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