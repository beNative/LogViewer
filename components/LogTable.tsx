import React from 'react';
// FIX: Import the centralized Theme type from types.ts.
import { LogEntry, FilterState, PageTimestampRange, ColumnVisibilityState, ColumnStyles, ColumnKey, PanelWidths, ViewMode, ConsoleMessage, OverallTimeRange, FileTimeRange, LogDensityPoint, IconSet, LogTableDensity, Theme } from '../types.ts';
import { Icon } from './icons/index.tsx';
import { FilterBar } from './FilterBar.tsx';
import { LogDetailPanel } from './LogDetailPanel.tsx';
import { highlightText } from '../utils.ts';
import { ColumnSelector } from './ColumnSelector.tsx';
import { TimeRangeSelector } from './TimeRangeSelector.tsx';
import { getSqlDateTime } from '../db.ts';
import { ActiveFilters } from './ActiveFilters.tsx';
import { ContextMenu } from './ContextMenu.tsx';
import { DensityControl } from './DensityControl.tsx';

// FIX: Removed local Theme type definition. It is now imported from types.ts.
type ContextMenuState = { x: number; y: number; entry: LogEntry; value: string; key: ColumnKey; };

interface LogTableProps {
  entries: LogEntry[];
  loadedFileNames: string[];
  pageTimestampRanges: PageTimestampRange[];
  onViewModeChange: (mode: ViewMode) => void;
  filters: FilterState; // This is formFilters
  appliedFilters: FilterState; // This is the source of truth for the view
  onFiltersChange: (filters: FilterState) => void;
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
  logDensity: LogDensityPoint[];
  overallLogDensity: LogDensityPoint[];
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
}

export const getLevelColor = (level: string): string => {
  const lowerLevel = level.toLowerCase();
  if (lowerLevel.includes('error')) {
    return 'bg-red-100 text-red-800 dark:bg-red-800/50 dark:text-red-300';
  }
  if (lowerLevel.includes('warn')) {
    return 'bg-yellow-100 text-yellow-800 dark:bg-yellow-800/50 dark:text-yellow-300';
  }
  if (lowerLevel.includes('info ext')) {
    return 'bg-sky-100 text-sky-800 dark:bg-sky-800/50 dark:text-sky-300';
  }
  if (lowerLevel.includes('info')) {
    return 'bg-blue-100 text-blue-800 dark:bg-blue-800/50 dark:text-blue-300';
  }
  return 'bg-gray-200 text-gray-700 dark:bg-gray-700/50 dark:text-gray-300';
};


export const LogTable: React.FC<LogTableProps> = ({ 
  entries,
  loadedFileNames,
  pageTimestampRanges,
  filters,
  appliedFilters,
  onFiltersChange,
  onApplyFilters,
  onResetFilters,
  onClearTimeRange,
  uniqueValues,
  theme,
  viewMode,
  onViewModeChange,
  columnVisibility,
  onColumnVisibilityChange,
  columnStyles,
  panelWidths,
  onPanelWidthsChange,
  onApplyFilter,
  onContextMenuFilter,
  customFilterPresets,
  onSavePreset,
  onDeletePreset,
  onLoadPreset,
  onLoadMore,
  hasMore,
  isBusy,
  logToConsole,
  overallTimeRange,
  onTimeRangeSelectorChange,
  isTimeRangeSelectorVisible,
  onTimeRangeSelectorVisibilityChange,
  fileTimeRanges,
  logDensity,
  overallLogDensity,
  datesWithLogs,
  onCursorChange,
  onFileSelect,
  onDateSelect,
  keyboardSelectedId,
  setKeyboardSelectedId,
  jumpToEntryId,
  timelineViewRange,
  onTimelineViewRangeChange,
  onTimelineZoomToSelection,
  onTimelineZoomReset,
  isInitialLoad,
  iconSet,
  onRemoveAppliedFilter,
  logTableDensity,
  onLogTableDensityChange
}) => {
  const [selectedEntry, setSelectedEntry] = React.useState<LogEntry | null>(null);
  
  const [localPanelWidths, setLocalPanelWidths] = React.useState<PanelWidths>(panelWidths);
  const [isResizing, setIsResizing] = React.useState<'filters' | 'details' | null>(null);
  const [isDebugMenuOpen, setIsDebugMenuOpen] = React.useState(false);
  const [isDetailPanelVisible, setIsDetailPanelVisible] = React.useState(false);
  const [contextMenu, setContextMenu] = React.useState<ContextMenuState | null>(null);
  
  // Refs for layout and scrolling
  const fullContainerRef = React.useRef<HTMLDivElement>(null);
  const mainContainerRef = React.useRef<HTMLElement>(null);
  const scrollContainerRef = React.useRef<HTMLDivElement>(null);
  const tableBodyRef = React.useRef<HTMLTableSectionElement>(null);
  const debugButtonRef = React.useRef<HTMLDivElement>(null);

  React.useEffect(() => {
    setLocalPanelWidths(panelWidths);
  }, [panelWidths]);

  // When switching view modes, reset any selection state
  React.useEffect(() => {
    setSelectedEntry(null);
    setKeyboardSelectedId(null);
    setIsDetailPanelVisible(false);
  }, [viewMode, setKeyboardSelectedId]);
  
  // Infinite scroll listener
  React.useEffect(() => {
    const container = scrollContainerRef.current;
    if (!container || viewMode !== 'scroll') return;

    const handleScroll = () => {
        if (container.scrollTop + container.clientHeight >= container.scrollHeight - 500 && hasMore && !isBusy) {
            onLoadMore();
        }
    };

    container.addEventListener('scroll', handleScroll, { passive: true });
    return () => container.removeEventListener('scroll', handleScroll);
  }, [viewMode, hasMore, isBusy, onLoadMore]);

  // Close debug menu and context menu if clicked outside
  React.useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
        if (isDebugMenuOpen && debugButtonRef.current && !debugButtonRef.current.contains(event.target as Node)) {
            setIsDebugMenuOpen(false);
        }
        if (contextMenu) {
            setContextMenu(null);
        }
    };
    document.addEventListener('mousedown', handleClickOutside);
    return () => document.removeEventListener('mousedown', handleClickOutside);
  }, [isDebugMenuOpen, contextMenu]);

  const handleResizeMouseDown = (e: React.MouseEvent, panel: 'filters' | 'details') => {
    e.preventDefault();
    setIsResizing(panel);
  };

  const handleResizeMouseUp = React.useCallback(() => {
    if (isResizing) {
        onPanelWidthsChange(localPanelWidths);
        setIsResizing(null);
    }
  }, [isResizing, localPanelWidths, onPanelWidthsChange]);

  const handleResizeMouseMove = React.useCallback((e: MouseEvent) => {
    if (!isResizing || !fullContainerRef.current) return;
    
    e.preventDefault();

    const containerRect = fullContainerRef.current.getBoundingClientRect();
    let newWidth: number;
    
    if (isResizing === 'details') {
        newWidth = containerRect.right - e.clientX;
        const minPanelWidth = 300;
        const maxPanelWidth = containerRect.width - localPanelWidths.filters - 200; // Keep at least 200px for table
        newWidth = Math.max(minPanelWidth, Math.min(newWidth, maxPanelWidth));
        setLocalPanelWidths(prev => ({...prev, details: newWidth}));
    } else if (isResizing === 'filters') {
        newWidth = e.clientX - containerRect.left;
        const minPanelWidth = 250;
        const detailsWidth = isDetailPanelVisible && selectedEntry ? localPanelWidths.details : 0;
        const maxPanelWidth = containerRect.width - detailsWidth - 200; // Keep 200px for table
        newWidth = Math.max(minPanelWidth, Math.min(newWidth, maxPanelWidth));
        setLocalPanelWidths(prev => ({...prev, filters: newWidth}));
    }

  }, [isResizing, localPanelWidths.filters, localPanelWidths.details, selectedEntry, isDetailPanelVisible]);

  const handleLogLayout = () => {
    setIsDebugMenuOpen(false);
    logToConsole("Layout debugging is not applicable for the current scroll mode.", 'INFO');
    alert('The layout debugging tool is for the legacy virtualization mode and is not needed for the current table implementation.');
  };

  React.useEffect(() => {
    window.addEventListener('mousemove', handleResizeMouseMove);
    window.addEventListener('mouseup', handleResizeMouseUp);

    return () => {
      window.removeEventListener('mousemove', handleResizeMouseMove);
      window.removeEventListener('mouseup', handleResizeMouseUp);
    };
  }, [handleResizeMouseMove, handleResizeMouseUp]);

  React.useEffect(() => {
    if (keyboardSelectedId === null) return;
    
    // In both modes, we can now use a consistent querySelector
    const body = tableBodyRef.current;
    if (body) {
        const rowElement = body.querySelector(`[data-row-id="${keyboardSelectedId}"]`);
        if (rowElement) {
            rowElement.scrollIntoView({ behavior: 'smooth', block: 'nearest' });
        }
    }
  }, [keyboardSelectedId]);

  React.useEffect(() => {
    if (keyboardSelectedId !== null) {
        const entryToUpdate = entries.find(entry => entry.id === keyboardSelectedId);
        if (entryToUpdate) {
            setSelectedEntry(entryToUpdate);
        } else {
            if (!isBusy) {
                 setSelectedEntry(null);
            }
        }
    } else {
        setSelectedEntry(null);
    }
  }, [keyboardSelectedId, entries, isBusy]);
  
  React.useEffect(() => {
    if (jumpToEntryId !== null && entries.some(e => e.id === jumpToEntryId)) {
        setKeyboardSelectedId(jumpToEntryId);
    }
  }, [jumpToEntryId, entries, setKeyboardSelectedId]);


  React.useEffect(() => {
    const mainEl = mainContainerRef.current;
    if (!mainEl) return;
    
    const handleKeyDown = (e: KeyboardEvent) => {
      const handledKeys = ['ArrowUp', 'ArrowDown', 'PageUp', 'PageDown', 'Home', 'End'];
      if (!handledKeys.includes(e.key) || entries.length === 0) {
        return;
      }
      e.preventDefault();

      let currentIndex = -1;
      if (keyboardSelectedId !== null) {
        currentIndex = entries.findIndex(entry => entry.id === keyboardSelectedId);
      }
      
      switch (e.key) {
        case 'ArrowDown': {
          if (currentIndex < entries.length - 1) {
            const nextIndex = currentIndex === -1 ? 0 : currentIndex + 1;
            setKeyboardSelectedId(entries[nextIndex].id);
          } else if (viewMode === 'scroll' && hasMore && !isBusy) {
            onLoadMore();
          }
          break;
        }
        case 'ArrowUp': {
          if (currentIndex > 0) {
            setKeyboardSelectedId(entries[currentIndex - 1].id);
          } else if (currentIndex === -1 && entries.length > 0) {
            setKeyboardSelectedId(entries[entries.length - 1].id);
          }
          break;
        }
        case 'PageDown': {
            const scrollableView = scrollContainerRef.current;
            if (!scrollableView) break;

            if (currentIndex === entries.length - 1) break;

            const tableBody = tableBodyRef.current;
            if (!tableBody) break;
            const averageRowHeight = tableBody.scrollHeight > 0 ? tableBody.scrollHeight / entries.length : 50;
            const itemsPerPage = Math.max(1, Math.floor(scrollableView.clientHeight / averageRowHeight));
            
            const newIndex = Math.min(entries.length - 1, (currentIndex === -1 ? -1 : currentIndex) + itemsPerPage);
            setKeyboardSelectedId(entries[newIndex].id);
            break;
        }
        case 'PageUp': {
            const scrollableView = scrollContainerRef.current;
            if (!scrollableView) break;

            if (currentIndex <= 0) break;

            const tableBody = tableBodyRef.current;
            if (!tableBody) break;
            const averageRowHeight = tableBody.scrollHeight > 0 ? tableBody.scrollHeight / entries.length : 50;
            const itemsPerPage = Math.max(1, Math.floor(scrollableView.clientHeight / averageRowHeight));

            const newIndex = Math.max(0, currentIndex - itemsPerPage);
            setKeyboardSelectedId(entries[newIndex].id);
            break;
        }
        case 'Home': {
          if (entries.length > 0) {
            setKeyboardSelectedId(entries[0].id);
             if (scrollContainerRef.current) {
                scrollContainerRef.current.scrollTop = 0;
            }
          }
          break;
        }
        case 'End': {
          if (viewMode === 'scroll' && hasMore) {
              onLoadMore(); // This will continue until all loaded
              // We can't easily jump to the end if not all are loaded, so this is a reasonable behavior.
          } else if (entries.length > 0) {
            setKeyboardSelectedId(entries[entries.length - 1].id);
            if (scrollContainerRef.current) {
                scrollContainerRef.current.scrollTop = scrollContainerRef.current.scrollHeight;
            }
          }
          break;
        }
      }
    };
    
    mainEl.addEventListener('keydown', handleKeyDown);
    return () => mainEl.removeEventListener('keydown', handleKeyDown);
  }, [entries, keyboardSelectedId, viewMode, hasMore, isBusy, onLoadMore, setKeyboardSelectedId]);
  
  const handleEntrySelect = React.useCallback((entry: LogEntry) => {
      setKeyboardSelectedId(entry.id);
  }, [setKeyboardSelectedId]);

  const handleContextMenu = (e: React.MouseEvent, entry: LogEntry) => {
    e.preventDefault();
    setContextMenu(null); // Close any existing menu

    const target = e.target as HTMLElement;
    const cell = target.closest('td');
    if (!cell) return;

    const key = cell.dataset.key as ColumnKey;
    const value = cell.dataset.value;

    if (key && value) {
        setContextMenu({
            x: e.clientX,
            y: e.clientY,
            entry,
            key,
            value,
        });
    }
  };

  const handleToggleDetailsPanel = () => {
    const turningOn = !isDetailPanelVisible;
    if (turningOn && keyboardSelectedId === null && entries.length > 0) {
        setKeyboardSelectedId(entries[0].id);
    }
    setIsDetailPanelVisible(turningOn);
  };

  const includeTerms = filters.includeMsg.split('\n').filter(Boolean);

  const visibleColumnCount = React.useMemo(() => {
    return Object.values(columnVisibility).filter(v => v).length;
  }, [columnVisibility]);
  
  const getTdStyle = (key: ColumnKey): React.CSSProperties => {
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

  const getRowPaddingClass = React.useCallback(() => {
    switch (logTableDensity) {
      case 'compact': return 'py-1';
      case 'normal': return 'py-2';
      case 'comfortable': return 'py-4';
      default: return 'py-2';
    }
  }, [logTableDensity]);
  
  const getSelectedTimestamps = React.useCallback((sourceFilters: FilterState) => {
    let selectedStartTime: number | null = null;
    let selectedEndTime: number | null = null;

    if (sourceFilters.dateFrom) {
        let timePart = sourceFilters.timeFrom || '00:00:00';
        if (timePart.length === 5) timePart += ':00';
        const dateString = `${sourceFilters.dateFrom}T${timePart}.000Z`;
        const date = new Date(dateString);
        if (!isNaN(date.getTime())) {
            selectedStartTime = date.getTime();
        }
    }
     if (sourceFilters.dateTo) {
        let timePart = sourceFilters.timeTo || '23:59:59';
        if (timePart.length === 5) timePart += ':00';
        const dateString = `${sourceFilters.dateTo}T${timePart}.999Z`;
        const date = new Date(dateString);
         if (!isNaN(date.getTime())) {
            selectedEndTime = date.getTime();
        }
    }
    return { selectedStartTime, selectedEndTime };
  }, []);

  const { selectedStartTime, selectedEndTime } = getSelectedTimestamps(filters);

  const zoomToSelectionEnabled = React.useMemo(() => {
    if (!appliedFilters.dateFrom || !appliedFilters.dateTo) return false;
    const { selectedStartTime: appliedStart, selectedEndTime: appliedEnd } = getSelectedTimestamps(appliedFilters);
    return appliedStart !== null && appliedEnd !== null && appliedStart < appliedEnd;
  }, [appliedFilters, getSelectedTimestamps]);
  
  const activeDate = React.useMemo(() => {
    if (filters.dateFrom && filters.dateTo && filters.dateFrom === filters.dateTo) {
        return filters.dateFrom;
    }
    return null;
  }, [filters.dateFrom, filters.dateTo]);

  const activeFileName = React.useMemo(() => {
    if (filters.fileName?.length === 1) {
        return filters.fileName[0];
    }
    return null;
  }, [filters.fileName]);

  const ViewModeToggle: React.FC = () => {
    const buttonBase = "px-3 py-1 text-xs font-semibold rounded-md transition-colors";
    const activeClass = "bg-white dark:bg-gray-600 text-sky-700 dark:text-white shadow-sm";
    const inactiveClass = "text-gray-500 dark:text-gray-400 hover:bg-white/60 dark:hover:bg-gray-500/50";
    
    return (
      <div className="flex items-center gap-1 p-0.5 bg-gray-200 dark:bg-gray-700/80 rounded-lg">
        <button onClick={() => onViewModeChange('pagination')} className={`${buttonBase} ${viewMode === 'pagination' ? activeClass : inactiveClass}`}>
          Paginate
        </button>
        <button onClick={() => onViewModeChange('scroll')} className={`${buttonBase} ${viewMode === 'scroll' ? activeClass : inactiveClass}`}>
          Scroll
        </button>
      </div>
    );
  };

  const SkeletonRow: React.FC<{ visibility: ColumnVisibilityState }> = React.memo(({ visibility }) => {
    const shimmerBg = 'linear-gradient(to right, transparent 0%, rgba(255,255,255,0.1) 50%, transparent 100%)';
    const lightShimmerBg = 'linear-gradient(to right, transparent 0%, rgba(0,0,0,0.05) 50%, transparent 100%)';
    const paddingClass = getRowPaddingClass();

    return (
        <tr className="bg-white dark:bg-gray-900">
            {visibility.time && <td className={`whitespace-nowrap pl-4 pr-3 sm:pl-6 ${paddingClass}`}><div className="h-4 rounded bg-gray-200 dark:bg-gray-700/80 w-3/4 animate-shimmer" style={{ backgroundSize: '1000px 100%', backgroundImage: theme === 'dark' ? shimmerBg : lightShimmerBg }}></div></td>}
            {visibility.level && <td className={`whitespace-nowrap px-3 ${paddingClass}`}><div className="h-6 w-20 rounded-full bg-gray-200 dark:bg-gray-700/80 animate-shimmer" style={{ backgroundSize: '1000px 100%', backgroundImage: theme === 'dark' ? shimmerBg : lightShimmerBg }}></div></td>}
            {visibility.sndrtype && <td className={`whitespace-nowrap px-3 ${paddingClass}`}><div className="h-4 rounded bg-gray-200 dark:bg-gray-700/80 w-2/3 animate-shimmer" style={{ backgroundSize: '1000px 100%', backgroundImage: theme === 'dark' ? shimmerBg : lightShimmerBg }}></div></td>}
            {visibility.sndrname && <td className={`whitespace-nowrap px-3 ${paddingClass}`}><div className="h-4 rounded bg-gray-200 dark:bg-gray-700/80 w-3/4 animate-shimmer" style={{ backgroundSize: '1000px 100%', backgroundImage: theme === 'dark' ? shimmerBg : lightShimmerBg }}></div></td>}
            {visibility.fileName && <td className={`whitespace-nowrap px-3 ${paddingClass}`}><div className="h-4 rounded bg-gray-200 dark:bg-gray-700/80 w-5/6 animate-shimmer" style={{ backgroundSize: '1000px 100%', backgroundImage: theme === 'dark' ? shimmerBg : lightShimmerBg }}></div></td>}
            {visibility.msg && <td className={`px-3 ${paddingClass}`}><div className="h-4 rounded bg-gray-200 dark:bg-gray-700/80 w-full animate-shimmer" style={{ backgroundSize: '1000px 100%', backgroundImage: theme === 'dark' ? shimmerBg : lightShimmerBg }}></div></td>}
        </tr>
    );
});
  
  const TableContent = (
    <table className="min-w-full divide-y divide-gray-200 dark:divide-gray-700">
      <thead className="bg-gray-100 dark:bg-gray-800 sticky top-0 z-10">
        <tr>
          {columnVisibility.time && <th scope="col" className="py-3.5 pl-4 pr-3 text-left text-sm font-semibold text-gray-900 dark:text-gray-300 sm:pl-6">Timestamp</th>}
          {columnVisibility.level && <th scope="col" className="px-3 py-3.5 text-left text-sm font-semibold text-gray-900 dark:text-gray-300">Level</th>}
          {columnVisibility.sndrtype && <th scope="col" className="px-3 py-3.5 text-left text-sm font-semibold text-gray-900 dark:text-gray-300">Sender Type</th>}
          {columnVisibility.sndrname && <th scope="col" className="px-3 py-3.5 text-left text-sm font-semibold text-gray-900 dark:text-gray-300">Sender Name</th>}
          {columnVisibility.fileName && <th scope="col" className="px-3 py-3.5 text-left text-sm font-semibold text-gray-900 dark:text-gray-300">Filename</th>}
          {columnVisibility.msg && <th scope="col" className="px-3 py-3.5 text-left text-sm font-semibold text-gray-900 dark:text-gray-300">Message</th>}
        </tr>
      </thead>
      <tbody className="divide-y divide-gray-200/50 dark:divide-gray-700/50 bg-white dark:bg-gray-900" ref={tableBodyRef}>
        {isBusy && entries.length === 0 ? (
            <>
                {Array.from({ length: 15 }).map((_, i) => <SkeletonRow key={i} visibility={columnVisibility} />)}
            </>
        ) : (
            <>
                {entries.map((entry) => (
                  <tr key={entry.id} 
                    data-row-id={entry.id}
                    onClick={() => handleEntrySelect(entry)}
                    onContextMenu={(e) => handleContextMenu(e, entry)}
                    className={`relative hover:bg-gray-100 dark:hover:bg-gray-800/70 transition-colors duration-150 cursor-pointer ${
                      keyboardSelectedId === entry.id ? 'bg-sky-50 dark:bg-sky-900/60 ring-2 ring-inset ring-sky-500' : ''
                    }`}
                  >
                    {columnVisibility.time && <td data-key="time" data-value={entry.time} style={getTdStyle('time')} className={`whitespace-nowrap pl-4 pr-3 sm:pl-6 ${getRowPaddingClass()}`}>{entry.time}</td>}
                    
                    {columnVisibility.level && <td data-key="level" data-value={entry.level} className={`whitespace-nowrap px-3 ${getRowPaddingClass()}`}>
                      <span style={getTdStyle('level')} className={`px-2 py-1 rounded-full font-medium ${getLevelColor(entry.level)}`}>
                        {entry.level}
                      </span>
                    </td>}

                    {columnVisibility.sndrtype && <td data-key="sndrtype" data-value={entry.sndrtype} style={getTdStyle('sndrtype')} className={`whitespace-nowrap px-3 ${getRowPaddingClass()}`}>{entry.sndrtype}</td>}
                    
                    {columnVisibility.sndrname && <td data-key="sndrname" data-value={entry.sndrname} style={getTdStyle('sndrname')} className={`whitespace-nowrap px-3 ${getRowPaddingClass()}`}>{entry.sndrname}</td>}
                    
                    {columnVisibility.fileName && <td data-key="fileName" data-value={entry.fileName} style={getTdStyle('fileName')} className={`whitespace-nowrap max-w-xs truncate px-3 ${getRowPaddingClass()}`} title={entry.fileName}>{entry.fileName}</td>}

                    {columnVisibility.msg && <td data-key="msg" data-value={entry.msg} style={getTdStyle('msg')} className={`whitespace-pre-wrap break-words px-3 ${getRowPaddingClass()}`} dangerouslySetInnerHTML={{ __html: highlightText(entry.msg, includeTerms, theme) }} />}
                  </tr>
                ))}
                {entries.length === 0 && (
                    <tr>
                      <td colSpan={visibleColumnCount} className="text-center py-12">
                        {isInitialLoad && entries.length === 0 ? (
                            <div className="text-gray-500 dark:text-gray-400">
                                <Icon name="Filter" iconSet={iconSet} className="w-16 h-16 text-gray-300 dark:text-gray-600 mx-auto mb-4" />
                                <h3 className="text-xl font-semibold mb-2 text-gray-700 dark:text-gray-300">Data Ready for Analysis</h3>
                                <p>Use the filters on the left and click <strong className="text-amber-600 dark:text-amber-400">Apply</strong> to view logs.</p>
                            </div>
                        ) : (
                            <div className="text-gray-500 dark:text-gray-400">
                                No log items found matching the filters.
                            </div>
                        )}
                      </td>
                    </tr>
                )}
            </>
        )}
      </tbody>
    </table>
  );

  return (
    <div className="flex flex-col flex-1 bg-white dark:bg-gray-800/50 overflow-hidden">
        {contextMenu && (
            <ContextMenu
                x={contextMenu.x}
                y={contextMenu.y}
                entry={contextMenu.entry}
                contextKey={contextMenu.key}
                contextValue={contextMenu.value}
                onClose={() => setContextMenu(null)}
                onFilter={onContextMenuFilter}
                iconSet={iconSet}
            />
        )}
      <div className="flex flex-1 min-h-0" ref={fullContainerRef}>
        <aside
          style={{ width: `${localPanelWidths.filters}px` }} 
          className="flex-shrink-0 bg-gray-50 dark:bg-gray-800 border-r border-gray-200 dark:border-gray-700">
          <FilterBar
            filters={filters}
            appliedFilters={appliedFilters}
            onFiltersChange={onFiltersChange}
            onApplyFilters={onApplyFilters}
            onResetFilters={onResetFilters}
            uniqueValues={uniqueValues}
            customFilterPresets={customFilterPresets}
            onSavePreset={onSavePreset}
            onDeletePreset={onDeletePreset}
            onLoadPreset={onLoadPreset}
            iconSet={iconSet}
            isInitialLoad={isInitialLoad}
          />
        </aside>

        <div
            onMouseDown={(e) => handleResizeMouseDown(e, 'filters')}
            className="w-1.5 flex-shrink-0 cursor-col-resize bg-gray-200 dark:bg-gray-700/50 hover:bg-sky-500 transition-colors duration-200"
        />

        <main className="flex-1 flex flex-col min-h-0" ref={mainContainerRef} tabIndex={-1}>
            <div className="flex-1 flex min-h-0">
                <div className="flex-1 min-w-0 min-h-0 flex flex-col">
                    {isTimeRangeSelectorVisible && overallTimeRange && (
                        <div className="flex-shrink-0 p-4 border-b border-gray-200 dark:border-gray-700 bg-gray-50 dark:bg-gray-800">
                            <TimeRangeSelector
                                minTime={overallTimeRange.min}
                                maxTime={overallTimeRange.max}
                                selectedStartTime={selectedStartTime}
                                selectedEndTime={selectedEndTime}
                                onRangeChange={onTimeRangeSelectorChange}
                                onClear={onClearTimeRange}
                                theme={theme}
                                pageTimestampRanges={pageTimestampRanges}
                                fileTimeRanges={fileTimeRanges}
                                logDensity={logDensity}
                                overallLogDensity={overallLogDensity}
                                datesWithLogs={datesWithLogs}
                                viewMode={viewMode}
                                cursorTime={selectedEntry ? new Date(selectedEntry.time + 'Z').getTime() : null}
                                activeFileName={activeFileName}
                                activeDate={activeDate}
                                onCursorChange={onCursorChange}
                                onFileSelect={onFileSelect}
                                onDateSelect={onDateSelect}
                                viewRange={timelineViewRange}
                                onViewRangeChange={onTimelineViewRangeChange}
                                onZoomToSelection={onTimelineZoomToSelection}
                                onZoomToExtent={onTimelineZoomReset}
                                zoomToSelectionEnabled={zoomToSelectionEnabled}
                                iconSet={iconSet}
                            />
                        </div>
                    )}

                    <div className="flex-shrink-0">
                      <ActiveFilters 
                        appliedFilters={appliedFilters} 
                        onRemoveFilter={onRemoveAppliedFilter}
                        iconSet={iconSet}
                      />
                    </div>

                    <div className="flex-shrink-0 flex items-center justify-end p-2 border-b border-gray-200 dark:border-gray-700 bg-gray-50 dark:bg-gray-800 flex-wrap gap-4">
                        <div className="flex items-center gap-x-6 gap-y-2 flex-wrap justify-end">
                            <DensityControl value={logTableDensity} onChange={onLogTableDensityChange} />
                            <ViewModeToggle />
                            <ColumnSelector
                                visibility={columnVisibility}
                                onChange