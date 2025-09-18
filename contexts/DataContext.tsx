import React, { createContext, useState, useCallback, useContext, useEffect, useRef } from 'react';
import { FilterState, LogEntry, PageTimestampRange, DashboardData, FileTimeRange, LogDensityPointByLevel, StockInfoEntry, StockInfoFilters, StockArticleSuggestion, LogDensityPoint, ConsoleMessage } from '../types';
import { getSqlDateTime } from '../db';
import { useSession } from './SessionContext';
import { useUI } from './UIContext';
import { useConsole } from './ConsoleContext';
import { useSettings } from './SettingsContext';

const INFINITE_SCROLL_CHUNK_SIZE = 200;

const initialFilters: FilterState = {
  dateFrom: '', timeFrom: '', dateTo: '', timeTo: '', level: [], levelFilterMode: 'include',
  sndrtype: [], sndrtypeFilterMode: 'include', sndrname: [], sndrnameFilterMode: 'include',
  fileName: [], fileNameFilterMode: 'include', includeMsg: '', excludeMsg: '',
  includeMsgMode: 'OR', excludeMsgMode: 'AND', sqlQuery: '', sqlQueryEnabled: false,
};

const initialDashboardData: DashboardData = { timeline: [], levels: [], senderTypes: [] };

type DataContextType = {
    // Log Viewer State & Logic
    filteredEntries: LogEntry[];
    totalFilteredCount: number;
    hasMoreLogs: boolean;
    pageTimestampRanges: PageTimestampRange[];
    formFilters: FilterState;
    setFormFilters: React.Dispatch<React.SetStateAction<FilterState>>;
    appliedFilters: FilterState;
    uniqueValues: { level: string[]; sndrtype: string[]; sndrname: string[]; fileName: string[]; };
    fileTimeRanges: FileTimeRange[];
    logDensity: LogDensityPointByLevel[];
    overallLogDensity: LogDensityPointByLevel[];
    datesWithLogs: string[];
    currentPage: number;
    totalPages: number;
    pageSize: number;
    handleApplyFilters: () => void;
    handleResetFilters: () => void;
    handleClearTimeRange: () => void;
    onLoadMore: () => void;
    goToPage: (pageNumber: number) => void;
    handlePageSizeChange: (newSize: number) => void;
    handleRemoveAppliedFilter: (key: keyof FilterState, valueToRemove?: string) => void;
    handleContextMenuFilter: (key: 'level' | 'sndrtype' | 'sndrname' | 'fileName', value: string, exclude: boolean) => void;
    handleCursorChange: (time: number) => void;
    handleFileSelect: (fileName: string) => void;
    handleDateSelect: (date: string) => void;
    handleApplyDetailFilter: (key: 'level' | 'sndrtype' | 'sndrname' | 'fileName', value: string) => void;
    timelineViewRange: { min: number, max: number } | null;
    setTimelineViewRange: React.Dispatch<React.SetStateAction<{ min: number, max: number } | null>>;
    handleTimelineZoomToSelection: () => void;
    handleTimelineZoomReset: () => void;
    
    // Dashboard State & Logic
    dashboardData: DashboardData;
    handleTimeRangeSelect: (startTime: number, endTime: number) => void;
    handleCategorySelect: (category: 'level' | 'sndrtype', value: string) => void;

    // Stock Tracker State & Logic
    stockHistory: StockInfoEntry[];
    setStockHistory: React.Dispatch<React.SetStateAction<StockInfoEntry[]>>;
    overallStockTimeRange: { min: string, max: string } | null;
    overallStockDensity: LogDensityPoint[];
    handleSearchStock: (filters: StockInfoFilters) => void;
    handleRebuildStockData: () => Promise<void>;
    handleFetchStockSuggestions: (searchTerm: string, timeFilters: StockInfoFilters) => Promise<StockArticleSuggestion[]>;

    // Shared
    logToConsoleForEntries: (message: string, type: "DEBUG" | "INFO" | "WARNING" | "ERROR") => void;
    lastConsoleMessageForStatus: ConsoleMessage | null;
} & Pick<ReturnType<typeof useUI>, 'keyboardSelectedId' | 'setKeyboardSelectedId' | 'jumpToEntryId' | 'isInitialLoad'>;


const DataContext = createContext<DataContextType | undefined>(undefined);

export const DataProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const { db, hasData, loadedFileNames, handleRebuildStockDataInWorker, overallStockTimeRange, setOverallStockTimeRange, overallStockDensity, setOverallStockDensity } = useSession();
    const { logToConsole, lastConsoleMessage } = useConsole();
    const { setIsBusy, isBusy, isStockBusy, setIsStockBusy, setActiveView, setJumpToEntryId, jumpToEntryId, isInitialLoad, setIsInitialLoad, keyboardSelectedId, setKeyboardSelectedId, addToast } = useUI();
    const { viewMode } = useSettings();

    // Log Viewer State
    const [filteredEntries, setFilteredEntries] = useState<LogEntry[]>([]);
    const [entriesOffset, setEntriesOffset] = useState(0);
    const [totalFilteredCount, setTotalFilteredCount] = useState(0);
    const [formFilters, setFormFilters] = useState<FilterState>(initialFilters);
    const [appliedFilters, setAppliedFilters] = useState<FilterState>(initialFilters);
    const [uniqueValues, setUniqueValues] = useState<{level: string[]; sndrtype: string[]; sndrname: string[]; fileName: string[];}>({ level: [], sndrtype: [], sndrname: [], fileName: [] });
    const [currentPage, setCurrentPage] = useState<number>(1);
    const [pageSize, setPageSize] = useState<number>(1000);
    const [pageTimestampRanges, setPageTimestampRanges] = useState<PageTimestampRange[]>([]);
    const [hasMoreLogs, setHasMoreLogs] = useState(true);
    const [timelineViewRange, setTimelineViewRange] = useState<{ min: number; max: number } | null>(null);

    // Dashboard State
    const [dashboardData, setDashboardData] = useState<DashboardData>(initialDashboardData);
    const [fileTimeRanges, setFileTimeRanges] = useState<FileTimeRange[]>([]);
    const [logDensity, setLogDensity] = useState<LogDensityPointByLevel[]>([]);
    const [overallLogDensity, setOverallLogDensity] = useState<LogDensityPointByLevel[]>([]);
    const [datesWithLogs, setDatesWithLogs] = useState<string[]>([]);
    
    // Stock Tracker State
    const [stockHistory, setStockHistory] = useState<StockInfoEntry[]>([]);

    const busyTaskRef = useRef(0);

    // Effect to initialize/reset filters when data source changes
    useEffect(() => {
        if (db && hasData) {
            const savedFiltersJson = db.getMeta('appliedFilters');
            let filtersToUse = { ...initialFilters };
            let loadedFromSession = false;

            if (savedFiltersJson) {
                try {
                    const savedFilters = JSON.parse(savedFiltersJson);
                    if (savedFilters && typeof savedFilters === 'object' && 'sqlQueryEnabled' in savedFilters) {
                        filtersToUse = { ...initialFilters, ...savedFilters };
                        logToConsole('Loaded saved filters from session.', 'INFO');
                        loadedFromSession = true;
                    } else {
                        throw new Error('Invalid format');
                    }
                } catch (e) {
                     logToConsole(`Could not parse saved filters, using defaults.`, 'WARNING');
                }
            }

            if (!loadedFromSession) {
                const { minTime, maxTime } = db.getMinMaxTime();
                if (minTime && maxTime) {
                    const [minDate, minTimeStr] = minTime.split(' ');
                    const [maxDate, maxTimeStr] = maxTime.split(' ');
                    filtersToUse = {
                        ...initialFilters,
                        dateFrom: minDate, timeFrom: minTimeStr.substring(0, 8),
                        dateTo: maxDate, timeTo: maxTimeStr.substring(0, 8),
                    };
                }
            }

            setFormFilters(filtersToUse);
            setAppliedFilters(filtersToUse);
            if (loadedFromSession) {
                setIsInitialLoad(false); // A loaded session should show data immediately.
            }

            setUniqueValues({
                level: db.getUniqueColumnValues('level'),
                sndrtype: db.getUniqueColumnValues('sndrtype'),
                sndrname: db.getUniqueColumnValues('sndrname'),
                fileName: loadedFileNames,
            });
            setOverallLogDensity(db.getLogDensityByLevel(initialFilters, 300));
        } else {
            setFormFilters(initialFilters);
            setAppliedFilters(initialFilters);
            setUniqueValues({ level: [], sndrtype: [], sndrname: [], fileName: [] });
        }
    }, [db, hasData, loadedFileNames, logToConsole, setIsInitialLoad]);


    // Main data fetching effect
    useEffect(() => {
        if (!db || !hasData) {
            setTotalFilteredCount(0);
            setFilteredEntries([]);
            setEntriesOffset(0);
            setDashboardData(initialDashboardData);
            setPageTimestampRanges([]);
            setFileTimeRanges([]);
            setLogDensity([]);
            setDatesWithLogs([]);
            return;
        }

        setIsBusy(true);
        clearTimeout(busyTaskRef.current);
        
        if (!isInitialLoad) {
            setFilteredEntries([]);
        }
        setEntriesOffset(0);

        busyTaskRef.current = window.setTimeout(() => {
            logToConsole(`Updating view...`, 'DEBUG');
            const count = db.getFilteredLogCount(appliedFilters);
            setTotalFilteredCount(count);

            setDashboardData({
                timeline: db.getLogVolumeByInterval(appliedFilters),
                levels: db.getCountsByColumn('level', appliedFilters),
                senderTypes: db.getCountsByColumn('sndrtype', appliedFilters),
            });
            setFileTimeRanges(db.getTimeRangePerFile(appliedFilters));
            setLogDensity(db.getLogDensityByLevel(appliedFilters, 200));
            setDatesWithLogs(db.getDatesWithLogs(appliedFilters));
            
            if (count > 0 && !isInitialLoad) {
                if (viewMode === 'pagination') {
                    const entries = db.queryLogEntries(appliedFilters, pageSize, (currentPage - 1) * pageSize);
                    const ranges = db.getPageTimestampRanges(appliedFilters, pageSize);
                    setFilteredEntries(entries);
                    setPageTimestampRanges(ranges);
                    setHasMoreLogs(false);
                } else { // 'scroll' mode
                    const entries = db.queryLogEntries(appliedFilters, INFINITE_SCROLL_CHUNK_SIZE, 0);
                    setFilteredEntries(entries);
                    setEntriesOffset(0);
                    setHasMoreLogs(entries.length < count);
                    setPageTimestampRanges([]);
                }
                logToConsole(`View updated. Found ${count} matching entries.`, 'DEBUG');
            } else {
                setFilteredEntries([]);
                setPageTimestampRanges([]);
                setHasMoreLogs(false);
                if (count === 0) {
                     setDashboardData(initialDashboardData);
                     setFileTimeRanges([]);
                     setLogDensity([]);
                     setDatesWithLogs([]);
                } else if (isInitialLoad) {
                    logToConsole(`Data is ready. Found ${count} matching entries. Apply filters to view.`, 'INFO');
                }
            }
            setIsBusy(false);
        }, 50);

        return () => clearTimeout(busyTaskRef.current);
    }, [db, hasData, appliedFilters, currentPage, pageSize, viewMode, logToConsole, isInitialLoad, setIsBusy]);

    useEffect(() => {
        if (jumpToEntryId && filteredEntries.length > 0) {
            if (filteredEntries.some(e => e.id === jumpToEntryId)) {
                setKeyboardSelectedId(jumpToEntryId);
                setJumpToEntryId(null);
            }
        }
    }, [jumpToEntryId, filteredEntries, setKeyboardSelectedId, setJumpToEntryId]);

    // LOGIC & ACTIONS
    const handleApplyFilters = useCallback(() => {
        logToConsole('Applying filters...', 'INFO');
        setAppliedFilters(formFilters);
        setCurrentPage(1);
        setEntriesOffset(0);
        setTimelineViewRange(null);
        setIsInitialLoad(false);

        if (db) {
            try {
                db.setMeta('appliedFilters', JSON.stringify(formFilters));
                logToConsole('Saved current filter configuration to the session database.', 'DEBUG');
            } catch(e) {
                const msg = e instanceof Error ? e.message : String(e);
                logToConsole(`Could not save filters to session: ${msg}`, 'WARNING');
            }
        }
    }, [db, formFilters, logToConsole, setIsInitialLoad]);

    const handleResetFilters = useCallback(() => {
        logToConsole('Resetting filters...', 'INFO');
        setTimelineViewRange(null);
        const { minTime, maxTime } = db ? db.getMinMaxTime() : { minTime: null, maxTime: null };
        let newFilters = initialFilters;
        if (minTime && maxTime) {
            const [minDate, minTimeStr] = minTime.split(' ');
            const [maxDate, maxTimeStr] = maxTime.split(' ');
            newFilters = { ...initialFilters, dateFrom: minDate, timeFrom: minTimeStr.substring(0, 8), dateTo: maxDate, timeTo: maxTimeStr.substring(0, 8) };
        }
        setFormFilters(newFilters);
        setAppliedFilters(newFilters);
        setIsInitialLoad(false);
        setCurrentPage(1);
        setEntriesOffset(0);
    }, [logToConsole, db, setIsInitialLoad]);

    const onLoadMore = useCallback(() => {
        if (!db || !hasMoreLogs || isBusy) return;
        setTimeout(() => {
            setIsBusy(true);
            const newOffset = entriesOffset + INFINITE_SCROLL_CHUNK_SIZE;
            logToConsole(`Loading more logs at offset ${newOffset.toLocaleString()}...`, 'DEBUG');
            const newEntries = db.queryLogEntries(appliedFilters, INFINITE_SCROLL_CHUNK_SIZE, newOffset);
            setFilteredEntries(prev => [...prev, ...newEntries]);
            setEntriesOffset(newOffset);
            setHasMoreLogs(newOffset + newEntries.length < totalFilteredCount);
            setIsBusy(false);
            logToConsole(`Loaded ${newEntries.length} more logs.`, 'DEBUG');
        }, 10);
    }, [db, hasMoreLogs, isBusy, entriesOffset, appliedFilters, totalFilteredCount, logToConsole, setIsBusy]);
    
    const goToPage = useCallback((pageNumber: number) => setCurrentPage(pageNumber), []);
    const handlePageSizeChange = useCallback((newSize: number) => { setPageSize(newSize); setCurrentPage(1); }, []);

    const handleClearTimeRange = useCallback(() => {
        logToConsole('Clearing time range filters...', 'INFO');
        const updater = (f: FilterState) => ({...f, dateFrom: '', timeFrom: '', dateTo: '', timeTo: ''});
        setFormFilters(updater);
        setAppliedFilters(updater);
        setIsInitialLoad(false);
        setCurrentPage(1);
        setEntriesOffset(0);
    }, [logToConsole, setIsInitialLoad]);

    const handleTimeRangeSelect = useCallback((startTime: number, endTime: number) => {
        const startDate = new Date(startTime);
        const endDate = new Date(endTime);
        const dateToYYYYMMDD = (d: Date) => d.toISOString().split('T')[0];
        const dateToHHMMSS = (d: Date) => d.toISOString().split('T')[1].substring(0, 8);
        const updater = (f: FilterState) => ({...f, dateFrom: dateToYYYYMMDD(startDate), timeFrom: dateToHHMMSS(startDate), dateTo: dateToYYYYMMDD(endDate), timeTo: dateToHHMMSS(endDate)});
        setFormFilters(updater);
        setAppliedFilters(updater);
        setIsInitialLoad(false);
        setCurrentPage(1);
        setEntriesOffset(0);
        setActiveView('viewer');
    }, [setIsInitialLoad, setActiveView]);

    const handleCategorySelect = useCallback((category: 'level' | 'sndrtype', value: string) => {
        setTimelineViewRange(null);
        const modeKey = `${category}FilterMode` as const;
        const updater = (f: FilterState) => ({...f, [modeKey]: 'include', [category]: [...(f[category] || []), value]});
        setFormFilters(updater);
        setAppliedFilters(updater);
        setIsInitialLoad(false);
        setCurrentPage(1);
        setEntriesOffset(0);
        setActiveView('viewer');
    }, [setIsInitialLoad, setActiveView]);

    const handleRemoveAppliedFilter = useCallback((key: keyof FilterState, valueToRemove?: string) => {
        logToConsole(`Removing filter: ${key} = ${valueToRemove || '(cleared)'}`, 'DEBUG');
        const updater = (f: FilterState): FilterState => {
            const newFilters = { ...f };
            const currentValue = newFilters[key];
            if (Array.isArray(currentValue) && valueToRemove) {
                (newFilters[key] as string[]) = currentValue.filter(v => v !== valueToRemove);
                if ((newFilters[key] as string[]).length === 0) {
                    const modeKey = `${key}FilterMode` as keyof FilterState;
                    if (modeKey in newFilters) (newFilters as any)[modeKey] = 'include';
                }
            } else {
                (newFilters as any)[key] = Array.isArray(initialFilters[key as keyof FilterState]) ? [] : '';
                const modeKey = `${key}FilterMode` as keyof FilterState;
                if (modeKey in newFilters) (newFilters as any)[modeKey] = 'include';
            }
            return newFilters;
        };
        setFormFilters(updater);
        setAppliedFilters(updater);
        setIsInitialLoad(false);
        setCurrentPage(1);
        setEntriesOffset(0);
    }, [logToConsole, setIsInitialLoad]);

    const handleContextMenuFilter = useCallback((key: 'level' | 'sndrtype' | 'sndrname' | 'fileName', value: string, exclude: boolean) => {
        logToConsole(`Applying context filter: ${key} ${exclude ? '!=' : '='} ${value}`, 'DEBUG');
        const modeKey = `${key}FilterMode` as const;
        const updater = (f: FilterState) => {
            const newF = { ...f };
            const newMode = exclude ? 'exclude' : 'include';
            if (newF[modeKey] === newMode) {
                if (!(newF[key] || []).includes(value)) newF[key] = [...(newF[key] || []), value];
            } else {
                newF[modeKey] = newMode;
                newF[key] = [value];
            }
            return newF;
        };
        setFormFilters(updater);
        setAppliedFilters(updater);
        setIsInitialLoad(false);
        setCurrentPage(1);
        setEntriesOffset(0);
        setActiveView('viewer');
    }, [logToConsole, setIsInitialLoad, setActiveView]);

    const handleApplyDetailFilter = useCallback((key: 'level' | 'sndrtype' | 'sndrname' | 'fileName', value: string) => {
        logToConsole(`Applying quick filter: ${key} = ${value}`, 'DEBUG');
        const modeKey = `${key}FilterMode` as const;
        const updater = (f: FilterState) => {
            const newF = { ...f };
            if (newF[modeKey] === 'include') {
                if (!(newF[key] || []).includes(value)) newF[key] = [...(newF[key] || []), value];
            } else {
                newF[modeKey] = 'include';
                newF[key] = [value];
            }
            return newF;
        };
        setFormFilters(updater);
        setAppliedFilters(updater);
        setIsInitialLoad(false);
        setCurrentPage(1);
        setEntriesOffset(0);
        setActiveView('viewer');
    }, [logToConsole, setIsInitialLoad, setActiveView]);

    const handleCursorChange = useCallback((time: number) => {
        if (!db || isBusy) return;
        const nearestEntry = db.getNearestLogEntry(time, appliedFilters);
        if (!nearestEntry) return;

        const index = db.getLogEntryIndex(nearestEntry.id, appliedFilters);
        if (index === -1) return;
        
        setJumpToEntryId(nearestEntry.id);
        setIsInitialLoad(false);
        
        if (viewMode === 'pagination') {
            const newPage = Math.floor(index / pageSize) + 1;
            if (newPage !== currentPage) setCurrentPage(newPage);
        } else {
           if (filteredEntries.some(e => e.id === nearestEntry.id)) return;
           setIsBusy(true);
           const newOffset = Math.max(0, index - Math.floor(INFINITE_SCROLL_CHUNK_SIZE / 2));
           setTimeout(() => {
               const newEntries = db.queryLogEntries(appliedFilters, INFINITE_SCROLL_CHUNK_SIZE, newOffset);
               setFilteredEntries(newEntries);
               setEntriesOffset(newOffset);
               setHasMoreLogs(newOffset + newEntries.length < totalFilteredCount);
               setIsBusy(false);
           }, 50);
        }
    }, [db, isBusy, appliedFilters, viewMode, pageSize, currentPage, filteredEntries, totalFilteredCount, setJumpToEntryId, setIsInitialLoad, setIsBusy]);

    const handleFileSelect = useCallback((fileName: string) => {
        if (db) {
            const range = db.getTimeRangePerFile({ ...initialFilters, fileName: [fileName], fileNameFilterMode: 'include' });
            if (range.length > 0 && range[0].startTime && range[0].endTime) {
                const min = new Date(range[0].startTime.replace(' ', 'T') + 'Z').getTime();
                const max = new Date(range[0].endTime.replace(' ', 'T') + 'Z').getTime();
                if (min < max) setTimelineViewRange({ min, max });
            }
        }
        const updater = (f: FilterState) => ({...f, dateFrom: '', timeFrom: '', dateTo: '', timeTo: '', fileName: [fileName], fileNameFilterMode: 'include' as const});
        setFormFilters(updater);
        setAppliedFilters(updater);
        setIsInitialLoad(false);
        setCurrentPage(1);
        setEntriesOffset(0);
        setActiveView('viewer');
    }, [db, setIsInitialLoad, setActiveView]);

    const handleDateSelect = useCallback((date: string) => {
        setTimelineViewRange({ min: new Date(`${date}T00:00:00.000Z`).getTime(), max: new Date(`${date}T23:59:59.999Z`).getTime() });
        const updater = (f: FilterState) => ({...f, dateFrom: date, timeFrom: '00:00:00', dateTo: date, timeTo: '23:59:59', fileName: []});
        setFormFilters(updater);
        setAppliedFilters(updater);
        setIsInitialLoad(false);
        setCurrentPage(1);
        setEntriesOffset(0);
        setActiveView('viewer');
    }, [setIsInitialLoad, setActiveView]);

    const handleTimelineZoomToSelection = useCallback(() => {
        const startTimeStr = getSqlDateTime(appliedFilters.dateFrom, appliedFilters.timeFrom);
        const endTimeStr = getSqlDateTime(appliedFilters.dateTo, appliedFilters.timeTo, true);
        if (startTimeStr && endTimeStr) {
            const min = new Date(startTimeStr.replace(' ', 'T') + 'Z').getTime();
            const max = new Date(endTimeStr.replace(' ', 'T') + 'Z').getTime();
            if (min < max) setTimelineViewRange({ min, max });
        }
    }, [appliedFilters]);

    const handleTimelineZoomReset = useCallback(() => setTimelineViewRange(null), []);

    // Stock Logic
    const handleSearchStock = useCallback((filters: StockInfoFilters) => {
        if (!db) return;
        setIsStockBusy(true);
        setTimeout(() => {
            try {
                const { entries: results } = db.queryStockInfo(filters);
                setStockHistory(results);
                if (results.length > 0 && !overallStockTimeRange) {
                    const { minTime, maxTime } = db.getMinMaxStockTime();
                    if (minTime && maxTime) {
                        setOverallStockTimeRange({ min: minTime, max: maxTime });
                        setOverallStockDensity(db.getStockDensity({} as StockInfoFilters, 300));
                    }
                }
            } catch (e) { logToConsole(`Stock search failed: ${e instanceof Error ? e.message : String(e)}`, 'ERROR');
            } finally { setIsStockBusy(false); }
        }, 50);
    }, [db, logToConsole, setIsStockBusy, overallStockTimeRange, setOverallStockDensity, setOverallStockTimeRange]);
    
    const handleFetchStockSuggestions = useCallback(async (searchTerm: string, timeFilters: StockInfoFilters): Promise<StockArticleSuggestion[]> => {
        if (!db || searchTerm.length < 2) return [];
        try { return db.getUniqueArticles(searchTerm, timeFilters); } 
        catch (e) { logToConsole(`Failed to fetch stock suggestions: ${e instanceof Error ? e.message : String(e)}`, 'ERROR'); return []; }
    }, [db, logToConsole]);

    const handleRebuildStockData = useCallback(async () => {
        if (!db || !hasData) {
            addToast({ type: 'error', title: 'Rebuild Failed', message: 'No data is currently loaded.' });
            return;
        }
        await handleRebuildStockDataInWorker();
        // After worker is done, the SessionContext will call updateStateFromDb, which will
        // update the stock time range and density. We just need to clear the local history.
        setStockHistory([]);
    }, [db, hasData, addToast, handleRebuildStockDataInWorker]);

    const value = {
        filteredEntries, totalFilteredCount, hasMoreLogs, pageTimestampRanges, formFilters, setFormFilters,
        appliedFilters, uniqueValues, fileTimeRanges, logDensity, overallLogDensity, datesWithLogs,
        currentPage, totalPages: Math.ceil(totalFilteredCount / pageSize), pageSize, handleApplyFilters, handleResetFilters, handleClearTimeRange,
        onLoadMore, goToPage, handlePageSizeChange, handleRemoveAppliedFilter, handleContextMenuFilter, handleCursorChange,
        handleFileSelect, handleDateSelect, handleApplyDetailFilter, timelineViewRange, setTimelineViewRange,
        handleTimelineZoomToSelection, handleTimelineZoomReset, dashboardData, handleTimeRangeSelect, handleCategorySelect,
        stockHistory, setStockHistory, overallStockTimeRange, overallStockDensity, handleSearchStock, handleRebuildStockData,
        handleFetchStockSuggestions, logToConsoleForEntries: logToConsole, lastConsoleMessageForStatus: lastConsoleMessage,
        keyboardSelectedId, setKeyboardSelectedId, jumpToEntryId, isInitialLoad
    };

    return (
        <DataContext.Provider value={value}>
            {children}
        </DataContext.Provider>
    );
};

export const useData = (): DataContextType => {
    const context = useContext(DataContext);
    if (context === undefined) {
        throw new Error('useData must be used within a DataProvider');
    }
    return context;
};