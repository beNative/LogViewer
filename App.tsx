import React from 'react';
import { 
    LogEntry, ConsoleMessage, FilterState, ConsoleMessageType, DashboardData, 
    PageTimestampRange, SessionFile, ColumnVisibilityState, ColumnStyles, 
    PanelWidths, ViewMode, OverallTimeRange, FileTimeRange, LogDensityPointByLevel, 
    IconSet, LogTableDensity, Theme, Settings as SettingsType, ProgressPhase,
    StockInfoEntry, StockInfoFilters, ToastMessage, StockArticleSuggestion, LogDensityPoint,
    TimelineBarVisibility,
} from './types';
import { LogTable } from './components/LogTable';
import { ProgressIndicator } from './components/ProgressIndicator';
import { Database, getSqlDateTime } from './db';
import { Header } from './components/Header';
import { Console } from './components/Console';
import { DataHub } from './components/DataHub';
import { Icon } from './components/icons';
import { Dashboard } from './components/Dashboard';
import { Settings } from './components/Settings';
import { Info } from './components/Info';
import { StatusBar } from './components/StatusBar';
import { AboutDialog } from './components/AboutDialog';
import { StockTracker } from './components/StockTracker';
import { Toast } from './components/Toast';
import { FocusDebugger } from './components/FocusDebugger';

// JSZip is loaded from script tag
declare const JSZip: any;
const INFINITE_SCROLL_CHUNK_SIZE = 200;
const UPDATE_TOAST_ID = 'app-update-toast';

const initialFilters: FilterState = {
  dateFrom: '',
  timeFrom: '',
  dateTo: '',
  timeTo: '',
  level: [],
  levelFilterMode: 'include',
  sndrtype: [],
  sndrtypeFilterMode: 'include',
  sndrname: [],
  sndrnameFilterMode: 'include',
  fileName: [],
  fileNameFilterMode: 'include',
  includeMsg: '',
  excludeMsg: '',
  includeMsgMode: 'OR',
  excludeMsgMode: 'AND',
  sqlQuery: '',
  sqlQueryEnabled: false,
};

const initialConsoleFilters: Record<ConsoleMessageType, boolean> = {
    DEBUG: true,
    INFO: true,
    WARNING: true,
    ERROR: true,
};

const initialDashboardData: DashboardData = {
    timeline: [],
    levels: [],
    senderTypes: [],
};

const initialColumnVisibility: ColumnVisibilityState = {
  time: true,
  level: true,
  sndrtype: true,
  sndrname: true,
  fileName: true,
  msg: true,
};

const MONO_FONT_STACK = 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace';

const initialColumnStyles: ColumnStyles = {
  time: { font: MONO_FONT_STACK, fontSize: 13, isBold: false, isItalic: false, color: '#6B7280', darkColor: '#9CA3AF' },
  level: { font: 'sans-serif', fontSize: 12, isBold: true, isItalic: false, color: '', darkColor: '' },
  sndrtype: { font: 'sans-serif', fontSize: 14, isBold: false, isItalic: false, color: '#374151', darkColor: '#D1D5DB' },
  sndrname: { font: 'sans-serif', fontSize: 14, isBold: false, isItalic: false, color: '#374151', darkColor: '#D1D5DB' },
  fileName: { font: 'sans-serif', fontSize: 13, isBold: false, isItalic: false, color: '#6B7280', darkColor: '#9CA3AF' },
  msg: { font: MONO_FONT_STACK, fontSize: 13, isBold: false, isItalic: false, color: '#1F2937', darkColor: '#F3F4F6' },
};

const initialPanelWidths: PanelWidths = {
    filters: 320, // w-80
    details: 500
};

const initialTimelineBarVisibility: TimelineBarVisibility = {
    pages: true,
    files: true,
    dates: true,
    density: true,
    overview: true,
};

const App: React.FC = () => {
  const [db, setDb] = React.useState<Database | null>(null);
  const [hasData, setHasData] = React.useState<boolean>(false);
  
  const [filteredEntries, setFilteredEntries] = React.useState<LogEntry[]>([]);
  const [entriesOffset, setEntriesOffset] = React.useState(0);
  const [totalFilteredCount, setTotalFilteredCount] = React.useState(0);
  const [totalEntryCount, setTotalEntryCount] = React.useState(0);
  const [overallTimeRange, setOverallTimeRange] = React.useState<OverallTimeRange | null>(null);
  const [timelineViewRange, setTimelineViewRange] = React.useState<OverallTimeRange | null>(null);

  const [error, setError] = React.useState<string | null>(null);
  const [loadedFileNames, setLoadedFileNames] = React.useState<string[]>([]);
  
  const [isLoading, setIsLoading] = React.useState<boolean>(false);
  const [isBusy, setIsBusy] = React.useState<boolean>(false);
  const [progress, setProgress] = React.useState<number>(0);
  const [progressMessage, setProgressMessage] = React.useState<string>('');
  const [progressPhase, setProgressPhase] = React.useState<ProgressPhase>('reading');
  const [detailedProgress, setDetailedProgress] = React.useState({
    currentFile: '',
    fileBytesRead: 0,
    fileTotalBytes: 0,
    fileLogCount: null as number | null,
  });
  
  const [consoleMessages, setConsoleMessages] = React.useState<ConsoleMessage[]>([]);
  const [lastConsoleMessage, setLastConsoleMessage] = React.useState<ConsoleMessage | null>(null);
  const [consoleFilters, setConsoleFilters] = React.useState(initialConsoleFilters);
  const [consoleSearchTerm, setConsoleSearchTerm] = React.useState('');

  const [currentPage, setCurrentPage] = React.useState<number>(1);
  const [pageSize, setPageSize] = React.useState<number>(1000);
  const [pageTimestampRanges, setPageTimestampRanges] = React.useState<PageTimestampRange[]>([]);
  const [formFilters, setFormFilters] = React.useState<FilterState>(initialFilters);
  const [appliedFilters, setAppliedFilters] = React.useState<FilterState>(initialFilters);

  const [uniqueValues, setUniqueValues] = React.useState<{
    level: string[];
    sndrtype: string[];
    sndrname: string[];
    fileName: string[];
  }>({ level: [], sndrtype: [], sndrname: [], fileName: [] });
  
  const [dashboardData, setDashboardData] = React.useState<DashboardData>(initialDashboardData);
  const [fileTimeRanges, setFileTimeRanges] = React.useState<FileTimeRange[]>([]);
  const [logDensity, setLogDensity] = React.useState<LogDensityPointByLevel[]>([]);
  const [overallLogDensity, setOverallLogDensity] = React.useState<LogDensityPointByLevel[]>([]);
  const [datesWithLogs, setDatesWithLogs] = React.useState<string[]>([]);

  const [activeView, setActiveView] = React.useState<'data' | 'viewer' | 'dashboard' | 'console' | 'settings' | 'info' | 'stock'>('data');
  const [theme, setTheme] = React.useState<Theme>('light');
  const [viewMode, setViewMode] = React.useState<ViewMode>('pagination');
  const [iconSet, setIconSet] = React.useState<IconSet>('sharp');
  const [columnVisibility, setColumnVisibility] = React.useState<ColumnVisibilityState>(initialColumnVisibility);
  const [columnStyles, setColumnStyles] = React.useState<ColumnStyles>(initialColumnStyles);
  const [customFilterPresets, setCustomFilterPresets] = React.useState<Record<string, FilterState>>({});
  const [panelWidths, setPanelWidths] = React.useState<PanelWidths>(initialPanelWidths);
  const [isTimeRangeSelectorVisible, setIsTimeRangeSelectorVisible] = React.useState(true);
  const [isDetailPanelVisible, setIsDetailPanelVisible] = React.useState(false);
  const [isFocusDebuggerVisible, setIsFocusDebuggerVisible] = React.useState<boolean>(false);
  const [timelineBarVisibility, setTimelineBarVisibility] = React.useState<TimelineBarVisibility>(initialTimelineBarVisibility);
  const [logTableDensity, setLogTableDensity] = React.useState<LogTableDensity>('normal');
  const [allowPrerelease, setAllowPrerelease] = React.useState<boolean>(false);
  const [isAutoUpdateEnabled, setIsAutoUpdateEnabled] = React.useState<boolean>(true);
  const [githubToken, setGithubToken] = React.useState<string>('');
  const [uiScale, setUiScale] = React.useState<number>(1);
  
  const [hasMoreLogs, setHasMoreLogs] = React.useState(true);
  const [keyboardSelectedId, setKeyboardSelectedId] = React.useState<number | null>(null);
  const [jumpToEntryId, setJumpToEntryId] = React.useState<number | null>(null);
  const [isInitialLoad, setIsInitialLoad] = React.useState(true);
  const [toasts, setToasts] = React.useState<ToastMessage[]>([]);

  // Stock Tracker State
  const [stockHistory, setStockHistory] = React.useState<StockInfoEntry[]>([]);
  const [isStockBusy, setIsStockBusy] = React.useState<boolean>(false);
  const [overallStockTimeRange, setOverallStockTimeRange] = React.useState<{ min: string, max: string } | null>(null);
  const [overallStockDensity, setOverallStockDensity] = React.useState<LogDensityPoint[]>([]);


  // Electron-specific state
  const [isElectron, setIsElectron] = React.useState(false);
  const [sessions, setSessions] = React.useState<SessionFile[]>([]);
  const [activeSessionName, setActiveSessionName] = React.useState<string | null>(null);
  const [isDirty, setIsDirty] = React.useState<boolean>(false);
  const busyTaskRef = React.useRef(0);
  const [isAboutDialogOpen, setIsAboutDialogOpen] = React.useState(false);

  const addToast = React.useCallback((toast: Omit<ToastMessage, 'id'> & { id?: string }) => {
    const id = toast.id || new Date().getTime().toString() + Math.random();
    setToasts(prev => {
        // Avoid duplicate toasts if one with the same ID already exists
        if (prev.some(t => t.id === id)) return prev;
        return [...prev, { ...toast, id }];
    });
  }, []);

  const removeToast = React.useCallback((id: string) => {
      setToasts(prev => prev.filter(t => t.id !== id));
  }, []);


  const logToConsole = React.useCallback((message: string, type: ConsoleMessage['type']) => {
    // Also send to electron main process for file logging if available
    if (window.electronAPI?.logMessage) {
        window.electronAPI.logMessage(type, message);
    }
    const newMessage: ConsoleMessage = { 
        message, 
        type, 
        timestamp: new Date().toLocaleTimeString('en-US', { hour12: false }) 
    };
    setConsoleMessages(prev => [...prev, newMessage]);
    setLastConsoleMessage(newMessage);
  }, []);
  
  const handleClearConsole = React.useCallback(() => {
    setConsoleMessages([]);
    logToConsole('Console cleared.', 'INFO');
  }, [logToConsole]);

  const handleLoadMore = React.useCallback(() => {
    if (!db || !hasMoreLogs || isBusy) return;

    setIsBusy(true);
    // Defer the database query to the next event loop tick.
    // This allows the UI to update with the `isBusy=true` state immediately,
    // preventing multiple, simultaneous calls to this function from rapid-fire events.
    setTimeout(() => {
        try {
            const newOffset = entriesOffset + INFINITE_SCROLL_CHUNK_SIZE;
            logToConsole(`Loading more logs at offset ${newOffset.toLocaleString()}...`, 'DEBUG');

            const newEntries = db.queryLogEntries(appliedFilters, INFINITE_SCROLL_CHUNK_SIZE, newOffset);
            
            setFilteredEntries(prev => [...prev, ...newEntries]);
            setEntriesOffset(newOffset);
            setHasMoreLogs(newOffset + newEntries.length < totalFilteredCount);
            logToConsole(`Loaded ${newEntries.length} more logs.`, 'DEBUG');
        } catch(e) {
            const msg = e instanceof Error ? e.message : String(e);
            setError(msg);
            logToConsole(`Error loading more logs: ${msg}`, 'ERROR');
        } finally {
            setIsBusy(false);
        }
    }, 0);
  }, [db, hasMoreLogs, isBusy, entriesOffset, appliedFilters, totalFilteredCount, logToConsole]);

  const fetchSessions = React.useCallback(async () => {
    if (!window.electronAPI) return;
    try {
        const fetchedSessions = await window.electronAPI.listSessions();
        setSessions(fetchedSessions);
    } catch (e) {
        const msg = e instanceof Error ? e.message : String(e);
        logToConsole(`Failed to fetch sessions: ${msg}`, 'ERROR');
    }
  }, [logToConsole]);
  
  const handleNewSession = React.useCallback(async (log = true) => {
    // Create a new in-memory DB.
    const newDb = await Database.create();
    // Update the app's state to use this new DB.
    setDb(newDb);

    // Reset all other relevant state to initial values.
    setFilteredEntries([]);
    setEntriesOffset(0);
    setError(null);
    setLoadedFileNames([]);
    setCurrentPage(1);
    setFormFilters(initialFilters);
    setAppliedFilters(initialFilters);
    setTotalFilteredCount(0);
    setTotalEntryCount(0);
    setHasData(false);
    setOverallTimeRange(null);
    setOverallStockTimeRange(null);
    setOverallStockDensity([]);
    setOverallLogDensity([]);
    setTimelineViewRange(null);
    setActiveSessionName(null);
    setUniqueValues({ level: [], sndrtype: [], sndrname: [], fileName: [] });
    setDashboardData(initialDashboardData);
    setStockHistory([]);
    setIsInitialLoad(true);
    setIsDirty(false); // A new blank session is not dirty
    if (log) logToConsole('Started new blank session.', 'INFO');
  }, [logToConsole]);

  const updateStateFromDb = React.useCallback(async (newDb: Database, fromSessionLoad: boolean) => {
    logToConsole('Reading metadata from database...', 'DEBUG');

    const totalEntries = newDb.getTotalEntryCount();
    setTotalEntryCount(totalEntries);
    const hasLogData = totalEntries > 0;

    const { minTime: minStockTime, maxTime: maxStockTime } = newDb.getMinMaxStockTime();
    const hasStockData = !!(minStockTime && maxStockTime);

    if (!hasLogData && !hasStockData) {
        await handleNewSession(false);
        logToConsole('Database is empty.', 'INFO');
        return;
    }

    // If we reach here, there's some data to process.
    setHasData(true);
    setCurrentPage(1);
    setEntriesOffset(0);
    setIsInitialLoad(true);

    // Process stock data if it exists
    if (hasStockData) {
        setOverallStockTimeRange({ min: minStockTime!, max: maxStockTime! });
        setOverallStockDensity(newDb.getStockDensity({} as StockInfoFilters, 300));
        logToConsole('Stock data found and initialized.', 'DEBUG');
    } else {
        setOverallStockTimeRange(null);
        setOverallStockDensity([]);
    }

    // Process log data if it exists
    if (hasLogData) {
        let newFilters = { ...initialFilters };
        let filtersLoadedFromSession = false;

        const { minTime, maxTime } = newDb.getMinMaxTime();
        if (minTime && maxTime) {
            const overallRange = {
                min: new Date(minTime + 'Z').getTime(),
                max: new Date(maxTime + 'Z').getTime()
            };
            setOverallTimeRange(overallRange);
            setOverallLogDensity(newDb.getLogDensityByLevel(initialFilters, 300));
        }

        if (fromSessionLoad) {
            const savedFiltersJson = newDb.getMeta('appliedFilters');
            if (savedFiltersJson) {
                try {
                    let savedFilters = JSON.parse(savedFiltersJson);
                    
                    const migrateOldFilters = (oldFilters: any): Partial<FilterState> => {
                        const migrated: Partial<FilterState> = { ...oldFilters };
                        const attributes: ['level', 'sndrtype', 'sndrname', 'fileName'] = ['level', 'sndrtype', 'sndrname', 'fileName'];
                        let wasMigrated = false;
                        
                        attributes.forEach(attr => {
                            const excludeKey = `${attr}Exclude` as const;
                            const modeKey = `${attr}FilterMode` as const;

                            if (migrated.hasOwnProperty(excludeKey)) {
                                wasMigrated = true;
                                const includeList = migrated[attr] || [];
                                const excludeList = migrated[excludeKey] || [];
                                
                                if (includeList.length > 0) {
                                    // Prioritize include list if both exist
                                    migrated[modeKey] = 'include';
                                } else if (excludeList.length > 0) {
                                    // Otherwise, use exclude list
                                    migrated[attr] = excludeList;
                                    migrated[modeKey] = 'exclude';
                                }
                                delete (migrated as any)[excludeKey];
                            }
                        });
                        if (wasMigrated) {
                             logToConsole('Migrating filter settings from older session format.', 'INFO');
                        }
                        return migrated;
                    };

                    savedFilters = migrateOldFilters(savedFilters);
                    
                    newFilters = { ...initialFilters, ...savedFilters };
                    filtersLoadedFromSession = true;
                    logToConsole('Loaded filter settings from session.', 'INFO');
                } catch(e) {
                    logToConsole('Could not parse filters from session, using defaults.', 'WARNING');
                }
            }
        }
        
        if (!filtersLoadedFromSession && minTime && maxTime) {
              const [minDate, minTimeStr] = minTime.split(' ');
              const [maxDate, maxTimeStr] = maxTime.split(' ');
              newFilters = {
                ...initialFilters,
                dateFrom: minDate,
                timeFrom: minTimeStr.substring(0, 8),
                dateTo: maxDate,
                timeTo: maxTimeStr.substring(0, 8),
              };
        }
        
        const uniqueFileNames = newDb.getUniqueColumnValues('fileName');
        setLoadedFileNames(uniqueFileNames);
        setFormFilters(newFilters);
        setAppliedFilters(newFilters);
        setUniqueValues({
            level: newDb.getUniqueColumnValues('level'),
            sndrtype: newDb.getUniqueColumnValues('sndrtype'),
            sndrname: newDb.getUniqueColumnValues('sndrname'),
            fileName: uniqueFileNames,
        });
        logToConsole(`Database loaded. Total entries: ${totalEntries}. Apply filters to view.`, 'INFO');
    } else {
        // If there is only stock data, we should still set some sensible defaults
        // for the log viewer part of the state to avoid errors, even if it won't be shown.
        setFormFilters(initialFilters);
        setAppliedFilters(initialFilters);
        setUniqueValues({ level: [], sndrtype: [], sndrname: [], fileName: [] });
        setLoadedFileNames(newDb.getUniqueColumnValues('fileName')); // Show loaded files
        logToConsole('Database contains only stock data. Log viewer is empty.', 'INFO');
    }
  }, [logToConsole, handleNewSession]);

  React.useEffect(() => {
    const isElectronEnv = !!window.electronAPI;
    setIsElectron(isElectronEnv);
    logToConsole('Initializing application...', 'INFO');

    if (isElectronEnv) {
        logToConsole('Electron environment detected. Loading settings...', 'DEBUG');
        fetchSessions();
        window.electronAPI.getSettings()
            .then(settings => {
                logToConsole('Settings loaded successfully.', 'DEBUG');
                if (settings.theme === 'light' || settings.theme === 'dark') {
                    setTheme(settings.theme);
                    logToConsole(`Theme set to '${settings.theme}' from settings.`, 'DEBUG');
                }
                if (settings.viewMode === 'pagination' || settings.viewMode === 'scroll') {
                    setViewMode(settings.viewMode);
                    logToConsole(`View mode set to '${settings.viewMode}' from settings.`, 'DEBUG');
                }
                if (settings.iconSet) {
                    setIconSet(settings.iconSet);
                    logToConsole(`Icon set to '${settings.iconSet}' from settings.`, 'DEBUG');
                }
                if (settings.logTableDensity) {
                    setLogTableDensity(settings.logTableDensity);
                    logToConsole(`Log table density set to '${settings.logTableDensity}' from settings.`, 'DEBUG');
                }
                if (settings.columnVisibility) {
                    setColumnVisibility({ ...initialColumnVisibility, ...settings.columnVisibility });
                    logToConsole('Column visibility settings loaded.', 'DEBUG');
                }
                if (settings.columnStyles) {
                    // Deep merge to handle newly added style properties like fontSize
                    const mergedStyles = { ...initialColumnStyles };
                    for (const key in mergedStyles) {
                        if (settings.columnStyles[key as keyof ColumnStyles]) {
                            mergedStyles[key as keyof ColumnStyles] = { 
                                ...mergedStyles[key as keyof ColumnStyles], 
                                ...settings.columnStyles[key as keyof ColumnStyles] 
                            };
                        }
                    }
                    setColumnStyles(mergedStyles);
                    logToConsole('Column style settings loaded.', 'DEBUG');
                }
                if (settings.customFilterPresets) {
                    setCustomFilterPresets(settings.customFilterPresets);
                    logToConsole('Custom filter presets loaded.', 'DEBUG');
                }
                if (settings.panelWidths) {
                    setPanelWidths({ ...initialPanelWidths, ...settings.panelWidths });
                    logToConsole('Panel widths loaded.', 'DEBUG');
                }
                 if (typeof settings.isTimeRangeSelectorVisible === 'boolean') {
                    setIsTimeRangeSelectorVisible(settings.isTimeRangeSelectorVisible);
                    logToConsole(`Timeline visibility set to '${settings.isTimeRangeSelectorVisible}' from settings.`, 'DEBUG');
                }
                if (typeof settings.isDetailPanelVisible === 'boolean') {
                    setIsDetailPanelVisible(settings.isDetailPanelVisible);
                    logToConsole(`Details panel visibility set to '${settings.isDetailPanelVisible}' from settings.`, 'DEBUG');
                }
                if (typeof settings.isFocusDebuggerVisible === 'boolean') {
                    setIsFocusDebuggerVisible(settings.isFocusDebuggerVisible);
                    logToConsole(`Focus debugger visibility set to '${settings.isFocusDebuggerVisible}' from settings.`, 'DEBUG');
                }
                if (settings.timelineBarVisibility) {
                    setTimelineBarVisibility({ ...initialTimelineBarVisibility, ...settings.timelineBarVisibility });
                    logToConsole('Timeline bar visibility settings loaded.', 'DEBUG');
                }
                if (typeof settings.allowPrerelease === 'boolean') {
                    setAllowPrerelease(settings.allowPrerelease);
                    logToConsole(`Pre-release updates set to '${settings.allowPrerelease}' from settings.`, 'DEBUG');
                }
                if (typeof settings.isAutoUpdateEnabled === 'boolean') {
                    setIsAutoUpdateEnabled(settings.isAutoUpdateEnabled);
                    logToConsole(`Auto-updates set to '${settings.isAutoUpdateEnabled}' from settings.`, 'DEBUG');
                }
                if (typeof settings.githubToken === 'string') {
                    setGithubToken(settings.githubToken);
                    logToConsole(`GitHub token loaded from settings.`, 'DEBUG');
                }
                if (typeof settings.uiScale === 'number') {
                    setUiScale(settings.uiScale);
                    logToConsole(`UI Scale set to '${(settings.uiScale * 100).toFixed(0)}%' from settings.`, 'DEBUG');
                }
            })
            .catch(err => {
                const msg = err instanceof Error ? err.message : String(err);
                logToConsole(`Failed to load settings: ${msg}`, 'ERROR');
            });
    }

    logToConsole('Initializing database...', 'INFO');
    Database.create().then(database => {
        setDb(database);
        logToConsole('Database ready.', 'DEBUG');
    }).catch(e => {
        const msg = e instanceof Error ? e.message : 'Unknown error';
        logToConsole(`Database initialization failed: ${msg}`, 'ERROR');
        setError(`Database initialization failed: ${msg}`);
    });
  }, [logToConsole, fetchSessions]);
  
  // Auto-update listener effect
  React.useEffect(() => {
    if (!isElectron || !window.electronAPI.onUpdateStatus) return;

    const handleUpdate = (status: string, data: any) => {
        switch(status) {
            case 'checking':
                logToConsole('Checking for application updates...', 'INFO');
                addToast({ type: 'info', title: 'Updates', message: 'Checking for updates...', duration: 3000 });
                break;
            case 'available':
                logToConsole(`New update available: v${data.info.version}. Starting download...`, 'INFO');
                addToast({
                    id: UPDATE_TOAST_ID,
                    type: 'progress',
                    title: `Downloading v${data.info.version}`,
                    message: 'A new update is available and is being downloaded.',
                    progress: 0,
                    duration: 0, // Persistent
                });
                break;
            case 'not-available':
                logToConsole(`Application is up to date (v${data.info.version}).`, 'INFO');
                addToast({ type: 'success', title: 'Updates', message: `You're on the latest version (${data.info.version}).`, duration: 4000 });
                break;
            case 'progress':
                // Do not log this event to avoid spamming the console. The toast is sufficient for user feedback.
                setToasts(prev => prev.map(t =>
                    t.id === UPDATE_TOAST_ID
                        ? { ...t, progress: data.progress.percent, message: `Downloaded ${Math.round(data.progress.percent)}%` }
                        : t
                ));
                break;
            case 'downloaded':
                logToConsole(`Update v${data.info.version} downloaded successfully. Ready to install on restart.`, 'INFO');
                setToasts(prev => prev.map(t =>
                    t.id === UPDATE_TOAST_ID
                        ? {
                            ...t,
                            type: 'success',
                            title: 'Update Ready!',
                            message: `Version ${data.info.version} is ready. Restart to install.`,
                            progress: 100,
                            actions: [{ label: 'Restart & Install', onClick: () => window.electronAPI.installUpdate() }]
                        }
                        : t
                ));
                break;
            case 'error':
                 logToConsole(`Update error: ${data.error}`, 'ERROR');
                 addToast({ type: 'error', title: 'Update Error', message: `An error occurred: ${data.error}`, duration: 8000 });
                 removeToast(UPDATE_TOAST_ID);
                 break;
        }
    };
    
    const removeListener = window.electronAPI.onUpdateStatus(handleUpdate);
    return () => removeListener();
  }, [isElectron, addToast, removeToast, logToConsole]);

  React.useEffect(() => {
    const root = window.document.documentElement;
    if (theme === 'light') {
        root.classList.remove('dark');
    } else {
        root.classList.add('dark');
    }
  }, [theme]);
  
  React.useEffect(() => {
    // This effect manages the application's global zoom level.
    // By applying `zoom` to the `<body>` element, the entire UI is scaled.
    // To avoid issues with `vh` units being scaled incorrectly, the application's root
    // element uses `h-full` and we ensure `<html>` and `<body>` are also `h-full` via CSS.
    // This provides a more robust scaling model than `transform: scale` without text blurring.
    const bodyEl = document.body;
    (bodyEl.style as any).zoom = uiScale === 1 ? '' : `${uiScale}`;

    // A cleanup function is essential to remove the style when the component unmounts.
    return () => {
        (bodyEl.style as any).zoom = '';
    };
  }, [uiScale]);

  // This single effect handles all data fetching and state updates when filters or pagination change.
  // It replaces the two previous, separate effects to prevent race conditions and ensure UI consistency.
  React.useEffect(() => {
      if (!db || !hasData) {
          // Ensure a clean state if there's no data
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
          setFilteredEntries([]); // Clear old entries before fetching new ones, but not on initial load
      }
      setEntriesOffset(0);

      // Use a timeout to allow the spinner to render before blocking the main thread for synchronous DB calls.
      busyTaskRef.current = window.setTimeout(() => {
          logToConsole(`Updating view...`, 'DEBUG');

          // 1. Get total count and metadata first
          const count = db.getFilteredLogCount(appliedFilters);
          setTotalFilteredCount(count);

          const newDashboardData = {
              timeline: db.getLogVolumeByInterval(appliedFilters),
              levels: db.getCountsByColumn('level', appliedFilters),
              senderTypes: db.getCountsByColumn('sndrtype', appliedFilters),
          };
          setDashboardData(newDashboardData);
          setFileTimeRanges(db.getTimeRangePerFile(appliedFilters));
          setLogDensity(db.getLogDensityByLevel(appliedFilters, 200));
          setDatesWithLogs(db.getDatesWithLogs(appliedFilters));
          
          // 2. Conditionally fetch the actual log entries
          if (count > 0 && !isInitialLoad) {
              if (viewMode === 'pagination') {
                  const entries = db.queryLogEntries(appliedFilters, pageSize, (currentPage - 1) * pageSize);
                  const ranges = db.getPageTimestampRanges(appliedFilters, pageSize);
                  setFilteredEntries(entries);
                  setPageTimestampRanges(ranges);
                  setHasMoreLogs(false);
              } else { // 'scroll' mode (Infinite Scroll)
                  const entries = db.queryLogEntries(appliedFilters, INFINITE_SCROLL_CHUNK_SIZE, 0);
                  setFilteredEntries(entries);
                  setEntriesOffset(0);
                  setHasMoreLogs(entries.length < count);
                  setPageTimestampRanges([]);
              }
              logToConsole(`View updated. Found ${count} matching entries.`, 'DEBUG');
          } else {
              // If no results, or if it's an initial load, clear entries.
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

      // Cleanup timeout on unmount or re-run
      return () => clearTimeout(busyTaskRef.current);
  }, [db, hasData, appliedFilters, currentPage, pageSize, viewMode, logToConsole, isInitialLoad]);

  // This effect handles the final step of jumping to a specific log entry.
  // It runs after data fetching is complete, ensuring the target entry is visible before selection.
  React.useEffect(() => {
    if (jumpToEntryId && filteredEntries.length > 0) {
        const entryIsVisible = filteredEntries.some(e => e.id === jumpToEntryId);
        if (entryIsVisible) {
            setKeyboardSelectedId(jumpToEntryId);
            setJumpToEntryId(null); // Reset trigger
        }
    }
  }, [jumpToEntryId, filteredEntries]);

  
  const saveCurrentDbAsSession = React.useCallback(async (dbToSave: Database, name: string): Promise<boolean> => {
    if (!isElectron || !dbToSave) return false;

    // Persist the currently active filters to the session DB's metadata before exporting it.
    // This ensures that when the session is reloaded, it starts with the same view.
    try {
        dbToSave.setMeta('appliedFilters', JSON.stringify(appliedFilters));
        logToConsole('Saved current filter configuration to the session database.', 'DEBUG');
    } catch (e) {
        const msg = e instanceof Error ? e.message : String(e);
        logToConsole(`Could not save filters to session metadata: ${msg}`, 'WARNING');
    }
    
    const buffer = dbToSave.export();
    const totalEntriesInDb = dbToSave.getTotalEntryCount();
    if (buffer.length <= 1024 && totalEntriesInDb === 0) {
        logToConsole('Database is empty, skipping session save.', 'DEBUG');
        return true; // Technically not a failure
    }
    
    try {
        logToConsole(`Saving session as '${name}'...`, 'INFO');
        const { success, error } = await window.electronAPI.saveSession(buffer, name);
        if (success) {
            logToConsole(`Session saved successfully.`, 'INFO');
            setActiveSessionName(name);
            setIsDirty(false);
            await fetchSessions();
            return true;
        } else {
            throw new Error(error || 'Unknown error saving session.');
        }
    } catch(e) {
        const msg = e instanceof Error ? e.message : String(e);
        setError(msg);
        logToConsole(`Failed to save session: ${msg}`, 'ERROR');
        return false;
    }
  }, [isElectron, logToConsole, fetchSessions, appliedFilters, setActiveSessionName, setIsDirty, setError]);

  const handleSaveSession = React.useCallback(async () => {
    if (!db) {
        logToConsole('Cannot save session, no database is active.', 'WARNING');
        return false;
    }
    if (activeSessionName) {
      return await saveCurrentDbAsSession(db, activeSessionName);
    } else {
      // If the session is new (no active name), generate one automatically.
      const now = new Date();
      const timestamp = now.toISOString().slice(0, 19).replace(/:/g, '-').replace('T', '_');
      const newSessionName = `session_${timestamp}.sqlite`;
      logToConsole(`New session detected. Saving with generated name: ${newSessionName}`, 'INFO');
      return await saveCurrentDbAsSession(db, newSessionName);
    }
  }, [activeSessionName, saveCurrentDbAsSession, logToConsole, db]);

  const processFilesToDb = React.useCallback(async (files: FileList, targetDb: Database) => {
    setIsLoading(true);
    setProgress(0);
    setError(null);
    logToConsole(`Processing ${files.length} file(s)...`, 'INFO');
    setDetailedProgress({ currentFile: '', fileBytesRead: 0, fileTotalBytes: 0, fileLogCount: null });

    targetDb.createTable();

    // --- Phase 1: Read and Unzip files (0-30% of progress) ---
    setProgressMessage('Calculating file sizes...');
    setProgressPhase('reading');
    const fileList = Array.from(files);
    const totalBytesToRead = fileList.reduce((acc, file) => acc + file.size, 0);
    let bytesReadSoFar = 0;
    const xmlFileObjects: { name: string, content: string, size: number }[] = [];

    for (const file of fileList) {
        setDetailedProgress({ currentFile: file.name, fileBytesRead: 0, fileTotalBytes: file.size, fileLogCount: null });
        if (file.name.toLowerCase().endsWith('.zip')) {
            setProgressPhase('unzipping');
            setProgressMessage(`Unzipping ${file.name}...`);
            logToConsole(`Unzipping ${file.name}...`, 'DEBUG');
            
            try {
                const zip = await JSZip.loadAsync(file, {
                    update: (metadata: any) => {
                        const fileProgressBytes = file.size * (metadata.percent / 100);
                        setProgress(((bytesReadSoFar + fileProgressBytes) / totalBytesToRead) * 30);
                        setDetailedProgress(prev => ({...prev, fileBytesRead: fileProgressBytes }));
                    }
                });
                
                const xmlZipEntries = (Object.values(zip.files) as any[]).filter((entry: any) => !entry.dir && entry.name.toLowerCase().endsWith('.xml'));
                for (const zipEntry of xmlZipEntries) {
                    const content = await zipEntry.async('string');
                    xmlFileObjects.push({ name: `${file.name}/${zipEntry.name}`, content, size: content.length });
                }

            } catch(e) {
                 logToConsole(`Failed to unzip ${file.name}. It might be corrupted. Skipping.`, 'ERROR');
            }
            bytesReadSoFar += file.size;

        } else if (file.name.toLowerCase().endsWith('.xml')) {
            setProgressPhase('reading');
            setProgressMessage(`Reading ${file.name}...`);
            logToConsole(`Reading ${file.name}...`, 'DEBUG');
            
            try {
                const content = await new Promise<string>((resolve, reject) => {
                    const reader = new FileReader();
                    reader.onprogress = (event) => {
                        if (event.lengthComputable) {
                            setProgress(((bytesReadSoFar + event.loaded) / totalBytesToRead) * 30);
                            setDetailedProgress(prev => ({ ...prev, fileBytesRead: event.loaded }));
                        }
                    };
                    reader.onload = () => resolve(reader.result as string);
                    reader.onerror = () => reject(new Error(`Error reading file: ${file.name}`));
                    reader.readAsText(file);
                });
                xmlFileObjects.push({ name: file.name, content, size: content.length });

            } catch(e) {
                logToConsole((e as Error).message, 'ERROR');
            }
            bytesReadSoFar += file.size;

        } else {
             logToConsole(`Skipped file (not .xml or .zip): ${file.name}`, 'WARNING');
             bytesReadSoFar += file.size; // Still count it as "read"
             setProgress((bytesReadSoFar / totalBytesToRead) * 30);
        }
    }
    
    if (xmlFileObjects.length === 0) {
      throw new Error("No .xml files found to process.");
    }
    
    // --- Phase 2: Parse and Insert (30-100% of progress) ---
    const totalXmlSize = xmlFileObjects.reduce((acc, file) => acc + file.size, 0);
    let processedXmlSize = 0;
    
    // SPEED OPTIMIZATION: Drop indexes before bulk insert
    logToConsole('Dropping indexes for faster insertion...', 'DEBUG');
    targetDb.dropIndexes();
    let insertionError: Error | null = null;
    
    try {
      for (const file of xmlFileObjects) {
          setProgressPhase('parsing');
          setProgressMessage(`Parsing: ${file.name}`);
          setDetailedProgress({
              currentFile: file.name,
              fileBytesRead: file.size, // Reading is done
              fileTotalBytes: file.size,
              fileLogCount: null,
          });
          
          const sanitizedContent = file.content.replace(/[\u0000-\u0008\u000B\u000C\u000E-\u001F\u007F]/g, '');
          const parser = new DOMParser();
          const xmlDoc = parser.parseFromString(sanitizedContent, 'application/xml');
          
          const parserError = xmlDoc.getElementsByTagName('parsererror');
          if (parserError.length > 0) {
            logToConsole(`Error parsing XML in ${file.name}, skipping.`, 'ERROR');
            processedXmlSize += file.size;
            continue;
          }

          const logNodes = xmlDoc.querySelectorAll('logging > log');
          const totalEntriesInFile = logNodes.length;
          setDetailedProgress(prev => ({ ...prev, fileLogCount: totalEntriesInFile }));

          if (totalEntriesInFile === 0) {
            logToConsole(`No <log> items found in ${file.name}, skipping.`, 'WARNING');
            processedXmlSize += file.size;
            continue;
          }
          
          setProgressPhase('inserting');
          setProgressMessage(`Inserting ${totalEntriesInFile.toLocaleString()} records from: ${file.name}`);

          const entries: Omit<LogEntry, 'id' | 'fileName'>[] = [];
          const stockEntries: Omit<StockInfoEntry, 'id'>[] = [];

          Array.from(logNodes).forEach(node => {
              const msg = node.getAttribute('msg') || '';
              const time = node.getAttribute('time') || 'N/A';

              if (msg.includes('<WWKS') && msg.includes('<StockInfoMessage')) {
                  try {
                      const xmlStartIndex = msg.indexOf('<WWKS');
                      const xmlEndIndex = msg.lastIndexOf('</WWKS>') + 7;

                      if (xmlStartIndex > -1 && xmlEndIndex > xmlStartIndex) {
                          const xmlFragment = msg.substring(xmlStartIndex, xmlEndIndex);
                          const wwksParser = new DOMParser();
                          const wwksDoc = wwksParser.parseFromString(xmlFragment, "application/xml");
                          
                          if (wwksDoc.getElementsByTagName("parsererror").length === 0) {
                              const wwksNode = wwksDoc.querySelector('WWKS');
                              const stockNode = wwksDoc.querySelector('StockInfoMessage');
                              const articleNode = stockNode?.querySelector('Article');
                              
                              if (wwksNode && stockNode && articleNode) {
                                  let parsedTimestamp: string;
                                  const wwksTimestamp = wwksNode.getAttribute('TimeStamp');
                                  if (wwksTimestamp) {
                                      parsedTimestamp = new Date(wwksTimestamp).toISOString();
                                  } else {
                                      parsedTimestamp = new Date(time.replace(/ (\d+)$/, '.$1') + 'Z').toISOString();
                                  }

                                  const stockEntry = {
                                      timestamp: parsedTimestamp,
                                      message_id: parseInt(stockNode.getAttribute('Id') || '0', 10),
                                      source: stockNode.getAttribute('Source') || 'N/A',
                                      destination: stockNode.getAttribute('Destination') || 'N/A',
                                      article_id: articleNode.getAttribute('Id') || 'N/A',
                                      article_name: articleNode.getAttribute('Name') || 'N/A',
                                      dosage_form: articleNode.getAttribute('DosageForm') || 'N/A',
                                      max_sub_item_quantity: parseInt(articleNode.getAttribute('MaxSubItemQuantity') || '0', 10),
                                      quantity: parseInt(articleNode.getAttribute('Quantity') || '0', 10),
                                  };
                                  stockEntries.push(stockEntry);
                              }
                          }
                      }
                  } catch (e) {
                      logToConsole(`Failed to parse StockInfoMessage from log message in ${file.name}. Error: ${e instanceof Error ? e.message : String(e)}`, 'WARNING');
                  }
              }

              entries.push({
                  level: node.getAttribute('level') || 'N/A',
                  time: time,
                  sndrtype: node.getAttribute('sndrtype') || 'N/A',
                  sndrname: node.getAttribute('sndrname') || 'N/A',
                  msg: msg,
              });
          });


          const onProgressCallback = (processedInFile: number) => {
              const progressWithinFile = totalEntriesInFile > 0 
                  ? (processedInFile / totalEntriesInFile) * file.size 
                  : file.size;
              const currentTotalProgress = processedXmlSize + progressWithinFile;
              // This phase contributes 70% of the total progress (from 30 to 100)
              setProgress(Math.min(100, 30 + (currentTotalProgress / totalXmlSize) * 70));
          };

          targetDb.insertLogs(entries, file.name, onProgressCallback);
          if (stockEntries.length > 0) {
              logToConsole(`Found and inserting ${stockEntries.length} stock info messages from ${file.name}.`, 'DEBUG');
              targetDb.insertStockInfo(stockEntries);
          }
          
          processedXmlSize += file.size;
          // After the file is fully processed, update progress to reflect its completion for this phase.
          setProgress(30 + (processedXmlSize / totalXmlSize) * 70);
      }
    } catch (e) {
      insertionError = e instanceof Error ? e : new Error(String(e));
    } finally {
        // SPEED OPTIMIZATION: Recreate indexes after bulk insert.
        // This runs even if there was an error, ensuring the DB is always in a queryable state.
        logToConsole('Recreating indexes...', 'INFO');
        setProgressPhase('indexing');
        setProgressMessage('Finalizing database...');
        targetDb.createIndexes();
        logToConsole('Indexes recreated successfully.', 'DEBUG');
    }

    if (insertionError) {
        throw insertionError; // Re-throw the error after indexes are restored
    }
  }, [logToConsole]);


  const handleCreateNewSessionFromFiles = React.useCallback(async (files: FileList) => {
    setIsLoading(true);
    setProgress(0);
    setError(null);
    setDetailedProgress({ currentFile: '', fileBytesRead: 0, fileTotalBytes: 0, fileLogCount: null });
    logToConsole(`Creating new session from ${files.length} file(s)...`, 'INFO');

    try {
        const newSessionDb = await Database.create();
        await processFilesToDb(files, newSessionDb);
        setDb(newSessionDb);
        await updateStateFromDb(newSessionDb, false);

        const now = new Date();
        const timestamp = now.toISOString().slice(0, 19).replace(/:/g, '-').replace('T', '_');
        const sessionName = `session_${timestamp}.sqlite`;
        
        logToConsole(`New session will be saved with generated name: ${sessionName}`, 'INFO');

        const success = await saveCurrentDbAsSession(newSessionDb, sessionName); 
        if (success) {
            logToConsole('New session created and saved successfully.', 'INFO');
            addToast({ type: 'success', title: 'Session Created', message: `New session '${sessionName}' created successfully.` });
        } else {
            throw new Error('Failed to save the new session after processing files.');
        }
    } catch(err) {
        const msg = err instanceof Error ? err.message : String(err);
        setError(msg);
        logToConsole(msg, 'ERROR');
        addToast({ type: 'error', title: 'Session Creation Failed', message: msg });
        await handleNewSession(false);
    } finally {
        setProgressMessage('Done!');
        setTimeout(() => setIsLoading(false), 500);
    }
  }, [processFilesToDb, updateStateFromDb, logToConsole, saveCurrentDbAsSession, addToast, handleNewSession]);

  const handleAddFilesToCurrentSession = React.useCallback(async (files: FileList) => {
    if (!db) {
        logToConsole('Database not ready, cannot add files.', 'ERROR');
        return;
    }
    if (!activeSessionName) {
        logToConsole('No active session. Create a new session first.', 'WARNING');
        addToast({ type: 'warning', title: 'No Active Session', message: 'Please create or load a session before adding more files.' });
        return;
    }

    logToConsole(`Adding ${files.length} file(s) to current session '${activeSessionName}'...`, 'INFO');

    try {
        await processFilesToDb(files, db);
        await updateStateFromDb(db, false);
        setIsDirty(true); // Mark as dirty
        const success = await handleSaveSession(); // Save the changes immediately
        if (success) {
            logToConsole(`Successfully added files and saved session.`, 'INFO');
            addToast({ type: 'success', title: 'Files Added', message: `Successfully added files to ${activeSessionName}.` });
        } else {
            throw new Error('Failed to save session after adding files.');
        }
    } catch(err) {
        const msg = err instanceof Error ? err.message : String(err);
        setError(msg);
        logToConsole(msg, 'ERROR');
        addToast({ type: 'error', title: 'Error Adding Files', message: msg });
    } finally {
        setProgressMessage('Done!');
        setTimeout(() => setIsLoading(false), 500);
    }
  }, [db, processFilesToDb, updateStateFromDb, logToConsole, activeSessionName, handleSaveSession, addToast]);
  
  const handleApplyFilters = React.useCallback(() => {
    logToConsole('Applying filters...', 'INFO');
    setAppliedFilters(formFilters);
    setCurrentPage(1);
    setEntriesOffset(0);
    setTimelineViewRange(null); // Reset zoom on filter change
    setIsInitialLoad(false);

    if (db) {
        try {
            // Persist the applied filters to the database itself.
            db.setMeta('appliedFilters', JSON.stringify(formFilters));
            logToConsole('Saved current filter configuration to the session database.', 'DEBUG');
            setIsDirty(true); // Saving to the DB is a modification.
        } catch(e) {
            const msg = e instanceof Error ? e.message : String(e);
            logToConsole(`Could not save filters to session: ${msg}`, 'WARNING');
        }
    }
  }, [db, formFilters, logToConsole, setIsDirty]);

  const handleResetFilters = React.useCallback(() => {
      logToConsole('Resetting filters...', 'INFO');
      setTimelineViewRange(null);
      const { minTime, maxTime } = db ? db.getMinMaxTime() : { minTime: null, maxTime: null };
      if (minTime && maxTime) {
        const [minDate, minTimeStr] = minTime.split(' ');
        const [maxDate, maxTimeStr] = maxTime.split(' ');
        const newFilters = {
          ...initialFilters,
          dateFrom: minDate,
          timeFrom: minTimeStr.substring(0, 8),
          dateTo: maxDate,
          timeTo: maxTimeStr.substring(0, 8),
        };
        setFormFilters(newFilters);
        setAppliedFilters(newFilters);
      } else {
        setFormFilters(initialFilters);
        setAppliedFilters(initialFilters);
      }
      setIsInitialLoad(false);
      setCurrentPage(1);
      setEntriesOffset(0);
  }, [logToConsole, db]);

  const handleClearTimeRange = React.useCallback(() => {
    logToConsole('Clearing time range filters...', 'INFO');
    const updater = (currentFilters: FilterState) => ({
      ...currentFilters,
      dateFrom: '',
      timeFrom: '',
      dateTo: '',
      timeTo: '',
    });
    setFormFilters(updater);
    setAppliedFilters(updater);
    setIsInitialLoad(false);
    setCurrentPage(1);
    setEntriesOffset(0);
  }, [logToConsole]);

  const goToPage = React.useCallback((pageNumber: number) => {
    setCurrentPage(pageNumber);
  }, []);

  const handlePageSizeChange = React.useCallback((newSize: number) => {
    setPageSize(newSize);
    setCurrentPage(1);
  }, []);

  const handleDownloadDb = React.useCallback(() => {
    if (!db) return;
    try {
      logToConsole('Exporting database...', 'INFO');
      const data: Uint8Array = db.export();
      const blob = new Blob([data], { type: 'application/x-sqlite3' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      const defaultName = activeSessionName || `log_database_${new Date().toISOString().replace(/[:.]/g, '-')}.sqlite`;
      a.download = defaultName;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
      logToConsole('Database exported successfully.', 'DEBUG');
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err);
      setError(msg);
      logToConsole(`Database export failed: ${msg}`, 'ERROR');
    }
  }, [db, logToConsole, activeSessionName]);
  
  const handleImportDb = React.useCallback(async (file: File) => {
    if (!file) return;

    setIsLoading(true);
    setProgressMessage(`Importing database: ${file.name}`);
    setProgressPhase('loading');
    setDetailedProgress({ currentFile: '', fileBytesRead: 0, fileTotalBytes: 0, fileLogCount: null });
    setError(null);
    logToConsole(`Importing database from ${file.name}...`, 'INFO');

    try {
        const buffer = await file.arrayBuffer();
        const newDb = await Database.createFromBuffer(new Uint8Array(buffer));
        setDb(newDb);
        await updateStateFromDb(newDb, true);
        setActiveSessionName(file.name.endsWith('.sqlite') ? file.name : `${file.name}.sqlite`);
        setIsDirty(true);
        if (isElectron) {
            // No auto-save. The user must save it explicitly.
            logToConsole('External DB imported. Session is marked as unsaved.', 'INFO');
        }
    } catch (err) {
        const msg = err instanceof Error ? err.message : `An unknown error occurred during import. Is this a valid database file?`;
        setError(msg);
        logToConsole(`Database import failed: ${msg}`, 'ERROR');
    } finally {
        setProgressMessage('Done!');
        setTimeout(() => setIsLoading(false), 500);
    }
}, [logToConsole, isElectron, updateStateFromDb]);

  const handleTimeRangeSelect = React.useCallback((startTime: number, endTime: number) => {
    const startDate = new Date(startTime);
    const endDate = new Date(endTime);

    // Use toISOString() to get a 'YYYY-MM-DDTHH:mm:ss.sssZ' string, which guarantees
    // the date/time parts are UTC and not affected by the user's local timezone.
    const dateToYYYYMMDD = (d: Date) => d.toISOString().split('T')[0];
    const dateToHHMMSS = (d: Date) => d.toISOString().split('T')[1].substring(0, 8);

    const updater = (currentFilters: FilterState) => ({
        ...currentFilters,
        dateFrom: dateToYYYYMMDD(startDate),
        timeFrom: dateToHHMMSS(startDate),
        dateTo: dateToYYYYMMDD(endDate),
        timeTo: dateToHHMMSS(endDate),
    });
    setFormFilters(updater);
    setAppliedFilters(updater); // Apply immediately for a seamless experience
    setIsInitialLoad(false);
    setCurrentPage(1);
    setEntriesOffset(0);
    setActiveView('viewer');
  }, []);

  const handleCategorySelect = React.useCallback((category: 'level' | 'sndrtype', value: string) => {
    setTimelineViewRange(null); // Reset zoom
    const modeKey = `${category}FilterMode` as const;

    const updater = (currentFilters: FilterState) => {
        const newFilters = { ...currentFilters };
        newFilters[modeKey] = 'include';
        newFilters[category] = [...(currentFilters[category] || []), value];
        return newFilters;
    };
    
    setFormFilters(updater);
    setAppliedFilters(updater);
    setIsInitialLoad(false);
    setCurrentPage(1);
    setEntriesOffset(0);
    setActiveView('viewer');
  }, []);

  const handleThemeChange = async (newTheme: Theme) => {
    setTheme(newTheme);
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, theme: newTheme });
            logToConsole(`Theme changed to '${newTheme}' and settings saved.`, 'INFO');
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save theme setting: ${msg}`, 'ERROR');
        }
    }
  };
  
  const handleViewModeChange = async (newMode: ViewMode) => {
    // CRITICAL FIX: By clearing the entries before changing the mode,
    // we prevent the next render cycle from attempting to render the
    // pagination view with the (potentially huge) list of entries
    // accumulated during scroll mode.
    setFilteredEntries([]);
    setEntriesOffset(0);
    
    setViewMode(newMode);
    setCurrentPage(1); // Reset page when changing mode
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, viewMode: newMode });
            logToConsole(`View mode changed to '${newMode}' and settings saved.`, 'INFO');
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save view mode setting: ${msg}`, 'ERROR');
        }
    }
  };
  
  const handleIconSetChange = async (newIconSet: IconSet) => {
    setIconSet(newIconSet);
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, iconSet: newIconSet });
            logToConsole(`Icon set changed to '${newIconSet}' and settings saved.`, 'INFO');
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save icon set setting: ${msg}`, 'ERROR');
        }
    }
  };
  
  const handleLogTableDensityChange = async (newDensity: LogTableDensity) => {
    setLogTableDensity(newDensity);
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, logTableDensity: newDensity });
            logToConsole(`Log table density changed to '${newDensity}' and settings saved.`, 'INFO');
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save log table density setting: ${msg}`, 'ERROR');
        }
    }
  };

  const handleColumnVisibilityChange = async (newVisibility: ColumnVisibilityState) => {
    setColumnVisibility(newVisibility);
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, columnVisibility: newVisibility });
            logToConsole('Column visibility settings saved.', 'DEBUG');
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save column visibility settings: ${msg}`, 'ERROR');
        }
    }
  };
  
  const handleColumnStylesChange = async (newStyles: ColumnStyles) => {
    setColumnStyles(newStyles);
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, columnStyles: newStyles });
            logToConsole('Column style settings saved.', 'DEBUG');
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save column style settings: ${msg}`, 'ERROR');
        }
    }
  };
  
  const handlePanelWidthsChange = (newWidths: PanelWidths) => {
    setPanelWidths(newWidths);
    if (window.electronAPI) {
        // Debounce saving to avoid excessive writes during drag
        clearTimeout((window as any).__panelWidthsSaveTimeout);
        (window as any).__panelWidthsSaveTimeout = setTimeout(async () => {
            try {
                const settings = await window.electronAPI.getSettings();
                await window.electronAPI.setSettings({ ...settings, panelWidths: newWidths });
                logToConsole('Panel widths saved.', 'DEBUG');
            } catch (err) {
                const msg = err instanceof Error ? err.message : String(err);
                logToConsole(`Failed to save panel widths: ${msg}`, 'ERROR');
            }
        }, 500);
    }
  };

  const handleTimeRangeSelectorVisibilityChange = async (isVisible: boolean) => {
    setIsTimeRangeSelectorVisible(isVisible);
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, isTimeRangeSelectorVisible: isVisible });
            logToConsole(`Timeline visibility setting saved: ${isVisible}.`, 'INFO');
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save timeline visibility setting: ${msg}`, 'ERROR');
        }
    }
};

const handleDetailPanelVisibilityChange = async (isVisible: boolean) => {
    setIsDetailPanelVisible(isVisible);
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, isDetailPanelVisible: isVisible });
            logToConsole(`Details panel visibility setting saved: ${isVisible}.`, 'INFO');
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save details panel visibility setting: ${msg}`, 'ERROR');
        }
    }
};

const handleFocusDebuggerVisibilityChange = async (isVisible: boolean) => {
    setIsFocusDebuggerVisible(isVisible);
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, isFocusDebuggerVisible: isVisible });
            logToConsole(`Focus Debugger visibility setting saved: ${isVisible}.`, 'INFO');
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save focus debugger visibility setting: ${msg}`, 'ERROR');
        }
    }
};

const handleTimelineBarVisibilityChange = async (newVisibility: TimelineBarVisibility) => {
    setTimelineBarVisibility(newVisibility);
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, timelineBarVisibility: newVisibility });
            logToConsole('Timeline bar visibility settings saved.', 'DEBUG');
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save timeline bar visibility settings: ${msg}`, 'ERROR');
        }
    }
};

const handleAllowPrereleaseChange = async (allow: boolean) => {
    setAllowPrerelease(allow);
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, allowPrerelease: allow });
            logToConsole(`Pre-release updates ${allow ? 'enabled' : 'disabled'}. Setting saved. Restart the app for this to take effect.`, 'INFO');
            addToast({
                type: 'info',
                title: 'Restart Required',
                message: `Pre-release updates have been ${allow ? 'enabled' : 'disabled'}. Please restart the application for the change to take effect.`,
                duration: 6000
            });
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save pre-release update setting: ${msg}`, 'ERROR');
        }
    }
};

const handleAutoUpdateEnabledChange = async (enabled: boolean) => {
    setIsAutoUpdateEnabled(enabled);
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, isAutoUpdateEnabled: enabled });
            logToConsole(`Automatic updates ${enabled ? 'enabled' : 'disabled'}. Setting saved. Restart the app for this to take effect.`, 'INFO');
            addToast({
                type: 'info',
                title: 'Restart Required',
                message: `Automatic updates have been ${enabled ? 'enabled' : 'disabled'}. Please restart the application for the change to take effect.`,
                duration: 6000
            });
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save automatic update setting: ${msg}`, 'ERROR');
        }
    }
};

const handleGithubTokenChange = async (token: string) => {
    setGithubToken(token);
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, githubToken: token });
            logToConsole(`GitHub token updated and saved.`, 'INFO');
            addToast({
                type: 'info',
                title: 'Restart Required',
                message: 'Please restart the application for the new GitHub token to take effect for private repository updates.',
                duration: 8000
            });
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save GitHub token: ${msg}`, 'ERROR');
        }
    }
};

const handleUiScaleChange = async (newScale: number) => {
    setUiScale(newScale);
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, uiScale: newScale });
            logToConsole(`UI Scale changed to '${(newScale * 100).toFixed(0)}%' and settings saved.`, 'INFO');
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save UI Scale setting: ${msg}`, 'ERROR');
        }
    }
  };

  const handleFullSettingsUpdate = async (newSettings: SettingsType) => {
    // Save to disk first
    if (window.electronAPI) {
        const { success, error: saveError } = await window.electronAPI.setSettings(newSettings);
        if (!success) {
            const msg = `Failed to save updated settings: ${saveError || 'Unknown error'}`;
            logToConsole(msg, 'ERROR');
            setError(msg); // Show error to user
            return; // Abort state update if save fails
        }
    }

    // Update all relevant state variables in the app
    if (newSettings.theme === 'light' || newSettings.theme === 'dark') setTheme(newSettings.theme);
    if (newSettings.viewMode === 'pagination' || newSettings.viewMode === 'scroll') setViewMode(newSettings.viewMode);
    if (newSettings.iconSet) setIconSet(newSettings.iconSet);
    if (newSettings.logTableDensity) setLogTableDensity(newSettings.logTableDensity);
    if (newSettings.columnVisibility) setColumnVisibility({ ...initialColumnVisibility, ...newSettings.columnVisibility });
    
    // Deep merge column styles
    const mergedStyles = { ...initialColumnStyles };
    for (const key in mergedStyles) {
        if (newSettings.columnStyles[key as keyof ColumnStyles]) {
            mergedStyles[key as keyof ColumnStyles] = { 
                ...mergedStyles[key as keyof ColumnStyles], 
                ...newSettings.columnStyles[key as keyof ColumnStyles] 
            };
        }
    }
    setColumnStyles(mergedStyles);

    if (newSettings.customFilterPresets) setCustomFilterPresets(newSettings.customFilterPresets);
    if (newSettings.panelWidths) setPanelWidths({ ...initialPanelWidths, ...newSettings.panelWidths });
    if (typeof newSettings.isTimeRangeSelectorVisible === 'boolean') setIsTimeRangeSelectorVisible(newSettings.isTimeRangeSelectorVisible);
    if (typeof newSettings.isDetailPanelVisible === 'boolean') setIsDetailPanelVisible(newSettings.isDetailPanelVisible);
    if (typeof newSettings.isFocusDebuggerVisible === 'boolean') setIsFocusDebuggerVisible(newSettings.isFocusDebuggerVisible);
    if (newSettings.timelineBarVisibility) setTimelineBarVisibility({ ...initialTimelineBarVisibility, ...newSettings.timelineBarVisibility });
    if (typeof newSettings.allowPrerelease === 'boolean') setAllowPrerelease(newSettings.allowPrerelease);
    if (typeof newSettings.isAutoUpdateEnabled === 'boolean') setIsAutoUpdateEnabled(newSettings.isAutoUpdateEnabled);
    if (typeof newSettings.githubToken === 'string') setGithubToken(newSettings.githubToken);
    if (typeof newSettings.uiScale === 'number') setUiScale(newSettings.uiScale);
    
    logToConsole('Settings updated and applied from JSON editor.', 'INFO');
};

  const handleLoadSession = React.useCallback(async (name: string) => {
    if (!window.electronAPI) return;
    setIsLoading(true);
    setProgressMessage(`Loading session: ${name}`);
    setProgressPhase('loading');
    setDetailedProgress({ currentFile: '', fileBytesRead: 0, fileTotalBytes: 0, fileLogCount: null });
    logToConsole(`Loading session: ${name}`, 'INFO');
    try {
        const {success, buffer, error} = await window.electronAPI.getSessionBuffer(name);
        if (!success || !buffer) throw new Error(error || 'Failed to get session buffer.');
        const newDb = await Database.createFromBuffer(new Uint8Array(buffer));
        setDb(newDb);
        setActiveSessionName(name);
        await updateStateFromDb(newDb, true);
        setIsDirty(false);
    } catch(e) {
        const msg = e instanceof Error ? e.message : String(e);
        setError(msg);
        logToConsole(`Failed to load session: ${msg}`, 'ERROR');
    } finally {
        setTimeout(() => setIsLoading(false), 500);
    }
  }, [updateStateFromDb, logToConsole]);

  const handleRenameSession = React.useCallback(async (oldName: string, newName: string) => {
    if (!window.electronAPI) return false;
    if (!newName.toLowerCase().endsWith('.sqlite')) {
        newName += '.sqlite';
    }
    logToConsole(`Renaming session '${oldName}' to '${newName}'`, 'INFO');
    const { success, error } = await window.electronAPI.renameSession(oldName, newName);
    if (success) {
        await fetchSessions();
        if (activeSessionName === oldName) {
            setActiveSessionName(newName);
        }
        logToConsole('Rename successful.', 'DEBUG');
        return true;
    }
    const msg = error || 'Failed to rename.';
    setError(msg);
    logToConsole(`Rename failed: ${msg}`, 'ERROR');
    return false;
  }, [fetchSessions, activeSessionName, logToConsole]);

  const handleDeleteSession = React.useCallback(async (name: string) => {
    if (!window.electronAPI) return;
    logToConsole(`Deleting session '${name}'`, 'INFO');
    const { success, error } = await window.electronAPI.deleteSession(name);
    if (success) {
        await fetchSessions();
        if (activeSessionName === name) {
            await handleNewSession(false);
            logToConsole(`Active session '${name}' was deleted. Resetting to a blank state.`, 'INFO');
        }
    } else {
        const msg = error || 'Failed to delete.';
        setError(msg);
        logToConsole(`Delete failed: ${msg}`, 'ERROR');
    }
  }, [fetchSessions, activeSessionName, handleNewSession, logToConsole]);
  
  // Effect for handling quit sequence from Electron main process
  React.useEffect(() => {
    if (!isElectron) return;
    
    // 1. Expose a function for main to check if we have unsaved changes
    window.isAppDirty = () => ({
        isDirty: isDirty,
        sessionName: activeSessionName
    });

    // 2. Listen for a "save before quitting" command from main
    const removeListener = window.electronAPI.onSaveBeforeQuit(async () => {
        logToConsole('Main process requested save before quitting...', 'DEBUG');
        const success = await handleSaveSession();
        if (success) {
            logToConsole('Save successful. Notifying main process to quit.', 'DEBUG');
            window.electronAPI.savedAndReadyToQuit();
        } else {
            logToConsole('Save was cancelled or failed. App will not quit.', 'WARNING');
            // If save fails or is cancelled, we do nothing, and the app remains open.
        }
    });

    return () => {
        delete window.isAppDirty;
        removeListener();
    };
  }, [isElectron, isDirty, activeSessionName, handleSaveSession, logToConsole]);

  // Effect for updating window title
  React.useEffect(() => {
      if (isElectron) {
          const baseTitle = 'Log Analyser';
          let title = baseTitle;
          if (activeSessionName) {
              title = `${activeSessionName}${isDirty ? '*' : ''} - ${baseTitle}`;
          }
          window.electronAPI.setTitle(title);
      }
  }, [isElectron, activeSessionName, isDirty]);

  const handleApplyDetailFilter = React.useCallback((key: 'level' | 'sndrtype' | 'sndrname' | 'fileName', value: string) => {
      logToConsole(`Applying quick filter: ${key} = ${value}`, 'DEBUG');
      const modeKey = `${key}FilterMode` as const;

      const updater = (currentFilters: FilterState) => {
          const newFilters = { ...currentFilters };
          const currentMode = newFilters[modeKey];
          const existingValues = newFilters[key] || [];

          if (currentMode === 'include') {
              if (!existingValues.includes(value)) {
                  newFilters[key] = [...existingValues, value];
              }
          } else {
              // If mode was 'exclude', switch to 'include' and set this as the only value.
              newFilters[modeKey] = 'include';
              newFilters[key] = [value];
          }
          return newFilters;
      };

      setFormFilters(updater);
      setAppliedFilters(updater);
      setIsInitialLoad(false);
      setCurrentPage(1);
      setEntriesOffset(0);
      setActiveView('viewer');
  }, [logToConsole]);

  const handleSaveFilterPreset = async (name: string) => {
      if (!name || !name.trim()) {
          logToConsole('Preset name cannot be empty.', 'WARNING');
          return;
      }
      const newPresets = { ...customFilterPresets, [name.trim()]: { ...formFilters } };
      setCustomFilterPresets(newPresets);
      if (isElectron) {
          try {
              const settings = await window.electronAPI.getSettings();
              await window.electronAPI.setSettings({ ...settings, customFilterPresets: newPresets });
              logToConsole(`Filter preset '${name.trim()}' saved.`, 'INFO');
          } catch (err) {
              const msg = err instanceof Error ? err.message : String(err);
              logToConsole(`Error saving filter preset: ${msg}`, 'ERROR');
          }
      }
  };

  const handleDeleteFilterPreset = async (name: string) => {
      if (!name || !customFilterPresets[name]) return;
      const newPresets = { ...customFilterPresets };
      delete newPresets[name];
      setCustomFilterPresets(newPresets);
      if (isElectron) {
          try {
              const settings = await window.electronAPI.getSettings();
              await window.electronAPI.setSettings({ ...settings, customFilterPresets: newPresets });
              logToConsole(`Filter preset '${name}' deleted.`, 'INFO');
          } catch (err) {
              const msg = err instanceof Error ? err.message : String(err);
              logToConsole(`Error deleting filter preset: ${msg}`, 'ERROR');
          }
      }
  };

  const handleLoadFilterPreset = (name: string) => {
      if (customFilterPresets[name]) {
          // Set a copy of the preset to ensure a state change is detected
          setFormFilters({ ...customFilterPresets[name] });
          logToConsole(`Loaded filter preset '${name}'. Click Apply to see results.`, 'INFO');
      }
  };
  
  const handleCursorChange = React.useCallback((time: number) => {
    if (!db || isBusy) return;
    const nearestEntry = db.getNearestLogEntry(time, appliedFilters);
    if (!nearestEntry) {
        logToConsole(`No log entry found near timestamp ${time}.`, 'DEBUG');
        return;
    }

    const index = db.getLogEntryIndex(nearestEntry.id, appliedFilters);
    if (index === -1) {
        logToConsole(`Could not find index for entry ID ${nearestEntry.id}`, 'WARNING');
        return;
    }
    
    // Set the target ID for the effect to handle the selection once data is ready.
    setJumpToEntryId(nearestEntry.id);
    setIsInitialLoad(false);
    
    if (viewMode === 'pagination') {
        const newPage = Math.floor(index / pageSize) + 1;
        if (newPage !== currentPage) {
            // This triggers a re-fetch, which will then trigger the selection effect.
            setCurrentPage(newPage);
        }
    } else { // Scroll mode (infinite scroll)
       const isEntryLoaded = filteredEntries.some(e => e.id === nearestEntry.id);
       if (isEntryLoaded) {
           logToConsole(`Entry ${nearestEntry.id} already loaded, selecting.`, 'DEBUG');
           return;
       }

       logToConsole(`Jumping to index ${index} in scroll mode. Refetching data window.`, 'DEBUG');
       setIsBusy(true);

       // Center the new view on the target entry
       const newOffset = Math.max(0, index - Math.floor(INFINITE_SCROLL_CHUNK_SIZE / 2));
       
       setTimeout(() => {
           const newEntries = db.queryLogEntries(appliedFilters, INFINITE_SCROLL_CHUNK_SIZE, newOffset);
           
           // Replace the current entries with the new window.
           setFilteredEntries(newEntries);
           setEntriesOffset(newOffset);
           setHasMoreLogs(newOffset + newEntries.length < totalFilteredCount);
           
           setIsBusy(false);
           logToConsole(`Loaded window of ${newEntries.length} logs for jump-to-time.`, 'DEBUG');
       }, 50);
    }
  }, [db, isBusy, appliedFilters, viewMode, pageSize, currentPage, logToConsole, filteredEntries, totalFilteredCount]);

  const handleFileSelect = React.useCallback((fileName: string) => {
      if (db) {
          // Find the time range for this specific file to zoom in
          const range = db.getTimeRangePerFile({ ...initialFilters, fileName: [fileName], fileNameFilterMode: 'include' });
          if (range.length > 0 && range[0].startTime && range[0].endTime) {
              const min = new Date(range[0].startTime + 'Z').getTime();
              const max = new Date(range[0].endTime + 'Z').getTime();
              if (min < max) {
                 setTimelineViewRange({ min, max });
              }
          }
      }

      const updater = (currentFilters: FilterState) => ({
          ...currentFilters,
          // Clear date filters when selecting a file to avoid conflicts
          dateFrom: '',
          timeFrom: '',
          dateTo: '',
          timeTo: '',
          fileName: [fileName],
          fileNameFilterMode: 'include' as 'include' | 'exclude',
      });
      setFormFilters(updater);
      setAppliedFilters(updater);
      setIsInitialLoad(false);
      setCurrentPage(1);
      setEntriesOffset(0);
      setActiveView('viewer');
  }, [db]);

  const handleDateSelect = React.useCallback((date: string) => {
      const startTime = new Date(`${date}T00:00:00.000Z`).getTime();
      const endTime = new Date(`${date}T23:59:59.999Z`).getTime();
      setTimelineViewRange({ min: startTime, max: endTime });
      
      const updater = (currentFilters: FilterState) => ({
          ...currentFilters,
          dateFrom: date,
          timeFrom: '00:00:00',
          dateTo: date,
          timeTo: '23:59:59',
          // Clear file filter when selecting a date to avoid conflicts
          fileName: [], 
      });
      setFormFilters(updater);
      setAppliedFilters(updater);
      setIsInitialLoad(false);
      setCurrentPage(1);
      setEntriesOffset(0);
      setActiveView('viewer');
  }, []);

  const handleTimelineZoomToSelection = React.useCallback(() => {
    const startTimeStr = getSqlDateTime(appliedFilters.dateFrom, appliedFilters.timeFrom);
    const endTimeStr = getSqlDateTime(appliedFilters.dateTo, appliedFilters.timeTo, true);

    if (startTimeStr && endTimeStr) {
        const min = new Date(startTimeStr.replace(' ', 'T') + 'Z').getTime();
        const max = new Date(endTimeStr.replace(' ', 'T') + 'Z').getTime();
        if (min < max) {
            setTimelineViewRange({ min, max });
            logToConsole('Timeline zoomed to selection.', 'DEBUG');
        }
    }
  }, [appliedFilters, logToConsole]);

  const handleTimelineZoomReset = React.useCallback(() => {
    setTimelineViewRange(null);
    logToConsole('Timeline zoom reset to full extent.', 'DEBUG');
  }, [logToConsole]);

  const handleRemoveAppliedFilter = React.useCallback((key: keyof FilterState, valueToRemove?: string) => {
      logToConsole(`Removing filter: ${key} = ${valueToRemove || '(cleared)'}`, 'DEBUG');
      const updater = (currentFilters: FilterState): FilterState => {
          const newFilters = { ...currentFilters };
          const currentValue = newFilters[key];

          if (Array.isArray(currentValue) && valueToRemove) {
              const newArray = currentValue.filter(v => v !== valueToRemove);
              (newFilters[key] as string[]) = newArray;
              // If the array is now empty, reset the mode for that attribute
              if (newArray.length === 0) {
                  const modeKey = `${key}FilterMode` as keyof FilterState;
                  if (modeKey in newFilters) {
                      (newFilters as any)[modeKey] = 'include';
                  }
              }
          } else {
              // For non-array filters (like includeMsg), just clear them.
              (newFilters as any)[key] = Array.isArray(initialFilters[key as keyof FilterState]) ? [] : '';
              const modeKey = `${key}FilterMode` as keyof FilterState;
              if (modeKey in newFilters) {
                  (newFilters as any)[modeKey] = 'include';
              }
          }
          return newFilters;
      };
      setFormFilters(updater);
      setAppliedFilters(updater);
      setIsInitialLoad(false);
      setCurrentPage(1);
      setEntriesOffset(0);
  }, [logToConsole]);
  
  const handleContextMenuFilter = React.useCallback((key: 'level' | 'sndrtype' | 'sndrname' | 'fileName', value: string, exclude: boolean) => {
    logToConsole(`Applying context filter: ${key} ${exclude ? '!=' : '='} ${value}`, 'DEBUG');
    
    const modeKey = `${key}FilterMode` as const;

    const updater = (currentFilters: FilterState) => {
        const newFilters = { ...currentFilters };
        const newMode = exclude ? 'exclude' : 'include';
        const currentMode = newFilters[modeKey];
        const existingValues = newFilters[key] || [];

        if (currentMode === newMode) {
            // Same mode, add value if it's not there.
            if (!existingValues.includes(value)) {
                newFilters[key] = [...existingValues, value];
            }
        } else {
            // Different mode, so switch mode and set this as the only value.
            newFilters[modeKey] = newMode;
            newFilters[key] = [value];
        }
        return newFilters;
    };

    setFormFilters(updater);
    setAppliedFilters(updater);
    setIsInitialLoad(false);
    setCurrentPage(1);
    setEntriesOffset(0);
    setActiveView('viewer');
  }, [logToConsole]);

    const handleSearchStock = React.useCallback((filters: StockInfoFilters) => {
        if (!db) {
            logToConsole('Database not available for stock search.', 'ERROR');
            return;
        }
        setIsStockBusy(true);
        logToConsole(`Searching stock for: ${filters.searchTerm}`, 'INFO');
        
        setTimeout(() => {
            try {
                const { entries: results, sql, params } = db.queryStockInfo(filters);
                
                logToConsole(`Executing stock search SQL: ${sql}`, 'DEBUG');
                logToConsole(`With parameters: ${JSON.stringify(params)}`, 'DEBUG');

                setStockHistory(results);
                logToConsole(`Found ${results.length} stock history records.`, 'DEBUG');

                // Fail-safe: If search returns results but the overall time range isn't set,
                // calculate it now. This ensures the timeline appears as expected.
                if (results.length > 0 && !overallStockTimeRange) {
                    logToConsole('Stock data found; re-calculating overall time range for timeline.', 'DEBUG');
                    const { minTime: minStockTime, maxTime: maxStockTime } = db.getMinMaxTime();
                    if (minStockTime && maxStockTime) {
                        setOverallStockTimeRange({ min: minStockTime, max: maxStockTime });
                        setOverallStockDensity(db.getStockDensity({} as StockInfoFilters, 300));
                    }
                }
            } catch (e) {
                const msg = e instanceof Error ? e.message : String(e);
                setError(msg);
                logToConsole(`Stock search failed: ${msg}`, 'ERROR');
            } finally {
                setIsStockBusy(false);
            }
        }, 50);
    }, [db, logToConsole, overallStockTimeRange]);
    
    const handleFetchStockSuggestions = React.useCallback(async (searchTerm: string, timeFilters: StockInfoFilters): Promise<StockArticleSuggestion[]> => {
        if (!db || searchTerm.length < 2) {
            return [];
        }
        try {
            return db.getUniqueArticles(searchTerm, timeFilters);
        } catch (e) {
            logToConsole(`Failed to fetch stock suggestions: ${e instanceof Error ? e.message : String(e)}`, 'ERROR');
            return [];
        }
    }, [db, logToConsole]);

    const handleRebuildStockData = React.useCallback(async () => {
        if (!db || !hasData) {
            logToConsole('Cannot rebuild stock data: No database or data loaded.', 'ERROR');
            addToast({ type: 'error', title: 'Rebuild Failed', message: 'No data is currently loaded.' });
            return;
        }
        
        logToConsole('Starting stock data rebuild from log entries...', 'INFO');
        addToast({ id: 'rebuild-stock', type: 'progress', title: 'Rebuilding Stock Data', message: 'Initializing...', progress: 0, duration: 0 });

        setIsStockBusy(true);

        // Use a timeout to allow the UI to update with the busy state and toast
        setTimeout(async () => {
            try {
                const onProgress = (processed: number, total: number, parsed: number) => {
                    if (total === 0) {
                        logToConsole('No potential stock log entries found to process during rebuild.', 'DEBUG');
                        setToasts(prev => prev.map(t =>
                            t.id === 'rebuild-stock'
                                ? { ...t, progress: 100, message: 'No relevant log entries found.' }
                                : t
                        ));
                        return;
                    }
                    const progressPercentage = total > 0 ? (processed / total) * 100 : 100;
                    logToConsole(`Rebuild Progress: Scanned ${processed.toLocaleString()} of ${total.toLocaleString()} potential logs. Parsed ${parsed.toLocaleString()} stock entries so far.`, 'DEBUG');
                    setToasts(prev => prev.map(t =>
                        t.id === 'rebuild-stock'
                            ? { ...t, progress: progressPercentage, message: `Scanning... ${processed.toLocaleString()}/${total.toLocaleString()}` }
                            : t
                    ));
                };
                
                logToConsole('Counting potential stock messages for rebuild...', 'DEBUG');
                const rebuiltCount = db.rebuildStockInfoFromLogs(onProgress);
                
                logToConsole(`Rebuild scan complete. A total of ${rebuiltCount.toLocaleString()} stock entries were parsed and inserted.`, 'INFO');
                
                logToConsole('Refreshing application state after rebuild...', 'DEBUG');
                await updateStateFromDb(db, false);
                
                // Clear current search results in the tracker view
                setStockHistory([]);

                setIsDirty(true);
                await handleSaveSession(); // Auto-save the change

                addToast({ id: 'rebuild-stock', type: 'success', title: 'Rebuild Complete', message: `Successfully rebuilt ${rebuiltCount.toLocaleString()} stock entries.` });
            } catch (e) {
                const msg = e instanceof Error ? e.message : String(e);
                logToConsole(`Stock data rebuild failed: ${msg}`, 'ERROR');
                addToast({ id: 'rebuild-stock', type: 'error', title: 'Rebuild Failed', message: msg });
            } finally {
                setIsStockBusy(false);
            }
        }, 50);
    }, [db, hasData, logToConsole, addToast, updateStateFromDb, handleSaveSession]);


  const totalPages = Math.ceil(totalFilteredCount / pageSize);
  
  // Find the selected entry's time for the timeline cursor
  const selectedEntryForCursor = React.useMemo(() => {
    if (keyboardSelectedId === null) return null;
    return filteredEntries.find(e => e.id === keyboardSelectedId);
  }, [keyboardSelectedId, filteredEntries]);

  const cursorTime = selectedEntryForCursor 
    ? new Date(selectedEntryForCursor.time.replace(/ (\d+)$/, '.$1') + 'Z').getTime() 
    : null;

  return (
    <div className="flex flex-col h-full bg-gray-50 dark:bg-gray-900 text-gray-900 dark:text-gray-100">
       <div className="fixed bottom-4 right-4 z-50 w-full max-w-sm space-y-3">
          {toasts.map(toast => (
              <Toast key={toast.id} toast={toast} onDismiss={() => removeToast(toast.id)} iconSet={iconSet} />
          ))}
      </div>
      {isAboutDialogOpen && (
          <AboutDialog
              isOpen={isAboutDialogOpen}
              onClose={() => setIsAboutDialogOpen(false)}
              iconSet={iconSet}
              version="0.19.0"
          />
      )}
      {isLoading && <ProgressIndicator progress={progress} message={progressMessage} phase={progressPhase} detailedProgress={detailedProgress} iconSet={iconSet} />}
      <Header activeView={activeView} onViewChange={setActiveView} isBusy={isBusy || isStockBusy} iconSet={iconSet} />
      <main className="flex-grow flex flex-col min-h-0">
        {activeView === 'data' && (
           <DataHub
            onCreateSessionFromFiles={handleCreateNewSessionFromFiles}
            onAddFilesToSession={handleAddFilesToCurrentSession}
            onImportDb={handleImportDb}
            onDownloadDb={handleDownloadDb}
            onNewSession={handleNewSession}
            error={error}
            hasData={hasData}
            totalEntryCount={totalEntryCount}
            overallTimeRange={overallTimeRange}
            loadedFileNames={loadedFileNames}
            isElectron={isElectron}
            sessions={sessions}
            activeSessionName={activeSessionName}
            onLoadSession={handleLoadSession}
            onRenameSession={handleRenameSession}
            onDeleteSession={handleDeleteSession}
            isDirty={isDirty}
            iconSet={iconSet}
          />
        )}
        {activeView === 'viewer' && (
           <>
            {hasData ? (
              <LogTable 
                entries={filteredEntries}
                totalFilteredCount={totalFilteredCount}
                loadedFileNames={loadedFileNames}
                pageTimestampRanges={pageTimestampRanges}
                onViewModeChange={handleViewModeChange}
                filters={formFilters}
                appliedFilters={appliedFilters}
                onFiltersChange={setFormFilters}
                onApplyFilters={handleApplyFilters}
                onResetFilters={handleResetFilters}
                onClearTimeRange={handleClearTimeRange}
                uniqueValues={uniqueValues}
                theme={theme}
                viewMode={viewMode}
                columnVisibility={columnVisibility}
                onColumnVisibilityChange={handleColumnVisibilityChange}
                columnStyles={columnStyles}
                panelWidths={panelWidths}
                onPanelWidthsChange={handlePanelWidthsChange}
                isDetailPanelVisible={isDetailPanelVisible}
                onDetailPanelVisibilityChange={handleDetailPanelVisibilityChange}
                onApplyFilter={handleApplyDetailFilter}
                onContextMenuFilter={handleContextMenuFilter}
                customFilterPresets={customFilterPresets}
                onSavePreset={handleSaveFilterPreset}
                onDeletePreset={handleDeleteFilterPreset}
                onLoadPreset={handleLoadFilterPreset}
                onLoadMore={handleLoadMore}
                hasMore={hasMoreLogs}
                isBusy={isBusy}
                logToConsole={logToConsole}
                overallTimeRange={overallTimeRange}
                onTimeRangeSelectorChange={handleTimeRangeSelect}
                isTimeRangeSelectorVisible={isTimeRangeSelectorVisible}
                onTimeRangeSelectorVisibilityChange={handleTimeRangeSelectorVisibilityChange}
                fileTimeRanges={fileTimeRanges}
                logDensity={logDensity}
                overallLogDensity={overallLogDensity}
                datesWithLogs={datesWithLogs}
                onCursorChange={handleCursorChange}
                onFileSelect={handleFileSelect}
                onDateSelect={handleDateSelect}
                keyboardSelectedId={keyboardSelectedId}
                setKeyboardSelectedId={setKeyboardSelectedId}
                jumpToEntryId={jumpToEntryId}
                timelineViewRange={timelineViewRange}
                onTimelineViewRangeChange={setTimelineViewRange}
                onTimelineZoomToSelection={handleTimelineZoomToSelection}
                onTimelineZoomReset={handleTimelineZoomReset}
                isInitialLoad={isInitialLoad}
                iconSet={iconSet}
                onRemoveAppliedFilter={handleRemoveAppliedFilter}
                logTableDensity={logTableDensity}
                onLogTableDensityChange={handleLogTableDensityChange}
                uiScale={uiScale}
                cursorTime={cursorTime}
                timelineBarVisibility={timelineBarVisibility}
                onTimelineBarVisibilityChange={handleTimelineBarVisibilityChange}
              />
            ) : (
                <div className="flex-grow flex items-center justify-center p-8 text-center bg-white dark:bg-gray-900">
                    <div>
                        <Icon name="Table" iconSet={iconSet} className="w-24 h-24 text-gray-300 dark:text-gray-700 mx-auto mb-6" />
                        <h2 className="text-2xl font-semibold text-gray-600 dark:text-gray-300">No Data Loaded</h2>
                        <p className="text-gray-500 dark:text-gray-500 mt-2 max-w-md mx-auto">
                            Switch to the <strong className="text-gray-700 dark:text-gray-400">Data Hub</strong> tab to {isElectron ? 'load a session or create a new one' : 'add log files'}.
                        </p>
                    </div>
              </div>
            )}
          </>
        )}
        {activeView === 'dashboard' && (
            <Dashboard 
                data={dashboardData}
                hasData={hasData}
                onTimeRangeSelect={handleTimeRangeSelect}
                onCategorySelect={handleCategorySelect}
                theme={theme}
                iconSet={iconSet}
            />
        )}
        {activeView === 'stock' && (
            <StockTracker
                onSearch={handleSearchStock}
                history={stockHistory}
                isBusy={isStockBusy}
                iconSet={iconSet}
                theme={theme}
                overallTimeRange={overallStockTimeRange}
                overallStockDensity={overallStockDensity}
                uiScale={uiScale}
                onRebuildStockData={handleRebuildStockData}
                onFetchSuggestions={handleFetchStockSuggestions}
                timelineBarVisibility={timelineBarVisibility}
                onTimelineBarVisibilityChange={handleTimelineBarVisibilityChange}
            />
        )}
        {activeView === 'console' && (
             <Console
                messages={consoleMessages}
                onClear={handleClearConsole}
                filters={consoleFilters}
                onFiltersChange={setConsoleFilters}
                iconSet={iconSet}
                searchTerm={consoleSearchTerm}
                onSearchTermChange={setConsoleSearchTerm}
                theme={theme}
             />
        )}
        {activeView === 'info' && (
            <Info 
              iconSet={iconSet} 
              onOpenAboutDialog={() => setIsAboutDialogOpen(true)}
            />
        )}
        {activeView === 'settings' && (
            <Settings 
              theme={theme}
              onThemeChange={handleThemeChange}
              viewMode={viewMode}
              onViewModeChange={handleViewModeChange}
              iconSet={iconSet}
              onIconSetChange={handleIconSetChange}
              columnStyles={columnStyles}
              onColumnStylesChange={handleColumnStylesChange}
              isTimeRangeSelectorVisible={isTimeRangeSelectorVisible}
              onTimeRangeSelectorVisibilityChange={handleTimeRangeSelectorVisibilityChange}
              isDetailPanelVisible={isDetailPanelVisible}
              onDetailPanelVisibilityChange={handleDetailPanelVisibilityChange}
              isFocusDebuggerVisible={isFocusDebuggerVisible}
              onFocusDebuggerVisibilityChange={handleFocusDebuggerVisibilityChange}
              logTableDensity={logTableDensity}
              onLogTableDensityChange={handleLogTableDensityChange}
              allowPrerelease={allowPrerelease}
              onAllowPrereleaseChange={handleAllowPrereleaseChange}
              isAutoUpdateEnabled={isAutoUpdateEnabled}
              onAutoUpdateEnabledChange={handleAutoUpdateEnabledChange}
              githubToken={githubToken}
              onGithubTokenChange={handleGithubTokenChange}
              uiScale={uiScale}
              onUiScaleChange={handleUiScaleChange}
              onFullSettingsUpdate={handleFullSettingsUpdate}
              columnVisibility={columnVisibility}
              customFilterPresets={customFilterPresets}
              panelWidths={panelWidths}
              timelineBarVisibility={timelineBarVisibility}
              onTimelineBarVisibilityChange={handleTimelineBarVisibilityChange}
            />
        )}
      </main>
      <StatusBar
        totalEntries={totalEntryCount}
        filteredCount={totalFilteredCount}
        activeSessionName={activeSessionName}
        isDirty={isDirty}
        viewMode={viewMode}
        currentPage={currentPage}
        totalPages={totalPages}
        visibleRowCount={filteredEntries.length}
        pageSize={pageSize}
        onPageSizeChange={handlePageSizeChange}
        onGoToPage={goToPage}
        isBusy={isBusy}
        lastConsoleMessage={lastConsoleMessage}
        theme={theme}
        onThemeChange={() => handleThemeChange(theme === 'light' ? 'dark' : 'light')}
        iconSet={iconSet}
        logTableDensity={logTableDensity}
        onLogTableDensityChange={handleLogTableDensityChange}
        isDetailPanelVisible={isDetailPanelVisible}
        onDetailPanelVisibilityChange={handleDetailPanelVisibilityChange}
      />
      <FocusDebugger isVisible={isFocusDebuggerVisible} iconSet={iconSet} />
    </div>
  );
};

export default App;