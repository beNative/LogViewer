

import React from 'react';
import { LogEntry, ConsoleMessage, FilterState, ConsoleMessageType, DashboardData, PageTimestampRange, SessionFile, ColumnVisibilityState, ColumnStyles, PanelWidths, ViewMode, OverallTimeRange, FileTimeRange, LogDensityPoint } from './types.ts';
import { LogTable } from './components/LogTable.tsx';
import { ProgressIndicator } from './components/ProgressIndicator.tsx';
import { Database, getSqlDateTime } from './db.ts';
import { Header } from './components/Header.tsx';
import { Console } from './components/Console.tsx';
import { DataHub } from './components/DataHub.tsx';
import { TableIcon } from './components/icons/TableIcon.tsx';
import { Dashboard } from './components/Dashboard.tsx';
import { Settings } from './components/Settings.tsx';
import { Info } from './components/Info.tsx';

// JSZip is loaded from script tag
declare const JSZip: any;
const INFINITE_SCROLL_CHUNK_SIZE = 200;

const initialFilters: FilterState = {
  dateFrom: '',
  timeFrom: '',
  dateTo: '',
  timeTo: '',
  level: [],
  sndrtype: [],
  sndrname: [],
  fileName: [],
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


type Theme = 'light' | 'dark';
type ProgressPhase = 'reading' | 'unzipping' | 'parsing' | 'inserting' | 'indexing' | 'loading';

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
  const [consoleFilters, setConsoleFilters] = React.useState(initialConsoleFilters);

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
  const [logDensity, setLogDensity] = React.useState<LogDensityPoint[]>([]);
  const [datesWithLogs, setDatesWithLogs] = React.useState<string[]>([]);

  const [activeView, setActiveView] = React.useState<'data' | 'viewer' | 'dashboard' | 'console' | 'settings' | 'info'>('data');
  const [theme, setTheme] = React.useState<Theme>('light');
  const [viewMode, setViewMode] = React.useState<ViewMode>('pagination');
  const [columnVisibility, setColumnVisibility] = React.useState<ColumnVisibilityState>(initialColumnVisibility);
  const [columnStyles, setColumnStyles] = React.useState<ColumnStyles>(initialColumnStyles);
  const [customFilterPresets, setCustomFilterPresets] = React.useState<Record<string, FilterState>>({});
  const [panelWidths, setPanelWidths] = React.useState<PanelWidths>(initialPanelWidths);
  const [isTimeRangeSelectorVisible, setIsTimeRangeSelectorVisible] = React.useState(true);
  
  const [hasMoreLogs, setHasMoreLogs] = React.useState(true);
  const [keyboardSelectedId, setKeyboardSelectedId] = React.useState<number | null>(null);
  const [jumpToEntryId, setJumpToEntryId] = React.useState<number | null>(null);
  const [isInitialLoad, setIsInitialLoad] = React.useState(true);


  // Electron-specific state
  const [isElectron, setIsElectron] = React.useState(false);
  const [sessions, setSessions] = React.useState<SessionFile[]>([]);
  const [activeSessionName, setActiveSessionName] = React.useState<string | null>(null);
  const [isDirty, setIsDirty] = React.useState<boolean>(false);
  const busyTaskRef = React.useRef(0);


  const logToConsole = React.useCallback((message: string, type: ConsoleMessage['type']) => {
    // Also send to electron main process for file logging if available
    if (window.electronAPI?.logMessage) {
        window.electronAPI.logMessage(type, message);
    }
    const timestamp = new Date().toLocaleTimeString('en-US', { hour12: false });
    setConsoleMessages(prev => [...prev, { message, type, timestamp }]);
  }, []);
  
  const handleClearConsole = React.useCallback(() => {
    setConsoleMessages([]);
    logToConsole('Console cleared.', 'INFO');
  }, [logToConsole]);

  const handleLoadMore = React.useCallback(() => {
    if (!db || !hasMoreLogs || isBusy) return;

    // Use a timeout to allow any UI state to update before blocking the main thread.
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
  
  const handleNewSession = React.useCallback((log = true) => {
    db?.createTable();
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
    setTimelineViewRange(null);
    setActiveSessionName(null);
    setUniqueValues({ level: [], sndrtype: [], sndrname: [], fileName: [] });
    setDashboardData(initialDashboardData);
    setIsInitialLoad(true);
    setIsDirty(false);
    if (log) logToConsole('Started new blank session.', 'INFO');
  }, [db, logToConsole]);

  const updateStateFromDb = React.useCallback((newDb: Database, fromSessionLoad: boolean) => {
    logToConsole('Reading metadata from database...', 'DEBUG');
    const totalEntries = newDb.getTotalEntryCount();
    setTotalEntryCount(totalEntries);

    if (totalEntries > 0) {
        let newFilters = { ...initialFilters };
        let filtersLoadedFromSession = false;

        const { minTime, maxTime } = newDb.getMinMaxTime();
        if (minTime && maxTime) {
            setOverallTimeRange({
                min: new Date(minTime + 'Z').getTime(), // Add Z to treat as UTC
                max: new Date(maxTime + 'Z').getTime()
            });
        }

        if (fromSessionLoad) {
            const savedFiltersJson = newDb.getMeta('appliedFilters');
            if (savedFiltersJson) {
                try {
                    const savedFilters = JSON.parse(savedFiltersJson);
                    // Deep merge to be safe with older session files
                    newFilters = { ...initialFilters, ...savedFilters };
                    filtersLoadedFromSession = true;
                    logToConsole('Loaded filter settings from session.', 'INFO');
                } catch(e) {
                    logToConsole('Could not parse filters from session, using defaults.', 'WARNING');
                }
            }
        }
        
        // If filters were not loaded from session, calculate from min/max time
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
        setCurrentPage(1);
        setEntriesOffset(0);
        setHasData(true);
        setIsInitialLoad(true);
        setUniqueValues({
            level: newDb.getUniqueColumnValues('level'),
            sndrtype: newDb.getUniqueColumnValues('sndrtype'),
            sndrname: newDb.getUniqueColumnValues('sndrname'),
            fileName: uniqueFileNames,
        });
        logToConsole(`Database loaded. Total entries: ${totalEntries}. Apply filters to view.`, 'INFO');
    } else {
        handleNewSession(false);
        logToConsole('Database is empty.', 'INFO');
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

  React.useEffect(() => {
    const root = window.document.documentElement;
    if (theme === 'light') {
        root.classList.remove('dark');
    } else {
        root.classList.add('dark');
    }
  }, [theme]);
  

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
          setLogDensity(db.getLogDensity(appliedFilters, 200));
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

  
  const saveCurrentDbAsSession = React.useCallback(async (name: string): Promise<boolean> => {
    if (!isElectron || !db) return false;

    // The 'appliedFilters' are now saved to the DB meta table whenever they are applied,
    // so we no longer need to explicitly save them here. This is more robust.
    
    const buffer = db.export();
    if (buffer.length <= 1024 && totalEntryCount === 0) {
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
  }, [db, isElectron, logToConsole, fetchSessions, totalEntryCount, setActiveSessionName, setIsDirty, setError]);

  const handleSaveSession = React.useCallback(async () => {
    if (activeSessionName) {
      return await saveCurrentDbAsSession(activeSessionName);
    } else {
      // If the session is new (no active name), generate one automatically.
      const now = new Date();
      const timestamp = now.toISOString().slice(0, 19).replace(/:/g, '-').replace('T', '_');
      const newSessionName = `session_${timestamp}.sqlite`;
      logToConsole(`New session detected. Saving with generated name: ${newSessionName}`, 'INFO');
      return await saveCurrentDbAsSession(newSessionName);
    }
  }, [activeSessionName, saveCurrentDbAsSession, logToConsole]);

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
          const entries: Omit<LogEntry, 'id' | 'fileName'>[] = Array.from(logNodes).map(node => ({
              level: node.getAttribute('level') || 'N/A',
              time: node.getAttribute('time') || 'N/A',
              sndrtype: node.getAttribute('sndrtype') || 'N/A',
              sndrname: node.getAttribute('sndrname') || 'N/A',
              msg: node.getAttribute('msg') || 'N/A',
          }));

          const onProgressCallback = (processedInFile: number) => {
              const progressWithinFile = totalEntriesInFile > 0 
                  ? (processedInFile / totalEntriesInFile) * file.size 
                  : file.size;
              const currentTotalProgress = processedXmlSize + progressWithinFile;
              // This phase contributes 70% of the total progress (from 30 to 100)
              setProgress(Math.min(100, 30 + (currentTotalProgress / totalXmlSize) * 70));
          };

          targetDb.insertLogs(entries, file.name, onProgressCallback);
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


  const handleFileDrop = React.useCallback(async (files: FileList) => {
    if (!db) {
        logToConsole('Database not ready, cannot process files.', 'ERROR');
        return;
    }
    try {
        await processFilesToDb(files, db);
        updateStateFromDb(db, false);
        setIsDirty(true);
    } catch(err) {
        const msg = err instanceof Error ? err.message : String(err);
        setError(msg);
        logToConsole(msg, 'ERROR');
    } finally {
        setProgressMessage('Done!');
        setTimeout(() => setIsLoading(false), 500);
    }
  }, [db, processFilesToDb, updateStateFromDb, logToConsole]);
  
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
        updateStateFromDb(newDb, true);
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
    const updater = (currentFilters: FilterState) => ({
        ...currentFilters,
        [category]: [...(currentFilters[category] || []), value]
    });
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
  
  const handlePanelWidthsChange = async (newWidths: PanelWidths) => {
    setPanelWidths(newWidths);
    if (window.electronAPI) {
        try {
            const settings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...settings, panelWidths: newWidths });
            logToConsole('Panel widths saved.', 'DEBUG');
        } catch (err) {
            const msg = err instanceof Error ? err.message : String(err);
            logToConsole(`Failed to save panel widths: ${msg}`, 'ERROR');
        }
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
        updateStateFromDb(newDb, true);
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
            handleNewSession(false);
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
    const updater = (currentFilters: FilterState) => {
      const existingValues = currentFilters[key] || [];
      if (existingValues.includes(value)) {
        return currentFilters; // Avoid adding duplicates
      }
      return {
        ...currentFilters,
        [key]: [...existingValues, value],
      };
    };

    setFormFilters(updater);
    setAppliedFilters(updater); // Apply immediately
    setIsInitialLoad(false);
    setCurrentPage(1);
    setEntriesOffset(0);
    setActiveView('viewer'); // Switch to viewer to see results
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
          const range = db.getTimeRangePerFile({ ...initialFilters, fileName: [fileName] });
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


  const totalPages = Math.ceil(totalFilteredCount / pageSize);

  return (
    <div className="flex flex-col h-screen bg-gray-50 dark:bg-gray-900 text-gray-900 dark:text-gray-100">
      {isLoading && <ProgressIndicator progress={progress} message={progressMessage} phase={progressPhase} detailedProgress={detailedProgress} />}
      <Header activeView={activeView} onViewChange={setActiveView} isBusy={isBusy} />
      <main className="flex-grow flex flex-col min-h-0">
        {activeView === 'data' && (
           <DataHub
            onFileDrop={handleFileDrop}
            onImportDb={handleImportDb}
            onDownloadDb={handleDownloadDb}
            onNewSession={handleNewSession}
            error={error}
            hasData={hasData}
            isElectron={isElectron}
            sessions={sessions}
            activeSessionName={activeSessionName}
            onLoadSession={handleLoadSession}
            onRenameSession={handleRenameSession}
            onDeleteSession={handleDeleteSession}
            isDirty={isDirty}
            onSaveSession={handleSaveSession}
          />
        )}
        {activeView === 'viewer' && (
           <>
            {hasData ? (
              <LogTable 
                entries={filteredEntries}
                loadedFileNames={loadedFileNames}
                currentPage={currentPage}
                totalPages={totalPages}
                filteredEntriesCount={totalFilteredCount}
                totalEntries={totalEntryCount}
                pageSize={pageSize}
                pageTimestampRanges={pageTimestampRanges}
                onGoToPage={goToPage}
                onPageSizeChange={handlePageSizeChange}
                filters={formFilters}
                appliedFilters={appliedFilters}
                onFiltersChange={setFormFilters}
                onApplyFilters={handleApplyFilters}
                onResetFilters={handleResetFilters}
                onClearTimeRange={handleClearTimeRange}
                uniqueValues={uniqueValues}
                theme={theme}
                viewMode={viewMode}
                onViewModeChange={handleViewModeChange}
                columnVisibility={columnVisibility}
                onColumnVisibilityChange={handleColumnVisibilityChange}
                columnStyles={columnStyles}
                panelWidths={panelWidths}
                onPanelWidthsChange={handlePanelWidthsChange}
                onApplyFilter={handleApplyDetailFilter}
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
                datesWithLogs={datesWithLogs}
                onCursorChange={handleCursorChange}
                onFileSelect={handleFileSelect}
                onDateSelect={handleDateSelect}
                keyboardSelectedId={keyboardSelectedId}
                setKeyboardSelectedId={setKeyboardSelectedId}
                jumpToEntryId={jumpToEntryId}
                timelineViewRange={timelineViewRange}
                onTimelineZoomToSelection={handleTimelineZoomToSelection}
                onTimelineZoomReset={handleTimelineZoomReset}
                isInitialLoad={isInitialLoad}
              />
            ) : (
                <div className="flex-grow flex items-center justify-center p-8 text-center bg-white dark:bg-gray-900">
                    <div>
                        <TableIcon className="w-24 h-24 text-gray-300 dark:text-gray-700 mx-auto mb-6" />
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
            />
        )}
        {activeView === 'console' && (
          <div className="flex-grow min-h-0 p-4 bg-gray-100 dark:bg-gray-900">
             <Console
                messages={consoleMessages}
                onClear={handleClearConsole}
                filters={consoleFilters}
                onFiltersChange={setConsoleFilters}
             />
          </div>
        )}
        {activeView === 'info' && (
            <Info />
        )}
        {activeView === 'settings' && (
            <Settings 
              theme={theme}
              onThemeChange={handleThemeChange}
              viewMode={viewMode}
              onViewModeChange={handleViewModeChange}
              columnStyles={columnStyles}
              onColumnStylesChange={handleColumnStylesChange}
              isTimeRangeSelectorVisible={isTimeRangeSelectorVisible}
              onTimeRangeSelectorVisibilityChange={handleTimeRangeSelectorVisibilityChange}
            />
        )}
      </main>
    </div>
  );
};

export default App;