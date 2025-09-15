import React from 'react';
import { produce } from 'immer';
import { Database } from './db.ts';
import {
    LogEntry, FilterState, Settings, SessionFile, OverallTimeRange, DashboardData,
    ToastMessage, ConsoleMessage, ColumnVisibilityState, ColumnStyles, PanelWidths,
    ViewMode, Theme, IconSet, LogTableDensity, StockInfoFilters, StockInfoEntry,
    LogDensityPoint, StockArticleSuggestion, TimelineBarVisibility
} from './types.ts';
import { Header } from './components/Header.tsx';
import { DataHub } from './components/DataHub.tsx';
import { LogTable } from './components/LogTable.tsx';
import { FilterBar } from './components/FilterBar.tsx';
import { ProgressIndicator } from './components/ProgressIndicator.tsx';
import { parseLogFiles } from './parser.ts';
import { LogDetailPanel } from './components/LogDetailPanel.tsx';
import { ActiveFilters } from './components/ActiveFilters.tsx';
import { StatusBar } from './components/StatusBar.tsx';
import { areFiltersEqual } from './utils.ts';
import { Dashboard } from './components/Dashboard.tsx';
import { Console } from './components/Console.tsx';
import { Settings as SettingsComponent } from './components/Settings.tsx';
import { Info } from './components/Info.tsx';
import { Toast } from './components/Toast.tsx';
import { AboutDialog } from './components/AboutDialog.tsx';
import { StockTracker } from './components/StockTracker.tsx';

const MONO_FONT_STACK = 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace';

const initialFilters: FilterState = {
    dateFrom: '', timeFrom: '', dateTo: '', timeTo: '',
    level: [], levelFilterMode: 'include',
    sndrtype: [], sndrtypeFilterMode: 'include',
    sndrname: [], sndrnameFilterMode: 'include',
    fileName: [], fileNameFilterMode: 'include',
    includeMsg: '', excludeMsg: '',
    includeMsgMode: 'OR', excludeMsgMode: 'AND',
    sqlQuery: '', sqlQueryEnabled: false,
};

const App: React.FC = () => {
    const [db, setDb] = React.useState<Database | null>(null);
    const [logEntries, setLogEntries] = React.useState<LogEntry[]>([]);
    const [filteredCount, setFilteredCount] = React.useState(0);
    const [totalCount, setTotalCount] = React.useState(0);
    const [isLoading, setIsLoading] = React.useState(false);
    const [progress, setProgress] = React.useState({ percent: 0, message: '', phase: 'reading' as const, detailedProgress: { currentFile: '', fileBytesRead: 0, fileTotalBytes: 0, fileLogCount: 0 } });
    const [error, setError] = React.useState<string | null>(null);
    const [activeView, setActiveView] = React.useState<'data' | 'viewer' | 'dashboard' | 'console' | 'settings' | 'info' | 'stock'>('data');
    const [settings, setSettings] = React.useState<Settings | null>(null);
    const [sessions, setSessions] = React.useState<SessionFile[]>([]);
    const [activeSessionName, setActiveSessionName] = React.useState<string | null>(null);
    const [isDirty, setIsDirty] = React.useState(false);
    
    // Viewer-specific state
    const [filters, setFilters] = React.useState<FilterState>(initialFilters);
    const [appliedFilters, setAppliedFilters] = React.useState<FilterState>(initialFilters);
    const [uniqueValues, setUniqueValues] = React.useState<{ level: string[], sndrtype: string[], sndrname: string[], fileName: string[] }>({ level: [], sndrtype: [], sndrname: [], fileName: [] });
    const [selectedEntry, setSelectedEntry] = React.useState<LogEntry | null>(null);
    const [overallTimeRange, setOverallTimeRange] = React.useState<OverallTimeRange | null>(null);
    const [currentPage, setCurrentPage] = React.useState(1);
    const [pageSize, setPageSize] = React.useState(1000);
    const [isInitialLoad, setIsInitialLoad] = React.useState(true);
    const [totalPages, setTotalPages] = React.useState(0);

    // Dashboard data
    const [dashboardData, setDashboardData] = React.useState<DashboardData | null>(null);

    // Console messages
    const [consoleMessages, setConsoleMessages] = React.useState<ConsoleMessage[]>([]);
    const [consoleFilters, setConsoleFilters] = React.useState<Record<string, boolean>>({ DEBUG: true, INFO: true, WARNING: true, ERROR: true });
    const [consoleSearchTerm, setConsoleSearchTerm] = React.useState('');

    // Toasts
    const [toasts, setToasts] = React.useState<ToastMessage[]>([]);

    // About Dialog
    const [isAboutDialogOpen, setIsAboutDialogOpen] = React.useState(false);
    const [appVersion, setAppVersion] = React.useState('0.16.0');

    // Stock Tracker
    const [stockHistory, setStockHistory] = React.useState<StockInfoEntry[]>([]);
    const [overallStockTimeRange, setOverallStockTimeRange] = React.useState<{ min: string, max: string } | null>(null);
    const [overallStockDensity, setOverallStockDensity] = React.useState<LogDensityPoint[]>([]);

    const logToConsole = React.useCallback((message: string, type: 'DEBUG' | 'INFO' | 'WARNING' | 'ERROR' = 'INFO') => {
        const newMessage: ConsoleMessage = { message, type, timestamp: new Date().toLocaleTimeString() };
        setConsoleMessages(prev => [...prev, newMessage]);
        if (window.electronAPI) {
            window.electronAPI.logMessage(type, message);
        }
    }, []);

    const addToast = React.useCallback((toast: Omit<ToastMessage, 'id'>) => {
        const id = new Date().getTime().toString() + Math.random();
        setToasts(prev => [...prev, { ...toast, id }]);
    }, []);

    // Load settings on startup
    React.useEffect(() => {
        const loadSettings = async () => {
            logToConsole("Initializing settings...");
            if (window.electronAPI) {
                try {
                    const fetchedSettings = await window.electronAPI.getSettings();
                    setSettings(fetchedSettings);
                    setPageSize(fetchedSettings.viewMode === 'pagination' ? 1000 : 500);
                    logToConsole("Settings loaded successfully from electron main process.");
                } catch (error) {
                    const message = error instanceof Error ? error.message : String(error);
                    logToConsole(`Failed to load settings: ${message}`, 'ERROR');
                    setError(`Could not load settings: ${message}`);
                }
            } else {
                 // Fallback for web environment
                 const defaultSettings: Settings = {
                    theme: "light", viewMode: "pagination", allowPrerelease: false, isAutoUpdateEnabled: true,
                    githubToken: "", iconSet: "sharp", logTableDensity: "normal",
                    columnVisibility: { time: true, level: true, sndrtype: true, sndrname: true, fileName: true, msg: true },
                    customFilterPresets: {},
                    columnStyles: {
                      time: { font: MONO_FONT_STACK, isBold: false, isItalic: false, fontSize: 13, color: '#6B7280', darkColor: '#9CA3AF' },
                      level: { font: 'sans-serif', isBold: true, isItalic: false, fontSize: 12, color: '', darkColor: '' },
                      sndrtype: { font: 'sans-serif', isBold: false, isItalic: false, fontSize: 14, color: '#374151', darkColor: '#D1D5DB' },
                      sndrname: { font: 'sans-serif', isBold: false, isItalic: false, fontSize: 14, color: '#374151', darkColor: '#D1D5DB' },
                      fileName: { font: 'sans-serif', isBold: false, isItalic: false, fontSize: 13, color: '#6B7280', darkColor: '#9CA3AF' },
                      msg: { font: MONO_FONT_STACK, isBold: false, isItalic: false, fontSize: 13, color: '#1F2937', darkColor: '#F3F4F6' },
                    },
                    panelWidths: { filters: 320, details: 500 },
                    isTimeRangeSelectorVisible: true, isDetailPanelVisible: false, isFocusDebuggerVisible: false,
                    timelineBarVisibility: { pages: true, files: true, dates: true, density: true, overview: true },
                    uiScale: 1,
                };
                setSettings(defaultSettings);
                logToConsole("Running in web mode, using default settings.");
            }
        };
        loadSettings();
    }, [logToConsole]);

    if (!settings) {
        return <div className="p-4 text-center">Loading Settings...</div>;
    }

    const currentView = () => {
      switch(activeView) {
        case 'data': return <DataHub // ... full props
            onCreateSessionFromFiles={() => {}}
            onAddFilesToSession={() => {}}
            onImportDb={() => {}}
            onDownloadDb={() => {}}
            onNewSession={() => {}}
            error={error}
            hasData={totalCount > 0}
            totalEntryCount={totalCount}
            overallTimeRange={overallTimeRange}
            loadedFileNames={uniqueValues.fileName}
            isElectron={!!window.electronAPI}
            sessions={sessions}
            activeSessionName={activeSessionName}
            onLoadSession={() => {}}
            onRenameSession={async () => false}
            onDeleteSession={() => {}}
            isDirty={isDirty}
            iconSet={settings.iconSet}
            />;
        // ... Other views would be here
        default: return <div className="p-4 text-center">View not implemented yet.</div>
      }
    }

    return (
        <div className="h-screen w-screen flex flex-col bg-gray-100 dark:bg-gray-900 overflow-hidden">
            <Header activeView={activeView} onViewChange={setActiveView} isBusy={isLoading} iconSet={settings.iconSet} />
            <main className="flex-grow flex flex-col min-h-0">
               {currentView()}
            </main>
            {isLoading && <ProgressIndicator progress={progress.percent} message={progress.message} phase={progress.phase} detailedProgress={progress.detailedProgress} iconSet={settings.iconSet} />}
            <AboutDialog isOpen={isAboutDialogOpen} onClose={() => setIsAboutDialogOpen(false)} iconSet={settings.iconSet} version={appVersion} />
            <div className="fixed top-4 right-4 z-[100] w-full max-w-sm space-y-2">
                {toasts.map(toast => (
                    <Toast key={toast.id} toast={toast} onDismiss={() => setToasts(t => t.filter(item => item.id !== toast.id))} iconSet={settings.iconSet} />
                ))}
            </div>
        </div>
    );
};

export default App;
