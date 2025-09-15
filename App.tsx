import React from 'react';
import { Header } from './components/Header.tsx';
import { DataHub } from './components/DataHub.tsx';
import { LogTable } from './components/LogTable.tsx';
import { FilterBar } from './components/FilterBar.tsx';
import { Database } from './db.ts';
import { LogEntry, FilterState, Settings, SessionFile, OverallTimeRange, DashboardData, ToastMessage, ConsoleMessage } from './types.ts';
import { ProgressIndicator } from './components/ProgressIndicator.tsx';
import { parseLogFiles } from './parser.ts';
import { Splitter } from './components/Splitter.tsx';
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
import { produce } from 'immer';

const App: React.FC = () => {
    const [db, setDb] = React.useState<Database | null>(null);
    const [logEntries, setLogEntries] = React.useState<LogEntry[]>([]);
    const [filteredCount, setFilteredCount] = React.useState(0);
    const [totalCount, setTotalCount] = React.useState(0);
    const [isLoading, setIsLoading] = React.useState(false);
    const [progress, setProgress] = React.useState({ percent: 0, message: '', phase: 'reading' as const });
    const [error, setError] = React.useState<string | null>(null);
    const [activeView, setActiveView] = React.useState<'data' | 'viewer' | 'dashboard' | 'console' | 'settings' | 'info' | 'stock'>('data');
    const [settings, setSettings] = React.useState<Settings | null>(null);
    const [sessions, setSessions] = React.useState<SessionFile[]>([]);
    const [activeSessionName, setActiveSessionName] = React.useState<string | null>(null);
    const [isDirty, setIsDirty] = React.useState(false);
    
    // Viewer-specific state
    const [filters, setFilters] = React.useState<FilterState | null>(null);
    const [appliedFilters, setAppliedFilters] = React.useState<FilterState | null>(null);
    const [uniqueValues, setUniqueValues] = React.useState({ level: [], sndrtype: [], sndrname: [], fileName: [] });
    const [selectedEntry, setSelectedEntry] = React.useState<LogEntry | null>(null);
    const [overallTimeRange, setOverallTimeRange] = React.useState<OverallTimeRange | null>(null);
    const [currentPage, setCurrentPage] = React.useState(1);
    const [pageSize, setPageSize] = React.useState(1000);

    // Dashboard data
    const [dashboardData, setDashboardData] = React.useState<DashboardData | null>(null);

    // Console messages
    const [consoleMessages, setConsoleMessages] = React.useState<ConsoleMessage[]>([]);

    // Toasts
    const [toasts, setToasts] = React.useState<ToastMessage[]>([]);

    // About Dialog
    const [isAboutDialogOpen, setIsAboutDialogOpen] = React.useState(false);
    const [appVersion, setAppVersion] = React.useState('1.0.0'); // Default, can be updated from electron

    React.useEffect(() => {
      // Mock version for web, Electron would provide this
      if (!window.electronAPI) {
        setAppVersion('2.0.0 (Web)');
      } else {
        // In a real electron app, you might get this via an IPC call
        // window.electronAPI.getAppVersion().then(setAppVersion);
      }
    }, []);

    // ... The rest of the App component logic will go here
    
    // Dummy implementation for now
    const onFileDrop = (files: FileList) => {
        console.log('Files dropped:', files);
    };

    if (!settings) {
        // Can show a loading screen here
        return <div>Loading Settings...</div>;
    }

    const currentView = () => {
        switch(activeView) {
            case 'data':
                return <DataHub 
                          onCreateSessionFromFiles={onFileDrop}
                          onAddFilesToSession={onFileDrop}
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
                        />
            default:
                return (
                    <div className="flex-grow flex items-center justify-center p-8 text-center bg-gray-100 dark:bg-transparent">
                        <p>View not implemented yet.</p>
                    </div>
                );
        }
    }

    return (
        <div className="h-screen w-screen flex flex-col bg-gray-100 dark:bg-gray-900 overflow-hidden">
            <Header activeView={activeView} onViewChange={setActiveView} isBusy={isLoading} iconSet={settings.iconSet} />
            <main className="flex-grow flex flex-col min-h-0">
               {currentView()}
            </main>
            {isLoading && <ProgressIndicator progress={progress.percent} message={progress.message} phase={progress.phase} detailedProgress={{ currentFile: '', fileBytesRead: 0, fileTotalBytes: 0, fileLogCount: 0 }} iconSet={settings.iconSet} />}
            <AboutDialog isOpen={isAboutDialogOpen} onClose={() => setIsAboutDialogOpen(false)} iconSet={settings.iconSet} version={appVersion} />
        </div>
    );
};

export default App;
