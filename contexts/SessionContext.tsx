import React, { createContext, useState, useCallback, useContext, useEffect, useRef } from 'react';
import { SessionFile, ProgressPhase, FilterState } from '../types';
import { Database } from '../db';
import { useUI } from './UIContext';
import { useConsole } from './ConsoleContext';
import { useToast } from './ToastContext';

declare const JSZip: any;

type SessionContextType = {
    db: Database | null;
    isElectron: boolean;
    sessions: SessionFile[];
    activeSessionName: string | null;
    isDirty: boolean;
    setIsDirty: React.Dispatch<React.SetStateAction<boolean>>;
    hasData: boolean;
    totalEntryCount: number;
    overallTimeRange: { min: number, max: number } | null;
    loadedFileNames: string[];
    error: string | null;
    handleNewSession: (log?: boolean) => Promise<void>;
    handleSaveSession: () => Promise<boolean>;
    handleLoadSession: (name: string) => Promise<void>;
    onRenameSession: (oldName: string, newName: string) => Promise<boolean>;
    onDeleteSession: (name: string) => Promise<void>;
    handleCreateNewSessionFromFiles: (files: FileList) => Promise<void>;
    handleAddFilesToCurrentSession: (files: FileList) => Promise<void>;
    handleImportDb: (file: File) => Promise<void>;
    handleDownloadDb: () => void;
    updateStateFromDb: (newDb: Database, fromSessionLoad: boolean) => Promise<void>;
    overallStockTimeRange: { min: string, max: string } | null;
    setOverallStockTimeRange: React.Dispatch<React.SetStateAction<{ min: string, max: string } | null>>;
    overallStockDensity: any[]; // Use any to avoid circular dependency with DataContext
    setOverallStockDensity: React.Dispatch<React.SetStateAction<any[]>>;
};

const SessionContext = createContext<SessionContextType | undefined>(undefined);

export const SessionProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const { setIsLoading, setProgress, setProgressMessage, setProgressPhase, setDetailedProgress, setIsInitialLoad, addToast, error, setError, setActiveView } = useUI();
    const { logToConsole } = useConsole();
    
    const [db, setDb] = useState<Database | null>(null);
    const [isElectron, setIsElectron] = useState(false);
    const [sessions, setSessions] = useState<SessionFile[]>([]);
    const [activeSessionName, setActiveSessionName] = useState<string | null>(null);
    const [isDirty, setIsDirty] = useState<boolean>(false);
    const [hasData, setHasData] = useState<boolean>(false);
    const [totalEntryCount, setTotalEntryCount] = useState(0);
    const [overallTimeRange, setOverallTimeRange] = useState<{ min: number; max: number } | null>(null);
    const [loadedFileNames, setLoadedFileNames] = useState<string[]>([]);
    const sessionNameHint = useRef<string | null>(null);

    // State that is also part of DataContext but needed here for updates
    const [overallStockTimeRange, setOverallStockTimeRange] = useState<{ min: string, max: string } | null>(null);
    const [overallStockDensity, setOverallStockDensity] = useState<any[]>([]);

    const fetchSessions = useCallback(async () => {
        if (!window.electronAPI) return;
        try {
            const fetchedSessions = await window.electronAPI.listSessions();
            setSessions(fetchedSessions);
        } catch (e) {
            const msg = e instanceof Error ? e.message : String(e);
            logToConsole(`Failed to fetch sessions: ${msg}`, 'ERROR');
        }
    }, [logToConsole]);
    
    const handleNewSession = useCallback(async (log = true) => {
        const newDb = await Database.create();
        setDb(newDb);
        setHasData(false);
        setTotalEntryCount(0);
        setError(null);
        setLoadedFileNames([]);
        setOverallTimeRange(null);
        setOverallStockTimeRange(null);
        setOverallStockDensity([]);
        setActiveSessionName(null);
        setIsInitialLoad(true);
        setIsDirty(false);
        if (log) logToConsole('Started new blank session.', 'INFO');
    }, [logToConsole, setError, setIsInitialLoad]);

    useEffect(() => {
        const isElectronEnv = !!window.electronAPI;
        setIsElectron(isElectronEnv);
        logToConsole('Initializing database...', 'INFO');
        if (isElectronEnv) fetchSessions();
        Database.create().then(database => {
            setDb(database);
            logToConsole('Database ready.', 'DEBUG');
        }).catch(e => {
            const msg = e instanceof Error ? e.message : 'Unknown error';
            logToConsole(`Database initialization failed: ${msg}`, 'ERROR');
            setError(`Database initialization failed: ${msg}`);
        });
    }, [logToConsole, fetchSessions, setError]);

    const updateStateFromDb = useCallback(async (newDb: Database) => {
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

        setHasData(true);

        if (hasStockData) {
            setOverallStockTimeRange({ min: minStockTime!, max: maxStockTime! });
            setOverallStockDensity(newDb.getStockDensity({} as any, 300));
        } else {
            setOverallStockTimeRange(null);
            setOverallStockDensity([]);
        }

        if (hasLogData) {
            const { minTime, maxTime } = newDb.getMinMaxTime();
            if (minTime && maxTime) {
                setOverallTimeRange({
                    min: new Date(minTime.replace(' ', 'T') + 'Z').getTime(),
                    max: new Date(maxTime.replace(' ', 'T') + 'Z').getTime()
                });
            }
            const uniqueFileNames = newDb.getUniqueColumnValues('fileName');
            setLoadedFileNames(uniqueFileNames);
        } else {
             setOverallTimeRange(null);
             setLoadedFileNames([]);
        }
    }, [logToConsole, handleNewSession]);

    const processFilesToDb = useCallback(async (files: FileList, targetDb: Database) => {
        const xmlFiles: { name: string; content: string }[] = [];
        const totalSize = Array.from(files).reduce((acc, file) => acc + file.size, 0);
        let bytesRead = 0;
        
        setProgressPhase('reading');
        for (const file of files) {
            setDetailedProgress({ currentFile: file.name, fileBytesRead: 0, fileTotalBytes: file.size, fileLogCount: null });
            if (file.name.endsWith('.zip')) {
                setProgressPhase('unzipping');
                try {
                    const zip = await JSZip.loadAsync(file);
                    for (const filename in zip.files) {
                        if (filename.endsWith('.xml')) {
                            const content = await zip.files[filename].async('string');
                            xmlFiles.push({ name: filename, content });
                        }
                    }
                } catch (e) {
                    throw new Error(`Failed to read zip file ${file.name}: ${(e as Error).message}`);
                }
            } else if (file.name.endsWith('.xml')) {
                const content = await file.text();
                xmlFiles.push({ name: file.name, content });
            }
            bytesRead += file.size;
            setProgress(30 * (bytesRead / totalSize));
        }

        setProgressPhase('parsing');
        targetDb.dropIndexes(); // Drop indexes for faster insertion
        const parser = new DOMParser();
        let totalLogsToInsert = 0;
        for (const [i, xmlFile] of xmlFiles.entries()) {
            setDetailedProgress({ currentFile: xmlFile.name, fileBytesRead: 0, fileTotalBytes: 0, fileLogCount: null });
            setProgressMessage(`Parsing ${xmlFile.name}...`);
            const doc = parser.parseFromString(xmlFile.content, "application/xml");
            const logNodes = doc.querySelectorAll('log');
            if (logNodes.length === 0) continue;

            const entries = Array.from(logNodes).map(node => ({
                time: node.getAttribute('time') || '',
                level: node.getAttribute('level') || '',
                sndrtype: node.getAttribute('sndrtype') || '',
                sndrname: node.getAttribute('sndrname') || '',
                msg: node.getAttribute('msg') || '',
            }));
            
            totalLogsToInsert += entries.length;
            setProgressMessage(`Inserting ${entries.length.toLocaleString()} entries from ${xmlFile.name}`);
            setProgressPhase('inserting');
            
            let insertedCountInFile = 0;
            const handleProgress = (processed: number) => {
                insertedCountInFile = processed;
                setDetailedProgress(d => ({...d, fileLogCount: insertedCountInFile }));
                setProgress(30 + (70 * (i / xmlFiles.length + (processed / entries.length) / xmlFiles.length)));
            };
            
            try {
                targetDb.insertLogs(entries, xmlFile.name, handleProgress);
            } catch(e) {
                 throw new Error(`Database error in ${xmlFile.name}: ${(e as Error).message}`);
            }
        }
        
        setProgressPhase('indexing');
        setProgressMessage('Creating database indexes for faster queries...');
        targetDb.createIndexes();
        return totalLogsToInsert;
    }, [setProgress, setProgressMessage, setProgressPhase, setDetailedProgress]);

    const saveCurrentDbAsSession = useCallback(async (dbToSave: Database, name: string): Promise<boolean> => {
        if (!isElectron || !dbToSave) return false;
        const buffer = dbToSave.export();
        if (buffer.length <= 1024 && dbToSave.getTotalEntryCount() === 0) return true;
        
        try {
            const { success, error } = await window.electronAPI.saveSession(buffer, name);
            if (success) {
                setActiveSessionName(name);
                setIsDirty(false);
                await fetchSessions();
                return true;
            }
            throw new Error(error || 'Unknown error saving session.');
        } catch(e) {
            setError((e as Error).message);
            return false;
        }
    }, [isElectron, fetchSessions, setError]);

    const handleSaveSession = useCallback(async (): Promise<boolean> => {
        if (!isElectron || !db || !activeSessionName) return false;
        logToConsole(`Saving session '${activeSessionName}'...`, 'INFO');
        addToast({ id: 'save-session', type: 'progress', title: 'Saving Session', message: `Saving changes to ${activeSessionName}...`, progress: 50 });

        try {
            const success = await saveCurrentDbAsSession(db, activeSessionName);
            if (success) {
                logToConsole('Session saved.', 'INFO');
                addToast({ id: 'save-session', type: 'success', title: 'Session Saved', message: `Changes to '${activeSessionName}' have been saved.` });
            } else {
                throw new Error('Failed to save session via Electron API.');
            }
            return success;
        } catch (e) {
            const msg = e instanceof Error ? e.message : String(e);
            logToConsole(`Error saving session: ${msg}`, 'ERROR');
            addToast({ id: 'save-session', type: 'error', title: 'Save Failed', message: msg });
            return false;
        }
    }, [isElectron, db, activeSessionName, logToConsole, addToast, saveCurrentDbAsSession]);

     const handleLoadSession = useCallback(async (name: string) => {
        if (!window.electronAPI) return;
        setIsLoading(true);
        setProgress(0);
        setProgressPhase('loading');
        setProgressMessage(`Loading session: ${name}`);
        logToConsole(`Loading session '${name}'...`, 'INFO');
        setError(null);

        try {
            const result = await window.electronAPI.getSessionBuffer(name);
            if (!result.success || !result.buffer) {
                throw new Error(result.error || 'Failed to get session buffer.');
            }

            setProgress(50);
            const newDb = await Database.createFromBuffer(new Uint8Array(result.buffer));
            setDb(newDb);

            await updateStateFromDb(newDb);
            
            setActiveSessionName(name);
            setIsDirty(false);
            await fetchSessions();
            logToConsole(`Session '${name}' loaded successfully.`, 'INFO');
            addToast({ type: 'success', title: 'Session Loaded', message: `Successfully loaded '${name}'.` });
            setActiveView('viewer');
        } catch (e) {
            const msg = e instanceof Error ? e.message : String(e);
            setError(msg);
            logToConsole(`Error loading session: ${msg}`, 'ERROR');
            addToast({ type: 'error', title: 'Load Failed', message: msg });
            await handleNewSession(false);
        } finally {
            setIsLoading(false);
            setProgress(100);
        }
    }, [logToConsole, setError, setIsLoading, setProgress, setProgressMessage, setProgressPhase, updateStateFromDb, fetchSessions, addToast, setActiveView, handleNewSession]);

    const onRenameSession = useCallback(async (oldName: string, newName: string): Promise<boolean> => {
        if (!window.electronAPI) return false;
        try {
            const { success, error } = await window.electronAPI.renameSession(oldName, newName);
            if (!success) throw new Error(error);
            addToast({ type: 'success', title: 'Renamed', message: `Renamed '${oldName}' to '${newName}'.`});
            if (activeSessionName === oldName) setActiveSessionName(newName);
            await fetchSessions();
            return true;
        } catch(e) {
            const msg = (e as Error).message;
            addToast({ type: 'error', title: 'Rename Failed', message: msg });
            return false;
        }
    }, [activeSessionName, addToast, fetchSessions]);

    const onDeleteSession = useCallback(async (name: string) => {
        if (!window.electronAPI) return;
        try {
            const { success, error } = await window.electronAPI.deleteSession(name);
            if (!success) throw new Error(error);
            addToast({ type: 'info', title: 'Deleted', message: `Session '${name}' was deleted.` });
            if (activeSessionName === name) await handleNewSession();
            await fetchSessions();
        } catch(e) {
            addToast({ type: 'error', title: 'Delete Failed', message: (e as Error).message });
        }
    }, [activeSessionName, addToast, fetchSessions, handleNewSession]);

    const handleCreateNewSessionFromFiles = useCallback(async (files: FileList) => {
        if (files.length === 0) return;
        setIsLoading(true);
        setError(null);
        sessionNameHint.current = `session_from_${files[0].name.replace(/\.[^/.]+$/, "")}.sqlite`;

        try {
            const newDb = await Database.create();
            newDb.createTable();
            const count = await processFilesToDb(files, newDb);
            await updateStateFromDb(newDb);
            setDb(newDb);

            if (isElectron) {
                await saveCurrentDbAsSession(newDb, sessionNameHint.current);
            }
            addToast({ type: 'success', title: 'Processing Complete', message: `Successfully loaded ${count.toLocaleString()} log entries.` });
            setActiveView('viewer');
        } catch (e) {
            const msg = (e as Error).message;
            setError(msg);
            addToast({ type: 'error', title: 'Processing Failed', message: msg });
        } finally {
            setIsLoading(false);
        }
    }, [processFilesToDb, updateStateFromDb, isElectron, saveCurrentDbAsSession, addToast, setIsLoading, setError, setActiveView]);

    const handleAddFilesToCurrentSession = useCallback(async (files: FileList) => {
        if (!db) return;
        if (files.length === 0) return;
        setIsLoading(true);
        setError(null);

        try {
            const count = await processFilesToDb(files, db);
            await updateStateFromDb(db);
            setIsDirty(true);
            if (isElectron) {
                await handleSaveSession();
            }
            addToast({ type: 'success', title: 'Files Added', message: `Successfully added ${count.toLocaleString()} new log entries.` });
        } catch (e) {
            const msg = (e as Error).message;
            setError(msg);
            addToast({ type: 'error', title: 'Processing Failed', message: msg });
        } finally {
            setIsLoading(false);
        }
    }, [db, processFilesToDb, updateStateFromDb, isElectron, handleSaveSession, addToast, setIsLoading, setError]);
    
    const handleImportDb = useCallback(async (file: File) => {
        setIsLoading(true);
        setProgress(0);
        setProgressPhase('loading');
        setProgressMessage(`Importing ${file.name}`);
        setError(null);
        try {
            const buffer = await file.arrayBuffer();
            const newDb = await Database.createFromBuffer(new Uint8Array(buffer));
            await updateStateFromDb(newDb);
            setDb(newDb);
            if (isElectron) {
                await saveCurrentDbAsSession(newDb, file.name);
            }
            addToast({ type: 'success', title: 'Import Complete', message: `Successfully imported '${file.name}'.`});
            setActiveView('viewer');
        } catch (e) {
            const msg = e instanceof Error ? e.message : 'Invalid database file.';
            setError(msg);
            addToast({ type: 'error', title: 'Import Failed', message: msg });
        } finally {
            setIsLoading(false);
        }
    }, [updateStateFromDb, isElectron, saveCurrentDbAsSession, setIsLoading, setProgress, setProgressPhase, setProgressMessage, setError, addToast, setActiveView]);

    const handleDownloadDb = useCallback(() => {
        if (!db || !hasData) return;
        const data = db.export();
        const blob = new Blob([data], { type: 'application/x-sqlite3' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = activeSessionName || `log-analyser-export-${Date.now()}.sqlite`;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
    }, [db, hasData, activeSessionName]);


    // Electron-specific effects
    useEffect(() => {
        if (!isElectron) return;
        window.isAppDirty = () => ({ isDirty: isDirty, sessionName: activeSessionName });
        const removeListener = window.electronAPI.onSaveBeforeQuit(async () => {
            const success = await handleSaveSession();
            // Always quit, even if save fails, to prevent getting stuck. Toast will show error.
            window.electronAPI.savedAndReadyToQuit();
        });
        return () => { delete window.isAppDirty; removeListener(); };
    }, [isElectron, isDirty, activeSessionName, handleSaveSession]);
    
    useEffect(() => {
        if (isElectron) {
            const baseTitle = 'Log Analyser';
            let title = activeSessionName ? `${activeSessionName}${isDirty ? '*' : ''} - ${baseTitle}` : baseTitle;
            window.electronAPI.setTitle(title);
        }
    }, [isElectron, activeSessionName, isDirty]);

    const value = {
        db, isElectron, sessions, activeSessionName, isDirty, setIsDirty, hasData, totalEntryCount,
        overallTimeRange, loadedFileNames, error, handleNewSession, handleSaveSession, handleLoadSession,
        onRenameSession, onDeleteSession, handleCreateNewSessionFromFiles, handleAddFilesToCurrentSession,
        handleImportDb, handleDownloadDb, updateStateFromDb, overallStockTimeRange, setOverallStockTimeRange,
        overallStockDensity, setOverallStockDensity
    };
    
    return (
        <SessionContext.Provider value={value}>
            {children}
        </SessionContext.Provider>
    );
};

export const useSession = (): SessionContextType => {
    const context = useContext(SessionContext);
    if (context === undefined) {
        throw new Error('useSession must be used within a SessionProvider');
    }
    return context;
};
