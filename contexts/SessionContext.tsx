import React, { createContext, useState, useCallback, useContext, useEffect, useRef } from 'react';
import { SessionFile, ProgressPhase } from '../types';
import { Database } from '../db';
import { useUI } from './UIContext';
import { useConsole } from './ConsoleContext';
import { useToast } from './ToastContext';
import { useSettings } from './SettingsContext';

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
    handleCancelProcessing: () => void;
    handleRebuildStockDataInWorker: () => Promise<void>;
};

const SessionContext = createContext<SessionContextType | undefined>(undefined);

export const SessionProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const { setIsLoading, setProgress, setProgressMessage, setProgressPhase, setDetailedProgress, setProgressTitle, setIsInitialLoad, addToast, error, setError, setActiveView } = useUI();
    const { logToConsole } = useConsole();
    const { logSqlQueries } = useSettings();
    
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
    const workerRef = useRef<Worker | null>(null);
    const [isProcessing, setIsProcessing] = useState(false);

    // State that is also part of DataContext but needed here for updates
    const [overallStockTimeRange, setOverallStockTimeRange] = useState<{ min: string, max: string } | null>(null);
    const [overallStockDensity, setOverallStockDensity] = useState<any[]>([]);

    // Effect to attach logger to the DB instance when it changes or the setting changes
    useEffect(() => {
        if (db) {
            db.setLogger(logToConsole, logSqlQueries);
        }
    }, [db, logToConsole, logSqlQueries]);

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
        newDb.createTable(); // Ensure tables exist even on a blank session
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
            database.createTable(); // Ensure tables exist on initial load
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

    const processFilesToDb = useCallback(async (files: FileList) => {
        return new Promise<{ db: Database, count: number }>(async (resolve, reject) => {
            if (isProcessing) {
                return reject(new Error("Another process is already running."));
            }
            setIsProcessing(true);
            if (workerRef.current) workerRef.current.terminate();

            const worker = new Worker('./dist/worker.js');
            workerRef.current = worker;

            worker.onmessage = async (event) => {
                const { type, payload } = event.data;
                switch (type) {
                    case 'progress':
                        setProgressPhase(payload.phase as ProgressPhase);
                        setProgressMessage(payload.message);
                        setProgress(payload.progress);
                        setDetailedProgress(payload.details);
                        break;
                    case 'done-import':
                        const finalDb = await Database.createFromBuffer(payload.dbBuffer);
                        worker.terminate();
                        workerRef.current = null;
                        setIsProcessing(false);
                        resolve({ db: finalDb, count: payload.count });
                        break;
                    case 'error':
                        worker.terminate();
                        workerRef.current = null;
                        setIsProcessing(false);
                        reject(new Error(payload.error));
                        break;
                }
            };
            
            worker.onerror = (err) => {
                worker.terminate();
                workerRef.current = null;
                setIsProcessing(false);
                reject(new Error(`Worker error: ${err.message}`));
            };
            
            const xmlFiles: { name: string; content: string }[] = [];
            let totalSize = Array.from(files).reduce((acc, file) => acc + file.size, 0);
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
                        setIsProcessing(false);
                        return reject(new Error(`Failed to read zip file ${file.name}: ${(e as Error).message}`));
                    }
                } else if (file.name.endsWith('.xml')) {
                    const content = await file.text();
                    xmlFiles.push({ name: file.name, content });
                }
                bytesRead += file.size;
                setProgress(30 * (bytesRead / totalSize));
            }
            
            worker.postMessage({ type: 'import-logs', payload: { xmlFiles } });
        });
    }, [isProcessing, setProgress, setProgressMessage, setProgressPhase, setDetailedProgress]);

    const handleCancelProcessing = useCallback(() => {
        if (workerRef.current) {
            workerRef.current.terminate();
            workerRef.current = null;
            setIsLoading(false);
            setIsProcessing(false);
            setError('Processing was cancelled by the user.');
            logToConsole('User cancelled file processing.', 'WARNING');
            addToast({ type: 'warning', title: 'Cancelled', message: 'The file processing was cancelled.'});
        }
    }, [setIsLoading, setError, logToConsole, addToast]);

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
        setProgressTitle("Processing Files...");
        sessionNameHint.current = `session_from_${files[0].name.replace(/\.[^/.]+$/, "")}.sqlite`;

        try {
            const { db: newDb, count } = await processFilesToDb(files);
            await updateStateFromDb(newDb);
            setDb(newDb);

            if (isElectron) {
                await saveCurrentDbAsSession(newDb, sessionNameHint.current);
            }
            addToast({ type: 'success', title: 'Processing Complete', message: `Successfully loaded ${count.toLocaleString()} log entries.` });
            setActiveView('viewer');
        } catch (e) {
            const msg = (e as Error).message;
            if (msg.includes('cancelled')) return;
            setError(msg);
            addToast({ type: 'error', title: 'Processing Failed', message: msg });
        } finally {
            setIsLoading(false);
        }
    }, [processFilesToDb, updateStateFromDb, isElectron, saveCurrentDbAsSession, addToast, setIsLoading, setError, setActiveView, setProgressTitle]);

    const handleAddFilesToCurrentSession = useCallback(async (files: FileList) => {
        logToConsole("Adding files to an existing session is not yet supported with Web Workers. Creating a new session instead.", "INFO");
        await handleCreateNewSessionFromFiles(files);
    }, [handleCreateNewSessionFromFiles, logToConsole]);
    
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
    
    const handleRebuildStockDataInWorker = useCallback(async () => {
        return new Promise<void>(async (resolve, reject) => {
            if (!db || !hasData) {
                const message = 'No data is currently loaded.';
                logToConsole(`Rebuild failed: ${message}`, 'ERROR');
                addToast({ type: 'error', title: 'Rebuild Failed', message });
                return reject(new Error(message));
            }
            if (isProcessing) {
                 const message = 'Another process is already running.';
                 logToConsole(`Rebuild failed: ${message}`, 'WARNING');
                addToast({ type: 'warning', title: 'Busy', message });
                return reject(new Error(message));
            }

            setIsLoading(true);
            setIsProcessing(true);
            setProgressTitle("Rebuilding Stock Data...");
            setProgress(0);
            setDetailedProgress({ currentFile: '', fileBytesRead: 0, fileTotalBytes: 0, fileLogCount: null });

            if (workerRef.current) workerRef.current.terminate();

            const worker = new Worker('./dist/worker.js');
            workerRef.current = worker;
            const dbBuffer = db.export();
            
            worker.onmessage = async (event) => {
                const { type, payload } = event.data;
                switch (type) {
                    case 'progress':
                        setProgressPhase(payload.phase as ProgressPhase);
                        setProgressMessage(payload.message);
                        setProgress(payload.progress);
                        break;
                    case 'done-rebuild':
                        const newDb = await Database.createFromBuffer(payload.dbBuffer);
                        await updateStateFromDb(newDb);
                        setDb(newDb);
                        setIsDirty(true);
                        await handleSaveSession();
                        worker.terminate();
                        workerRef.current = null;
                        setIsLoading(false);
                        setIsProcessing(false);
                        addToast({ type: 'success', title: 'Rebuild Complete', message: `Successfully rebuilt ${payload.count.toLocaleString()} stock entries.` });
                        resolve();
                        break;
                    case 'error':
                        const msg = payload.error || "An unknown worker error occurred.";
                        logToConsole(`Stock rebuild failed in worker: ${msg}`, 'ERROR');
                        addToast({ type: 'error', title: 'Rebuild Failed', message: msg });
                        worker.terminate();
                        workerRef.current = null;
                        setIsLoading(false);
                        setIsProcessing(false);
                        reject(new Error(msg));
                        break;
                }
            };
            
            worker.onerror = (err) => {
                const msg = err.message || "The rebuild worker crashed unexpectedly.";
                logToConsole(`Stock rebuild worker crashed: ${msg}`, 'ERROR');
                worker.terminate();
                workerRef.current = null;
                setIsLoading(false);
                setIsProcessing(false);
                addToast({ type: 'error', title: 'Rebuild Failed', message: msg });
                reject(err);
            };
            
            worker.postMessage({ type: 'rebuild-stock', payload: { dbBuffer } }, [dbBuffer.buffer]);
        });
    }, [db, hasData, addToast, isProcessing, setIsLoading, setProgress, setProgressMessage, setProgressPhase, setProgressTitle, updateStateFromDb, handleSaveSession, setDetailedProgress, logToConsole]);

    // Electron-specific effects
    useEffect(() => {
        if (!isElectron) return;
        window.isAppDirty = () => ({ isDirty: isDirty, sessionName: activeSessionName });
        const removeListener = window.electronAPI.onSaveBeforeQuit(async () => {
            await handleSaveSession();
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
        overallStockDensity, setOverallStockDensity, handleCancelProcessing, handleRebuildStockDataInWorker
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