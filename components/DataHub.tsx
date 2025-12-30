import React from 'react';
import { Dropzone } from './Dropzone';
import { Icon } from './icons';
import { SessionFile, IconSet, OverallTimeRange } from '../types';
import { SessionManager } from './SessionManager';
import { formatBytes } from '../utils';
import { Tooltip } from './Tooltip.tsx';

interface DataHubProps {
    onCreateSessionFromFiles: (files: FileList) => void;
    onAddFilesToSession: (files: FileList) => void;
    onImportDb: (file: File) => void;
    onDownloadDb: () => void;
    onNewSession: () => void;
    error: string | null;
    hasData: boolean;
    totalEntryCount: number;
    overallTimeRange: OverallTimeRange | null;
    loadedFileNames: string[];
    isElectron: boolean;
    sessions: SessionFile[];
    activeSessionName: string | null;
    onLoadSession: (name: string) => void;
    onRenameSession: (oldName: string, newName: string) => Promise<boolean>;
    onDeleteSession: (name: string) => void;
    isDirty: boolean;
    iconSet: IconSet;
}

const ActionButton: React.FC<{
    icon: React.ComponentProps<typeof Icon>['name'];
    title: string;
    description: string;
    onClick: () => void;
    disabled?: boolean;
    iconSet: IconSet;
}> = ({ icon, title, description, onClick, disabled = false, iconSet }) => (
    <button
        onClick={onClick}
        disabled={disabled}
        className="w-full text-left p-4 rounded-lg flex items-center gap-4 transition-all duration-200 hover:bg-gray-200/50 dark:hover:bg-gray-700/50 disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:bg-transparent"
    >
        <div className="flex-shrink-0 w-10 h-10 rounded-lg bg-gray-200 dark:bg-gray-700 flex items-center justify-center">
            <Icon name={icon} iconSet={iconSet} className="w-6 h-6 text-gray-600 dark:text-gray-300" />
        </div>
        <div>
            <p className="font-semibold text-gray-800 dark:text-gray-200">{title}</p>
            <p className="text-sm text-gray-500 dark:text-gray-400">{description}</p>
        </div>
        <Icon name="ChevronRight" iconSet={iconSet} className="w-5 h-5 ml-auto text-gray-400 dark:text-gray-500" />
    </button>
);

const ActiveSessionInfo: React.FC<{
    session: SessionFile;
    totalEntryCount: number;
    overallTimeRange: OverallTimeRange | null;
    loadedFileNames: string[];
    onAddFilesToSession: (files: FileList) => void;
    onRenameSession: (oldName: string, newName: string) => Promise<boolean>;
    iconSet: IconSet;
}> = ({ session, totalEntryCount, overallTimeRange, loadedFileNames, onAddFilesToSession, onRenameSession, iconSet }) => {

    const timeRangeString = overallTimeRange
        ? `${new Date(overallTimeRange.min).toLocaleString()} - ${new Date(overallTimeRange.max).toLocaleString()}`
        : 'N/A';

    const [isRenaming, setIsRenaming] = React.useState(false);
    const [newName, setNewName] = React.useState(session.name);
    const inputRef = React.useRef<HTMLInputElement>(null);

    React.useEffect(() => {
        setNewName(session.name);
        setIsRenaming(false);
    }, [session.name]);

    React.useEffect(() => {
        if (isRenaming) {
            inputRef.current?.focus();
            inputRef.current?.select();
        }
    }, [isRenaming]);

    const handleRename = async () => {
        if (newName && newName !== session.name) {
            const success = await onRenameSession(session.name, newName);
            if (success) {
                setIsRenaming(false);
            }
        } else {
            setNewName(session.name);
            setIsRenaming(false);
        }
    };

    const handleKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
        if (e.key === 'Enter') handleRename();
        if (e.key === 'Escape') {
            setNewName(session.name);
            setIsRenaming(false);
        }
    };

    return (
        <div className="p-6 bg-white dark:bg-gray-800/50 rounded-2xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm flex-shrink-0">
            <div className="group flex items-center justify-between gap-4 mb-4">
                <div className="flex items-center gap-2 min-w-0">
                    <Icon name="Database" iconSet={iconSet} className="w-6 h-6 text-sky-500 flex-shrink-0" />
                    {isRenaming ? (
                        <input
                            ref={inputRef}
                            type="text"
                            value={newName}
                            onChange={(e) => setNewName(e.target.value)}
                            onBlur={handleRename}
                            onKeyDown={handleKeyDown}
                            className="w-full bg-white dark:bg-gray-700 border-gray-300 dark:border-gray-600 text-xl font-bold text-sky-600 dark:text-sky-400 rounded-md shadow-sm focus:ring-sky-500 focus:border-sky-500 transition"
                        />
                    ) : (
                        <h2 className="text-xl font-bold text-gray-900 dark:text-white truncate">
                            <span>Active Session: <span className="text-sky-600 dark:text-sky-400">{session.name}</span></span>
                        </h2>
                    )}
                </div>
                {!isRenaming && (
                    <Tooltip content="Rename Session">
                        <button
                            onClick={() => setIsRenaming(true)}
                            className="p-2 text-gray-400 hover:text-gray-800 dark:hover:text-white rounded-full hover:bg-gray-200 dark:hover:bg-gray-700 opacity-0 group-hover:opacity-100 focus:opacity-100 transition-opacity"
                        >
                            <Icon name="PencilSquare" iconSet={iconSet} className="w-5 h-5" />
                        </button>
                    </Tooltip>
                )}
            </div>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-x-6 gap-y-3 text-sm">
                <div className="flex justify-between border-b border-gray-200 dark:border-gray-700/50 py-1.5">
                    <span className="text-gray-500 dark:text-gray-400">Total Records:</span>
                    <span className="font-semibold text-gray-800 dark:text-gray-200">{totalEntryCount.toLocaleString()}</span>
                </div>
                <div className="flex justify-between border-b border-gray-200 dark:border-gray-700/50 py-1.5">
                    <span className="text-gray-500 dark:text-gray-400">Database Size:</span>
                    <span className="font-semibold text-gray-800 dark:text-gray-200">{formatBytes(session.size)}</span>
                </div>
                <div className="md:col-span-2 flex justify-between border-b border-gray-200 dark:border-gray-700/50 py-1.5">
                    <span className="text-gray-500 dark:text-gray-400">Time Range:</span>
                    <span className="font-semibold text-gray-800 dark:text-gray-200 font-mono">{timeRangeString}</span>
                </div>
                <div className="md:col-span-2 pt-2">
                    <p className="text-gray-500 dark:text-gray-400 mb-1">Included Log Files ({loadedFileNames.length}):</p>
                    <div className="max-h-20 overflow-y-auto bg-gray-100 dark:bg-gray-900/50 rounded-md p-2 text-xs font-mono text-gray-600 dark:text-gray-400">
                        {loadedFileNames.map(name => <div key={name} className="truncate" title={name}>{name}</div>)}
                    </div>
                </div>
            </div>
            <div className="mt-4 border-t border-gray-200 dark:border-gray-700 pt-4">
                <h3 className="text-md font-semibold text-gray-800 dark:text-gray-200">Add Log Files</h3>
                <p className="text-sm text-gray-500 dark:text-gray-400 mb-2">Drop more files below to add them to this session. The session will be saved automatically.</p>
                <div className="h-40">
                    <Dropzone onFileDrop={onAddFilesToSession} iconSet={iconSet} title="Drop files to add" />
                </div>
            </div>
        </div>
    );
};


export const DataHub: React.FC<DataHubProps> = ({
    onCreateSessionFromFiles,
    onAddFilesToSession,
    onImportDb,
    onDownloadDb,
    onNewSession,
    error,
    hasData,
    totalEntryCount,
    overallTimeRange,
    loadedFileNames,
    isElectron,
    sessions,
    activeSessionName,
    onLoadSession,
    onRenameSession,
    onDeleteSession,
    isDirty,
    iconSet,
}) => {
    const importInputRef = React.useRef<HTMLInputElement>(null);

    const activeSessionDetails = activeSessionName ? sessions.find(s => s.name === activeSessionName) : null;

    const handleImportClick = () => {
        importInputRef.current?.click();
    };

    const handleFileSelect = (e: React.ChangeEvent<HTMLInputElement>) => {
        if (e.target.files && e.target.files.length > 0) {
            onImportDb(e.target.files[0]);
            e.target.value = '';
        }
    };

    // Render the full-featured desktop layout
    if (isElectron) {
        return (
            <div className="flex-grow bg-gray-100 dark:bg-gray-900/50 overflow-y-auto">
                <div className="grid grid-cols-1 xl:grid-cols-5 gap-8 p-4 sm:p-6 lg:p-8">
                    {/* Left Column: Session Management */}
                    <div className="xl:col-span-3 flex flex-col h-full min-h-0 gap-6">
                        {hasData && activeSessionDetails && (
                            <ActiveSessionInfo
                                session={activeSessionDetails}
                                totalEntryCount={totalEntryCount}
                                overallTimeRange={overallTimeRange}
                                loadedFileNames={loadedFileNames}
                                onAddFilesToSession={onAddFilesToSession}
                                onRenameSession={onRenameSession}
                                iconSet={iconSet}
                            />
                        )}
                        <div className="flex-grow flex flex-col p-6 bg-white dark:bg-gray-800/50 rounded-2xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm min-h-0">
                            <h2 className="text-xl font-bold text-gray-900 dark:text-white flex items-center gap-2 mb-4 flex-shrink-0">
                                <Icon name="Folder" iconSet={iconSet} className="w-6 h-6 text-amber-500" />
                                <span>Recent Sessions</span>
                            </h2>
                            <div className="flex-grow overflow-y-auto -mr-2 pr-2">
                                <SessionManager
                                    sessions={sessions}
                                    activeSessionName={activeSessionName}
                                    onLoad={onLoadSession}
                                    onRename={onRenameSession}
                                    onDelete={onDeleteSession}
                                    isDirty={isDirty}
                                    iconSet={iconSet}
                                />
                            </div>
                        </div>
                    </div>

                    {/* Right Column: Actions */}
                    <div className="xl:col-span-2 flex flex-col gap-8">
                        <div className="flex flex-col gap-8 p-6 bg-white dark:bg-gray-800/50 rounded-2xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
                            <div>
                                <h2 className="text-xl font-bold text-gray-900 dark:text-white flex items-center gap-2">
                                    <Icon name="DocumentPlus" iconSet={iconSet} className="w-6 h-6 text-sky-500" />
                                    <span>Create New Session</span>
                                </h2>
                                <p className="text-sm text-gray-500 dark:text-gray-400 mt-1">Drag and drop log files to start a new analysis. A new session will be created and saved automatically.</p>
                            </div>
                            <div className="h-64">
                                <Dropzone onFileDrop={onCreateSessionFromFiles} error={error} iconSet={iconSet} />
                            </div>
                        </div>

                        <div className="p-6 bg-white dark:bg-gray-800/50 rounded-2xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
                            <h2 className="text-xl font-bold text-gray-900 dark:text-white flex items-center gap-2 mb-2">
                                <Icon name="Cog" iconSet={iconSet} className="w-6 h-6 text-purple-500" />
                                <span>More Actions</span>
                            </h2>
                            <div className="divide-y divide-gray-200 dark:divide-gray-700/60 -mx-4">
                                <input type="file" ref={importInputRef} onChange={handleFileSelect} className="hidden" accept=".sqlite,application/x-sqlite3" />
                                <ActionButton icon="ArrowUpTray" title="Import Session" description="Load an external .sqlite database file." onClick={handleImportClick} iconSet={iconSet} />
                                <ActionButton icon="Download" title="Export Active Session" description="Save the current session to a file." onClick={onDownloadDb} disabled={!hasData} iconSet={iconSet} />
                                <ActionButton icon="ArchiveBox" title="New Blank Session" description="Start a fresh, empty session." onClick={onNewSession} iconSet={iconSet} />
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        );
    }

    // Render the simplified web layout
    return (
        <div className="flex-grow bg-gray-100 dark:bg-gray-900/50 overflow-y-auto p-4 sm:p-6 lg:p-8">
            <div className="max-w-3xl mx-auto">
                <div className="text-center p-8 bg-white dark:bg-gray-800/50 rounded-2xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
                    <Icon name="InformationCircle" iconSet={iconSet} className="w-16 h-16 text-sky-500 dark:text-sky-400 mx-auto mb-4" />
                    <h2 className="text-2xl font-bold text-gray-900 dark:text-white">Desktop Version Recommended</h2>
                    <p className="mt-2 text-gray-600 dark:text-gray-300">
                        For the best experience, including session management, automatic saving, and application updates, please use the desktop application.
                    </p>
                    <p className="mt-1 text-sm text-gray-500 dark:text-gray-400">
                        The web version provides a basic, temporary log viewer.
                    </p>
                </div>

                <div className="mt-8 p-6 bg-white dark:bg-gray-800/50 rounded-2xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
                    <h2 className="text-xl font-bold text-gray-900 dark:text-white flex items-center gap-2">
                        <Icon name="DocumentPlus" iconSet={iconSet} className="w-6 h-6 text-sky-500" />
                        <span>Load Log Files</span>
                    </h2>
                    <p className="text-sm text-gray-500 dark:text-gray-400 mt-1 mb-4">Drag and drop log files to start a temporary analysis session.</p>
                    <div className="h-64">
                        <Dropzone onFileDrop={onCreateSessionFromFiles} error={error} iconSet={iconSet} />
                    </div>
                </div>
            </div>
        </div>
    );
};