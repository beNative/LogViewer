import React from 'react';
import { Dropzone } from './Dropzone.tsx';
import { Icon } from './icons/index.tsx';
import { SessionFile, IconSet } from '../types.ts';
import { SessionManager } from './SessionManager.tsx';

interface DataHubProps {
  onFileDrop: (files: FileList) => void;
  onImportDb: (file: File) => void;
  onDownloadDb: () => void;
  onNewSession: () => void;
  error: string | null;
  hasData: boolean;
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


export const DataHub: React.FC<DataHubProps> = ({ 
    onFileDrop,
    onImportDb,
    onDownloadDb,
    onNewSession,
    error,
    hasData,
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

  const handleImportClick = () => {
    importInputRef.current?.click();
  };

  const handleFileSelect = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files && e.target.files.length > 0) {
      onImportDb(e.target.files[0]);
      e.target.value = '';
    }
  };

  return (
    <div className="flex-grow grid grid-cols-1 xl:grid-cols-5 gap-8 p-4 sm:p-6 lg:p-8 bg-gray-100 dark:bg-gray-900/50 overflow-hidden">
        {/* Left Column: Actions */}
        <div className="xl:col-span-2 flex flex-col gap-8">
            <div className="flex-grow flex flex-col gap-8 p-6 bg-white dark:bg-gray-800/50 rounded-2xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
                <div>
                    <h2 className="text-xl font-bold text-gray-900 dark:text-white flex items-center gap-2">
                        <Icon name="DocumentPlus" iconSet={iconSet} className="w-6 h-6 text-sky-500" />
                        <span>Create New Session</span>
                    </h2>
                    <p className="text-sm text-gray-500 dark:text-gray-400 mt-1">Drag and drop log files to start a new analysis.</p>
                </div>
                <div className="flex-grow min-h-[200px]">
                    <Dropzone onFileDrop={onFileDrop} error={error} iconSet={iconSet} />
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

        {/* Right Column: Session Manager */}
        {isElectron && (
            <div className="xl:col-span-3 flex flex-col h-full min-h-0">
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
        )}
    </div>
  );
};