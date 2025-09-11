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

  const UnifiedLayout = () => (
    <div className="max-w-6xl mx-auto w-full space-y-12">
        {/* Section 1: Create New Session */}
        <div>
            <div className="text-center mb-6">
                <Icon name="DocumentPlus" iconSet={iconSet} className="w-12 h-12 mx-auto text-sky-500" />
                <h2 className="mt-2 text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">Create a New Session</h2>
                <p className="mt-2 text-lg leading-8 text-gray-600 dark:text-gray-400">
                    Drag & drop log files (.xml or .zip) below to start a new analysis.
                </p>
            </div>
            <div className="p-1 bg-white dark:bg-gray-800/50 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-lg">
                <Dropzone onFileDrop={onFileDrop} error={error} iconSet={iconSet} />
            </div>
        </div>

        {isElectron && (
          <>
            <div className="relative">
                <div className="absolute inset-0 flex items-center" aria-hidden="true">
                    <div className="w-full border-t border-gray-300 dark:border-gray-600" />
                </div>
                <div className="relative flex justify-center">
                    <span className="bg-gray-100 dark:bg-gray-900/50 px-3 text-base font-semibold leading-6 text-gray-900 dark:text-white">Or</span>
                </div>
            </div>

            {/* Section 2: Manage Existing Sessions */}
            <div className="space-y-8">
                <div className="text-center">
                    <Icon name="Folder" iconSet={iconSet} className="w-12 h-12 mx-auto text-purple-500" />
                    <h2 className="mt-2 text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">Manage Recent Sessions</h2>
                    <p className="mt-2 text-lg leading-8 text-gray-600 dark:text-gray-400">
                        Load, rename, or delete your previously saved sessions.
                    </p>
                </div>

                <div className="bg-white dark:bg-gray-800/50 p-4 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
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
          </>
        )}

        {/* Section 3: Other Actions */}
        <div className="bg-white dark:bg-gray-800/50 p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
            <h3 className="text-lg font-semibold text-gray-800 dark:text-gray-200 mb-4">Other Actions</h3>
            <div className="flex flex-wrap gap-4">
                <input type="file" ref={importInputRef} onChange={handleFileSelect} className="hidden" accept=".sqlite,application/x-sqlite3" />
                <button onClick={handleImportClick} className="inline-flex items-center gap-2 px-4 py-2 font-semibold rounded-lg transition-colors duration-200 bg-gray-200 hover:bg-gray-300 text-gray-800 dark:bg-gray-700 dark:hover:bg-gray-600 dark:text-gray-200">
                    <Icon name="ArrowUpTray" iconSet={iconSet} className="w-5 h-5"/><span>Import External DB...</span>
                </button>
                <button onClick={onDownloadDb} disabled={!hasData} className="inline-flex items-center gap-2 px-4 py-2 font-semibold rounded-lg transition-colors duration-200 bg-gray-200 hover:bg-gray-300 text-gray-800 dark:bg-gray-700 dark:hover:bg-gray-600 dark:text-gray-200 disabled:opacity-50 disabled:cursor-not-allowed">
                    <Icon name="Download" iconSet={iconSet} className="w-5 h-5"/><span>Export Active Session...</span>
                </button>
                <button onClick={onNewSession} className="inline-flex items-center gap-2 px-4 py-2 font-semibold rounded-lg transition-colors duration-200 bg-gray-200 hover:bg-gray-300 text-gray-800 dark:bg-gray-700 dark:hover:bg-gray-600 dark:text-gray-200">
                    <Icon name="DocumentPlus" iconSet={iconSet} className="w-5 h-5"/><span>New Blank Session</span>
                </button>
            </div>
        </div>
    </div>
  );

  return (
    <div className="flex-grow flex flex-col p-4 sm:p-6 lg:p-8 overflow-y-auto bg-gray-100 dark:bg-gray-900/50">
        <UnifiedLayout />
    </div>
  );
};