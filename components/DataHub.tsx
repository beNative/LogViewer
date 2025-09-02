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
  onSaveSession: () => Promise<boolean>;
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
    onSaveSession,
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

  const WebLayout = () => (
    <>
      <div>
        <h2 className="text-2xl font-bold text-gray-900 dark:text-white mb-4">Add Log Files</h2>
        <div className="p-1 bg-white dark:bg-gray-800/50 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
          <Dropzone onFileDrop={onFileDrop} error={error} iconSet={iconSet} />
        </div>
      </div>
      <div>
        <h2 className="text-2xl font-bold text-gray-900 dark:text-white mb-4">Manage Database</h2>
        <div className="bg-white dark:bg-gray-800/50 p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
            <h3 className="font-semibold text-lg text-gray-800 dark:text-gray-300 mb-4">Actions</h3>
            <div className="flex flex-wrap gap-4">
               <input type="file" ref={importInputRef} onChange={handleFileSelect} className="hidden" accept=".sqlite,application/x-sqlite3" />
               <button onClick={handleImportClick} className="inline-flex items-center gap-2 px-4 py-2 font-semibold rounded-lg transition-colors duration-200 bg-sky-600 hover:bg-sky-700 text-white dark:bg-sky-600 dark:hover:bg-sky-500">
                 <Icon name="ArrowUpTray" iconSet={iconSet} className="w-5 h-5"/><span>Import DB</span>
               </button>
               <button onClick={onDownloadDb} disabled={!hasData} className="inline-flex items-center gap-2 px-4 py-2 font-semibold rounded-lg transition-colors duration-200 bg-gray-200 hover:bg-gray-300 text-gray-800 dark:bg-gray-700 dark:hover:bg-gray-600 dark:text-gray-200 disabled:opacity-50 disabled:cursor-not-allowed">
                 <Icon name="Download" iconSet={iconSet} className="w-5 h-5"/><span>Download DB</span>
               </button>
               <button onClick={() => onNewSession()} disabled={!hasData} className="inline-flex items-center gap-2 px-4 py-2 font-semibold rounded-lg transition-colors duration-200 bg-red-600 hover:bg-red-700 text-white dark:bg-red-600 dark:hover:bg-red-500 disabled:opacity-50 disabled:cursor-not-allowed">
                 <Icon name="Trash" iconSet={iconSet} className="w-5 h-5"/><span>Clear All Data</span>
               </button>
            </div>
          </div>
      </div>
    </>
  );

  const ElectronLayout = () => (
    <div className="grid grid-cols-1 lg:grid-cols-2 gap-8 h-full">
        {/* Left Column: Session Management */}
        <div className="flex flex-col h-full">
            <SessionManager 
                sessions={sessions}
                activeSessionName={activeSessionName}
                onLoad={onLoadSession}
                onRename={onRenameSession}
                onDelete={onDeleteSession}
                onNew={onNewSession}
                isDirty={isDirty}
                onSave={onSaveSession}
                iconSet={iconSet}
            />
        </div>

        {/* Right Column: Create new and other actions */}
        <div className="space-y-8">
            <div id="create-new-session">
                <h2 className="text-2xl font-bold text-gray-900 dark:text-white mb-4">Add Files to Current Session</h2>
                 <div className="p-1 bg-white dark:bg-gray-800/50 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
                    <Dropzone onFileDrop={onFileDrop} error={error} iconSet={iconSet} />
                </div>
            </div>

            <div id="other-actions">
                 <h2 className="text-2xl font-bold text-gray-900 dark:text-white mb-4">Other Actions</h2>
                  <div className="bg-white dark:bg-gray-800/50 p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
                    <div className="flex flex-wrap gap-4">
                        <input type="file" ref={importInputRef} onChange={handleFileSelect} className="hidden" accept=".sqlite,application/x-sqlite3" />
                        <button onClick={handleImportClick} className="inline-flex items-center gap-2 px-4 py-2 font-semibold rounded-lg transition-colors duration-200 bg-gray-200 hover:bg-gray-300 text-gray-800 dark:bg-gray-700 dark:hover:bg-gray-600 dark:text-gray-200">
                            <Icon name="ArrowUpTray" iconSet={iconSet} className="w-5 h-5"/><span>Import External DB...</span>
                        </button>
                        <button onClick={onDownloadDb} disabled={!hasData} className="inline-flex items-center gap-2 px-4 py-2 font-semibold rounded-lg transition-colors duration-200 bg-gray-200 hover:bg-gray-300 text-gray-800 dark:bg-gray-700 dark:hover:bg-gray-600 dark:text-gray-200 disabled:opacity-50 disabled:cursor-not-allowed">
                            <Icon name="Download" iconSet={iconSet} className="w-5 h-5"/><span>Export Active Session...</span>
                        </button>
                    </div>
                </div>
            </div>
        </div>
    </div>
  );


  return (
    <div className="flex-grow flex flex-col p-4 sm:p-6 lg:p-8 space-y-8 overflow-y-auto bg-gray-100 dark:bg-transparent">
        {isElectron ? <ElectronLayout /> : <WebLayout />}
    </div>
  );
};