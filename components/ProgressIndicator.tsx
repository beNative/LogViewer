import React from 'react';
import { formatBytes } from '../utils.ts';
import { Icon } from './icons/index.tsx';
import { IconSet, ProgressPhase } from '../types.ts';

type IconName = React.ComponentProps<typeof Icon>['name'];

interface ProgressIndicatorProps {
  progress: number;
  message: string;
  phase: ProgressPhase;
  detailedProgress: {
    currentFile: string;
    fileBytesRead: number;
    fileTotalBytes: number;
    fileLogCount: number | null;
  }
  iconSet: IconSet;
}

const phaseDetails: Record<ProgressPhase, { iconName: IconName; label: string, isAnimated?: boolean }> = {
    reading: { iconName: 'File', label: 'Reading Files' },
    unzipping: { iconName: 'ArchiveBox', label: 'Unzipping Archives' },
    parsing: { iconName: 'CodeBracket', label: 'Parsing Data' },
    inserting: { iconName: 'Database', label: 'Inserting to Database' },
    indexing: { iconName: 'Database', label: 'Creating Indexes', isAnimated: true },
    loading: { iconName: 'ArrowPath', label: 'Loading...', isAnimated: true },
};


export const ProgressIndicator: React.FC<ProgressIndicatorProps> = ({ progress, message, phase, detailedProgress, iconSet }) => {
  const currentPhase = phaseDetails[phase];

  return (
    <div className="fixed inset-0 bg-gray-900 bg-opacity-80 dark:bg-opacity-80 flex flex-col items-center justify-center z-50 transition-opacity duration-300">
      <div className="w-full max-w-lg p-8 bg-white dark:bg-gray-800 rounded-2xl shadow-xl text-center">
        <h2 className="text-xl font-semibold text-gray-900 dark:text-white mb-4">Processing Files...</h2>
        
        <div className="flex items-center justify-center gap-3 my-4 text-sky-600 dark:text-sky-400">
            <Icon name={currentPhase.iconName} iconSet={iconSet} className={`w-8 h-8 ${currentPhase.isAnimated ? 'animate-spin' : ''}`} />
            <span className="text-lg font-medium">{currentPhase.label}</span>
        </div>

        <p className="text-gray-600 dark:text-gray-300 mb-6 h-10 flex items-center justify-center text-center px-4">{message}</p>
        
        {detailedProgress.currentFile && (
            <div className="text-sm text-gray-500 dark:text-gray-400 mb-4 w-full bg-gray-100 dark:bg-gray-700/50 p-3 rounded-lg text-left font-mono space-y-1">
                <p className="truncate">
                    <span className="font-semibold text-gray-700 dark:text-gray-300">File:</span> {detailedProgress.currentFile}
                </p>
                {detailedProgress.fileTotalBytes > 0 && (
                    <p>
                        <span className="font-semibold text-gray-700 dark:text-gray-300">Size:</span> {formatBytes(detailedProgress.fileBytesRead)} / {formatBytes(detailedProgress.fileTotalBytes)}
                    </p>
                )}
                {detailedProgress.fileLogCount !== null && (
                     <p>
                        <span className="font-semibold text-gray-700 dark:text-gray-300">Entries Found:</span> {detailedProgress.fileLogCount.toLocaleString()}
                     </p>
                )}
            </div>
        )}
        
        <div className="w-full bg-gray-200 dark:bg-gray-700 rounded-full h-4">
          <div
            className="bg-sky-500 h-4 rounded-full transition-all duration-300 ease-out"
            style={{ width: `${progress}%` }}
          ></div>
        </div>
        <p className="text-gray-900 dark:text-white font-mono text-2xl mt-4">{progress.toFixed(0)}%</p>
      </div>
    </div>
  );
};
