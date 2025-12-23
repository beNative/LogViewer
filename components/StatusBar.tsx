import React from 'react';
import { ConsoleMessage, IconSet, LogTableDensity, Theme } from '../types.ts';
import { Icon } from './icons/index.tsx';
import { Tooltip } from './Tooltip.tsx';
import { DensityControl } from './DensityControl.tsx';

// Props interface
interface StatusBarProps {
    totalEntries: number;
    filteredCount: number;
    activeSessionName: string | null;
    isDirty: boolean;
    currentPage: number;
    totalPages: number;
    visibleRowCount: number;
    pageSize: number;
    onPageSizeChange: (size: number) => void;
    onGoToPage: (page: number) => void;
    isBusy: boolean;
    lastConsoleMessage: ConsoleMessage | null;
    theme: Theme;
    onThemeChange: () => void;
    iconSet: IconSet;
    logTableDensity: LogTableDensity;
    onLogTableDensityChange: (density: LogTableDensity) => void;
    isDetailPanelVisible: boolean;
    onDetailPanelVisibilityChange: (isVisible: boolean) => void;
}

const StatItem: React.FC<{ icon: React.ReactNode; label: string; value: string | number; title?: string }> = ({ icon, label, value, title }) => (
    <Tooltip content={title || `${label}: ${value}`}>
        <div className="flex items-center gap-1.5 px-2 border-r border-gray-300 dark:border-gray-600 last:border-r-0">
            {icon}
            <span className="font-semibold text-gray-700 dark:text-gray-200 truncate">{value}</span>
        </div>
    </Tooltip>
);

const getMessageColorClass = (type: ConsoleMessage['type']) => {
    switch (type) {
        case 'ERROR': return 'text-red-600 dark:text-red-400';
        case 'WARNING': return 'text-orange-500 dark:text-orange-400';
        case 'DEBUG': return 'text-green-600 dark:text-green-400';
        case 'INFO': return 'text-blue-600 dark:text-blue-400';
        default: return 'text-gray-600 dark:text-gray-400';
    }
}

export const StatusBar: React.FC<StatusBarProps> = (props) => {
    const {
        totalEntries, filteredCount, activeSessionName, isDirty,
        currentPage, totalPages, visibleRowCount,
        isBusy, lastConsoleMessage, theme, onThemeChange, iconSet,
        pageSize, onPageSizeChange, onGoToPage,
        logTableDensity, onLogTableDensityChange,
        isDetailPanelVisible, onDetailPanelVisibilityChange
    } = props;

    const startEntry = (currentPage - 1) * pageSize + 1;
    const endEntry = startEntry + visibleRowCount - 1;

    return (
        <footer className="flex-shrink-0 flex items-center justify-between px-3 py-1 border-t border-gray-200 dark:border-gray-700 bg-gray-100 dark:bg-gray-800 text-sm text-gray-600 dark:text-gray-400">
            {/* Left Section */}
            <div className="flex items-center gap-2">
                <StatItem icon={<Icon name="Database" iconSet={iconSet} className="w-4 h-4" />} label="Total" value={totalEntries.toLocaleString()} title="Total Entries in Database" />
                <StatItem icon={<Icon name="Filter" iconSet={iconSet} className="w-4 h-4" />} label="Filtered" value={filteredCount.toLocaleString()} title="Filtered Entries" />
                <div className="flex items-center gap-2 pl-2">
                    <Icon name="Folder" iconSet={iconSet} className="w-4 h-4 text-sky-600 dark:text-sky-500" />
                    <span className="font-semibold text-gray-800 dark:text-gray-200">{activeSessionName || 'Unsaved Session'}{isDirty ? '*' : ''}</span>
                </div>
            </div>

            {/* Middle Section */}
            <div className="flex items-center gap-4">
                {totalPages > 0 && (
                    <div className="flex items-center gap-4">
                        <span className="whitespace-nowrap">
                            Showing <span className="font-semibold text-gray-800 dark:text-gray-200">{startEntry.toLocaleString()}-{endEntry.toLocaleString()}</span>
                        </span>
                        <div className="flex items-center space-x-1">
                            <button onClick={() => onGoToPage(currentPage - 1)} disabled={currentPage === 1} className="p-1 rounded-md hover:bg-gray-200 dark:hover:bg-gray-700 disabled:opacity-50 disabled:cursor-not-allowed">
                                <Icon name="ChevronLeft" iconSet={iconSet} className="w-5 h-5" />
                            </button>
                            <span>Page <span className="font-semibold text-gray-800 dark:text-gray-200">{currentPage}</span> of <span className="font-semibold text-gray-800 dark:text-gray-200">{totalPages}</span></span>
                            <button onClick={() => onGoToPage(currentPage + 1)} disabled={currentPage === totalPages} className="p-1 rounded-md hover:bg-gray-200 dark:hover:bg-gray-700 disabled:opacity-50 disabled:cursor-not-allowed">
                                <Icon name="ChevronRight" iconSet={iconSet} className="w-5 h-5" />
                            </button>
                        </div>
                        <div className="flex items-center space-x-2">
                            <label htmlFor="pageSizeStatus" className="text-sm">Rows/page:</label>
                            <select
                                id="pageSizeStatus"
                                value={pageSize}
                                onChange={(e) => onPageSizeChange(Number(e.target.value))}
                                className="bg-gray-50 dark:bg-gray-700 border-gray-300 dark:border-gray-600 text-gray-900 dark:text-white sm:text-xs rounded-md shadow-sm focus:ring-sky-500 focus:border-sky-500 transition py-0.5"
                            >
                                <option value="100">100</option>
                                <option value="500">500</option>
                                <option value="1000">1000</option>
                                <option value="5000">5000</option>
                                <option value="10000">10000</option>
                            </select>
                        </div>
                    </div>
                )}
                {(totalPages > 0) && (
                    <div className="flex items-center gap-2">
                        <div className="h-4 w-px bg-gray-300 dark:bg-gray-600" />
                        <DensityControl value={logTableDensity} onChange={onLogTableDensityChange} />
                        <Tooltip content={isDetailPanelVisible ? "Hide Details" : "Show Details"}>
                            <button
                                onClick={() => onDetailPanelVisibilityChange(!isDetailPanelVisible)}
                                className={`p-1.5 rounded-md transition-colors ${isDetailPanelVisible
                                        ? 'bg-sky-600 text-white hover:bg-sky-700'
                                        : 'text-gray-500 dark:text-gray-400 hover:bg-gray-200 dark:hover:bg-gray-700'
                                    }`}
                            >
                                <Icon name="SidebarRight" iconSet={iconSet} className="w-5 h-5" />
                            </button>
                        </Tooltip>
                    </div>
                )}
            </div>

            {/* Right Section */}
            <div className="flex items-center gap-4">
                {isBusy ? (
                    <div className="flex items-center gap-2 text-sky-600 dark:text-sky-400 animate-pulse">
                        <Icon name="ArrowPath" iconSet={iconSet} className="w-4 h-4 animate-spin" />
                        <span>Processing...</span>
                    </div>
                ) : (
                    lastConsoleMessage && (
                        <div className={`flex items-center gap-2 truncate ${getMessageColorClass(lastConsoleMessage.type)}`}>
                            <Icon name={
                                lastConsoleMessage.type === 'ERROR' ? 'XCircle' :
                                    lastConsoleMessage.type === 'WARNING' ? 'ExclamationTriangle' :
                                        lastConsoleMessage.type === 'DEBUG' ? 'CheckCircle' :
                                            'InformationCircle'
                            } iconSet={iconSet} className="w-4 h-4 flex-shrink-0" />
                            <span className="truncate" title={lastConsoleMessage.message}>{lastConsoleMessage.message}</span>
                        </div>
                    )
                )}
                <Tooltip content="Toggle Theme">
                    <button onClick={onThemeChange} className="p-1 rounded-md hover:bg-gray-200 dark:hover:bg-gray-700">
                        <Icon name={theme === 'dark' ? 'Sun' : 'Moon'} iconSet={iconSet} className="w-5 h-5" />
                    </button>
                </Tooltip>
            </div>
        </footer>
    );
}