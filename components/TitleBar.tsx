import React, { useState, useEffect, useRef } from 'react';
import { Icon, IconName } from './icons';
import { IconSet } from '../types';
import { useSettings } from '../contexts/SettingsContext';
import { useData } from '../contexts/DataContext';
import appIcon from '../build/icon.svg';
import { Tooltip } from './Tooltip';

interface TitleBarProps {
    activeView: 'data' | 'viewer' | 'dashboard' | 'settings' | 'info' | 'stock';
    onViewChange: (view: 'data' | 'viewer' | 'dashboard' | 'settings' | 'info' | 'stock') => void;
    isBusy: boolean;
    onToggleLogPanel: () => void;
    isLogPanelVisible: boolean;
}

type NavColor = 'purple' | 'yellow' | 'red' | 'indigo' | 'blue' | 'green' | 'teal';

const colorStyles: Record<NavColor, {
    active: string;
    inactive: string;
    iconActive: string;
    iconInactive: string;
}> = {
    purple: {
        active: 'bg-purple-600 hover:bg-purple-700 text-white',
        inactive: 'text-purple-600 dark:text-purple-400',
        iconActive: 'text-white',
        iconInactive: 'text-purple-500 dark:text-purple-400'
    },
    yellow: {
        active: 'bg-yellow-500 hover:bg-yellow-600 text-yellow-900',
        inactive: 'text-yellow-600 dark:text-yellow-400',
        iconActive: 'text-yellow-900',
        iconInactive: 'text-yellow-500 dark:text-yellow-400'
    },
    red: {
        active: 'bg-red-600 hover:bg-red-700 text-white',
        inactive: 'text-red-600 dark:text-red-400',
        iconActive: 'text-white',
        iconInactive: 'text-red-500 dark:text-red-400'
    },
    indigo: {
        active: 'bg-indigo-700 hover:bg-indigo-800 text-white',
        inactive: 'text-indigo-700 dark:text-indigo-400',
        iconActive: 'text-white',
        iconInactive: 'text-indigo-600 dark:text-indigo-400'
    },
    blue: {
        active: 'bg-blue-600 hover:bg-blue-700 text-white',
        inactive: 'text-blue-600 dark:text-blue-400',
        iconActive: 'text-white',
        iconInactive: 'text-blue-500 dark:text-blue-400'
    },
    green: {
        active: 'bg-green-600 hover:bg-green-700 text-white',
        inactive: 'text-green-600 dark:text-green-400',
        iconActive: 'text-white',
        iconInactive: 'text-green-500 dark:text-green-400'
    },
    teal: {
        active: 'bg-teal-600 hover:bg-teal-700 text-white',
        inactive: 'text-teal-600 dark:text-teal-400',
        iconActive: 'text-white',
        iconInactive: 'text-teal-500 dark:text-teal-400'
    }
};

const NavItem: React.FC<{
    iconName: IconName;
    label: string;
    isActive: boolean;
    onClick: () => void;
    color: NavColor;
    iconSet: IconSet;
}> = ({ iconName, label, isActive, onClick, color, iconSet }) => {
    const styles = colorStyles[color];
    const baseClasses = "flex items-center gap-1.5 px-2 py-1 text-xs font-semibold transition-all duration-200 rounded-md";
    const inactiveBg = "bg-gray-300/50 dark:bg-gray-700/50 hover:bg-gray-300 dark:hover:bg-gray-600";
    const buttonClasses = `${baseClasses} ${isActive ? `${styles.active} shadow-sm` : `${styles.inactive} ${inactiveBg}`}`;
    const iconClasses = `w-4 h-4 ${isActive ? styles.iconActive : styles.iconInactive}`;

    return (
        <Tooltip content={label}>
            <button
                onClick={onClick}
                className={buttonClasses}
                style={{ WebkitAppRegion: 'no-drag' } as React.CSSProperties}
            >
                <Icon name={iconName} iconSet={iconSet} className={iconClasses} />
                <span className="hidden lg:inline">{label}</span>
            </button>
        </Tooltip>
    );
};

const WindowControlButton: React.FC<{ onClick: () => void; children: React.ReactNode; className?: string, title: string }> = ({ onClick, children, className = '', title }) => (
    <button
        onClick={onClick}
        title={title}
        className={`w-12 h-9 flex items-center justify-center transition-colors duration-150 hover:bg-gray-400/20 dark:hover:bg-gray-500/30 ${className}`}
        style={{ WebkitAppRegion: 'no-drag' } as React.CSSProperties}
    >
        {children}
    </button>
);

export const TitleBar: React.FC<TitleBarProps> = ({ activeView, onViewChange, isBusy, onToggleLogPanel, isLogPanelVisible }) => {
    const [isMaximized, setIsMaximized] = useState(false);
    const { iconSet } = useSettings();
    const { formFilters, setFormFilters, handleApplyFilters, handleRemoveAppliedFilter } = useData();
    const searchInputRef = useRef<HTMLInputElement>(null);
    const debounceTimeoutRef = useRef<number | null>(null);

    const [inputValue, setInputValue] = useState('');

    const isViewer = activeView === 'viewer';
    const isSearchable = isViewer;

    useEffect(() => {
        if (!window.electronAPI) return;

        let isMounted = true;

        window.electronAPI.isWindowMaximized().then(status => {
            if (isMounted) setIsMaximized(status);
        });

        const removeListener = window.electronAPI.onWindowMaximizedStatus(status => {
            if (isMounted) setIsMaximized(status);
        });

        return () => { isMounted = false; removeListener(); };
    }, []);

    useEffect(() => {
        if (isViewer) {
            setInputValue(formFilters.includeMsg.split('\n')[0] || '');
        } else {
            setInputValue('');
        }
    }, [activeView, formFilters.includeMsg, isViewer]);

    const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        const newValue = e.target.value;
        setInputValue(newValue);

        if (isViewer) {
            setFormFilters(prev => {
                const [, ...remainingLines] = prev.includeMsg.split('\n');
                const rebuiltIncludeMsg = remainingLines.length > 0
                    ? [newValue, ...remainingLines].join('\n')
                    : newValue;
                return { ...prev, includeMsg: rebuiltIncludeMsg };
            });
        }
    };

    useEffect(() => {
        return () => {
            if (debounceTimeoutRef.current) {
                clearTimeout(debounceTimeoutRef.current);
            }
        };
    }, []);

    const handleClear = () => {
        setInputValue('');
        if (isViewer) {
            handleRemoveAppliedFilter('includeMsg');
        }
        searchInputRef.current?.focus();
    };

    const getPlaceholder = () => {
        switch (activeView) {
            case 'viewer': return 'Search log messages (real-time)...';
            default: return 'Command Palette';
        }
    };

    const handleMinimize = () => window.electronAPI?.minimizeWindow();
    const handleMaximize = () => window.electronAPI?.maximizeWindow();
    const handleClose = () => window.electronAPI?.closeWindow();

    if (!window.electronAPI) return null;

    return (
        <header
            className="flex-shrink-0 bg-gray-200 dark:bg-gray-800 text-gray-900 dark:text-gray-200 h-9 flex items-center"
            style={{ WebkitAppRegion: 'drag' } as React.CSSProperties}
        >
            {/* Left section: App icon + main nav */}
            <div className="flex items-center h-full">
                <div className="flex items-center gap-2 px-3">
                    <img src={appIcon} alt="App Icon" className="w-5 h-5" />
                    <span className="text-sm font-semibold hidden xl:inline">Log Analyzer</span>
                </div>
                <nav className="flex items-center gap-1 px-1" style={{ WebkitAppRegion: 'no-drag' } as React.CSSProperties}>
                    <NavItem iconName="ArchiveBox" label="Data Hub" isActive={activeView === 'data'} onClick={() => onViewChange('data')} color="purple" iconSet={iconSet} />
                    <NavItem iconName="Table" label="Log Viewer" isActive={activeView === 'viewer'} onClick={() => onViewChange('viewer')} color="yellow" iconSet={iconSet} />
                    <NavItem iconName="Cube" label="Stock Tracker" isActive={activeView === 'stock'} onClick={() => onViewChange('stock')} color="teal" iconSet={iconSet} />
                    <NavItem iconName="ChartBar" label="Dashboard" isActive={activeView === 'dashboard'} onClick={() => onViewChange('dashboard')} color="red" iconSet={iconSet} />
                </nav>
            </div>

            {/* Center section: Search bar */}
            <div className="flex-grow flex items-center justify-center px-2">
                <div className="relative w-full max-w-md">
                    <div className="absolute inset-y-0 left-0 pl-2.5 flex items-center pointer-events-none">
                        <Icon name={isSearchable ? "Filter" : "Terminal"} iconSet={iconSet} className="w-3.5 h-3.5 text-gray-400" />
                    </div>
                    <input
                        ref={searchInputRef}
                        type="text"
                        placeholder={getPlaceholder()}
                        value={inputValue}
                        onChange={handleInputChange}
                        className="w-full h-6 pl-8 pr-7 py-1 text-xs bg-gray-100 dark:bg-gray-700/80 border border-gray-300 dark:border-gray-600/50 rounded-md focus:ring-1 focus:ring-sky-500 focus:border-sky-500 transition-all placeholder-gray-400"
                        style={{ WebkitAppRegion: 'no-drag' } as React.CSSProperties}
                        onKeyDown={(e) => {
                            if (e.key === 'Enter') {
                                handleApplyFilters();
                            }
                        }}
                    />
                    {inputValue && (
                        <button
                            onClick={handleClear}
                            className="absolute inset-y-0 right-0 pr-2 flex items-center text-gray-400 hover:text-gray-600 dark:hover:text-gray-300"
                            aria-label="Clear search"
                            style={{ WebkitAppRegion: 'no-drag' } as React.CSSProperties}
                        >
                            <Icon name="XCircle" iconSet={iconSet} className="w-3.5 h-3.5" />
                        </button>
                    )}
                </div>
            </div>

            {/* Right section: Secondary nav + busy indicator + window controls */}
            <div className="flex items-center h-full">
                {isBusy && (
                    <div className="flex items-center gap-1.5 px-2 text-xs font-semibold text-sky-600 dark:text-sky-400 animate-pulse">
                        <Icon name="ArrowPath" iconSet={iconSet} className="w-4 h-4 animate-spin" />
                        <span className="hidden md:inline">Processing...</span>
                    </div>
                )}
                <nav className="flex items-center gap-1 px-1" style={{ WebkitAppRegion: 'no-drag' } as React.CSSProperties}>
                    <NavItem iconName="Terminal" label="Application Log" isActive={isLogPanelVisible} onClick={onToggleLogPanel} color="indigo" iconSet={iconSet} />
                    <NavItem iconName="BookOpen" label="Info" isActive={activeView === 'info'} onClick={() => onViewChange('info')} color="blue" iconSet={iconSet} />
                    <NavItem iconName="Cog" label="Settings" isActive={activeView === 'settings'} onClick={() => onViewChange('settings')} color="green" iconSet={iconSet} />
                </nav>

                {/* Window controls */}
                <div className="flex items-center h-full">
                    <WindowControlButton onClick={handleMinimize} title="Minimize">
                        <Icon name="Minimize" iconSet="sharp" className="w-4 h-4" />
                    </WindowControlButton>
                    <WindowControlButton onClick={handleMaximize} title={isMaximized ? "Restore" : "Maximize"}>
                        <Icon name={isMaximized ? "Restore" : "Maximize"} iconSet="sharp" className="w-4 h-4" />
                    </WindowControlButton>
                    <WindowControlButton onClick={handleClose} className="hover:bg-red-500 dark:hover:bg-red-500 hover:text-white" title="Close">
                        <Icon name="XMark" iconSet="sharp" className="w-4 h-4" />
                    </WindowControlButton>
                </div>
            </div>
        </header>
    );
};