import React, { useState, useEffect, useRef } from 'react';
import { Icon } from './icons';
import { useSettings } from '../contexts/SettingsContext';
import { useConsole } from '../contexts/ConsoleContext';
import { useData } from '../contexts/DataContext';

interface TitleBarProps {
    activeView: 'data' | 'viewer' | 'dashboard' | 'console' | 'settings' | 'info' | 'stock';
}

const WindowControlButton: React.FC<{ onClick: () => void; children: React.ReactNode; className?: string, title: string }> = ({ onClick, children, className = '', title }) => (
    <button
        onClick={onClick}
        title={title}
        className={`w-12 h-8 flex items-center justify-center transition-colors duration-150 hover:bg-gray-400/20 dark:hover:bg-gray-500/30 ${className}`}
        style={{ WebkitAppRegion: 'no-drag' } as React.CSSProperties}
    >
        {children}
    </button>
);


export const TitleBar: React.FC<TitleBarProps> = ({ activeView }) => {
    const [isMaximized, setIsMaximized] = useState(false);
    const { iconSet } = useSettings();
    const { consoleSearchTerm, setConsoleSearchTerm } = useConsole();
    const { formFilters, setFormFilters, handleApplyFilters, handleRemoveAppliedFilter } = useData();
    const searchInputRef = useRef<HTMLInputElement>(null);
    const debounceTimeoutRef = useRef<number | null>(null);

    const [inputValue, setInputValue] = useState('');

    const isConsole = activeView === 'console';
    const isViewer = activeView === 'viewer';
    const isSearchable = isConsole || isViewer;

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

    // This effect runs ONLY when the active view changes.
    // It populates the input with the correct value from the context for the new view.
    useEffect(() => {
        if (isConsole) {
            setInputValue(consoleSearchTerm);
        } else if (isViewer) {
            // Only use the first line of includeMsg for the search bar
            setInputValue(formFilters.includeMsg.split('\n')[0] || '');
        } else {
            setInputValue(''); // Clear for other views
        }
    }, [activeView, consoleSearchTerm, formFilters.includeMsg, isConsole, isViewer]);


    const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        const newValue = e.target.value;
        setInputValue(newValue); // Update local state immediately for responsiveness

        if (isConsole) {
            setConsoleSearchTerm(newValue);
        } else if (isViewer) {
            // Update only the first line of the include message filter to avoid
            // clobbering additional terms configured from the filter panel.
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
        if (!isViewer) {
            if (debounceTimeoutRef.current) {
                clearTimeout(debounceTimeoutRef.current);
                debounceTimeoutRef.current = null;
            }
            return;
        }

        if (debounceTimeoutRef.current) {
            clearTimeout(debounceTimeoutRef.current);
        }

        debounceTimeoutRef.current = window.setTimeout(() => {
            handleApplyFilters();
        }, 500);

        return () => {
            if (debounceTimeoutRef.current) {
                clearTimeout(debounceTimeoutRef.current);
                debounceTimeoutRef.current = null;
            }
        };
    }, [inputValue, isViewer, handleApplyFilters]);

    useEffect(() => {
        return () => {
            if (debounceTimeoutRef.current) {
                clearTimeout(debounceTimeoutRef.current);
            }
        };
    }, []);
    
    const handleClear = () => {
        setInputValue('');
        if (isConsole) {
            setConsoleSearchTerm('');
        } else if (isViewer) {
            handleRemoveAppliedFilter('includeMsg');
        }
        searchInputRef.current?.focus();
    };
    
    const getPlaceholder = () => {
        switch(activeView) {
            case 'viewer': return 'Search log messages (real-time)...';
            case 'console': return 'Search application log (real-time)...';
            default: return 'Command Palette';
        }
    };

    const handleMinimize = () => window.electronAPI?.minimizeWindow();
    const handleMaximize = () => window.electronAPI?.maximizeWindow();
    const handleClose = () => window.electronAPI?.closeWindow();

    if (!window.electronAPI) return null;
    
    return (
        <header
            className="flex-shrink-0 bg-gray-200 dark:bg-gray-800 text-gray-900 dark:text-gray-200 h-8 flex items-center justify-between"
            style={{ WebkitAppRegion: 'drag' } as React.CSSProperties}
        >
            <div className="flex items-center h-full">
                <div className="flex items-center gap-2 px-3">
                    <Icon name="BugAnt" iconSet={iconSet} className="w-5 h-5 text-sky-600 dark:text-sky-400" />
                    <span className="text-sm font-semibold">Private Log Analyser</span>
                </div>
            </div>

            <div className="flex-grow flex items-center justify-center px-4">
                <div className="relative w-full max-w-md">
                    <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                         <Icon name={isSearchable ? "Filter" : "Terminal"} iconSet={iconSet} className="w-4 h-4 text-gray-400" />
                    </div>
                    <input
                        ref={searchInputRef}
                        type="text"
                        placeholder={getPlaceholder()}
                        value={inputValue}
                        onChange={handleInputChange}
                        className="w-full h-6 pl-10 pr-8 py-1 text-xs bg-gray-100 dark:bg-gray-700/80 border border-gray-300 dark:border-gray-600/50 rounded-md focus:ring-1 focus:ring-sky-500 focus:border-sky-500 transition-all placeholder-gray-400"
                        style={{ WebkitAppRegion: 'no-drag' } as React.CSSProperties}
                    />
                     {inputValue && (
                        <button
                            onClick={handleClear}
                            className="absolute inset-y-0 right-0 pr-2 flex items-center text-gray-400 hover:text-gray-600 dark:hover:text-gray-300"
                            aria-label="Clear search"
                            style={{ WebkitAppRegion: 'no-drag' } as React.CSSProperties}
                        >
                            <Icon name="XCircle" iconSet={iconSet} className="w-4 h-4" />
                        </button>
                    )}
                </div>
            </div>

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
        </header>
    );
};