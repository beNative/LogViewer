import React, { useState, useEffect } from 'react';
import { Icon } from './icons';
import { useSettings } from '../contexts/SettingsContext';

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


export const TitleBar: React.FC = () => {
    const [isMaximized, setIsMaximized] = useState(false);
    const { iconSet } = useSettings();

    useEffect(() => {
        if (!window.electronAPI) return;

        let isMounted = true;

        window.electronAPI.isWindowMaximized().then(status => {
            if (isMounted) {
                setIsMaximized(status);
            }
        });
        
        const removeListener = window.electronAPI.onWindowMaximizedStatus(status => {
            if (isMounted) {
                setIsMaximized(status);
            }
        });
        
        return () => {
            isMounted = false;
            removeListener();
        };

    }, []);

    const handleMinimize = () => window.electronAPI?.minimizeWindow();
    const handleMaximize = () => window.electronAPI?.maximizeWindow();
    const handleClose = () => window.electronAPI?.closeWindow();

    // The component should only render in an Electron environment
    if (!window.electronAPI) {
        return null;
    }
    
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

            <div className="flex-grow flex items-center justify-center px-4" style={{ WebkitAppRegion: 'no-drag' } as React.CSSProperties}>
                <div className="relative w-full max-w-md">
                    <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                        <Icon name="Filter" iconSet={iconSet} className="w-4 h-4 text-gray-400" />
                    </div>
                    <input 
                        type="text"
                        placeholder="Command Palette"
                        className="w-full h-6 px-10 py-1 text-xs bg-gray-100 dark:bg-gray-700/80 border border-gray-300 dark:border-gray-600/50 rounded-md focus:ring-1 focus:ring-sky-500 focus:border-sky-500 transition-all placeholder-gray-400"
                    />
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
