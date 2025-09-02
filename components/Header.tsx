import React from 'react';
import { Icon } from './icons/index.tsx';
import { IconSet } from '../types.ts';

interface HeaderProps {
    activeView: 'data' | 'viewer' | 'dashboard' | 'console' | 'settings' | 'info';
    onViewChange: (view: 'data' | 'viewer' | 'dashboard' | 'console' | 'settings' | 'info') => void;
    isBusy: boolean;
    iconSet: IconSet;
}

const NavItem: React.FC<{
    icon: React.ReactNode;
    label: string;
    isActive: boolean;
    onClick: () => void;
}> = ({ icon, label, isActive, onClick }) => {
    const baseClasses = "flex items-center space-x-3 px-4 py-3 font-medium transition-colors duration-200 rounded-t-lg border-b-2";
    const activeClasses = "border-sky-500 text-sky-600 bg-white dark:border-sky-400 dark:text-sky-400 dark:bg-gray-800/80";
    const inactiveClasses = "border-transparent text-gray-500 hover:bg-gray-200 hover:text-gray-700 dark:text-gray-400 dark:hover:bg-gray-700/50 dark:hover:text-white";

    return (
        <button onClick={onClick} className={`${baseClasses} ${isActive ? activeClasses : inactiveClasses}`}>
            {icon}
            <span>{label}</span>
        </button>
    )
}

export const Header: React.FC<HeaderProps> = ({ activeView, onViewChange, isBusy, iconSet }) => {
    return (
        <header className="flex-shrink-0 bg-gray-100 dark:bg-gray-900 px-4 pt-3">
            <nav className="flex items-end border-b border-gray-200 dark:border-gray-700">
                <div className="flex items-end space-x-2">
                    <NavItem
                        icon={<Icon name="ArchiveBox" iconSet={iconSet} className="w-5 h-5" />}
                        label="Data Hub"
                        isActive={activeView === 'data'}
                        onClick={() => onViewChange('data')}
                    />
                    <NavItem
                        icon={<Icon name="Table" iconSet={iconSet} className="w-5 h-5" />}
                        label="Log Viewer"
                        isActive={activeView === 'viewer'}
                        onClick={() => onViewChange('viewer')}
                    />
                    <NavItem
                        icon={<Icon name="ChartBar" iconSet={iconSet} className="w-5 h-5" />}
                        label="Dashboard"
                        isActive={activeView === 'dashboard'}
                        onClick={() => onViewChange('dashboard')}
                    />
                    <NavItem
                        icon={<Icon name="Terminal" iconSet={iconSet} className="w-5 h-5" />}
                        label="Application Log"
                        isActive={activeView === 'console'}
                        onClick={() => onViewChange('console')}
                    />
                </div>
                <div className="flex-grow" />
                
                {isBusy && (
                    <div className="flex items-center gap-2 px-4 py-3 text-sm font-semibold text-sky-600 dark:text-sky-400 animate-pulse">
                        <Icon name="ArrowPath" iconSet={iconSet} className="w-5 h-5 animate-spin" />
                        <span>Processing...</span>
                    </div>
                )}
                
                <div className="flex items-end space-x-2">
                     <NavItem
                        icon={<Icon name="BookOpen" iconSet={iconSet} className="w-5 h-5" />}
                        label="Info"
                        isActive={activeView === 'info'}
                        onClick={() => onViewChange('info')}
                    />
                     <NavItem
                        icon={<Icon name="Cog" iconSet={iconSet} className="w-5 h-5" />}
                        label="Settings"
                        isActive={activeView === 'settings'}
                        onClick={() => onViewChange('settings')}
                    />
                </div>
            </nav>
        </header>
    );
};