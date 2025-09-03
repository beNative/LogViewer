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
    const baseClasses = "flex items-center space-x-2 px-3 py-2 text-sm font-semibold transition-all duration-200 rounded-md shadow-sm";
    const activeClasses = "bg-white dark:bg-gray-700 text-sky-600 dark:text-sky-400 scale-105 shadow-lg";
    const inactiveClasses = "bg-sky-600 text-white hover:bg-sky-700 dark:bg-sky-700 dark:hover:bg-sky-600";

    return (
        <button onClick={onClick} className={`${baseClasses} ${isActive ? activeClasses : inactiveClasses}`}>
            {icon}
            <span>{label}</span>
        </button>
    )
}

export const Header: React.FC<HeaderProps> = ({ activeView, onViewChange, isBusy, iconSet }) => {
    return (
        <header className="flex-shrink-0 bg-gray-100 dark:bg-gray-900 p-3">
            <nav className="flex items-center">
                <div className="flex items-center space-x-2">
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
                
                <div className="flex items-center space-x-2">
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