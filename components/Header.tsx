import React from 'react';
import { Icon, IconName } from './icons/index.tsx';
import { IconSet } from '../types.ts';

interface HeaderProps {
    activeView: 'data' | 'viewer' | 'dashboard' | 'console' | 'settings' | 'info' | 'stock';
    onViewChange: (view: 'data' | 'viewer' | 'dashboard' | 'console' | 'settings' | 'info' | 'stock') => void;
    isBusy: boolean;
    iconSet: IconSet;
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
    indigo: { // Navy
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
    const baseClasses = "flex items-center space-x-2 px-3 py-2 text-sm font-semibold transition-all duration-200 rounded-md shadow-sm";
    
    // A neutral background for inactive tabs.
    const inactiveBg = "bg-gray-200 dark:bg-gray-700/80 hover:bg-gray-300 dark:hover:bg-gray-600";
    
    const buttonClasses = `${baseClasses} ${isActive ? `${styles.active} scale-105 shadow-lg` : `${styles.inactive} ${inactiveBg}`}`;
    const iconClasses = `w-5 h-5 ${isActive ? styles.iconActive : styles.iconInactive}`;

    return (
        <button onClick={onClick} className={buttonClasses}>
            <Icon name={iconName} iconSet={iconSet} className={iconClasses} />
            <span>{label}</span>
        </button>
    );
};

export const Header: React.FC<HeaderProps> = ({ activeView, onViewChange, isBusy, iconSet }) => {
    return (
        <header className="flex-shrink-0 bg-gray-100 dark:bg-gray-900 p-3">
            <nav className="flex items-center">
                <div className="flex items-center space-x-2">
                    <NavItem
                        iconName="ArchiveBox"
                        label="Data Hub"
                        isActive={activeView === 'data'}
                        onClick={() => onViewChange('data')}
                        color="purple"
                        iconSet={iconSet}
                    />
                    <NavItem
                        iconName="Table"
                        label="Log Viewer"
                        isActive={activeView === 'viewer'}
                        onClick={() => onViewChange('viewer')}
                        color="yellow"
                        iconSet={iconSet}
                    />
                     <NavItem
                        iconName="Cube"
                        label="Stock Tracker"
                        isActive={activeView === 'stock'}
                        onClick={() => onViewChange('stock')}
                        color="teal"
                        iconSet={iconSet}
                    />
                    <NavItem
                        iconName="ChartBar"
                        label="Dashboard"
                        isActive={activeView === 'dashboard'}
                        onClick={() => onViewChange('dashboard')}
                        color="red"
                        iconSet={iconSet}
                    />
                    <NavItem
                        iconName="Terminal"
                        label="Application Log"
                        isActive={activeView === 'console'}
                        onClick={() => onViewChange('console')}
                        color="indigo"
                        iconSet={iconSet}
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
                        iconName="BookOpen"
                        label="Info"
                        isActive={activeView === 'info'}
                        onClick={() => onViewChange('info')}
                        color="blue"
                        iconSet={iconSet}
                    />
                     <NavItem
                        iconName="Cog"
                        label="Settings"
                        isActive={activeView === 'settings'}
                        onClick={() => onViewChange('settings')}
                        color="green"
                        iconSet={iconSet}
                    />
                </div>
            </nav>
        </header>
    );
};