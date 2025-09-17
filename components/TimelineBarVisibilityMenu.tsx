import React from 'react';
import { TimelineBarVisibility, ViewMode } from '../types.ts';

interface TimelineBarVisibilityMenuProps {
    x: number;
    y: number;
    onClose: () => void;
    visibility: TimelineBarVisibility;
    onVisibilityChange: (newVisibility: TimelineBarVisibility) => void;
    viewMode: ViewMode;
}

const MenuItem: React.FC<{
    label: string;
    checked: boolean;
    onChange: () => void;
    disabled?: boolean;
}> = ({ label, checked, onChange, disabled }) => (
    <li>
        <label className={`flex w-full items-center space-x-3 p-2 rounded-md ${disabled ? 'opacity-50 cursor-not-allowed' : 'hover:bg-gray-100 dark:hover:bg-gray-700 cursor-pointer'}`}>
            <input 
                type="checkbox" 
                checked={checked}
                onChange={onChange}
                disabled={disabled}
                className="h-4 w-4 rounded bg-gray-100 dark:bg-gray-800 border-gray-300 dark:border-gray-500 text-sky-600 dark:text-sky-500 focus:ring-sky-500 cursor-pointer disabled:cursor-not-allowed"
            />
            <span className="text-sm text-gray-800 dark:text-gray-200">{label}</span>
        </label>
    </li>
);

export const TimelineBarVisibilityMenu: React.FC<TimelineBarVisibilityMenuProps> = ({
    x, y, onClose, visibility, onVisibilityChange, viewMode
}) => {
    const menuRef = React.useRef<HTMLDivElement>(null);

    React.useEffect(() => {
        const handleClickOutside = (event: MouseEvent) => {
            if (menuRef.current && !menuRef.current.contains(event.target as Node)) {
                onClose();
            }
        };
        document.addEventListener('mousedown', handleClickOutside);
        return () => document.removeEventListener('mousedown', handleClickOutside);
    }, [onClose]);

    const style: React.CSSProperties = {
        top: y,
        left: x,
        transform: 'translateX(0)',
    };
    
    React.useLayoutEffect(() => {
        if (menuRef.current) {
            const menu = menuRef.current;
            const { innerWidth, innerHeight } = window;
            const menuRect = menu.getBoundingClientRect();

            if (menuRect.right > innerWidth) {
                menu.style.left = `${x - menuRect.width}px`;
            }
            if (menuRect.bottom > innerHeight) {
                menu.style.top = `${y - menuRect.height}px`;
            }
        }
    }, [x, y]);

    const handleToggle = (key: keyof TimelineBarVisibility) => {
        onVisibilityChange({ ...visibility, [key]: !visibility[key] });
    };

    return (
        <div
            ref={menuRef}
            style={style}
            className="fixed z-50 w-56 bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg shadow-2xl p-2 animate-fadeIn"
        >
            <div className="px-3 py-2 border-b border-gray-200 dark:border-gray-700 mb-1">
                <p className="text-sm font-semibold text-gray-900 dark:text-white truncate">
                    Visible Timeline Bars
                </p>
            </div>
            <ul className="space-y-1">
                <MenuItem
                    label="Pages"
                    checked={visibility.pages}
                    onChange={() => handleToggle('pages')}
                    disabled={viewMode !== 'pagination'}
                />
                <MenuItem
                    label="Files"
                    checked={visibility.files}
                    onChange={() => handleToggle('files')}
                />
                <MenuItem
                    label="Dates"
                    checked={visibility.dates}
                    onChange={() => handleToggle('dates')}
                />
                <MenuItem
                    label="Density"
                    checked={visibility.density}
                    onChange={() => handleToggle('density')}
                />
                 <div className="h-px bg-gray-200 dark:bg-gray-700 my-1" />
                <MenuItem
                    label="Overview"
                    checked={visibility.overview}
                    onChange={() => handleToggle('overview')}
                />
            </ul>
        </div>
    );
};