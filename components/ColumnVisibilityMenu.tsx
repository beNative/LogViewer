import React from 'react';
import { ColumnVisibilityState, ColumnKey } from '../types.ts';
import { COLUMN_DEFINITIONS } from '../constants';

// Props interface
interface ColumnVisibilityMenuProps {
    x: number;
    y: number;
    visibility: ColumnVisibilityState;
    onChange: (newState: ColumnVisibilityState) => void;
    onClose: () => void;
}

// MenuItem sub-component
const MenuItem: React.FC<{
    label: string;
    columnKey: ColumnKey;
    checked: boolean;
    onChange: (key: ColumnKey) => void;
}> = ({ label, columnKey, checked, onChange }) => (
    <li>
        <label className="flex w-full items-center space-x-3 p-2 rounded-md hover:bg-gray-100 dark:hover:bg-gray-700 cursor-pointer">
            <input
                type="checkbox"
                checked={checked}
                onChange={() => onChange(columnKey)}
                className="h-4 w-4 rounded bg-gray-100 dark:bg-gray-800 border-gray-300 dark:border-gray-500 text-sky-600 dark:text-sky-500 focus:ring-sky-500 cursor-pointer"
            />
            <span className="text-sm text-gray-800 dark:text-gray-200">{label}</span>
        </label>
    </li>
);

// Main component
export const ColumnVisibilityMenu: React.FC<ColumnVisibilityMenuProps> = ({ x, y, visibility, onChange, onClose }) => {
    const menuRef = React.useRef<HTMLDivElement>(null);

    // Effect for closing on outside click
    React.useEffect(() => {
        const handleClickOutside = (event: MouseEvent) => {
            if (menuRef.current && !menuRef.current.contains(event.target as Node)) {
                onClose();
            }
        };
        document.addEventListener('mousedown', handleClickOutside);
        return () => document.removeEventListener('mousedown', handleClickOutside);
    }, [onClose]);

    // Effect for positioning
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

    const handleToggle = (key: ColumnKey) => {
        onChange({ ...visibility, [key]: !visibility[key] });
    };

    return (
        <div
            ref={menuRef}
            style={{ top: y, left: x }}
            className="fixed z-50 w-56 bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg shadow-2xl p-2 animate-fadeIn"
        >
            <div className="px-3 py-2 border-b border-gray-200 dark:border-gray-700 mb-1">
                <p className="text-sm font-semibold text-gray-900 dark:text-white truncate">
                    Visible Columns
                </p>
            </div>
            <ul className="space-y-1">
                {COLUMN_DEFINITIONS.map(({ key, label }) => (
                    <MenuItem
                        key={key}
                        label={label}
                        columnKey={key}
                        checked={visibility[key]}
                        onChange={handleToggle}
                    />
                ))}
            </ul>
        </div>
    );
};