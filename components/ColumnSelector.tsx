import React from 'react';
import { ColumnKey, ColumnVisibilityState, IconSet } from '../types.ts';
import { COLUMN_DEFINITIONS } from '../constants';
import { Icon } from './icons/index.tsx';

interface ColumnSelectorProps {
    visibility: ColumnVisibilityState;
    onChange: (newState: ColumnVisibilityState) => void;
    iconSet: IconSet;
}

export const ColumnSelector: React.FC<ColumnSelectorProps> = ({ visibility, onChange, iconSet }) => {
    const [isOpen, setIsOpen] = React.useState(false);
    const wrapperRef = React.useRef<HTMLDivElement>(null);

    React.useEffect(() => {
        const handleClickOutside = (event: MouseEvent) => {
            if (wrapperRef.current && !wrapperRef.current.contains(event.target as Node)) {
                setIsOpen(false);
            }
        };
        document.addEventListener('mousedown', handleClickOutside);
        return () => document.removeEventListener('mousedown', handleClickOutside);
    }, []);

    const handleToggle = (key: ColumnKey) => {
        onChange({ ...visibility, [key]: !visibility[key] });
    };

    return (
        <div className="relative" ref={wrapperRef}>
            <button
                onClick={() => setIsOpen(!isOpen)}
                className="inline-flex items-center gap-2 px-3 py-1.5 text-sm font-semibold rounded-md text-white bg-blue-600 hover:bg-blue-700 transition-colors"
            >
                <Icon name="ViewColumns" iconSet={iconSet} className="w-5 h-5" />
                <span>Columns</span>
                <Icon name="ChevronDown" iconSet={iconSet} className={`w-4 h-4 transition-transform ${isOpen ? 'rotate-180' : ''}`} />
            </button>
            {isOpen && (
                <div className="absolute top-full mt-2 right-0 w-56 bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg shadow-xl z-20">
                    <div className="p-2 border-b border-gray-200 dark:border-gray-700">
                        <p className="text-sm font-semibold text-gray-800 dark:text-gray-200 px-2">Visible Columns</p>
                    </div>
                    <ul className="p-2 space-y-1">
                        {COLUMN_DEFINITIONS.map(({ key, label }) => (
                            <li key={key}>
                                <label className="flex w-full items-center space-x-3 p-2 rounded-md hover:bg-gray-100 dark:hover:bg-gray-700 cursor-pointer">
                                    <input
                                        type="checkbox"
                                        checked={visibility[key]}
                                        onChange={() => handleToggle(key)}
                                        className="h-4 w-4 rounded bg-gray-100 dark:bg-gray-800 border-gray-300 dark:border-gray-500 text-sky-600 dark:text-sky-500 focus:ring-sky-500 cursor-pointer"
                                    />
                                    <span className="text-sm text-gray-800 dark:text-gray-200">{label}</span>
                                </label>
                            </li>
                        ))}
                    </ul>
                </div>
            )}
        </div>
    );
};