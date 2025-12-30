import React from 'react';
import { LogEntry, ColumnKey, IconSet } from '../types.ts';
import { Icon, IconName } from './icons/index.tsx';

interface ContextMenuProps {
    x: number;
    y: number;
    entry: LogEntry;
    contextKey: ColumnKey;
    contextValue: string;
    onClose: () => void;
    onFilter: (key: 'level' | 'sndrtype' | 'sndrname' | 'fileName' | 'msg', value: string, exclude: boolean) => void;
    iconSet: IconSet;
    selectedEntries?: LogEntry[];
    onSetTimeStart: () => void;
    onSetTimeEnd: () => void;
    onSetTimeRange: () => void;
}

const MenuItem: React.FC<{
    label: string;
    iconName: IconName;
    iconSet: IconSet;
    onClick: () => void;
    disabled?: boolean;
}> = ({ label, iconName, iconSet, onClick, disabled }) => (
    <button
        onClick={onClick}
        disabled={disabled}
        className="w-full text-left flex items-center gap-3 px-3 py-2 text-sm rounded-md text-gray-700 dark:text-gray-200 hover:bg-gray-100 dark:hover:bg-gray-700 disabled:opacity-50 disabled:cursor-not-allowed group"
    >
        <div className="flex-none">
            <Icon name={iconName} iconSet={iconSet} className="w-5 h-5 text-gray-500 dark:text-gray-400" />
        </div>
        <span className="truncate min-w-0 flex-1">{label}</span>
    </button>
);

export const ContextMenu: React.FC<ContextMenuProps> = ({
    x, y, entry, contextKey, contextValue, onClose, onFilter, iconSet, selectedEntries = [], onSetTimeStart, onSetTimeEnd, onSetTimeRange
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

    // Update check to include 'msg'
    const canFilter = ['level', 'sndrtype', 'sndrname', 'fileName', 'msg'].includes(contextKey);

    const handleCopy = (content: string) => {
        navigator.clipboard.writeText(content);
        onClose();
    };

    const handleFilter = (exclude: boolean) => {
        if (canFilter) {
            onFilter(contextKey as 'level' | 'sndrtype' | 'sndrname' | 'fileName' | 'msg', contextValue, exclude);
        }
        onClose();
    };

    const selectedCount = selectedEntries && selectedEntries.length > 1 ? selectedEntries.length : 0;

    const handleCopyMessages = () => {
        if (selectedCount > 0) {
            const textToCopy = selectedEntries.map(e => e.msg).join('\n');
            navigator.clipboard.writeText(textToCopy);
        } else {
            navigator.clipboard.writeText(entry.msg);
        }
        onClose();
    };

    const style: React.CSSProperties = {
        top: y,
        left: x,
        transform: 'translateX(0)',
    };

    // Adjust position if it overflows the viewport
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

    return (
        <div
            ref={menuRef}
            style={style}
            className="fixed z-50 w-64 bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg shadow-2xl p-2"
        >
            <div className="px-3 py-2 border-b border-gray-200 dark:border-gray-700 mb-1">
                <p className="text-xs text-gray-500 dark:text-gray-400 uppercase tracking-wider">{contextKey}</p>
                <p className="text-sm font-semibold text-gray-900 dark:text-white truncate" title={contextValue}>
                    {contextValue}
                </p>
            </div>
            <ul className="space-y-1">
                <li>
                    <MenuItem
                        label={`Filter by "${contextValue}"`}
                        iconName="Filter"
                        iconSet={iconSet}
                        onClick={() => handleFilter(false)}
                        disabled={!canFilter}
                    />
                </li>
                <li>
                    <MenuItem
                        label={`Exclude "${contextValue}"`}
                        iconName="XCircle"
                        iconSet={iconSet}
                        onClick={() => handleFilter(true)}
                        disabled={!canFilter}
                    />
                </li>
                <div className="h-px bg-gray-200 dark:bg-gray-700 my-1" />
                <li>
                    <MenuItem
                        label="Set Time Range Start"
                        iconName="Clock"
                        iconSet={iconSet}
                        onClick={() => { onSetTimeStart(); onClose(); }}
                    />
                </li>
                <li>
                    <MenuItem
                        label="Set Time Range End"
                        iconName="Clock"
                        iconSet={iconSet}
                        onClick={() => { onSetTimeEnd(); onClose(); }}
                    />
                </li>
                {selectedCount > 1 && (
                    <>
                        <div className="h-px bg-gray-200 dark:bg-gray-700 my-1" />
                        <li>
                            <MenuItem
                                label="Set Time Range to Selection"
                                iconName="Clock"
                                iconSet={iconSet}
                                onClick={() => { onSetTimeRange(); onClose(); }}
                            />
                        </li>
                    </>
                )}
                <div className="h-px bg-gray-200 dark:bg-gray-700 my-1" />
                <li>
                    <MenuItem
                        label={selectedCount > 1 ? `Copy ${selectedCount} Messages` : "Copy Message"}
                        iconName="ClipboardDocument"
                        iconSet={iconSet}
                        onClick={handleCopyMessages}
                    />
                </li>
                <li>
                    <MenuItem
                        label={`Copy ${contextKey}`}
                        iconName="ClipboardDocument"
                        iconSet={iconSet}
                        onClick={() => handleCopy(contextValue)}
                    />
                </li>
                <li>
                    <MenuItem
                        label="Copy Row as JSON"
                        iconName="ClipboardDocument"
                        iconSet={iconSet}
                        onClick={() => handleCopy(JSON.stringify(entry, null, 2))}
                    />
                </li>
            </ul>
        </div>
    );
};