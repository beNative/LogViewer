import React from 'react';
import { LogEntry, ColumnVisibilityState, ColumnStyles, ColumnKey, Theme, IconSet, LogTableDensity } from '../types.ts';
import { COLUMN_DEFINITIONS, highlightText } from '../utils.ts';
import { ContextMenu } from './ContextMenu.tsx';
import { Icon } from './icons/index.tsx';

export const getLevelColor = (level: string) => {
    switch (level?.toUpperCase()) {
        case 'ERROR': return 'bg-red-100 text-red-800 dark:bg-red-900/50 dark:text-red-300';
        case 'WARNING': return 'bg-orange-100 text-orange-800 dark:bg-orange-900/50 dark:text-orange-300';
        case 'INFO': return 'bg-blue-100 text-blue-800 dark:bg-blue-900/50 dark:text-blue-300';
        case 'DEBUG': return 'bg-green-100 text-green-800 dark:bg-green-900/50 dark:text-green-300';
        default: return 'bg-gray-100 text-gray-800 dark:bg-gray-800 dark:text-gray-300';
    }
};

interface LogTableProps {
    logEntries: LogEntry[];
    columnVisibility: ColumnVisibilityState;
    columnStyles: ColumnStyles;
    highlightTerms: string[];
    theme: Theme;
    onRowClick: (entry: LogEntry) => void;
    selectedEntryId: number | null;
    density: LogTableDensity;
    onFilter: (key: 'level' | 'sndrtype' | 'sndrname' | 'fileName', value: string, exclude: boolean) => void;
    iconSet: IconSet;
}

interface ContextMenuState {
    x: number;
    y: number;
    entry: LogEntry;
    contextKey: ColumnKey;
    contextValue: string;
}

export const LogTable: React.FC<LogTableProps> = ({ logEntries, columnVisibility, columnStyles, highlightTerms, theme, onRowClick, selectedEntryId, density, onFilter, iconSet }) => {
    const [contextMenu, setContextMenu] = React.useState<ContextMenuState | null>(null);
    const tableContainerRef = React.useRef<HTMLDivElement>(null);

    React.useEffect(() => {
        if (selectedEntryId) {
            const row = document.getElementById(`log-row-${selectedEntryId}`);
            row?.scrollIntoView({ behavior: 'smooth', block: 'center' });
        }
    }, [selectedEntryId]);

    const handleContextMenu = (e: React.MouseEvent, entry: LogEntry, key: ColumnKey) => {
        e.preventDefault();
        const value = String(entry[key]);
        setContextMenu({ x: e.clientX, y: e.clientY, entry, contextKey: key, contextValue: value });
    };

    const getRowClass = (density: LogTableDensity) => {
        switch (density) {
            case 'compact': return 'py-0.5 px-2';
            case 'normal': return 'py-1.5 px-2';
            case 'comfortable': return 'py-2.5 px-2';
            default: return 'py-1.5 px-2';
        }
    }

    const getStyle = (key: ColumnKey): React.CSSProperties => {
        const styleConf = columnStyles[key];
        const properties: React.CSSProperties = {
            fontFamily: styleConf.font || 'inherit',
            fontSize: `${styleConf.fontSize}px`,
            fontWeight: styleConf.isBold ? 'bold' : 'normal',
            fontStyle: styleConf.isItalic ? 'italic' : 'normal',
        };

        const color = theme === 'dark' ? styleConf.darkColor : styleConf.color;
        if (color) {
            properties.color = color;
        }

        return properties;
    };

    if (!logEntries || logEntries.length === 0) {
        return (
            <div className="flex-grow flex items-center justify-center text-center text-gray-500 dark:text-gray-400">
                <div>
                    <Icon name="Table" iconSet={iconSet} className="w-24 h-24 text-gray-300 dark:text-gray-700 mx-auto mb-6" />
                    <h2 className="text-2xl font-semibold text-gray-700 dark:text-gray-300">No Log Entries Found</h2>
                    <p className="text-gray-500 dark:text-gray-500 mt-2">Try adjusting your filters or loading a data file.</p>
                </div>
            </div>
        );
    }
    
    return (
        <div ref={tableContainerRef} className="flex-grow min-h-0 overflow-auto bg-white dark:bg-gray-900">
            <table className="min-w-full divide-y divide-gray-200 dark:divide-gray-800 border-separate" style={{ borderSpacing: 0 }}>
                <thead className="bg-gray-50 dark:bg-gray-800/80 sticky top-0 z-10">
                    <tr>
                        {COLUMN_DEFINITIONS.filter(c => columnVisibility[c.key]).map(col => (
                            <th key={col.key} scope="col" className="px-2 py-2 text-left text-xs font-semibold text-gray-500 dark:text-gray-300 uppercase tracking-wider border-b border-gray-200 dark:border-gray-700">
                                {col.label}
                            </th>
                        ))}
                    </tr>
                </thead>
                <tbody className="divide-y divide-gray-200/80 dark:divide-gray-800/50">
                    {logEntries.map(entry => (
                        <tr
                            key={entry.id}
                            id={`log-row-${entry.id}`}
                            onClick={() => onRowClick(entry)}
                            className={`cursor-pointer transition-colors ${selectedEntryId === entry.id ? 'bg-sky-100 dark:bg-sky-900/50' : 'hover:bg-gray-100/70 dark:hover:bg-gray-800/70'}`}
                        >
                            {COLUMN_DEFINITIONS.filter(c => columnVisibility[c.key]).map(col => (
                                <td
                                    key={col.key}
                                    style={getStyle(col.key)}
                                    className={`${getRowClass(density)} text-sm whitespace-nowrap align-top`}
                                    onContextMenu={(e) => handleContextMenu(e, entry, col.key)}
                                >
                                    {col.key === 'level' ? (
                                        <span className={`px-2 py-0.5 rounded-full text-xs font-medium ${getLevelColor(entry.level)}`}>
                                            {entry.level}
                                        </span>
                                    ) : col.key === 'msg' ? (
                                        <div className="whitespace-pre-wrap break-all" style={{ ...getStyle('msg'), whiteSpace: 'nowrap', textOverflow: 'ellipsis', overflow: 'hidden' }}>
                                            <span dangerouslySetInnerHTML={{ __html: highlightText(entry.msg, highlightTerms, theme) }} />
                                        </div>
                                    ) : (
                                        String(entry[col.key])
                                    )}
                                </td>
                            ))}
                        </tr>
                    ))}
                </tbody>
            </table>
            {contextMenu && (
                <ContextMenu
                    {...contextMenu}
                    onClose={() => setContextMenu(null)}
                    onFilter={onFilter}
                    iconSet={iconSet}
                />
            )}
        </div>
    );
};
