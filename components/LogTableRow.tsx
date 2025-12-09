import React from 'react';
import { LogEntry, ColumnDefinition, LogTableDensity, Theme, FilterState, ColumnStyles, ColumnKey } from '../types';
import { highlightText } from '../utils';
import { getLevelColor, getRowClass, getCellClass } from './tableUtils';

interface LogTableRowProps {
    entry: LogEntry;
    columns: ColumnDefinition[];
    logTableDensity: LogTableDensity;
    theme: Theme;
    isSelected: boolean;
    style: React.CSSProperties;
    onRowClick: (entry: LogEntry) => void;
    onContextMenu: (e: React.MouseEvent, entry: LogEntry, key: ColumnKey, value: string) => void;
    appliedFilters: FilterState;
    columnStyles: ColumnStyles;
}

export const LogTableRow: React.FC<LogTableRowProps> = React.memo(({
    entry,
    columns,
    logTableDensity,
    theme,
    isSelected,
    style,
    onRowClick,
    onContextMenu,
    appliedFilters,
    columnStyles
}) => {
    const getStyle = (key: ColumnKey): React.CSSProperties => {
        const styleConf = columnStyles[key];
        if (!styleConf) return {};
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

    const renderCell = (column: ColumnDefinition) => {
        const baseClasses = `${getRowClass(logTableDensity)} ${getCellClass(logTableDensity)}`;

        switch (column.key) {
            case 'time':
                return (
                    <div
                        key="time"
                        onContextMenu={(e) => onContextMenu(e, entry, 'time', entry.time)}
                        style={getStyle('time')}
                        className={`${baseClasses} whitespace-nowrap`}
                    >
                        {entry.time}
                    </div>
                );
            case 'level':
                return (
                    <div
                        key="level"
                        onContextMenu={(e) => onContextMenu(e, entry, 'level', entry.level)}
                        className={`${baseClasses} whitespace-nowrap`}
                    >
                        <span
                            style={getStyle('level')}
                            className={`px-2 py-0.5 rounded-full text-xs font-medium ${getLevelColor(entry.level)}`}
                        >
                            {entry.level}
                        </span>
                    </div>
                );
            case 'sndrtype':
                return (
                    <div
                        key="sndrtype"
                        onContextMenu={(e) => onContextMenu(e, entry, 'sndrtype', entry.sndrtype)}
                        style={getStyle('sndrtype')}
                        className={`${baseClasses} whitespace-nowrap truncate`}
                    >
                        {entry.sndrtype}
                    </div>
                );
            case 'sndrname':
                return (
                    <div
                        key="sndrname"
                        onContextMenu={(e) => onContextMenu(e, entry, 'sndrname', entry.sndrname)}
                        style={getStyle('sndrname')}
                        className={`${baseClasses} whitespace-nowrap truncate`}
                    >
                        {entry.sndrname}
                    </div>
                );
            case 'fileName':
                return (
                    <div
                        key="fileName"
                        onContextMenu={(e) => onContextMenu(e, entry, 'fileName', entry.fileName)}
                        style={getStyle('fileName')}
                        className={`${baseClasses} whitespace-nowrap truncate`}
                    >
                        {entry.fileName}
                    </div>
                );
            case 'msg':
            default:
                return (
                    <div
                        key="msg"
                        onContextMenu={(e) => onContextMenu(e, entry, 'msg', entry.msg)}
                        style={getStyle('msg')}
                        className={`${baseClasses} whitespace-nowrap truncate`}
                        dangerouslySetInnerHTML={{ __html: highlightText(entry.msg, [appliedFilters.includeMsg], theme) }}
                    />
                );
        }
    };

    return (
        <div
            onClick={() => onRowClick(entry)}
            className={`grid font-sans items-center transition-colors duration-100 cursor-pointer border-b border-gray-200 dark:border-gray-700 ${isSelected
                    ? 'bg-sky-100 dark:bg-sky-900/60 border-l-4 border-l-sky-500'
                    : 'hover:bg-gray-50 dark:hover:bg-gray-800/50 border-l-4 border-l-transparent'
                }`}
            style={style}
        >
            {columns.map((column) => renderCell(column))}
        </div>
    );
});

LogTableRow.displayName = 'LogTableRow';
