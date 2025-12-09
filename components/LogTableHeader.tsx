import React from 'react';
import { ColumnDefinition, LogTableDensity } from '../types';
import { getCellClass } from './tableUtils';

interface LogTableHeaderProps {
    columns: ColumnDefinition[];
    logTableDensity: LogTableDensity;
    gridTemplateColumns: string;
    onContextMenu: (e: React.MouseEvent) => void;
}

export const LogTableHeader: React.FC<LogTableHeaderProps> = ({
    columns,
    logTableDensity,
    gridTemplateColumns,
    onContextMenu
}) => {
    return (
        <div
            className="sticky top-0 z-10 shadow-sm"
            onContextMenu={onContextMenu}
        >
            <div
                className="grid font-sans bg-gray-100 dark:bg-gray-800 border-b border-gray-200 dark:border-gray-700 items-center"
                style={{ gridTemplateColumns }}
            >
                {columns.length > 0 ? (
                    columns.map((column) => (
                        <div
                            key={column.key}
                            className={`py-2 ${getCellClass(logTableDensity)} text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider`}
                        >
                            {column.label}
                        </div>
                    ))
                ) : (
                    <div className={`py-2 ${getCellClass(logTableDensity)} text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider`}>
                        No columns selected
                    </div>
                )}
            </div>
        </div>
    );
};
