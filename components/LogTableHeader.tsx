import React from 'react';
import { ColumnDefinition, LogTableDensity, ColumnKey, ColumnWidths } from '../types';
import { getCellClass } from './tableUtils';

interface LogTableHeaderProps {
    columns: ColumnDefinition[];
    logTableDensity: LogTableDensity;
    gridTemplateColumns: string;
    onContextMenu: (e: React.MouseEvent) => void;
    onColumnResize?: (key: ColumnKey, width: number) => void;
}

export const LogTableHeader: React.FC<LogTableHeaderProps> = ({
    columns,
    logTableDensity,
    gridTemplateColumns,
    onContextMenu,
    onColumnResize
}) => {
    const [resizing, setResizing] = React.useState<{ key: ColumnKey; startX: number; startWidth: number } | null>(null);

    const handleMouseDown = React.useCallback((e: React.MouseEvent, column: ColumnDefinition, headerElement: HTMLDivElement) => {
        e.preventDefault();
        e.stopPropagation();
        const rect = headerElement.getBoundingClientRect();
        setResizing({
            key: column.key,
            startX: e.clientX,
            startWidth: rect.width
        });
    }, []);

    React.useEffect(() => {
        if (!resizing) return;

        const handleMouseMove = (e: MouseEvent) => {
            const delta = e.clientX - resizing.startX;
            const newWidth = Math.max(50, resizing.startWidth + delta);
            onColumnResize?.(resizing.key, newWidth);
        };

        const handleMouseUp = () => {
            setResizing(null);
        };

        document.addEventListener('mousemove', handleMouseMove);
        document.addEventListener('mouseup', handleMouseUp);

        return () => {
            document.removeEventListener('mousemove', handleMouseMove);
            document.removeEventListener('mouseup', handleMouseUp);
        };
    }, [resizing, onColumnResize]);

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
                            className={`relative py-2 ${getCellClass(logTableDensity)} text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider select-none`}
                            ref={(el) => {
                                // Store ref for resize calculation
                                if (el) (el as any)._headerRef = el;
                            }}
                        >
                            {column.label}
                            {/* Resize handle */}
                            {onColumnResize && (
                                <div
                                    className={`absolute right-0 top-0 bottom-0 w-1 cursor-col-resize hover:bg-blue-400 transition-colors ${resizing?.key === column.key ? 'bg-blue-500' : ''}`}
                                    onMouseDown={(e) => {
                                        const headerEl = e.currentTarget.parentElement as HTMLDivElement;
                                        handleMouseDown(e, column, headerEl);
                                    }}
                                />
                            )}
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
