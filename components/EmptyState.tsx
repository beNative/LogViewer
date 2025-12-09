import React from 'react';
import { getRowClass, getCellClass } from './tableUtils';
import { LogTableDensity } from '../types';

interface EmptyStateProps {
    isBusy: boolean;
    rowVirtualizerTotalSize: number;
    rowHeight: number;
    gridTemplateColumns: string;
    logTableDensity: LogTableDensity;
}

export const EmptyState: React.FC<EmptyStateProps> = ({
    isBusy,
    rowVirtualizerTotalSize,
    rowHeight,
    gridTemplateColumns,
    logTableDensity
}) => {
    if (!isBusy) return null;

    return (
        <div
            className="grid font-sans items-center border-t border-gray-200 dark:border-gray-700"
            style={{
                position: 'absolute',
                top: `${rowVirtualizerTotalSize}px`,
                left: 0,
                right: 0,
                height: `${rowHeight}px`,
                gridTemplateColumns,
            }}
        >
            <div
                className={`${getRowClass(logTableDensity)} ${getCellClass(logTableDensity)} text-center text-sm text-gray-500 dark:text-gray-400`}
                style={{ gridColumn: '1 / -1' }}
            >
                Loading...
            </div>
        </div>
    );
};
