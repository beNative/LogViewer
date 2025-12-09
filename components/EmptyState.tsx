import React from 'react';
import { getRowClass, getCellClass } from './tableUtils';
import { LogTableDensity } from '../types';

interface EmptyStateProps {
    isBusy: boolean;
    rowVirtualizerTotalSize: number;
    rowHeight: number;
    gridTemplateColumns: string;
    logTableDensity: LogTableDensity;
    entriesCount: number;
}

export const EmptyState: React.FC<EmptyStateProps> = ({
    isBusy,
    rowVirtualizerTotalSize,
    rowHeight,
    gridTemplateColumns,
    logTableDensity,
    entriesCount
}) => {
    // Show loading indicator when busy
    if (isBusy) {
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
    }

    // Show empty state message when no entries and not loading
    if (entriesCount === 0) {
        return (
            <div
                className="flex items-center justify-center py-12"
                style={{
                    position: 'absolute',
                    top: 0,
                    left: 0,
                    right: 0,
                    minHeight: '200px',
                }}
            >
                <div className="text-center">
                    <svg
                        className="mx-auto h-12 w-12 text-gray-400 dark:text-gray-500"
                        fill="none"
                        viewBox="0 0 24 24"
                        stroke="currentColor"
                    >
                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9.75 9.75l4.5 4.5m0-4.5l-4.5 4.5M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                    </svg>
                    <h3 className="mt-2 text-sm font-medium text-gray-900 dark:text-gray-100">No matching entries</h3>
                    <p className="mt-1 text-sm text-gray-500 dark:text-gray-400">Try adjusting your filters or time range.</p>
                </div>
            </div>
        );
    }

    return null;
};
