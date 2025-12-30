import React, { useEffect, useRef } from 'react';
import { LogEntry } from '../types';

interface UseTableNavigationProps {
    tableContainerRef: React.RefObject<HTMLDivElement>;
    entries: LogEntry[];
    keyboardSelectedId: number | null;
    onNavigate: (id: number, event: KeyboardEvent) => void;
    rowHeight: number;
    hasMore: boolean;
    hasPrevLogs: boolean;
    onLoadMore: () => void;
    onLoadPrev: () => void;
    isBusy: boolean;
}

export const useTableNavigation = ({
    tableContainerRef,
    entries,
    keyboardSelectedId,
    onNavigate,
    rowHeight,
    hasMore,
    hasPrevLogs,
    onLoadMore,
    onLoadPrev,
    isBusy
}: UseTableNavigationProps) => {
    // Track the last valid navigation index to handle loading transitions
    const lastNavIndexRef = useRef<number>(0);

    useEffect(() => {
        const container = tableContainerRef.current;
        if (!container) return;

        const handleKeyDown = (e: KeyboardEvent) => {
            const navKeys = ['ArrowUp', 'ArrowDown', 'PageUp', 'PageDown', 'Home', 'End'];
            if (!navKeys.includes(e.key)) {
                return;
            }
            // Prevent default scrolling behavior for navigation keys
            e.preventDefault();

            // Don't process navigation while loading to prevent race conditions
            if (isBusy) {
                return;
            }

            let currentIndex = entries.findIndex(entry => entry.id === keyboardSelectedId);

            // If selected entry not found, use last known navigation index
            if (currentIndex === -1) {
                if (entries.length === 0) return;
                // Clamp to valid range
                currentIndex = Math.max(0, Math.min(lastNavIndexRef.current, entries.length - 1));
            } else {
                lastNavIndexRef.current = currentIndex;
            }

            // Calculate visible rows per page based on container height
            const visibleRows = Math.max(1, Math.floor(container.clientHeight / rowHeight) - 1);

            let nextIndex = currentIndex;
            switch (e.key) {
                case 'ArrowDown':
                    nextIndex = currentIndex + 1;
                    break;
                case 'ArrowUp':
                    nextIndex = currentIndex - 1;
                    break;
                case 'PageDown':
                    nextIndex = Math.min(currentIndex + visibleRows, entries.length - 1);
                    break;
                case 'PageUp':
                    nextIndex = Math.max(currentIndex - visibleRows, 0);
                    break;
                case 'Home':
                    nextIndex = 0;
                    break;
                case 'End':
                    nextIndex = entries.length - 1;
                    break;
            }

            // Ensure index is valid
            if (nextIndex < 0) nextIndex = 0;
            if (nextIndex >= entries.length) nextIndex = entries.length - 1;

            if (nextIndex !== currentIndex && entries[nextIndex]) {
                onNavigate(entries[nextIndex].id, e);
                lastNavIndexRef.current = nextIndex;
            }
        };

        container.addEventListener('keydown', handleKeyDown);
        return () => container.removeEventListener('keydown', handleKeyDown);

    }, [entries, keyboardSelectedId, onNavigate, hasMore, hasPrevLogs, onLoadMore, onLoadPrev, rowHeight, isBusy, tableContainerRef]);
};
