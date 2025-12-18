import React from 'react';
import { OverallTimeRange } from '../types';

export const useOptimisticState = (
    selectedStartTime: number | null,
    selectedEndTime: number | null,
    viewRange: OverallTimeRange | null,
    setSelection: (start: number, end: number) => void
) => {
    const [optimisticSelection, setOptimisticSelection] = React.useState<{ start: number, end: number } | null>(null);
    const [optimisticViewRange, setOptimisticViewRange] = React.useState<{ min: number, max: number } | null>(null);

    // Clear optimistic state when props update to match (or close enough)
    React.useEffect(() => {
        if (optimisticSelection && selectedStartTime !== null && selectedEndTime !== null) {
            // If props have caught up to optimistic state (within 100ms tolerance), clear optimistic
            if (Math.abs(selectedStartTime - optimisticSelection.start) < 100 &&
                Math.abs(selectedEndTime - optimisticSelection.end) < 100) {
                setOptimisticSelection(null);
            }
        }
        // Sync props to Context Selection so zoomToSelection works
        if (selectedStartTime !== null && selectedEndTime !== null) {
            setSelection(selectedStartTime, selectedEndTime);
        }
    }, [selectedStartTime, selectedEndTime, optimisticSelection, setSelection]);

    React.useEffect(() => {
        if (optimisticViewRange && viewRange) {
            if (Math.abs(viewRange.min - optimisticViewRange.min) < 100 &&
                Math.abs(viewRange.max - optimisticViewRange.max) < 100) {
                setOptimisticViewRange(null);
            }
        }
    }, [viewRange, optimisticViewRange]);

    // Fallback: Clear optimistic state after a timeout to prevent getting stuck if props never update
    React.useEffect(() => {
        if (optimisticSelection || optimisticViewRange) {
            const timer = setTimeout(() => {
                setOptimisticSelection(null);
                setOptimisticViewRange(null);
            }, 1000);
            return () => clearTimeout(timer);
        }
    }, [optimisticSelection, optimisticViewRange]);

    return {
        optimisticSelection,
        setOptimisticSelection,
        optimisticViewRange,
        setOptimisticViewRange
    };
};
