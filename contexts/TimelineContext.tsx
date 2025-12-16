import React, { createContext, useContext, useState, useCallback, useMemo } from 'react';
import { OverallTimeRange, LogDensityPointByLevel, LogDensityPoint } from '../types';

type TimelineContextType = {
    // State
    viewRange: OverallTimeRange | null;
    selection: { start: number; end: number } | null;
    density: LogDensityPointByLevel[] | LogDensityPoint[];

    // Actions
    setViewRange: (range: OverallTimeRange | null) => void;
    setSelection: (start: number | null, end: number | null) => void;
    setDensity: (density: LogDensityPointByLevel[] | LogDensityPoint[]) => void;

    // Helpers
    zoomToSelection: () => void;
    resetZoom: () => void;
    lastLog?: string;
};

const TimelineContext = createContext<TimelineContextType | undefined>(undefined);

export const TimelineProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const [viewRange, setViewRangeState] = useState<OverallTimeRange | null>(null);
    const [selection, setSelectionState] = useState<{ start: number; end: number } | null>(null);
    const [density, setDensity] = useState<LogDensityPointByLevel[] | LogDensityPoint[]>([]);



    const [lastLog, setLastLog] = useState<string>('Ready');

    const setViewRange = useCallback((range: OverallTimeRange | null) => {
        const msg = range ? `setViewRange: ${new Date(range.min).toISOString().substring(11, 19)} - ${new Date(range.max).toISOString().substring(11, 19)}` : 'setViewRange: FULL EXTENT';
        console.log(msg);
        setLastLog(prev => msg + ' | ' + prev.substring(0, 50));
        setViewRangeState(range);
    }, []);

    const setSelection = useCallback((start: number | null, end: number | null) => {
        if (start !== null && end !== null) {
            setSelectionState({ start, end });
        } else {
            setSelectionState(null);
        }
    }, []);

    const zoomToSelection = useCallback(() => {
        if (selection) {
            setLastLog(prev => 'ACTION: ZoomToSelection | ' + prev);
            setViewRange({ min: selection.start, max: selection.end });
        } else {
            setLastLog(prev => 'ACTION: ZoomToSelection (No Selection) | ' + prev);
        }
    }, [selection, setViewRange]);

    const resetZoom = useCallback(() => {
        setLastLog(prev => 'ACTION: ResetZoom | ' + prev);
        setViewRange(null);
    }, [setViewRange]);

    const value = useMemo(() => ({
        viewRange,
        selection,
        density,
        setViewRange,
        setSelection,
        setDensity,
        zoomToSelection,
        resetZoom
    }), [viewRange, selection, density, zoomToSelection, resetZoom]);

    return (
        <TimelineContext.Provider value={value}>
            {children}
        </TimelineContext.Provider>
    );
};

export const useTimeline = () => {
    const context = useContext(TimelineContext);
    if (context === undefined) {
        throw new Error('useTimeline must be used within a TimelineProvider');
    }
    return context;
};
