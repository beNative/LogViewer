import React, { createContext, useContext, useState, useEffect, useMemo, useRef } from 'react';
import { DashboardData, FileTimeRange, LogDensityPointByLevel, OverallTimeRange } from '../types';
import { useSession } from './SessionContext';
import { useData } from './DataContext';
import { useTimeline } from './TimelineContext';
import { useConsole } from './ConsoleContext';

const initialDashboardData: DashboardData = { timeline: [], levels: [], senderTypes: [] };

type AnalyticsContextType = {
    dashboardData: DashboardData;
    fileTimeRanges: FileTimeRange[];
    logDensity: LogDensityPointByLevel[];
    overallLogDensity: LogDensityPointByLevel[];
    datesWithLogs: string[];
    overallLogTimeRange: OverallTimeRange | null;
};

const AnalyticsContext = createContext<AnalyticsContextType | undefined>(undefined);

export const AnalyticsProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const { db, hasData } = useSession();
    const { appliedFilters, isInitialLoad } = useData();
    const { viewRange } = useTimeline();
    const { logToConsole } = useConsole();

    const [dashboardData, setDashboardData] = useState<DashboardData>(initialDashboardData);
    const [fileTimeRanges, setFileTimeRanges] = useState<FileTimeRange[]>([]);
    const [logDensity, setLogDensity] = useState<LogDensityPointByLevel[]>([]);
    const [overallLogDensity, setOverallLogDensity] = useState<LogDensityPointByLevel[]>([]);
    const [datesWithLogs, setDatesWithLogs] = useState<string[]>([]);
    const [overallLogTimeRange, setOverallLogTimeRange] = useState<OverallTimeRange | null>(null);

    const busyTaskRef = useRef(0);

    // Initial overall density when DB is loaded
    useEffect(() => {
        if (db && hasData) {
            // Calculate overall density with empty filters (or initial filters if we had access, but for overall usually empty is fine? 
            // Actually DataContext uses initialFilters. Let's assume empty for overall density or derived? 
            // DataContext: setOverallLogDensity(db.getLogDensityByLevel(initialFilters, 300));
            // We can't access initialFilters here easily unless exported. 
            // But usually overall means "everything". 
            // Let's use empty object {} if allowed, or we can fetch keys from db.getUniqueColumnValues...
            // Wait, DataContext used initialFilters. 
            // Let's rely on what DataContext passes? No, we want to extract logic.
            // We'll calculate it from db.
            // Note: DataContext had access to 'initialFilters'.
            setOverallLogDensity(db.getLogDensityByLevel({} as any, 300));

            const { minTime, maxTime } = db.getMinMaxTime();
            if (minTime && maxTime) {
                // Assuming DB time is string like "YYYY-MM-DD HH:MM:SS.mmm". 
                // We append 'Z' to treat as UTC, consistent with other parts.
                const min = new Date(minTime.replace(' ', 'T') + 'Z').getTime();
                const max = new Date(maxTime.replace(' ', 'T') + 'Z').getTime();
                setOverallLogTimeRange({ min, max });
            } else {
                setOverallLogTimeRange(null);
            }
        } else {
            setOverallLogDensity([]);
            setOverallLogTimeRange(null);
        }
    }, [db, hasData]);

    // Main Analytics Calculation
    useEffect(() => {
        if (!db || !hasData) {
            setDashboardData(initialDashboardData);
            setFileTimeRanges([]);
            setLogDensity([]);
            setDatesWithLogs([]);
            return;
        }

        // We use a small timeout to avoid blocking UI and allow DataContext to update state if needed
        // but typically this runs in parallel with DataContext's query.
        clearTimeout(busyTaskRef.current);

        busyTaskRef.current = window.setTimeout(() => {
            logToConsole('Updating analytics...', 'DEBUG');

            // Dashboard Data
            setDashboardData({
                timeline: db.getLogVolumeByInterval(appliedFilters),
                levels: db.getCountsByColumn('level', appliedFilters),
                senderTypes: db.getCountsByColumn('sndrtype', appliedFilters),
            });

            setFileTimeRanges(db.getTimeRangePerFile(appliedFilters));
            setDatesWithLogs(db.getDatesWithLogs(appliedFilters));

            // Log Density (default view)
            // If viewRange is NOT set, we compute density based on filters.
            // If viewRange IS set, strict zoom logic handles it in separate effect? 
            // DataContext calculated it here regardless. 
            setLogDensity(db.getLogDensityByLevel(appliedFilters, 200));

            logToConsole('Analytics updated.', 'DEBUG');
        }, 50); // Same debounce as DataContext

        return () => clearTimeout(busyTaskRef.current);
    }, [db, hasData, appliedFilters, logToConsole]);

    // High-resolution density on zoom
    useEffect(() => {
        if (!db || !hasData) return;

        if (viewRange) {
            const zoomedDensity = db.getLogDensityByLevel(appliedFilters, 200, viewRange.min, viewRange.max);
            setLogDensity(zoomedDensity);
        } else {
            // Reset is handled by main effect when filters change or viewRange becomes null?
            // Actually main effect depends on 'appliedFilters'. 
            // If viewRange changes to null, main effect doesn't re-run. 
            // We need to reset to global density.
            setLogDensity(db.getLogDensityByLevel(appliedFilters, 200));
        }
    }, [db, hasData, viewRange, appliedFilters]);

    const value = useMemo(() => ({
        dashboardData,
        fileTimeRanges,
        logDensity,
        overallLogDensity,

        datesWithLogs,
        overallLogTimeRange
    }), [dashboardData, fileTimeRanges, logDensity, overallLogDensity, datesWithLogs, overallLogTimeRange]);

    return (
        <AnalyticsContext.Provider value={value}>
            {children}
        </AnalyticsContext.Provider>
    );
};

export const useAnalytics = (): AnalyticsContextType => {
    const context = useContext(AnalyticsContext);
    if (context === undefined) {
        throw new Error('useAnalytics must be used within an AnalyticsProvider');
    }
    return context;
};
