import React, { createContext, useContext, useState, useCallback, useMemo } from 'react';
import { StockInfoEntry, StockInfoFilters, StockArticleSuggestion, LogDensityPoint } from '../types';
import { useSession } from './SessionContext';
import { useUI } from './UIContext';
import { useConsole } from './ConsoleContext';

type StockContextType = {
    stockHistory: StockInfoEntry[];
    setStockHistory: React.Dispatch<React.SetStateAction<StockInfoEntry[]>>;
    overallStockTimeRange: { min: string, max: string } | null;
    overallStockDensity: LogDensityPoint[];
    handleSearchStock: (filters: StockInfoFilters) => void;
    handleRebuildStockData: () => Promise<void>;
    handleFetchStockSuggestions: (searchTerm: string, timeFilters: StockInfoFilters, limit?: number) => Promise<StockArticleSuggestion[]>;
};

const StockContext = createContext<StockContextType | undefined>(undefined);

export const StockProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const { db, hasData, handleRebuildStockDataInWorker, overallStockTimeRange, setOverallStockTimeRange, overallStockDensity, setOverallStockDensity } = useSession();
    const { logToConsole } = useConsole();
    const { setIsStockBusy, addToast } = useUI();

    const [stockHistory, setStockHistory] = useState<StockInfoEntry[]>([]);

    const handleSearchStock = useCallback((filters: StockInfoFilters) => {
        if (!db) return;
        setIsStockBusy(true);
        setTimeout(() => {
            try {
                const { entries: results } = db.queryStockInfo(filters);
                setStockHistory(results);
                if (results.length > 0 && !overallStockTimeRange) {
                    const { minTime, maxTime } = db.getMinMaxStockTime();
                    if (minTime && maxTime) {
                        setOverallStockTimeRange({ min: minTime, max: maxTime });
                        setOverallStockDensity(db.getStockDensity({} as StockInfoFilters, 300));
                    }
                }
            } catch (e) {
                logToConsole(`Stock search failed: ${e instanceof Error ? e.message : String(e)}`, 'ERROR');
            } finally { setIsStockBusy(false); }
        }, 50);
    }, [db, logToConsole, setIsStockBusy, overallStockTimeRange, setOverallStockDensity, setOverallStockTimeRange]);

    const handleFetchStockSuggestions = useCallback(async (searchTerm: string, timeFilters: StockInfoFilters, limit: number = 15): Promise<StockArticleSuggestion[]> => {
        if (!db) return [];
        // Allow empty search term if we want to list all
        if (searchTerm.length < 2 && limit !== 0 && searchTerm !== '') return [];
        try { return db.getUniqueArticles(searchTerm, timeFilters, limit); }
        catch (e) { logToConsole(`Failed to fetch stock suggestions: ${e instanceof Error ? e.message : String(e)}`, 'ERROR'); return []; }
    }, [db, logToConsole]);

    const handleRebuildStockData = useCallback(async () => {
        if (!db || !hasData) {
            addToast({ type: 'error', title: 'Rebuild Failed', message: 'No data is currently loaded.' });
            return;
        }
        await handleRebuildStockDataInWorker(true);
        // After worker is done, the SessionContext will call updateStateFromDb, which will
        // update the stock time range and density. We just need to clear the local history.
        setStockHistory([]);
    }, [db, hasData, addToast, handleRebuildStockDataInWorker]);

    const value = useMemo(() => ({
        stockHistory,
        setStockHistory,
        overallStockTimeRange,
        overallStockDensity,
        handleSearchStock,
        handleRebuildStockData,
        handleFetchStockSuggestions
    }), [
        stockHistory,
        overallStockTimeRange,
        overallStockDensity,
        handleSearchStock,
        handleRebuildStockData,
        handleFetchStockSuggestions
    ]);

    return (
        <StockContext.Provider value={value}>
            {children}
        </StockContext.Provider>
    );
};

export const useStock = (): StockContextType => {
    const context = useContext(StockContext);
    if (context === undefined) {
        throw new Error('useStock must be used within a StockProvider');
    }
    return context;
};
