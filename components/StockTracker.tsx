import React from 'react';
import { StockInfoEntry, StockInfoFilters, IconSet, Theme, LogDensityPoint, OverallTimeRange } from '../types.ts';
import { Icon } from './icons/index.tsx';
import { StockHistoryChart } from './StockHistoryChart.tsx';
import { TimeRangeSelector } from './TimeRangeSelector.tsx';

interface StockTrackerProps {
  onSearch: (filters: StockInfoFilters) => void;
  history: StockInfoEntry[];
  isBusy: boolean;
  iconSet: IconSet;
  theme: Theme;
  overallTimeRange: { min: string, max: string } | null;
  overallStockDensity: LogDensityPoint[];
  uiScale: number;
}

const inputStyles = "w-full bg-white dark:bg-gray-700/80 border-gray-300 dark:border-gray-600 text-gray-900 dark:text-white sm:text-sm rounded-md shadow-sm focus:ring-sky-500 focus:border-sky-500 transition";


export const StockTracker: React.FC<StockTrackerProps> = ({ onSearch, history, isBusy, iconSet, theme, overallTimeRange, overallStockDensity, uiScale }) => {
    const [filters, setFilters] = React.useState<StockInfoFilters>({
        searchTerm: '',
        dateFrom: '',
        timeFrom: '',
        dateTo: '',
        timeTo: '',
    });
    const [timelineViewRange, setTimelineViewRange] = React.useState<OverallTimeRange | null>(null);


    React.useEffect(() => {
        if (overallTimeRange && (!filters.dateFrom || !filters.dateTo)) {
            const [minDate, minTimeStr] = overallTimeRange.min.split(' ');
            const [maxDate, maxTimeStr] = overallTimeRange.max.split(' ');
            setFilters(prev => ({
                ...prev,
                dateFrom: minDate,
                timeFrom: minTimeStr?.substring(0, 8) || '00:00:00',
                dateTo: maxDate,
                timeTo: maxTimeStr?.substring(0, 8) || '23:59:59',
            }));
        }
    }, [overallTimeRange, filters.dateFrom, filters.dateTo]);


    const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        setFilters({ ...filters, [e.target.name]: e.target.value });
    };

    const handleSearch = (e: React.FormEvent) => {
        e.preventDefault();
        onSearch(filters);
    };

    const chartData = React.useMemo(() => {
        if (!history || history.length === 0) return [];
        return history.map(entry => ({
            time: entry.timestamp.replace(' ', 'T') + 'Z',
            quantity: entry.quantity,
        }));
    }, [history]);

    const handleTimeRangeChange = (startTime: number, endTime: number) => {
        const startDate = new Date(startTime);
        const endDate = new Date(endTime);

        const dateToYYYYMMDD = (d: Date) => d.toISOString().split('T')[0];
        const dateToHHMMSS = (d: Date) => d.toISOString().split('T')[1].substring(0, 8);
        
        setFilters(currentFilters => ({
            ...currentFilters,
            dateFrom: dateToYYYYMMDD(startDate),
            timeFrom: dateToHHMMSS(startDate),
            dateTo: dateToYYYYMMDD(endDate),
            timeTo: dateToHHMMSS(endDate),
        }));
    };

    const handleTimeRangeClear = () => {
        setFilters(currentFilters => ({
            ...currentFilters,
            dateFrom: '',
            timeFrom: '',
            dateTo: '',
            timeTo: '',
        }));
    };

    const getSelectedTimestamps = React.useCallback(() => {
        let selectedStartTime: number | null = null;
        let selectedEndTime: number | null = null;
    
        if (filters.dateFrom) {
            let timePart = filters.timeFrom || '00:00:00';
            if (timePart.length === 5) timePart += ':00';
            const dateString = `${filters.dateFrom}T${timePart}.000Z`;
            const date = new Date(dateString);
            if (!isNaN(date.getTime())) {
                selectedStartTime = date.getTime();
            }
        }
         if (filters.dateTo) {
            let timePart = filters.timeTo || '23:59:59';
            if (timePart.length === 5) timePart += ':00';
            const dateString = `${filters.dateTo}T${timePart}.999Z`;
            const date = new Date(dateString);
             if (!isNaN(date.getTime())) {
                selectedEndTime = date.getTime();
            }
        }
        return { selectedStartTime, selectedEndTime };
    }, [filters.dateFrom, filters.timeFrom, filters.dateTo, filters.timeTo]);

    const { selectedStartTime, selectedEndTime } = getSelectedTimestamps();

    const handleTimelineZoomToSelection = React.useCallback(() => {
        if (selectedStartTime && selectedEndTime && selectedStartTime < selectedEndTime) {
            setTimelineViewRange({ min: selectedStartTime, max: selectedEndTime });
        }
    }, [selectedStartTime, selectedEndTime]);

    const zoomToSelectionEnabled = React.useMemo(() => {
        return selectedStartTime !== null && selectedEndTime !== null && selectedStartTime < selectedEndTime;
    }, [selectedStartTime, selectedEndTime]);

    return (
        <div className="flex-grow p-4 sm:p-6 lg:p-8 overflow-y-auto space-y-6 bg-gray-100 dark:bg-transparent">
            <div className="bg-white dark:bg-gray-800/50 p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
                <h2 className="text-xl font-semibold text-gray-800 dark:text-gray-200 mb-4">Search Article Stock</h2>
                <form onSubmit={handleSearch} className="space-y-4">
                     <div className="grid grid-cols-1 md:grid-cols-3 gap-4 items-end">
                        <div className="md:col-span-2">
                            <label htmlFor="searchTerm" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">Article ID or Name</label>
                            <input
                                type="text"
                                name="searchTerm"
                                id="searchTerm"
                                value={filters.searchTerm}
                                onChange={handleInputChange}
                                className={inputStyles}
                                placeholder="e.g., 3400936009011 or CARTEOL"
                            />
                        </div>
                        <button
                            type="submit"
                            disabled={isBusy || !filters.searchTerm}
                            className="inline-flex items-center justify-center gap-2 px-4 py-2 font-semibold rounded-lg transition-colors duration-200 bg-sky-600 hover:bg-sky-700 text-white dark:bg-sky-600 dark:hover:bg-sky-500 disabled:opacity-50 disabled:cursor-not-allowed"
                        >
                            <Icon name="Filter" iconSet={iconSet} className="w-5 h-5" />
                            <span>{isBusy ? 'Searching...' : 'Search'}</span>
                        </button>
                    </div>

                    {overallTimeRange && (
                        <div className="pt-4 border-t border-gray-200 dark:border-gray-700">
                            <TimeRangeSelector
                                minTime={new Date(overallTimeRange.min + 'Z').getTime()}
                                maxTime={new Date(overallTimeRange.max + 'Z').getTime()}
                                selectedStartTime={selectedStartTime}
                                selectedEndTime={selectedEndTime}
                                onRangeChange={handleTimeRangeChange}
                                onClear={handleTimeRangeClear}
                                theme={theme}
                                pageTimestampRanges={[]}
                                fileTimeRanges={[]}
                                logDensity={[]}
                                overallLogDensity={overallStockDensity}
                                datesWithLogs={[]}
                                viewMode="scroll"
                                onCursorChange={() => {}}
                                onFileSelect={() => {}}
                                onDateSelect={() => {}}
                                viewRange={timelineViewRange}
                                onViewRangeChange={setTimelineViewRange}
                                onZoomToSelection={handleTimelineZoomToSelection}
                                onZoomToExtent={() => setTimelineViewRange(null)}
                                zoomToSelectionEnabled={zoomToSelectionEnabled}
                                iconSet={iconSet}
                                uiScale={uiScale}
                            />
                        </div>
                    )}
                </form>
            </div>
            
            {history.length > 0 && (
                 <div className="grid grid-cols-1 xl:grid-cols-2 gap-6">
                    <div className="bg-white dark:bg-gray-800/50 p-4 sm:p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
                        <h3 className="text-lg font-semibold text-gray-800 dark:text-gray-200 mb-4">Stock History</h3>
                         <div className="overflow-x-auto max-h-96">
                            <table className="min-w-full divide-y divide-gray-200 dark:divide-gray-700">
                                <thead className="bg-gray-50 dark:bg-gray-800/50 sticky top-0">
                                    <tr>
                                        <th className="px-3 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">Timestamp</th>
                                        <th className="px-3 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">Article Name</th>
                                        <th className="px-3 py-2 text-right text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">Quantity</th>
                                    </tr>
                                </thead>
                                <tbody className="bg-white dark:bg-gray-900 divide-y divide-gray-200/50 dark:divide-gray-700/50">
                                    {history.map((entry, index) => (
                                        <tr key={`${entry.timestamp}-${index}`}>
                                            <td className="px-3 py-2 whitespace-nowrap text-sm text-gray-500 dark:text-gray-400 font-mono">{entry.timestamp}</td>
                                            <td className="px-3 py-2 whitespace-nowrap text-sm text-gray-800 dark:text-gray-200">{entry.article_name}</td>
                                            <td className="px-3 py-2 whitespace-nowrap text-sm text-right text-gray-800 dark:text-gray-200 font-semibold">{entry.quantity}</td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        </div>
                    </div>
                     <div className="bg-white dark:bg-gray-800/50 p-4 sm:p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
                        <h3 className="text-lg font-semibold text-gray-800 dark:text-gray-200 mb-4">Quantity Over Time</h3>
                        <div className="h-96">
                            <StockHistoryChart data={chartData} theme={theme} />
                        </div>
                    </div>
                 </div>
            )}

            {!isBusy && history.length === 0 && (
                <div className="text-center py-10">
                    <Icon name="Cube" iconSet={iconSet} className="w-16 h-16 text-gray-300 dark:text-gray-600 mx-auto mb-4" />
                    <h3 className="text-xl font-semibold mb-2 text-gray-700 dark:text-gray-300">No Stock Data</h3>
                    <p className="text-gray-500 dark:text-gray-400">Perform a search to view stock history for an article.</p>
                </div>
            )}
        </div>
    );
};