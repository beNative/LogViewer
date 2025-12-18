import React from 'react';
import { StockInfoEntry, StockInfoFilters, IconSet, Theme, LogDensityPoint, OverallTimeRange, StockArticleSuggestion, TimelineBarVisibility } from '../types.ts';
import { Icon } from './icons/index.tsx';
import { StockHistoryChart } from './StockHistoryChart.tsx';
import { TimeRangeSelector } from './TimeRangeSelector.tsx';
import { useConfirmDialog } from '../contexts/ConfirmDialogContext';
import { useTimeline } from '../contexts/TimelineContext';
import { useStock } from '../contexts/StockContext';
import { useUI } from '../contexts/UIContext';
import { useSettings } from '../contexts/SettingsContext';

interface StockTrackerProps { }

const inputStyles = "w-full px-3 py-2 bg-white dark:bg-gray-700/80 border border-gray-300 dark:border-gray-600 text-gray-900 dark:text-white sm:text-sm rounded-md shadow-sm focus:ring-sky-500 focus:border-sky-500 transition";


export const StockTracker: React.FC<StockTrackerProps> = () => {
    const { stockHistory, overallStockTimeRange, overallStockDensity, handleSearchStock, handleRebuildStockData, handleFetchStockSuggestions } = useStock();
    const { isStockBusy } = useUI();
    const { iconSet, theme, uiScale, timelineBarVisibility, onTimelineBarVisibilityChange } = useSettings();
    const [filters, setFilters] = React.useState<StockInfoFilters>({
        searchTerm: '',
        dateFrom: '',
        timeFrom: '',
        dateTo: '',
        timeTo: '',
    });
    // const [timelineViewRange, setTimelineViewRange] = React.useState<OverallTimeRange | null>(null); // Removed in favor of context
    const { setViewRange, setSelection } = useTimeline();
    const [suggestions, setSuggestions] = React.useState<StockArticleSuggestion[]>([]);
    const [isSuggestionsVisible, setIsSuggestionsVisible] = React.useState(false);
    const [activeSuggestionIndex, setActiveSuggestionIndex] = React.useState(-1);
    const [selectedHistoryIndex, setSelectedHistoryIndex] = React.useState<number | null>(null);
    const debounceTimeoutRef = React.useRef<number | null>(null);
    const suggestionContainerRef = React.useRef<HTMLDivElement>(null);
    const tableContainerRef = React.useRef<HTMLDivElement>(null);
    const rowRefs = React.useRef<Map<number, HTMLTableRowElement | null>>(new Map());
    const { confirm } = useConfirmDialog();


    React.useEffect(() => {
        if (overallStockTimeRange && (!filters.dateFrom || !filters.dateTo)) {
            const [minDate, minTimeStr] = overallStockTimeRange.min.split(' ');
            const [maxDate, maxTimeStr] = overallStockTimeRange.max.split(' ');
            setFilters(prev => ({
                ...prev,
                dateFrom: minDate,
                timeFrom: minTimeStr?.substring(0, 8) || '00:00:00',
                dateTo: maxDate,
                timeTo: maxTimeStr?.substring(0, 8) || '23:59:59',
            }));
        }
    }, [overallStockTimeRange, filters.dateFrom, filters.dateTo]);

    // Effect to hide suggestions on outside click
    React.useEffect(() => {
        const handleClickOutside = (event: MouseEvent) => {
            if (suggestionContainerRef.current && !suggestionContainerRef.current.contains(event.target as Node)) {
                setIsSuggestionsVisible(false);
            }
        };
        document.addEventListener('mousedown', handleClickOutside);
        return () => document.removeEventListener('mousedown', handleClickOutside);
    }, []);

    // Effect to scroll the selected history item into view
    React.useEffect(() => {
        if (selectedHistoryIndex !== null) {
            const rowEl = rowRefs.current.get(selectedHistoryIndex);
            rowEl?.scrollIntoView({ block: 'nearest', behavior: 'smooth' });
        }
    }, [selectedHistoryIndex]);

    const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        const { name, value } = e.target;

        setFilters(prevFilters => {
            const newFilters = { ...prevFilters, [name]: value };

            if (name === 'searchTerm') {
                if (debounceTimeoutRef.current) {
                    clearTimeout(debounceTimeoutRef.current);
                }

                if (value.length < 2) {
                    setSuggestions([]);
                    setIsSuggestionsVisible(false);
                } else {
                    debounceTimeoutRef.current = window.setTimeout(async () => {
                        const timeFilters: StockInfoFilters = {
                            searchTerm: '', // Not needed for suggestion fetching based on time
                            dateFrom: newFilters.dateFrom,
                            timeFrom: newFilters.timeFrom,
                            dateTo: newFilters.dateTo,
                            timeTo: newFilters.timeTo,
                        };
                        const fetchedSuggestions = await handleFetchStockSuggestions(value, timeFilters);
                        setSuggestions(fetchedSuggestions);
                        setIsSuggestionsVisible(fetchedSuggestions.length > 0);
                        setActiveSuggestionIndex(-1);
                    }, 300);
                }
            }

            return newFilters;
        });
    };

    const handleSuggestionClick = (suggestion: StockArticleSuggestion) => {
        setFilters({ ...filters, searchTerm: suggestion.name });
        setSuggestions([]);
        setIsSuggestionsVisible(false);
    };

    const handleSuggestionKeyDown = (e: React.KeyboardEvent) => {
        if (!isSuggestionsVisible || suggestions.length === 0) return;

        if (e.key === 'ArrowDown') {
            e.preventDefault();
            setActiveSuggestionIndex(prev => (prev + 1) % suggestions.length);
        } else if (e.key === 'ArrowUp') {
            e.preventDefault();
            setActiveSuggestionIndex(prev => (prev - 1 + suggestions.length) % suggestions.length);
        } else if (e.key === 'Enter') {
            if (activeSuggestionIndex > -1) {
                e.preventDefault();
                handleSuggestionClick(suggestions[activeSuggestionIndex]);
            }
        } else if (e.key === 'Escape') {
            setIsSuggestionsVisible(false);
        }
    };


    const handleSearch = (e: React.FormEvent) => {
        e.preventDefault();
        setSelectedHistoryIndex(null);
        handleSearchStock(filters);
        setIsSuggestionsVisible(false);
    };

    const handleRebuildClick = async () => {
        const confirmed = await confirm({
            title: 'Rebuild Stock Data',
            message: 'This will clear all existing stock information and recreate it from the log entries in the current session. This process can be slow and the session will be saved automatically.',
            confirmText: 'Rebuild',
            type: 'warning',
        });
        if (confirmed) {
            handleRebuildStockData();
        }
    };

    const handleHistoryTableKeyDown = (e: React.KeyboardEvent) => {
        if (!['ArrowUp', 'ArrowDown'].includes(e.key)) {
            return;
        }
        e.preventDefault();

        if (stockHistory.length === 0) return;

        const currentIndex = selectedHistoryIndex;
        let nextIndex: number;

        if (currentIndex === null) {
            nextIndex = e.key === 'ArrowUp' ? stockHistory.length - 1 : 0;
        } else {
            if (e.key === 'ArrowUp') {
                nextIndex = Math.max(0, currentIndex - 1);
            } else { // ArrowDown
                nextIndex = Math.min(stockHistory.length - 1, currentIndex + 1);
            }
        }
        setSelectedHistoryIndex(nextIndex);
    };

    const handleRowClick = (index: number) => {
        setSelectedHistoryIndex(index);
        tableContainerRef.current?.focus({ preventScroll: true });
    };

    const chartData = React.useMemo(() => {
        if (!stockHistory || stockHistory.length === 0) return [];
        return stockHistory.map(entry => ({
            time: entry.timestamp.replace(' ', 'T') + 'Z',
            quantity: entry.quantity,
        }));
    }, [stockHistory]);

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

    // Sync selection to TimelineContext
    React.useEffect(() => {
        setSelection(selectedStartTime, selectedEndTime);
    }, [selectedStartTime, selectedEndTime, setSelection]);

    // Handle Timeline Zoom is now managed by Context + TimeRangeSelector internally.
    // We don't need local handleTimelineZoomToSelection.



    const zoomToSelectionEnabled = React.useMemo(() => {
        return selectedStartTime !== null && selectedEndTime !== null && selectedStartTime < selectedEndTime;
    }, [selectedStartTime, selectedEndTime]);

    return (
        <div className="flex-grow p-4 sm:p-6 lg:p-8 overflow-y-auto space-y-6 bg-gray-100 dark:bg-transparent">
            <div className="bg-white dark:bg-gray-800/50 p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
                <div className="flex justify-between items-start mb-4">
                    <div>
                        <h2 className="text-xl font-semibold text-gray-800 dark:text-gray-200">Search Article Stock</h2>
                    </div>
                    <button
                        onClick={handleRebuildClick}
                        disabled={isStockBusy}
                        className="inline-flex items-center justify-center gap-2 px-3 py-2 text-sm font-semibold rounded-lg transition-colors duration-200 bg-amber-500 hover:bg-amber-600 text-white dark:bg-amber-600 dark:hover:bg-amber-500 disabled:opacity-50 disabled:cursor-not-allowed"
                        title="Clear and rebuild all stock data from the main log entries in this session."
                    >
                        <Icon name="ArrowPath" iconSet={iconSet} className="w-5 h-5" />
                        <span>Rebuild Stock Data</span>
                    </button>
                </div>
                <form onSubmit={handleSearch} className="space-y-4">
                    <div className="grid grid-cols-1 md:grid-cols-10 gap-4 items-end">
                        <div className="md:col-span-3">
                            <label htmlFor="dateFromStock" className="block text-xs font-medium text-gray-500 dark:text-gray-400 mb-1">From</label>
                            <div className="flex gap-2">
                                <input type="date" name="dateFrom" id="dateFromStock" value={filters.dateFrom} onChange={handleInputChange} className={inputStyles} />
                                <input type="time" name="timeFrom" id="timeFromStock" value={filters.timeFrom} onChange={handleInputChange} className={inputStyles} step="1" />
                            </div>
                        </div>
                        <div className="md:col-span-3">
                            <label htmlFor="dateToStock" className="block text-xs font-medium text-gray-500 dark:text-gray-400 mb-1">To</label>
                            <div className="flex gap-2">
                                <input type="date" name="dateTo" id="dateToStock" value={filters.dateTo} onChange={handleInputChange} className={inputStyles} />
                                <input type="time" name="timeTo" id="timeToStock" value={filters.timeTo} onChange={handleInputChange} className={inputStyles} step="1" />
                            </div>
                        </div>
                        <div className="md:col-span-3" ref={suggestionContainerRef}>
                            <label htmlFor="searchTerm" className="block text-xs font-medium text-gray-500 dark:text-gray-400 mb-1">Article ID or Name</label>
                            <div className="relative">
                                <input
                                    type="text"
                                    name="searchTerm"
                                    id="searchTerm"
                                    value={filters.searchTerm}
                                    onChange={handleInputChange}
                                    onKeyDown={handleSuggestionKeyDown}
                                    onFocus={() => { if (suggestions.length > 0) setIsSuggestionsVisible(true); }}
                                    className={inputStyles}
                                    placeholder="e.g., CARTEOL (min 2 chars)"
                                    autoComplete="off"
                                />
                                {isSuggestionsVisible && suggestions.length > 0 && (
                                    <ul className="absolute z-10 w-full mt-1 bg-white dark:bg-gray-800 border border-gray-300 dark:border-gray-600 rounded-md shadow-lg max-h-60 overflow-auto">
                                        {suggestions.map((suggestion, index) => (
                                            <li
                                                key={`${suggestion.id}-${suggestion.name}`}
                                                className={`px-3 py-2 cursor-pointer hover:bg-sky-100 dark:hover:bg-sky-900/50 ${index === activeSuggestionIndex ? 'bg-sky-100 dark:bg-sky-900/50' : ''}`}
                                                onMouseDown={() => handleSuggestionClick(suggestion)}
                                            >
                                                <div className="font-semibold text-gray-900 dark:text-gray-100">{suggestion.name}</div>
                                                <div className="text-sm text-gray-500 dark:text-gray-400 font-mono">{suggestion.id}</div>
                                            </li>
                                        ))}
                                    </ul>
                                )}
                            </div>
                        </div>
                        <button
                            type="submit"
                            disabled={isStockBusy || !filters.searchTerm}
                            className="md:col-span-1 h-10 inline-flex items-center justify-center gap-2 px-4 py-2 font-semibold rounded-lg transition-colors duration-200 bg-sky-600 hover:bg-sky-700 text-white dark:bg-sky-600 dark:hover:bg-sky-500 disabled:opacity-50 disabled:cursor-not-allowed"
                        >
                            <Icon name="Filter" iconSet={iconSet} className="w-5 h-5" />
                            <span>{isStockBusy ? '...' : 'Search'}</span>
                        </button>
                    </div>

                    <div className="pt-4 mt-4 border-t border-gray-200 dark:border-gray-700">
                        {overallStockTimeRange ? (
                            <TimeRangeSelector
                                minTime={new Date(overallStockTimeRange.min + 'Z').getTime()}
                                maxTime={new Date(overallStockTimeRange.max + 'Z').getTime()}
                                selectedStartTime={selectedStartTime}
                                selectedEndTime={selectedEndTime}
                                onRangeChange={handleTimeRangeChange}
                                onClear={handleTimeRangeClear}
                                theme={theme}
                                pageTimestampRanges={[]}
                                fileTimeRanges={[]}
                                logDensity={overallStockDensity}
                                overallLogDensity={overallStockDensity}
                                datesWithLogs={[]}
                                onCursorChange={() => { }}
                                onFileSelect={() => { }}
                                onDateSelect={() => { }}

                                zoomToSelectionEnabled={zoomToSelectionEnabled}
                                iconSet={iconSet}
                                uiScale={uiScale}
                                timelineBarVisibility={timelineBarVisibility}
                                onTimelineBarVisibilityChange={onTimelineBarVisibilityChange}
                            />
                        ) : (
                            <div className="h-40 flex items-center justify-center bg-gray-200 dark:bg-gray-700/50 rounded-lg text-sm text-gray-500 dark:text-gray-400 text-center p-4">
                                The timeline selector will appear here once log files containing stock data have been loaded.
                            </div>
                        )}
                    </div>
                </form>
            </div>

            {stockHistory.length > 0 && (
                <div className="grid grid-cols-1 xl:grid-cols-2 gap-6">
                    <div className="bg-white dark:bg-gray-800/50 p-4 sm:p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
                        <h3 className="text-lg font-semibold text-gray-800 dark:text-gray-200 mb-4">Stock History</h3>
                        <div
                            ref={tableContainerRef}
                            onKeyDown={handleHistoryTableKeyDown}
                            tabIndex={0}
                            className="overflow-y-auto max-h-96 focus:outline-none rounded-md"
                        >
                            <table className="min-w-full divide-y divide-gray-200 dark:divide-gray-700">
                                <thead className="bg-gray-50 dark:bg-gray-800/50 sticky top-0">
                                    <tr>
                                        <th className="px-3 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">Timestamp</th>
                                        <th className="px-3 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">Article ID</th>
                                        <th className="px-3 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">Article Name</th>
                                        <th className="px-3 py-2 text-right text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">Quantity</th>
                                    </tr>
                                </thead>
                                <tbody className="bg-white dark:bg-gray-900 divide-y divide-gray-200/50 dark:divide-gray-700/50">
                                    {stockHistory.map((entry, index) => (
                                        <tr
                                            key={`${entry.timestamp}-${index}`}
                                            ref={el => { rowRefs.current.set(index, el); }}
                                            onClick={() => handleRowClick(index)}
                                            className={`cursor-pointer transition-colors duration-150 ${selectedHistoryIndex === index ? 'bg-sky-100 dark:bg-sky-900/50' : 'hover:bg-gray-50 dark:hover:bg-gray-800/50'}`}
                                        >
                                            <td className="px-3 py-2 whitespace-nowrap text-sm text-gray-500 dark:text-gray-400 font-mono">{entry.timestamp}</td>
                                            <td className="px-3 py-2 whitespace-nowrap text-sm text-gray-500 dark:text-gray-400 font-mono">{entry.article_id}</td>
                                            <td className="px-3 py-2 whitespace-nowrap text-sm text-gray-800 dark:text-gray-200">{entry.article_name}</td>
                                            <td className="px-3 py-2 whitespace-nowrap text-sm text-right text-gray-800 dark:text-gray-200 font-semibold">{Math.round(entry.quantity).toLocaleString()}</td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        </div>
                    </div>
                    <div className="bg-white dark:bg-gray-800/50 p-4 sm:p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
                        <h3 className="text-lg font-semibold text-gray-800 dark:text-gray-200 mb-4">Quantity Over Time</h3>
                        <div className="h-96">
                            <StockHistoryChart data={chartData} theme={theme} selectedIndex={selectedHistoryIndex} />
                        </div>
                    </div>
                </div>
            )}

            {!isStockBusy && stockHistory.length === 0 && (
                <div className="text-center py-10">
                    <Icon name="Cube" iconSet={iconSet} className="w-16 h-16 text-gray-300 dark:text-gray-600 mx-auto mb-4" />
                    <h3 className="text-xl font-semibold mb-2 text-gray-700 dark:text-gray-300">No Stock Data</h3>
                    <p className="text-gray-500 dark:text-gray-400">Perform a search to view stock history for an article.</p>
                </div>
            )}
        </div>
    );
};