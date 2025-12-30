import React from 'react';
import { StockInfoEntry, StockInfoFilters, IconSet, Theme, LogDensityPoint, OverallTimeRange, StockArticleSuggestion, TimelineBarVisibility, StockChartDataset } from '../types.ts';
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
    const [selectedArticles, setSelectedArticles] = React.useState<StockArticleSuggestion[]>([]);
    const [availableArticles, setAvailableArticles] = React.useState<StockArticleSuggestion[]>([]);
    const [isArticlesLoading, setIsArticlesLoading] = React.useState(false);
    const [articleSearchTerm, setArticleSearchTerm] = React.useState('');
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

    // Load all available articles on mount (or when time range significantly changes if needed, but for now just load all)
    React.useEffect(() => {
        const loadArticles = async () => {
            setIsArticlesLoading(true);
            const timeFilters: StockInfoFilters = {
                searchTerm: '',
                dateFrom: filters.dateFrom,
                timeFrom: filters.timeFrom,
                dateTo: filters.dateTo,
                timeTo: filters.timeTo,
            };
            // Limit 0 means fetch all
            const articles = await handleFetchStockSuggestions('', timeFilters, 0);
            setAvailableArticles(articles);
            setIsArticlesLoading(false);
        };
        if (overallStockTimeRange) {
            loadArticles();
        }
    }, [overallStockTimeRange, handleFetchStockSuggestions]); // Only re-load when overall range (session) changes



    // Effect to scroll the selected history item into view
    React.useEffect(() => {
        if (selectedHistoryIndex !== null) {
            const rowEl = rowRefs.current.get(selectedHistoryIndex);
            rowEl?.scrollIntoView({ block: 'nearest', behavior: 'smooth' });
        }
    }, [selectedHistoryIndex]);

    const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        const { name, value } = e.target;
        setFilters(prevFilters => ({ ...prevFilters, [name]: value }));
    };

    const toggleArticleSelection = (article: StockArticleSuggestion) => {
        setSelectedArticles(prev => {
            const exists = prev.find(a => a.id === article.id);
            if (exists) {
                return prev.filter(a => a.id !== article.id);
            } else {
                return [...prev, article];
            }
        });
    };

    const handleSelectAll = () => {
        if (selectedArticles.length === availableArticles.length) {
            setSelectedArticles([]);
        } else {
            setSelectedArticles([...availableArticles]);
        }
    };

    const handleSearch = (e: React.FormEvent) => {
        e.preventDefault();
        setSelectedHistoryIndex(null);

        // If specific articles are selected, use those. 
        // If nothing selected but search box has text -> use text.
        // If nothing selected and no text -> search all (or maybe warn? let's search all matching filters)

        const searchFilters: StockInfoFilters = {
            ...filters,
            articleIds: selectedArticles.length > 0 ? selectedArticles.map(a => a.id) : undefined
        };

        // If explicit articles selected, we ignore searchTerm for the query itself to be precise
        if (selectedArticles.length > 0) {
            searchFilters.searchTerm = '';
        }

        handleSearchStock(searchFilters);
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

    const chartDatasets = React.useMemo<StockChartDataset[]>(() => {
        if (!stockHistory || stockHistory.length === 0) return [];

        // Group by Article ID (or Name if preferred, ID is safer)
        const groups = new Map<string, StockChartDataset>();

        stockHistory.forEach(entry => {
            if (!groups.has(entry.article_id)) {
                groups.set(entry.article_id, {
                    id: entry.article_id,
                    label: entry.article_name || entry.article_id,
                    data: []
                });
            }
            const time = entry.timestamp.replace(' ', 'T') + 'Z';
            groups.get(entry.article_id)!.data.push({
                time,
                quantity: entry.quantity
            });
        });

        return Array.from(groups.values());
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
        <div className="flex flex-col h-full p-4 sm:p-6 lg:p-8 overflow-hidden space-y-6 bg-gray-100 dark:bg-transparent">
            <div className="flex-none bg-white dark:bg-gray-800/50 p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
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
                        <div className="md:col-span-3">
                            <label className="block text-xs font-medium text-gray-500 dark:text-gray-400 mb-1">
                                Articles ({selectedArticles.length}/{availableArticles.length})
                            </label>
                            <div className="relative group">
                                <button
                                    type="button"
                                    className={`${inputStyles} text-left flex justify-between items-center cursor-default`}
                                    onClick={() => {
                                        setIsSuggestionsVisible(!isSuggestionsVisible);
                                        // Clear search on close, or keep it? user preference. Let's keep it but focus input.
                                        // If opening, we might want to focus input. We'll do that via autoFocus on input.
                                    }}
                                >
                                    <span className="truncate">
                                        {selectedArticles.length === 0
                                            ? "Select articles..."
                                            : selectedArticles.length === availableArticles.length
                                                ? "All articles selected"
                                                : `${selectedArticles.length} selected`}
                                    </span>
                                    <Icon name="ChevronDown" iconSet={iconSet} className="w-4 h-4 text-gray-500" />
                                </button>

                                {(isSuggestionsVisible) && (
                                    <div className={`absolute z-20 w-full mt-1 bg-white dark:bg-gray-800 border border-gray-300 dark:border-gray-600 rounded-md shadow-lg max-h-[60vh] overflow-hidden flex flex-col transition-all duration-200 origin-top ${isSuggestionsVisible ? 'opacity-100 scale-100' : 'opacity-0 scale-95 pointer-events-none'}`}>
                                        <div className="p-2 border-b border-gray-100 dark:border-gray-700 bg-gray-50 dark:bg-gray-800/80 sticky top-0 z-10 space-y-2">
                                            <input
                                                type="text"
                                                value={articleSearchTerm}
                                                onChange={(e) => setArticleSearchTerm(e.target.value)}
                                                placeholder="Filter articles..."
                                                className="w-full px-2 py-1 text-xs border border-gray-300 dark:border-gray-600 rounded bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100 focus:ring-1 focus:ring-sky-500 focus:border-sky-500"
                                                onClick={(e) => e.stopPropagation()}
                                                autoFocus
                                            />
                                            <div className="flex justify-between items-center">
                                                <button
                                                    type="button"
                                                    onClick={handleSelectAll}
                                                    className="text-xs text-sky-600 dark:text-sky-400 font-medium hover:underline"
                                                >
                                                    {selectedArticles.length === availableArticles.length ? 'Deselect All' : 'Select All'}
                                                </button>
                                                <button
                                                    type="button"
                                                    onClick={() => setIsSuggestionsVisible(false)}
                                                    className="text-xs text-gray-500 hover:text-gray-700 dark:hover:text-gray-300"
                                                >
                                                    Close
                                                </button>
                                            </div>
                                        </div>
                                        <div className="overflow-y-auto p-1">
                                            {isArticlesLoading ? (
                                                <div className="p-3 text-center text-sm text-gray-500">Loading articles...</div>
                                            ) : availableArticles.length === 0 ? (
                                                <div className="p-3 text-center text-sm text-gray-500">No articles found.</div>
                                            ) : (
                                                <ul className="space-y-0.5">
                                                    {availableArticles
                                                        .filter(article => {
                                                            if (!articleSearchTerm) return true;
                                                            const term = articleSearchTerm.toLowerCase();
                                                            return (article.name || '').toLowerCase().includes(term) ||
                                                                (article.id || '').toLowerCase().includes(term);
                                                        })
                                                        .map(article => {
                                                            const isSelected = selectedArticles.some(a => a.id === article.id);
                                                            return (
                                                                <li
                                                                    key={article.id}
                                                                    onClick={() => toggleArticleSelection(article)}
                                                                    className={`px-3 py-2 text-sm rounded cursor-pointer flex items-center gap-2 select-none transition-colors ${isSelected ? 'bg-sky-100 dark:bg-sky-900/40 text-sky-800 dark:text-sky-200' : 'hover:bg-gray-100 dark:hover:bg-gray-700/50 text-gray-700 dark:text-gray-300'}`}
                                                                >
                                                                    <div className={`w-4 h-4 rounded border flex items-center justify-center transition-colors ${isSelected ? 'bg-sky-500 border-sky-500' : 'border-gray-300 dark:border-gray-500 bg-white dark:bg-gray-700'}`}>
                                                                        {isSelected && <Icon name="Check" iconSet={iconSet} className="w-3 h-3 text-white" />}
                                                                    </div>
                                                                    <div className="flex flex-col min-w-0">
                                                                        <span className="font-medium truncate">{article.name}</span>
                                                                        <span className="text-xs opacity-70 truncate font-mono">{article.id}</span>
                                                                    </div>
                                                                </li>
                                                            );
                                                        })}
                                                </ul>
                                            )}
                                        </div>
                                    </div>
                                )}

                                {/* Overlay to close when clicking outside is handled by a fixed inset if open, or simpler click-outside hook provided above but applied differently. 
                                    Re-using the existing click outside effect by attaching ref.
                                */}
                                <div ref={suggestionContainerRef} className="hidden" />
                            </div>
                        </div>
                        <button
                            type="submit"
                            disabled={isStockBusy}
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
                <div className="flex-1 min-h-0 grid grid-cols-1 xl:grid-cols-2 gap-6 pb-2">
                    <div className="bg-white dark:bg-gray-800/50 p-4 sm:p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm flex flex-col h-full">
                        <h3 className="flex-none text-lg font-semibold text-gray-800 dark:text-gray-200 mb-4">Stock History</h3>
                        <div
                            ref={tableContainerRef}
                            onKeyDown={handleHistoryTableKeyDown}
                            tabIndex={0}
                            className="flex-1 min-h-0 overflow-y-auto focus:outline-none rounded-md border border-gray-100 dark:border-gray-700"
                        >
                            <table className="min-w-full divide-y divide-gray-200 dark:divide-gray-700">
                                <thead className="bg-gray-50 dark:bg-gray-800/50 sticky top-0 z-10">
                                    <tr>
                                        <th className="px-3 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider backdrop-blur-sm bg-gray-50/90 dark:bg-gray-800/90">Timestamp</th>
                                        <th className="px-3 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider backdrop-blur-sm bg-gray-50/90 dark:bg-gray-800/90">Article ID</th>
                                        <th className="px-3 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider backdrop-blur-sm bg-gray-50/90 dark:bg-gray-800/90">Article Name</th>
                                        <th className="px-3 py-2 text-right text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider backdrop-blur-sm bg-gray-50/90 dark:bg-gray-800/90">Quantity</th>
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
                    <div className="bg-white dark:bg-gray-800/50 p-4 sm:p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm flex flex-col h-full">
                        <h3 className="flex-none text-lg font-semibold text-gray-800 dark:text-gray-200 mb-4">Quantity Over Time</h3>
                        <div className="flex-1 min-h-0 relative">
                            <div className="absolute inset-0">
                                <StockHistoryChart
                                    datasets={chartDatasets}
                                    theme={theme}
                                    selectedPoint={selectedHistoryIndex !== null ? {
                                        articleId: stockHistory[selectedHistoryIndex].article_id,
                                        timestamp: stockHistory[selectedHistoryIndex].timestamp.replace(' ', 'T') + 'Z'
                                    } : null}
                                />
                            </div>
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