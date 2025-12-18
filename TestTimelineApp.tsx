import React, { useState } from 'react';
import ReactDOM from 'react-dom/client';
import { TimelineProvider, useTimeline } from './contexts/TimelineContext';
import { TimeRangeSelector } from './components/TimeRangeSelector';
import './styles.css'; // Tailwind styles
import { Theme } from './utils/timelineUtils';
import { LogDensityPointByLevel, TimelineBarVisibility, FileTimeRange } from './types';

// Mock Data
const MOCK_MIN_TIME = new Date('2024-01-01T00:00:00Z').getTime();
const MOCK_MAX_TIME = new Date('2024-01-01T12:00:00Z').getTime(); // 12 hours
const MOCK_DENSITY = Array.from({ length: 120 }, (_, i) => ({
    time: MOCK_MIN_TIME + i * (3600000 / 10), // Every 6 mins
    counts: { 'INFO': Math.floor(Math.random() * 100), 'ERROR': Math.floor(Math.random() * 10) }
}));

// Pre-calculate mock metadata
const MOCK_DATES = Array.from(new Set(MOCK_DENSITY.map(d => new Date(d.time).toISOString().split('T')[0])));
const MOCK_FILES: FileTimeRange[] = [{
    name: 'simulation.log',
    startTime: new Date(MOCK_MIN_TIME).toISOString(),
    endTime: new Date(MOCK_MAX_TIME).toISOString()
}];

const DebugDisplay = () => {
    const { lastLog, viewRange } = useTimeline();
    return (
        <div className="fixed bottom-4 right-4 bg-red-900/90 text-white p-4 rounded shadow-lg font-mono text-xs z-[9999] border border-red-500 max-w-lg break-all pointer-events-none">
            <h3 className="font-bold border-b border-red-700 mb-1">Timeline Debug Log</h3>
            <div className="mb-2 text-yellow-300">
                View: {viewRange ? `${new Date(viewRange.min).toLocaleTimeString()} - ${new Date(viewRange.max).toLocaleTimeString()}` : 'FULL EXTENT'}
            </div>
            {lastLog || 'No logs yet'}
        </div>
    );
};

const TestTimelineApp = () => {
    const [theme, setTheme] = useState<Theme>('light');
    const [selection, setSelection] = useState<{ start: number, end: number } | null>(null);
    const [logDensity, setLogDensity] = useState<LogDensityPointByLevel[]>(MOCK_DENSITY);
    const [minTime, setMinTime] = useState(MOCK_MIN_TIME);
    const [maxTime, setMaxTime] = useState(MOCK_MAX_TIME);

    // Initialize with mock data so strips are visible by default
    const [fileRanges, setFileRanges] = useState<FileTimeRange[]>(MOCK_FILES);
    const [datesWithLogs, setDatesWithLogs] = useState<string[]>(MOCK_DATES);
    const [isLoading, setIsLoading] = useState(false);

    // Default all to true so they are visible immediately
    const [timelineBarVisibility, setTimelineBarVisibility] = useState<TimelineBarVisibility>({
        overview: true,
        density: true,
        pages: false,
        files: true,
        dates: true
    });

    const handleRangeChange = (start: number, end: number) => {
        setSelection({ start, end });
    };

    const handleFileUpload = (e: React.ChangeEvent<HTMLInputElement>) => {
        const file = e.target.files?.[0];
        if (!file) return;

        setIsLoading(true);
        const reader = new FileReader();
        reader.onload = (event) => {
            try {
                const text = event.target?.result as string;
                // Regex parsers
                const timeRegex = /Time="([^"]+)"/g;
                const levelRegex = /Level="([^"]+)"/g;
                const fileRegex = /File="([^"]+)"/g;

                const times: number[] = [];
                const levels: string[] = [];
                const files: string[] = [];

                // Parse Times
                let timeMatch;
                while ((timeMatch = timeRegex.exec(text)) !== null) {
                    const date = new Date(timeMatch[1]);
                    if (!isNaN(date.getTime())) {
                        times.push(date.getTime());
                    }
                }

                // Parse Levels
                let levelMatch;
                while ((levelMatch = levelRegex.exec(text)) !== null) {
                    levels.push(levelMatch[1]);
                }

                // Parse Files
                let fileMatch;
                while ((fileMatch = fileRegex.exec(text)) !== null) {
                    files.push(fileMatch[1]);
                }

                console.log('Parsed:', { times: times.length, levels: levels.length, files: files.length });

                if (times.length === 0) {
                    alert('No logs found or format incorrect');
                    setIsLoading(false);
                    return;
                }

                const min = Math.min(...times);
                const max = Math.max(...times);

                // 1. Density Calculation (Buckets)
                const numBuckets = 120;
                const interval = (max - min) / numBuckets;
                const newDensity: LogDensityPointByLevel[] = [];

                for (let i = 0; i < numBuckets; i++) {
                    newDensity.push({
                        time: min + i * interval,
                        counts: {}
                    });
                }

                times.forEach((t, index) => {
                    const bucketIndex = Math.floor((t - min) / interval);
                    const safeIndex = Math.min(Math.max(0, bucketIndex), numBuckets - 1);
                    const level = levels[index] || 'INFO';

                    if (!newDensity[safeIndex].counts[level]) newDensity[safeIndex].counts[level] = 0;
                    newDensity[safeIndex].counts[level]++;
                });

                // 2. File Time Ranges
                const fileMap = new Map<string, { start: number, end: number }>();
                const hasFiles = files.length > 0;
                if (hasFiles) {
                    times.forEach((t, index) => {
                        const fName = files[index] || 'Unknown.log';
                        if (!fileMap.has(fName)) {
                            fileMap.set(fName, { start: t, end: t });
                        } else {
                            const current = fileMap.get(fName)!;
                            current.start = Math.min(current.start, t);
                            current.end = Math.max(current.end, t);
                        }
                    });
                }

                const newFileRanges: FileTimeRange[] = Array.from(fileMap.entries()).map(([name, range]) => ({
                    name,
                    startTime: new Date(range.start).toISOString(),
                    endTime: new Date(range.end).toISOString()
                }));

                // 3. Unique Dates
                const uniqueDates = new Set<string>();
                times.forEach(t => {
                    uniqueDates.add(new Date(t).toISOString().split('T')[0]);
                });

                console.log('Derived Data:', {
                    newFileRanges,
                    dateCount: uniqueDates.size,
                    hasFiles
                });

                setLogDensity(newDensity);
                setMinTime(min);
                setMaxTime(max);
                // If files found, use them, else empty (or keep mock?)
                // Actually if user loads file, we should overwrite mocks.
                if (newFileRanges.length > 0) setFileRanges(newFileRanges);
                else setFileRanges([]); // Or keep mock? Let's assume if parsing specific file, we want that file's data.

                setDatesWithLogs(Array.from(uniqueDates));
                setSelection(null);

            } catch (err) {
                console.error(err);
                alert('Error parsing file: ' + err);
            } finally {
                setIsLoading(false);
            }
        };
        reader.readAsText(file);
    };

    const toggleVisibility = (key: keyof TimelineBarVisibility) => {
        setTimelineBarVisibility(prev => ({
            ...prev,
            [key]: !prev[key]
        }));
    };

    return (
        <TimelineProvider>
            <DebugDisplay />
            <div className={`w-full h-screen p-8 flex flex-col gap-8 ${theme === 'dark' ? 'dark bg-gray-900 text-white' : 'bg-gray-50 text-black'}`}>
                <header className="flex flex-col gap-4 border-b pb-4">
                    <div className="flex justify-between items-center">
                        <div>
                            <h1 className="text-2xl font-bold">Timeline Test Harness</h1>
                            <p className="text-sm text-gray-500">Isolated environment for TimeRangeSelector</p>
                        </div>
                        <div className="flex gap-4 items-center">
                            <label className={`cursor-pointer px-4 py-2 bg-indigo-600 hover:bg-indigo-700 text-white rounded transition-colors ${isLoading ? 'opacity-50 cursor-wait' : ''}`}>
                                {isLoading ? 'Parsing...' : 'Load XML Log File'}
                                <input type="file" accept=".xml" onChange={handleFileUpload} disabled={isLoading} className="hidden" />
                            </label>
                            <button onClick={() => setTheme(t => t === 'light' ? 'dark' : 'light')} className="px-4 py-2 border border-gray-300 dark:border-gray-600 rounded hover:bg-gray-100 dark:hover:bg-gray-800 transition-colors">
                                {theme === 'light' ? '🌙 Dark Mode' : '☀️ Light Mode'}
                            </button>
                        </div>
                    </div>

                    {/* Visibility Controls */}
                    <div className="flex gap-4 items-center bg-gray-100 dark:bg-gray-800 p-2 rounded">
                        <span className="text-sm font-semibold text-gray-600 dark:text-gray-300">Visibility:</span>
                        {(['overview', 'density', 'files', 'dates'] as const).map(key => (
                            <label key={key} className="flex items-center space-x-2 cursor-pointer select-none">
                                <input
                                    type="checkbox"
                                    checked={timelineBarVisibility[key]}
                                    onChange={() => toggleVisibility(key)}
                                    className="rounded border-gray-300 text-indigo-600 focus:ring-indigo-500"
                                />
                                <span className="capitalize text-sm">{key}</span>
                            </label>
                        ))}
                    </div>
                </header>

                <div className="flex gap-4 items-center pl-1">
                    <div className="font-mono text-sm bg-gray-100 dark:bg-gray-800 px-3 py-1 rounded">
                        <strong>Selection:</strong> {selection ? `${new Date(selection.start).toLocaleString()} - ${new Date(selection.end).toLocaleString()}` : 'None'}
                    </div>
                    <div className="font-mono text-sm bg-gray-100 dark:bg-gray-800 px-3 py-1 rounded text-xs">
                        Files: {fileRanges.length}, Dates: {datesWithLogs.length}
                    </div>
                </div>

                <div className="border border-gray-200 dark:border-gray-700 p-6 rounded-lg shadow-sm bg-white dark:bg-gray-800/50 backdrop-blur-sm">
                    <TimeRangeSelector
                        minTime={minTime}
                        maxTime={maxTime}
                        selectedStartTime={selection?.start ?? null}
                        selectedEndTime={selection?.end ?? null}
                        onRangeChange={handleRangeChange}
                        onClear={() => setSelection(null)}
                        theme={theme}
                        pageTimestampRanges={[]}
                        fileTimeRanges={fileRanges}
                        logDensity={logDensity}
                        overallLogDensity={logDensity}
                        datesWithLogs={datesWithLogs}

                        onCursorChange={(t) => { }}
                        onFileSelect={() => { }}
                        onDateSelect={() => { }}
                        zoomToSelectionEnabled={true}
                        iconSet="solid"
                        uiScale={1}
                        timelineBarVisibility={timelineBarVisibility}
                        onTimelineBarVisibilityChange={setTimelineBarVisibility}
                    />
                </div>
            </div>
        </TimelineProvider>
    );
};

ReactDOM.createRoot(document.getElementById('root')!).render(
    <React.StrictMode>
        <TestTimelineApp />
    </React.StrictMode>
);
