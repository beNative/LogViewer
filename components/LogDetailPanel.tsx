import React from 'react';
import { LogEntry, ColumnStyles, ColumnKey, GridData, IconSet } from '../types';
import { getLevelColor } from './tableUtils';
import { highlightText } from '../utils';
import { XmlTreeView } from './XmlTreeView';
import { KeyValueTableView } from './KeyValueTableView';
import { SqlSyntaxHighlighter } from './SqlSyntaxHighlighter';
import { parseLogMessage } from '../parsers';
import { GridView } from './GridView';
import { Icon } from './icons';
import { Tooltip } from './Tooltip';

type Theme = 'light' | 'dark';

interface LogDetailPanelProps {
    entry: LogEntry | null;
    onClose: () => void;
    width: number;
    highlightTerms?: string[];
    theme: Theme;
    onApplyFilter: (key: 'level' | 'sndrtype' | 'sndrname' | 'fileName', value: string) => void;
    columnStyles: ColumnStyles;
    iconSet: IconSet;
    selectedEntries?: LogEntry[];
}

const DetailRow: React.FC<{
    label: string;
    value: React.ReactNode;
    valueClassName?: string;
    valueStyle?: React.CSSProperties;
    filterKey?: 'level' | 'sndrtype' | 'sndrname' | 'fileName';
    filterValue?: string;
    onApplyFilter?: (key: 'level' | 'sndrtype' | 'sndrname' | 'fileName', value: string) => void;
    iconSet: IconSet;
}> = ({ label, value, valueClassName = 'text-gray-800 dark:text-gray-200', valueStyle, filterKey, filterValue, onApplyFilter, iconSet }) => (
    <div className="group flex flex-col sm:flex-row sm:items-start py-2.5 border-b border-gray-200/80 dark:border-gray-700/50 relative">
        <dt className="w-28 text-sm font-medium text-gray-500 dark:text-gray-400 flex-shrink-0">{label}</dt>
        <dd style={valueStyle} className={`mt-1 text-sm sm:mt-0 break-words w-full ${valueClassName}`}>{value}</dd>
        {filterKey && filterValue && onApplyFilter && (
            <Tooltip content={`Add filter for ${label}: ${filterValue}`}>
                <button
                    onClick={() => onApplyFilter(filterKey, filterValue)}
                    className="absolute top-1/2 -translate-y-1/2 right-0 p-1 text-gray-400 dark:text-gray-500 rounded-full hover:bg-gray-200 dark:hover:bg-gray-700 hover:text-sky-500 dark:hover:text-sky-400 opacity-0 group-hover:opacity-100 focus:outline-none transition-opacity"
                    aria-label={`Add filter for ${label}: ${filterValue}`}
                >
                    <Icon name="PlusCircle" iconSet={iconSet} className="w-5 h-5" />
                </button>
            </Tooltip>
        )}
    </div>
);

export const LogDetailPanel: React.FC<LogDetailPanelProps> = ({ entry, onClose, width, highlightTerms = [], theme, onApplyFilter, columnStyles, iconSet, selectedEntries = [] }) => {
    const [activeTab, setActiveTab] = React.useState<'parsed' | 'raw' | 'multi'>('parsed');
    const [copiedState, setCopiedState] = React.useState<'parsed' | 'raw' | 'multi' | null>(null);

    const parsedContent = React.useMemo(() => {
        if (!entry) return null;
        return parseLogMessage(entry.msg);
    }, [entry]);

    React.useEffect(() => {
        // When the entry changes, reset the tab to the default and clear copy feedback
        setActiveTab('parsed');
        setCopiedState(null);
    }, [entry?.id]);

    const getCopyContent = (type: 'parsed' | 'raw'): string => {
        if (type === 'raw' || !entry || !parsedContent) {
            return entry?.msg || '';
        }

        let parsedString = '';
        switch (parsedContent.type) {
            case 'text':
            case 'xml':
                parsedString = parsedContent.data as string;
                break;
            case 'sql':
                const sqlData = parsedContent.data as { sql: string, result: string | null };
                parsedString = sqlData.result ? `${sqlData.sql}\n\n-- Result --\n${sqlData.result}` : sqlData.sql;
                break;
            case 'kv':
                parsedString = (parsedContent.data as { key: string; value: string }[])
                    .map(pair => `${pair.key}=${pair.value}`)
                    .join('\n');
                break;
            case 'grid':
                const gridData = parsedContent.data as GridData;
                const header = gridData.headers.join('\t');
                const rows = gridData.rows.map(row => row.join('\t')).join('\n');
                parsedString = `${header}\n${rows}`;
                break;
            default:
                // Fallback for unknown parsed types
                parsedString = entry.msg;
                break;
        }

        // Prepend the prefix if it exists
        if (parsedContent.prefix) {
            return `${parsedContent.prefix}\n\n${parsedString}`;
        }
        return parsedString;
    };

    const handleCopy = (type: 'parsed' | 'raw') => {
        const content = getCopyContent(type);
        navigator.clipboard.writeText(content);
        setCopiedState(type);
        setTimeout(() => setCopiedState(null), 2000);
    };

    const TabButton: React.FC<{ label: string; tabName: 'parsed' | 'raw'; }> = ({ label, tabName }) => {
        const isActive = activeTab === tabName;
        return (
            <button
                onClick={() => setActiveTab(tabName)}
                className={`px-3 py-1.5 text-sm font-semibold rounded-md transition-colors ${isActive
                    ? 'bg-sky-100 dark:bg-sky-900/50 text-sky-700 dark:text-sky-300'
                    : 'text-gray-500 dark:text-gray-400 hover:bg-gray-200/60 dark:hover:bg-gray-700/60'
                    }`}
            >
                {label}
            </button>
        );
    };

    const CopyButton: React.FC<{ type: 'parsed' | 'raw' }> = ({ type }) => {
        const hasCopied = copiedState === type;
        return (
            <Tooltip content={hasCopied ? 'Copied!' : 'Copy to clipboard'}>
                <button
                    onClick={() => handleCopy(type)}
                    className="p-1.5 text-gray-400 dark:text-gray-500 rounded-full hover:bg-gray-200 dark:hover:bg-gray-700 hover:text-sky-500 dark:hover:text-sky-400 focus:outline-none transition-all"
                >
                    <Icon name={hasCopied ? "CheckCircle" : "ClipboardDocument"} iconSet={iconSet} className={`w-5 h-5 ${hasCopied ? 'text-green-500' : ''}`} />
                </button>
            </Tooltip>
        );
    };

    const getStyle = (key: ColumnKey): React.CSSProperties => {
        const styleConf = columnStyles[key];
        if (!styleConf) return {};

        const properties: React.CSSProperties = {
            fontFamily: styleConf.font || 'inherit',
            fontSize: `${styleConf.fontSize}px`,
            fontWeight: styleConf.isBold ? 'bold' : 'normal',
            fontStyle: styleConf.isItalic ? 'italic' : 'normal',
        };

        const color = theme === 'dark' ? styleConf.darkColor : styleConf.color;

        if (color) {
            properties.color = color;
        }

        return properties;
    };

    // Multi-select logic
    const isMultiSelect = selectedEntries.length > 1;

    React.useEffect(() => {
        if (isMultiSelect) {
            setActiveTab('multi');
        } else {
            setActiveTab('parsed');
        }
    }, [isMultiSelect, entry?.id]);

    // Calculate stats
    const stats = React.useMemo(() => {
        if (!isMultiSelect) return null;

        const count = selectedEntries.length;
        const times = selectedEntries.map(e => new Date(e.time).getTime()).filter(t => !isNaN(t)).sort((a, b) => a - b);
        const startTime = times.length > 0 ? new Date(times[0]).toISOString() : '-';
        const endTime = times.length > 0 ? new Date(times[times.length - 1]).toISOString() : '-';

        let durationStr = '-';
        let timeDeltaStr = '-'; // Specific for exactly 2 items

        if (times.length > 1) {
            const diffMs = times[times.length - 1] - times[0];
            durationStr = `${diffMs}ms`;
            if (diffMs > 1000) durationStr += ` (${(diffMs / 1000).toFixed(2)}s)`;
        }

        if (selectedEntries.length === 2) {
            const diffMs = Math.abs(times[1] - times[0]);
            timeDeltaStr = `${diffMs}ms`;
            if (diffMs > 1000) timeDeltaStr += ` (${(diffMs / 1000).toFixed(2)}s)`;
        }

        const levels: Record<string, number> = {};
        selectedEntries.forEach(e => {
            levels[e.level] = (levels[e.level] || 0) + 1;
        });

        return { count, startTime, endTime, duration: durationStr, levels, timeDelta: timeDeltaStr };
    }, [selectedEntries, isMultiSelect]);

    const handleCopyAllMessages = () => {
        const allMessages = selectedEntries.map(e => e.msg).join('\n');
        navigator.clipboard.writeText(allMessages);
        setCopiedState('multi');
        setTimeout(() => setCopiedState(null), 2000);
    };

    // ... (existing helper functions)

    if (isMultiSelect && stats) {
        return (
            <aside
                style={{ width: `${width}px` }}
                className="flex-shrink-0 bg-white dark:bg-gray-800/60 border-l border-gray-200 dark:border-gray-700 flex flex-col"
            >
                <div className="flex items-center justify-between p-4 border-b border-gray-200 dark:border-gray-700 flex-shrink-0">
                    <h3 className="text-lg font-semibold text-gray-900 dark:text-white">Selection Details</h3>
                    <button
                        onClick={onClose}
                        className="p-1 text-gray-500 dark:text-gray-400 rounded-md hover:bg-gray-200 dark:hover:bg-gray-700 hover:text-gray-800 dark:hover:text-white transition-colors"
                        aria-label="Close details"
                    >
                        <Icon name="XMark" iconSet={iconSet} className="w-6 h-6" />
                    </button>
                </div>
                <div className="flex-grow p-4 overflow-y-auto space-y-6">
                    {/* Summary Stats */}
                    <div className="bg-gray-50 dark:bg-gray-800/50 rounded-lg p-4 ring-1 ring-gray-200 dark:ring-gray-700">
                        <h4 className="text-sm font-semibold text-gray-900 dark:text-white mb-3 flex items-center gap-2">
                            <Icon name="ChartBar" iconSet={iconSet} className="w-4 h-4 text-sky-500" />
                            Summary
                        </h4>
                        <dl className="grid grid-cols-2 gap-x-4 gap-y-4">
                            <div>
                                <dt className="text-xs font-medium text-gray-500 dark:text-gray-400">Count</dt>
                                <dd className="mt-1 text-sm font-semibold text-gray-900 dark:text-white">{stats.count}</dd>
                            </div>
                            <div>
                                <dt className="text-xs font-medium text-gray-500 dark:text-gray-400">Duration</dt>
                                <dd className="mt-1 text-sm text-gray-900 dark:text-white">{stats.duration}</dd>
                            </div>
                            <div className="col-span-2">
                                <dt className="text-xs font-medium text-gray-500 dark:text-gray-400">Start Time</dt>
                                <dd className="mt-1 text-xs font-mono text-gray-700 dark:text-gray-300 truncate">{stats.startTime}</dd>
                            </div>
                            <div className="col-span-2">
                                <dt className="text-xs font-medium text-gray-500 dark:text-gray-400">End Time</dt>
                                <dd className="mt-1 text-xs font-mono text-gray-700 dark:text-gray-300 truncate">{stats.endTime}</dd>
                            </div>
                        </dl>
                    </div>

                    {/* Time Delta (Only for 2 items) */}
                    {selectedEntries.length === 2 && (
                        <div className="bg-blue-50 dark:bg-blue-900/20 rounded-lg p-4 ring-1 ring-blue-200 dark:ring-blue-800/50">
                            <h4 className="text-sm font-semibold text-blue-900 dark:text-blue-100 mb-2 flex items-center gap-2">
                                <Icon name="Clock" iconSet={iconSet} className="w-4 h-4 text-blue-500" />
                                Time Difference
                            </h4>
                            <div className="text-2xl font-bold text-blue-700 dark:text-blue-300">
                                {stats.timeDelta}
                            </div>
                        </div>
                    )}

                    {/* Level Breakdown */}
                    <div>
                        <h4 className="text-xs font-semibold text-gray-500 dark:text-gray-400 mb-2 uppercase tracking-wider">Level Breakdown</h4>
                        <div className="space-y-2">
                            {Object.entries(stats.levels).sort((a, b) => b[1] - a[1]).map(([level, count]) => (
                                <div key={level} className="flex items-center justify-between text-sm">
                                    <span className={`px-2 py-0.5 rounded-full text-xs font-medium ${getLevelColor(level)}`}>{level}</span>
                                    <span className="text-gray-600 dark:text-gray-400 font-mono">{count}</span>
                                </div>
                            ))}
                        </div>
                    </div>

                    {/* Actions */}
                    <div>
                        <h4 className="text-xs font-semibold text-gray-500 dark:text-gray-400 mb-2 uppercase tracking-wider">Actions</h4>
                        <button
                            onClick={handleCopyAllMessages}
                            className="w-full flex items-center justify-center gap-2 px-4 py-2 bg-white dark:bg-gray-700 border border-gray-300 dark:border-gray-600 rounded-md shadow-sm text-sm font-medium text-gray-700 dark:text-gray-200 hover:bg-gray-50 dark:hover:bg-gray-600 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-sky-500 transition-colors"
                        >
                            <Icon name={copiedState === 'multi' ? "Check" : "ClipboardDocument"} iconSet={iconSet} className={`w-4 h-4 ${copiedState === 'multi' ? 'text-green-500' : ''}`} />
                            {copiedState === 'multi' ? 'Copied All to Clipboard' : 'Copy All Messages'}
                        </button>
                    </div>
                </div>
            </aside>
        );
    }

    return (
        <aside
            style={{ width: `${width}px` }}
            className="flex-shrink-0 bg-white dark:bg-gray-800/60 border-l border-gray-200 dark:border-gray-700 flex flex-col"
        >
            {/* ... (Original return content for single entry) */}
            <div className="flex items-center justify-between p-4 border-b border-gray-200 dark:border-gray-700 flex-shrink-0">
                <h3 className="text-lg font-semibold text-gray-900 dark:text-white">Log Details</h3>
                <button
                    onClick={onClose}
                    className="p-1 text-gray-500 dark:text-gray-400 rounded-md hover:bg-gray-200 dark:hover:bg-gray-700 hover:text-gray-800 dark:hover:text-white transition-colors"
                    aria-label="Close details"
                >
                    <Icon name="XMark" iconSet={iconSet} className="w-6 h-6" />
                </button>
            </div>
            <div className="flex-grow p-4 overflow-y-auto">
                {entry && parsedContent ? (
                    // ... (rest of the original single entry view)

                    <>
                        <dl>
                            <DetailRow iconSet={iconSet} label="Timestamp" value={entry.time} valueStyle={getStyle('time')} valueClassName="text-gray-600 dark:text-gray-300" />
                            <DetailRow
                                iconSet={iconSet}
                                label="Level"
                                value={<span style={getStyle('level')} className={`px-2 py-1 rounded-full text-xs font-medium ${getLevelColor(entry.level)}`}>{entry.level}</span>}
                                filterKey="level"
                                filterValue={entry.level}
                                onApplyFilter={onApplyFilter}
                            />
                            <DetailRow
                                iconSet={iconSet}
                                label="Sender Type"
                                value={entry.sndrtype}
                                valueStyle={getStyle('sndrtype')}
                                filterKey="sndrtype"
                                filterValue={entry.sndrtype}
                                onApplyFilter={onApplyFilter}
                            />
                            <DetailRow
                                iconSet={iconSet}
                                label="Sender Name"
                                value={entry.sndrname}
                                valueStyle={getStyle('sndrname')}
                                filterKey="sndrname"
                                filterValue={entry.sndrname}
                                onApplyFilter={onApplyFilter}
                            />
                            <DetailRow
                                iconSet={iconSet}
                                label="Filename"
                                value={entry.fileName}
                                valueStyle={getStyle('fileName')}
                                valueClassName="text-gray-500 dark:text-gray-400"
                                filterKey="fileName"
                                filterValue={entry.fileName}
                                onApplyFilter={onApplyFilter}
                            />
                        </dl>
                        <div className="mt-4">
                            <label className="block text-sm font-medium text-gray-500 dark:text-gray-400 mb-2">Message</label>

                            <div className="flex items-center gap-2 border-b border-gray-200 dark:border-gray-700 mb-3">
                                <TabButton label="Parsed" tabName="parsed" />
                                <TabButton label="Raw" tabName="raw" />
                            </div>

                            {activeTab === 'parsed' && (
                                <div className="space-y-3">
                                    {parsedContent.prefix && (
                                        <div className="bg-gray-100 dark:bg-gray-900/70 rounded-lg p-3 ring-1 ring-gray-200 dark:ring-gray-700">
                                            <h4 className="text-xs font-semibold text-gray-500 dark:text-gray-500 mb-1.5 uppercase tracking-wider">Message Prefix</h4>
                                            <pre className="text-sm text-gray-800 dark:text-gray-200 whitespace-pre-wrap break-all font-mono">
                                                <code dangerouslySetInnerHTML={{ __html: highlightText(parsedContent.prefix, highlightTerms, theme) }} />
                                            </pre>
                                        </div>
                                    )}
                                    <div className="relative group/parsed">
                                        <div className="absolute top-1 right-1 z-10 opacity-0 group-hover/parsed:opacity-100 transition-opacity">
                                            <CopyButton type="parsed" />
                                        </div>
                                        {parsedContent.type === 'grid' && <GridView data={parsedContent.data as GridData} />}
                                        {parsedContent.type === 'kv' && <KeyValueTableView data={parsedContent.data as { key: string, value: string }[]} />}
                                        {parsedContent.type === 'xml' && <XmlTreeView xmlString={parsedContent.data as string} />}
                                        {parsedContent.type === 'sql' && (
                                            <>
                                                <SqlSyntaxHighlighter sql={(parsedContent.data as { sql: string }).sql} />
                                                {(parsedContent.data as { result: string | null }).result && (
                                                    <div className="mt-2 bg-green-50 dark:bg-green-900/40 rounded-lg p-3 ring-1 ring-green-200 dark:ring-green-700/50">
                                                        <h4 className="text-xs font-semibold text-green-700 dark:text-green-400 mb-1.5 uppercase tracking-wider">Query Result</h4>
                                                        <pre className="text-sm text-green-800 dark:text-green-200 whitespace-pre-wrap break-all font-mono">
                                                            <code>{(parsedContent.data as { result: string }).result}</code>
                                                        </pre>
                                                    </div>
                                                )}
                                            </>
                                        )}
                                        {parsedContent.type === 'text' && (
                                            <div className="bg-gray-100 dark:bg-gray-900/70 rounded-lg p-3 ring-1 ring-gray-200 dark:ring-gray-700">
                                                <pre className="text-sm text-gray-800 dark:text-gray-200 whitespace-pre-wrap break-all font-mono">
                                                    <code dangerouslySetInnerHTML={{ __html: highlightText(parsedContent.data as string, highlightTerms, theme) }} />
                                                </pre>
                                            </div>
                                        )}
                                    </div>
                                </div>
                            )}

                            {activeTab === 'raw' && (
                                <div className="relative group/raw bg-gray-100 dark:bg-gray-900/70 rounded-lg p-3 ring-1 ring-gray-200 dark:ring-gray-700">
                                    <div className="absolute top-1 right-1 z-10 opacity-0 group-hover/raw:opacity-100 transition-opacity">
                                        <CopyButton type="raw" />
                                    </div>
                                    <pre className="text-sm text-gray-800 dark:text-gray-200 whitespace-pre-wrap break-all font-mono">
                                        <code>{entry.msg}</code>
                                    </pre>
                                </div>
                            )}
                        </div>
                    </>
                ) : (
                    <div className="flex flex-col items-center justify-center h-full text-center text-gray-500 dark:text-gray-400">
                        <Icon name="InformationCircle" iconSet={iconSet} className="w-12 h-12 text-gray-400 dark:text-gray-500 mb-4" />
                        <h4 className="text-lg font-semibold text-gray-700 dark:text-gray-300">No Selection</h4>
                        <p className="mt-1 text-sm">Click on a log entry in the table to see its details here.</p>
                    </div>
                )}
            </div>
        </aside>
    );
};
