import React from 'react';
import { LogEntry, ColumnStyles, ColumnKey, GridData, IconSet } from '../types';
import { getLevelColor } from './LogTable';
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

export const LogDetailPanel: React.FC<LogDetailPanelProps> = ({ entry, onClose, width, highlightTerms = [], theme, onApplyFilter, columnStyles, iconSet }) => {
    
    const [activeTab, setActiveTab] = React.useState<'parsed' | 'raw'>('parsed');
    const [copiedState, setCopiedState] = React.useState<'parsed' | 'raw' | null>(null);

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

        switch (parsedContent.type) {
            case 'text':
            case 'xml':
                return parsedContent.data as string;
            case 'sql':
                return (parsedContent.data as { sql: string }).sql;
            case 'kv':
                return (parsedContent.data as { key: string; value: string }[])
                    .map(pair => `${pair.key}=${pair.value}`)
                    .join('\n');
            case 'grid':
                const gridData = parsedContent.data as GridData;
                const header = gridData.headers.join('\t');
                const rows = gridData.rows.map(row => row.join('\t')).join('\n');
                return `${header}\n${rows}`;
            default:
                return entry.msg;
        }
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
                className={`px-3 py-1.5 text-sm font-semibold rounded-md transition-colors ${
                    isActive 
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

    return (
        <aside 
            style={{ width: `${width}px` }}
            className="flex-shrink-0 bg-white dark:bg-gray-800/60 border-l border-gray-200 dark:border-gray-700 flex flex-col"
        >
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
                            
                            {parsedContent.prefix && (
                                <div className="bg-gray-100 dark:bg-gray-900/70 rounded-lg p-3 ring-1 ring-gray-200 dark:ring-gray-700 mb-2">
                                    <h4 className="text-xs font-semibold text-gray-500 dark:text-gray-500 mb-1.5 uppercase tracking-wider">Message Prefix</h4>
                                    <pre className="text-sm text-gray-800 dark:text-gray-200 whitespace-pre-wrap break-all font-mono">
                                        <code dangerouslySetInnerHTML={{ __html: highlightText(parsedContent.prefix, highlightTerms, theme) }} />
                                    </pre>
                                </div>
                            )}

                            <div className="flex items-center gap-2 border-b border-gray-200 dark:border-gray-700 mb-3">
                                <TabButton label="Parsed" tabName="parsed" />
                                <TabButton label="Raw" tabName="raw" />
                            </div>

                            {activeTab === 'parsed' && (
                                <div className="relative group/parsed">
                                    <div className="absolute top-1 right-1 z-10 opacity-0 group-hover/parsed:opacity-100 transition-opacity">
                                        <CopyButton type="parsed" />
                                    </div>
                                    {parsedContent.type === 'grid' && <GridView data={parsedContent.data as GridData} />}
                                    {parsedContent.type === 'kv' && <KeyValueTableView data={parsedContent.data as {key: string, value: string}[]} />}
                                    {parsedContent.type === 'xml' && <XmlTreeView xmlString={parsedContent.data as string} />}
                                    {parsedContent.type === 'sql' && (
                                        <>
                                            <SqlSyntaxHighlighter sql={(parsedContent.data as {sql: string}).sql} />
                                            {(parsedContent.data as {result: string | null}).result && (
                                                <div className="mt-2 bg-green-50 dark:bg-green-900/40 rounded-lg p-3 ring-1 ring-green-200 dark:ring-green-700/50">
                                                    <h4 className="text-xs font-semibold text-green-700 dark:text-green-400 mb-1.5 uppercase tracking-wider">Query Result</h4>
                                                    <pre className="text-sm text-green-800 dark:text-green-200 whitespace-pre-wrap break-all font-mono">
                                                        <code>{(parsedContent.data as {result: string}).result}</code>
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