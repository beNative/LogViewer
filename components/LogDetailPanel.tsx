import React from 'react';
import { LogEntry, ColumnStyles, ColumnKey, GridData } from '../types.ts';
import { XMarkIcon } from './icons/XMarkIcon.tsx';
import { getLevelColor } from './LogTable.tsx';
import { highlightText } from '../utils.ts';
import { XmlTreeView } from './XmlTreeView.tsx';
import { KeyValueTableView } from './KeyValueTableView.tsx';
import { SqlSyntaxHighlighter } from './SqlSyntaxHighlighter.tsx';
import { parseLogMessage } from '../parsers.ts';
import { PlusCircleIcon } from './icons/PlusCircleIcon.tsx';
import { GridView } from './GridView.tsx';
import { InformationCircleIcon } from './icons/InformationCircleIcon.tsx';

type Theme = 'light' | 'dark';

interface LogDetailPanelProps {
    entry: LogEntry | null;
    onClose: () => void;
    width: number;
    highlightTerms?: string[];
    theme: Theme;
    onApplyFilter: (key: 'level' | 'sndrtype' | 'sndrname' | 'fileName', value: string) => void;
    columnStyles: ColumnStyles;
}

const DetailRow: React.FC<{
    label: string;
    value: React.ReactNode;
    valueClassName?: string;
    valueStyle?: React.CSSProperties;
    filterKey?: 'level' | 'sndrtype' | 'sndrname' | 'fileName';
    filterValue?: string;
    onApplyFilter?: (key: 'level' | 'sndrtype' | 'sndrname' | 'fileName', value: string) => void;
}> = ({ label, value, valueClassName = 'text-gray-800 dark:text-gray-200', valueStyle, filterKey, filterValue, onApplyFilter }) => (
    <div className="group flex flex-col sm:flex-row sm:items-start py-2.5 border-b border-gray-200/80 dark:border-gray-700/50 relative">
        <dt className="w-28 text-sm font-medium text-gray-500 dark:text-gray-400 flex-shrink-0">{label}</dt>
        <dd style={valueStyle} className={`mt-1 text-sm sm:mt-0 break-words w-full ${valueClassName}`}>{value}</dd>
        {filterKey && filterValue && onApplyFilter && (
            <button
                onClick={() => onApplyFilter(filterKey, filterValue)}
                className="absolute top-1/2 -translate-y-1/2 right-0 p-1 text-gray-400 dark:text-gray-500 rounded-full hover:bg-gray-200 dark:hover:bg-gray-700 hover:text-sky-500 dark:hover:text-sky-400 opacity-0 group-hover:opacity-100 focus:outline-none transition-opacity"
                aria-label={`Add filter for ${label}: ${filterValue}`}
                title={`Add filter for ${label}: ${filterValue}`}
            >
                <PlusCircleIcon className="w-5 h-5" />
            </button>
        )}
    </div>
);

export const LogDetailPanel: React.FC<LogDetailPanelProps> = ({ entry, onClose, width, highlightTerms = [], theme, onApplyFilter, columnStyles }) => {
    
    const parsedContent = React.useMemo(() => {
        if (!entry) return null;
        return parseLogMessage(entry.msg);
    }, [entry]);

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
                    <XMarkIcon className="w-6 h-6" />
                </button>
            </div>
            <div className="flex-grow p-4 overflow-y-auto">
                {entry && parsedContent ? (
                    <>
                        <dl>
                            <DetailRow label="Timestamp" value={entry.time} valueStyle={getStyle('time')} valueClassName="text-gray-600 dark:text-gray-300" />
                            <DetailRow
                                label="Level"
                                value={<span style={getStyle('level')} className={`px-2 py-1 rounded-full text-xs font-medium ${getLevelColor(entry.level)}`}>{entry.level}</span>}
                                filterKey="level"
                                filterValue={entry.level}
                                onApplyFilter={onApplyFilter}
                            />
                            <DetailRow
                                label="Sender Type"
                                value={entry.sndrtype}
                                valueStyle={getStyle('sndrtype')}
                                filterKey="sndrtype"
                                filterValue={entry.sndrtype}
                                onApplyFilter={onApplyFilter}
                            />
                            <DetailRow
                                label="Sender Name"
                                value={entry.sndrname}
                                valueStyle={getStyle('sndrname')}
                                filterKey="sndrname"
                                filterValue={entry.sndrname}
                                onApplyFilter={onApplyFilter}
                            />
                            <DetailRow
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

                            {parsedContent.type === 'grid' && (
                                <GridView data={parsedContent.data as GridData} />
                            )}

                            {parsedContent.type === 'kv' && (
                                <KeyValueTableView data={parsedContent.data as {key: string, value: string}[]} />
                            )}

                            {parsedContent.type === 'xml' && (
                                <XmlTreeView xmlString={parsedContent.data as string} />
                            )}

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
                    </>
                ) : (
                     <div className="flex flex-col items-center justify-center h-full text-center text-gray-500 dark:text-gray-400">
                        <InformationCircleIcon className="w-12 h-12 text-gray-400 dark:text-gray-500 mb-4" />
                        <h4 className="text-lg font-semibold text-gray-700 dark:text-gray-300">No Selection</h4>
                        <p className="mt-1 text-sm">Click on a log entry in the table to see its details here.</p>
                    </div>
                )}
            </div>
        </aside>
    );
};