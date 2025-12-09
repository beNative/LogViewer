import React from 'react';
import { ConsoleMessage, ConsoleMessageType, IconSet, Theme } from '../types';
import { Icon } from './icons';
import { highlightText } from '../utils';

interface BottomLogPanelProps {
    isVisible: boolean;
    onToggle: () => void;
    messages: ConsoleMessage[];
    onClear: () => void;
    filters: Record<ConsoleMessageType, boolean>;
    onFiltersChange: (newFilters: Record<ConsoleMessageType, boolean>) => void;
    iconSet: IconSet;
    theme: Theme;
    height: number;
    onHeightChange: (height: number) => void;
}

const getIcon = (type: ConsoleMessageType, iconSet: IconSet) => {
    switch (type) {
        case 'INFO':
            return <Icon name="InformationCircle" iconSet={iconSet} className="w-4 h-4 text-blue-500" />;
        case 'DEBUG':
            return <Icon name="CheckCircle" iconSet={iconSet} className="w-4 h-4 text-green-500" />;
        case 'WARNING':
            return <Icon name="ExclamationTriangle" iconSet={iconSet} className="w-4 h-4 text-orange-400" />;
        case 'ERROR':
            return <Icon name="XCircle" iconSet={iconSet} className="w-4 h-4 text-red-500" />;
        default:
            return null;
    }
};

const FilterPill: React.FC<{
    type: ConsoleMessageType;
    count: number;
    isActive: boolean;
    onClick: () => void;
}> = ({ type, count, isActive, onClick }) => {
    const colors: Record<ConsoleMessageType, string> = {
        DEBUG: 'text-green-600 dark:text-green-400',
        INFO: 'text-blue-600 dark:text-blue-400',
        WARNING: 'text-orange-500 dark:text-orange-400',
        ERROR: 'text-red-600 dark:text-red-400',
    };

    return (
        <button
            onClick={onClick}
            className={`flex items-center gap-1 px-2 py-0.5 text-xs font-mono rounded transition-all ${isActive
                ? `${colors[type]} bg-gray-200 dark:bg-gray-700`
                : 'text-gray-400 dark:text-gray-600 line-through'
                }`}
        >
            <span>{type}</span>
            <span className="opacity-70">({count})</span>
        </button>
    );
};

export const BottomLogPanel: React.FC<BottomLogPanelProps> = ({
    isVisible,
    onToggle,
    messages,
    onClear,
    filters,
    onFiltersChange,
    iconSet,
    theme,
    height,
    onHeightChange,
}) => {
    const containerRef = React.useRef<HTMLDivElement>(null);
    const logEndRef = React.useRef<HTMLDivElement>(null);

    const messageCounts = React.useMemo(() => {
        return messages.reduce((acc, msg) => {
            acc[msg.type] = (acc[msg.type] || 0) + 1;
            return acc;
        }, {} as Record<ConsoleMessageType, number>);
    }, [messages]);

    const filteredMessages = React.useMemo(() => {
        return messages.filter(msg => filters[msg.type]);
    }, [messages, filters]);

    // Auto-scroll to bottom when new messages arrive
    React.useEffect(() => {
        if (isVisible) {
            logEndRef.current?.scrollIntoView({ behavior: 'smooth' });
        }
    }, [filteredMessages, isVisible]);

    // Resize handling - matches Splitter.tsx pattern
    const handleResizeMouseDown = (downEvent: React.MouseEvent) => {
        downEvent.preventDefault();

        const startY = downEvent.clientY;
        const startHeight = height;

        const handleMouseMove = (moveEvent: MouseEvent) => {
            const deltaY = startY - moveEvent.clientY;
            const newHeight = Math.max(100, Math.min(600, startHeight + deltaY));
            onHeightChange(newHeight);
        };

        const handleMouseUp = () => {
            document.removeEventListener('mousemove', handleMouseMove);
            document.removeEventListener('mouseup', handleMouseUp);
            document.body.style.cursor = '';
        };

        document.body.style.cursor = 'ns-resize';
        document.addEventListener('mousemove', handleMouseMove);
        document.addEventListener('mouseup', handleMouseUp);
    };

    const handleToggleFilter = (type: ConsoleMessageType) => {
        onFiltersChange({ ...filters, [type]: !filters[type] });
    };

    // Hidden state - completely invisible when not toggled
    if (!isVisible) {
        return null;
    }

    // Expanded state
    return (
        <div
            ref={containerRef}
            className="flex-shrink-0 bg-white dark:bg-gray-800 border-t border-gray-200 dark:border-gray-700 flex flex-col"
            style={{ height: `${height}px` }}
        >
            {/* Resize handle - styled like Splitter component */}
            <div
                className="flex-shrink-0 h-1.5 cursor-ns-resize bg-gray-300 dark:bg-gray-600 hover:bg-sky-500 active:bg-sky-600 transition-colors duration-150"
                onMouseDown={handleResizeMouseDown}
                style={{ touchAction: 'none' }}
            />

            {/* Header */}
            <div className="flex items-center justify-between px-3 py-1.5 border-b border-gray-200 dark:border-gray-700 flex-shrink-0">
                <div className="flex items-center gap-3">
                    <button
                        onClick={onToggle}
                        className="p-0.5 hover:bg-gray-200 dark:hover:bg-gray-700 rounded transition-colors"
                    >
                        <Icon name="ChevronDown" iconSet={iconSet} className="w-4 h-4 text-gray-500" />
                    </button>
                    <span className="text-xs font-semibold text-gray-500 dark:text-gray-400 font-mono">APPLICATION LOG</span>
                </div>

                <div className="flex items-center gap-2">
                    {(['DEBUG', 'INFO', 'WARNING', 'ERROR'] as ConsoleMessageType[]).map(type => (
                        <FilterPill
                            key={type}
                            type={type}
                            count={messageCounts[type] || 0}
                            isActive={filters[type]}
                            onClick={() => handleToggleFilter(type)}
                        />
                    ))}
                    <button
                        onClick={onClear}
                        className="ml-2 px-2 py-0.5 text-xs font-semibold text-gray-600 dark:text-gray-400 hover:bg-gray-200 dark:hover:bg-gray-700 rounded transition-colors"
                    >
                        Clear
                    </button>
                    <button
                        onClick={onToggle}
                        className="ml-1 p-1 text-gray-500 hover:text-gray-700 dark:hover:text-gray-300 hover:bg-gray-200 dark:hover:bg-gray-700 rounded transition-colors"
                        title="Close panel"
                    >
                        <Icon name="XMark" iconSet={iconSet} className="w-4 h-4" />
                    </button>
                </div>
            </div>

            {/* Log messages */}
            <div className="flex-grow min-h-0 overflow-y-auto p-2 font-mono text-xs">
                {filteredMessages.map((msg, index) => (
                    <div key={index} className="flex items-start gap-2 py-0.5 hover:bg-gray-50 dark:hover:bg-gray-700/50">
                        <span className="flex-shrink-0">{getIcon(msg.type, iconSet)}</span>
                        <span className="flex-shrink-0 text-gray-400 dark:text-gray-500 text-[10px]">{msg.timestamp}</span>
                        <p className={`flex-grow break-words whitespace-pre-wrap ${msg.type === 'ERROR' ? 'text-red-600 dark:text-red-400' :
                            msg.type === 'DEBUG' ? 'text-green-600 dark:text-green-400' :
                                msg.type === 'WARNING' ? 'text-orange-500 dark:text-orange-400' :
                                    'text-blue-600 dark:text-blue-400'
                            }`}>
                            {msg.message}
                        </p>
                    </div>
                ))}
                <div ref={logEndRef} />
            </div>
        </div>
    );
};
