import React from 'react';
import { ConsoleMessage, ConsoleMessageType, IconSet, Theme } from '../types';
import { Icon } from './icons';
import { highlightText } from '../utils';

interface ConsoleProps {
  messages: ConsoleMessage[];
  onClear: () => void;
  filters: Record<ConsoleMessageType, boolean>;
  onFiltersChange: (newFilters: Record<ConsoleMessageType, boolean>) => void;
  iconSet: IconSet;
  searchTerm: string;
  theme: Theme;
}

const getIcon = (type: ConsoleMessageType, iconSet: IconSet) => {
  switch (type) {
    case 'INFO':
      return <Icon name="InformationCircle" iconSet={iconSet} className="w-5 h-5 text-blue-500" />;
    case 'DEBUG':
      return <Icon name="CheckCircle" iconSet={iconSet} className="w-5 h-5 text-green-500" />;
    case 'WARNING':
        return <Icon name="ExclamationTriangle" iconSet={iconSet} className="w-5 h-5 text-orange-400" />;
    case 'ERROR':
      return <Icon name="XCircle" iconSet={iconSet} className="w-5 h-5 text-red-500" />;
    default:
      return null;
  }
};

const FilterButton: React.FC<{
  type: ConsoleMessageType;
  count: number;
  isActive: boolean;
  onClick: () => void;
}> = ({ type, count, isActive, onClick }) => {
  const colors = {
    DEBUG:   'border-green-400 dark:border-green-600 text-green-700 dark:text-green-300 bg-green-100 dark:bg-green-900/40 hover:bg-green-200 dark:hover:bg-green-800/50',
    INFO:    'border-blue-400 dark:border-blue-600 text-blue-700 dark:text-blue-300 bg-blue-100 dark:bg-blue-900/40 hover:bg-blue-200 dark:hover:bg-blue-800/50',
    WARNING: 'border-orange-400 dark:border-orange-600 text-orange-700 dark:text-orange-300 bg-orange-100 dark:bg-orange-900/40 hover:bg-orange-200 dark:hover:bg-orange-800/50',
    ERROR:   'border-red-400 dark:border-red-600 text-red-700 dark:text-red-300 bg-red-100 dark:bg-red-900/40 hover:bg-red-200 dark:hover:bg-red-800/50',
  };
  const baseClasses = 'flex items-center space-x-2 px-2.5 py-1 text-xs font-semibold rounded-md border transition-all duration-200';
  const activeClasses = colors[type];
  const inactiveClasses = 'border-gray-300 dark:border-gray-700 text-gray-500 bg-gray-100 dark:bg-gray-800 hover:bg-gray-200 dark:hover:bg-gray-700/70 opacity-60';
  
  return (
    <button onClick={onClick} className={`${baseClasses} ${isActive ? activeClasses : inactiveClasses}`} disabled={!isActive && count === 0}>
      <span>{type}</span>
      <span className={`px-1.5 py-0.5 rounded-full text-xs font-mono ${isActive ? 'bg-black/10 dark:bg-black/20' : 'bg-gray-300 dark:bg-gray-600 text-gray-500 dark:text-gray-400'}`}>{count.toLocaleString()}</span>
    </button>
  );
};


export const Console: React.FC<ConsoleProps> = ({ messages, onClear, filters, onFiltersChange, iconSet, searchTerm, theme }) => {
  const consoleEndRef = React.useRef<HTMLDivElement>(null);

  const messageCounts = React.useMemo(() => {
    return messages.reduce((acc, msg) => {
        acc[msg.type] = (acc[msg.type] || 0) + 1;
        return acc;
    }, {} as Record<ConsoleMessageType, number>);
  }, [messages]);

  const filteredMessages = React.useMemo(() => {
    const lowerCaseSearchTerm = searchTerm.toLowerCase();
    return messages
      .filter(msg => filters[msg.type])
      .filter(msg => {
          if (!lowerCaseSearchTerm) return true;
          return msg.message.toLowerCase().includes(lowerCaseSearchTerm);
      });
  }, [messages, filters, searchTerm]);

  React.useEffect(() => {
    consoleEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [filteredMessages]);

  const handleToggleFilter = (type: ConsoleMessageType) => {
    onFiltersChange({ ...filters, [type]: !filters[type] });
  };

  return (
    <div className="bg-white dark:bg-gray-800 flex flex-col flex-grow">
      <div className="flex items-center justify-between p-3 border-b border-gray-200 dark:border-gray-700 flex-shrink-0 flex-wrap gap-4">
          <h3 className="font-mono text-sm font-semibold text-gray-600 dark:text-gray-300 flex-grow">APPLICATION LOG</h3>
          
          <div className="flex items-center gap-4">
            <div className="flex items-center space-x-2">
                {(['DEBUG', 'INFO', 'WARNING', 'ERROR'] as ConsoleMessageType[]).map(type => (
                    <FilterButton
                        key={type}
                        type={type}
                        count={messageCounts[type] || 0}
                        isActive={filters[type]}
                        onClick={() => handleToggleFilter(type)}
                    />
                ))}
            </div>
            <button 
              onClick={onClear} 
              className="px-3 py-1 text-xs font-semibold text-gray-700 dark:text-gray-300 bg-gray-200 dark:bg-gray-700 hover:bg-gray-300 dark:hover:bg-gray-600 rounded-md transition-colors"
            >
              Clear
            </button>
          </div>
      </div>
      <div className="flex-grow min-h-0 relative">
        <div className="absolute inset-0 p-3 overflow-y-auto font-mono text-xs">
          {filteredMessages.map((msg, index) => (
            <div key={index} className={`flex items-start space-x-3 mb-1.5`}>
              <span className="flex-shrink-0 pt-0.5">{getIcon(msg.type, iconSet)}</span>
              <span className="flex-shrink-0 text-gray-400 dark:text-gray-500">{msg.timestamp}</span>
              <p className={`flex-grow break-words whitespace-pre-wrap ${
              msg.type === 'ERROR' ? 'text-red-600 dark:text-red-400' : 
              msg.type === 'DEBUG' ? 'text-green-600 dark:text-green-400' :
              msg.type === 'WARNING' ? 'text-orange-500 dark:text-orange-400' :
              msg.type === 'INFO' ? 'text-blue-600 dark:text-blue-400' : 'text-gray-600 dark:text-gray-300'
            }
            `}>{searchTerm.trim()
                ? <span dangerouslySetInnerHTML={{ __html: highlightText(msg.message, [searchTerm], theme) }} />
                : msg.message
            }</p>
            </div>
          ))}
          <div ref={consoleEndRef} />
        </div>
      </div>
    </div>
  );
};