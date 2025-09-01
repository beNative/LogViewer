import React from 'react';
import { ChevronDownIcon } from './icons/ChevronDownIcon.tsx';

interface MultiSelectDropdownProps {
    label: string;
    options: string[];
    selectedOptions: string[];
    onSelectionChange: (selected: string[]) => void;
}

export const MultiSelectDropdown: React.FC<MultiSelectDropdownProps> = ({
    label,
    options,
    selectedOptions,
    onSelectionChange
}) => {
    const [isOpen, setIsOpen] = React.useState(false);
    const [searchTerm, setSearchTerm] = React.useState('');
    const wrapperRef = React.useRef<HTMLDivElement>(null);
    const inputRef = React.useRef<HTMLInputElement>(null);

    React.useEffect(() => {
        const handleClickOutside = (event: MouseEvent) => {
            if (wrapperRef.current && !wrapperRef.current.contains(event.target as Node)) {
                setIsOpen(false);
            }
        };
        document.addEventListener('mousedown', handleClickOutside);
        return () => document.removeEventListener('mousedown', handleClickOutside);
    }, [wrapperRef]);

    React.useEffect(() => {
        if (!isOpen) {
            setSearchTerm('');
        } else {
            // Focus input when dropdown opens
            setTimeout(() => inputRef.current?.focus(), 100);
        }
    }, [isOpen]);

    const handleSelect = (option: string) => {
        const newSelection = selectedOptions.includes(option)
            ? selectedOptions.filter(item => item !== option)
            : [...selectedOptions, option];
        onSelectionChange(newSelection);
    };
    
    const buttonLabel = React.useMemo(() => {
        if (selectedOptions.length > 0) {
            const selectionText = selectedOptions.join(', ');
            return `${selectedOptions.length} selected [${selectionText}]`;
        }
        return label;
    }, [selectedOptions, label]);

    const filteredOptions = options.filter(option =>
        option.toLowerCase().includes(searchTerm.toLowerCase())
    );
    
    const handleSelectAll = () => {
        const selectedSet = new Set(selectedOptions);
        filteredOptions.forEach(opt => selectedSet.add(opt));
        onSelectionChange(Array.from(selectedSet));
    };

    const handleDeselectAll = () => {
        const filteredOptionsSet = new Set(filteredOptions);
        const newSelection = selectedOptions.filter(opt => !filteredOptionsSet.has(opt));
        onSelectionChange(newSelection);
    };

    return (
        <div className="relative w-full" ref={wrapperRef}>
            <button 
                onClick={() => setIsOpen(!isOpen)} 
                className="w-full h-10 px-3 py-2 bg-white dark:bg-gray-700/80 border border-gray-300 dark:border-gray-600 rounded-md shadow-sm text-gray-900 dark:text-white text-sm flex items-center justify-between focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-100 dark:focus:ring-offset-gray-800 focus:ring-sky-500"
            >
                <span className="truncate">{buttonLabel}</span>
                <ChevronDownIcon className={`w-5 h-5 text-gray-400 transition-transform duration-200 ${isOpen ? 'transform rotate-180' : ''}`} />
            </button>

            {isOpen && (
                <div className="absolute z-30 mt-1 w-full bg-white dark:bg-gray-800 border border-gray-300 dark:border-gray-600 rounded-md shadow-lg flex flex-col">
                    <div className="p-2 border-b border-gray-200 dark:border-gray-700">
                        <input
                            ref={inputRef}
                            type="text"
                            value={searchTerm}
                            onChange={(e) => setSearchTerm(e.target.value)}
                            onClick={(e) => e.stopPropagation()}
                            placeholder="Search options..."
                            className="w-full px-2 py-1.5 bg-gray-100 dark:bg-gray-700 border-gray-300 dark:border-gray-600 text-gray-900 dark:text-white sm:text-sm rounded-md shadow-sm focus:ring-sky-500 focus:border-sky-500 transition"
                        />
                        <div className="flex items-center justify-between mt-2">
                             <span className="text-xs text-gray-500 dark:text-gray-400 px-1">
                                {filteredOptions.length} visible
                             </span>
                            <div className="flex items-center space-x-2">
                                <button
                                    onClick={handleDeselectAll}
                                    type="button"
                                    className="px-2 py-0.5 text-xs font-medium text-gray-700 dark:text-gray-300 rounded hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors"
                                >
                                    Deselect All
                                </button>
                                <button
                                    onClick={handleSelectAll}
                                    type="button"
                                    className="px-2 py-0.5 text-xs font-medium text-gray-700 dark:text-gray-300 rounded hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors"
                                >
                                    Select All
                                </button>
                            </div>
                        </div>
                    </div>
                    <ul className="p-1 max-h-52 overflow-auto">
                        {filteredOptions.length > 0 ? (
                            filteredOptions.map(option => (
                                <li key={option}>
                                    <label className="flex w-full items-center space-x-3 p-2 rounded-md hover:bg-gray-100 dark:hover:bg-gray-700 cursor-pointer">
                                        <input 
                                            type="checkbox" 
                                            checked={selectedOptions.includes(option)}
                                            onChange={() => handleSelect(option)}
                                            className="h-4 w-4 rounded bg-gray-100 dark:bg-gray-800 border-gray-300 dark:border-gray-500 text-sky-600 dark:text-sky-500 focus:ring-sky-500 cursor-pointer"
                                        />
                                        <span className="text-sm text-gray-800 dark:text-gray-200 truncate" title={option}>{option}</span>
                                     </label>
                                </li>
                            ))
                        ) : (
                            <li className="p-2 text-sm text-gray-500 dark:text-gray-400 text-center">
                                No options found.
                            </li>
                        )}
                    </ul>
                </div>
            )}
        </div>
    )
}