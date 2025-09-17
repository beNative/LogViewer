import React from 'react';
import { FilterState, IconSet } from '../types';
import { MultiSelectDropdown } from './MultiSelectDropdown';
import { areFiltersEqual, areArraysEqualUnordered } from '../utils';
import { SqlEditor } from './SqlEditor';
import { Icon } from './icons';
import { Tooltip } from './Tooltip';

interface FilterBarProps {
    filters: FilterState;
    appliedFilters: FilterState;
    onFiltersChange: (newFilters: FilterState) => void;
    onApplyFilters: () => void;
    onResetFilters: () => void;
    uniqueValues: {
        level: string[];
        sndrtype: string[];
        sndrname: string[];
        fileName: string[];
    };
    customFilterPresets: Record<string, FilterState>;
    onSavePreset: (name: string) => void;
    onDeletePreset: (name: string) => void;
    onLoadPreset: (name: string) => void;
    iconSet: IconSet;
    isInitialLoad: boolean;
}

const inputStyles = "w-full bg-white dark:bg-gray-700/80 border-gray-300 dark:border-gray-600 text-gray-900 dark:text-white sm:text-sm rounded-md shadow-sm focus:ring-sky-500 focus:border-sky-500 transition";
const textareaStyles = `${inputStyles} resize-y min-h-[60px]`;

const FilterGroup: React.FC<{
  title: string;
  children: React.ReactNode;
  isDisabled?: boolean;
  isActive?: boolean;
  isDirty?: boolean;
}> = ({ title, children, isDisabled = false, isActive = false, isDirty = false }) => {
    const dotTitle = isDirty ? 'Filter changed' : isActive ? 'Filter active' : 'Filter inactive';
    
    const dot = (
        <Tooltip content={dotTitle}>
            <span className={`w-2 h-2 rounded-full transition-colors ${
                isDirty
                    ? 'bg-amber-400'
                    : isActive
                    ? 'bg-sky-500'
                    : 'bg-gray-300 dark:bg-gray-600'
            }`} />
        </Tooltip>
    );

    return (
        <div className={`transition-opacity duration-300 ${isDisabled ? 'opacity-50 pointer-events-none' : ''}`}>
            <h4 className="text-sm font-semibold text-gray-600 dark:text-gray-300 mb-2 flex items-center gap-2">
                {dot}
                {title}
            </h4>
            <div className="space-y-3">{children}</div>
        </div>
    );
};


const ToggleButton: React.FC<{
    label: string;
    isActive: boolean;
    onClick: () => void;
}> = ({ label, isActive, onClick }) => {
    return (
        <button
            type="button"
            onClick={onClick}
            className={`px-2.5 py-1 text-xs font-semibold rounded-md transition-colors ${
                isActive 
                ? 'bg-sky-600 text-white' 
                : 'bg-gray-200 dark:bg-gray-600 text-gray-700 dark:text-gray-300 hover:bg-gray-300 dark:hover:bg-gray-500'
            }`}
        >
            {label}
        </button>
    );
};

const ToggleSwitch: React.FC<{
  enabled: boolean;
  onChange: (enabled: boolean) => void;
}> = ({ enabled, onChange }) => {
  return (
      <button
        type="button"
        className={`relative inline-flex h-6 w-11 flex-shrink-0 cursor-pointer rounded-full border-2 border-transparent transition-colors duration-200 ease-in-out focus:outline-none focus:ring-2 focus:ring-sky-500 focus:ring-offset-2 focus:ring-offset-gray-100 dark:focus:ring-offset-gray-800 ${
          enabled ? 'bg-sky-600' : 'bg-gray-300 dark:bg-gray-600'
        }`}
        role="switch"
        aria-checked={enabled}
        onClick={() => onChange(!enabled)}
      >
        <span
          aria-hidden="true"
          className={`pointer-events-none inline-block h-5 w-5 transform rounded-full bg-white shadow ring-0 transition duration-200 ease-in-out ${
            enabled ? 'translate-x-5' : 'translate-x-0'
          }`}
        />
      </button>
  );
};


export const FilterBar: React.FC<FilterBarProps> = ({
    filters,
    appliedFilters,
    onFiltersChange,
    onApplyFilters,
    onResetFilters,
    uniqueValues,
    customFilterPresets,
    onSavePreset,
    onDeletePreset,
    onLoadPreset,
    iconSet,
    isInitialLoad,
}) => {
    
    const [selectedPreset, setSelectedPreset] = React.useState('');
    const [isSavingPreset, setIsSavingPreset] = React.useState(false);
    const [newPresetName, setNewPresetName] = React.useState('');
    const [saveTarget, setSaveTarget] = React.useState('--new--');

    const PREDEFINED_SQL_QUERY = `SELECT * FROM logs\nWHERE level = 'ERROR'\nORDER BY time DESC`;
    
    const isOverallDirty = !areFiltersEqual(filters, appliedFilters);

    const isDateRangeActive = !!(appliedFilters.dateFrom || appliedFilters.timeFrom || appliedFilters.dateTo || appliedFilters.timeTo);
    const isDateRangeDirty = filters.dateFrom !== appliedFilters.dateFrom || filters.timeFrom !== appliedFilters.timeFrom || filters.dateTo !== appliedFilters.dateTo || filters.timeTo !== appliedFilters.timeTo;
    
    const isAttributesActive = appliedFilters.level.length > 0 || appliedFilters.sndrtype.length > 0 || appliedFilters.sndrname.length > 0 || appliedFilters.fileName.length > 0;
    const isAttributesDirty = !areArraysEqualUnordered(filters.level, appliedFilters.level) || filters.levelFilterMode !== appliedFilters.levelFilterMode ||
                              !areArraysEqualUnordered(filters.sndrtype, appliedFilters.sndrtype) || filters.sndrtypeFilterMode !== appliedFilters.sndrtypeFilterMode ||
                              !areArraysEqualUnordered(filters.sndrname, appliedFilters.sndrname) || filters.sndrnameFilterMode !== appliedFilters.sndrnameFilterMode ||
                              !areArraysEqualUnordered(filters.fileName, appliedFilters.fileName) || filters.fileNameFilterMode !== appliedFilters.fileNameFilterMode;

    const isMessageActive = !!(appliedFilters.includeMsg || appliedFilters.excludeMsg);
    const isMessageDirty = filters.includeMsg !== appliedFilters.includeMsg || filters.excludeMsg !== appliedFilters.excludeMsg || filters.includeMsgMode !== appliedFilters.includeMsgMode || filters.excludeMsgMode !== appliedFilters.excludeMsgMode;

    const isSqlActive = appliedFilters.sqlQueryEnabled;
    const isSqlDirty = filters.sqlQueryEnabled !== appliedFilters.sqlQueryEnabled || filters.sqlQuery !== appliedFilters.sqlQuery;


    // This effect ensures the dropdown reflects the form's state.
    // If the filters are modified to no longer match the selected preset,
    // the dropdown resets to show that the current filters are "custom".
    React.useEffect(() => {
        if (selectedPreset && customFilterPresets[selectedPreset]) {
            // Using a robust deep comparison instead of JSON.stringify
            if (!areFiltersEqual(filters, customFilterPresets[selectedPreset])) {
                setSelectedPreset('');
            }
        }
    }, [filters, selectedPreset, customFilterPresets]);

    const handleInputChange = (e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
        onFiltersChange({ ...filters, [e.target.name]: e.target.value });
    };
    
    const handleFieldChange = (fieldName: keyof FilterState, value: any) => {
        onFiltersChange({ ...filters, [fieldName]: value });
    };

    type MultiSelectKeys = 'level' | 'sndrtype' | 'sndrname' | 'fileName';
    const handleMultiSelectChange = (name: MultiSelectKeys, values: string[]) => {
        onFiltersChange({ ...filters, [name]: values });
    };

    const handleModeChange = (
        filter: 'includeMsgMode' | 'excludeMsgMode',
        mode: 'AND' | 'OR'
    ) => {
        onFiltersChange({ ...filters, [filter]: mode });
    };

    const handleSaveClick = () => {
        setIsSavingPreset(true);
        if (selectedPreset) {
            setSaveTarget(selectedPreset);
        } else {
            setSaveTarget('--new--');
        }
        setNewPresetName('');
    };

    const handleConfirmSave = () => {
        const isNew = saveTarget === '--new--';
        const nameToSave = isNew ? newPresetName.trim() : saveTarget;
    
        if (!nameToSave) {
            return;
        }
    
        const saveAction = () => {
            onSavePreset(nameToSave);
            setSelectedPreset(nameToSave); // Update the main selection dropdown
            setIsSavingPreset(false);
            setNewPresetName('');
            setSaveTarget('--new--');
        };
        
        // If overwriting an existing preset selected from the dropdown
        if (!isNew) {
            if (window.confirm(`Are you sure you want to overwrite the preset '${nameToSave}'?`)) {
                saveAction();
            }
            return;
        }
    
        // If creating a new one, check if the name conflicts with an existing one.
        if (customFilterPresets[nameToSave]) {
            if (window.confirm(`A preset named '${nameToSave}' already exists. Do you want to overwrite it?`)) {
                saveAction();
            }
        } else {
            // It's a genuinely new preset, no confirmation needed.
            saveAction();
        }
    };
    
    const handleCancelSave = () => {
        setIsSavingPreset(false);
        setNewPresetName('');
        setSaveTarget('--new--');
    };
    
    const handleDeleteClick = () => {
        if (selectedPreset && window.confirm(`Are you sure you want to delete the preset '${selectedPreset}'?`)) {
            onDeletePreset(selectedPreset);
            setSelectedPreset('');
        }
    };

    const handleSelectChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
        const name = e.target.value;
        setSelectedPreset(name);
        if (name) {
            onLoadPreset(name);
        }
    };
    
    const renderAttributeFilter = (
        key: 'level' | 'sndrtype' | 'sndrname' | 'fileName',
        label: string,
        placeholder: string,
        options: string[]
    ) => {
        const modeKey = `${key}FilterMode` as const;
        return (
            <div>
                <div className="flex justify-between items-center mb-1">
                    <label className="block text-xs font-medium text-gray-500 dark:text-gray-400">{label}</label>
                    <div className="flex items-center rounded-lg bg-gray-200/80 dark:bg-gray-700/50 p-0.5 space-x-1">
                        <ToggleButton label="Include" isActive={filters[modeKey] === 'include'} onClick={() => handleFieldChange(modeKey, 'include')} />
                        <ToggleButton label="Exclude" isActive={filters[modeKey] === 'exclude'} onClick={() => handleFieldChange(modeKey, 'exclude')} />
                    </div>
                </div>
                <MultiSelectDropdown 
                    label={placeholder}
                    options={options} 
                    selectedOptions={filters[key]} 
                    onSelectionChange={(values) => handleMultiSelectChange(key, values)} 
                />
            </div>
        );
    };


    return (
        <div className="flex flex-col h-full">
            <div className="p-4 flex-shrink-0 border-b border-gray-200 dark:border-gray-700 flex items-center justify-between gap-3">
                <h3 className="text-lg font-semibold text-gray-900 dark:text-white">Filters</h3>
                <div className="flex items-center gap-2">
                    <button onClick={onResetFilters} className="flex items-center justify-center gap-2 px-3 py-1.5 text-sm bg-red-600 hover:bg-red-700 text-white font-semibold rounded-md transition-colors duration-200">
                        <Icon name="XMark" iconSet={iconSet} className="w-4 h-4" />
                        Clear
                    </button>
                    <button
                        onClick={onApplyFilters}
                        disabled={!isOverallDirty && !isInitialLoad}
                        className={`flex items-center justify-center gap-2 px-3 py-1.5 text-sm text-white font-semibold rounded-md transition-colors duration-200
                            ${(isOverallDirty || isInitialLoad)
                                ? 'bg-amber-500 hover:bg-amber-600 animate-pulse'
                                : 'bg-gray-400 dark:bg-gray-600 cursor-not-allowed'
                            }
                        `}
                    >
                        <Icon name="Filter" iconSet={iconSet} className="w-4 h-4" />
                        Apply
                    </button>
                </div>
            </div>
            {isOverallDirty && (
                <div className="p-3 bg-amber-100 dark:bg-amber-900/40 border-b border-amber-200 dark:border-amber-800/60 flex-shrink-0">
                    <div className="flex items-center gap-2 text-amber-800 dark:text-amber-300 text-sm font-medium">
                        <Icon name="InformationCircle" iconSet={iconSet} className="w-5 h-5 flex-shrink-0" />
                        <p>Filter settings have changed. Click Apply to update.</p>
                    </div>
                </div>
            )}
            <div className="flex-1 p-4 overflow-y-auto">
                <div className="space-y-6">
                     <FilterGroup title="Presets" isActive={!!selectedPreset} isDirty={false /* Presets can't be dirty */}>
                        <select
                            value={selectedPreset}
                            onChange={handleSelectChange}
                            className={inputStyles}
                            aria-label="Filter presets"
                        >
                            <option value="">-- Custom Filters --</option>
                            {Object.keys(customFilterPresets).sort().map(name => (
                                <option key={name} value={name}>{name}</option>
                            ))}
                        </select>
                        {isSavingPreset ? (
                             <div className="mt-2 space-y-2 p-2.5 bg-gray-100 dark:bg-gray-900/50 rounded-lg border border-gray-200 dark:border-gray-700/50">
                                <label htmlFor="save-target" className="block text-xs font-medium text-gray-500 dark:text-gray-400">Save Target</label>
                                <select
                                    id="save-target"
                                    value={saveTarget}
                                    onChange={(e) => setSaveTarget(e.target.value)}
                                    className={inputStyles}
                                >
                                    <option value="--new--">-- Create New Preset --</option>
                                    {Object.keys(customFilterPresets).sort().map(name => (
                                        <option key={name} value={name}>{`Overwrite '${name}'`}</option>
                                    ))}
                                </select>
                                
                                {saveTarget === '--new--' && (
                                    <input
                                        type="text"
                                        value={newPresetName}
                                        onChange={(e) => setNewPresetName(e.target.value)}
                                        placeholder="Enter new preset name..."
                                        className={inputStyles}
                                        autoFocus
                                        onKeyDown={(e) => {
                                            if (e.key === 'Enter') handleConfirmSave();
                                            if (e.key === 'Escape') handleCancelSave();
                                        }}
                                    />
                                )}
                                <div className="flex justify-end gap-2">
                                    <button
                                        onClick={handleCancelSave}
                                        className="px-3 py-1 text-sm bg-white hover:bg-gray-200 text-gray-800 border border-gray-300 dark:bg-gray-700 dark:hover:bg-gray-600 dark:text-white dark:border-gray-500 font-semibold rounded-md"
                                    >
                                        Cancel
                                    </button>
                                    <button
                                        onClick={handleConfirmSave}
                                        disabled={saveTarget === '--new--' && !newPresetName.trim()}
                                        className="px-3 py-1 text-sm bg-sky-600 hover:bg-sky-700 text-white font-semibold rounded-md disabled:opacity-50 disabled:cursor-not-allowed"
                                    >
                                        {saveTarget === '--new--' ? 'Save New' : 'Overwrite'}
                                    </button>
                                </div>
                            </div>
                        ) : (
                            <div className="flex justify-between items-center gap-2 mt-2">
                                <Tooltip content="Save current filters as a new or existing preset">
                                    <button
                                        onClick={handleSaveClick}
                                        className="flex items-center justify-center gap-2 w-full px-3 py-1.5 text-sm bg-white hover:bg-gray-200 text-gray-800 border border-gray-300 dark:bg-gray-700 dark:hover:bg-gray-600 dark:text-white dark:border-gray-500 font-semibold rounded-lg transition-colors duration-200"
                                    >
                                        <Icon name="SaveDisk" iconSet={iconSet} className="w-4 h-4" />
                                        Save As...
                                    </button>
                                </Tooltip>
                                <Tooltip content={selectedPreset ? `Delete preset '${selectedPreset}'` : 'Select a preset to delete'}>
                                    <button
                                        onClick={handleDeleteClick}
                                        disabled={!selectedPreset}
                                        className="flex items-center justify-center gap-2 w-full px-3 py-1.5 text-sm bg-white hover:bg-gray-200 text-gray-800 border border-gray-300 dark:bg-gray-700 dark:hover:bg-gray-600 dark:text-white dark:border-gray-500 font-semibold rounded-lg transition-colors duration-200 disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:bg-gray-700"
                                    >
                                        <Icon name="Trash" iconSet={iconSet} className="w-4 h-4" />
                                        Delete Preset
                                    </button>
                                </Tooltip>
                            </div>
                        )}
                    </FilterGroup>

                    <FilterGroup title="Date Range" isDisabled={filters.sqlQueryEnabled} isActive={isDateRangeActive} isDirty={isDateRangeDirty}>
                         <div>
                            <label htmlFor="dateFrom" className="block text-xs font-medium text-gray-500 dark:text-gray-400 mb-1">From</label>
                            <div className="flex gap-2">
                                <input type="date" name="dateFrom" id="dateFrom" value={filters.dateFrom} onChange={handleInputChange} className={inputStyles} />
                                <input type="time" name="timeFrom" id="timeFrom" value={filters.timeFrom} onChange={handleInputChange} className={inputStyles} step="1" />
                            </div>
                        </div>
                         <div>
                            <label htmlFor="dateTo" className="block text-xs font-medium text-gray-500 dark:text-gray-400 mb-1">To</label>
                            <div className="flex gap-2">
                                <input type="date" name="dateTo" id="dateTo" value={filters.dateTo} onChange={handleInputChange} className={inputStyles} />
                                <input type="time" name="timeTo" id="timeTo" value={filters.timeTo} onChange={handleInputChange} className={inputStyles} step="1" />
                            </div>
                        </div>
                    </FilterGroup>
                    
                    <FilterGroup title="Log Attributes" isDisabled={filters.sqlQueryEnabled} isActive={isAttributesActive} isDirty={isAttributesDirty}>
                        {renderAttributeFilter('level', 'Level', 'Select levels...', uniqueValues.level)}
                        {renderAttributeFilter('sndrtype', 'Sender Type', 'Select types...', uniqueValues.sndrtype)}
                        {renderAttributeFilter('sndrname', 'Sender Name', 'Select names...', uniqueValues.sndrname)}
                        {renderAttributeFilter('fileName', 'Filename', 'Select files...', uniqueValues.fileName)}
                    </FilterGroup>
                    
                    <FilterGroup title="Message Content" isDisabled={filters.sqlQueryEnabled} isActive={isMessageActive} isDirty={isMessageDirty}>
                        <div>
                            <div className="flex justify-between items-center mb-1.5">
                                <label htmlFor="includeMsg" className="block text-xs font-medium text-gray-500 dark:text-gray-400">Message contains</label>
                                <div className="flex items-center rounded-lg bg-gray-200/80 dark:bg-gray-700/50 p-0.5 space-x-1">
                                    <ToggleButton label="OR" isActive={filters.includeMsgMode === 'OR'} onClick={() => handleModeChange('includeMsgMode', 'OR')} />
                                    <ToggleButton label="AND" isActive={filters.includeMsgMode === 'AND'} onClick={() => handleModeChange('includeMsgMode', 'AND')} />
                                </div>
                            </div>
                            <textarea
                                name="includeMsg"
                                id="includeMsg"
                                value={filters.includeMsg}
                                onChange={handleInputChange}
                                className={textareaStyles}
                                placeholder="e.g. error&#10;timeout"
                                rows={3}
                            />
                             <p className="text-xs text-gray-400 dark:text-gray-500 mt-1">One search term per line.</p>
                        </div>
                        <div>
                            <div className="flex justify-between items-center mb-1.5">
                                <label htmlFor="excludeMsg" className="block text-xs font-medium text-gray-500 dark:text-gray-400">Message does NOT contain</label>
                                <div className="flex items-center rounded-lg bg-gray-200/80 dark:bg-gray-700/50 p-0.5 space-x-1">
                                    <ToggleButton label="AND" isActive={filters.excludeMsgMode === 'AND'} onClick={() => handleModeChange('excludeMsgMode', 'AND')} />
                                    <ToggleButton label="OR" isActive={filters.excludeMsgMode === 'OR'} onClick={() => handleModeChange('excludeMsgMode', 'OR')} />
                                </div>
                            </div>
                            <textarea
                                name="excludeMsg"
                                id="excludeMsg"
                                value={filters.excludeMsg}
                                onChange={handleInputChange}
                                className={textareaStyles}
                                placeholder="e.g. success&#10;heartbeat"
                                rows={3}
                            />
                             <p className="text-xs text-gray-400 dark:text-gray-500 mt-1">One search term per line.</p>
                        </div>
                    </FilterGroup>
                    
                    <div className="border-t border-gray-200 dark:border-gray-700 pt-4">
                        <FilterGroup title="Advanced SQL Query" isActive={isSqlActive} isDirty={isSqlDirty}>
                             <div className="flex items-center justify-between p-2 bg-gray-100 dark:bg-gray-900/50 rounded-lg">
                                <label className="font-semibold text-gray-800 dark:text-gray-200">
                                    Enable SQL Query Filter
                                </label>
                                <ToggleSwitch
                                    enabled={filters.sqlQueryEnabled}
                                    onChange={(val) => handleFieldChange('sqlQueryEnabled', val)}
                                />
                            </div>
                            <div className={`transition-opacity duration-300 ${!filters.sqlQueryEnabled ? 'opacity-50 pointer-events-none' : ''}`}>
                                <SqlEditor
                                    value={filters.sqlQuery}
                                    onChange={(val) => handleFieldChange('sqlQuery', val)}
                                />
                                <div className="flex justify-between items-center mt-2">
                                     <p className="text-xs text-gray-400 dark:text-gray-500">Bypasses all UI filters above.</p>
                                     <button
                                        type="button"
                                        onClick={() => handleFieldChange('sqlQuery', PREDEFINED_SQL_QUERY)}
                                        className="flex items-center gap-1.5 px-2 py-1 text-xs font-semibold text-sky-700 dark:text-sky-300 bg-sky-100 dark:bg-sky-900/40 hover:bg-sky-200 dark:hover:bg-sky-800/50 rounded-md transition-colors"
                                    >
                                        <Icon name="CodeBracketSquare" iconSet={iconSet} className="w-4 h-4" />
                                        Insert Example
                                    </button>
                                </div>
                                <div className="mt-3 text-xs text-gray-500 dark:text-gray-400 p-2.5 bg-gray-100 dark:bg-gray-800/60 rounded-lg border border-gray-200 dark:border-gray-700/50 space-y-1">
                                    <p className="font-semibold text-gray-600 dark:text-gray-300">Query Reference</p>
                                    <p>Table name is <code className="text-sky-700 dark:text-sky-300 font-mono bg-sky-100 dark:bg-sky-900/40 px-1 py-0.5 rounded">logs</code>.</p>
                                    <p>Available columns: <code className="text-sky-700 dark:text-sky-300 font-mono bg-sky-100 dark:bg-sky-900/40 px-1 py-0.5 rounded">id, time, level, sndrtype, sndrname, msg, fileName</code></p>
                                </div>
                            </div>
                        </FilterGroup>
                    </div>

                </div>
            </div>
        </div>
    );
};