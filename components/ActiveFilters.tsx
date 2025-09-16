import React from 'react';
import { FilterState, IconSet } from '../types.ts';
import { Icon } from './icons/index.tsx';
import { Tooltip } from './Tooltip.tsx';

interface ActiveFiltersProps {
    appliedFilters: FilterState;
    onRemoveFilter: (key: keyof FilterState, value?: string) => void;
    iconSet: IconSet;
}

const filterKeyToLabel: Record<string, string> = {
    dateFrom: 'From',
    timeFrom: 'From',
    dateTo: 'To',
    timeTo: 'To',
    level: 'Level',
    sndrtype: 'Sender Type',
    sndrname: 'Sender Name',
    fileName: 'Filename',
    includeMsg: 'Message Contains',
    excludeMsg: 'Message Excludes',
    sqlQuery: 'SQL Query',
    sqlQueryEnabled: 'SQL Query Active',
};

const Pill: React.FC<{ label: string; onRemove: () => void; iconSet: IconSet }> = ({ label, onRemove, iconSet }) => (
    <div className="flex items-center gap-1.5 bg-sky-100 dark:bg-sky-900/60 text-sky-800 dark:text-sky-200 text-sm font-medium pl-3 pr-1.5 py-1 rounded-full animate-fadeIn">
        <Tooltip content={label}>
            <span className="truncate">{label}</span>
        </Tooltip>
        <Tooltip content={`Remove filter: ${label}`}>
            <button 
                onClick={onRemove} 
                className="flex-shrink-0 p-0.5 rounded-full hover:bg-sky-200/70 dark:hover:bg-sky-800/70 text-sky-600 dark:text-sky-300"
                aria-label={`Remove filter: ${label}`}
            >
                <Icon name="XCircle" iconSet={iconSet} className="w-4 h-4" />
            </button>
        </Tooltip>
    </div>
);

const ExcludePill: React.FC<{ label: string; onRemove: () => void; iconSet: IconSet }> = ({ label, onRemove, iconSet }) => (
    <div className="flex items-center gap-1.5 bg-red-100 dark:bg-red-900/60 text-red-800 dark:text-red-200 text-sm font-medium pl-3 pr-1.5 py-1 rounded-full animate-fadeIn">
        <Tooltip content={label}>
             <span className="truncate">{label}</span>
        </Tooltip>
        <Tooltip content={`Remove filter: ${label}`}>
            <button 
                onClick={onRemove} 
                className="flex-shrink-0 p-0.5 rounded-full hover:bg-red-200/70 dark:hover:bg-red-800/70 text-red-600 dark:text-red-300"
                aria-label={`Remove filter: ${label}`}
            >
                <Icon name="XCircle" iconSet={iconSet} className="w-4 h-4" />
            </button>
        </Tooltip>
    </div>
);


export const ActiveFilters: React.FC<ActiveFiltersProps> = ({ appliedFilters, onRemoveFilter, iconSet }) => {
    const activeFilterPills = React.useMemo(() => {
        const pills: React.ReactNode[] = [];
        
        // Date/Time range as one pill
        if (appliedFilters.dateFrom || appliedFilters.dateTo) {
            const from = `${appliedFilters.dateFrom || '...'} ${appliedFilters.timeFrom || ''}`.trim();
            const to = `${appliedFilters.dateTo || '...'} ${appliedFilters.timeTo || ''}`.trim();
            pills.push(
                <Pill 
                    key="daterange"
                    label={`Time: ${from} → ${to}`}
                    onRemove={() => {
                        onRemoveFilter('dateFrom');
                        onRemoveFilter('timeFrom');
                        onRemoveFilter('dateTo');
                        onRemoveFilter('timeTo');
                    }}
                    iconSet={iconSet}
                />
            );
        }

        // Attribute-based filters (both include and exclude)
        const attributeFilters: (keyof FilterState)[] = ['level', 'sndrtype', 'sndrname', 'fileName'];
        attributeFilters.forEach(key => {
            const values = appliedFilters[key];
            const mode = appliedFilters[`${key}FilterMode` as keyof FilterState];
            
            if (Array.isArray(values)) {
                values.forEach(value => {
                    const label = `${filterKeyToLabel[key]}${mode === 'exclude' ? ' NOT' : ''}: ${value}`;
                    const PillComponent = mode === 'exclude' ? ExcludePill : Pill;
                    pills.push(
                        <PillComponent
                            key={`${key}-${value}`}
                            label={label}
                            onRemove={() => onRemoveFilter(key, value)}
                            iconSet={iconSet}
                        />
                    );
                });
            }
        });


        // String-based filters
        const stringFilters: (keyof FilterState)[] = ['includeMsg', 'excludeMsg'];
        stringFilters.forEach(key => {
            if (appliedFilters[key]) {
                const terms = (appliedFilters[key] as string).split('\n').filter(Boolean).join(', ');
                pills.push(
                    <Pill
                        key={key}
                        label={`${filterKeyToLabel[key]}: "${terms}"`}
                        onRemove={() => onRemoveFilter(key)}
                        iconSet={iconSet}
                    />
                );
            }
        });

        // SQL Query
        if (appliedFilters.sqlQueryEnabled && appliedFilters.sqlQuery) {
            pills.push(
                <Pill
                    key="sqlQuery"
                    label="Custom SQL Query Active"
                    onRemove={() => onRemoveFilter('sqlQueryEnabled')}
                    iconSet={iconSet}
                />
            );
        }

        return pills;
    }, [appliedFilters, onRemoveFilter, iconSet]);

    if (activeFilterPills.length === 0) {
        return null;
    }

    return (
        <div className="p-3 border-b border-gray-200 dark:border-gray-700 bg-gray-50 dark:bg-gray-800/50">
            <div className="flex items-center gap-2 flex-wrap">
                <h4 className="text-sm font-semibold text-gray-600 dark:text-gray-300 mr-2 flex-shrink-0">Active Filters:</h4>
                {activeFilterPills}
            </div>
        </div>
    );
};
