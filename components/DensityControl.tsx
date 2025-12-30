import React from 'react';
import { LogTableDensity } from '../types.ts';

interface DensityControlProps {
    value: LogTableDensity;
    onChange: (density: LogTableDensity) => void;
}

const options: { value: LogTableDensity; label: string }[] = [
    { value: 'compact', label: 'Compact' },
    { value: 'normal', label: 'Normal' },
    { value: 'comfortable', label: 'Comfortable' },
];

export const DensityControl: React.FC<DensityControlProps> = ({ value, onChange }) => {
    return (
        <div className="flex items-center gap-1 p-0.5 bg-gray-200 dark:bg-gray-700/80 rounded-lg">
            {options.map(opt => {
                const isActive = value === opt.value;
                return (
                    <button
                        key={opt.value}
                        onClick={() => onChange(opt.value)}
                        className={`px-2 py-0.5 text-xs font-semibold rounded-md transition-colors ${isActive
                                ? "bg-white dark:bg-gray-600 text-sky-700 dark:text-white shadow-sm"
                                : "text-gray-500 dark:text-gray-400 hover:bg-white/60 dark:hover:bg-gray-500/50"
                            }`}
                    >
                        {opt.label}
                    </button>
                )
            })}
        </div>
    );
};
