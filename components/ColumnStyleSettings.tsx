import React from 'react';
import { ColumnKey, ColumnStyle, ColumnStyles } from '../types';
import { COLUMN_DEFINITIONS } from '../utils';

const FONT_FAMILIES_WEB = ['sans-serif', 'serif', 'monospace', 'system-ui', 'inherit'];

const LIGHT_THEME_SWATCHES = [
    '#334155', '#1f2937', '#dc2626', '#ea580c', '#f59e0b', '#84cc16', '#16a34a', '#059669',
    '#14b8a6', '#06b6d4', '#0ea5e9', '#2563eb', '#4f46e5', '#7c3aed', '#9333ea', '#c026d3'
];
const DARK_THEME_SWATCHES = [
    '#cbd5e1', '#e5e7eb', '#f87171', '#fb923c', '#facc15', '#a3e635', '#4ade80', '#34d399',
    '#2dd4bf', '#22d3ee', '#38bdf8', '#60a5fa', '#818cf8', '#a78bfa', '#c084fc', '#f472b6'
];

const inputBaseClasses = "w-full bg-gray-100 dark:bg-gray-700/80 border-gray-300 dark:border-gray-600 text-gray-900 dark:text-white sm:text-sm rounded-md shadow-sm focus:ring-sky-500 focus:border-sky-500 transition";
const colorInputClasses = "p-0 h-8 w-12 rounded-md cursor-pointer disabled:opacity-50 disabled:cursor-not-allowed border-gray-300 dark:border-gray-600";

interface ColorSwatchesProps {
    colors: string[];
    onSelect: (color: string) => void;
}

const ColorSwatches: React.FC<ColorSwatchesProps> = ({ colors, onSelect }) => (
    <div className="flex items-center gap-1.5 mt-1.5 flex-wrap">
        {colors.map(color => (
            <button
                key={color}
                type="button"
                className="w-5 h-5 rounded-full ring-1 ring-inset ring-black/20"
                style={{ backgroundColor: color }}
                onClick={() => onSelect(color)}
                aria-label={`Select color ${color}`}
            />
        ))}
    </div>
);


interface ColumnStyleEditorProps {
    columnKey: ColumnKey;
    label: string;
    style: ColumnStyle;
    fontList: string[];
    onChange: (key: ColumnKey, style: ColumnStyle) => void;
}

const ColumnStyleEditor: React.FC<ColumnStyleEditorProps> = ({ columnKey, label, style, fontList, onChange }) => {
    
    const handleChange = <K extends keyof ColumnStyle>(key: K, value: ColumnStyle[K]) => {
        onChange(columnKey, { ...style, [key]: value });
    };

    const handleFontSizeChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        const size = parseInt(e.target.value, 10);
        if (!isNaN(size)) {
            handleChange('fontSize', size);
        }
    };

    return (
        <div className="grid grid-cols-1 md:grid-cols-6 gap-x-4 gap-y-2 items-center p-3 border-b border-gray-200 dark:border-gray-700 last:border-b-0">
            <div className="font-semibold text-gray-800 dark:text-gray-200">{label}</div>
            
            <div>
                <select 
                    value={style.fontFamily}
                    onChange={e => handleChange('fontFamily', e.target.value)}
                    className={inputBaseClasses}
                    aria-label={`${label} font family`}
                >
                    {fontList.map(f => <option key={f} value={f}>{f}</option>)}
                </select>
            </div>

             <div className="relative">
                <input
                    type="number"
                    value={style.fontSize}
                    onChange={handleFontSizeChange}
                    className={`${inputBaseClasses} pr-7`}
                    min="8"
                    max="48"
                    aria-label={`${label} font size`}
                />
                <span className="absolute inset-y-0 right-2 flex items-center text-gray-500 text-sm">px</span>
            </div>
            
            <div className="flex items-center gap-2">
                <button
                    onClick={() => handleChange('isBold', !style.isBold)}
                    className={`px-3 py-1 text-sm font-bold rounded-md transition-colors w-10 ${style.isBold ? 'bg-sky-600 text-white' : 'bg-gray-200 dark:bg-gray-600 text-gray-700 dark:text-gray-300'}`}
                    aria-pressed={style.isBold}
                    title={`${label} bold`}
                >
                    B
                </button>
                <button
                    onClick={() => handleChange('isItalic', !style.isItalic)}
                    className={`px-3 py-1 text-sm italic rounded-md transition-colors w-10 ${style.isItalic ? 'bg-sky-600 text-white' : 'bg-gray-200 dark:bg-gray-600 text-gray-700 dark:text-gray-300'}`}
                    aria-pressed={style.isItalic}
                    title={`${label} italic`}
                >
                    I
                </button>
            </div>
            
            <div>
                <input 
                    type="color"
                    value={style.color}
                    onChange={e => handleChange('color', e.target.value)}
                    className={colorInputClasses}
                    disabled={columnKey === 'level'}
                    aria-label={`${label} light theme color`}
                    title={columnKey === 'level' ? "Color for 'Level' is handled automatically by its class." : "Light theme color"}
                />
                 {columnKey !== 'level' && <ColorSwatches colors={LIGHT_THEME_SWATCHES} onSelect={color => handleChange('color', color)} />}
            </div>

            <div>
                 <input 
                    type="color"
                    value={style.darkColor}
                    onChange={e => handleChange('darkColor', e.target.value)}
                    className={colorInputClasses}
                    disabled={columnKey === 'level'}
                    aria-label={`${label} dark theme color`}
                    title={columnKey === 'level' ? "Color for 'Level' is handled automatically by its class." : "Dark theme color"}
                />
                {columnKey !== 'level' && <ColorSwatches colors={DARK_THEME_SWATCHES} onSelect={color => handleChange('darkColor', color)} />}
            </div>
        </div>
    );
};


interface ColumnStyleSettingsProps {
    styles: ColumnStyles;
    onChange: (newStyles: ColumnStyles) => void;
}

export const ColumnStyleSettings: React.FC<ColumnStyleSettingsProps> = ({ styles, onChange }) => {
    const [systemFonts, setSystemFonts] = React.useState<string[] | null>(null);
    const [isLoadingFonts, setIsLoadingFonts] = React.useState(true);

    React.useEffect(() => {
        if (window.electronAPI?.getSystemFonts) {
            window.electronAPI.getSystemFonts()
                .then(fonts => {
                    // Add web-safe fonts as a fallback and for consistency
                    const fullList = [...new Set([...FONT_FAMILIES_WEB, ...fonts])].sort();
                    setSystemFonts(fullList);
                })
                .catch(err => {
                    console.error("Failed to load system fonts:", err);
                    setSystemFonts(FONT_FAMILIES_WEB); // Fallback on error
                })
                .finally(() => {
                    setIsLoadingFonts(false);
                });
        } else {
            setSystemFonts(FONT_FAMILIES_WEB);
            setIsLoadingFonts(false);
        }
    }, []);
    
    const handleStyleChange = (key: ColumnKey, newStyle: ColumnStyle) => {
        onChange({ ...styles, [key]: newStyle });
    };

    const fontList = isLoadingFonts ? ['Loading fonts...'] : (systemFonts || FONT_FAMILIES_WEB);

    return (
        <div className="space-y-2">
             <div className="grid grid-cols-1 md:grid-cols-6 gap-x-4 gap-y-2 items-center px-3 pb-2 text-xs font-semibold text-gray-500 dark:text-gray-400 uppercase">
                <span>Column</span>
                <span>Font Family</span>
                <span>Font Size</span>
                <span>Style</span>
                <span>Light Color</span>
                <span>Dark Color</span>
            </div>
            <div className="rounded-lg border border-gray-200 dark:border-gray-700">
                {COLUMN_DEFINITIONS.map(({ key, label }) => (
                    styles[key] &&
                    <ColumnStyleEditor
                        key={key}
                        columnKey={key}
                        label={label}
                        style={styles[key]}
                        fontList={fontList}
                        onChange={handleStyleChange}
                    />
                ))}
            </div>
        </div>
    );
};