import React from 'react';
import { ColumnStyleSettings } from './ColumnStyleSettings.tsx';
import { ColumnStyles, ViewMode, IconSet, LogTableDensity } from '../types.ts';
import { JsonSyntaxHighlighter } from './JsonSyntaxHighlighter.tsx';
import { Icon } from './icons/index.tsx';
import { DensityControl } from './DensityControl.tsx';

type Theme = 'light' | 'dark';

interface SettingsProps {
  theme: Theme;
  onThemeChange: (newTheme: Theme) => void;
  viewMode: ViewMode;
  onViewModeChange: (newMode: ViewMode) => void;
  iconSet: IconSet;
  onIconSetChange: (newIconSet: IconSet) => void;
  columnStyles: ColumnStyles;
  onColumnStylesChange: (newStyles: ColumnStyles) => void;
  isTimeRangeSelectorVisible: boolean;
  onTimeRangeSelectorVisibilityChange: (newVisibility: boolean) => void;
  logTableDensity: LogTableDensity;
  onLogTableDensityChange: (newDensity: LogTableDensity) => void;
  allowPrerelease: boolean;
  onAllowPrereleaseChange: (allow: boolean) => void;
  uiScale: number;
  onUiScaleChange: (newScale: number) => void;
}

const ToggleSwitch: React.FC<{
  label: string;
  enabled: boolean;
  onChange: (enabled: boolean) => void;
}> = ({ label, enabled, onChange }) => {
  return (
    <div className="flex items-center justify-between">
      <span className="font-medium text-gray-800 dark:text-gray-200">{label}</span>
      <button
        type="button"
        className={`relative inline-flex h-6 w-11 flex-shrink-0 cursor-pointer rounded-full border-2 border-transparent transition-colors duration-200 ease-in-out focus:outline-none focus:ring-2 focus:ring-sky-500 focus:ring-offset-2 focus:ring-offset-white dark:focus:ring-offset-gray-800 ${
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
    </div>
  );
};

const SegmentedControl: React.FC<{
  label: string;
  value: string;
  options: { label: string; value: string }[];
  onChange: (value: any) => void;
}> = ({ label, value, options, onChange }) => (
    <div className="flex items-center justify-between">
        <span className="font-medium text-gray-800 dark:text-gray-200">{label}</span>
        <div className="flex items-center gap-1 p-0.5 bg-gray-200 dark:bg-gray-700/80 rounded-lg">
            {options.map(opt => {
                const isActive = value === opt.value;
                return (
                    <button 
                        key={opt.value}
                        onClick={() => onChange(opt.value)} 
                        className={`px-3 py-1 text-sm font-semibold rounded-md transition-colors ${
                            isActive 
                                ? "bg-white dark:bg-gray-600 text-sky-700 dark:text-white shadow-sm"
                                : "text-gray-500 dark:text-gray-400 hover:bg-white/60 dark:hover:bg-gray-500/50"
                        }`}
                    >
                        {opt.label}
                    </button>
                )
            })}
        </div>
    </div>
);

const TabButton: React.FC<{ label: string; isActive: boolean; onClick: () => void; }> = ({ label, isActive, onClick }) => {
    return (
        <button
            onClick={onClick}
            className={`whitespace-nowrap border-b-2 py-3 px-1 text-sm font-medium transition-colors
                ${isActive
                    ? 'border-sky-500 text-sky-600 dark:border-sky-400 dark:text-sky-400'
                    : 'border-transparent text-gray-500 hover:border-gray-300 hover:text-gray-700 dark:hover:border-gray-600 dark:hover:text-gray-300'
                }
            `}
        >
            {label}
        </button>
    );
};


export const Settings: React.FC<SettingsProps> = ({ 
    theme, onThemeChange, 
    viewMode, onViewModeChange, 
    iconSet, onIconSetChange,
    columnStyles, onColumnStylesChange,
    isTimeRangeSelectorVisible, onTimeRangeSelectorVisibilityChange,
    logTableDensity, onLogTableDensityChange,
    allowPrerelease, onAllowPrereleaseChange,
    uiScale, onUiScaleChange
}) => {
    const [settings, setSettings] = React.useState<any>(null);
    const [settingsPath, setSettingsPath] = React.useState<string>('');
    const [error, setError] = React.useState<string>('');
    const [activeTab, setActiveTab] = React.useState<'controls' | 'json'>('controls');

    React.useEffect(() => {
        if (!window.electronAPI) {
            return;
        }

        const fetchSettingsData = async () => {
            try {
                const fetchedSettings = await window.electronAPI.getSettings();
                const fetchedPath = await window.electronAPI.getSettingsPath();
                setSettings(fetchedSettings);
                setSettingsPath(fetchedPath);
            } catch (e) {
                const message = e instanceof Error ? e.message : 'An unknown error occurred';
                setError(`Failed to load settings: ${message}`);
                console.error(e);
            }
        };
        fetchSettingsData();
    }, []);

    const handleOpenFile = () => {
        if (window.electronAPI) {
            window.electronAPI.showSettingsFile();
        }
    }

    return (
        <div className="flex-grow overflow-y-auto bg-gray-100 dark:bg-gray-900 text-gray-900 dark:text-gray-300">
            <div className="sticky top-0 z-10 bg-gray-100 dark:bg-gray-900 border-b border-gray-200 dark:border-gray-700">
                <nav className="-mb-px flex space-x-8 px-4 sm:px-6 lg:px-8" aria-label="Tabs">
                    <TabButton label="Controls" isActive={activeTab === 'controls'} onClick={() => setActiveTab('controls')} />
                    {window.electronAPI && (
                        <TabButton label="JSON Source" isActive={activeTab === 'json'} onClick={() => setActiveTab('json')} />
                    )}
                </nav>
            </div>

            <div className="p-4 sm:p-6 lg:p-8">
                {error && (
                    <div className="bg-red-100 dark:bg-red-900/50 border border-red-300 dark:border-red-700 text-red-700 dark:text-red-300 rounded-lg p-4 text-center mb-6">
                        <p className="font-bold">Error</p>
                        <p>{error}</p>
                    </div>
                )}
                
                {activeTab === 'controls' && (
                    <div className="grid grid-cols-1 lg:grid-cols-3 gap-8 items-start">
                         <div className="lg:col-span-1">
                            <div className="bg-white dark:bg-gray-800/50 p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10">
                                <h2 className="text-xl font-semibold text-gray-800 dark:text-sky-400 mb-4">Appearance</h2>
                                <div className="space-y-4">
                                    <ToggleSwitch 
                                        label="Dark Mode"
                                        enabled={theme === 'dark'}
                                        onChange={(enabled) => onThemeChange(enabled ? 'dark' : 'light')}
                                    />
                                    {window.electronAPI && (
                                        <ToggleSwitch 
                                            label="Update to Pre-releases"
                                            enabled={allowPrerelease}
                                            onChange={onAllowPrereleaseChange}
                                        />
                                    )}
                                     <div className="flex items-center justify-between">
                                        <span className="font-medium text-gray-800 dark:text-gray-200">UI Scale</span>
                                        <div className="flex items-center gap-2">
                                            <input
                                                type="range"
                                                min="0.5"
                                                max="4"
                                                step="0.05"
                                                value={uiScale}
                                                onChange={(e) => onUiScaleChange(parseFloat(e.target.value))}
                                                className="w-32 h-2 bg-gray-200 rounded-lg appearance-none cursor-pointer dark:bg-gray-700"
                                            />
                                            <span className="w-16 text-center font-mono text-gray-700 dark:text-gray-300 bg-gray-200 dark:bg-gray-700/80 rounded-md px-2 py-1 text-sm">
                                                {(uiScale * 100).toFixed(0)}%
                                            </span>
                                            <button
                                                onClick={() => onUiScaleChange(1)}
                                                disabled={uiScale === 1}
                                                className="p-1.5 text-gray-500 dark:text-gray-400 rounded-md hover:bg-gray-200 dark:hover:bg-gray-600 disabled:opacity-50 disabled:cursor-not-allowed"
                                                title="Reset to 100%"
                                            >
                                                <Icon name="ArrowPath" iconSet={iconSet} className="w-4 h-4" />
                                            </button>
                                        </div>
                                    </div>
                                    <SegmentedControl
                                        label="Log Viewer Mode"
                                        value={viewMode}
                                        onChange={onViewModeChange}
                                        options={[
                                            { label: 'Paginate', value: 'pagination' },
                                            { label: 'Scroll', value: 'scroll' },
                                        ]}
                                    />
                                    <div className="flex items-center justify-between">
                                        <span className="font-medium text-gray-800 dark:text-gray-200">Log Row Density</span>
                                        <DensityControl value={logTableDensity} onChange={onLogTableDensityChange} />
                                    </div>
                                    <ToggleSwitch
                                        label="Show Timeline by Default"
                                        enabled={isTimeRangeSelectorVisible}
                                        onChange={onTimeRangeSelectorVisibilityChange}
                                    />
                                    <SegmentedControl
                                        label="Icon Set"
                                        value={iconSet}
                                        onChange={onIconSetChange}
                                        options={[
                                            { label: 'Sharp', value: 'sharp' },
                                            { label: 'Solid', value: 'solid' },
                                            { label: 'Feather', value: 'feather' },
                                            { label: 'Tabler', value: 'tabler' },
                                            { label: 'Lucide', value: 'lucide' },
                                        ]}
                                    />
                                </div>
                            </div>
                         </div>

                         <div className="lg:col-span-2">
                            <div className="bg-white dark:bg-gray-800/50 p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10">
                                <h2 className="text-xl font-semibold text-gray-800 dark:text-sky-400 mb-4">Log Table Styles</h2>
                                <p className="mb-4 text-sm text-gray-600 dark:text-gray-400">
                                    Customize the font, style, and color for each column in the Log Viewer. Changes are saved automatically.
                                </p>
                                <ColumnStyleSettings styles={columnStyles} onChange={onColumnStylesChange} />
                            </div>
                         </div>
                    </div>
                )}
                
                {activeTab === 'json' && window.electronAPI && (
                     <div className="grid grid-cols-1 lg:grid-cols-3 gap-8 items-start">
                         <div className="lg:col-span-1">
                             <div className="bg-white dark:bg-gray-800/50 p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10">
                                <h2 className="text-xl font-semibold text-gray-800 dark:text-sky-400 mb-4">Settings File</h2>
                                <p className="mb-4 text-sm text-gray-600 dark:text-gray-400">
                                    For advanced configuration, you can edit the settings file directly. Click the button below to show the file in its folder.
                                </p>
                                <div className="flex flex-col sm:flex-row gap-4 items-start sm:items-center">
                                    <button
                                        onClick={handleOpenFile}
                                        disabled={!window.electronAPI}
                                        className="bg-sky-600 hover:bg-sky-700 text-white font-semibold rounded-lg px-4 py-2 transition-colors duration-200 whitespace-nowrap disabled:opacity-50 disabled:cursor-not-allowed"
                                    >
                                        Show Settings File
                                    </button>
                                    <code className="bg-gray-100 dark:bg-gray-900/70 p-2 rounded-md text-sm text-gray-500 dark:text-gray-400 break-all w-full">
                                        {settingsPath || 'Loading path...'}
                                    </code>
                                </div>
                            </div>
                         </div>

                         <div className="lg:col-span-2">
                            <div className="bg-white dark:bg-gray-800/50 p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 h-full">
                                <h2 className="text-xl font-semibold text-gray-800 dark:text-300 mb-4">File Content</h2>
                                {settings ? (
                                    <JsonSyntaxHighlighter jsonString={JSON.stringify(settings, null, 2)} />
                                ) : (
                                    <p className="text-gray-500 dark:text-gray-400">Loading settings...</p>
                                )}
                            </div>
                         </div>
                     </div>
                )}
            </div>
        </div>
    );
}