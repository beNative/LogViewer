import React from 'react';
import { ColumnStyleSettings } from './ColumnStyleSettings';
import { ColumnStyles, ViewMode, IconSet, LogTableDensity, Settings as SettingsType, ColumnVisibilityState, PanelWidths, FilterState, TimelineBarVisibility } from '../types';
import { JsonEditor } from './JsonEditor';
import { Icon, IconName } from './icons';
import { DensityControl } from './DensityControl';

type Theme = 'light' | 'dark';
type SettingsCategory = 'appearance' | 'behavior' | 'updates' | 'integrations' | 'styles' | 'debugging';

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
  isDetailPanelVisible: boolean;
  onDetailPanelVisibilityChange: (newVisibility: boolean) => void;
  isFocusDebuggerVisible: boolean;
  onFocusDebuggerVisibilityChange: (isVisible: boolean) => void;
  logTableDensity: LogTableDensity;
  onLogTableDensityChange: (newDensity: LogTableDensity) => void;
  allowPrerelease: boolean;
  onAllowPrereleaseChange: (allow: boolean) => void;
  isAutoUpdateEnabled: boolean;
  onAutoUpdateEnabledChange: (enabled: boolean) => void;
  githubToken: string;
  onGithubTokenChange: (token: string) => void;
  uiScale: number;
  onUiScaleChange: (newScale: number) => void;
  onFullSettingsUpdate: (newSettings: SettingsType) => Promise<void>;
  // Props needed for constructing the full settings object
  columnVisibility: ColumnVisibilityState;
  customFilterPresets: Record<string, FilterState>;
  panelWidths: PanelWidths;
  timelineBarVisibility: TimelineBarVisibility;
  onTimelineBarVisibilityChange: (newVisibility: TimelineBarVisibility) => void;
}

const ToggleSwitch: React.FC<{
  label: string;
  enabled: boolean;
  onChange: (enabled: boolean) => void;
}> = ({ label, enabled, onChange }) => (
    <div className="flex items-center justify-between py-3 border-b border-gray-200 dark:border-gray-700">
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

const SegmentedControl: React.FC<{
  label: string;
  value: string;
  options: { label: string; value: string }[];
  onChange: (value: any) => void;
}> = ({ label, value, options, onChange }) => (
    <div className="flex items-center justify-between py-3 border-b border-gray-200 dark:border-gray-700">
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

const TabButton: React.FC<{ label: string; isActive: boolean; onClick: () => void; }> = ({ label, isActive, onClick }) => (
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

const CategoryButton: React.FC<{ label: string; icon: IconName; isActive: boolean; onClick: () => void; }> = ({ label, icon, isActive, onClick }) => (
    <button
        onClick={onClick}
        className={`flex items-center gap-3 w-full text-left px-3 py-2.5 text-sm font-medium rounded-md transition-colors duration-150 ${
            isActive
            ? 'bg-sky-100 dark:bg-sky-900/50 text-sky-700 dark:text-sky-300'
            : 'text-gray-600 dark:text-gray-400 hover:bg-gray-100 dark:hover:bg-gray-700/50 hover:text-gray-900 dark:hover:text-gray-200'
        }`}
    >
        <Icon name={icon} iconSet="sharp" className="w-5 h-5 flex-shrink-0" />
        {label}
    </button>
);


export const Settings: React.FC<SettingsProps> = (props) => {
    const { 
        theme, onThemeChange, 
        viewMode, onViewModeChange, 
        iconSet, onIconSetChange,
        columnStyles, onColumnStylesChange,
        isTimeRangeSelectorVisible, onTimeRangeSelectorVisibilityChange,
        isDetailPanelVisible, onDetailPanelVisibilityChange,
        isFocusDebuggerVisible, onFocusDebuggerVisibilityChange,
        timelineBarVisibility, onTimelineBarVisibilityChange,
        logTableDensity, onLogTableDensityChange,
        allowPrerelease, onAllowPrereleaseChange,
        isAutoUpdateEnabled, onAutoUpdateEnabledChange,
        githubToken, onGithubTokenChange,
        uiScale, onUiScaleChange,
        onFullSettingsUpdate
    } = props;
    
    const [savedSettings, setSavedSettings] = React.useState<SettingsType | null>(null);
    const [settingsPath, setSettingsPath] = React.useState<string>('');
    const [error, setError] = React.useState<string>('');
    const [activeTab, setActiveTab] = React.useState<'controls' | 'json'>('controls');
    const [activeCategory, setActiveCategory] = React.useState<SettingsCategory>('appearance');
    const [jsonText, setJsonText] = React.useState('');
    const [isJsonValid, setIsJsonValid] = React.useState(true);
    const [isDirty, setIsDirty] = React.useState(false);
    const importInputRef = React.useRef<HTMLInputElement>(null);

    // Local state for UI scale slider
    const [tempUiScale, setTempUiScale] = React.useState(uiScale);
    const isScaleDirty = tempUiScale !== uiScale;

     // Local state for GitHub token
    const [localGithubToken, setLocalGithubToken] = React.useState(githubToken);
    const [isTokenVisible, setIsTokenVisible] = React.useState(false);
    const isTokenDirty = localGithubToken !== githubToken;
    
    React.useEffect(() => {
        setTempUiScale(uiScale);
        setLocalGithubToken(githubToken);
    }, [uiScale, githubToken]);


    React.useEffect(() => {
        if (!window.electronAPI) return;
        const fetchSettingsData = async () => {
            try {
                const fetchedSettings = await window.electronAPI.getSettings();
                const fetchedPath = await window.electronAPI.getSettingsPath();
                setSavedSettings(fetchedSettings);
                setSettingsPath(fetchedPath);
                setJsonText(JSON.stringify(fetchedSettings, null, 2));
                setIsJsonValid(true);
                setIsDirty(false);
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

    const handleJsonChange = (newText: string) => {
        setJsonText(newText);
        try {
            const parsed = JSON.parse(newText);
            setIsJsonValid(true);
            setIsDirty(JSON.stringify(parsed, null, 2) !== JSON.stringify(savedSettings, null, 2));
        } catch (e) {
            setIsJsonValid(false);
            setIsDirty(true);
        }
    };

    const handleSaveChanges = async () => {
        if (!isJsonValid) {
            alert("Cannot save: The JSON is invalid.");
            return;
        }
        const newSettings = JSON.parse(jsonText);
        await onFullSettingsUpdate(newSettings);
        setSavedSettings(newSettings);
        setIsDirty(false);
    };

    const handleDiscardChanges = () => {
        if (savedSettings) {
            setJsonText(JSON.stringify(savedSettings, null, 2));
            setIsJsonValid(true);
            setIsDirty(false);
        }
    };

    const handleExport = () => {
        const blob = new Blob([jsonText], { type: 'application/json' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'log-analyser-settings.json';
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
    };

    const handleImportClick = () => {
        importInputRef.current?.click();
    };

    const handleFileImport = (e: React.ChangeEvent<HTMLInputElement>) => {
        const file = e.target.files?.[0];
        if (!file) return;

        const reader = new FileReader();
        reader.onload = (event) => {
            const content = event.target?.result as string;
            try {
                JSON.parse(content);
                handleJsonChange(JSON.stringify(JSON.parse(content), null, 2));
            } catch (error) {
                alert('Invalid JSON file. Please select a valid settings file.');
            }
        };
        reader.readAsText(file);
        e.target.value = '';
    };

    const handleSaveToken = () => {
        onGithubTokenChange(localGithubToken);
    };


    const ControlsView = (
        <div className="flex flex-grow min-h-0">
            <aside className="w-64 flex-shrink-0 bg-gray-50 dark:bg-gray-800 border-r border-gray-200 dark:border-gray-700 p-2">
                <nav className="space-y-1">
                    <CategoryButton label="Appearance" icon="Sun" isActive={activeCategory === 'appearance'} onClick={() => setActiveCategory('appearance')} />
                    <CategoryButton label="Behavior" icon="Cog" isActive={activeCategory === 'behavior'} onClick={() => setActiveCategory('behavior')} />
                    <CategoryButton label="Updates" icon="ArrowPath" isActive={activeCategory === 'updates'} onClick={() => setActiveCategory('updates')} />
                    <CategoryButton label="Integrations" icon="CodeBracketSquare" isActive={activeCategory === 'integrations'} onClick={() => setActiveCategory('integrations')} />
                    <CategoryButton label="Table Styles" icon="Table" isActive={activeCategory === 'styles'} onClick={() => setActiveCategory('styles')} />
                    <CategoryButton label="Debugging" icon="BugAnt" isActive={activeCategory === 'debugging'} onClick={() => setActiveCategory('debugging')} />
                </nav>
            </aside>
            <main className="flex-1 p-6 sm:p-8 overflow-y-auto">
                 <div className="max-w-4xl mx-auto">
                    {activeCategory === 'appearance' && (
                        <div className="divide-y divide-gray-200 dark:divide-gray-700">
                             <h2 className="text-xl font-bold text-gray-800 dark:text-sky-400 pb-4">Appearance</h2>
                             <ToggleSwitch label="Dark Mode" enabled={theme === 'dark'} onChange={(enabled) => onThemeChange(enabled ? 'dark' : 'light')} />
                             <div className="flex items-center justify-between py-3 border-b border-gray-200 dark:border-gray-700">
                                <span className="font-medium text-gray-800 dark:text-gray-200">UI Scale</span>
                                <div className="flex items-center gap-2">
                                    <input type="range" min="0.5" max="4" step="0.05" value={tempUiScale} onChange={(e) => setTempUiScale(parseFloat(e.target.value))} className="w-32 h-2 bg-gray-200 rounded-lg appearance-none cursor-pointer dark:bg-gray-700" />
                                    <span className="w-16 text-center font-mono text-gray-700 dark:text-gray-300 bg-gray-200 dark:bg-gray-700/80 rounded-md px-2 py-1 text-sm">{(tempUiScale * 100).toFixed(0)}%</span>
                                    <button onClick={() => setTempUiScale(1)} disabled={tempUiScale === 1} className="p-1.5 text-gray-500 dark:text-gray-400 rounded-md hover:bg-gray-200 dark:hover:bg-gray-600 disabled:opacity-50" title="Reset to 100%">
                                        <Icon name="ArrowPath" iconSet={iconSet} className="w-4 h-4" />
                                    </button>
                                    <button onClick={() => onUiScaleChange(tempUiScale)} disabled={!isScaleDirty} className="px-3 py-1.5 text-sm font-semibold rounded-md transition-colors bg-sky-600 text-white hover:bg-sky-700 disabled:bg-gray-400 disabled:dark:bg-gray-600 disabled:cursor-not-allowed">Apply</button>
                                </div>
                            </div>
                            <div className="flex items-center justify-between py-3 border-b border-gray-200 dark:border-gray-700">
// FIX: The component was missing its return statement, causing a type error. The main JSX structure for the component has been added.
    return (
        <div className="flex flex-col h-full bg-white dark:bg-gray-800">
            <div className="flex-shrink-0 border-b border-gray-200 dark:border-gray-700">
                <div className="flex items-center justify-between px-4 sm:px-6 lg:px-8">
                    <div className="flex border-b-2 border-transparent -mb-px space-x-6">
                        <TabButton label="UI Controls" isActive={activeTab === 'controls'} onClick={() => setActiveTab('controls')} />
                        <TabButton label="JSON Editor" isActive={activeTab === 'json'} onClick={() => setActiveTab('json')} />
                    </div>
                    {activeTab === 'json' && window.electronAPI && (
                        <div className="flex items-center gap-2 py-2">
                            <button onClick={handleImportClick} className="flex items-center gap-2 px-3 py-1.5 text-sm font-semibold text-gray-700 dark:text-gray-300 bg-gray-200/80 dark:bg-gray-700/50 hover:bg-gray-300 dark:hover:bg-gray-600 rounded-md transition-colors">
                                <Icon name="ArrowUpTray" iconSet={iconSet} className="w-4 h-4" />
                                Import...
                            </button>
                            <input type="file" ref={importInputRef} onChange={handleFileImport} className="hidden" accept=".json" />
                            <button onClick={handleExport} className="flex items-center gap-2 px-3 py-1.5 text-sm font-semibold text-gray-700 dark:text-gray-300 bg-gray-200/80 dark:bg-gray-700/50 hover:bg-gray-300 dark:hover:bg-gray-600 rounded-md transition-colors">
                                <Icon name="Download" iconSet={iconSet} className="w-4 h-4" />
                                Export...
                            </button>
                        </div>
                    )}
                </div>
            </div>

            {activeTab === 'controls' ? (
                ControlsView
            ) : (
                <div className="flex flex-col flex-grow min-h-0 p-4 sm:p-6 lg:p-8 space-y-4">
                    <div className="flex-grow relative">
                        <JsonEditor value={jsonText} onChange={handleJsonChange} />
                    </div>
                    <div className="flex-shrink-0 flex justify-between items-center">
                        <div className="text-sm">
                            {!isJsonValid ? (
                                <span className="text-red-600 dark:text-red-400 font-semibold">Invalid JSON</span>
                            ) : (
                                <span className="text-gray-500 dark:text-gray-400">Settings file path: <button onClick={handleOpenFile} className="underline hover:text-sky-500">{settingsPath}</button></span>
                            )}
                        </div>
                        <div className="flex items-center gap-2">
                            <button onClick={handleDiscardChanges} disabled={!isDirty} className="px-3 py-1.5 text-sm bg-red-600 hover:bg-red-700 text-white font-semibold rounded-md transition-colors duration-200 disabled:opacity-50 disabled:cursor-not-allowed">
                                Discard Changes
                            </button>
                            <button onClick={handleSaveChanges} disabled={!isDirty || !isJsonValid} className="px-3 py-1.5 text-sm bg-sky-600 hover:bg-sky-700 text-white font-semibold rounded-md transition-colors duration-200 disabled:opacity-50 disabled:cursor-not-allowed">
                                Save Changes
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
};
