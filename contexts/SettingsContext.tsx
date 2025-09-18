import React, { createContext, useState, useCallback, useContext, useEffect } from 'react';
import { Theme, ViewMode, IconSet, LogTableDensity, ColumnVisibilityState, ColumnStyles, PanelWidths, FilterState, TimelineBarVisibility, Settings as SettingsType } from '../types';
import { useConsole } from './ConsoleContext';
import { useToast } from './ToastContext';

// Initial state constants
const MONO_FONT_STACK = 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace';
const initialColumnVisibility: ColumnVisibilityState = { time: true, level: true, sndrtype: true, sndrname: true, fileName: true, msg: true };
const initialColumnStyles: ColumnStyles = {
  time: { font: MONO_FONT_STACK, fontSize: 13, isBold: false, isItalic: false, color: '#6B7280', darkColor: '#9CA3AF' },
  level: { font: 'sans-serif', fontSize: 12, isBold: true, isItalic: false, color: '', darkColor: '' },
  sndrtype: { font: 'sans-serif', fontSize: 14, isBold: false, isItalic: false, color: '#374151', darkColor: '#D1D5DB' },
  sndrname: { font: 'sans-serif', fontSize: 14, isBold: false, isItalic: false, color: '#374151', darkColor: '#D1D5DB' },
  fileName: { font: 'sans-serif', fontSize: 13, isBold: false, isItalic: false, color: '#6B7280', darkColor: '#9CA3AF' },
  msg: { font: MONO_FONT_STACK, fontSize: 13, isBold: false, isItalic: false, color: '#1F2937', darkColor: '#F3F4F6' },
};
const initialPanelWidths: PanelWidths = { filters: 320, details: 500 };
const initialTimelineBarVisibility: TimelineBarVisibility = { pages: true, files: true, dates: true, density: true, overview: true };

type SettingsContextType = {
    theme: Theme;
    viewMode: ViewMode;
    allowPrerelease: boolean;
    isAutoUpdateEnabled: boolean;
    githubToken: string;
    iconSet: IconSet;
    logTableDensity: LogTableDensity;
    columnVisibility: ColumnVisibilityState;
    customFilterPresets: Record<string, FilterState>;
    columnStyles: ColumnStyles;
    panelWidths: PanelWidths;
    isTimeRangeSelectorVisible: boolean;
    isDetailPanelVisible: boolean;
    isFocusDebuggerVisible: boolean;
    timelineBarVisibility: TimelineBarVisibility;
    uiScale: number;
    onThemeChange: (newTheme: Theme) => void;
    onViewModeChange: (newMode: ViewMode) => void;
    onAllowPrereleaseChange: (allow: boolean) => void;
    onAutoUpdateEnabledChange: (enabled: boolean) => void;
    onGithubTokenChange: (token: string) => void;
    onIconSetChange: (newIconSet: IconSet) => void;
    onLogTableDensityChange: (newDensity: LogTableDensity) => void;
    onColumnVisibilityChange: (newVisibility: ColumnVisibilityState) => void;
    onColumnStylesChange: (newStyles: ColumnStyles) => void;
    onPanelWidthsChange: (newWidths: PanelWidths) => void;
    onTimeRangeSelectorVisibilityChange: (isVisible: boolean) => void;
    onDetailPanelVisibilityChange: (isVisible: boolean) => void;
    onFocusDebuggerVisibilityChange: (isVisible: boolean) => void;
    onTimelineBarVisibilityChange: (newVisibility: TimelineBarVisibility) => void;
    onUiScaleChange: (newScale: number) => void;
    onFullSettingsUpdate: (newSettings: SettingsType) => Promise<void>;
    onSaveFilterPreset: (name: string, filtersToSave: FilterState) => Promise<void>;
    onDeleteFilterPreset: (name: string) => Promise<void>;
    onLoadFilterPreset: (name: string) => FilterState | null;
};

const SettingsContext = createContext<SettingsContextType | undefined>(undefined);

export const SettingsProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const { logToConsole } = useConsole();
    const { addToast } = useToast();

    // All settings states
    const [theme, setTheme] = useState<Theme>('light');
    const [viewMode, setViewMode] = useState<ViewMode>('pagination');
    const [iconSet, setIconSet] = useState<IconSet>('sharp');
    const [columnVisibility, setColumnVisibility] = useState<ColumnVisibilityState>(initialColumnVisibility);
    const [columnStyles, setColumnStyles] = useState<ColumnStyles>(initialColumnStyles);
    const [customFilterPresets, setCustomFilterPresets] = useState<Record<string, FilterState>>({});
    const [panelWidths, setPanelWidths] = useState<PanelWidths>(initialPanelWidths);
    const [isTimeRangeSelectorVisible, setIsTimeRangeSelectorVisible] = useState(true);
    const [isDetailPanelVisible, setIsDetailPanelVisible] = useState(false);
    const [isFocusDebuggerVisible, setIsFocusDebuggerVisible] = useState<boolean>(false);
    const [timelineBarVisibility, setTimelineBarVisibility] = useState<TimelineBarVisibility>(initialTimelineBarVisibility);
    const [logTableDensity, setLogTableDensity] = useState<LogTableDensity>('normal');
    const [allowPrerelease, setAllowPrerelease] = useState<boolean>(false);
    const [isAutoUpdateEnabled, setIsAutoUpdateEnabled] = useState<boolean>(true);
    const [githubToken, setGithubToken] = useState<string>('');
    const [uiScale, setUiScale] = useState<number>(1);
    
    // Load settings on startup
    useEffect(() => {
        if (!window.electronAPI) return;
        window.electronAPI.getSettings().then(settings => {
            setTheme(settings.theme || 'light');
            setViewMode(settings.viewMode || 'pagination');
            setIconSet(settings.iconSet || 'sharp');
            setColumnVisibility({ ...initialColumnVisibility, ...settings.columnVisibility });
            // Deep merge styles
            const mergedStyles: ColumnStyles = JSON.parse(JSON.stringify(initialColumnStyles));
            for (const key in mergedStyles) {
                if (settings.columnStyles[key as keyof ColumnStyles]) {
                    mergedStyles[key as keyof ColumnStyles] = { ...mergedStyles[key as keyof ColumnStyles], ...settings.columnStyles[key as keyof ColumnStyles] };
                }
            }
            setColumnStyles(mergedStyles);
            setCustomFilterPresets(settings.customFilterPresets || {});
            setPanelWidths({ ...initialPanelWidths, ...settings.panelWidths });
            setIsTimeRangeSelectorVisible(settings.isTimeRangeSelectorVisible ?? true);
            setIsDetailPanelVisible(settings.isDetailPanelVisible ?? false);
            setIsFocusDebuggerVisible(settings.isFocusDebuggerVisible ?? false);
            setTimelineBarVisibility({ ...initialTimelineBarVisibility, ...settings.timelineBarVisibility });
            setLogTableDensity(settings.logTableDensity || 'normal');
            setAllowPrerelease(settings.allowPrerelease ?? false);
            setIsAutoUpdateEnabled(settings.isAutoUpdateEnabled ?? true);
            setGithubToken(settings.githubToken || '');
            setUiScale(settings.uiScale || 1);
        }).catch(err => logToConsole(`Failed to load settings: ${err.message}`, 'ERROR'));
    }, [logToConsole]);

    // Effects to apply settings to the DOM
    useEffect(() => { document.documentElement.classList.toggle('dark', theme === 'dark'); }, [theme]);
    useEffect(() => { (document.body.style as any).zoom = uiScale === 1 ? '' : `${uiScale}`; }, [uiScale]);
    
    // Generic settings updater
    const updateSettings = useCallback(async (newSettings: Partial<SettingsType>) => {
        if (!window.electronAPI) return;
        try {
            const currentSettings = await window.electronAPI.getSettings();
            await window.electronAPI.setSettings({ ...currentSettings, ...newSettings });
        } catch (err) {
            logToConsole(`Failed to save settings: ${(err as Error).message}`, 'ERROR');
        }
    }, [logToConsole]);

    // All handle...Change functions that call updateSettings
    const onThemeChange = (newTheme: Theme) => { setTheme(newTheme); updateSettings({ theme: newTheme }); };
    const onViewModeChange = (newMode: ViewMode) => { setViewMode(newMode); updateSettings({ viewMode: newMode }); };
    const onIconSetChange = (newIconSet: IconSet) => { setIconSet(newIconSet); updateSettings({ iconSet: newIconSet }); };
    const onLogTableDensityChange = (newDensity: LogTableDensity) => { setLogTableDensity(newDensity); updateSettings({ logTableDensity: newDensity }); };
    const onColumnVisibilityChange = (newVisibility: ColumnVisibilityState) => { setColumnVisibility(newVisibility); updateSettings({ columnVisibility: newVisibility }); };
    const onColumnStylesChange = (newStyles: ColumnStyles) => { setColumnStyles(newStyles); updateSettings({ columnStyles: newStyles }); };
    const onPanelWidthsChange = (newWidths: PanelWidths) => { setPanelWidths(newWidths); updateSettings({ panelWidths: newWidths }); };
    const onTimeRangeSelectorVisibilityChange = (isVisible: boolean) => { setIsTimeRangeSelectorVisible(isVisible); updateSettings({ isTimeRangeSelectorVisible: isVisible }); };
    const onDetailPanelVisibilityChange = (isVisible: boolean) => { setIsDetailPanelVisible(isVisible); updateSettings({ isDetailPanelVisible: isVisible }); };
    const onFocusDebuggerVisibilityChange = (isVisible: boolean) => { setIsFocusDebuggerVisible(isVisible); updateSettings({ isFocusDebuggerVisible: isVisible }); };
    const onTimelineBarVisibilityChange = (newVisibility: TimelineBarVisibility) => { setTimelineBarVisibility(newVisibility); updateSettings({ timelineBarVisibility: newVisibility }); };
    const onAllowPrereleaseChange = (allow: boolean) => { setAllowPrerelease(allow); updateSettings({ allowPrerelease: allow }); };
    const onAutoUpdateEnabledChange = (enabled: boolean) => { setIsAutoUpdateEnabled(enabled); updateSettings({ isAutoUpdateEnabled: enabled }); };
    const onGithubTokenChange = (token: string) => { setGithubToken(token); updateSettings({ githubToken: token }); };
    const onUiScaleChange = (newScale: number) => { setUiScale(newScale); updateSettings({ uiScale: newScale }); };
    
    const onFullSettingsUpdate = async (newSettings: SettingsType) => {
        setTheme(newSettings.theme);
        setViewMode(newSettings.viewMode);
        setIconSet(newSettings.iconSet);
        setColumnVisibility(newSettings.columnVisibility);
        setColumnStyles(newSettings.columnStyles);
        setCustomFilterPresets(newSettings.customFilterPresets);
        setPanelWidths(newSettings.panelWidths);
        setIsTimeRangeSelectorVisible(newSettings.isTimeRangeSelectorVisible);
        setIsDetailPanelVisible(newSettings.isDetailPanelVisible);
        setIsFocusDebuggerVisible(newSettings.isFocusDebuggerVisible);
        setTimelineBarVisibility(newSettings.timelineBarVisibility);
        setLogTableDensity(newSettings.logTableDensity);
        setAllowPrerelease(newSettings.allowPrerelease);
        setIsAutoUpdateEnabled(newSettings.isAutoUpdateEnabled);
        setGithubToken(newSettings.githubToken);
        setUiScale(newSettings.uiScale);
        await updateSettings(newSettings);
        addToast({ type: 'success', title: 'Settings Updated', message: 'Settings have been imported.' });
    };

    const onSaveFilterPreset = async (name: string, filtersToSave: FilterState) => {
        const newPresets = { ...customFilterPresets, [name.trim()]: { ...filtersToSave } };
        setCustomFilterPresets(newPresets);
        await updateSettings({ customFilterPresets: newPresets });
        logToConsole(`Filter preset '${name.trim()}' saved.`, 'INFO');
        addToast({ type: 'success', title: 'Preset Saved', message: `Preset '${name.trim()}' has been saved.`});
    };
    
    const onDeleteFilterPreset = async (name: string) => {
        const newPresets = { ...customFilterPresets };
        delete newPresets[name];
        setCustomFilterPresets(newPresets);
        await updateSettings({ customFilterPresets: newPresets });
        logToConsole(`Filter preset '${name}' deleted.`, 'INFO');
        addToast({ type: 'info', title: 'Preset Deleted', message: `Preset '${name}' has been deleted.`});
    };

    const onLoadFilterPreset = (name: string): FilterState | null => {
        if (customFilterPresets[name]) {
            logToConsole(`Loaded filter preset '${name}'. Click Apply to see results.`, 'INFO');
            addToast({ type: 'info', title: 'Preset Loaded', message: `Preset '${name}' loaded. Click 'Apply' to see changes.`});
            return { ...customFilterPresets[name] };
        }
        return null;
    };
    
    const value: SettingsContextType = {
        theme, viewMode, allowPrerelease, isAutoUpdateEnabled, githubToken, iconSet, logTableDensity,
        columnVisibility, customFilterPresets, columnStyles, panelWidths, isTimeRangeSelectorVisible,
        isDetailPanelVisible, isFocusDebuggerVisible, timelineBarVisibility, uiScale,
        onThemeChange, onViewModeChange, onAllowPrereleaseChange, onAutoUpdateEnabledChange, onGithubTokenChange,
        onIconSetChange, onLogTableDensityChange, onColumnVisibilityChange, onColumnStylesChange,
        onPanelWidthsChange, onTimeRangeSelectorVisibilityChange, onDetailPanelVisibilityChange,
        onFocusDebuggerVisibilityChange, onTimelineBarVisibilityChange, onUiScaleChange,
        onFullSettingsUpdate, onSaveFilterPreset, onDeleteFilterPreset, onLoadFilterPreset,
    };
    
    return (
        <SettingsContext.Provider value={value}>
            {children}
        </SettingsContext.Provider>
    );
};

export const useSettings = (): SettingsContextType => {
    const context = useContext(SettingsContext);
    if (context === undefined) {
        throw new Error('useSettings must be used within a SettingsProvider');
    }
    return context;
};
