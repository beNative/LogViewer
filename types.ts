// FIX: Removed incorrect import of `ToastMessage` from `App.tsx` to resolve a circular dependency error. The `ToastMessage` type is already defined within this file.

export type LogEntry = {
    id: number;
    time: string;
    level: string;
    sndrtype: string;
    sndrname: string;
    msg: string;
    fileName: string;
};

export type StockInfoEntry = {
    timestamp: string;
    message_id: number;
    source: string;
    destination: string;
    article_id: string;
    article_name: string;
    dosage_form: string;
    max_sub_item_quantity: number;
    quantity: number;
};

export type StockInfoFilters = {
    searchTerm: string;
    dateFrom: string;
    timeFrom: string;
    dateTo: string;
    timeTo: string;
};

export type StockArticleSuggestion = {
    id: string;
    name: string;
};

export type StockChartDataPoint = {
    time: string;
    quantity: number;
};

export type FilterState = {
    dateFrom: string;
    timeFrom: string;
    dateTo: string;
    timeTo: string;
    level: string[];
    levelFilterMode: 'include' | 'exclude';
    sndrtype: string[];
    sndrtypeFilterMode: 'include' | 'exclude';
    sndrname: string[];
    sndrnameFilterMode: 'include' | 'exclude';
    fileName: string[];
    fileNameFilterMode: 'include' | 'exclude';
    includeMsg: string;
    excludeMsg: string;
    includeMsgMode: 'OR' | 'AND';
    excludeMsgMode: 'OR' | 'AND';
    sqlQuery: string;
    sqlQueryEnabled: boolean;
};

export type ConsoleMessageType = 'DEBUG' | 'INFO' | 'WARNING' | 'ERROR';

export type ConsoleMessage = {
    message: string;
    type: ConsoleMessageType;
    timestamp: string;
};

export type TimelineDataPoint = {
    time: string; // ISO format
    count: number;
};

export type CategoryDataPoint = {
    name: string;
    count: number;
};

export type DashboardData = {
    timeline: TimelineDataPoint[];
    levels: CategoryDataPoint[];
    senderTypes: CategoryDataPoint[];
};

export type PageTimestampRange = {
    page: number;
    startTime: string;
    endTime: string;
};

export type FileTimeRange = {
    name: string;
    startTime: string;
    endTime: string;
};

export type LogDensityPoint = {
    time: number; // timestamp ms
    count: number; // density value
};

export type LogDensityPointByLevel = {
    time: number; // timestamp ms
    counts: Record<string, number>; // e.g., { "ERROR": 10, "INFO": 50 }
};

export type SessionFile = {
    name: string;
    path: string;
    size: number;
    mtime: number;
};

export type ColumnKey = 'time' | 'level' | 'sndrtype' | 'sndrname' | 'fileName' | 'msg';

export type ColumnVisibilityState = Record<ColumnKey, boolean>;

export type ColumnStyle = {
    font: string;
    fontSize: number;
    isBold: boolean;
    isItalic: boolean;
    color: string;
    darkColor: string;
};

export type ColumnStyles = Record<ColumnKey, ColumnStyle>;

export type PanelWidths = {
    filters: number;
    details: number;
};

export type ViewMode = 'pagination' | 'scroll';

export type OverallTimeRange = {
    min: number; // timestamp ms
    max: number; // timestamp ms
};

export type Theme = 'light' | 'dark';

export type IconSet = 'sharp' | 'solid' | 'feather' | 'tabler' | 'lucide';

export type LogTableDensity = 'compact' | 'normal' | 'comfortable';

export type ProgressPhase = 'reading' | 'unzipping' | 'parsing' | 'inserting' | 'indexing' | 'loading';

export type TimelineBarVisibility = {
    pages: boolean;
    files: boolean;
    dates: boolean;
    density: boolean;
    overview: boolean;
};

export type Settings = {
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
    logSqlQueries: boolean;
};

export type ToastMessage = {
    id: string;
    type: 'info' | 'success' | 'warning' | 'error' | 'progress';
    title: string;
    message: string;
    duration?: number;
    progress?: number;
    actions?: { label: string; onClick: () => void; }[];
};

export type LogMessageContent = {
    type: 'text' | 'xml' | 'kv' | 'sql' | 'grid';
    prefix: string | null;
    data: string | { key: string, value: string }[] | { sql: string, result: string | null } | GridData;
};

export type GridData = {
    headers: string[];
    rows: string[][];
};