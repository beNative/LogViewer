export interface LogEntry {
  id: number;
  level: string;
  time: string;
  sndrtype: string;
  sndrname: string;
  msg: string;
  fileName:string;
}

export type ConsoleMessageType = 'DEBUG' | 'INFO' | 'WARNING' | 'ERROR';

export interface ConsoleMessage {
  type: ConsoleMessageType;
  message: string;
  timestamp: string;
}

export interface FilterState {
  dateFrom: string;
  timeFrom: string;
  dateTo: string;
  timeTo: string;
  level: string[];
  sndrtype: string[];
  sndrname: string[];
  fileName: string[];
  levelExclude: string[];
  sndrtypeExclude: string[];
  sndrnameExclude: string[];
  fileNameExclude: string[];
  includeMsg: string;
  excludeMsg: string;
  includeMsgMode: 'AND' | 'OR';
  excludeMsgMode: 'AND' | 'OR';
  sqlQuery: string;
  sqlQueryEnabled: boolean;
}

export interface TimelineDataPoint {
  time: string;
  count: number;
}

export interface CategoryDataPoint {
  name:string;
  count: number;
}

export interface PageTimestampRange {
  page: number;
  startTime: string | null;
  endTime: string | null;
}

export interface DashboardData {
  timeline: TimelineDataPoint[];
  levels: CategoryDataPoint[];
  senderTypes: CategoryDataPoint[];
}

export type LogMessageContentType = 'text' | 'kv' | 'sql' | 'xml' | 'grid';

export interface GridData {
    headers: string[];
    rows: string[][];
}

export interface LogMessageContent {
    type: LogMessageContentType;
    prefix: string | null;
    data: any; // Could be string, {key, value}[], {sql, result}, string, or GridData
}

export interface SessionFile {
  name: string;
  path: string;
  size: number;
  mtime: number;
}

export type ColumnKey = 'time' | 'level' | 'sndrtype' | 'sndrname' | 'fileName' | 'msg';

export interface ColumnVisibilityState {
  time: boolean;
  level: boolean;
  sndrtype: boolean;
  sndrname: boolean;
  fileName: boolean;
  msg: boolean;
}

export interface ColumnStyle {
  font: string;
  fontSize: number;
  isBold: boolean;
  isItalic: boolean;
  color: string;
  darkColor: string;
}

export type ColumnStyles = Record<ColumnKey, ColumnStyle>;

export interface PanelWidths {
    filters: number;
    details: number;
}

export type Theme = 'light' | 'dark';
export type ViewMode = 'pagination' | 'scroll';
export type IconSet = 'sharp' | 'solid' | 'feather' | 'tabler' | 'lucide';
export type LogTableDensity = 'compact' | 'normal' | 'comfortable';

export interface Settings {
    theme: Theme;
    viewMode: ViewMode;
    iconSet: IconSet;
    columnVisibility: ColumnVisibilityState;
    columnStyles: ColumnStyles;
    customFilterPresets: Record<string, FilterState>;
    panelWidths: PanelWidths;
    isTimeRangeSelectorVisible: boolean;
    logTableDensity: LogTableDensity;
    allowPrerelease: boolean;
    uiScale: number;
}

export interface OverallTimeRange {
    min: number;
    max: number;
}

export interface FileTimeRange {
    name: string;
    startTime: string;
    endTime: string;
}

export interface LogDensityPoint {
    time: number; // Start time of the bucket
    count: number;
}

export type ProgressPhase = 'reading' | 'unzipping' | 'parsing' | 'inserting' | 'indexing' | 'loading';

// --- Stock Tracker Types ---

export interface StockInfoEntry {
  timestamp: string;
  message_id: number;
  source: string;
  destination: string;
  article_id: string;
  article_name: string;
  dosage_form: string;
  max_sub_item_quantity: number;
  quantity: number;
}

export interface StockInfoFilters {
    searchTerm: string;
    dateFrom: string;
    timeFrom: string;
    dateTo: string;
    timeTo: string;
}

export interface StockChartDataPoint {
  time: string;
  quantity: number;
}

// --- Toast Notification Types ---
export type ToastType = 'info' | 'success' | 'warning' | 'error' | 'progress';

export interface ToastAction {
    label: string;
    onClick: () => void;
}

export interface ToastMessage {
    id: string;
    type: ToastType;
    title: string;
    message: string;
    duration?: number; // in ms, 0 for persistent
    progress?: number; // 0-100 for progress type
    actions?: ToastAction[];
}
