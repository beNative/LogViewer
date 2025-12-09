import React, { Suspense } from 'react';
import { APP_VERSION } from './constants';
import { Header } from './components/Header';
import { Console } from './components/Console';
import { DataHub } from './components/DataHub';
import { Dashboard } from './components/Dashboard';
import { StatusBar } from './components/StatusBar';
import { AboutDialog } from './components/AboutDialog';
import { Toast } from './components/Toast';
import { FocusDebugger } from './components/FocusDebugger';
import { Icon } from './components/icons';
import { LogTable } from './components/LogTable';
import { ProgressIndicator } from './components/ProgressIndicator';

// Lazy load less frequently used components for better initial load performance
const Settings = React.lazy(() => import('./components/Settings').then(m => ({ default: m.Settings })));
const Info = React.lazy(() => import('./components/Info').then(m => ({ default: m.Info })));
const StockTracker = React.lazy(() => import('./components/StockTracker').then(m => ({ default: m.StockTracker })));

/** Loading fallback for lazy loaded components */
const LazyLoadFallback: React.FC = () => (
  <div className="flex-grow flex items-center justify-center">
    <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-500"></div>
  </div>
);

import { useUI } from './contexts/UIContext';
import { useToast } from './contexts/ToastContext';
import { useConsole } from './contexts/ConsoleContext';
import { useSettings } from './contexts/SettingsContext';
import { useSession } from './contexts/SessionContext';
import { useData } from './contexts/DataContext';
import { TitleBar } from './components/TitleBar';

const App: React.FC = () => {
  // UI State and Actions from Hooks
  const {
    activeView, setActiveView, isAboutDialogOpen, setIsAboutDialogOpen,
    isLoading, progress, progressMessage, progressPhase, detailedProgress, progressTitle,
    isBusy, isStockBusy,
  } = useUI();

  const { toasts, removeToast } = useToast();
  const { consoleMessages, consoleFilters, setConsoleFilters, consoleSearchTerm } = useConsole();

  // Settings State and Actions from Hooks
  const settings = useSettings();
  const {
    isFocusDebuggerVisible, theme, iconSet, viewMode, logTableDensity,
    isDetailPanelVisible, onDetailPanelVisibilityChange, onThemeChange,
  } = settings;

  // Session State and Actions from Hooks
  const {
    isElectron, sessions, activeSessionName, isDirty, hasData, totalEntryCount,
    overallTimeRange, loadedFileNames, error,
    handleCreateNewSessionFromFiles, handleAddFilesToCurrentSession, handleImportDb,
    handleDownloadDb, handleNewSession, handleLoadSession, onRenameSession, onDeleteSession, handleCancelProcessing
  } = useSession();

  // Data (Log & Stock) State and Actions from Hooks
  const data = useData();
  const {
    filteredEntries, pageTimestampRanges, formFilters, setFormFilters,
    appliedFilters, handleApplyFilters, handleResetFilters, handleClearTimeRange,
    uniqueValues, onLoadMore, hasMoreLogs, keyboardSelectedId, setKeyboardSelectedId,
    jumpToEntryId, isInitialLoad, handleRemoveAppliedFilter, handleContextMenuFilter,
    handleCursorChange, handleFileSelect, handleDateSelect, handleApplyDetailFilter,
    timelineViewRange, setTimelineViewRange, handleTimelineZoomToSelection, handleTimelineZoomReset,
    dashboardData, handleTimeRangeSelect, handleCategorySelect,
    stockHistory, overallStockTimeRange, overallStockDensity, handleSearchStock,
    handleRebuildStockData, handleFetchStockSuggestions,
    currentPage, totalPages, pageSize, handlePageSizeChange, goToPage,
    fileTimeRanges, logDensity, overallLogDensity, datesWithLogs
  } = data;

  // Derived state for cursor
  const selectedEntryForCursor = React.useMemo(() => {
    if (keyboardSelectedId === null) return null;
    return filteredEntries.find(e => e.id === keyboardSelectedId);
  }, [keyboardSelectedId, filteredEntries]);

  const cursorTime = selectedEntryForCursor
    ? new Date(selectedEntryForCursor.time.replace(/ (\d+)$/, '.$1') + 'Z').getTime()
    : null;

  // --- Composition Handlers to fix circular dependency ---
  const handleSavePreset = (name: string) => {
    settings.onSaveFilterPreset(name, data.formFilters);
  };

  const handleLoadPreset = (name: string) => {
    const preset = settings.onLoadFilterPreset(name);
    if (preset) {
      data.setFormFilters(preset);
    }
  };

  return (
    <div className="flex flex-col h-full bg-gray-50 dark:bg-gray-900 text-gray-900 dark:text-gray-100">
      <TitleBar activeView={activeView} />
      <div className="fixed bottom-4 right-4 z-50 w-full max-w-sm space-y-3">
        {toasts.map(toast => (
          <Toast key={toast.id} toast={toast} onDismiss={() => removeToast(toast.id)} iconSet={iconSet} />
        ))}
      </div>
      {isAboutDialogOpen && (
        <AboutDialog
          isOpen={isAboutDialogOpen}
          onClose={() => setIsAboutDialogOpen(false)}
          iconSet={iconSet}
          version={APP_VERSION}
        />
      )}
      {isLoading && <ProgressIndicator title={progressTitle} progress={progress} message={progressMessage} phase={progressPhase} detailedProgress={detailedProgress} iconSet={iconSet} onCancel={handleCancelProcessing} />}
      <Header activeView={activeView} onViewChange={setActiveView} isBusy={isBusy || isStockBusy} iconSet={iconSet} />
      <main className="flex-grow flex flex-col min-h-0">
        {activeView === 'data' && (
          <DataHub
            onCreateSessionFromFiles={handleCreateNewSessionFromFiles}
            onAddFilesToSession={handleAddFilesToCurrentSession}
            onImportDb={handleImportDb}
            onDownloadDb={handleDownloadDb}
            onNewSession={handleNewSession}
            error={error}
            hasData={hasData}
            totalEntryCount={totalEntryCount}
            overallTimeRange={overallTimeRange}
            loadedFileNames={loadedFileNames}
            isElectron={isElectron}
            sessions={sessions}
            activeSessionName={activeSessionName}
            onLoadSession={handleLoadSession}
            onRenameSession={onRenameSession}
            onDeleteSession={onDeleteSession}
            isDirty={isDirty}
            iconSet={iconSet}
          />
        )}
        {activeView === 'viewer' && (
          <>
            {hasData ? (
              <LogTable
                entries={filteredEntries}
                loadedFileNames={loadedFileNames}
                pageTimestampRanges={pageTimestampRanges}
                onViewModeChange={settings.onViewModeChange}
                filters={formFilters}
                appliedFilters={appliedFilters}
                onFiltersChange={setFormFilters}
                onApplyFilters={handleApplyFilters}
                onResetFilters={handleResetFilters}
                onClearTimeRange={handleClearTimeRange}
                uniqueValues={uniqueValues}
                theme={theme}
                viewMode={viewMode}
                columnVisibility={settings.columnVisibility}
                onColumnVisibilityChange={settings.onColumnVisibilityChange}
                columnStyles={settings.columnStyles}
                panelWidths={settings.panelWidths}
                onPanelWidthsChange={settings.onPanelWidthsChange}
                isDetailPanelVisible={isDetailPanelVisible}
                onDetailPanelVisibilityChange={onDetailPanelVisibilityChange}
                onApplyFilter={handleApplyDetailFilter}
                onContextMenuFilter={handleContextMenuFilter}
                customFilterPresets={settings.customFilterPresets}
                onSavePreset={handleSavePreset}
                onDeletePreset={settings.onDeleteFilterPreset}
                onLoadPreset={handleLoadPreset}
                onLoadMore={onLoadMore}
                hasMore={hasMoreLogs}
                isBusy={isBusy}
                logToConsole={data.logToConsoleForEntries}
                overallTimeRange={overallTimeRange}
                onTimeRangeSelectorChange={handleTimeRangeSelect}
                isTimeRangeSelectorVisible={settings.isTimeRangeSelectorVisible}
                onTimeRangeSelectorVisibilityChange={settings.onTimeRangeSelectorVisibilityChange}
                fileTimeRanges={fileTimeRanges}
                logDensity={logDensity}
                overallLogDensity={overallLogDensity}
                datesWithLogs={datesWithLogs}
                onCursorChange={handleCursorChange}
                onFileSelect={handleFileSelect}
                onDateSelect={handleDateSelect}
                keyboardSelectedId={keyboardSelectedId}
                setKeyboardSelectedId={setKeyboardSelectedId}
                jumpToEntryId={jumpToEntryId}
                timelineViewRange={timelineViewRange}
                onTimelineViewRangeChange={setTimelineViewRange}
                onTimelineZoomToSelection={handleTimelineZoomToSelection}
                onTimelineZoomReset={handleTimelineZoomReset}
                isInitialLoad={isInitialLoad}
                iconSet={iconSet}
                onRemoveAppliedFilter={handleRemoveAppliedFilter}
                logTableDensity={logTableDensity}
                onLogTableDensityChange={settings.onLogTableDensityChange}
                uiScale={settings.uiScale}
                cursorTime={cursorTime}
                timelineBarVisibility={settings.timelineBarVisibility}
                onTimelineBarVisibilityChange={settings.onTimelineBarVisibilityChange}
              />
            ) : (
              <div className="flex-grow flex items-center justify-center p-8 text-center bg-white dark:bg-gray-900">
                <div>
                  <Icon name="Table" iconSet={iconSet} className="w-24 h-24 text-gray-300 dark:text-gray-700 mx-auto mb-6" />
                  <h2 className="text-2xl font-semibold text-gray-600 dark:text-gray-300">No Data Loaded</h2>
                  <p className="text-gray-500 dark:text-gray-500 mt-2 max-w-md mx-auto">
                    Switch to the <strong className="text-gray-700 dark:text-gray-400">Data Hub</strong> tab to {isElectron ? 'load a session or create a new one' : 'add log files'}.
                  </p>
                </div>
              </div>
            )}
          </>
        )}
        {activeView === 'dashboard' && (
          <Dashboard
            data={dashboardData}
            hasData={hasData}
            onTimeRangeSelect={handleTimeRangeSelect}
            onCategorySelect={handleCategorySelect}
            theme={theme}
            iconSet={iconSet}
          />
        )}
        {activeView === 'stock' && (
          <Suspense fallback={<LazyLoadFallback />}>
            <StockTracker
              onSearch={handleSearchStock}
              history={stockHistory}
              isBusy={isStockBusy}
              iconSet={iconSet}
              theme={theme}
              overallTimeRange={overallStockTimeRange}
              overallStockDensity={overallStockDensity}
              uiScale={settings.uiScale}
              onRebuildStockData={handleRebuildStockData}
              onFetchSuggestions={handleFetchStockSuggestions}
              timelineBarVisibility={settings.timelineBarVisibility}
              onTimelineBarVisibilityChange={settings.onTimelineBarVisibilityChange}
            />
          </Suspense>
        )}
        {activeView === 'console' && (
          <Console
            messages={consoleMessages}
            onClear={useConsole().handleClearConsole}
            filters={consoleFilters}
            onFiltersChange={setConsoleFilters}
            iconSet={iconSet}
            searchTerm={consoleSearchTerm}
            theme={theme}
          />
        )}
        {activeView === 'info' && (
          <Suspense fallback={<LazyLoadFallback />}>
            <Info
              iconSet={iconSet}
              onOpenAboutDialog={() => setIsAboutDialogOpen(true)}
            />
          </Suspense>
        )}
        {activeView === 'settings' && (
          <Suspense fallback={<LazyLoadFallback />}>
            <Settings {...settings} />
          </Suspense>
        )}
      </main>
      <StatusBar
        totalEntries={totalEntryCount}
        filteredCount={data.totalFilteredCount}
        activeSessionName={activeSessionName}
        isDirty={isDirty}
        viewMode={viewMode}
        currentPage={currentPage}
        totalPages={totalPages}
        visibleRowCount={filteredEntries.length}
        pageSize={pageSize}
        onPageSizeChange={handlePageSizeChange}
        onGoToPage={goToPage}
        isBusy={isBusy}
        lastConsoleMessage={data.lastConsoleMessageForStatus}
        theme={theme}
        onThemeChange={() => onThemeChange(theme === 'light' ? 'dark' : 'light')}
        iconSet={iconSet}
        logTableDensity={logTableDensity}
        onLogTableDensityChange={settings.onLogTableDensityChange}
        isDetailPanelVisible={isDetailPanelVisible}
        onDetailPanelVisibilityChange={onDetailPanelVisibilityChange}
      />
      <FocusDebugger isVisible={isFocusDebuggerVisible} iconSet={iconSet} />
    </div>
  );
};

export default App;