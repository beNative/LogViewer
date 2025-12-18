import initSqlJs from 'sql.js';
import { FilterState, LogEntry, TimelineDataPoint, CategoryDataPoint, PageTimestampRange, FileTimeRange, LogDensityPoint, StockInfoEntry, StockInfoFilters, StockArticleSuggestion, LogDensityPointByLevel, ConsoleMessageType } from './types.ts';

const locateSqlWasm = (file: string) => {
    if (typeof window !== 'undefined' && window.location) {
        return new URL(`./dist/${file}`, window.location.href).toString();
    }

    if (typeof globalThis?.location?.href === 'string') {
        return new URL(`./dist/${file}`, globalThis.location.href).toString();
    }

    return `./dist/${file}`;
};
type SqlJsDatabase = any;

// Helper to convert date/time inputs to a UTC SQL-comparable format.
// Supports time strings with or without milliseconds (HH:MM:SS or HH:MM:SS.mmm)
export const getSqlDateTime = (dateStr: string, timeStr: string, endOfDay = false): string | null => {
    if (!dateStr) return null;
    let time = timeStr || (endOfDay ? '23:59:59' : '00:00:00');
    if (time.length === 5) time += ':00'; // Add seconds if missing (HH:MM -> HH:MM:SS)

    // Check if milliseconds are already present (HH:MM:SS.mmm has length 12)
    const hasMilliseconds = time.includes('.');

    // Construct a UTC-based ISO string directly from the UTC date/time parts.
    // This avoids any local timezone interpretation.
    // Only add milliseconds if not already present
    let isoString: string;
    if (hasMilliseconds) {
        isoString = `${dateStr}T${time}Z`;
    } else {
        isoString = `${dateStr}T${time}${endOfDay ? '.999' : '.000'}Z`;
    }

    // Now, we can just format it for SQLite string comparison.
    return isoString.replace('T', ' ').replace('Z', '');
};

export class Database {
    private db: SqlJsDatabase;
    private logToConsole?: (message: string, type: ConsoleMessageType) => void;
    private logSqlQueries?: boolean;

    // Cache for _buildWhereClause to avoid repeated computation for same filters
    private _whereClauseCache: {
        filterKey: string;
        result: { whereSql: string, params: (string | number)[] };
    } | null = null;

    private constructor(db: SqlJsDatabase) {
        this.db = db;
    }

    public setLogger(logger: (message: string, type: ConsoleMessageType) => void, logSql: boolean) {
        this.logToConsole = logger;
        this.logSqlQueries = logSql;
    }

    private _logSql(sql: string, params?: (string | number | null)[]) {
        if (this.logSqlQueries && this.logToConsole) {
            const cleanSql = sql.trim().replace(/\s+/g, ' ');
            let paramStr = params && params.length > 0 ? `\n  Params: ${JSON.stringify(params)}` : '';
            this.logToConsole(`[SQL] ${cleanSql}${paramStr}`, 'DEBUG');
        }
    }

    /**
     * Create a new empty Database instance.
     * Initializes sql.js and returns a Database wrapper with no data.
     */
    static async create(): Promise<Database> {
        const SQL = await initSqlJs({
            locateFile: locateSqlWasm
        });
        const db = new SQL.Database();
        return new Database(db);
    }

    /**
     * Create a Database instance from an existing SQLite buffer.
     * Used when loading saved sessions or receiving data from a worker.
     * @param buffer - Uint8Array containing the SQLite database file contents
     */
    static async createFromBuffer(buffer: Uint8Array): Promise<Database> {
        const SQL = await initSqlJs({
            locateFile: locateSqlWasm
        });
        const db = new SQL.Database(buffer);
        return new Database(db);
    }

    createTable() {
        // Using "IF NOT EXISTS" is crucial for appending data to existing sessions
        // without dropping the table. This now ONLY creates tables. Indexes are handled separately.
        const sql1 = `
            CREATE TABLE IF NOT EXISTS logs (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                time TEXT,
                level TEXT,
                sndrtype TEXT,
                sndrname TEXT,
                msg TEXT,
                fileName TEXT
            );
        `;
        const sql2 = `
            CREATE TABLE IF NOT EXISTS meta (
                key TEXT PRIMARY KEY,
                value TEXT
            );
        `;
        const sql3 = `
            CREATE TABLE IF NOT EXISTS stock_info (
                timestamp TEXT,
                message_id INTEGER,
                source TEXT,
                destination TEXT,
                article_id TEXT,
                article_name TEXT,
                dosage_form TEXT,
                max_sub_item_quantity INTEGER,
                quantity INTEGER
            );
        `;
        this._logSql(sql1); this.db.exec(sql1);
        this._logSql(sql2); this.db.exec(sql2);
        this._logSql(sql3); this.db.exec(sql3);
    }

    dropIndexes() {
        const queries = [
            `DROP INDEX IF EXISTS idx_time;`, `DROP INDEX IF EXISTS idx_level;`,
            `DROP INDEX IF EXISTS idx_sndrtype;`, `DROP INDEX IF EXISTS idx_sndrname;`,
            `DROP INDEX IF EXISTS idx_fileName;`, `DROP INDEX IF EXISTS idx_stock_timestamp;`,
            `DROP INDEX IF EXISTS idx_stock_article_id;`, `DROP INDEX IF EXISTS idx_stock_article_name;`
        ];
        queries.forEach(q => { this._logSql(q); this.db.exec(q); });
    }

    createIndexes() {
        const queries = [
            `CREATE INDEX IF NOT EXISTS idx_time ON logs(time);`, `CREATE INDEX IF NOT EXISTS idx_level ON logs(level);`,
            `CREATE INDEX IF NOT EXISTS idx_sndrtype ON logs(sndrtype);`, `CREATE INDEX IF NOT EXISTS idx_sndrname ON logs(sndrname);`,
            `CREATE INDEX IF NOT EXISTS idx_fileName ON logs(fileName);`, `CREATE INDEX IF NOT EXISTS idx_stock_timestamp ON stock_info(timestamp);`,
            `CREATE INDEX IF NOT EXISTS idx_stock_article_id ON stock_info(article_id);`, `CREATE INDEX IF NOT EXISTS idx_stock_article_name ON stock_info(article_name);`
        ];
        queries.forEach(q => { this._logSql(q); this.db.exec(q); });
    }

    setMeta(key: string, value: string) {
        const sql = `INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)`;
        const params = [key, value];
        this._logSql(sql, params);
        const stmt = this.db.prepare(sql);
        stmt.run(params);
        stmt.free();
    }

    getMeta(key: string): string | null {
        try {
            const sql = `SELECT value FROM meta WHERE key = ?`;
            const params = [key];
            this._logSql(sql, params);
            const stmt = this.db.prepare(sql);
            stmt.bind(params);
            let value: string | null = null;
            if (stmt.step()) {
                value = stmt.get()[0] as string;
            }
            stmt.free();
            return value;
        } catch (e) {
            // Meta table might not exist in very old DB files
            return null;
        }
    }

    insertLogs(
        entries: Omit<LogEntry, 'id' | 'fileName'>[],
        fileName: string,
        onProgress?: (processedCount: number) => void
    ) {
        if (entries.length === 0) {
            onProgress?.(0);
            return;
        }

        const sql = `
            INSERT INTO logs (time, level, sndrtype, sndrname, msg, fileName) 
            VALUES (?, ?, ?, ?, ?, ?)
        `;
        this._logSql(`BEGIN TRANSACTION;`);
        this._logSql(sql, ['(sample)']); // Log a sample of the prepared statement
        this._logSql(`COMMIT;`);

        const stmt = this.db.prepare(sql);

        this.db.exec("BEGIN TRANSACTION;");
        try {
            entries.forEach((entry, index) => {
                try {
                    // Format time for SQLite: "2024-01-01 12:30:00 123" -> "2024-01-01 12:30:00.123"
                    const sqlTime = entry.time.replace(/ (\d+)$/, '.$1');
                    stmt.run([sqlTime, entry.level, entry.sndrtype, entry.sndrname, entry.msg, fileName]);

                    // Report progress periodically to avoid overwhelming the main thread
                    if (onProgress && (index % 500 === 0 || index === entries.length - 1)) {
                        onProgress(index + 1);
                    }
                } catch (e) {
                    const originalError = e instanceof Error ? e.message : String(e);
                    const entryPreview = JSON.stringify(entry);
                    throw new Error(`Error on item #${index + 1} in file ${fileName}.\nDetails: ${originalError}\nLog Data: ${entryPreview}`);
                }
            });
            this.db.exec("COMMIT;");
        } catch (e) {
            this.db.exec("ROLLBACK;");
            throw e; // Re-throw the detailed error
        } finally {
            stmt.free();
        }
    }

    insertStockInfo(entries: Omit<StockInfoEntry, 'id'>[]) {
        if (entries.length === 0) return;

        const sql = `
            INSERT INTO stock_info (timestamp, message_id, source, destination, article_id, article_name, dosage_form, max_sub_item_quantity, quantity)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        `;
        this._logSql('BEGIN TRANSACTION;');
        this._logSql(sql, ['(sample)']);
        this._logSql('COMMIT;');

        const stmt = this.db.prepare(sql);

        this.db.exec("BEGIN TRANSACTION;");
        try {
            entries.forEach(entry => {
                // Ensure the timestamp is in a format SQLite understands for date functions
                const sqlTime = entry.timestamp.replace('T', ' ').replace('Z', '');
                stmt.run([
                    sqlTime,
                    entry.message_id,
                    entry.source,
                    entry.destination,
                    entry.article_id,
                    entry.article_name,
                    entry.dosage_form,
                    entry.max_sub_item_quantity,
                    entry.quantity
                ]);
            });
            this.db.exec("COMMIT;");
        } catch (e) {
            this.db.exec("ROLLBACK;");
            throw e;
        } finally {
            stmt.free();
        }
    }

    private _buildStockWhereClause(filters: StockInfoFilters): { whereSql: string, params: (string | number)[] } {
        const whereClauses: string[] = [];
        const params: (string | number)[] = [];

        const fromDate = getSqlDateTime(filters.dateFrom, filters.timeFrom);
        if (fromDate) {
            whereClauses.push(`timestamp >= ?`);
            params.push(fromDate);
        }

        const toDate = getSqlDateTime(filters.dateTo, filters.timeTo, true);
        if (toDate) {
            whereClauses.push(`timestamp <= ?`);
            params.push(toDate);
        }

        if (filters.searchTerm) {
            whereClauses.push(`(article_id LIKE ? OR article_name LIKE ?)`);
            params.push(`%${filters.searchTerm}%`, `%${filters.searchTerm}%`);
        }

        const whereSql = whereClauses.length > 0 ? `WHERE ${whereClauses.join(' AND ')}` : '';
        return { whereSql, params };
    }

    queryStockInfo(filters: StockInfoFilters): { entries: StockInfoEntry[], sql: string, params: (string | number)[] } {
        const { whereSql, params } = this._buildStockWhereClause(filters);

        // Compact the SQL string for cleaner logging in the console
        const sql = `
            SELECT timestamp, message_id, source, destination, article_id, article_name, dosage_form, max_sub_item_quantity, quantity 
            FROM stock_info 
            ${whereSql} 
            ORDER BY timestamp ASC
        `.trim().replace(/\s\s+/g, ' ');

        this._logSql(sql, params);
        const stmt = this.db.prepare(sql);
        stmt.bind(params);

        const entries: StockInfoEntry[] = [];
        while (stmt.step()) {
            const row = stmt.get();
            entries.push({
                timestamp: row[0],
                message_id: row[1],
                source: row[2],
                destination: row[3],
                article_id: row[4],
                article_name: row[5],
                dosage_form: row[6],
                max_sub_item_quantity: row[7],
                quantity: row[8]
            });
        }
        stmt.free();
        return { entries, sql, params };
    }

    getUniqueArticles(searchTerm: string, timeFilters: { dateFrom: string, timeFrom: string, dateTo: string, timeTo: string }): StockArticleSuggestion[] {
        const fromDate = getSqlDateTime(timeFilters.dateFrom, timeFilters.timeFrom);
        const toDate = getSqlDateTime(timeFilters.dateTo, timeFilters.timeTo, true);

        let whereClauses = [];
        let params: (string | number)[] = [];

        if (searchTerm) {
            whereClauses.push(`(article_id LIKE ? OR article_name LIKE ?)`);
            params.push(`%${searchTerm}%`, `%${searchTerm}%`);
        }

        if (fromDate) {
            whereClauses.push(`timestamp >= ?`);
            params.push(fromDate);
        }

        if (toDate) {
            whereClauses.push(`timestamp <= ?`);
            params.push(toDate);
        }

        const whereSql = whereClauses.length > 0 ? `WHERE ${whereClauses.join(' AND ')}` : '';

        const sql = `
            SELECT DISTINCT article_id, article_name
            FROM stock_info
            ${whereSql}
            ORDER BY article_name, article_id
            LIMIT 15
        `;

        this._logSql(sql, params);
        const stmt = this.db.prepare(sql);
        stmt.bind(params);

        const articles: StockArticleSuggestion[] = [];
        while (stmt.step()) {
            const row = stmt.get();
            articles.push({ id: row[0], name: row[1] });
        }
        stmt.free();
        return articles;
    }

    private _buildWhereClause(filters: FilterState): { whereSql: string, params: (string | number)[] } {
        // Use memoization to avoid rebuilding for same filters
        const filterKey = JSON.stringify(filters);
        if (this._whereClauseCache && this._whereClauseCache.filterKey === filterKey) {
            return this._whereClauseCache.result;
        }

        const whereClauses: string[] = [];
        const params: (string | number)[] = [];

        if (filters) {
            const fromDate = getSqlDateTime(filters.dateFrom, filters.timeFrom);
            if (fromDate) {
                whereClauses.push(`time >= ?`);
                params.push(fromDate);
            }

            const toDate = getSqlDateTime(filters.dateTo, filters.timeTo, true);
            if (toDate) {
                whereClauses.push(`time <= ?`);
                params.push(toDate);
            }

            const attributes: (keyof FilterState)[] = ['level', 'sndrtype', 'sndrname', 'fileName'];
            attributes.forEach(attr => {
                const values = filters[attr] as string[] | undefined;
                const mode = filters[`${attr}FilterMode` as keyof FilterState] as 'include' | 'exclude' | undefined;

                if (values && values.length > 0) {
                    const operator = mode === 'include' ? 'IN' : 'NOT IN';
                    whereClauses.push(`${attr} ${operator} (${'?,'.repeat(values.length).slice(0, -1)})`);
                    params.push(...values);
                }
            });

            // NOTE: Message search uses LOWER(msg) LIKE '%term%' which cannot use indexes
            // and requires full table scan. For large datasets, consider implementing SQLite
            // FTS5 (Full-Text Search) extension for significantly faster text search.
            // See: https://www.sqlite.org/fts5.html
            const includeTerms = filters.includeMsg?.split('\n').map(t => t.trim().toLowerCase()).filter(Boolean) || [];
            if (includeTerms.length > 0) {
                const joiner = ` ${filters.includeMsgMode} `;
                const includeConditions = includeTerms.map(() => `LOWER(msg) LIKE ?`).join(joiner);
                whereClauses.push(`(${includeConditions})`);
                params.push(...includeTerms.map(term => `%${term}%`));
            }

            const excludeTerms = filters.excludeMsg?.split('\n').map(t => t.trim().toLowerCase()).filter(Boolean) || [];
            if (excludeTerms.length > 0) {
                const joiner = ` ${filters.excludeMsgMode} `;
                const excludeConditions = excludeTerms.map(() => `LOWER(msg) NOT LIKE ?`).join(joiner);
                whereClauses.push(`(${excludeConditions})`);
                params.push(...excludeTerms.map(term => `%${term}%`));
            }
        }

        const whereSql = whereClauses.length > 0 ? `WHERE ${whereClauses.join(' AND ')}` : '';
        const result = { whereSql, params };

        // Cache the result
        this._whereClauseCache = { filterKey, result };

        return result;
    }

    getTotalEntryCount(): number {
        let totalEntries = 0;
        try {
            const sql = "SELECT COUNT(*) FROM logs";
            this._logSql(sql);
            const totalResult = this.db.exec(sql);
            if (totalResult.length > 0 && totalResult[0].values.length > 0) {
                totalEntries = totalResult[0].values[0][0];
            }
        } catch (e) {
            // Table might not exist yet if db was just imported and is invalid
            return 0;
        }
        return totalEntries;
    }

    getFilteredLogCount(filters: FilterState): number {
        if (filters.sqlQueryEnabled && filters.sqlQuery) {
            // Sanitize by removing trailing semicolon to avoid issues with wrapping
            const saneQuery = filters.sqlQuery.trim().replace(/;$/, '');
            const sql = `SELECT COUNT(*) FROM (${saneQuery})`;
            try {
                this._logSql(sql);
                const stmt = this.db.prepare(sql);
                let count = 0;
                if (stmt.step()) {
                    count = stmt.get()[0] as number;
                }
                stmt.free();
                return count;
            } catch (e) {
                console.error("Error executing custom SQL count query:", e);
                throw e; // Re-throw to be caught by UI
            }
        }

        const { whereSql, params } = this._buildWhereClause(filters);
        const sql = `SELECT COUNT(*) FROM logs ${whereSql}`;
        this._logSql(sql, params);
        const stmt = this.db.prepare(sql);
        stmt.bind(params);
        let count = 0;
        if (stmt.step()) {
            count = stmt.get()[0] as number;
        }
        stmt.free();
        return count;
    }

    queryLogEntries(filters: FilterState, limit: number, offset: number): LogEntry[] {
        if (filters.sqlQueryEnabled && filters.sqlQuery) {
            const saneQuery = filters.sqlQuery.trim().replace(/;$/, '');
            const sql = `${saneQuery} LIMIT ? OFFSET ?`;
            const params = [limit, offset];
            try {
                this._logSql(sql, params);
                const stmt = this.db.prepare(sql);
                stmt.bind(params);
                const entries: LogEntry[] = [];
                const columnNames = stmt.getColumnNames();

                // Map results to LogEntry, filling missing columns with defaults
                while (stmt.step()) {
                    const row = stmt.get();
                    const entry: any = { id: 0, level: 'N/A', time: 'N/A', sndrtype: 'N/A', sndrname: 'N/A', msg: 'N/A', fileName: 'N/A' };
                    columnNames.forEach((colName: string, i: number) => {
                        if (Object.prototype.hasOwnProperty.call(entry, colName)) {
                            entry[colName] = row[i];
                        }
                    });
                    // If msg is missing but other fields aren't, serialize the row as msg
                    if (entry.msg === 'N/A' && columnNames.length > 1) {
                        entry.msg = JSON.stringify(Object.fromEntries(columnNames.map((c: string, i: number) => [c, row[i]])));
                    }
                    entries.push(entry);
                }
                stmt.free();
                return entries;
            } catch (e) {
                console.error("Error executing custom SQL query:", e);
                throw e;
            }
        }

        const { whereSql, params } = this._buildWhereClause(filters);
        const sql = `SELECT id, time, level, sndrtype, sndrname, msg, fileName FROM logs ${whereSql} ORDER BY time ASC LIMIT ? OFFSET ?`;
        const finalParams = [...params, limit, offset];
        this._logSql(sql, finalParams);
        const stmt = this.db.prepare(sql);
        stmt.bind(finalParams);

        const entries: LogEntry[] = [];
        while (stmt.step()) {
            const row = stmt.get();
            entries.push({ id: row[0], time: row[1], level: row[2], sndrtype: row[3], sndrname: row[4], msg: row[5], fileName: row[6] });
        }
        stmt.free();
        return entries;
    }

    getPageTimestampRanges(filters: FilterState, pageSize: number): PageTimestampRange[] {
        // This is too complex and unreliable with arbitrary user SQL, so we disable it.
        if (filters.sqlQueryEnabled && filters.sqlQuery) {
            return [];
        }

        const { whereSql, params } = this._buildWhereClause(filters);

        // Using a window function to group rows by page number, then find the min and max time in each group.
        // This is much more efficient than running a query for each page.
        const sql = `
            WITH RankedLogs AS (
                SELECT
                    time,
                    -- Integer division to calculate a 0-indexed page number for each row
                    (ROW_NUMBER() OVER (ORDER BY time ASC) - 1) / ? AS page_index
                FROM logs
                ${whereSql}
            )
            SELECT
                page_index + 1 AS page,
                MIN(time) AS startTime, -- In ASC order, the min time is the first entry on a page
                MAX(time) AS endTime   -- In ASC order, the max time is the last entry on a page
            FROM RankedLogs
            GROUP BY page_index
            ORDER BY page_index;
        `;

        const ranges: PageTimestampRange[] = [];
        try {
            const finalParams = [pageSize, ...params];
            this._logSql(sql, finalParams);
            const stmt = this.db.prepare(sql);
            // The first '?' is for pageSize in the window function, then come the filter params
            stmt.bind(finalParams);
            while (stmt.step()) {
                const [page, startTime, endTime] = stmt.get();
                ranges.push({ page, startTime, endTime });
            }
            stmt.free();
        } catch (e) {
            console.error("Failed to get page timestamp ranges:", e);
        }
        return ranges;
    }

    getUniqueColumnValues(columnName: 'level' | 'sndrtype' | 'sndrname' | 'fileName'): string[] {
        // Whitelist validation to prevent SQL injection - column names cannot be parameterized
        const allowedColumns = ['level', 'sndrtype', 'sndrname', 'fileName'] as const;
        if (!allowedColumns.includes(columnName)) {
            console.error(`Invalid column name: ${columnName}`);
            return [];
        }

        try {
            const sql = `SELECT DISTINCT ${columnName} FROM logs WHERE ${columnName} IS NOT NULL ORDER BY ${columnName}`;
            this._logSql(sql);
            const stmt = this.db.prepare(sql);
            const values: string[] = [];
            while (stmt.step()) {
                values.push(stmt.get()[0]);
            }
            stmt.free();
            return values;
        } catch (e) {
            // Table might not exist
            return [];
        }
    }

    getMinMaxTime(filters?: FilterState): { minTime: string | null; maxTime: string | null } {
        let result: { minTime: string | null; maxTime: string | null } = { minTime: null, maxTime: null };
        try {
            const { whereSql, params } = this._buildWhereClause(filters || {} as FilterState);
            const sql = `SELECT MIN(time), MAX(time) FROM logs ${whereSql}`;
            this._logSql(sql, params);
            const stmt = this.db.prepare(sql);
            stmt.bind(params);
            if (stmt.step()) {
                const [min, max] = stmt.get();
                if (min !== null && max !== null) {
                    result = { minTime: min, maxTime: max };
                }
            }
            stmt.free();
        } catch (e) {
            // Table might not exist
        }
        return result;
    }

    getMinMaxStockTime(filters?: StockInfoFilters): { minTime: string | null; maxTime: string | null } {
        let result: { minTime: string | null; maxTime: string | null } = { minTime: null, maxTime: null };
        try {
            const { whereSql, params } = this._buildStockWhereClause(filters || {} as StockInfoFilters);
            const sql = `SELECT MIN(timestamp), MAX(timestamp) FROM stock_info ${whereSql}`;
            this._logSql(sql, params);
            const stmt = this.db.prepare(sql);
            stmt.bind(params);
            if (stmt.step()) {
                const [min, max] = stmt.get();
                if (min !== null && max !== null) {
                    result = { minTime: min as string, maxTime: max as string };
                }
            }
            stmt.free();
        } catch (e) {
            // Table might not exist or be empty
        }
        return result;
    }

    getLogVolumeByInterval(filters: FilterState): TimelineDataPoint[] {
        if (filters.sqlQueryEnabled && filters.sqlQuery) {
            return []; // Cannot determine timeline from arbitrary query
        }

        const { whereSql, params } = this._buildWhereClause(filters);
        const { minTime, maxTime } = this.getMinMaxTime(filters);

        if (!minTime || !maxTime) return [];

        const durationHours = (new Date(maxTime).getTime() - new Date(minTime).getTime()) / (1000 * 60 * 60);

        let timeFormat: string;
        if (durationHours < 2) {
            timeFormat = '%Y-%m-%dT%H:%M:00.000Z'; // Group by minute
        } else if (durationHours < 72) {
            timeFormat = '%Y-%m-%dT%H:00:00.000Z'; // Group by hour
        } else {
            timeFormat = '%Y-%m-%dT00:00:00.000Z'; // Group by day
        }

        const sql = `
            SELECT strftime(?, time) as interval_start, COUNT(*) as count 
            FROM logs 
            ${whereSql}
            GROUP BY interval_start 
            ORDER BY interval_start
        `;

        const results: TimelineDataPoint[] = [];
        try {
            const finalParams = [timeFormat, ...params];
            this._logSql(sql, finalParams);
            const stmt = this.db.prepare(sql);
            stmt.bind(finalParams);
            while (stmt.step()) {
                const [time, count] = stmt.get();
                results.push({ time, count });
            }
            stmt.free();
        } catch (e) {
            // Fallback or error logging
            console.error("Failed to get log volume:", e);
        }
        return results;
    }

    getCountsByColumn(columnName: 'level' | 'sndrtype', filters: FilterState): CategoryDataPoint[] {
        // Whitelist validation to prevent SQL injection - column names cannot be parameterized
        const allowedColumns = ['level', 'sndrtype'] as const;
        if (!allowedColumns.includes(columnName)) {
            console.error(`Invalid column name: ${columnName}`);
            return [];
        }

        if (filters.sqlQueryEnabled && filters.sqlQuery) {
            return []; // Cannot determine categories from arbitrary query
        }
        const { whereSql, params } = this._buildWhereClause(filters);
        const sql = `SELECT ${columnName}, COUNT(*) as count FROM logs ${whereSql} GROUP BY ${columnName} ORDER BY count DESC`;
        const results: CategoryDataPoint[] = [];
        try {
            this._logSql(sql, params);
            const stmt = this.db.prepare(sql);
            stmt.bind(params);
            while (stmt.step()) {
                const [name, count] = stmt.get();
                if (name !== null) { // Exclude null groups
                    results.push({ name, count });
                }
            }
            stmt.free();
        } catch (e) {
            console.error(`Failed to get counts for column ${columnName}:`, e);
        }
        return results;
    }

    getTimeRangePerFile(filters: FilterState): FileTimeRange[] {
        if (filters.sqlQueryEnabled && filters.sqlQuery) {
            return [];
        }
        const { whereSql, params } = this._buildWhereClause(filters);
        const sql = `
            SELECT fileName as name, MIN(time) as startTime, MAX(time) as endTime
            FROM logs
            ${whereSql}
            GROUP BY fileName
            HAVING startTime IS NOT NULL AND endTime IS NOT NULL
            ORDER BY startTime;
        `;
        const results: FileTimeRange[] = [];
        try {
            this._logSql(sql, params);
            const stmt = this.db.prepare(sql);
            stmt.bind(params);
            while (stmt.step()) {
                const [name, startTime, endTime] = stmt.get();
                results.push({ name, startTime, endTime });
            }
            stmt.free();
        } catch (e) {
            console.error("Failed to get time range per file:", e);
        }
        return results;
    }

    getDatesWithLogs(filters: FilterState): string[] {
        if (filters.sqlQueryEnabled && filters.sqlQuery) {
            return []; // Cannot determine from arbitrary query
        }
        const { whereSql, params } = this._buildWhereClause(filters);
        const sql = `SELECT DISTINCT strftime('%Y-%m-%d', time) FROM logs ${whereSql} ORDER BY 1`;
        const dates: string[] = [];
        try {
            this._logSql(sql, params);
            const stmt = this.db.prepare(sql);
            stmt.bind(params);
            while (stmt.step()) {
                const dateVal = stmt.get()[0];
                if (dateVal) { // Ensure we don't push nulls
                    dates.push(dateVal);
                }
            }
            stmt.free();
        } catch (e) {
            console.error("Failed to get dates with logs:", e);
        }
        return dates;
    }

    getLogDensityByLevel(filters: FilterState, bucketCount: number, rangeMinMs?: number, rangeMaxMs?: number): LogDensityPointByLevel[] {
        if (filters.sqlQueryEnabled && filters.sqlQuery) {
            return [];
        }

        // If no range provided, use the full data range
        let minTimeMs: number;
        let maxTimeMs: number;
        let baseTime: string;

        if (rangeMinMs !== undefined && rangeMaxMs !== undefined) {
            // Use provided range for zoomed view
            minTimeMs = rangeMinMs;
            maxTimeMs = rangeMaxMs;
            baseTime = new Date(rangeMinMs).toISOString().replace('T', ' ').replace('Z', '');
        } else {
            // Use data range
            const { minTime, maxTime } = this.getMinMaxTime(filters);
            if (!minTime || !maxTime) return [];
            minTimeMs = new Date(minTime).getTime();
            maxTimeMs = new Date(maxTime).getTime();
            baseTime = minTime;
        }

        const totalDuration = maxTimeMs - minTimeMs;
        if (totalDuration <= 0) return [];

        const bucketDuration = totalDuration / bucketCount;

        const { whereSql, params } = this._buildWhereClause(filters);

        // Group by both bucket and level to get compositional counts
        const sql = `
            SELECT
                CAST((julianday(time) - julianday(?)) * 86400000 / ? AS INTEGER) AS bucket,
                level,
                COUNT(*) as count
            FROM logs
            ${whereSql}
            GROUP BY bucket, level
        `;

        const bucketMap: Map<number, Record<string, number>> = new Map();
        try {
            const finalParams = [baseTime, bucketDuration, ...params];
            this._logSql(sql, finalParams);
            const stmt = this.db.prepare(sql);
            stmt.bind(finalParams);
            while (stmt.step()) {
                const [bucket, level, count] = stmt.get();
                if (typeof bucket !== 'number' || typeof level !== 'string' || typeof count !== 'number') continue;

                if (!bucketMap.has(bucket)) {
                    bucketMap.set(bucket, {});
                }
                const levelCounts = bucketMap.get(bucket)!;
                levelCounts[level] = count;
            }
            stmt.free();
        } catch (e) {
            console.error("Failed to get log density by level:", e);
            return [];
        }

        // Pre-allocate array for better performance with large bucket counts
        const densityData: LogDensityPointByLevel[] = new Array(bucketCount);
        for (let i = 0; i < bucketCount; i++) {
            densityData[i] = {
                time: minTimeMs + i * bucketDuration,
                counts: bucketMap.get(i) || {},
            };
        }
        return densityData;
    }

    getStockDensity(filters: StockInfoFilters, bucketCount: number): LogDensityPoint[] {
        const { minTime, maxTime } = this.getMinMaxStockTime(filters);
        if (!minTime || !maxTime) return [];

        const minTimeMs = new Date(minTime).getTime();
        const maxTimeMs = new Date(maxTime).getTime();
        const totalDuration = maxTimeMs - minTimeMs;
        if (totalDuration <= 0) return [];

        const bucketDuration = totalDuration / bucketCount;

        const { whereSql, params } = this._buildStockWhereClause(filters);

        const sql = `
            SELECT
                CAST((julianday(timestamp) - julianday(?)) * 86400000 / ? AS INTEGER) AS bucket,
                COUNT(*) as count
            FROM stock_info
            ${whereSql}
            GROUP BY bucket
        `;

        const dbResults: { bucket: number; count: number }[] = [];
        try {
            const finalParams = [minTime, bucketDuration, ...params];
            this._logSql(sql, finalParams);
            const stmt = this.db.prepare(sql);
            stmt.bind(finalParams);
            while (stmt.step()) {
                const [bucket, count] = stmt.get();
                dbResults.push({ bucket, count });
            }
            stmt.free();
        } catch (e) {
            console.error("Failed to get stock density:", e);
            return [];
        }

        // Pre-allocate and directly initialize for better performance
        const densityData: LogDensityPoint[] = new Array(bucketCount);
        for (let i = 0; i < bucketCount; i++) {
            densityData[i] = {
                time: minTimeMs + i * bucketDuration,
                count: 0
            };
        }

        let maxCount = 0;
        for (const row of dbResults) {
            if (row.bucket >= 0 && row.bucket < bucketCount) {
                densityData[row.bucket].count = row.count;
                if (row.count > maxCount) maxCount = row.count;
            }
        }

        if (maxCount > 0) {
            for (let i = 0; i < bucketCount; i++) {
                densityData[i].count = Math.round((densityData[i].count / maxCount) * 100);
            }
        }

        return densityData;
    }

    getNearestLogEntry(time: number, filters: FilterState): LogEntry | null {
        if (filters.sqlQueryEnabled) return null;

        const { whereSql, params } = this._buildWhereClause(filters);
        const timeAsSqlString = new Date(time).toISOString().replace('T', ' ').replace('Z', '');

        const sql = `
            SELECT id, time, level, sndrtype, sndrname, msg, fileName 
            FROM logs 
            ${whereSql} 
            ORDER BY ABS(julianday(time) - julianday(?)) 
            LIMIT 1
        `;

        try {
            const finalParams = [...params, timeAsSqlString];
            this._logSql(sql, finalParams);
            const stmt = this.db.prepare(sql);
            stmt.bind(finalParams);
            if (stmt.step()) {
                const row = stmt.get();
                const entry: LogEntry = { id: row[0], time: row[1], level: row[2], sndrtype: row[3], sndrname: row[4], msg: row[5], fileName: row[6] };
                stmt.free();
                return entry;
            }
            stmt.free();
        } catch (e) {
            console.error("Failed to get nearest log entry:", e);
        }

        return null;
    }

    getLogEntryIndex(entryId: number, filters: FilterState): number {
        if (filters.sqlQueryEnabled) return -1;
        const { whereSql, params } = this._buildWhereClause(filters);

        const sql = `
            WITH NumberedLogs AS (
                SELECT id, ROW_NUMBER() OVER (ORDER BY time ASC) as rn
                FROM logs
                ${whereSql}
            )
            SELECT rn FROM NumberedLogs WHERE id = ?
        `;

        try {
            const finalParams = [...params, entryId];
            this._logSql(sql, finalParams);
            const stmt = this.db.prepare(sql);
            stmt.bind(finalParams);
            if (stmt.step()) {
                const rowNumber = stmt.get()[0] as number;
                stmt.free();
                return rowNumber - 1; // Return 0-based index
            }
            stmt.free();
        } catch (e) {
            console.error("Failed to get log entry index:", e);
        }

        return -1;
    }


    export(): Uint8Array {
        return this.db.export();
    }
}