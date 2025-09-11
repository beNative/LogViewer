import { FilterState, LogEntry, TimelineDataPoint, CategoryDataPoint, PageTimestampRange, FileTimeRange, LogDensityPoint, StockInfoEntry, StockInfoFilters } from './types.ts';

// sql.js is loaded from a script tag and will be on the window object
declare const initSqlJs: (config: { locateFile: (file: string) => string; }) => Promise<any>;

// Helper to convert date/time inputs to a UTC SQL-comparable format.
export const getSqlDateTime = (dateStr: string, timeStr: string, endOfDay = false): string | null => {
    if (!dateStr) return null;
    let time = timeStr || (endOfDay ? '23:59:59' : '00:00:00');
    if (time.length === 5) time += ':00'; // Add seconds if missing

    // Construct a UTC-based ISO string directly from the UTC date/time parts.
    // This avoids any local timezone interpretation.
    const isoString = `${dateStr}T${time}${endOfDay ? '.999' : '.000'}Z`;

    // Now, we can just format it for SQLite string comparison.
    return isoString.replace('T', ' ').replace('Z', '');
};

export class Database {
    private db: any;

    private constructor(db: any) {
        this.db = db;
    }

    static async create(): Promise<Database> {
        const SQL = await initSqlJs({
            locateFile: (file: string) => `https://cdnjs.cloudflare.com/ajax/libs/sql.js/1.10.3/${file}`
        });
        const db = new SQL.Database();
        return new Database(db);
    }

    static async createFromBuffer(buffer: Uint8Array): Promise<Database> {
        const SQL = await initSqlJs({
            locateFile: (file: string) => `https://cdnjs.cloudflare.com/ajax/libs/sql.js/1.10.3/${file}`
        });
        const db = new SQL.Database(buffer);
        return new Database(db);
    }

    createTable() {
        // Using "IF NOT EXISTS" is crucial for appending data to existing sessions
        // without dropping the table. This now ONLY creates tables. Indexes are handled separately.
        this.db.exec(`
            CREATE TABLE IF NOT EXISTS logs (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                time TEXT,
                level TEXT,
                sndrtype TEXT,
                sndrname TEXT,
                msg TEXT,
                fileName TEXT
            );
        `);
        this.db.exec(`
            CREATE TABLE IF NOT EXISTS meta (
                key TEXT PRIMARY KEY,
                value TEXT
            );
        `);
        this.db.exec(`
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
        `);
    }

    dropIndexes() {
        // Drop all indexes for faster bulk inserts.
        this.db.exec(`DROP INDEX IF EXISTS idx_time;`);
        this.db.exec(`DROP INDEX IF EXISTS idx_level;`);
        this.db.exec(`DROP INDEX IF EXISTS idx_sndrtype;`);
        this.db.exec(`DROP INDEX IF EXISTS idx_sndrname;`);
        this.db.exec(`DROP INDEX IF EXISTS idx_fileName;`);
        this.db.exec(`DROP INDEX IF EXISTS idx_stock_timestamp;`);
        this.db.exec(`DROP INDEX IF EXISTS idx_stock_article_id;`);
        this.db.exec(`DROP INDEX IF EXISTS idx_stock_article_name;`);
    }

    createIndexes() {
        // Recreate all indexes. Can be called after bulk inserts.
        this.db.exec(`CREATE INDEX IF NOT EXISTS idx_time ON logs(time);`);
        this.db.exec(`CREATE INDEX IF NOT EXISTS idx_level ON logs(level);`);
        this.db.exec(`CREATE INDEX IF NOT EXISTS idx_sndrtype ON logs(sndrtype);`);
        this.db.exec(`CREATE INDEX IF NOT EXISTS idx_sndrname ON logs(sndrname);`);
        this.db.exec(`CREATE INDEX IF NOT EXISTS idx_fileName ON logs(fileName);`);
        this.db.exec(`CREATE INDEX IF NOT EXISTS idx_stock_timestamp ON stock_info(timestamp);`);
        this.db.exec(`CREATE INDEX IF NOT EXISTS idx_stock_article_id ON stock_info(article_id);`);
        this.db.exec(`CREATE INDEX IF NOT EXISTS idx_stock_article_name ON stock_info(article_name);`);
    }

    setMeta(key: string, value: string) {
        const stmt = this.db.prepare(`INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)`);
        stmt.run([key, value]);
        stmt.free();
    }

    getMeta(key: string): string | null {
        try {
            const stmt = this.db.prepare(`SELECT value FROM meta WHERE key = ?`);
            let value: string | null = null;
            if (stmt.step()) {
                value = stmt.get()[0] as string;
            }
            stmt.free();
            return value;
        } catch(e) {
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

        const stmt = this.db.prepare(`
            INSERT INTO logs (time, level, sndrtype, sndrname, msg, fileName) 
            VALUES (?, ?, ?, ?, ?, ?)
        `);

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
                } catch(e) {
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

        const stmt = this.db.prepare(`
            INSERT INTO stock_info (timestamp, message_id, source, destination, article_id, article_name, dosage_form, max_sub_item_quantity, quantity)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        `);

        this.db.exec("BEGIN TRANSACTION;");
        try {
            entries.forEach(entry => {
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

    queryStockInfo(filters: StockInfoFilters): StockInfoEntry[] {
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
        const sql = `
            SELECT timestamp, message_id, source, destination, article_id, article_name, dosage_form, max_sub_item_quantity, quantity 
            FROM stock_info 
            ${whereSql} 
            ORDER BY timestamp ASC
        `;
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
        return entries;
    }

    private _buildWhereClause(filters: FilterState): { whereSql: string, params: (string | number)[] } {
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

            if (filters.level?.length > 0) {
                whereClauses.push(`level IN (${'?,'.repeat(filters.level.length).slice(0, -1)})`);
                params.push(...filters.level);
            }
            if (filters.sndrtype?.length > 0) {
                whereClauses.push(`sndrtype IN (${'?,'.repeat(filters.sndrtype.length).slice(0, -1)})`);
                params.push(...filters.sndrtype);
            }
            if (filters.sndrname?.length > 0) {
                whereClauses.push(`sndrname IN (${'?,'.repeat(filters.sndrname.length).slice(0, -1)})`);
                params.push(...filters.sndrname);
            }
            if (filters.fileName?.length > 0) {
                whereClauses.push(`fileName IN (${'?,'.repeat(filters.fileName.length).slice(0, -1)})`);
                params.push(...filters.fileName);
            }

            if (filters.levelExclude?.length > 0) {
                whereClauses.push(`level NOT IN (${'?,'.repeat(filters.levelExclude.length).slice(0, -1)})`);
                params.push(...filters.levelExclude);
            }
            if (filters.sndrtypeExclude?.length > 0) {
                whereClauses.push(`sndrtype NOT IN (${'?,'.repeat(filters.sndrtypeExclude.length).slice(0, -1)})`);
                params.push(...filters.sndrtypeExclude);
            }
            if (filters.sndrnameExclude?.length > 0) {
                whereClauses.push(`sndrname NOT IN (${'?,'.repeat(filters.sndrnameExclude.length).slice(0, -1)})`);
                params.push(...filters.sndrnameExclude);
            }
            if (filters.fileNameExclude?.length > 0) {
                whereClauses.push(`fileName NOT IN (${'?,'.repeat(filters.fileNameExclude.length).slice(0, -1)})`);
                params.push(...filters.fileNameExclude);
            }
            
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
        return { whereSql, params };
    }

    getTotalEntryCount(): number {
      let totalEntries = 0;
      try {
          const totalResult = this.db.exec("SELECT COUNT(*) FROM logs");
          if (totalResult.length > 0 && totalResult[0].values.length > 0) {
               totalEntries = totalResult[0].values[0][0];
          }
      } catch(e) {
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
             try {
                const stmt = this.db.prepare(sql);
                stmt.bind([limit, offset]);
                const entries: LogEntry[] = [];
                const columnNames = stmt.getColumnNames();

                // Map results to LogEntry, filling missing columns with defaults
                while (stmt.step()) {
                    const row = stmt.get();
                    const entry: any = { id: 0, level: 'N/A', time: 'N/A', sndrtype: 'N/A', sndrname: 'N/A', msg: 'N/A', fileName: 'N/A' };
                    columnNames.forEach((colName, i) => {
                        if (Object.prototype.hasOwnProperty.call(entry, colName)) {
                            entry[colName] = row[i];
                        }
                    });
                    // If msg is missing but other fields aren't, serialize the row as msg
                    if (entry.msg === 'N/A' && columnNames.length > 1) {
                        entry.msg = JSON.stringify(Object.fromEntries(columnNames.map((c, i) => [c, row[i]])));
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
        const stmt = this.db.prepare(sql);
        stmt.bind([...params, limit, offset]);

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
            const stmt = this.db.prepare(sql);
            // The first '?' is for pageSize in the window function, then come the filter params
            stmt.bind([pageSize, ...params]);
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
        try {
            const stmt = this.db.prepare(`SELECT DISTINCT ${columnName} FROM logs WHERE ${columnName} IS NOT NULL ORDER BY ${columnName}`);
            const values: string[] = [];
            while(stmt.step()){
                values.push(stmt.get()[0]);
            }
            stmt.free();
            return values;
        } catch(e) {
            // Table might not exist
            return [];
        }
    }
    
    getMinMaxTime(filters?: FilterState): { minTime: string | null; maxTime: string | null } {
        let result = { minTime: null, maxTime: null };
        try {
            const { whereSql, params } = this._buildWhereClause(filters || {} as FilterState);
            const stmt = this.db.prepare(`SELECT MIN(time), MAX(time) FROM logs ${whereSql}`);
            stmt.bind(params);
            if (stmt.step()) {
                const [min, max] = stmt.get();
                if (min !== null && max !== null) {
                  result = { minTime: min, maxTime: max };
                }
            }
            stmt.free();
        } catch(e) {
            // Table might not exist
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
            const stmt = this.db.prepare(sql);
            stmt.bind([timeFormat, ...params]);
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
         if (filters.sqlQueryEnabled && filters.sqlQuery) {
            return []; // Cannot determine categories from arbitrary query
        }
        const { whereSql, params } = this._buildWhereClause(filters);
        const sql = `SELECT ${columnName}, COUNT(*) as count FROM logs ${whereSql} GROUP BY ${columnName} ORDER BY count DESC`;
        const results: CategoryDataPoint[] = [];
        try {
            const stmt = this.db.prepare(sql);
            stmt.bind(params);
            while(stmt.step()){
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

    getLogDensity(filters: FilterState, bucketCount: number): LogDensityPoint[] {
        if (filters.sqlQueryEnabled && filters.sqlQuery) {
            return [];
        }
        const { minTime, maxTime } = this.getMinMaxTime(filters);
        if (!minTime || !maxTime) return [];

        const minTimeMs = new Date(minTime).getTime();
        const maxTimeMs = new Date(maxTime).getTime();
        const totalDuration = maxTimeMs - minTimeMs;
        if (totalDuration <= 0) return [];

        const bucketDuration = totalDuration / bucketCount;

        const { whereSql, params } = this._buildWhereClause(filters);
        
        // julianday returns days since noon in Greenwich on November 24, 4714 B.C.
        // We convert to milliseconds to do the bucketing.
        const sql = `
            SELECT
                CAST((julianday(time) - julianday(?)) * 86400000 / ? AS INTEGER) AS bucket,
                COUNT(*) as count
            FROM logs
            ${whereSql}
            GROUP BY bucket
        `;

        const dbResults: { bucket: number; count: number }[] = [];
        try {
            const stmt = this.db.prepare(sql);
            // Bind minTime for offset calculation, bucketDuration for sizing, and then filter params.
            stmt.bind([minTime, bucketDuration, ...params]);
            while (stmt.step()) {
                const [bucket, count] = stmt.get();
                dbResults.push({ bucket, count });
            }
            stmt.free();
        } catch (e) {
            console.error("Failed to get log density:", e);
            return [];
        }

        // Create a full array of empty buckets and fill it with the DB results.
        const densityData = new Array(bucketCount).fill(0).map((_, i) => ({
            time: minTimeMs + i * bucketDuration,
            count: 0
        }));
        
        let maxCount = 0;
        dbResults.forEach(row => {
            if (row.bucket >= 0 && row.bucket < bucketCount) {
                densityData[row.bucket].count = row.count;
                if (row.count > maxCount) maxCount = row.count;
            }
        });
        
        // Normalize the counts to a 0-100 scale for easier heatmap coloring
        if (maxCount > 0) {
            for (const bucket of densityData) {
                bucket.count = Math.round((bucket.count / maxCount) * 100);
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
            const stmt = this.db.prepare(sql);
            stmt.bind([...params, timeAsSqlString]);
            if (stmt.step()) {
                const row = stmt.get();
                const entry: LogEntry = { id: row[0], time: row[1], level: row[2], sndrtype: row[3], sndrname: row[4], msg: row[5], fileName: row[6] };
                stmt.free();
                return entry;
            }
            stmt.free();
        } catch(e) {
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
            const stmt = this.db.prepare(sql);
            stmt.bind([...params, entryId]);
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