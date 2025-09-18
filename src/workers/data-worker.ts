// This script runs in a separate thread as a Web Worker.
// It does not have access to the DOM, window object, or React components.

// Import the sql.js library. Since it's not a module, we use importScripts.
importScripts('https://cdnjs.cloudflare.com/ajax/libs/sql.js/1.10.3/sql-wasm.js');

// Define the type for the SQL.js library, as it's not available in the worker's global scope by default.
declare const initSqlJs: (config: { locateFile: (file: string) => string; }) => Promise<any>;

/**
 * A fast, lightweight XML parser using regular expressions, specifically for the <log ... /> format.
 * This avoids the dependency on DOMParser, which could cause errors in some worker environments.
 * @param xmlContent The raw XML string content.
 * @returns An array of log entry objects.
 */
function parseLogXml(xmlContent: string) {
    const entries = [];
    const logRegex = /<log\s+([^>]+)\/?>/g;
    const attrRegex = /(\S+)=["'](.*?)["']/g;

    const decoder = (str: string) => 
        str.replace(/&quot;/g, '"')
           .replace(/&apos;/g, "'")
           .replace(/&lt;/g, '<')
           .replace(/&gt;/g, '>')
           .replace(/&amp;/g, '&');

    for (const match of xmlContent.matchAll(logRegex)) {
        const attrsContent = match[1];
        const entry: Record<string, string> = {};
        
        for (const attrMatch of attrsContent.matchAll(attrRegex)) {
            entry[attrMatch[1]] = decoder(attrMatch[2]);
        }

        entries.push({
            time: entry.time || '',
            level: entry.level || '',
            sndrtype: entry.sndrtype || '',
            sndrname: entry.sndrname || '',
            msg: entry.msg || '',
        });
    }
    return entries;
}

const handleImportLogs = async (payload: any) => {
    const { xmlFiles } = payload;
    if (!xmlFiles || !Array.isArray(xmlFiles)) {
        throw new Error("Worker received invalid data for log import.");
    }

    postMessage({ type: 'progress', payload: { phase: 'parsing', message: 'Initializing database engine...', progress: 30, details: {} } });
    const SQL = await initSqlJs({ locateFile: (file: string) => `https://cdnjs.cloudflare.com/ajax/libs/sql.js/1.10.3/${file}` });
    const db = new SQL.Database();
    db.exec(`CREATE TABLE logs (id INTEGER PRIMARY KEY, time TEXT, level TEXT, sndrtype TEXT, sndrname TEXT, msg TEXT, fileName TEXT);`);
    
    let totalLogsToInsert = 0;
    let totalLogsInserted = 0;

    postMessage({ type: 'progress', payload: { phase: 'parsing', message: 'Parsing XML files...', progress: 40, details: {} } });
    const allParsedEntries = [];
    for (const [i, xmlFile] of xmlFiles.entries()) {
        const entries = parseLogXml(xmlFile.content);
        if (entries.length === 0) continue;
        allParsedEntries.push({ fileName: xmlFile.name, entries });
        totalLogsToInsert += entries.length;
        postMessage({ type: 'progress', payload: { phase: 'parsing', message: `Parsed ${xmlFile.name} (${entries.length} logs)`, progress: 40 + 20 * ((i + 1) / xmlFiles.length), details: { currentFile: xmlFile.name, fileLogCount: entries.length } }});
    }
    
    postMessage({ type: 'progress', payload: { phase: 'inserting', message: 'Preparing to insert records...', progress: 60, details: {} } });
    const insertStmt = db.prepare(`INSERT INTO logs (time, level, sndrtype, sndrname, msg, fileName) VALUES (?, ?, ?, ?, ?, ?)`);
    db.exec("BEGIN TRANSACTION;");
    try {
        for (const { fileName, entries } of allParsedEntries) {
             postMessage({ type: 'progress', payload: { phase: 'inserting', message: `Inserting ${entries.length.toLocaleString()} entries from ${fileName}...`, progress: 60 + 30 * (totalLogsInserted / (totalLogsToInsert || 1)), details: { currentFile: fileName, fileLogCount: entries.length } }});
            for (const entry of entries) {
                const sqlTime = entry.time.replace(/ (\d+)$/, '.$1');
                insertStmt.run([sqlTime, entry.level, entry.sndrtype, entry.sndrname, entry.msg, fileName]);
                totalLogsInserted++;
            }
        }
        db.exec("COMMIT;");
    } catch(e) {
        db.exec("ROLLBACK;");
        throw e;
    } finally {
        insertStmt.free();
    }

    postMessage({ type: 'progress', payload: { phase: 'indexing', message: 'Creating database indexes...', progress: 95, details: {} } });
    db.exec(`CREATE INDEX idx_time ON logs(time);`);
    db.exec(`CREATE INDEX idx_level ON logs(level);`);
    db.exec(`CREATE INDEX idx_sndrtype ON logs(sndrtype);`);
    db.exec(`CREATE INDEX idx_sndrname ON logs(sndrname);`);
    db.exec(`CREATE INDEX idx_fileName ON logs(fileName);`);
    
    const dbBuffer = db.export();
    postMessage({ type: 'done-import', payload: { dbBuffer, count: totalLogsInserted } }, { transfer: [dbBuffer.buffer] });
};


const handleRebuildStockData = async (payload: any) => {
    const { dbBuffer } = payload;
    if (!dbBuffer) throw new Error("Worker received no database buffer for stock rebuild.");

    postMessage({ type: 'progress', payload: { phase: 'loading', message: 'Loading database...', progress: 0 } });
    const SQL = await initSqlJs({ locateFile: (file: string) => `https://cdnjs.cloudflare.com/ajax/libs/sql.js/1.10.3/${file}` });
    const db = new SQL.Database(dbBuffer);
    
    postMessage({ type: 'progress', payload: { phase: 'parsing', message: 'Clearing old stock data...', progress: 5 } });
    // FIX: Ensure the stock_info table exists before trying to delete from it.
    // This prevents errors when rebuilding on a new/blank session.
    db.exec(`
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
    db.exec("DELETE FROM stock_info;");
    
    const countResult = db.exec("SELECT COUNT(*) FROM logs WHERE msg LIKE '%<StockInfoMessage%'");
    const totalToScan = countResult[0]?.values[0]?.[0] || 0;
    if (totalToScan === 0) {
        const finalDbBuffer = db.export();
        postMessage({ type: 'done-rebuild', payload: { dbBuffer: finalDbBuffer, count: 0 } }, { transfer: [finalDbBuffer.buffer] });
        return;
    }

    // FIX: Added the 'g' (global) flag to the regex to fix "matchAll" error.
    const stockRegex = /<WWKS[^>]*?TimeStamp=["'](?<timestamp>[^"']+)["'][^>]*?>.*?<StockInfoMessage[^>]*?Id=["'](?<message_id>[^"']+)["'][^>]*?Source=["'](?<source>[^"']+)["'][^>]*?Destination=["'](?<destination>[^"']+)["'][^>]*?>.*?<Article[^>]*?Id=["'](?<article_id>[^"']+)["'][^>]*?Name=["'](?<article_name>[^"']+)["'][^>]*?DosageForm=["'](?<dosage_form>[^"']+)["'][^>]*?MaxSubItemQuantity=["'](?<max_sub_item_quantity>[^"']+)["'][^>]*?Quantity=["'](?<quantity>[^"']+)["'][^>]*?\/>.*?<\/StockInfoMessage>.*?<\/WWKS>/sg;
    
    const selectStmt = db.prepare("SELECT msg FROM logs WHERE msg LIKE '%<StockInfoMessage%'");
    
    const stockEntriesToInsert = [];
    let processedCount = 0;

    while (selectStmt.step()) {
        const [msg] = selectStmt.get();
        if (typeof msg === 'string') {
            for (const match of msg.matchAll(stockRegex)) {
                if (match.groups) {
                    stockEntriesToInsert.push({
                        timestamp: new Date(match.groups.timestamp).toISOString(),
                        message_id: parseInt(match.groups.message_id || '0', 10),
                        source: match.groups.source || 'N/A',
                        destination: match.groups.destination || 'N/A',
                        article_id: match.groups.article_id || 'N/A',
                        article_name: match.groups.article_name || 'N/A',
                        dosage_form: match.groups.dosage_form || 'N/A',
                        max_sub_item_quantity: parseInt(match.groups.max_sub_item_quantity || '0', 10),
                        quantity: parseInt(match.groups.quantity || '0', 10),
                    });
                }
            }
        }
        processedCount++;
        if (processedCount % 100 === 0 || processedCount === totalToScan) {
            postMessage({ type: 'progress', payload: { phase: 'parsing', message: `Scanned ${processedCount.toLocaleString()}/${totalToScan.toLocaleString()} logs...`, progress: 10 + 60 * (processedCount / totalToScan) } });
        }
    }
    selectStmt.free();

    postMessage({ type: 'progress', payload: { phase: 'inserting', message: `Inserting ${stockEntriesToInsert.length.toLocaleString()} stock entries...`, progress: 75 } });
    if (stockEntriesToInsert.length > 0) {
        const insertStmt = db.prepare(`INSERT INTO stock_info (timestamp, message_id, source, destination, article_id, article_name, dosage_form, max_sub_item_quantity, quantity) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)`);
        db.exec("BEGIN TRANSACTION;");
        try {
            stockEntriesToInsert.forEach(entry => {
                insertStmt.run([entry.timestamp.replace('T', ' ').replace('Z', ''), entry.message_id, entry.source, entry.destination, entry.article_id, entry.article_name, entry.dosage_form, entry.max_sub_item_quantity, entry.quantity]);
            });
            db.exec("COMMIT;");
        } catch (e) {
            db.exec("ROLLBACK;");
            throw e;
        } finally {
            insertStmt.free();
        }
    }

    postMessage({ type: 'progress', payload: { phase: 'indexing', message: 'Updating stock indexes...', progress: 95 } });
    db.exec(`DROP INDEX IF EXISTS idx_stock_timestamp;`);
    db.exec(`DROP INDEX IF EXISTS idx_stock_article_id;`);
    db.exec(`DROP INDEX IF EXISTS idx_stock_article_name;`);
    db.exec(`CREATE INDEX IF NOT EXISTS idx_stock_timestamp ON stock_info(timestamp);`);
    db.exec(`CREATE INDEX IF NOT EXISTS idx_stock_article_id ON stock_info(article_id);`);
    db.exec(`CREATE INDEX IF NOT EXISTS idx_stock_article_name ON stock_info(article_name);`);

    const finalDbBuffer = db.export();
    postMessage({ type: 'done-rebuild', payload: { dbBuffer: finalDbBuffer, count: stockEntriesToInsert.length } }, { transfer: [finalDbBuffer.buffer] });
};

// Main message handler for the worker
self.onmessage = async (event) => {
    try {
        const { type, payload } = event.data;
        if (type === 'import-logs') {
            await handleImportLogs(payload);
        } else if (type === 'rebuild-stock') {
            await handleRebuildStockData(payload);
        } else {
            throw new Error(`Unknown worker command: ${type}`);
        }
    } catch (e) {
        const error = e instanceof Error ? e.message : 'An unknown error occurred in the worker.';
        postMessage({ type: 'error', payload: { error } });
    }
};