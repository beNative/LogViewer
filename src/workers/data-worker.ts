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
    // Regex to find <log ... /> tags and capture their attributes content.
    const logRegex = /<log\s+([^>]+)\/?>/g;
    // Regex to find key="value" pairs within the attributes string.
    const attrRegex = /(\S+)=["'](.*?)["']/g;

    // A simple decoder for common XML entities.
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


// Main message handler for the worker
self.onmessage = async (event) => {
    try {
        const { xmlFiles } = event.data;
        if (!xmlFiles || !Array.isArray(xmlFiles)) {
            throw new Error("Worker received invalid data. Expected 'xmlFiles' array.");
        }

        // Initialize sql.js
        postMessage({ type: 'progress', payload: { phase: 'parsing', message: 'Initializing database engine...', progress: 30, details: {} } });
        const SQL = await initSqlJs({
            locateFile: (file: string) => `https://cdnjs.cloudflare.com/ajax/libs/sql.js/1.10.3/${file}`
        });
        const db = new SQL.Database();
        db.exec(`
            CREATE TABLE logs (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                time TEXT,
                level TEXT,
                sndrtype TEXT,
                sndrname TEXT,
                msg TEXT,
                fileName TEXT
            );
        `);
        
        let totalLogsToInsert = 0;
        let totalLogsInserted = 0;

        // --- Parsing Phase ---
        postMessage({ type: 'progress', payload: { phase: 'parsing', message: 'Parsing XML files...', progress: 40, details: {} } });
        const allParsedEntries = [];
        for (const [i, xmlFile] of xmlFiles.entries()) {
            // Use the new, fast regex parser instead of DOMParser
            const entries = parseLogXml(xmlFile.content);
            if (entries.length === 0) continue;

            allParsedEntries.push({ fileName: xmlFile.name, entries });
            totalLogsToInsert += entries.length;
            
             postMessage({ type: 'progress', payload: {
                phase: 'parsing',
                message: `Parsed ${xmlFile.name} (${entries.length} logs)`,
                progress: 40 + 20 * ((i + 1) / xmlFiles.length),
                details: { currentFile: xmlFile.name, fileLogCount: entries.length }
            }});
        }
        
        // --- Insertion Phase ---
        postMessage({ type: 'progress', payload: { phase: 'inserting', message: 'Preparing to insert records...', progress: 60, details: {} } });
        const insertStmt = db.prepare(`
            INSERT INTO logs (time, level, sndrtype, sndrname, msg, fileName) 
            VALUES (?, ?, ?, ?, ?, ?)
        `);
        
        db.exec("BEGIN TRANSACTION;");
        try {
            for (const { fileName, entries } of allParsedEntries) {
                 postMessage({ type: 'progress', payload: {
                    phase: 'inserting',
                    message: `Inserting ${entries.length.toLocaleString()} entries from ${fileName}...`,
                    progress: 60 + 30 * (totalLogsInserted / (totalLogsToInsert || 1)),
                    details: { currentFile: fileName, fileLogCount: entries.length }
                }});

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

        // --- Indexing Phase ---
        postMessage({ type: 'progress', payload: { phase: 'indexing', message: 'Creating database indexes...', progress: 95, details: {} } });
        db.exec(`CREATE INDEX idx_time ON logs(time);`);
        db.exec(`CREATE INDEX idx_level ON logs(level);`);
        db.exec(`CREATE INDEX idx_sndrtype ON logs(sndrtype);`);
        db.exec(`CREATE INDEX idx_sndrname ON logs(sndrname);`);
        db.exec(`CREATE INDEX idx_fileName ON logs(fileName);`);
        
        // --- Done ---
        const dbBuffer = db.export();
        postMessage({ 
            type: 'done', 
            payload: { 
                dbBuffer, 
                count: totalLogsInserted 
            }
        }, { transfer: [dbBuffer.buffer] });

    } catch (e) {
        const error = e instanceof Error ? e.message : 'An unknown error occurred in the worker.';
        postMessage({ type: 'error', payload: { error } });
    }
};