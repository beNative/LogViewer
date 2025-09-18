// This script runs in a separate thread as a Web Worker.
// It does not have access to the DOM, window object, or React components.

// Import the sql.js library. Since it's not a module, we use importScripts.
importScripts('https://cdnjs.cloudflare.com/ajax/libs/sql.js/1.10.3/sql-wasm.js');

// Define the type for the SQL.js library, as it's not available in the worker's global scope by default.
declare const initSqlJs: (config: { locateFile: (file: string) => string; }) => Promise<any>;

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
        
        const parser = new DOMParser();
        let totalLogsToInsert = 0;
        let totalLogsInserted = 0;

        // --- Parsing Phase ---
        postMessage({ type: 'progress', payload: { phase: 'parsing', message: 'Parsing XML files...', progress: 40, details: {} } });
        const allParsedEntries = [];
        for (const [i, xmlFile] of xmlFiles.entries()) {
            const doc = parser.parseFromString(xmlFile.content, "application/xml");
            const logNodes = doc.querySelectorAll('log');
            if (logNodes.length === 0) continue;

            const entries = Array.from(logNodes).map(node => ({
                time: node.getAttribute('time') || '',
                level: node.getAttribute('level') || '',
                sndrtype: node.getAttribute('sndrtype') || '',
                sndrname: node.getAttribute('sndrname') || '',
                msg: node.getAttribute('msg') || '',
            }));
            
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
                    progress: 60 + 30 * (totalLogsInserted / totalLogsToInsert),
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
        // FIX: Replaced the Transferable[] syntax with the modern options object to resolve a TypeScript error where the linter incorrectly assumes a 'window' context for the worker's postMessage function.
        postMessage({ 
            type: 'done', 
            payload: { 
                dbBuffer, 
                count: totalLogsInserted 
            }
        }, { transfer: [dbBuffer.buffer] }); // Transfer buffer for performance

    } catch (e) {
        const error = e instanceof Error ? e.message : 'An unknown error occurred in the worker.';
        postMessage({ type: 'error', payload: { error } });
    }
};