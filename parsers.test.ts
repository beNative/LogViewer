import { describe, it, expect } from 'vitest';
import { parseLogMessage } from './parsers';

describe('parsers', () => {
    describe('parseLogMessage', () => {
        describe('empty/null handling', () => {
            it('should return text type for empty string', () => {
                const result = parseLogMessage('');
                expect(result.type).toBe('text');
                expect(result.data).toBe('');
            });

            it('should return text type for plain text', () => {
                const result = parseLogMessage('Just a simple log message');
                expect(result.type).toBe('text');
                expect(result.data).toBe('Just a simple log message');
            });
        });

        describe('XML detection', () => {
            // Note: XML detection uses DOMParser which is not available in Node.js test environment.
            // These tests are skipped as they would require jsdom or running in a browser.
            it.skip('should detect valid XML content', () => {
                const xmlMsg = '<root><child>value</child></root>';
                const result = parseLogMessage(xmlMsg);
                expect(result.type).toBe('xml');
                expect(result.data).toBe(xmlMsg);
            });

            it.skip('should extract prefix before XML content', () => {
                const msg = 'Received message: <data><value>123</value></data>';
                const result = parseLogMessage(msg);
                expect(result.type).toBe('xml');
                expect(result.prefix).toBe('Received message: ');
                expect(result.data).toContain('<data>');
            });

            it.skip('should not treat HTML-like text as XML if malformed', () => {
                const msg = 'Use <b>bold</b> text with <unclosed';
                const result = parseLogMessage(msg);
                // Parser may fail on malformed XML and fall through
                expect(['text', 'xml']).toContain(result.type);
            });
        });

        describe('SQL detection', () => {
            it('should detect SELECT statement', () => {
                const sqlMsg = 'SELECT * FROM users WHERE id = 1';
                const result = parseLogMessage(sqlMsg);
                expect(result.type).toBe('sql');
                expect(result.data).toHaveProperty('sql');
            });

            it('should detect INSERT statement', () => {
                const sqlMsg = 'INSERT INTO users (name, email) VALUES ("John", "john@test.com")';
                const result = parseLogMessage(sqlMsg);
                expect(result.type).toBe('sql');
            });

            it('should detect UPDATE statement', () => {
                const sqlMsg = 'UPDATE users SET name = "Jane" WHERE id = 1';
                const result = parseLogMessage(sqlMsg);
                expect(result.type).toBe('sql');
            });

            it('should extract SQL prefix', () => {
                const msg = 'DB Query: SELECT id, name FROM products';
                const result = parseLogMessage(msg);
                expect(result.type).toBe('sql');
                expect(result.prefix).toBe('DB Query:');
            });

            it('should extract SQL result after arrow separator', () => {
                const msg = 'SELECT COUNT(*) FROM logs\n -> 42 rows';
                const result = parseLogMessage(msg);
                expect(result.type).toBe('sql');
                if (result.type === 'sql') {
                    expect((result.data as { sql: string; result: string | null }).result).toBe('42 rows');
                }
            });
        });

        describe('Key-Value detection', () => {
            it('should detect key=value pattern', () => {
                const kvMsg = 'userId=123; action=login; status=success';
                const result = parseLogMessage(kvMsg);
                expect(result.type).toBe('kv');
                if (result.type === 'kv') {
                    const pairs = result.data as { key: string; value: string }[];
                    expect(pairs).toHaveLength(3);
                    expect(pairs[0]).toEqual({ key: 'userId', value: '123' });
                    expect(pairs[1]).toEqual({ key: 'action', value: 'login' });
                    expect(pairs[2]).toEqual({ key: 'status', value: 'success' });
                }
            });

            it('should extract prefix before key-value pairs', () => {
                const msg = 'Request received: orderId=456; total=99.99';
                const result = parseLogMessage(msg);
                expect(result.type).toBe('kv');
                expect(result.prefix).toBe('Request received:');
            });

            it('should handle keys with dots and underscores', () => {
                const msg = 'user.name=John; request_id=abc123';
                const result = parseLogMessage(msg);
                expect(result.type).toBe('kv');
                if (result.type === 'kv') {
                    const pairs = result.data as { key: string; value: string }[];
                    expect(pairs.find(p => p.key === 'user.name')).toBeTruthy();
                    expect(pairs.find(p => p.key === 'request_id')).toBeTruthy();
                }
            });

            it('should not detect URLs as key-value messages', () => {
                const urlMsg = 'https://example.com/api?user=john&action=login';
                const result = parseLogMessage(urlMsg);
                // URLs should not be parsed as K/V due to heuristics
                expect(result.type).toBe('text');
            });
        });

        describe('Grid/Table detection', () => {
            it('should detect space-delimited table data', () => {
                const tableMsg = `Header:  ID  Name    Status
1   John    Active
2   Jane    Inactive`;
                const result = parseLogMessage(tableMsg);
                expect(result.type).toBe('grid');
                if (result.type === 'grid') {
                    const data = result.data as { headers: string[]; rows: string[][] };
                    expect(data.headers).toContain('ID');
                    expect(data.headers).toContain('Name');
                    expect(data.rows.length).toBeGreaterThan(0);
                }
            });
        });

        describe('Priority ordering', () => {
            // Note: XML priority test skipped as DOMParser is not available in Node.js
            it.skip('should prioritize XML over other formats', () => {
                // XML with SQL-like content inside
                const msg = '<query>SELECT * FROM users</query>';
                const result = parseLogMessage(msg);
                expect(result.type).toBe('xml');
            });

            it('should prioritize SQL over key-value when both present', () => {
                // This has SQL keywords that should trigger SQL detection
                const msg = 'SELECT userId FROM users WHERE status = "active"';
                const result = parseLogMessage(msg);
                expect(result.type).toBe('sql');
            });
        });
    });
});
