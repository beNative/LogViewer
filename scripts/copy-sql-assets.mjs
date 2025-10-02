import { promises as fs } from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const projectRoot = path.resolve(__dirname, '..');
const distDir = path.resolve(projectRoot, 'dist');

const assets = [
  ['node_modules/sql.js/dist/sql-wasm.wasm', 'sql-wasm.wasm'],
  ['node_modules/sql.js/dist/sql-wasm.js', 'sql-wasm.js']
];

async function copyAssets() {
  await fs.mkdir(distDir, { recursive: true });

  await Promise.all(
    assets.map(async ([sourceRel, targetName]) => {
      const sourcePath = path.resolve(projectRoot, sourceRel);
      const targetPath = path.resolve(distDir, targetName);
      try {
        await fs.copyFile(sourcePath, targetPath);
      } catch (error) {
        console.error(`Failed to copy ${sourceRel} -> ${targetName}:`, error);
        throw error;
      }
    })
  );

  console.log('sql.js assets copied to dist/.');
}

copyAssets().catch((error) => {
  process.exitCode = 1;
});
