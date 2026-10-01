// Node.js 22+: node examples/wasi.mjs path/to/bat.wasm [bat arguments...]
import fs from 'node:fs';
import { WASI } from 'node:wasi';

const [wasmPath, ...args] = process.argv.slice(2);
if (!wasmPath) {
    console.error('Usage: node examples/wasi.mjs path/to/bat.wasm [bat arguments...]');
    process.exit(2);
}
const wasi = new WASI({
    version: 'preview1',
    args: ['bat', '--paging=never', ...args],
    env: { HOME: '/', TERM: process.env.TERM || 'xterm-256color' },
    preopens: { '/': process.cwd() },
    returnOnExit: true,
});
const module = await WebAssembly.compile(fs.readFileSync(wasmPath));
const instance = await WebAssembly.instantiate(module, wasi.getImportObject());
process.exitCode = wasi.start(instance);
