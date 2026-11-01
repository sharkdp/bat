// Run with Node.js 22+: node tests/wasi/smoke.mjs path/to/bat.wasm
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { WASI } from 'node:wasi';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

// Model real CLI invocations with one WASI instance per host process. Besides
// bounding memory, this avoids the Node 22 crash observed after repeatedly
// creating and exiting WASI instances in the same process.
if (process.argv[3] === '--case') {
    const { args, env, root } = JSON.parse(process.argv[4]);
    const wasi = new WASI({ version: 'preview1', args, env, preopens: { '/': root }, returnOnExit: true });
    const module = await WebAssembly.compile(fs.readFileSync(process.argv[2]));
    const instance = await WebAssembly.instantiate(module, wasi.getImportObject());
    process.exit(wasi.start(instance));
}
const root = fs.mkdtempSync(path.join(os.tmpdir(), 'bat-wasi-'));
let checks = 0;
let invocation = 0;
function run(args, { stdin = '', env = {}, config = false, circular = false } = {}) {
    console.log(`WASI invocation ${invocation}: ${args.join(" ")}`);
    const prefix = path.join(root, `run-${invocation++}`);
    fs.writeFileSync(`${prefix}.in`, stdin);
    const input = fs.openSync(`${prefix}.in`, 'r');
    const output = fs.openSync(circular ? `${prefix}.in` : `${prefix}.out`, circular ? 'a' : 'w');
    const error = fs.openSync(`${prefix}.err`, 'w');
    try {
        const result = spawnSync(process.execPath, [fileURLToPath(import.meta.url), process.argv[2], '--case', JSON.stringify({
            args: ['bat', ...(config ? [] : ['--no-config']), ...args],
            env: { TERM: 'xterm-256color', ...env },
            root,
        })], { stdio: [input, output, error], timeout: 30000 });
        assert.equal(result.signal, null, `WASI host terminated with ${result.signal}`);
        if (result.error) throw result.error;
        const code = result.status;
        return {
            code,
            stdout: fs.readFileSync(circular ? `${prefix}.in` : `${prefix}.out`, 'utf8'),
            stderr: fs.readFileSync(`${prefix}.err`, 'utf8'),
        };
    } finally {
        fs.closeSync(input); fs.closeSync(output); fs.closeSync(error);
    }
}
function ok(result, expected) {
    assert.equal(result.code, 0, result.stderr);
    if (expected !== undefined) assert.equal(result.stdout, expected);
    checks++;
    return result.stdout;
}
const plain = ['--paging=never', '--color=never', '--style=plain'];
const strip = text => text.replace(/\x1b\[[0-9;]*m/g, '');
try {
    assert.match(ok(run(['--version'])), /^bat /);
    assert.match(ok(run(['--help'])), /Usage:/);
    assert.match(ok(run(['--list-languages', '--color=never'])), /Rust/);
    assert.match(ok(run(['--list-themes', '--color=never'])), /Monokai Extended/);
    ok(run(plain, { stdin: 'stdin without HOME\n' }), 'stdin without HOME\n');
    ok(run(plain, { stdin: '' }), '');
    ok(run(plain, { stdin: 'αβ\r\n第二行\r\nlast' }), 'αβ\r\n第二行\r\nlast');
    fs.writeFileSync(path.join(root, 'space ü.rs'), 'fn main() {}\n');
    fs.writeFileSync(path.join(root, 'second.txt'), 'second\n');
    ok(run([...plain, '/space ü.rs', '/second.txt']), 'fn main() {}\nsecond\n');
    ok(run([...plain, '--line-range=2:3'], { stdin: 'one\ntwo\nthree\nfour\n' }), 'two\nthree\n');
    for (const [language, code] of [
        ['C', 'int main(void) { return 0; }\n'],
        ['Rust', 'fn main() { println!("hello"); }\n'],
        ['Python', 'def hello():\n    print("hello")\n'],
        ['JSON', '{"hello": true}\n'],
        ['YAML', 'hello: true\n'],
        ['TOML', 'hello = true\n'],
        ['CSS', 'body { color: red; }\n'],
        ['HTML', '<p>Hello</p>\n'],
        ['Markdown', '# Hello\n\n**world**\n'],
        ['JavaScript', 'function hello() { return 42; }\n'],
    ]) {
        const output = ok(run(['--paging=never', '--color=always', '--style=plain', '--language', language], { stdin: code }));
        assert.match(output, /\x1b\[/, language);
        assert.equal(strip(output), code, language);
    }
    const numbered = ok(run(['--paging=never', '--color=never', '--decorations=always', '--style=numbers'], { stdin: 'a\nb\n' }));
    assert.match(numbered, /1.*a\n.*2.*b\n/s);
    const wrapped = ok(run(['--paging=never', '--color=never', '--decorations=always', '--style=plain', '--terminal-width=4', '--wrap=character'], { stdin: 'abcdefgh\n' }));
    assert.equal(wrapped, 'abcd\nefgh\n');
    const missing = run([...plain, '/missing.txt']);
    assert.notEqual(missing.code, 0); assert.match(missing.stderr, /missing.txt/); checks++;
    const paging = run(['--paging=always'], { stdin: 'hello\n' });
    assert.notEqual(paging.code, 0); assert.match(paging.stderr, /Paging is unavailable in WASI/); checks++;
    const circle = run(plain, { stdin: 'original\n', circular: true });
    assert.notEqual(circle.code, 0); assert.match(circle.stderr, /input.*output|output.*input/i);
    assert.equal(circle.stdout, 'original\n'); checks++;
    ok(run(plain, { stdin: '', circular: true }), '');
    for (const [directory, env] of [
        ['.config/bat', { HOME: '/' }],
        ['xdg/bat', { XDG_CONFIG_HOME: '/xdg' }],
        ['custom', { BAT_CONFIG_DIR: '/custom' }],
    ]) {
        fs.mkdirSync(path.join(root, directory), { recursive: true });
        fs.writeFileSync(path.join(root, directory, 'config'), '--line-range=2\n--style=plain\n--color=never\n--paging=never\n');
        ok(run([], { config: true, env, stdin: 'one\ntwo\nthree\n' }), 'two\n');
        fs.rmSync(path.join(root, directory), { recursive: true });
    }
    console.log(`${checks} WASI runtime checks passed`);
} finally {
    fs.rmSync(root, { recursive: true, force: true });
}
