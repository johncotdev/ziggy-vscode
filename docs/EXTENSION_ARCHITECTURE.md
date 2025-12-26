# VS Code Extension Architecture

This document details the implementation of the Ziggy DBL VS Code extension.

## Directory Structure

```
ziggy-vscode/
├── src/
│   └── extension.ts                  # Main extension entry point
├── syntaxes/
│   ├── dbl.tmLanguage.json          # DBL TextMate grammar
│   └── zbc-disasm.tmLanguage.json   # Bytecode disassembly grammar
├── out/                              # Compiled output
│   ├── extension.js
│   └── extension.js.map
├── package.json                      # Extension manifest
├── language-configuration.json       # Editor behavior
├── tsconfig.json                     # TypeScript config
├── README.md                         # User documentation
├── LICENSE                           # MIT License
└── docs/                             # Developer documentation
```

## Package.json Contributions

### Language Definitions

```json
{
  "languages": [
    {
      "id": "dbl",
      "aliases": ["DBL", "Synergy DBL", "Ziggy DBL"],
      "extensions": [".dbl", ".DBL"],
      "configuration": "./language-configuration.json"
    },
    {
      "id": "zbc",
      "aliases": ["Ziggy Bytecode", "ZBC"],
      "extensions": [".zbc", ".zbl", ".zbx"]
    },
    {
      "id": "zbc-disasm",
      "aliases": ["Ziggy Bytecode Disassembly"]
    }
  ]
}
```

### Grammars

```json
{
  "grammars": [
    {
      "language": "dbl",
      "scopeName": "source.dbl",
      "path": "./syntaxes/dbl.tmLanguage.json"
    },
    {
      "language": "zbc-disasm",
      "scopeName": "source.zbc-disasm",
      "path": "./syntaxes/zbc-disasm.tmLanguage.json"
    }
  ]
}
```

### Commands

| Command ID | Title | Icon | Description |
|-----------|-------|------|-------------|
| `ziggy.run` | Run DBL File | `$(play)` | Execute with interpreter |
| `ziggy.runBytecode` | Run with Bytecode VM | `$(vm)` | Execute via bytecode VM |
| `ziggy.compile` | Compile to Bytecode | `$(package)` | Compile to .zbc |
| `ziggy.disassemble` | Disassemble Bytecode | `$(code)` | View disassembly |

### Keybindings

| Keybinding | Platform | Command | Condition |
|-----------|----------|---------|-----------|
| `Ctrl+Shift+R` | Windows/Linux | Run | `editorLangId == dbl` |
| `Cmd+Shift+R` | macOS | Run | `editorLangId == dbl` |
| `Ctrl+Shift+B` | Windows/Linux | Compile | `editorLangId == dbl` |
| `Cmd+Shift+B` | macOS | Compile | `editorLangId == dbl` |
| `Ctrl+Shift+D` | Windows/Linux | Disassemble | `editorLangId == dbl \|\| zbc` |
| `Cmd+Shift+D` | macOS | Disassemble | `editorLangId == dbl \|\| zbc` |

### Menu Contributions

**Editor Title Bar:**
- Run button for `.dbl` files
- Run Bytecode button for `.dbl` and `.zbc` files

**Context Menu:**
- Run DBL File
- Compile to Bytecode
- Disassemble Bytecode
- Run with Bytecode VM

### Configuration Settings

```json
{
  "ziggy.executablePath": {
    "type": "string",
    "default": "ziggy",
    "description": "Path to the ziggy executable"
  },
  "ziggy.runInTerminal": {
    "type": "boolean",
    "default": true,
    "description": "Run in terminal (true) or output panel (false)"
  },
  "ziggy.executionMode": {
    "type": "string",
    "enum": ["interpreter", "bytecode"],
    "default": "interpreter",
    "description": "Default execution mode"
  },
  "ziggy.showDisassemblyOnCompile": {
    "type": "boolean",
    "default": false,
    "description": "Show disassembly after compiling"
  },
  "ziggy.bytecodeOutputDir": {
    "type": "string",
    "default": "",
    "description": "Output directory for compiled files"
  }
}
```

## Extension Entry Point

**File:** `src/extension.ts`

### Activation

```typescript
export function activate(context: vscode.ExtensionContext) {
    outputChannel = vscode.window.createOutputChannel('Ziggy DBL');
    disassemblyChannel = vscode.window.createOutputChannel('Ziggy Disassembly');

    // Register commands
    const runCommand = vscode.commands.registerCommand('ziggy.run', () => runDblFile('interpreter'));
    const runBytecodeCommand = vscode.commands.registerCommand('ziggy.runBytecode', () => runDblFile('bytecode'));
    const compileCommand = vscode.commands.registerCommand('ziggy.compile', compileToBytecode);
    const disassembleCommand = vscode.commands.registerCommand('ziggy.disassemble', disassembleBytecode);

    context.subscriptions.push(runCommand, runBytecodeCommand, compileCommand, disassembleCommand);
    context.subscriptions.push(outputChannel, disassemblyChannel);
}
```

**Activation Events:**
- `onLanguage:dbl` - Activates when a `.dbl` file is opened
- `onLanguage:zbc` - Activates when a `.zbc` file is opened

### Command Handlers

#### runDblFile(mode)

Executes a DBL file with the specified mode:

1. Gets the active editor
2. Validates file type (`.dbl` or `.zbc`)
3. Saves the file if dirty
4. Reads configuration settings
5. Builds command arguments (adds `--bytecode` for bytecode mode)
6. Executes via terminal or output panel

#### compileToBytecode()

Compiles DBL source to bytecode:

1. Validates `.dbl` file
2. Saves the file
3. Determines output path (uses `bytecodeOutputDir` if set)
4. Runs `ziggy compile <file> -o <output.zbc>`
5. Shows success message
6. Optionally shows disassembly

#### disassembleBytecode()

Shows bytecode disassembly:

1. Validates `.dbl` or `.zbc` file
2. Runs `ziggy disasm <file>`
3. Opens disassembly in new editor with syntax highlighting

### Execution Modes

#### Terminal Mode (default)

```typescript
function runInTerminalMode(executablePath: string, args: string[], filePath: string): void {
    let terminal = vscode.window.terminals.find(t => t.name === 'Ziggy DBL');
    if (!terminal) {
        terminal = vscode.window.createTerminal('Ziggy DBL');
    }
    terminal.show();
    const command = `"${executablePath}" ${args.map(a => `"${a}"`).join(' ')}`;
    terminal.sendText(`cd "${path.dirname(filePath)}" && ${command}`);
}
```

#### Output Panel Mode

```typescript
function runInOutputPanel(executablePath: string, args: string[], filePath: string): void {
    outputChannel.clear();
    outputChannel.show();

    const process = spawn(executablePath, args, {
        cwd: path.dirname(filePath),
        shell: true
    });

    process.stdout.on('data', (data) => outputChannel.append(data.toString()));
    process.stderr.on('data', (data) => outputChannel.append(data.toString()));
    process.on('close', (code) => { /* handle exit */ });
}
```

### Deactivation

```typescript
export function deactivate() {
    if (outputChannel) outputChannel.dispose();
    if (disassemblyChannel) disassemblyChannel.dispose();
}
```

## TextMate Grammars

### DBL Grammar (`syntaxes/dbl.tmLanguage.json`)

**Scope Name:** `source.dbl`

**Pattern Categories:**

1. **Comments:** `;` line comments
2. **Strings:** Double and single quoted with escapes
3. **Numbers:** Integers and decimals
4. **Operators:** Dot operators (`.EQ.`, `.AND.`) and symbols
5. **Keywords:** Structure, control flow, loops, I/O, OOP, etc.
6. **Types:** `a`, `d`, `i1-i8`, `p`, `string`, `@struct`
7. **Functions:** `%functionname`
8. **Variables:** Identifiers

### Bytecode Disassembly Grammar (`syntaxes/zbc-disasm.tmLanguage.json`)

**Scope Name:** `source.zbc-disasm`

**Pattern Categories:**

1. **Comments:** `;` comments and section headers
2. **Addresses:** `0000:` hex addresses
3. **Opcodes by category:**
   - Stack: `push_i8`, `pop`, `dup`, `swap`
   - Locals: `load_local_0`, `store_local`
   - Globals: `load_global`, `store_global`
   - Records: `new_record`, `load_field`
   - Arithmetic: `add`, `sub`, `mul`, `div`
   - Comparison: `cmp_eq`, `cmp_lt`, `log_and`
   - Control: `jump`, `jump_if_false`
   - Calls: `call`, `ret`, `xcall`
   - Channel I/O: `ch_open`, `ch_display`
   - ISAM: `isam_read`, `isam_store`
   - Strings: `str_concat`, `str_trim`
   - Conversion: `to_int`, `to_str`
   - Built-ins: `fn_abs`, `fn_date`
   - Extended: `halt`, `debug_break`
4. **Operands:** `@0`, `#1`, `argc=1`
5. **Constants:** `[0000]`, `id(`, `str(`
6. **Numbers:** Hex bytes, addresses

## Language Configuration

**File:** `language-configuration.json`

### Comments

```json
{ "lineComment": ";" }
```

### Brackets

```json
{ "brackets": [["(", ")"], ["[", "]"]] }
```

### Folding Markers

- **Start:** record, group, structure, if, case, class, method, etc.
- **End:** endrecord, endgroup, endstructure, end, endcase, endclass, etc.

### Indentation Rules

- **Increase after:** if, then, do, while, for, class, method, etc.
- **Decrease on:** end, else, endcase, endclass, endmethod, etc.

## What's NOT Implemented

### Language Server Protocol (LSP)

Missing features:
- IntelliSense/autocomplete
- Go to definition
- Find all references
- Hover documentation
- Rename refactoring
- Diagnostic linting

### Debugging

No Debug Adapter Protocol (DAP):
- No breakpoints
- No step debugging
- No variable inspection

### Snippets

No code snippet definitions.

### Semantic Highlighting

Only static TextMate grammar highlighting.

## Future Development

### 1. Add LSP Support

Create a language server that:
- Uses Ziggy's lexer/parser for analysis
- Provides completion items
- Implements hover providers
- Tracks symbol definitions

### 2. Add Debugging

Implement a debug adapter that:
- Communicates with Ziggy's bytecode VM
- Uses debug opcodes (`debug_line`, `debug_break`)
- Supports breakpoints via line table
- Provides variable inspection

### 3. Add Snippets

Create `snippets/dbl.json` with templates for:
- Record blocks
- If statements
- Loops
- Subroutines/functions
- Class definitions

## Build Process

### Development

```bash
npm install          # Install dependencies
npm run compile      # Compile TypeScript
npm run watch        # Watch mode
```

### Testing

Press `F5` in VS Code to launch Extension Development Host.

### Packaging

```bash
npm install -g @vscode/vsce
vsce package         # Creates .vsix file
```

## Dependencies

```json
{
  "devDependencies": {
    "@types/node": "^20.10.0",
    "@types/vscode": "^1.85.0",
    "typescript": "^5.3.0"
  },
  "engines": {
    "vscode": "^1.85.0"
  }
}
```

## File Reference Summary

| File | Purpose |
|------|---------|
| `src/extension.ts` | Main extension logic (~300 lines) |
| `syntaxes/dbl.tmLanguage.json` | DBL syntax highlighting |
| `syntaxes/zbc-disasm.tmLanguage.json` | Disassembly syntax highlighting |
| `language-configuration.json` | Editor behavior |
| `package.json` | Extension manifest |
