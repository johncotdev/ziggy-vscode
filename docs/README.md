# Ziggy DBL VS Code Extension - Documentation

This documentation provides comprehensive information about the Ziggy DBL VS Code extension and the Ziggy DBL language for developers and AI agents working on this project.

## Quick Reference

| Document | Purpose |
|----------|---------|
| [ZIGGY_LANGUAGE.md](./ZIGGY_LANGUAGE.md) | Complete Ziggy DBL language reference |
| [BYTECODE.md](./BYTECODE.md) | Bytecode system and VM documentation |
| [EXTENSION_ARCHITECTURE.md](./EXTENSION_ARCHITECTURE.md) | VS Code extension implementation details |
| [DEVELOPMENT_GUIDE.md](./DEVELOPMENT_GUIDE.md) | How to develop and extend the extension |

## Project Overview

**Ziggy DBL** is an open-source implementation of the Synergy DBL programming language, written in Zig. This VS Code extension provides language support for editing and executing Ziggy DBL source files, including bytecode compilation support.

### What is Synergy DBL?

Synergy/DE is a mature business application development platform originally derived from DEC's DIBOL language. Components include:
- **Synergy DBL** - The programming language
- **Synergy DBMS** - Database with ISAM support
- **UI Toolkit** - User interface components
- **Repository** - Metadata and schema management

### Ziggy DBL Goals

1. Achieve full DBL feature parity (compile and run DBL programs)
2. Build custom ISAM (Indexed Sequential Access Method) database
3. Provide an open-source alternative without vendor dependencies
4. Educational project for learning Zig and compiler development

## Repository Locations

| Repository | Path | Description |
|------------|------|-------------|
| Ziggy DBL Language | `~/ziggy` | Compiler/interpreter source code |
| VS Code Extension | `~/ziggy-vscode` | This extension |
| Tech Docs | `~/ziggy/tech-docs` | Technical implementation documentation |

## Current Extension Features

### Execution
- **Run with Interpreter** - Tree-walking interpreter execution (`Ctrl+Shift+R`)
- **Run with Bytecode VM** - Compiled bytecode execution
- **Terminal or Output Panel** - Configurable output mode

### Bytecode Support
- **Compile to Bytecode** - Compile `.dbl` to `.zbc` files (`Ctrl+Shift+B`)
- **Disassemble** - View bytecode disassembly (`Ctrl+Shift+D`)
- **Bytecode File Support** - Recognize `.zbc`, `.zbl`, `.zbx` files

### Editor Features
- **Syntax Highlighting** - Full TextMate grammar for DBL and bytecode disassembly
- **Code Folding** - Fold record, group, class, and control structures
- **Bracket Matching** - Automatic bracket pairing
- **Comment Toggling** - Toggle line comments with `;`

## File Types

| Extension | Description |
|-----------|-------------|
| `.dbl` | DBL source code |
| `.zbc` | Compiled bytecode module |
| `.zbl` | Bytecode library bundle |
| `.zbx` | Linked executable |

## Commands

| Command | Keybinding | Description |
|---------|------------|-------------|
| Run DBL File | `Ctrl+Shift+R` | Execute with interpreter |
| Run with Bytecode VM | - | Execute via bytecode VM |
| Compile to Bytecode | `Ctrl+Shift+B` | Compile to `.zbc` |
| Disassemble | `Ctrl+Shift+D` | Show bytecode disassembly |

## Configuration

| Setting | Default | Description |
|---------|---------|-------------|
| `ziggy.executablePath` | `"ziggy"` | Path to ziggy executable |
| `ziggy.runInTerminal` | `true` | Run in terminal vs output panel |
| `ziggy.executionMode` | `"interpreter"` | Default: interpreter or bytecode |
| `ziggy.showDisassemblyOnCompile` | `false` | Auto-show disassembly after compile |
| `ziggy.bytecodeOutputDir` | `""` | Output directory for compiled files |

## What's NOT Implemented

- Language Server Protocol (LSP) - No IntelliSense/autocomplete
- Debugging support
- Code snippets
- Semantic highlighting

## Key Files

```
ziggy-vscode/
├── src/extension.ts                    # Main extension logic
├── syntaxes/
│   ├── dbl.tmLanguage.json            # DBL syntax highlighting
│   └── zbc-disasm.tmLanguage.json     # Disassembly syntax highlighting
├── language-configuration.json         # Editor behavior
├── package.json                        # Extension manifest
└── docs/                               # This documentation
```

## Getting Started for Developers

1. Clone the repository
2. Run `npm install`
3. Open in VS Code
4. Press `F5` to launch Extension Development Host
5. Open a `.dbl` file to test

See [DEVELOPMENT_GUIDE.md](./DEVELOPMENT_GUIDE.md) for detailed instructions.

## Ziggy Technical Documentation

For in-depth technical details about the Ziggy implementation, see:

- `~/ziggy/tech-docs/README.md` - Tech docs overview
- `~/ziggy/tech-docs/bytecode.md` - Bytecode system details
- `~/ziggy/tech-docs/architecture.md` - System architecture
- `~/ziggy/docs/bytecode-design.md` - Full bytecode specification
