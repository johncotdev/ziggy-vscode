# Ziggy DBL Bytecode System

This document describes the bytecode compilation and execution system in Ziggy DBL.

## Overview

Ziggy DBL supports two execution modes:

1. **Tree-walking Interpreter** - Direct AST interpretation (default)
2. **Bytecode VM** - Compiled bytecode execution (faster)

```
Source (.dbl) → Lexer → Parser → AST → Compiler → Module (.zbc) → VM → Execution
```

## VS Code Extension Support

### Commands

| Command | Keybinding | Description |
|---------|------------|-------------|
| Compile to Bytecode | `Ctrl+Shift+B` | Compile `.dbl` to `.zbc` |
| Disassemble | `Ctrl+Shift+D` | View bytecode disassembly |
| Run with Bytecode VM | Context menu | Execute via bytecode VM |

### Configuration

```json
{
  "ziggy.executionMode": "bytecode",
  "ziggy.showDisassemblyOnCompile": true,
  "ziggy.bytecodeOutputDir": "./build"
}
```

## File Formats

### .zbc - Bytecode Module

Single compiled module with:
- Header (magic: "ZBC1", version, flags)
- Constants section (strings, numbers)
- Types section (record definitions)
- Routines section (subroutine metadata)
- Code section (bytecode instructions)
- Debug section (optional line mappings)

### .zbl - Library Bundle

Multiple modules bundled together for linking.
Magic: "ZBL1"

### .zbx - Executable

Fully linked executable with resolved imports.
Magic: "ZBX1"

## Bytecode Architecture

### Stack-Based VM

The VM uses an operand stack for all computations:

```
push_i8 42       ; Push 42 onto stack
push_i8 8        ; Push 8 onto stack
add              ; Pop two, push sum (50)
store_global @x  ; Pop and store in variable x
```

### Opcode Categories

| Range | Category | Examples |
|-------|----------|----------|
| 0x00-0x0F | Stack | `push_i8`, `pop`, `dup`, `swap` |
| 0x10-0x1F | Locals | `load_local_0`, `store_local` |
| 0x20-0x2F | Globals | `load_global`, `store_global` |
| 0x30-0x3F | Records | `new_record`, `load_field` |
| 0x40-0x4F | Arithmetic | `add`, `sub`, `mul`, `div` |
| 0x50-0x5F | Comparison | `cmp_eq`, `cmp_lt`, `log_and` |
| 0x60-0x6F | Control | `jump`, `jump_if_false` |
| 0x70-0x7F | Calls | `call`, `ret`, `xcall` |
| 0x80-0x8F | Channel I/O | `ch_open`, `ch_display` |
| 0x90-0x9F | ISAM | `isam_read`, `isam_store` |
| 0xA0-0xAF | Strings | `str_concat`, `str_slice` |
| 0xB0-0xBF | Conversion | `to_int`, `to_str` |
| 0xC0-0xCF | Built-ins | `fn_abs`, `fn_date` |
| 0xF0-0xFF | Extended | `halt`, `debug_break` |

### Optimized Opcodes

Common operations have dedicated opcodes:

```
load_local_0    ; Faster than load_local 0
store_local_1   ; Faster than store_local 1
load_field_0    ; Faster than load_field 0
```

## Disassembly Format

The disassembler produces human-readable output:

```
; Ziggy DBL Bytecode Disassembly
; Version: 0.1
; Entry Point: 0x0000

; === Constants (3) ===
;   [0000] id(tt)
;   [0001] str(Hello, World!)
;   [0002] id(customers)

; === Code (24 bytes) ===
  0000:  20 00 00        load_global @0        ; tt
  0003:  08 01 00        push_const #1         ; Hello, World!
  0006:  86 01           ch_display argc=1
  0008:  FF              halt
```

### Disassembly Syntax Highlighting

The extension provides syntax highlighting for disassembly output:
- Addresses: `0000:`
- Opcodes: `load_global`, `push_const`
- Operands: `@0`, `#1`
- Comments: `; Hello, World!`

## Compilation Process

### Three-Pass Compiler

**Pass 1: Collect Declarations**
- Record definitions → TypeDefs
- Field-to-offset mappings

**Pass 2: Collect Globals**
- Field declarations → Global variable slots
- Common blocks → Shared slots

**Pass 3: Code Generation**
- Statements after `proc` → Bytecode
- Expression compilation
- Label resolution

### Expression Compilation

```dbl
; Source
x = a + b * c

; Bytecode
load_global @a
load_global @b
load_global @c
mul
add
store_global @x
```

### Control Flow

```dbl
; Source
if (x > 0) then
    display(tt, "positive")
end

; Bytecode
  load_global @x
  push_i8 0
  cmp_gt
  jump_if_false +12    ; Skip to end
  load_global @tt
  push_const #"positive"
  ch_display 1
  ; (end)
```

## Value Types

The VM supports these runtime value types:

| Type | Description |
|------|-------------|
| `null` | Null/uninitialized |
| `boolean` | True/false |
| `integer` | 64-bit signed integer |
| `decimal` | Decimal with precision |
| `alpha` | Fixed-size string |
| `string` | Dynamic string |
| `record_ref` | Record reference |
| `handle` | External resource handle |

## Built-in Functions

Built-in functions map directly to opcodes:

| Function | Opcode |
|----------|--------|
| `%ABS(n)` | `fn_abs` |
| `%TRIM(s)` | `str_trim` |
| `%DATE()` | `fn_date` |
| `%SIZE(v)` | `fn_size` |
| `%SQRT(n)` | `fn_sqrt` |
| `%SIN(n)` | `fn_sin` |
| `%COS(n)` | `fn_cos` |

## ISAM Operations

ISAM database operations have dedicated opcodes:

```dbl
; Source
open(1, U:I, "customers.ism")
store(1, customer_rec)
read(1, customer_rec, cust_id)

; Bytecode
push_i8 1
push_const #"customers.ism"
isam_open
...
isam_store
...
isam_read
```

## Module Structure

### Header (32 bytes)

```
Offset  Size  Field
0       4     Magic ("ZBC1")
4       2     Version major
6       2     Version minor
8       4     Flags
12      4     Section count
16      4     Entry point
20      4     Source hash
24      8     Reserved
```

### Flags

| Flag | Bit | Description |
|------|-----|-------------|
| `has_debug` | 0 | Debug info included |
| `has_native` | 1 | Native code sections |
| `requires_isam` | 2 | ISAM operations used |
| `is_library` | 3 | Library (no entry point) |

## Source Files

The bytecode system is implemented in:

| File | Purpose |
|------|---------|
| `src/bytecode/opcodes.zig` | Opcode definitions |
| `src/bytecode/module.zig` | Binary format |
| `src/bytecode/compiler.zig` | AST→bytecode |
| `src/bytecode/vm.zig` | Virtual machine |
| `src/bytecode/disasm.zig` | Disassembler |

## Technical Documentation

For complete technical details, see:

- `~/ziggy/tech-docs/bytecode.md` - Implementation details
- `~/ziggy/docs/bytecode-design.md` - Full specification (1000+ lines)
- `~/ziggy/tech-docs/architecture.md` - System architecture
