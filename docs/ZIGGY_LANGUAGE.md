# Ziggy DBL Language Reference

This document provides a comprehensive reference for the Ziggy DBL programming language, an open-source implementation of Synergy DBL written in Zig.

## Language Source Location

The Ziggy DBL compiler/interpreter is located at `~/ziggy`.

## Architecture Overview

```
Source Code (.dbl)
       ↓
   LEXER (src/lexer/)
       ↓
    Tokens
       ↓
   PARSER (src/parser/)
       ↓
      AST (src/ast/)
       ↓
   RUNTIME (src/runtime/)
       ↓
   Execution
```

### Key Source Files

| File | Size | Purpose |
|------|------|---------|
| `src/lexer/lexer.zig` | 14 KB | Tokenizer implementation |
| `src/lexer/token.zig` | 5.1 KB | Token types and definitions |
| `src/parser/parser.zig` | 37 KB | Recursive descent parser |
| `src/ast/ast.zig` | 9.9 KB | Abstract Syntax Tree nodes |
| `src/runtime/runtime.zig` | 37 KB | Tree-walking interpreter |
| `src/builtins/builtins.zig` | 13 KB | Built-in functions |
| `src/isam/isam.zig` | 32 KB | ISAM file system |
| `src/isam/btree.zig` | 12 KB | B+ tree index |

## Program Structure

DBL programs have two divisions:

```dbl
; DATA DIVISION - Variable declarations
record
    name        ,a30
    age         ,d3
endrecord

; PROCEDURE DIVISION - Executable code
proc
    name = "World"
    display(1, "Hello, ", name, "!")
end
```

## Data Types

### Primitive Types

| Type | Syntax | Storage | Description |
|------|--------|---------|-------------|
| Alpha | `a`, `a30` | 1 byte/char | Fixed-length string |
| Decimal | `d`, `d6` | 1 byte/digit | Unpacked decimal |
| Implied Decimal | `d8.2` | Decimal + precision | Decimal with scale |
| Integer | `i1`, `i2`, `i4`, `i8` | 1,2,4,8 bytes | Signed integer |
| Packed Decimal | `p`, `p8` | (digits/2)+1 bytes | Packed BCD |
| String | `string` | Dynamic | Dynamic string |

### Type Syntax Examples

```dbl
record
    customer_name   ,a50        ; 50-character alpha
    quantity        ,d6         ; 6-digit decimal
    price           ,d10.2      ; 10 digits, 2 decimal places
    count           ,i4         ; 32-bit integer
    amount          ,p12        ; 12-digit packed decimal
    message         ,string     ; Dynamic string
endrecord
```

## Data Structures

### RECORD

Named or anonymous variable groups:

```dbl
record customer_rec
    cust_id     ,d8
    cust_name   ,a30
    balance     ,d12.2
endrecord
```

### GROUP

Nested field hierarchies:

```dbl
record
    group address
        street      ,a40
        city        ,a30
        state       ,a2
        zip         ,a10
    endgroup
endrecord
; Access: address.city
```

### STRUCTURE

Reusable templates:

```dbl
structure line_item
    item_code       ,d6
    description     ,a30
    quantity        ,d4
    unit_price      ,d10.2
endstructure

record
    order_lines     ,[10]line_item    ; Array of 10 items
endrecord
```

### LITERAL

Named constants:

```dbl
literal
    MAX_ITEMS       ,d5     ,100
    DEFAULT_NAME    ,a10    ,"UNKNOWN"
    PI              ,d5.4   ,3.1416
endliteral
```

### COMMON

Shared data between routines:

```dbl
common
    shared_counter  ,d10
    shared_buffer   ,a1000
endcommon
```

## Arrays

```dbl
record
    ; Fixed-size array
    items           ,[100]a30

    ; Multi-dimensional
    matrix          ,[10,10]d8

    ; Dynamic (requires allocation)
    dynamic_list    ,[#]i4
endrecord
```

**Indexing** (1-based):

```dbl
items(1) = "First"        ; First element
matrix(row, col) = 42     ; 2D access
```

## Operators

### Arithmetic

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `a + b` |
| `-` | Subtraction | `a - b` |
| `*` | Multiplication | `a * b` |
| `/` | Division | `a / b` |
| `#` | Integer division | `a # b` |
| `//` | Modulo | `a // b` |
| `**` | Exponentiation | `a ** 2` |

### Comparison

| DBL Style | C Style | Description |
|-----------|---------|-------------|
| `.EQ.` | `==` | Equal |
| `.NE.` | `!=` | Not equal |
| `.LT.` | `<` | Less than |
| `.LE.` | `<=` | Less or equal |
| `.GT.` | `>` | Greater than |
| `.GE.` | `>=` | Greater or equal |

### Logical

| DBL Style | C Style | Description |
|-----------|---------|-------------|
| `.AND.` | `&&` | Logical AND |
| `.OR.` | `\|\|` | Logical OR |
| `.NOT.` | `!` | Logical NOT |
| `.XOR.` | - | Exclusive OR |

### Bitwise

| Operator | Description |
|----------|-------------|
| `.BAND.` | Bitwise AND |
| `.BOR.` | Bitwise OR |
| `.BNOT.` | Bitwise NOT |
| `.BXOR.` | Bitwise XOR |
| `<<` | Left shift |
| `>>` | Right shift |

### String Operations

```dbl
; Concatenation
full_name = first_name + " " + last_name

; Substring (position:length)
first_char = name(1:1)

; Range (start,length)
middle = name(5,10)
```

## Control Flow

### IF Statement

```dbl
if (condition) then
    ; statements
else
    ; statements
end
```

### CASE Statement

```dbl
case value of
    1:  display(1, "One")
    2:  display(1, "Two")
    default: display(1, "Other")
endcase
```

### USING Statement

```dbl
using value select
(1),    display(1, "One")
(2, 3), display(1, "Two or Three")
(),     display(1, "Default")
endusing
```

## Loops

### DO-UNTIL

```dbl
do
    ; statements
until (condition)
```

### WHILE

```dbl
while (condition)
    ; statements
end
```

### FOR

```dbl
for i from 1 thru 10
    display(1, i)
end
```

### FOREACH

```dbl
foreach item in collection
    ; process item
end
```

### FOREVER

```dbl
forever
    if (done) exitloop
end
```

### Loop Control

- `exitloop` - Exit the current loop
- `nextloop` - Skip to next iteration

## Subroutines and Functions

### SUBROUTINE

```dbl
subroutine process_customer
    cust_id     ,d8         ; Parameters
    cust_name   ,a50
record
    temp_data   ,a100       ; Local variables
proc
    ; Logic here
    xreturn                 ; Exit
endsubroutine
```

### FUNCTION

```dbl
function calculate_tax, d10.2   ; Return type
    amount      ,d10.2
    rate        ,d5.4
proc
    freturn amount * (rate / 100)
endfunction
```

### Calling

```dbl
; Subroutine call
xcall process_customer(id, name)

; Function call (note % prefix)
tax = %calculate_tax(subtotal, 10.0)
```

## Object-Oriented Programming

### CLASS

```dbl
class Customer
    private m_id        ,d8
    private m_name      ,string

    public method Customer      ; Constructor
        id      ,d8
    proc
        m_id = id
    endmethod

    public method GetName, string
    proc
        mreturn m_name
    endmethod

    public property Id, d8
        method get
        proc
            mreturn m_id
        endmethod
    endproperty
endclass
```

### NAMESPACE

```dbl
namespace MyCompany.OrderProcessing
    class Order
        ; ...
    endclass
endnamespace
```

### Access Modifiers

| Modifier | Scope |
|----------|-------|
| `public` | Accessible everywhere |
| `private` | Class only |
| `protected` | Class and subclasses |
| `internal` | Assembly/namespace |

### Inheritance

```dbl
class PremiumCustomer extends Customer
    private m_discount  ,d5.2

    public override method CalculateTotal, d12.2
        amount  ,d12.2
    proc
        mreturn amount * (1 - m_discount)
    endmethod
endclass
```

## File I/O

### Opening Files

```dbl
open(channel, mode, "filename")
; Modes: I (input), O (output), U (update), A (append)
```

### Reading

```dbl
read(channel, record)           ; ISAM keyed read
reads(channel, record)          ; Sequential read
```

### Writing

```dbl
write(channel, record)          ; Update current record
writes(channel, record)         ; Sequential write
store(channel, record)          ; Insert new ISAM record
```

### Other Operations

```dbl
find(channel, , key)            ; Position without reading
delete(channel)                 ; Delete current record
unlock(channel)                 ; Release lock
close(channel)                  ; Close file
```

## Built-in Functions

### String Functions

| Function | Description |
|----------|-------------|
| `%TRIM(str)` | Remove trailing spaces |
| `%ATRIM(str)` | Trim trailing (alpha) |
| `%LTRIM(str)` | Remove leading spaces |
| `%LEN(str)` | String length |
| `%SIZE(var)` | Variable size |
| `%INSTR(pos, hay, needle)` | Find substring |
| `%UPPER(str)` | Uppercase |
| `%LOWER(str)` | Lowercase |
| `%STRING(n, fmt)` | Format number |

### Numeric Functions

| Function | Description |
|----------|-------------|
| `%ABS(n)` | Absolute value |
| `%INTEGER(x)` | Convert to integer |
| `%DECIMAL(x)` | Convert to decimal |
| `%ROUND(n, [dig])` | Round number |
| `%TRUNC(n)` | Truncate |

### Math Functions

| Function | Description |
|----------|-------------|
| `%SQRT(n)` | Square root |
| `%SIN(n)` | Sine |
| `%COS(n)` | Cosine |
| `%TAN(n)` | Tangent |
| `%LOG(n)` | Natural log |
| `%LOG10(n)` | Base-10 log |
| `%EXP(n)` | e^x |

### Date/Time

| Function | Description |
|----------|-------------|
| `%DATE()` | Current date |
| `%TIME()` | Current time |

### System

| Function | Description |
|----------|-------------|
| `%ERROR()` | Last error |
| `%MEM()` | Memory info |

## ISAM (Indexed Sequential Access)

### Overview

ISAM provides indexed database file access with:
- B+ tree indexes for O(log n) lookups
- Multiple key support
- Record-level locking
- Sequential and random access

### File Structure

```
customer.ism    ← Index file (B+ tree)
customer.is1    ← Data file (records)
```

### Key Definition

Keys can be composite (multiple segments):

```dbl
; Primary key on customer ID
; Secondary key on last_name + first_name
```

### Operations

```dbl
; Open ISAM file
open(1, U:I, "customer.ism")

; Insert record
store(1, customer_rec)

; Read by key
read(1, customer_rec, cust_id)

; Sequential read
reads(1, customer_rec)

; Update
write(1, customer_rec)

; Delete
delete(1)

; Close
close(1)
```

## Comments

```dbl
; This is a comment (semicolon to end of line)

record
    name    ,a30    ; Inline comment
endrecord
```

## Line Continuation

```dbl
long_expression = value1 + value2 &
                + value3 + value4
```

## Exception Handling

```dbl
try
    ; risky code
catch (ex)
    ; handle error
finally
    ; cleanup
endtry
```

## Building Ziggy

```bash
cd ~/ziggy
zig build          # Build
zig build run      # Build and run
zig test           # Run tests
```

## Example Program

```dbl
; hello.dbl - Simple example

record
    name        ,a30
    age         ,d3
endrecord

proc
    name = "World"
    age = 42

    display(1, "Hello, ", name, "!")

    if (age .GE. 18) then
        display(1, "You are an adult.")
    else
        display(1, "You are a minor.")
    end
end
```

## Reference Documentation

The Ziggy repository contains comprehensive documentation in `~/ziggy/sde-docs/`:

| File | Topic |
|------|-------|
| `01-overview.md` | Platform overview |
| `02-language-basics.md` | Characters, identifiers |
| `03-data-types.md` | Type reference |
| `04-program-structure.md` | Records, groups, arrays |
| `05-statements.md` | Control flow, I/O |
| `06-oop.md` | Classes, namespaces |
| `07-file-types.md` | ISAM, sequential |
| `08-isam-deep-dive.md` | Detailed ISAM guide |
| `09-operators.md` | Operators, precedence |
| `10-ziggy-implementation-notes.md` | Implementation details |
