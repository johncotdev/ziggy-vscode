# Ziggy DBL - VS Code Extension

Language support for Synergy DBL (Ziggy implementation) in Visual Studio Code and Cursor.

## Features

### Syntax Highlighting
Full syntax highlighting for DBL code including:
- Keywords (control flow, loops, I/O, OOP)
- Operators (arithmetic, comparison, logical)
- Dot operators (.EQ., .AND., .OR., etc.)
- Strings (single and double quoted)
- Numbers (integers and decimals)
- Comments (semicolon style)
- Built-in functions (%TRIM, %SIZE, etc.)
- Data types (a, d, i1-i8, p, string)

### Run Button
Click the play button in the editor title bar to run your DBL program, or use:
- **Keyboard shortcut:** `Cmd+Shift+R` (Mac) / `Ctrl+Shift+R` (Windows/Linux)
- **Command palette:** `Ziggy: Run DBL File`
- **Right-click context menu:** `Run DBL File`

### Language Configuration
- Comment toggling with `;`
- Bracket matching and auto-closing
- Code folding for blocks (record/endrecord, if/end, class/endclass, etc.)
- Smart indentation

## Requirements

- [Ziggy DBL](https://github.com/johncotdev/ziggy) compiler/interpreter installed
- VS Code 1.85.0 or later

## Installation

### From Source (Development)
1. Clone this repository
2. Run `npm install` to install dependencies
3. Run `npm run compile` to build
4. Press F5 to launch Extension Development Host

### Install to VS Code/Cursor
```bash
# Build the extension package
npm install
npm run compile
npx vsce package

# Install the .vsix file
code --install-extension ziggy-dbl-0.1.0.vsix
```

## Extension Settings

| Setting | Default | Description |
|---------|---------|-------------|
| `ziggy.executablePath` | `"ziggy"` | Path to the ziggy executable |
| `ziggy.runInTerminal` | `true` | Run in terminal (true) or output panel (false) |

### Example Configuration
```json
{
  "ziggy.executablePath": "/usr/local/bin/ziggy",
  "ziggy.runInTerminal": true
}
```

Or for development with a local build:
```json
{
  "ziggy.executablePath": "./zig-out/bin/ziggy"
}
```

## File Extensions

This extension activates for files with:
- `.dbl`
- `.DBL`

## Example DBL Code

```dbl
; hello.dbl - A simple Ziggy DBL example

record
    name        ,a30
    age         ,d3
endrecord

proc
    name = "World"
    age = 42

    display(1, "Hello, ", name, "!")
    display(1, "You are ", age, " years old.")

    if (age .GE. 18) then
        display(1, "You are an adult.")
    else
        display(1, "You are a minor.")

end
```

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## License

MIT License - see [LICENSE](LICENSE) for details.

## Related Projects

- [Ziggy DBL](https://github.com/johncotdev/ziggy) - The Zig implementation of Synergy DBL
