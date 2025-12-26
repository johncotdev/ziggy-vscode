# Development Guide

This guide covers how to develop and extend the Ziggy DBL VS Code extension.

## Prerequisites

- **Node.js** 18+
- **npm** 9+
- **VS Code** 1.85+
- **TypeScript** 5.3+

## Getting Started

### 1. Clone and Install

```bash
cd ~/ziggy-vscode
npm install
```

### 2. Open in VS Code

```bash
code .
```

### 3. Launch Development Host

Press `F5` to launch the Extension Development Host. This opens a new VS Code window with your extension loaded.

### 4. Test the Extension

1. Open or create a `.dbl` file
2. Verify syntax highlighting works
3. Press `Ctrl+Shift+R` (or `Cmd+Shift+R` on Mac) to run

## Build Commands

| Command | Description |
|---------|-------------|
| `npm run compile` | Compile TypeScript to JavaScript |
| `npm run watch` | Watch mode - recompile on changes |
| `npm run lint` | Run ESLint |

## Project Structure

```
src/
└── extension.ts       # Add new commands and features here

syntaxes/
└── dbl.tmLanguage.json   # Modify syntax highlighting here

language-configuration.json  # Modify editor behavior here

package.json              # Register new commands, settings, etc.
```

## Common Development Tasks

### Adding a New Command

1. **Register in package.json:**

```json
{
  "contributes": {
    "commands": [
      {
        "command": "ziggy.myCommand",
        "title": "My New Command",
        "category": "Ziggy"
      }
    ]
  }
}
```

2. **Implement in extension.ts:**

```typescript
export function activate(context: vscode.ExtensionContext) {
    let myCommand = vscode.commands.registerCommand('ziggy.myCommand', () => {
        vscode.window.showInformationMessage('My command executed!');
    });

    context.subscriptions.push(myCommand);
}
```

### Adding a Keybinding

In `package.json`:

```json
{
  "contributes": {
    "keybindings": [
      {
        "command": "ziggy.myCommand",
        "key": "ctrl+shift+m",
        "mac": "cmd+shift+m",
        "when": "editorLangId == dbl"
      }
    ]
  }
}
```

### Adding a Configuration Setting

1. **Define in package.json:**

```json
{
  "contributes": {
    "configuration": {
      "title": "Ziggy DBL",
      "properties": {
        "ziggy.mySetting": {
          "type": "string",
          "default": "defaultValue",
          "description": "Description of my setting"
        }
      }
    }
  }
}
```

2. **Read in extension.ts:**

```typescript
const config = vscode.workspace.getConfiguration('ziggy');
const myValue = config.get<string>('mySetting', 'defaultValue');
```

### Adding Syntax Highlighting

Edit `syntaxes/dbl.tmLanguage.json`.

**Add a new pattern:**

```json
{
  "repository": {
    "my-pattern": {
      "name": "keyword.my-category.dbl",
      "match": "(?i)\\b(myKeyword|anotherKeyword)\\b"
    }
  }
}
```

**Include in patterns:**

```json
{
  "patterns": [
    { "include": "#my-pattern" }
  ]
}
```

**Common scopes:**
- `keyword.control.dbl` - Control flow keywords
- `keyword.operator.dbl` - Operators
- `storage.type.dbl` - Type declarations
- `constant.numeric.dbl` - Numbers
- `string.quoted.dbl` - Strings
- `comment.line.dbl` - Comments
- `support.function.dbl` - Built-in functions
- `variable.other.dbl` - Variables

### Adding Code Snippets

1. **Create snippets file:**

Create `snippets/dbl.json`:

```json
{
  "Record Block": {
    "prefix": "record",
    "body": [
      "record ${1:name}",
      "    ${2:field}    ,${3:a10}",
      "endrecord",
      "$0"
    ],
    "description": "Create a record block"
  },

  "If Statement": {
    "prefix": "if",
    "body": [
      "if (${1:condition}) then",
      "    $0",
      "end"
    ],
    "description": "Create an if statement"
  },

  "Subroutine": {
    "prefix": "sub",
    "body": [
      "subroutine ${1:name}",
      "    ${2:param}    ,${3:a10}",
      "record",
      "    ${4:local}    ,${5:a10}",
      "proc",
      "    $0",
      "    xreturn",
      "endsubroutine"
    ],
    "description": "Create a subroutine"
  }
}
```

2. **Register in package.json:**

```json
{
  "contributes": {
    "snippets": [
      {
        "language": "dbl",
        "path": "./snippets/dbl.json"
      }
    ]
  }
}
```

### Modifying Language Configuration

Edit `language-configuration.json`.

**Add folding markers:**

```json
{
  "folding": {
    "markers": {
      "start": "(?i)\\b(myBlockStart)\\b",
      "end": "(?i)\\b(myBlockEnd)\\b"
    }
  }
}
```

**Add bracket pairs:**

```json
{
  "brackets": [
    ["{", "}"]
  ]
}
```

## Future Enhancements

### Implementing Language Server Protocol (LSP)

For features like autocomplete and go-to-definition, implement an LSP server.

**Option 1: TypeScript LSP**

```typescript
// server.ts
import {
    createConnection,
    TextDocuments,
    ProposedFeatures,
    CompletionItem,
} from 'vscode-languageserver/node';

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

connection.onCompletion((_textDocumentPosition): CompletionItem[] => {
    // Return completion items
    return [
        { label: 'record', kind: CompletionItemKind.Keyword },
        { label: 'endrecord', kind: CompletionItemKind.Keyword },
    ];
});
```

**Option 2: Use Ziggy Parser**

Create a bridge that:
1. Runs the Ziggy parser on the current file
2. Extracts symbol information
3. Returns it via LSP protocol

### Implementing Debugging

Create a Debug Adapter:

```typescript
// debugAdapter.ts
class ZiggyDebugSession extends DebugSession {
    protected launchRequest(response, args) {
        // Launch Ziggy runtime with debugging
    }

    protected setBreakPointsRequest(response, args) {
        // Set breakpoints
    }

    protected continueRequest(response, args) {
        // Continue execution
    }
}
```

Register in `package.json`:

```json
{
  "contributes": {
    "debuggers": [
      {
        "type": "ziggy",
        "label": "Ziggy DBL",
        "program": "./out/debugAdapter.js"
      }
    ]
  }
}
```

## Testing

### Manual Testing

1. Press `F5` to launch Extension Development Host
2. Open test `.dbl` files
3. Verify features work correctly

### Automated Testing

Create `src/test/extension.test.ts`:

```typescript
import * as assert from 'assert';
import * as vscode from 'vscode';

suite('Extension Test Suite', () => {
    test('Extension should activate', async () => {
        const ext = vscode.extensions.getExtension('johncotdev.ziggy-dbl');
        await ext?.activate();
        assert.ok(ext?.isActive);
    });

    test('Run command should be registered', async () => {
        const commands = await vscode.commands.getCommands();
        assert.ok(commands.includes('ziggy.run'));
    });
});
```

## Packaging and Publishing

### Create VSIX Package

```bash
npm install -g @vscode/vsce
vsce package
```

This creates `ziggy-dbl-0.1.0.vsix`.

### Install Locally

```bash
code --install-extension ziggy-dbl-0.1.0.vsix
```

### Publish to Marketplace

1. Create publisher account at https://marketplace.visualstudio.com/
2. Get Personal Access Token from Azure DevOps
3. Login: `vsce login johncotdev`
4. Publish: `vsce publish`

## Debugging the Extension

### View Extension Output

1. Open VS Code Command Palette
2. Run "Developer: Show Logs..."
3. Select "Extension Host"

### Debug Extension Code

1. Set breakpoints in `src/extension.ts`
2. Press `F5` to start debugging
3. Breakpoints will be hit when code executes

### View TextMate Grammar Scopes

1. Open a `.dbl` file
2. Run command "Developer: Inspect Editor Tokens and Scopes"
3. Click on any token to see its scope

## Best Practices

1. **Keep extension.ts focused** - Extract complex logic to separate modules
2. **Use disposables** - Add all subscriptions to `context.subscriptions`
3. **Handle errors gracefully** - Show user-friendly messages
4. **Test edge cases** - Empty files, syntax errors, large files
5. **Document changes** - Update README and CHANGELOG

## Troubleshooting

### Extension Not Activating

- Check activation events in package.json
- Look for errors in Extension Host log
- Verify file extension matches language definition

### Syntax Highlighting Not Working

- Validate JSON syntax in tmLanguage.json
- Check pattern regex is correct
- Use "Inspect Editor Tokens" to debug

### Command Not Found

- Verify command is registered in package.json
- Check command ID matches between package.json and extension.ts
- Reload VS Code window after changes

### Settings Not Reading

- Verify setting name matches between package.json and code
- Check setting type is correct
- Restart extension after changing configuration schema
