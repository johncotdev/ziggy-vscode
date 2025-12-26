import * as vscode from 'vscode';
import * as path from 'path';
import { spawn } from 'child_process';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel;
let disassemblyChannel: vscode.OutputChannel;

export function activate(context: vscode.ExtensionContext) {
    console.log('Zibol extension is now active');

    outputChannel = vscode.window.createOutputChannel('Zibol');
    disassemblyChannel = vscode.window.createOutputChannel('Zibol Disassembly');

    // Start the language server
    startLanguageServer(context);

    // Register commands
    const runCommand = vscode.commands.registerCommand('zibol.run', () => runZibolFile('interpreter'));
    const runBytecodeCommand = vscode.commands.registerCommand('zibol.runBytecode', () => runZibolFile('bytecode'));
    const compileCommand = vscode.commands.registerCommand('zibol.compile', compileToBytecode);
    const compileAndRunCommand = vscode.commands.registerCommand('zibol.compileAndRun', compileAndRun);
    const disassembleCommand = vscode.commands.registerCommand('zibol.disassemble', disassembleBytecode);
    const restartLspCommand = vscode.commands.registerCommand('zibol.restartLsp', () => restartLanguageServer(context));

    context.subscriptions.push(runCommand);
    context.subscriptions.push(runBytecodeCommand);
    context.subscriptions.push(compileCommand);
    context.subscriptions.push(compileAndRunCommand);
    context.subscriptions.push(disassembleCommand);
    context.subscriptions.push(restartLspCommand);
    context.subscriptions.push(outputChannel);
    context.subscriptions.push(disassemblyChannel);
}

function startLanguageServer(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('zibol');
    const lspPath = config.get<string>('lspPath', 'zibol-lsp');

    // Check if LSP server exists
    const serverOptions: ServerOptions = {
        run: {
            command: lspPath,
            transport: TransportKind.stdio
        },
        debug: {
            command: lspPath,
            transport: TransportKind.stdio
        }
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'zibol' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.zbl')
        },
        outputChannel: outputChannel
    };

    client = new LanguageClient(
        'zibol',
        'Zibol Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client - it will also launch the server
    client.start().catch((err) => {
        // Don't show error if server not found - it's optional
        console.log('Zibol LSP server not available:', err.message);
        outputChannel.appendLine(`Note: Language server not found at '${lspPath}'. Some features may be limited.`);
        outputChannel.appendLine('Set zibol.lspPath in settings to enable language features.');
    });
}

async function restartLanguageServer(context: vscode.ExtensionContext) {
    if (client) {
        await client.stop();
        client = undefined;
    }
    startLanguageServer(context);
    vscode.window.showInformationMessage('Zibol Language Server restarted');
}

async function runZibolFile(mode?: 'interpreter' | 'bytecode'): Promise<void> {
    const editor = vscode.window.activeTextEditor;

    if (!editor) {
        vscode.window.showErrorMessage('No active editor. Open a .zbl file first.');
        return;
    }

    const document = editor.document;

    if (document.languageId !== 'zibol') {
        vscode.window.showErrorMessage('Current file is not a Zibol file.');
        return;
    }

    // Save the file before running
    if (document.isDirty) {
        await document.save();
    }

    const filePath = document.uri.fsPath;
    const config = vscode.workspace.getConfiguration('zibol');
    const executablePath = config.get<string>('executablePath', 'ziggy');
    const runInTerminal = config.get<boolean>('runInTerminal', true);

    // Determine execution mode
    const executionMode = mode || config.get<string>('executionMode', 'interpreter');

    // Build command arguments
    const args: string[] = [];

    if (executionMode === 'bytecode') {
        // Running in bytecode mode - check if compiled version exists
        const fileDir = path.dirname(filePath);
        const fileName = path.basename(filePath, '.zbl');
        const outputDir = config.get<string>('bytecodeOutputDir', '');
        const binDir = outputDir || path.join(fileDir, 'bin');
        const zboFile = path.join(binDir, `${fileName}.zbo`);

        // Check if the compiled file exists
        try {
            await vscode.workspace.fs.stat(vscode.Uri.file(zboFile));
            args.push('run', zboFile);
        } catch {
            vscode.window.showWarningMessage(
                `Compiled bytecode not found at ${zboFile}. Compile first using Ctrl+Shift+B or run with interpreter.`
            );
            return;
        }
    } else {
        // Running with interpreter - just pass the file
        args.push(filePath);
    }

    if (runInTerminal) {
        runInTerminalMode(executablePath, args, filePath);
    } else {
        runInOutputPanel(executablePath, args, filePath);
    }
}

function runInTerminalMode(executablePath: string, args: string[], filePath: string): void {
    const terminalName = 'Zibol';

    // Find or create the terminal
    let terminal = vscode.window.terminals.find(t => t.name === terminalName);

    if (!terminal) {
        terminal = vscode.window.createTerminal(terminalName);
    }

    terminal.show();

    // Change to the file's directory and run
    const fileDir = path.dirname(filePath);
    const command = `"${executablePath}" ${args.map(a => `"${a}"`).join(' ')}`;

    terminal.sendText(`cd "${fileDir}" && ${command}`);
}

function runInOutputPanel(executablePath: string, args: string[], filePath: string): void {
    outputChannel.clear();
    outputChannel.show();
    outputChannel.appendLine(`Running: ${executablePath} ${args.join(' ')}`);
    outputChannel.appendLine('---');

    const fileDir = path.dirname(filePath);

    const process = spawn(executablePath, args, {
        cwd: fileDir,
        shell: true
    });

    process.stdout.on('data', (data: Buffer) => {
        outputChannel.append(data.toString());
    });

    process.stderr.on('data', (data: Buffer) => {
        outputChannel.append(data.toString());
    });

    process.on('close', (code: number) => {
        outputChannel.appendLine('---');
        if (code === 0) {
            outputChannel.appendLine('Program completed successfully.');
        } else {
            outputChannel.appendLine(`Program exited with code ${code}.`);
        }
    });

    process.on('error', (err: Error) => {
        outputChannel.appendLine(`Error: ${err.message}`);
        vscode.window.showErrorMessage(`Failed to run Ziggy: ${err.message}. Check the zibol.executablePath setting.`);
    });
}

async function compileToBytecode(): Promise<void> {
    const editor = vscode.window.activeTextEditor;

    if (!editor) {
        vscode.window.showErrorMessage('No active editor. Open a .zbl file first.');
        return;
    }

    const document = editor.document;

    if (document.languageId !== 'zibol') {
        vscode.window.showErrorMessage('Current file is not a Zibol file.');
        return;
    }

    // Save the file before compiling
    if (document.isDirty) {
        await document.save();
    }

    const filePath = document.uri.fsPath;
    const config = vscode.workspace.getConfiguration('zibol');
    const executablePath = config.get<string>('executablePath', 'ziggy');
    const outputDir = config.get<string>('bytecodeOutputDir', '');
    const showDisassembly = config.get<boolean>('showDisassemblyOnCompile', false);

    const fileDir = path.dirname(filePath);
    const fileName = path.basename(filePath, '.zbl');
    // Default to bin/ subfolder if no output directory specified
    const targetDir = outputDir || path.join(fileDir, 'bin');
    const outputFile = path.join(targetDir, `${fileName}.zbo`);

    outputChannel.clear();
    outputChannel.show();
    outputChannel.appendLine(`Compiling: ${filePath}`);
    outputChannel.appendLine(`Output: ${outputFile}`);
    outputChannel.appendLine('---');

    const args = ['compile', filePath, '-o', outputFile];

    const process = spawn(executablePath, args, {
        cwd: fileDir,
        shell: true
    });

    let hasError = false;

    process.stdout.on('data', (data: Buffer) => {
        outputChannel.append(data.toString());
    });

    process.stderr.on('data', (data: Buffer) => {
        outputChannel.append(data.toString());
        hasError = true;
    });

    process.on('close', async (code: number) => {
        outputChannel.appendLine('---');
        if (code === 0) {
            outputChannel.appendLine(`Compilation successful: ${outputFile}`);
            vscode.window.showInformationMessage(`Compiled to ${path.basename(outputFile)}`);

            if (showDisassembly) {
                await showDisassemblyForFile(outputFile);
            }
        } else {
            outputChannel.appendLine(`Compilation failed with code ${code}.`);
            if (!hasError) {
                vscode.window.showErrorMessage('Compilation failed.');
            }
        }
    });

    process.on('error', (err: Error) => {
        outputChannel.appendLine(`Error: ${err.message}`);
        vscode.window.showErrorMessage(`Failed to compile: ${err.message}. Check the zibol.executablePath setting.`);
    });
}

async function compileAndRun(): Promise<void> {
    const editor = vscode.window.activeTextEditor;

    if (!editor) {
        vscode.window.showErrorMessage('No active editor. Open a .zbl file first.');
        return;
    }

    const document = editor.document;

    if (document.languageId !== 'zibol') {
        vscode.window.showErrorMessage('Current file is not a Zibol file.');
        return;
    }

    // Save the file before compiling
    if (document.isDirty) {
        await document.save();
    }

    const filePath = document.uri.fsPath;
    const config = vscode.workspace.getConfiguration('zibol');
    const executablePath = config.get<string>('executablePath', 'ziggy');
    const outputDir = config.get<string>('bytecodeOutputDir', '');
    const runInTerminal = config.get<boolean>('runInTerminal', true);

    const fileDir = path.dirname(filePath);
    const fileName = path.basename(filePath, '.zbl');
    const targetDir = outputDir || path.join(fileDir, 'bin');
    const outputFile = path.join(targetDir, `${fileName}.zbo`);

    outputChannel.clear();
    outputChannel.show();
    outputChannel.appendLine(`Compiling: ${filePath}`);
    outputChannel.appendLine(`Output: ${outputFile}`);
    outputChannel.appendLine('---');

    const compileArgs = ['compile', filePath, '-o', outputFile];

    const compileProcess = spawn(executablePath, compileArgs, {
        cwd: fileDir,
        shell: true
    });

    let compileError = false;

    compileProcess.stdout.on('data', (data: Buffer) => {
        outputChannel.append(data.toString());
    });

    compileProcess.stderr.on('data', (data: Buffer) => {
        outputChannel.append(data.toString());
        compileError = true;
    });

    compileProcess.on('close', async (code: number) => {
        if (code === 0) {
            outputChannel.appendLine('Compilation successful. Running...');
            outputChannel.appendLine('---');

            // Now run the compiled file
            const runArgs = ['run', outputFile];

            if (runInTerminal) {
                runInTerminalMode(executablePath, runArgs, filePath);
            } else {
                runInOutputPanel(executablePath, runArgs, filePath);
            }
        } else {
            outputChannel.appendLine(`Compilation failed with code ${code}.`);
            if (!compileError) {
                vscode.window.showErrorMessage('Compilation failed.');
            }
        }
    });

    compileProcess.on('error', (err: Error) => {
        outputChannel.appendLine(`Error: ${err.message}`);
        vscode.window.showErrorMessage(`Failed to compile: ${err.message}. Check the zibol.executablePath setting.`);
    });
}

async function disassembleBytecode(): Promise<void> {
    const editor = vscode.window.activeTextEditor;

    if (!editor) {
        vscode.window.showErrorMessage('No active editor. Open a .zbl file first.');
        return;
    }

    const document = editor.document;

    if (document.languageId !== 'zibol') {
        vscode.window.showErrorMessage('Current file is not a Zibol file.');
        return;
    }

    // Save the file before disassembling
    if (document.isDirty) {
        await document.save();
    }

    const filePath = document.uri.fsPath;
    await showDisassemblyForFile(filePath);
}

async function showDisassemblyForFile(filePath: string): Promise<void> {
    const config = vscode.workspace.getConfiguration('zibol');
    const executablePath = config.get<string>('executablePath', 'ziggy');
    const fileDir = path.dirname(filePath);

    disassemblyChannel.clear();
    disassemblyChannel.show();
    disassemblyChannel.appendLine(`; Disassembly of: ${filePath}`);
    disassemblyChannel.appendLine('; ---');

    const args = ['disasm', filePath];

    const process = spawn(executablePath, args, {
        cwd: fileDir,
        shell: true
    });

    let output = '';

    process.stdout.on('data', (data: Buffer) => {
        output += data.toString();
    });

    process.stderr.on('data', (data: Buffer) => {
        disassemblyChannel.append(data.toString());
    });

    process.on('close', async (code: number) => {
        if (code === 0 && output) {
            // Create a new untitled document with the disassembly
            const doc = await vscode.workspace.openTextDocument({
                content: output,
                language: 'zibol-disasm'
            });
            await vscode.window.showTextDocument(doc, { preview: true, viewColumn: vscode.ViewColumn.Beside });
        } else if (code !== 0) {
            disassemblyChannel.appendLine('; ---');
            disassemblyChannel.appendLine(`; Disassembly failed with code ${code}.`);
        }
    });

    process.on('error', (err: Error) => {
        disassemblyChannel.appendLine(`; Error: ${err.message}`);
        vscode.window.showErrorMessage(`Failed to disassemble: ${err.message}. Check the zibol.executablePath setting.`);
    });
}

export function deactivate(): Thenable<void> | undefined {
    if (client) {
        return client.stop();
    }
    if (outputChannel) {
        outputChannel.dispose();
    }
    if (disassemblyChannel) {
        disassemblyChannel.dispose();
    }
    return undefined;
}
