import * as vscode from 'vscode';
import * as path from 'path';
import { spawn } from 'child_process';

let outputChannel: vscode.OutputChannel;
let disassemblyChannel: vscode.OutputChannel;

export function activate(context: vscode.ExtensionContext) {
    console.log('Ziggy DBL extension is now active');

    outputChannel = vscode.window.createOutputChannel('Ziggy DBL');
    disassemblyChannel = vscode.window.createOutputChannel('Ziggy Disassembly');

    // Register commands
    const runCommand = vscode.commands.registerCommand('ziggy.run', () => runDblFile('interpreter'));
    const runBytecodeCommand = vscode.commands.registerCommand('ziggy.runBytecode', () => runDblFile('bytecode'));
    const compileCommand = vscode.commands.registerCommand('ziggy.compile', compileToBytecode);
    const compileAndRunCommand = vscode.commands.registerCommand('ziggy.compileAndRun', compileAndRun);
    const disassembleCommand = vscode.commands.registerCommand('ziggy.disassemble', disassembleBytecode);

    context.subscriptions.push(runCommand);
    context.subscriptions.push(runBytecodeCommand);
    context.subscriptions.push(compileCommand);
    context.subscriptions.push(compileAndRunCommand);
    context.subscriptions.push(disassembleCommand);
    context.subscriptions.push(outputChannel);
    context.subscriptions.push(disassemblyChannel);
}

async function runDblFile(mode?: 'interpreter' | 'bytecode'): Promise<void> {
    const editor = vscode.window.activeTextEditor;

    if (!editor) {
        vscode.window.showErrorMessage('No active editor. Open a .dbl file first.');
        return;
    }

    const document = editor.document;
    const langId = document.languageId;

    if (langId !== 'dbl' && langId !== 'zbc') {
        vscode.window.showErrorMessage('Current file is not a DBL or bytecode file.');
        return;
    }

    // Save the file before running
    if (document.isDirty) {
        await document.save();
    }

    const filePath = document.uri.fsPath;
    const config = vscode.workspace.getConfiguration('ziggy');
    const executablePath = config.get<string>('executablePath', 'ziggy');
    const runInTerminal = config.get<boolean>('runInTerminal', true);

    // Determine execution mode
    const executionMode = mode || config.get<string>('executionMode', 'interpreter');

    // Build command arguments
    const args: string[] = [];

    // Determine how to run based on file type and mode
    if (langId === 'zbc') {
        // Running a bytecode file - use 'run' command
        args.push('run', filePath);
    } else if (executionMode === 'bytecode') {
        // Running DBL file in bytecode mode - check if compiled version exists in bin/
        const fileDir = path.dirname(filePath);
        const fileName = path.basename(filePath, '.dbl');
        const config = vscode.workspace.getConfiguration('ziggy');
        const outputDir = config.get<string>('bytecodeOutputDir', '');
        const binDir = outputDir || path.join(fileDir, 'bin');
        const zbcFile = path.join(binDir, `${fileName}.zbc`);

        // Check if the compiled file exists
        try {
            await vscode.workspace.fs.stat(vscode.Uri.file(zbcFile));
            args.push('run', zbcFile);
        } catch {
            vscode.window.showWarningMessage(
                `Compiled bytecode not found at ${zbcFile}. Compile first using Ctrl+Shift+B or run with interpreter.`
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
    const terminalName = 'Ziggy DBL';

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
        vscode.window.showErrorMessage(`Failed to run Ziggy: ${err.message}. Check the ziggy.executablePath setting.`);
    });
}

async function compileToBytecode(): Promise<void> {
    const editor = vscode.window.activeTextEditor;

    if (!editor) {
        vscode.window.showErrorMessage('No active editor. Open a .dbl file first.');
        return;
    }

    const document = editor.document;

    if (document.languageId !== 'dbl') {
        vscode.window.showErrorMessage('Current file is not a DBL file.');
        return;
    }

    // Save the file before compiling
    if (document.isDirty) {
        await document.save();
    }

    const filePath = document.uri.fsPath;
    const config = vscode.workspace.getConfiguration('ziggy');
    const executablePath = config.get<string>('executablePath', 'ziggy');
    const outputDir = config.get<string>('bytecodeOutputDir', '');
    const showDisassembly = config.get<boolean>('showDisassemblyOnCompile', false);

    const fileDir = path.dirname(filePath);
    const fileName = path.basename(filePath, '.dbl');
    // Default to bin/ subfolder if no output directory specified
    const targetDir = outputDir || path.join(fileDir, 'bin');
    const outputFile = path.join(targetDir, `${fileName}.zbc`);

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
                vscode.window.showErrorMessage('Compilation failed. The compile command may not be available in this version of Ziggy.');
            }
        }
    });

    process.on('error', (err: Error) => {
        outputChannel.appendLine(`Error: ${err.message}`);
        vscode.window.showErrorMessage(`Failed to compile: ${err.message}. Check the ziggy.executablePath setting.`);
    });
}

async function compileAndRun(): Promise<void> {
    const editor = vscode.window.activeTextEditor;

    if (!editor) {
        vscode.window.showErrorMessage('No active editor. Open a .dbl file first.');
        return;
    }

    const document = editor.document;

    if (document.languageId !== 'dbl') {
        vscode.window.showErrorMessage('Current file is not a DBL file.');
        return;
    }

    // Save the file before compiling
    if (document.isDirty) {
        await document.save();
    }

    const filePath = document.uri.fsPath;
    const config = vscode.workspace.getConfiguration('ziggy');
    const executablePath = config.get<string>('executablePath', 'ziggy');
    const outputDir = config.get<string>('bytecodeOutputDir', '');
    const runInTerminal = config.get<boolean>('runInTerminal', true);

    const fileDir = path.dirname(filePath);
    const fileName = path.basename(filePath, '.dbl');
    const targetDir = outputDir || path.join(fileDir, 'bin');
    const outputFile = path.join(targetDir, `${fileName}.zbc`);

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
        vscode.window.showErrorMessage(`Failed to compile: ${err.message}. Check the ziggy.executablePath setting.`);
    });
}

async function disassembleBytecode(): Promise<void> {
    const editor = vscode.window.activeTextEditor;

    if (!editor) {
        vscode.window.showErrorMessage('No active editor. Open a .dbl or .zbc file first.');
        return;
    }

    const document = editor.document;
    const langId = document.languageId;

    if (langId !== 'dbl' && langId !== 'zbc') {
        vscode.window.showErrorMessage('Current file is not a DBL or bytecode file.');
        return;
    }

    // Save the file before disassembling
    if (document.isDirty) {
        await document.save();
    }

    const filePath = document.uri.fsPath;

    // If it's a .dbl file, we need to compile first (or use disasm on source)
    // If it's a .zbc file, we can disassemble directly
    await showDisassemblyForFile(filePath);
}

async function showDisassemblyForFile(filePath: string): Promise<void> {
    const config = vscode.workspace.getConfiguration('ziggy');
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
                language: 'zbc-disasm'
            });
            await vscode.window.showTextDocument(doc, { preview: true, viewColumn: vscode.ViewColumn.Beside });
        } else if (code !== 0) {
            disassemblyChannel.appendLine('; ---');
            disassemblyChannel.appendLine(`; Disassembly failed with code ${code}.`);
            disassemblyChannel.appendLine('; The disasm command may not be available in this version of Ziggy.');

            // Show a more user-friendly message
            vscode.window.showWarningMessage(
                'Disassembly command not available. This feature requires Ziggy CLI with bytecode support.',
                'View Tech Docs'
            ).then(async selection => {
                if (selection === 'View Tech Docs') {
                    // Open the tech-docs if available
                    const techDocsPath = path.join(path.dirname(filePath), '..', 'ziggy', 'tech-docs', 'bytecode.md');
                    try {
                        const doc = await vscode.workspace.openTextDocument(techDocsPath);
                        await vscode.window.showTextDocument(doc);
                    } catch {
                        vscode.window.showInformationMessage('Tech docs not found. See ~/ziggy/tech-docs/ for bytecode documentation.');
                    }
                }
            });
        }
    });

    process.on('error', (err: Error) => {
        disassemblyChannel.appendLine(`; Error: ${err.message}`);
        vscode.window.showErrorMessage(`Failed to disassemble: ${err.message}. Check the ziggy.executablePath setting.`);
    });
}

export function deactivate() {
    if (outputChannel) {
        outputChannel.dispose();
    }
    if (disassemblyChannel) {
        disassemblyChannel.dispose();
    }
}
