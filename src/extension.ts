import * as vscode from 'vscode';
import * as path from 'path';

let outputChannel: vscode.OutputChannel;

export function activate(context: vscode.ExtensionContext) {
    console.log('Ziggy DBL extension is now active');

    outputChannel = vscode.window.createOutputChannel('Ziggy DBL');

    // Register the run command
    const runCommand = vscode.commands.registerCommand('ziggy.run', async () => {
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

        // Save the file before running
        if (document.isDirty) {
            await document.save();
        }

        const filePath = document.uri.fsPath;
        const config = vscode.workspace.getConfiguration('ziggy');
        const executablePath = config.get<string>('executablePath', 'ziggy');
        const runInTerminal = config.get<boolean>('runInTerminal', true);

        if (runInTerminal) {
            runInTerminalMode(executablePath, filePath);
        } else {
            runInOutputPanel(executablePath, filePath);
        }
    });

    context.subscriptions.push(runCommand);
    context.subscriptions.push(outputChannel);
}

function runInTerminalMode(executablePath: string, filePath: string): void {
    const terminalName = 'Ziggy DBL';

    // Find or create the terminal
    let terminal = vscode.window.terminals.find(t => t.name === terminalName);

    if (!terminal) {
        terminal = vscode.window.createTerminal(terminalName);
    }

    terminal.show();

    // Change to the file's directory and run
    const fileDir = path.dirname(filePath);
    const fileName = path.basename(filePath);

    terminal.sendText(`cd "${fileDir}" && "${executablePath}" "${fileName}"`);
}

function runInOutputPanel(executablePath: string, filePath: string): void {
    const { spawn } = require('child_process');

    outputChannel.clear();
    outputChannel.show();
    outputChannel.appendLine(`Running: ${executablePath} ${filePath}`);
    outputChannel.appendLine('---');

    const fileDir = path.dirname(filePath);

    const process = spawn(executablePath, [filePath], {
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

export function deactivate() {
    if (outputChannel) {
        outputChannel.dispose();
    }
}
