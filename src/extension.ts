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
let devServerTerminal: vscode.Terminal | undefined;
let workspaceStatusBarItem: vscode.StatusBarItem;
let devServerStatusBarItem: vscode.StatusBarItem;

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

    // Framework commands
    const initCommand = vscode.commands.registerCommand('zibol.init', initWorkspace);
    const newCommand = vscode.commands.registerCommand('zibol.new', newProject);
    const buildCommand = vscode.commands.registerCommand('zibol.buildWorkspace', buildWorkspace);
    const devCommand = vscode.commands.registerCommand('zibol.dev', startDevServer);
    const stopDevCommand = vscode.commands.registerCommand('zibol.stopDev', stopDevServer);
    const runAppCommand = vscode.commands.registerCommand('zibol.runApp', runApp);

    context.subscriptions.push(runCommand);
    context.subscriptions.push(runBytecodeCommand);
    context.subscriptions.push(compileCommand);
    context.subscriptions.push(compileAndRunCommand);
    context.subscriptions.push(disassembleCommand);
    context.subscriptions.push(restartLspCommand);
    context.subscriptions.push(initCommand);
    context.subscriptions.push(newCommand);
    context.subscriptions.push(buildCommand);
    context.subscriptions.push(devCommand);
    context.subscriptions.push(stopDevCommand);
    context.subscriptions.push(runAppCommand);
    context.subscriptions.push(outputChannel);
    context.subscriptions.push(disassemblyChannel);

    // Create status bar items
    createStatusBarItems(context);

    // Check for workspace on activation
    checkForWorkspace();
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

// ==================== Framework Commands ====================

function createStatusBarItems(context: vscode.ExtensionContext): void {
    // Workspace status bar item
    workspaceStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
    workspaceStatusBarItem.command = 'zibol.init';
    context.subscriptions.push(workspaceStatusBarItem);

    // Dev server status bar item
    devServerStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 99);
    devServerStatusBarItem.command = 'zibol.dev';
    context.subscriptions.push(devServerStatusBarItem);
}

async function checkForWorkspace(): Promise<void> {
    const workspaceFolders = vscode.workspace.workspaceFolders;
    if (!workspaceFolders || workspaceFolders.length === 0) {
        workspaceStatusBarItem.hide();
        devServerStatusBarItem.hide();
        return;
    }

    const rootPath = workspaceFolders[0].uri.fsPath;
    const zibolJsonPath = path.join(rootPath, 'zibol.json');

    try {
        await vscode.workspace.fs.stat(vscode.Uri.file(zibolJsonPath));
        // zibol.json exists - read it
        const content = await vscode.workspace.fs.readFile(vscode.Uri.file(zibolJsonPath));
        const config = JSON.parse(content.toString());

        workspaceStatusBarItem.text = `$(folder-library) ${config.name || 'Zibol Workspace'}`;
        workspaceStatusBarItem.tooltip = 'Zibol Workspace';
        workspaceStatusBarItem.show();

        devServerStatusBarItem.text = '$(play) Dev Server';
        devServerStatusBarItem.tooltip = 'Start Zibol Dev Server';
        devServerStatusBarItem.show();
    } catch {
        // No zibol.json found
        workspaceStatusBarItem.text = '$(add) Initialize Zibol';
        workspaceStatusBarItem.tooltip = 'Initialize Zibol Workspace';
        workspaceStatusBarItem.show();
        devServerStatusBarItem.hide();
    }
}

async function initWorkspace(): Promise<void> {
    const workspaceFolders = vscode.workspace.workspaceFolders;

    if (!workspaceFolders || workspaceFolders.length === 0) {
        // Ask for folder name to create
        const folderName = await vscode.window.showInputBox({
            prompt: 'Enter workspace name',
            placeHolder: 'my-company',
            validateInput: (value) => {
                if (!value || value.trim().length === 0) {
                    return 'Workspace name is required';
                }
                if (!/^[a-z0-9-_]+$/i.test(value)) {
                    return 'Workspace name can only contain letters, numbers, hyphens, and underscores';
                }
                return null;
            }
        });

        if (!folderName) {
            return;
        }

        // Use terminal to run ziggy init
        const terminal = vscode.window.createTerminal('Zibol Init');
        terminal.show();
        const config = vscode.workspace.getConfiguration('zibol');
        const executablePath = config.get<string>('executablePath', 'ziggy');
        terminal.sendText(`${executablePath} init ${folderName}`);
        return;
    }

    const rootPath = workspaceFolders[0].uri.fsPath;
    const zibolJsonPath = path.join(rootPath, 'zibol.json');

    try {
        await vscode.workspace.fs.stat(vscode.Uri.file(zibolJsonPath));
        vscode.window.showInformationMessage('Zibol workspace already initialized.');
        return;
    } catch {
        // zibol.json doesn't exist, initialize
    }

    const workspaceName = await vscode.window.showInputBox({
        prompt: 'Enter workspace name',
        placeHolder: path.basename(rootPath),
        value: path.basename(rootPath),
        validateInput: (value) => {
            if (!value || value.trim().length === 0) {
                return 'Workspace name is required';
            }
            return null;
        }
    });

    if (!workspaceName) {
        return;
    }

    const config = vscode.workspace.getConfiguration('zibol');
    const executablePath = config.get<string>('executablePath', 'ziggy');

    outputChannel.clear();
    outputChannel.show();
    outputChannel.appendLine(`Initializing Zibol workspace: ${workspaceName}`);
    outputChannel.appendLine('---');

    const process = spawn(executablePath, ['init', workspaceName], {
        cwd: rootPath,
        shell: true
    });

    process.stdout.on('data', (data: Buffer) => {
        outputChannel.append(data.toString());
    });

    process.stderr.on('data', (data: Buffer) => {
        outputChannel.append(data.toString());
    });

    process.on('close', async (code: number) => {
        if (code === 0) {
            outputChannel.appendLine('---');
            outputChannel.appendLine('Workspace initialized successfully!');
            vscode.window.showInformationMessage(`Zibol workspace '${workspaceName}' initialized!`);
            await checkForWorkspace();
        } else {
            outputChannel.appendLine('---');
            outputChannel.appendLine(`Initialization failed with code ${code}.`);
            vscode.window.showErrorMessage('Failed to initialize workspace.');
        }
    });

    process.on('error', (err: Error) => {
        outputChannel.appendLine(`Error: ${err.message}`);
        vscode.window.showErrorMessage(`Failed to initialize: ${err.message}`);
    });
}

async function newProject(): Promise<void> {
    const workspaceFolders = vscode.workspace.workspaceFolders;

    if (!workspaceFolders || workspaceFolders.length === 0) {
        vscode.window.showErrorMessage('Open a folder first, then initialize a Zibol workspace.');
        return;
    }

    const rootPath = workspaceFolders[0].uri.fsPath;

    // Check if workspace is initialized
    const zibolJsonPath = path.join(rootPath, 'zibol.json');
    try {
        await vscode.workspace.fs.stat(vscode.Uri.file(zibolJsonPath));
    } catch {
        const init = await vscode.window.showWarningMessage(
            'Zibol workspace not initialized. Initialize first?',
            'Initialize',
            'Cancel'
        );
        if (init === 'Initialize') {
            await initWorkspace();
        }
        return;
    }

    // Ask for project type
    const projectType = await vscode.window.showQuickPick(
        [
            { label: 'App', description: 'Create a new application', value: 'app' },
            { label: 'Library', description: 'Create a shared library package', value: 'library' }
        ],
        { placeHolder: 'Select project type' }
    );

    if (!projectType) {
        return;
    }

    // Ask for project name
    const projectName = await vscode.window.showInputBox({
        prompt: `Enter ${projectType.value} name`,
        placeHolder: projectType.value === 'app' ? 'inventory' : '@company/common',
        validateInput: (value) => {
            if (!value || value.trim().length === 0) {
                return 'Project name is required';
            }
            return null;
        }
    });

    if (!projectName) {
        return;
    }

    const config = vscode.workspace.getConfiguration('zibol');
    const executablePath = config.get<string>('executablePath', 'ziggy');

    outputChannel.clear();
    outputChannel.show();
    outputChannel.appendLine(`Creating new ${projectType.value}: ${projectName}`);
    outputChannel.appendLine('---');

    const args = ['new', projectName, '--template', projectType.value];

    const process = spawn(executablePath, args, {
        cwd: rootPath,
        shell: true
    });

    process.stdout.on('data', (data: Buffer) => {
        outputChannel.append(data.toString());
    });

    process.stderr.on('data', (data: Buffer) => {
        outputChannel.append(data.toString());
    });

    process.on('close', async (code: number) => {
        if (code === 0) {
            outputChannel.appendLine('---');
            outputChannel.appendLine(`${projectType.label} '${projectName}' created successfully!`);
            vscode.window.showInformationMessage(`Created ${projectType.value} '${projectName}'`);

            // Open the new project folder
            const projectPath = projectType.value === 'app'
                ? path.join(rootPath, 'apps', projectName)
                : path.join(rootPath, 'packages', projectName.replace('@', '').replace('/', '-'));

            try {
                await vscode.workspace.fs.stat(vscode.Uri.file(projectPath));
                // Reveal in explorer
                vscode.commands.executeCommand('revealInExplorer', vscode.Uri.file(projectPath));
            } catch {
                // Project folder doesn't exist where expected
            }
        } else {
            outputChannel.appendLine('---');
            outputChannel.appendLine(`Creation failed with code ${code}.`);
            vscode.window.showErrorMessage('Failed to create project.');
        }
    });

    process.on('error', (err: Error) => {
        outputChannel.appendLine(`Error: ${err.message}`);
        vscode.window.showErrorMessage(`Failed to create project: ${err.message}`);
    });
}

async function buildWorkspace(): Promise<void> {
    const workspaceFolders = vscode.workspace.workspaceFolders;

    if (!workspaceFolders || workspaceFolders.length === 0) {
        vscode.window.showErrorMessage('No workspace folder open.');
        return;
    }

    const rootPath = workspaceFolders[0].uri.fsPath;

    // Check if workspace is initialized
    const zibolJsonPath = path.join(rootPath, 'zibol.json');
    try {
        await vscode.workspace.fs.stat(vscode.Uri.file(zibolJsonPath));
    } catch {
        vscode.window.showErrorMessage('Not a Zibol workspace. Run "Zibol: Initialize Workspace" first.');
        return;
    }

    const config = vscode.workspace.getConfiguration('zibol');
    const executablePath = config.get<string>('executablePath', 'ziggy');

    outputChannel.clear();
    outputChannel.show();
    outputChannel.appendLine('Building Zibol workspace...');
    outputChannel.appendLine('---');

    const process = spawn(executablePath, ['build'], {
        cwd: rootPath,
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
            outputChannel.appendLine('Build completed successfully!');
            vscode.window.showInformationMessage('Zibol workspace built successfully!');
        } else {
            outputChannel.appendLine(`Build failed with code ${code}.`);
            vscode.window.showErrorMessage('Build failed. Check the output for details.');
        }
    });

    process.on('error', (err: Error) => {
        outputChannel.appendLine(`Error: ${err.message}`);
        vscode.window.showErrorMessage(`Build failed: ${err.message}`);
    });
}

async function startDevServer(): Promise<void> {
    const workspaceFolders = vscode.workspace.workspaceFolders;

    if (!workspaceFolders || workspaceFolders.length === 0) {
        vscode.window.showErrorMessage('No workspace folder open.');
        return;
    }

    const rootPath = workspaceFolders[0].uri.fsPath;

    // Check if workspace is initialized
    const zibolJsonPath = path.join(rootPath, 'zibol.json');
    try {
        await vscode.workspace.fs.stat(vscode.Uri.file(zibolJsonPath));
    } catch {
        vscode.window.showErrorMessage('Not a Zibol workspace. Run "Zibol: Initialize Workspace" first.');
        return;
    }

    // Check if dev server is already running
    if (devServerTerminal) {
        const restart = await vscode.window.showWarningMessage(
            'Dev server is already running. Restart?',
            'Restart',
            'Cancel'
        );
        if (restart === 'Restart') {
            devServerTerminal.dispose();
            devServerTerminal = undefined;
        } else {
            devServerTerminal.show();
            return;
        }
    }

    const config = vscode.workspace.getConfiguration('zibol');
    const executablePath = config.get<string>('executablePath', 'ziggy');

    // Create terminal for dev server
    devServerTerminal = vscode.window.createTerminal({
        name: 'Zibol Dev Server',
        cwd: rootPath
    });

    devServerTerminal.show();
    devServerTerminal.sendText(`${executablePath} dev`);

    // Update status bar
    devServerStatusBarItem.text = '$(debug-stop) Stop Dev Server';
    devServerStatusBarItem.tooltip = 'Stop Zibol Dev Server';
    devServerStatusBarItem.command = 'zibol.stopDev';

    // Watch for terminal close
    const disposable = vscode.window.onDidCloseTerminal((terminal) => {
        if (terminal === devServerTerminal) {
            devServerTerminal = undefined;
            devServerStatusBarItem.text = '$(play) Dev Server';
            devServerStatusBarItem.tooltip = 'Start Zibol Dev Server';
            devServerStatusBarItem.command = 'zibol.dev';
            disposable.dispose();
        }
    });
}

async function stopDevServer(): Promise<void> {
    if (devServerTerminal) {
        devServerTerminal.dispose();
        devServerTerminal = undefined;

        devServerStatusBarItem.text = '$(play) Dev Server';
        devServerStatusBarItem.tooltip = 'Start Zibol Dev Server';
        devServerStatusBarItem.command = 'zibol.dev';

        vscode.window.showInformationMessage('Dev server stopped.');
    } else {
        vscode.window.showInformationMessage('Dev server is not running.');
    }
}

async function runApp(): Promise<void> {
    const workspaceFolders = vscode.workspace.workspaceFolders;

    if (!workspaceFolders || workspaceFolders.length === 0) {
        vscode.window.showErrorMessage('No workspace folder open.');
        return;
    }

    const rootPath = workspaceFolders[0].uri.fsPath;

    // Check if workspace is initialized
    const zibolJsonPath = path.join(rootPath, 'zibol.json');
    try {
        await vscode.workspace.fs.stat(vscode.Uri.file(zibolJsonPath));
    } catch {
        vscode.window.showErrorMessage('Not a Zibol workspace. Run "Zibol: Initialize Workspace" first.');
        return;
    }

    // Find available apps in apps/ directory
    const appsDir = path.join(rootPath, 'apps');
    let apps: string[] = [];

    try {
        const entries = await vscode.workspace.fs.readDirectory(vscode.Uri.file(appsDir));
        apps = entries
            .filter(([, type]) => type === vscode.FileType.Directory)
            .map(([name]) => name);
    } catch {
        vscode.window.showErrorMessage('No apps directory found. Create an app first with "Zibol: New App/Package".');
        return;
    }

    if (apps.length === 0) {
        vscode.window.showErrorMessage('No apps found. Create an app first.');
        return;
    }

    // If only one app, run it directly. Otherwise, ask user to select.
    let appName: string | undefined;

    if (apps.length === 1) {
        appName = apps[0];
    } else {
        const selected = await vscode.window.showQuickPick(
            apps.map(name => ({ label: name, description: `apps/${name}` })),
            { placeHolder: 'Select app to run' }
        );
        appName = selected?.label;
    }

    if (!appName) {
        return;
    }

    const config = vscode.workspace.getConfiguration('zibol');
    const executablePath = config.get<string>('executablePath', 'ziggy');
    const runInTerminal = config.get<boolean>('runInTerminal', true);

    // Run the app using ziggy run <appname>
    const args = ['run', appName];

    if (runInTerminal) {
        const terminalName = 'Zibol';
        let terminal = vscode.window.terminals.find(t => t.name === terminalName);
        if (!terminal) {
            terminal = vscode.window.createTerminal(terminalName);
        }
        terminal.show();
        terminal.sendText(`cd "${rootPath}" && "${executablePath}" ${args.join(' ')}`);
    } else {
        outputChannel.clear();
        outputChannel.show();
        outputChannel.appendLine(`Running app: ${appName}`);
        outputChannel.appendLine('---');

        const process = spawn(executablePath, args, {
            cwd: rootPath,
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
                outputChannel.appendLine('App completed successfully.');
            } else {
                outputChannel.appendLine(`App exited with code ${code}.`);
            }
        });

        process.on('error', (err: Error) => {
            outputChannel.appendLine(`Error: ${err.message}`);
            vscode.window.showErrorMessage(`Failed to run app: ${err.message}`);
        });
    }
}

export function deactivate(): Thenable<void> | undefined {
    if (devServerTerminal) {
        devServerTerminal.dispose();
    }
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
