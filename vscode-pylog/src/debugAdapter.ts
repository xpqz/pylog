import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { execSync } from 'child_process';

/**
 * Factory for creating PyLog debug adapter descriptors.
 * This is responsible for spawning the Python DAP server process.
 */
export class PyLogDebugAdapterDescriptorFactory
    implements vscode.DebugAdapterDescriptorFactory, vscode.Disposable {

    private outputChannel: vscode.OutputChannel;

    constructor() {
        this.outputChannel = vscode.window.createOutputChannel('PyLog Debugger');
    }

    /**
     * Creates a debug adapter descriptor for a debug session.
     *
     * @param session - The debug session
     * @param _executable - Optional executable (not used)
     * @returns A debug adapter descriptor or undefined
     */
    createDebugAdapterDescriptor(
        session: vscode.DebugSession,
        _executable: vscode.DebugAdapterExecutable | undefined
    ): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
        try {
            // Get Python interpreter path
            const pythonPath = this.findPythonInterpreter(session);
            if (!pythonPath) {
                const message = 'Python interpreter not found. Please install Python 3.10 or higher.';
                this.outputChannel.appendLine(`ERROR: ${message}`);
                vscode.window.showErrorMessage(message);
                return undefined;
            }

            this.outputChannel.appendLine(`Using Python interpreter: ${pythonPath}`);

            // Verify PyLog DAP server is available
            if (!this.verifyDapServer(pythonPath)) {
                const message = 'PyLog DAP server not found. Please install PyLog with: uv sync';
                this.outputChannel.appendLine(`ERROR: ${message}`);
                vscode.window.showErrorMessage(message);
                return undefined;
            }

            // Build server command and arguments
            const serverArgs = this.buildServerArguments(session);

            this.outputChannel.appendLine(`Starting DAP server: ${pythonPath} -m prolog.dap.server ${serverArgs.join(' ')}`);
            this.outputChannel.show(true);

            // Return executable descriptor with stdio communication
            return new vscode.DebugAdapterExecutable(
                pythonPath,
                ['-m', 'prolog.dap.server', ...serverArgs],
                {
                    // Use stdio for DAP protocol communication
                    // stderr will be captured separately for logging
                }
            );

        } catch (error) {
            const message = `Failed to start PyLog debugger: ${error}`;
            this.outputChannel.appendLine(`ERROR: ${message}`);
            vscode.window.showErrorMessage(message);
            return undefined;
        }
    }

    /**
     * Find the Python interpreter to use.
     * Priority: workspace venv > global venv > system python
     */
    private findPythonInterpreter(session: vscode.DebugSession): string | undefined {
        const config = vscode.workspace.getConfiguration('pylog');
        const customPython = config.get<string>('pythonPath');

        if (customPython) {
            this.outputChannel.appendLine(`Using custom Python path: ${customPython}`);
            return customPython;
        }

        // Try to find workspace .venv
        const workspaceFolder = session.workspaceFolder?.uri.fsPath;
        if (workspaceFolder) {
            const venvPaths = [
                path.join(workspaceFolder, '.venv', 'bin', 'python'),
                path.join(workspaceFolder, '.venv', 'Scripts', 'python.exe'),
                path.join(workspaceFolder, 'venv', 'bin', 'python'),
                path.join(workspaceFolder, 'venv', 'Scripts', 'python.exe'),
            ];

            for (const venvPath of venvPaths) {
                if (fs.existsSync(venvPath)) {
                    this.outputChannel.appendLine(`Found workspace venv: ${venvPath}`);
                    return venvPath;
                }
            }
        }

        // Fall back to system python
        try {
            const pythonCmd = process.platform === 'win32' ? 'python' : 'python3';
            const locateCmd = process.platform === 'win32' ? 'where' : 'which';
            const systemPython = execSync(`${locateCmd} ${pythonCmd}`, { encoding: 'utf-8' }).trim();

            // On Windows, 'where' can return multiple paths (one per line); take the first
            const pythonPath = systemPython.split('\n')[0].trim();

            if (pythonPath && fs.existsSync(pythonPath)) {
                this.outputChannel.appendLine(`Using system Python: ${pythonPath}`);
                return pythonPath;
            }
        } catch (error) {
            this.outputChannel.appendLine(`Failed to find system Python: ${error}`);
        }

        return undefined;
    }

    /**
     * Verify that the PyLog DAP server module is available.
     */
    private verifyDapServer(pythonPath: string): boolean {
        try {
            // Try to import the prolog.dap.server module
            execSync(`"${pythonPath}" -c "import prolog.dap.server"`, {
                encoding: 'utf-8',
                stdio: 'pipe'
            });
            this.outputChannel.appendLine('PyLog DAP server module verified');
            return true;
        } catch (error) {
            this.outputChannel.appendLine(`PyLog DAP server verification failed: ${error}`);
            return false;
        }
    }

    /**
     * Build command-line arguments for the DAP server.
     */
    private buildServerArguments(session: vscode.DebugSession): string[] {
        const args: string[] = [];
        const config = session.configuration;

        // Add program path
        if (config.program) {
            args.push('--program', config.program);
        }

        // Add query
        if (config.query) {
            args.push('--query', config.query);
        }

        // Add stop on entry flag
        if (config.stopOnEntry) {
            args.push('--stop-on-entry');
        }

        // Add port filters
        if (config.ports && Array.isArray(config.ports)) {
            args.push('--ports', config.ports.join(','));
        }

        // Add predicate breakpoints
        if (config.predicateBreakpoints && Array.isArray(config.predicateBreakpoints)) {
            for (const bp of config.predicateBreakpoints) {
                const bpStr = `${bp.functor}/${bp.arity}`;
                if (bp.ports && Array.isArray(bp.ports)) {
                    args.push('--breakpoint', `${bpStr}:${bp.ports.join(',')}`);
                } else {
                    args.push('--breakpoint', bpStr);
                }
            }
        }

        // Add engine options
        if (config.occursCheck) {
            args.push('--occurs-check');
        }

        if (config.useIndexing !== undefined && !config.useIndexing) {
            args.push('--no-indexing');
        }

        return args;
    }

    dispose(): void {
        this.outputChannel.dispose();
    }
}
