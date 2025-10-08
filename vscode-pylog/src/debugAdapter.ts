import * as vscode from 'vscode';

/**
 * Factory for creating PyLog debug adapter descriptors.
 * This is responsible for spawning the Python DAP server process.
 */
export class PyLogDebugAdapterDescriptorFactory
    implements vscode.DebugAdapterDescriptorFactory, vscode.Disposable {

    /**
     * Creates a debug adapter descriptor for a debug session.
     *
     * @param session - The debug session
     * @param _executable - Optional executable (not used)
     * @returns A debug adapter descriptor or undefined
     */
    createDebugAdapterDescriptor(
        _session: vscode.DebugSession,
        _executable: vscode.DebugAdapterExecutable | undefined
    ): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
        // TODO: Implement in issue #284
        // This will:
        // 1. Find the Python interpreter
        // 2. Locate the pylog-dap entry point
        // 3. Build the command to spawn the DAP server
        // 4. Return a DebugAdapterExecutable with the command

        // For now, return undefined to indicate that the factory is not yet implemented
        vscode.window.showErrorMessage(
            'PyLog Debug Adapter is not yet fully implemented. See issue #284.'
        );
        return undefined;
    }

    dispose(): void {
        // Cleanup resources when the factory is disposed
        // Currently no resources to clean up
    }
}
