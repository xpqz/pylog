import * as vscode from 'vscode';
import { PyLogDebugAdapterDescriptorFactory } from './debugAdapter';

export function activate(context: vscode.ExtensionContext): void {
    // Register the debug adapter descriptor factory for the 'pylog' debug type
    const factory = new PyLogDebugAdapterDescriptorFactory();
    context.subscriptions.push(
        vscode.debug.registerDebugAdapterDescriptorFactory('pylog', factory)
    );

    // Ensure the factory is properly disposed when the extension is deactivated
    context.subscriptions.push(factory);
}

export function deactivate(): void {
    // Cleanup is handled by VS Code disposing subscriptions
}
