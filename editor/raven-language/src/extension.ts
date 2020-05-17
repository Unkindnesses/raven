import * as vscode from 'vscode';
import { activateEditorSupport } from './editor';

export function activate(context: vscode.ExtensionContext) {
	activateEditorSupport(context);
}

export function deactivate() {}
