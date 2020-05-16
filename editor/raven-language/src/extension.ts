import * as vscode from 'vscode';

export function activate(context: vscode.ExtensionContext) {
	let disposable = vscode.commands.registerTextEditorCommand('raven.space', (editor, edit) => {
		if (editor.selections.length == 1 && editor.selection.isEmpty) {
			edit.insert(editor.selection.active, ' ');
			return;
		}
		// Fallback: simulate standard behaviour
		editor.selections.forEach((selection, i) => {
			if (selection.isEmpty) {
				edit.insert(selection.active, ' ');
			} else {
				edit.replace(selection, ' ');
				editor.selections[i] = new vscode.Selection(selection.end, selection.end);
			}
		});
	});
	context.subscriptions.push(disposable);
}

export function deactivate() {}
