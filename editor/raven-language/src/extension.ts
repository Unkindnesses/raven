import * as vscode from 'vscode';

export function activate(context: vscode.ExtensionContext) {
	let disposable = vscode.commands.registerCommand('raven.helloWorld', () => {
		vscode.window.showInformationMessage('Hello from Raven!');
	});
	context.subscriptions.push(disposable);
}

export function deactivate() {}
