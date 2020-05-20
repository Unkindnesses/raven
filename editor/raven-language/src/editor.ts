import * as vscode from 'vscode'

async function space(editor: vscode.TextEditor, edit: vscode.TextEditorEdit) {
    if (editor.selections.length == 1 && editor.selection.isEmpty) {
        let position = editor.selection.active;
        // `{|}` -> `{ | }`
        let start = new vscode.Position(position.line, Math.max(position.character-1,0));
        var end   = new vscode.Position(position.line, position.character+1);
        var text = editor.document.getText(new vscode.Range(start, end));
        if (text == '{}') {
            await editor.edit(edit => { // Force this to happen before the cursor change.
                edit.insert(editor.selection.active, '  ');
            });
            let pos = new vscode.Position(position.line, position.character+1);
            editor.selection = new vscode.Selection(pos, pos);
            return;
        }
        // `| }` -> ` |}`
        var end   = new vscode.Position(position.line, position.character+2);
        var text = editor.document.getText(new vscode.Range(position, end));
        if (text == ' }') {
            let pos = new vscode.Position(position.line, position.character+1);
            editor.selection = new vscode.Selection(pos, pos);
            return;
        }
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
}

export function activateEditorSupport(context: vscode.ExtensionContext) {
    let disposable = vscode.commands.registerTextEditorCommand('raven.space', space);
    context.subscriptions.push(disposable);
}
