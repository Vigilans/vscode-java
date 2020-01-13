import * as vscode from "vscode";
import { LanguageClient, SemanticHighlightingNotification, SemanticHighlightingParams, SemanticHighlightingInformation } from "vscode-languageclient";

export function registerSemanticHighlightingProvider(languageClient: LanguageClient, context: vscode.ExtensionContext, scopes: string[][]) {
    // The parameter 'scopes' is not used here but a static map is provided instead
    // Since current legend mechanism is conflict with the textmate scope used by protocol
	const scopeBuilder = new SemanticScopeBuilder(SemanticScopeMap);
    const provider = new SemanticTokensProvider(languageClient, context, scopeBuilder);
	context.subscriptions.push(vscode.languages.registerSemanticTokensProvider({ language: 'java' }, provider, scopeBuilder.getLegend()));
}

interface SemanticToken {
    // since the line of token is modified frequently, we do not save it in a single token
	// line: number;
	character: number;
	length: number;
	scope: SemanticScope;
}

type SemanticTokens = Map<number, SemanticToken[]>; // Group tokens by line

type SemanticShift = { range: vscode.Range; shift: number };

interface SemanticEdit {
    version: number;
    shifts: SemanticShift[]; // Lines to shift back or forward
    tokens: SemanticTokens; // Lines to replace with new tokens
}

interface SemanticDocument {
	uri: string;
    currentVersion: number; // Current version is the version of current semantic tokens
    latestVersion: number;  // Latest version the latest semantic tokens version recieved from server
    semanticTokens: SemanticTokens; // Current semantic tokens
    semanticEdits: SemanticEdit[]; // Pending semantic edits
}

class SemanticTokensProvider implements vscode.SemanticTokensProvider {

	private semanticDocuments: Map<string, SemanticDocument> = new Map();

	private onDidUpdateSemanticTokens: vscode.EventEmitter<SemanticDocument> = new vscode.EventEmitter();

	public constructor(private readonly client: LanguageClient, private readonly context: vscode.ExtensionContext, private scopeBuilder: SemanticScopeBuilder) {
        client.onNotification('textDocument/semanticHighlighting', this.onSemanticHighlightingNotification.bind(this));
        vscode.workspace.onDidChangeTextDocument(this.onDidChangeTextDocument.bind(this), this, context.subscriptions);
    }

	public async provideSemanticTokens(textDocument: vscode.TextDocument, { previousResultId }: vscode.SemanticTokensRequestOptions, token: vscode.CancellationToken) {
        const disposables: vscode.Disposable[] = [];
		const semanticTokens = await new Promise<vscode.SemanticTokens | vscode.SemanticTokensEdits>((resolve) => {
            const document = this.ensureSemanticDocument(textDocument.uri.toString());
            // If buffered semantic version is newly enough, just return it
            if (document.latestVersion >= textDocument.version) {
				return resolve(this.buildTokens(document, textDocument.version, previousResultId));
			}
			// When cancelled, return currently latest version of semantic tokens (or no tokens?)
            token.onCancellationRequested(e => resolve(this.buildTokens(document, textDocument.version, previousResultId)), this, disposables);
            // Wait for newly enough version of semantic tokens
			this.onDidUpdateSemanticTokens.event(document => {
				if (document.uri === textDocument.uri.toString() && document.latestVersion >= textDocument.version) {
					resolve(this.buildTokens(document, textDocument.version, previousResultId));
				}
            }, this, disposables);
		});
		vscode.Disposable.from(...disposables).dispose();
		return semanticTokens;
	}

	private onSemanticHighlightingNotification({ textDocument: { uri, version }, lines }: SemanticHighlightingParams) {
        const document = this.ensureSemanticDocument(uri);
        const edit = this.ensureSemanticEdit(document, version);
        edit.tokens = this.decodeTokens(lines);
        document.latestVersion = version;
        this.onDidUpdateSemanticTokens.fire(document);
    }

    private onDidChangeTextDocument({ document: textDocument, contentChanges }: vscode.TextDocumentChangeEvent) {
        const document = this.semanticDocuments.get(textDocument.uri.toString());
        if (document) { // If a corresponding semantic document is found, fix its range
            for (const { range, text } of contentChanges) {
                const shift = text.split(/\r|\n|\r\n/).length - 1 - (range.end.line - range.start.line);
                if (shift !== 0) {
                    this.ensureSemanticEdit(document, textDocument.version).shifts.push({ range, shift });
                }
            }
        }
    }

	private ensureSemanticDocument(uri: string) {
        let document = this.semanticDocuments.get(uri);
        if (document === undefined) {
            document = { uri, currentVersion: 0, latestVersion: 0, semanticTokens: new Map(), semanticEdits: [] };
            this.semanticDocuments.set(uri, document);
        }
		return document;
    }

    private ensureSemanticEdit({ semanticEdits }: SemanticDocument, version: number): SemanticEdit {
        let index = semanticEdits.findIndex(edit => edit.version >= version);
        if (index === -1) { // version is latest, push to end of edits
            index = semanticEdits.length;
        }
        if (semanticEdits[index]?.version !== version) { // edit does not exist, create one
            semanticEdits.splice(index, 0, { version, shifts: [], tokens: new Map() });
        }
        return semanticEdits[index];
    }

	private decodeTokens(lines: SemanticHighlightingInformation[]): SemanticTokens {
		const tokens: Map<number, SemanticToken[]> = new Map();
		for (const { line, tokens: encodedTokens } of lines) {
            const lineTokens: SemanticToken[] = [];
            const decodedTokens = Buffer.from(encodedTokens || '', 'base64');
			for (let i = 0; i < decodedTokens.length; i += 2 * 4) {
				const character = decodedTokens.readUInt32BE(i);
				const length = decodedTokens.readUInt16BE(i + 4);
				const scope  = decodedTokens.readUInt16BE(i + 6);
				lineTokens.push({ character, length, scope });
            }
            tokens.set(line, lineTokens.sort((a, b) => a.character - b.character));
		}
		return tokens;
	}

	private buildTokens(document: SemanticDocument, targetVersion: number, previousResultId?: string): vscode.SemanticTokens | vscode.SemanticTokensEdits {
        // Flush the pending edits to target version
        this.flushEdits(document, targetVersion);

        // Push the synchronized tokens to history
        const newResultId = document.currentVersion.toString();

        // Build the token array to push
        if (true || !previousResultId) { // No previous version, return full representation of tokens
            const builder = new vscode.SemanticTokensBuilder();
            for (const [line, tokens] of document.semanticTokens) {
                for (const { character, length, scope } of tokens) {
                    const { tokenType, tokenModifiers } = this.scopeBuilder.encodeScope(scope);
                    builder.push(line, character, length, tokenType, tokenModifiers);
                }
            }
            return new vscode.SemanticTokens(builder.build(), newResultId);
        } else { // Return the delta edits based on previous version
            return new vscode.SemanticTokensEdits([], newResultId);
        }
    }

    private flushEdits2(document: SemanticDocument, targetVersion: number) {
        while (document.currentVersion < targetVersion) {
            const newEdit = document.semanticEdits.shift();
            const newTokens = new Map<number, SemanticToken[]>();
            document.semanticTokens.forEach((tokens, line) => { // Apply line shifts edit
                let newLine = line;
                for (const { range, shift } of newEdit.shifts) {
                    if (range.end.line < line) {
                        newLine += shift; // Line is affected by edit, shift it
                    } else if (range.start.line < line) {
                        return; // Line is directly in the edit, discard it
                    }
                }
                newTokens.set(newLine, tokens);
            });
            newEdit.tokens.forEach((tokens, line) => { // Apply new tokens edit
                newTokens.set(line, tokens);
            });
            document.currentVersion = newEdit.version;
            document.semanticTokens = newTokens;
        }
        // Note: builder can only accept sorted tokens
        // Line token is already sorted when decoding tokens, sort line sorting is needed here
        document.semanticTokens = new Map(Array.from(document.semanticTokens).sort((a, b) => a[0] - b[0]));
    }

    private flushEdits(document: SemanticDocument, targetVersion: number) {
        if (document.currentVersion >= targetVersion) {
            return; // Document's current tokens is already newly enough
        }
        // Apply at least one edits until edit's version is newly enough for target version
        const pendingEdits: SemanticEdit[] = [];
        for (const edit of document.semanticEdits) {
            pendingEdits.push(edit);
            if (edit.version > targetVersion) {
                break;
            }
        }
        // Right fold the edits to become one merged edit
        const mergedEdit = pendingEdits.reduceRight(this.mergeEdit.bind(this));
        document.semanticTokens = this.applyEdit(document.semanticTokens, mergedEdit);
        document.currentVersion = mergedEdit.version;
        document.semanticEdits = document.semanticEdits.slice(pendingEdits.length);

        // TODO: Then we can use the merged edit to generate vscode.SemanticTokensEdit[] now.
    }

    private applyEdit(tokens: SemanticTokens, edit: SemanticEdit): SemanticTokens {
        const newTokens = new Map<number, SemanticToken[]>();
        tokens.forEach((tokens, line) => { // Apply line shifts edit
            let newLine = line;
            for (const { range, shift } of edit.shifts) {
                if (range.end.line < line) {
                    newLine += shift; // Line is affected by edit, shift it
                } else if (range.start.line < line) {
                    return; // Line is directly in the edit, discard it
                }
            }
            newTokens.set(newLine, tokens);
        });
        edit.tokens.forEach((tokens, line) => { // Apply new tokens edit
            newTokens.set(line, tokens);
        });
        // Note: builder can only accept sorted tokens
        // Line token is already sorted when decoding tokens, sort line sorting is needed here
        return new Map(Array.from(newTokens).sort(([aLine, ], [bLine, ]) => aLine - bLine));
    }

    private mergeEdit(mergedEdit: SemanticEdit, baseEdit: SemanticEdit): SemanticEdit {
        const newShifts = baseEdit.shifts;
        for (const { range, shift } of mergedEdit.shifts) {
            let newStart = range.start.line;
            let newEnd = range.end.line;
            for (const base of baseEdit.shifts) {
                if (base.range.end.line < range.start.line) {
                    newStart -= base.shift;
                }
                if (base.range.end.line < range.end.line) {
                    newEnd -= base.shift;
                }
            }
            const newRange = new vscode.Range(range.start.with(newStart), range.end.with(newEnd));
            newShifts.push({ range: newRange, shift });
        }
        return {
            version: mergedEdit.version,
            shifts: newShifts,
            tokens: this.applyEdit(baseEdit.tokens, mergedEdit),
        };
    }
}

enum SemanticScope {
	DeprecatedMember = 0,
	Autobox = 1,
	StaticFinalField = 2,
	StaticField = 3,
	InheritedField = 4,
	Field = 5,
	MethodDeclaration = 6,
	StaticMethodInvocation = 7,
	AbstractMethodInvocation = 8,
	AnnotationElementReference = 9,
	InheritedMethodInvocation = 10,
	ParameterVariable = 11,
	LocalVariableDeclaration = 12,
	LocalVariable = 13,
	TypeVariable = 14, // before type arguments!
	Method = 15, // before types to get ctors
	TypeArgument = 16, // before other types
	AbstractClass = 17, // before classes
	Class = 18,
	Enum = 19,
	Annotation = 20, // before interfaces
	Interface = 21,
	Number = 22,
	VarKeyword = 23,
}

const SemanticScopeMap = new Map<SemanticScope, [string, string[]]>([
	[SemanticScope.DeprecatedMember, 		   ['variable',  ['member', 'deprecated']]],
	[SemanticScope.Autobox, 		 		   ['variable',  []]], // What should 'autobox' be like?
	[SemanticScope.StaticFinalField, 		   ['variable',  ['member', 'static']]], // How to express 'final'
	[SemanticScope.StaticField, 			   ['variable',  ['member', 'static']]],
	[SemanticScope.InheritedField, 			   ['variable',  ['member']]], // How to express 'inherited'?
	[SemanticScope.Field, 					   ['variable',  ['member']]],
	[SemanticScope.MethodDeclaration, 		   ['function',  ['declaration']]],
	[SemanticScope.StaticMethodInvocation, 	   ['function',  ['static']]], // How to distinguish function call between function def?
	[SemanticScope.AbstractMethodInvocation,   ['function',  ['abstract']]],
	[SemanticScope.AnnotationElementReference, ['parameter', []]],
	[SemanticScope.InheritedMethodInvocation,  ['function',  []]],
	[SemanticScope.ParameterVariable,          ['parameter', []]],
	[SemanticScope.LocalVariableDeclaration,   ['variable',  ['declaration']]],
	[SemanticScope.LocalVariable, 			   ['variable',  []]],
	[SemanticScope.TypeVariable, 			   ['type', 	 []]],
	[SemanticScope.Method,        			   ['function',  []]],
	[SemanticScope.TypeArgument,  			   ['parameterType', []]], // What's the difference between parameterType and type?
	[SemanticScope.AbstractClass, 			   ['class', 	 ['abstract']]],
	[SemanticScope.Class, 	      			   ['class', 	 []]],
	[SemanticScope.Enum, 		  			   ['enum', 	 []]],
	[SemanticScope.Annotation, 	  			   ['class', 	 []]],
	[SemanticScope.Interface, 	  			   ['interface', []]],
	[SemanticScope.Number, 	      			   ['number',  	 []]],
	[SemanticScope.VarKeyword, 	  			   ['keyword', 	 []]],
]);

class SemanticScopeBuilder {

	private tokensLegend: vscode.SemanticTokensLegend;
	private typeIndexMap: Map<string, number>;
	private modifierIndexMap: Map<string, number>;

	public constructor(private scopeMap: Map<SemanticScope, [string, string[]]>) {
		const tokenModifierSet = new Set<string>();
		const tokenTypeSet = new Set<string>();
		for (const [tokenType, tokenModifiers] of scopeMap.values()) {
			for (const modifier of tokenModifiers) {
				tokenModifierSet.add(modifier);
			}
			tokenTypeSet.add(tokenType);
		}
		const tokenModifiers = Array.from(tokenModifierSet.values());
		const tokenTypes = Array.from(tokenTypeSet.values());
		this.modifierIndexMap = new Map(tokenModifiers.map((modifier, index) => [modifier, index]));
		this.typeIndexMap = new Map(tokenTypes.map((type, index) => [type, index]));
		this.tokensLegend = new vscode.SemanticTokensLegend(tokenTypes, tokenModifiers);
	}

	public getLegend() {
		return this.tokensLegend;
	}

	public encodeScope(scope: SemanticScope): { tokenType: number, tokenModifiers: number } {
		const [type, modifiers] = this.scopeMap.get(scope);
		return {
			tokenType: this.encodeTokenType(type),
			tokenModifiers: this.encodeTokenModifiers(modifiers),
		};
	}

	private encodeTokenType(tokenType: string): number {
		return this.typeIndexMap.get(tokenType) || 0;
	}

	private encodeTokenModifiers(tokenModifiers: string[]): number {
		let result = 0;
		for (const tokenModifier of tokenModifiers) {
			const index = this.modifierIndexMap.get(tokenModifier);
			result |= index ? 1 << index : 0;
		}
		return result;
	}
}

// const tokenTypesLegend = [
// 	'comment', 'string', 'keyword', 'number', 'regexp', 'operator', 'namespace',
// 	'type', 'struct', 'class', 'interface', 'enum', 'parameterType', 'function',
// 	'macro', 'variable', 'constant', 'parameter', 'property', 'label'
// ];

// const tokenModifiersLegend = [
// 	'declaration', 'documentation', 'member', 'static', 'abstract', 'deprecated',
// 	'modification', 'async'
// ];
