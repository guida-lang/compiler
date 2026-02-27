/**
 * Configuration object expected by the Guida runner.
 */
export interface GuidaConfig {
    // Write text or binary data to a path.
    writeFile(path: string, data: string | Uint8Array | Buffer): Promise<void>;

    // Read file contents. Can return text or binary data.
    readFile(path: string): Promise<string | ArrayBuffer | Uint8Array | Buffer | { buffer: ArrayBuffer }>;

    // Read a directory and return a list of files.
    readDirectory(path: string): Promise<{ files: string[] | Buffer<ArrayBufferLike>[] }>;

    // Create a directory.
    createDirectory(path: string): Promise<void>;

    // Get details for a path (file or directory).
    details(path: string): Promise<{ type: 'file' | 'directory' | string; createdAt?: number }>;

    // Returns the current working directory.
    getCurrentDirectory(): Promise<string>;

    // Returns the string path of the current user's home directory.
    homedir(): Promise<string>;

    // Environment map used by the runner.
    env?: Record<string, any>;
}

export interface InitOptions {
    package?: boolean;
}

export interface MakeOptions {
    debug?: boolean;
    optimize?: boolean;
    sourcemaps?: boolean;
}

// The runtime returns various JSON-able responses. Keep this generic to reflect the dynamic
// nature of the results coming from the embedded Elm/runner process.
export type GuidaResponse = any;

export type Message = string | { bold: boolean, underline: boolean, color: null | string, string: string };

export type Problem = {
    title: string;
    region: {
        start: { line: number; column: number; };
        end: { line: number; column: number; };
    };
    message: Message[];
}

export type CompileError = {
    path: string;
    name: string;
    problems: Problem[];
}

export type DiagnosticsResult =
    | null
    | {
        type: "content-error";
        error: Problem;
    }
    | {
        type: "compile-errors";
        errors: CompileError[];
    }
    | {
        type: "error";
        path: null | string;
        title: string;
        message: Message[];
    };

export type Position = {
    line: number;
    character: number;
};

export type Location = {
    path: string;
    range: { start: Position, end: Position };
};

export declare const init: (config: GuidaConfig, options?: InitOptions) => Promise<GuidaResponse>;
export declare const make: (config: GuidaConfig, path: string, options?: MakeOptions) => Promise<GuidaResponse>;
export declare const format: (config: GuidaConfig, content: string) => Promise<GuidaResponse>;
export declare const install: (config: GuidaConfig, pkg: string) => Promise<GuidaResponse>;
export declare const uninstall: (config: GuidaConfig, pkg: string) => Promise<GuidaResponse>;
export declare const diagnostics: (config: GuidaConfig, args: { content: string } | { path: string }) => Promise<DiagnosticsResult>;
export declare const getDefinitionLocation: (config: GuidaConfig, args: { path: string, position: Position }) => Promise<null | Location>;
export declare const findReferences: (config: GuidaConfig, args: { path: string, position: Position }) => Promise<null | Location[]>;
export declare const getHoverInformation: (config: GuidaConfig, args: { path: string, position: Position }) => Promise<null | { documentation: string; range?: { start: Position, end: Position } }>;