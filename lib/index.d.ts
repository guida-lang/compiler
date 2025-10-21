/**
 * Configuration object expected by the Guida runner.
 */
export interface GuidaConfig {
    // Write text or binary data to a path.
    writeFile(path: string, data: string | ArrayBuffer | Uint8Array | Buffer): Promise<void>;

    // Read file contents. Can return text or binary data.
    readFile(path: string): Promise<string | ArrayBuffer | Uint8Array | Buffer | { buffer: ArrayBuffer }>;

    // Read a directory and return a list of files.
    readDirectory(path: string): Promise<{ files: string[] }>;

    // Create a directory.
    createDirectory(path: string): Promise<void>;

    // Get details for a path (file or directory).
    details(path: string): Promise<{ type: 'file' | 'directory' | string; createdAt?: number }>;

    // Environment map used by the runner.
    env?: Record<string, any>;
}

export interface MakeOptions {
    debug?: boolean;
    optimize?: boolean;
    sourcemaps?: boolean;
}

// The runtime returns various JSON-able responses. Keep this generic to reflect the dynamic
// nature of the results coming from the embedded Elm/runner process.
export type GuidaResponse = any;

export function make(config: GuidaConfig, path: string, options?: MakeOptions): Promise<GuidaResponse>;
export function format(config: GuidaConfig, content: string): Promise<GuidaResponse>;
export function install(config: GuidaConfig, pkg: string): Promise<GuidaResponse>;
export function uninstall(config: GuidaConfig, pkg: string): Promise<GuidaResponse>;

declare const _default: {
    make: (config: GuidaConfig, path: string, options?: MakeOptions) => Promise<GuidaResponse>;
    format: (config: GuidaConfig, content: string) => Promise<GuidaResponse>;
    install: (config: GuidaConfig, pkg: string) => Promise<GuidaResponse>;
    uninstall: (config: GuidaConfig, pkg: string) => Promise<GuidaResponse>;
};

export default _default;