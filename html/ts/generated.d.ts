// Stuff that Shake generates and injects in

// The version of Shake
declare const version: string;

/////////////////////////////////////////////////////////////////////
// PROFILE DATA

declare const profile: Profile[];

declare type timestamp = int

interface Trace {
    command: string;
    start: seconds;
    stop: seconds;
}

interface Profile {
    name: string; // Name of the thing I built
    built: timestamp; // Timestamp at which I was built
    changed: timestamp; // Timestamp at which I last changed
    depends: int[]; // Which 0-based indexes I depended on (always lower than my index)
    execution: seconds; // Seconds I took to execute
    traces?: Trace[]; // List of traces
}


/////////////////////////////////////////////////////////////////////
// PROGRESS DATA

declare const progress: { name: String, values: Progress[] }[];

interface Progress {
    idealSecs: number;
    idealPerc: number;
    actualSecs: number;
    actualPerc: number;
}
