

function unrawTrace(x : TraceRaw) : Trace
{
    return {
        command: x[0],
        start: x[1],
        stop: x[2]
    }
}

function unrawProfile(x : ProfileRaw) : Profile
{
    return {
        name: x[0],
        execution: x[1],
        built: x[2],
        changed: x[3],
        depends: x.length > 4 ? x[4] : [],
        traces: x.length > 5 ? x[5].map(unrawTrace) : [],
    }
}
