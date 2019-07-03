data = list(sqf03mod, sqf04mod, sqf05mod, sqf06mod, sqf07mod)
N = 5

for (N in 1:(length(data)-1))
{
    data[[N+1]] = merge(data[[N+1]], data[[N]], by = c("pct", "race"), all = TRUE)
    data[[N+1]]$numstops = data[[N+1]]$numstops.x + data[[N+1]]$numstops.y
    data[[N+1]]$numsearches = data[[N+1]]$numsearches.x + data[[N+1]]$numsearches.y
    data[[N+1]]$numhits = data[[N+1]]$numhits.x + data[[N+1]]$numhits.y
    data[[N+1]]$searchrate = data[[N+1]]$numsearches/data[[N+1]]$numstops
    data[[N+1]]$hitrate = data[[N+1]]$numhits/data[[N+1]]$numstops
    data[[N+1]] = select(data[[N+1]], pct, race, numstops, numsearches, numhits, searchrate, hitrate)
    mergeddata = data[[N+1]]
}

