#first run the datasets through datacleaning.R then run this script. 

#appends the total crimes for each year (each column) to 
#each separate year's dataframe. 
datasets = list(sqf03mod,sqf04mod,sqf05mod,sqf06mod,sqf07mod,sqf08mod,sqf09mod,
                sqf10mod,sqf11mod,sqf12mod,sqf13mod,sqf14mod,sqf15mod,sqf16mod,
                sqf17mod,sqf18mod)
crimes = read.csv(file = "precinct_crime_data.csv", stringsAsFactors = F)
for (i in 1: length(datasets))
{
  df = merge(datasets[[i]] %>% filter(race == "B"), datasets[[i]] 
             %>% filter(race == "H"), by = "pct", all = TRUE)
  datasets[[i]] = merge(df, datasets[[i]] %>% filter(race == "W"), by = "pct", all = TRUE)
}
for (i in 1: length(datasets))
{
  datasets[[i]]$numcrimes = crimes[,i+1] %>% na.omit()
}

sqf03mod = datasets[[1]]
sqf04mod = datasets[[2]]
sqf05mod = datasets[[3]]
sqf06mod = datasets[[4]]
sqf07mod = datasets[[5]]
sqf08mod = datasets[[6]]
sqf09mod = datasets[[7]]
sqf10mod = datasets[[8]]
sqf11mod = datasets[[9]]
sqf12mod = datasets[[10]]
sqf13mod = datasets[[11]]
sqf14mod = datasets[[12]]
sqf15mod = datasets[[13]]
sqf16mod = datasets[[14]]
sqf17mod = datasets[[16]]
sqf18mod = datasets[[16]]

#merges a list of 5 dataframes. to include more dataframes, add them in the below line
library(reshapmd2)
mergeddata = rbind(melt(datasets[[1]],id=colnames(datasets[[1]])), 
          melt(datasets[[2]],id=colnames(datasets[[2]])),
          melt(datasets[[3]],id=colnames(datasets[[3]])),
          melt(datasets[[4]],id=colnames(datasets[[4]])),
          melt(datasets[[5]],id=colnames(datasets[[5]])))
md1 = ddply(mergeddata[1:7],c("pct","race.x"),numcolwise(sum))
md1$searchrate.x = md1$numsearches.x/md1$numstops.x
md1$hitrate.x = md1$numhits.x/md1$numstops.x
md2 = ddply(mergeddata[c(1,8:13)],c("pct","race.y"),numcolwise(sum))
md2$searchrate.y = md2$numsearches.y/md2$numstops.y
md2$hitrate.y = md2$numhits.y/md2$numstops.y
md3 = ddply(mergeddata[c(1,14:20)],c("pct","race"),numcolwise(sum))
md3$searchrate = md3$numsearches/md3$numstops
md3$hitrate = md3$numhits/md3$numstops
mergeddata = merge(md1, md2) %>% merge(md3)


