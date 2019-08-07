#first run the datasets through datacleaning.R then run this script. 

#appends the total crimes for each year (each column) to 
#each separate year's dataframe. 
datasets = list(sqf03mod,sqf04mod,sqf05mod,sqf06mod,sqf07mod,sqf08mod,sqf09mod,
                sqf10mod,sqf11mod,sqf12mod,sqf13mod,sqf14mod,sqf15mod,sqf16mod,
                sqf17mod,sqf18mod)
crimes = read.csv(file = "precinct_crime_data.csv", stringsAsFactors = F)
for (i in 14: length(datasets))
{
  datasets[[i]] = merge(datasets[[i]] %>% filter(race == "B"), datasets[[i]]
             %>% filter(race == "H"), by = "pct", all = TRUE) %>%
             merge(datasets[[i]] %>% filter(race == "W"), by = "pct", all = TRUE)
  datasets[[i]]$race[is.na(datasets[[i]]$race)] = "W"
  datasets[[i]]$race.y[is.na(datasets[[i]]$race.y)] = "H"
  datasets[[i]]$race.x[is.na(datasets[[i]]$race.x)] = "B"
  datasets[[i]][is.na(datasets[[i]])] = 0
  names(datasets[[i]]) = c("pct", "race", "numstops", "numsearches", "numhits", "searchrate", 
                 "hitrate", "race", "numstops", "numsearches", "numhits", "searchrate",
                 "hitrate", "race", "numstops", "numsearches", "numhits", "searchrate",
                 "hitrate")
  datasets[[i]] = merge(datasets[[i]][,c(1,2:7)], datasets[[i]][,c(1,8:13)], all = TRUE) %>%
       merge(datasets[[i]][,c(1,14:19)], all = TRUE)
}
for (i in 1: length(datasets))
{
  datasets[[i]]$numcrimes = rep(crimes[,i+1] %>% na.omit(), each = 3, times = 1)
  
  datasets[[i]] = as.data.frame(datasets[[i]])
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
sqf17mod = datasets[[15]]
sqf18mod = datasets[[16]]

#merges a list of 4 dataframes. to include more dataframes, add them in line 39-43
library(reshape2)
# Chunk1
md1 = rbind(melt(datasets[[1]],id=colnames(datasets[[1]])),
          melt(datasets[[2]],id=colnames(datasets[[2]])),
          melt(datasets[[3]],id=colnames(datasets[[3]])),
          melt(datasets[[4]],id=colnames(datasets[[4]]))) %>%
          ddply(c("pct","race"),numcolwise(sum))
md1$searchrate = md1$numsearches/md1$numstops
md1$hitrate = md1$numhits/md1$numsearches
md1$pct = as.factor(md1$pct)

write.csv(md1, file = "mergeddata1.csv", row.names = F)

# Chunk2
md2 = rbind(melt(datasets[[5]],id=colnames(datasets[[5]])),
            melt(datasets[[6]],id=colnames(datasets[[6]])),
            melt(datasets[[7]],id=colnames(datasets[[7]])),
            melt(datasets[[8]],id=colnames(datasets[[8]]))) %>%
  ddply(c("pct","race"),numcolwise(sum))
md2$searchrate = md2$numsearches/md2$numstops
md2$hitrate = md2$numhits/md2$numsearches
md2$pct = as.factor(md2$pct)

write.csv(md2, file = "mergeddata2.csv", row.names = F)

# Chunk3
md3 = rbind(melt(datasets[[9]],id=colnames(datasets[[9]])),
            melt(datasets[[10]],id=colnames(datasets[[10]])),
            melt(datasets[[11]],id=colnames(datasets[[11]])),
            melt(datasets[[12]],id=colnames(datasets[[12]]))) %>%
  ddply(c("pct","race"),numcolwise(sum))
md3$searchrate = md3$numsearches/md3$numstops
md3$hitrate = md3$numhits/md3$numsearches
md3$pct = as.factor(md3$pct)

write.csv(md3, file = "mergeddata3.csv", row.names = F)

# Chunk4
md4 = rbind(melt(datasets[[13]],id=colnames(datasets[[13]])),
            melt(datasets[[14]],id=colnames(datasets[[14]])),
            melt(datasets[[15]],id=colnames(datasets[[15]])),
            melt(datasets[[16]],id=colnames(datasets[[16]]))) %>%
  ddply(c("pct","race"),numcolwise(sum))
md4$searchrate = md4$numsearches/md4$numstops
md4$hitrate = md4$numhits/md4$numsearches
md4$pct = as.factor(md4$pct)

write.csv(md4, file = "mergeddata4.csv", row.names = F)
