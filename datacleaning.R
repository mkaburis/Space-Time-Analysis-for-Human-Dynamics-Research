# datacleaning.R
# NJIT/HU REU (Summer 2019) 

library(plyr)
library(dplyr)
# library(tidyr)
# loading in files
# Ensure the working directory is set to where the "datasets" folder is

target_dir = "datasets"
myfiles = list.files(path=target_dir, pattern="*.csv", full.names=T)
sqf_dat = list()

# loop through all .csv files and add them to the list
for (i in 1:length(myfiles)) {
  
  # added print statement for testing purposes (can be removed for production)
  print(myfiles[i])
  sqf_dat[[i]] = read.csv(file = myfiles[i], stringsAsFactors = F)
}

#creating lists of older years and newer years

# 2003 - 2016 datasets
data = list(sqf_dat[[1]], sqf_dat[[2]], sqf_dat[[3]], sqf_dat[[4]], sqf_dat[[5]], sqf_dat[[6]],
            sqf_dat[[7]], sqf_dat[[8]], sqf_dat[[9]], sqf_dat[[10]], sqf_dat[[11]], sqf_dat[[12]],
            sqf_dat[[13]], sqf_dat[[14]])
# data = list(sqf03, sqf04, sqf05,sqf06,sqf07,sqf08,sqf09,sqf10,sqf11,sqf12,sqf13,sqf14,sqf15,sqf16)

# 2017 - 2018 datasets
data2 = list(sqf_dat[[15]], sqf_dat[[16]])
# data2 = list(sqf17, sqf18)

# cleaning function for older years
clean = function(dataset)
{
  # Precinct, Searched, Race, Create Col. Merge Contraband Columns
  dataset = select(dataset, pct, searched, contrabn, pistol, riflshot, asltweap, knifcuti,
       machgun, othrweap, race) %>%
  # turning searched and contraband columns into 0s and 1s to add them
  mutate(contraband_flag = if_else((dataset$contrabn == "Y" | dataset$pistol == "Y" |
                                    dataset$riflshot == "Y" | dataset$asltweap == "Y" |
                                    dataset$knifcuti == "Y" | dataset$machgun == "Y" |
                                    dataset$othrweap == "Y"),
                                   as.integer(1), as.integer(0))) %>%
  mutate(searched = if_else((dataset$searched == "N"), as.integer(0), as.integer(1))) %>%
  filter(searched >= contraband_flag)
  #grouping black hispanic and white hispanic as hispanic
  dataset$race[dataset$race == "P"] = "H"
  dataset$race[dataset$race == "Q"] = "H"
  # formatting 2003 data into the format of the NC data. removing extraneous races
  dataset = dataset %>%
    select(pct, searched, contraband_flag, race) %>%
    filter(race != "A") %>%
    filter(race != "I") %>%
    filter(race != "Z") %>%
    filter(race != "X") %>%
    filter(race != " ") %>%
    filter(pct <= 123) %>%
    group_by(pct, race) %>%
    #finding total stops, searches, and hits per race per precinct
    dplyr::summarize(numstops = length(searched), numsearches = sum(as.numeric(searched)),
              numhits = sum(as.numeric(contraband_flag)))
    dataset$race = as.factor(dataset$race)
    dataset$race = droplevels(dataset$race, exclude = "U")
    na.omit(dataset)
}

# cleaning function for newer years
clean2 = function(dataset)
{
  # Precinct, Searched, Race, Create Col. Merge Contraband Columns
  dataset = select(dataset, STOP_LOCATION_PRECINCT, SEARCHED_FLAG, OTHER_CONTRABAND_FLAG,
                   WEAPON_FOUND_FLAG, SUSPECT_RACE_DESCRIPTION)
    names(dataset)[1] = "pct"
    names(dataset)[2] = "searched"
    names(dataset)[5] = "race"
  # turning searched and contraband columns into 0s and 1s to add them
  dataset = dataset %>%
    mutate(contraband_flag = if_else((dataset$OTHER_CONTRABAND_FLAG == "Y" |
                                        dataset$WEAPON_FOUND_FLAG == "Y"),
                                     as.integer(1), as.integer(0))) %>%
    mutate(searched = if_else((dataset$searched == "N"), as.integer(0), as.integer(1))) %>%
    filter(searched >= contraband_flag)
  #grouping black hispanic and white hispanic as hispanic. renaming races
  dataset$race[dataset$race == "BLACK HISPANIC"] = "H"
  dataset$race[dataset$race == "WHITE HISPANIC"] = "H"
  dataset$race[dataset$race == "WHITE"] = "W"
  dataset$race[dataset$race == "BLACK"] = "B"

  # formatting 2003 data into the format of the NC data. removing extraneous races
  dataset = dataset %>%
    select(pct, searched, contraband_flag, race) %>%
    filter(race != "ASIAN/PAC.ISL") %>%
    filter(race != "ASIAN / PACIFIC ISLANDER") %>%
    filter(race != "AMERICAN INDIAN/ALASKAN NATIVE") %>%
    filter(race != "AMER IND") %>%
    filter(race != "(null)") %>%
    filter(pct <= 123) %>%
    group_by(pct, race) %>%
    # finding total stops, searches, and hits per race per precinct
    dplyr::summarize(numstops = length(searched), numsearches = sum(as.numeric(searched)),
                     numhits = sum(as.numeric(contraband_flag)))
    dataset$race = as.factor(dataset$race)
    dataset$race = droplevels(dataset$race, exclude = "U")
    na.omit(dataset)
}

# applying functions to older and newer years and extracting the info

data = lapply(data, clean)
data2 = lapply(data2, clean2)
for (i in 1:length(data))
{
  c = data[[i]]$numsearches/data[[i]]$numstops
  data[[i]]$searchrate = c
  d = data[[i]]$numhits/data[[i]]$numsearches
  data[[i]]$hitrate = d
  data[[i]]$hitrate[is.nan(data[[i]]$hitrate)] = 0
}
for (i in 1:length(data2))
{
  c = data2[[i]]$numsearches/data2[[i]]$numstops
  data2[[i]]$searchrate = c
  d = data2[[i]]$numhits/data2[[i]]$numsearches
  data2[[i]]$hitrate = d
  data2[[i]]$hitrate[is.nan(data2[[i]]$hitrate)] = 0
}
sqf03mod = data[[1]]
sqf04mod = data[[2]]
sqf05mod = data[[3]]
sqf06mod = data[[4]]
sqf07mod = data[[5]]
sqf08mod = data[[6]]
sqf09mod = data[[7]]
sqf10mod = data[[8]]
sqf11mod = data[[9]]
sqf12mod = data[[10]]
sqf13mod = data[[11]]
sqf14mod = data[[12]]
sqf15mod = data[[13]]
sqf16mod = data[[14]]
sqf17mod = data2[[1]]
sqf18mod = data2[[2]]
