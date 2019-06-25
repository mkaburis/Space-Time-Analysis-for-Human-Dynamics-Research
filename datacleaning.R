
#data cleaning
sqf03 = read.csv(file = "/Users/sam/Desktop/threshold_test/sqf03.csv")
sqf03 = select(sqf03, starts_with("pct"), starts_with("datestop"), 
  starts_with("timestop"), starts_with("crimsusp"), starts_with("searched"), 
  starts_with("anycontra"), starts_with("race"))


