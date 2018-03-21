# batchme.R

library(readxl)
library(tidyverse)
library(mi)

beacon= read_excel("./indata/beagess_IC-RISC.xlsx")

# beacon= rownames_to_column(beacon, var= "obsnum")

beacon= filter(beacon, age !=-9, age>= 40, age<=79, site != "30", site != "55",
               site != "21", site != "11", site != "12", site != "13", site != "14", site!= "18", site != "19")
beacon= select(beacon, -bmi_recent_healthy)

# blech= c("bmi5", "cig_smk_ever")
# beacon[blech]= sapply(beacon[blech], function(x) ifelse(x==-9, NA, x))

beacon[5:8]= sapply(beacon[5:8], function(x) ifelse(x==-9, NA, x))
beacon[5:8]= sapply(beacon[5:8], function(x) as.integer(x))
beacon$site= as.factor(beacon$site)
beacon$cc= ifelse(beacon$casegp=="CO", 0,
                  ifelse(beacon$casegp=="BE", 1,
                         ifelse(beacon$casegp=="EA", 2, NA)))
beacon= select(beacon, -casegp)
beacon= as.data.frame(beacon)

mdf= missing_data.frame(beacon)
show(mdf)
summary(mdf)
mdf2= mi(mdf)
beacon_imp= mi::complete(mdf2, 1)
beacon_imp= select(beacon_imp, -starts_with("missing"))
rm(beacon, mdf, mdf2)

nsim <- nrow(beacon_imp)
set.seed(492898)

beacon_imp= rename(beacon_imp,
                   nsaids= nsaid_ever,
                   reflux= hb_rf_freq,
                   bmi= bmi5,
                   smoking= cig_smk_ever) %>%
            mutate(biopsy.abn= 0, segment.length= 0)

race= runif(nsim)
family.history= runif(nsim)
statins= runif(nsim)
physical.activity= runif(nsim)
beacon_imp$cc= as.numeric(as.character(beacon_imp$cc))
beacon_imp$site= as.numeric(as.character(beacon_imp$site))
beacon_imp$sex= as.numeric(as.character(beacon_imp$sex))

beacon_imp= cbind(beacon_imp, family.history, statins, physical.activity, race)
beacon_imp$statins= ifelse(beacon_imp$cc==0,
                           cut(statins, c(0,.75,1), labels= c(1,0)),
                           cut(statins, c(0,.81,1), labels= c(1,0)))
beacon_imp$family.history= ifelse(beacon_imp$cc==0,
                                  cut(family.history, c(0, 0.93, 1), labels= c(0,1)),
                                  cut(family.history, c(0, 0.82, 1), labels= c(0,1)))
beacon_imp$physical.activity= ifelse(beacon_imp$cc==0,
                                  cut(physical.activity, breaks= 4, labels = c(3,2,1,0)),
                                  cut(physical.activity, c(0, .22, .46, 0.72, 1), labels= c(3,2,1,0)))
beacon_imp$race= ifelse(beacon_imp$cc==0,
                                  cut(race, c(0, 0.85, 1), labels= c(1,2)),
                                  cut(race, c(0, 0.966, 1), labels= c(1,2)))
beacon_imp$sex = ifelse(beacon_imp$race== 2, 1, beacon_imp$sex)

save(beacon_imp, file="beacon_imp.RData")

source("batchme3.R")
