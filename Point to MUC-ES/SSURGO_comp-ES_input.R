# Steps for ES to Component to Point
# ESG Analytics, 4/11/2022
# SSURGO_comp-ES_input.R
#
# load component and coecoclass tables & subset
#
library(dplyr)
#
component <- read.csv("E:/ESG_analytics/Data/component/component_20220308.txt", sep="|") # 1,242,310
coecoclass <- read.csv("E:/ESG_analytics/Data/component/coecoclass_20220308.txt", sep="|") #985,974
#
coecoclass <- coecoclass %>% subset(coecoclass$sourcesdwtablephysicalname == "coecosite") # get rid of plants, n = 609,346
#
component <- component %>% subset(component$cokey %in% coecoclass$cokey) # components with ES, n = 593474
#
component.coecoclass <- left_join(component, coecoclass, by = "cokey") %>% 
  select(mukey, cokey, ecoclassid, compname, comppct_r, majcompflag) # 609346
component.coecoclass <- unique(component.coecoclass) # 609241
#
# component.coecoclass is the full component list that have ES assignment (date 2021 SSURGO refresh, check date)
#
#
mapunit <- read.csv("E:/ESG_analytics/Data/component/mapunit_20220309.txt", sep="|") # 328,272
mapunit <- mapunit[c(1:2,5)]
mapunit <- unique(mapunit) 
ssurgo.all <- left_join(component.coecoclass, mapunit, by = "mukey")
ssurgo.all<- unique(ssurgo.all)
#
write.csv(ssurgo.all, "E:/ESG_analytics/WorkFlow/R/SSURGO_comp-ES_input.csv")
#
# END
# ssurgo.all (609241,9)
# c("X","mukey","cokey","ecoclassid","compname","comppct_r","majcompflag","nationalmusym","mukind")