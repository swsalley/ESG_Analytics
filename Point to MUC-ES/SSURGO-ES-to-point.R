# Steps for Component ES to Point
# ESG Analytics, 4/13/2022
# SSURGO-ES-to-point.R

# Libraries ####

library(dplyr)

# Input files ####

# every ES asigned to a component
# input file from: "E:/ESG_analytics/WorkFlow/R/SSURGO_comp-ES_input.R"
# ssurgo.all, "E:/ESG_analytics/WorkFlow/R/SSURGO_comp-ES_input.csv"
#  w/ esid, component and mapunit data, downloaded 20220308
# c("X","mukey","cokey","ecoclassid","compname","comppct_r","majcompflag","nationalmusym","mukind")

ssurgo.all<-read.csv("E:/ESG_analytics/WorkFlow/R/SSURGO_comp-ES_input.csv") # 609,241

# All point-pedon data from modeling data set with 2021 mukey extracted
# Extract raster (mukey) from gNATSGO v. 4 (2021), downloaded april, 2022, 
# pnts_gNATSGO, "E:/ESG_analytics/Data/point/all_points.csv"
# w/ 2021 es assingment and mapunit spot (raster=mukey)
# w/ 20220308 
# c("Field1","ID","latstddeci","longstddec","ecoclassid.2021","taxonname","siteobsiid", "mukey", RASTERVALU, Mukey_1)

all.points <- read.csv("E:/ESG_analytics/Data/point/pnts_gNATSGO.csv") # 291,327

# Analysis ####

ssurgo.all %>% subset(ssurgo.all$mukey %in% all.points$MUKEY_1) %>% summarize(n = n()) # n= 155278
all.points %>% subset(all.points$MUKEY_1 %in% ssurgo.all$mukey) %>% summarize(n = n()) # n= 255931

# join map unit data to point data

ssurgo.match <- ssurgo.all %>% subset(ssurgo.all$mukey %in% all.points$MUKEY_1)
all.points.ES <- inner_join(all.points, ssurgo.match, by = c("RASTERVALU" = "mukey")) 

# all components assigned to the point (many to one)

# 1. Taxa = comp name, component ecological site assigned to point ####

all.points.ES %>% subset(taxonname == compname) %>% summarize(n = n()) # 301091
points.taxa.1 <- all.points.ES %>% subset(taxonname == compname) 

# Data Clean & Check
points.taxa.2 <- points.taxa.1[,c(8,14)]
points.taxa.2 <- unique(points.taxa.2) # 172702
table(table(points.taxa.2$siteobsiid)) # n = 1 = 156140, n !=1 = 7782

# subset ES > 1
points.taxa.3.t <- table(points.taxa.2$siteobsiid) %>% data.frame() %>% subset(Freq > 1)
points.taxa.3 <- points.taxa.1 %>% subset(siteobsiid %in% points.taxa.3.t$Var1) 
head(points.taxa.3) # n = 66185

# review Freq > 1, these have more than 1 ES for a site observation 
# no way to reconcile a single ES, decision, drop those mapunit concepts.

# keep those with only 1

points.taxa.4.t <- table(points.taxa.2$siteobsiid) %>% data.frame() %>% subset(Freq == 1)
points.taxa.4 <- points.taxa.2 %>% subset(siteobsiid %in% points.taxa.4.t$Var1) 
head(points.taxa.4) # n = 156140
tail(table(table(points.taxa.4$ecoclassid.y)), 15) # R077CY022TX and R150AY741TX over 2k points each, 
# come back to this and make a map

Site.ES <- points.taxa.4 # n = 156140

# Site.ES, w/ points.taxa.4, n = 156140, relationship between siteobs and ESid

rm(points.taxa.1, points.taxa.2, points.taxa.3, points.taxa.3.t, points.taxa.4, points.taxa.4.t)

# 2. Consociation MU, major component ecological site assigned to point ####

all.points.ES %>% subset(mukind == "Consociation" & majcompflag == "Yes") %>% summarize(n = n()) # 152821
points.taxa.1 <- all.points.ES %>% subset(mukind == "Consociation" & majcompflag == "Yes")

# Data Clean & Check
points.taxa.1 <- points.taxa.1[,c(8,14)]
points.taxa.1 <- unique(points.taxa.1) # 98536
table(table(points.taxa.1$siteobsiid)) # n = 1 = 93310, n !=1 = 2434
#
points.taxa.2.t <- table(points.taxa.1$siteobsiid) %>% data.frame() %>% subset(Freq == 1)
points.taxa.2 <- points.taxa.1 %>% subset(siteobsiid %in% points.taxa.2.t$Var1) 
head(points.taxa.2) # n = 93310

# number of points in this set not in previous

points.taxa.2 %>% subset(Site.ES$siteobsiid %in% points.taxa.1$siteobsiid)  %>% summarize(n = n()) #91634
points.taxa.2 %>% subset(points.taxa.2$siteobsiid %in% Site.ES$siteobsiid)  %>% summarize(n = n()) #91399
# points.taxa.2, n = 91399, this is the dominant + consociation relationship between siteobs and ESid

# add new function [  '%!in%' <- function(x,y)!('%in%'(x,y))  ]
points.taxa.3 <- points.taxa.2 %>% subset(siteobsiid %!in% Site.ES$siteobsiid) # adds 1911 observations
head(points.taxa.3)

# rbind to previous first step

Site.ES <- rbind(Site.ES, points.taxa.3) # n = 158051
Site.ES <- unique(Site.ES)

table(table(Site.ES$site))

# take out the trash

rm(points.taxa.1, points.taxa.2, points.taxa.2.t, points.taxa.3)
gc()

# 3. Consociations Mono-taxa ES for MU, ####

head(ssurgo.all)

points.taxa.1 <- ssurgo.all %>% select(mukey, ecoclassid, comppct_r) # 609241
points.taxa.1 <- unique(points.taxa.1) # 589352
points.taxa.2 <- points.taxa.1 %>% group_by(mukey, ecoclassid) %>% summarise(ecopct_r = sum(comppct_r))
points.taxa.2 <- unique(points.taxa.2) # 473543
#
#points.taxa.3 <- points.taxa.2 %>% count(mukey) %>% subset(n == "1") # 90704
points.taxa.3 <- points.taxa.2 %>% subset(ecopct_r > 49) # 191835
points.taxa.3 <- unique(points.taxa.3) # 191835
#
points.taxa.4 <- points.taxa.3 %>% count(mukey) # 186558
# remove those with multiple
points.taxa.4 <- points.taxa.4[points.taxa.4$n == "1",]
#
points.taxa.5 <- points.taxa.3 %>% subset(mukey %in% points.taxa.4$mukey) 
points.taxa.5 <- left_join(points.taxa.5,unique(ssurgo.all[,c(2,9)]), by = "mukey") %>% subset(mukind == "Consociation")
#
# subset pedon points that match 
points.taxa.6 <- all.points.ES[all.points.ES$mukey %in% points.taxa.5$mukey,] %>% left_join(.,points.taxa.5, by = "mukey" )
points.taxa.6 <- points.taxa.6 %>% select(siteobsiid,ecoclassid)
points.taxa.6 <- unique(points.taxa.6) #sites in consociation map units with >50 of 1 named ES.
#
nrow(points.taxa.6[points.taxa.6$siteobsiid   %in% Site.ES$siteobsiid,]) # 93250
nrow(points.taxa.6[points.taxa.6$siteobsiid   %!in% Site.ES$siteobsiid,]) # 37
#
#This adds 37 point may not of been worth the extra 2 hours to figure that out.
points.taxa.7 <- points.taxa.6[points.taxa.6$siteobsiid   %!in% Site.ES$siteobsiid,] # 37
colnames(points.taxa.7)[2] <- "ecoclassid.y"
Site.ES <- rbind(Site.ES, points.taxa.7)  # n = 158088
Site.ES <- unique(Site.ES) # n = 158088
#

# Clean to share ####

clean1 <- unique(all.points.ES[,c(8,9,4:5)])
clean2 <- inner_join(Site.ES, df1 , by = "siteobsiid") #158088
clean2 <- clean2[,c(1,4,5,3,2)]
clean2 <- unique(clean2)

write.csv(clean2, "E:/ESG_analytics/Data/component/siteobs_to_comp-es.csv")
head(clean2) # this is the full ES training data obtained by component associations by siteobsiid

# End ####

siteobsiid, taxonname, latstddeci, longstddec, 
df1 <- unique(all.points.ES[,c(8,9,4:5)])
df2 <- inner_join(Site.ES, df1 , by = "siteobsiid") #158088



