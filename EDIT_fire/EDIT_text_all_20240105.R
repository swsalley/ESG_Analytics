# edit narratives api: download all narratives stored in EDIT by geocode
# clean and combine into a tidy text narative document
# 20240105, shawn.salley@usda.gov

library(stringr)
library(dplyr)


## download the list of MLRAs (geocode) ##
mlras <- read.table("https://edit.jornada.nmsu.edu/services/downloads/esd/geo-unit-list.txt", 
                    sep="\t", quote = "\"", header=TRUE, skip=2)
sum(mlras$Ecological.site.count)

# api to download all transition narratives from EDIT, 
tn.all <- NULL
for (i in 1:nrow(mlras)){
  link <- paste0("https://edit.jornada.nmsu.edu/services/downloads/esd/",
                 mlras[i,1],"/model-transition-narratives.txt") 
  x.i <- read.table(link, sep="\t", quote = "\"", header=TRUE, skip=2)
  tn.all <- rbind(tn.all, x.i)
  print(i, mlras[i,1])}

# api to download all state narratives from EDIT, 
sn.all <- NULL
for (i in 1:nrow(mlras)){
  link <- paste0("https://edit.jornada.nmsu.edu/services/downloads/esd/",
                 mlras[i,1],"/model-state-narratives.txt") 
  x.i <- read.table(link, sep="\t", quote = "\"", header=TRUE, skip=2)
  sn.all <- rbind(sn.all, x.i)
  print(i, mlras[i,1])}


## filter and clean naratives ##

# clean transitions
tn.all[is.na(tn.all)] <- ""
tn.all <- tn.all %>% pivot_longer(cols = Mechanism:Legend, names_to = "text_type", values_to = "text") 
tn.all <- tn.all[tn.all$text != "",] 
tn.all$statchange <- paste0(tn.all$From.ecosystem.state,"_", tn.all$To.ecosystem.state)
tn.all$narative <- "transition"
tn.all <- tn.all[,c(4,16,5,15,13:14)]
colnames(tn.all) <- c( "ES.id", "narrative_type","type", "state", "text_type", "text")
tn.all$type[tn.all$type == "community pathway"] <- "community"
tn.all$type[tn.all$type == "restoration pathway"] <- "restoration"

# clean states
sn.all[is.na(sn.all)] <- ""
sn.all <- sn.all %>% pivot_longer(cols = Description:Management, names_to = "text_type", values_to = "text") 
sn.all <- sn.all[sn.all$text != "",] 
sn.all$narative <- "state"
sn.all <- sn.all[,c(4,12,5,7,10:11)]
colnames(sn.all) <- c("ES.id", "narrative_type","type", "state", "text_type", "text")
sn.all$type[sn.all$type == "ecosystem state"] <- "state"
sn.all$type[sn.all$type == "plant community"] <- "community"
sn.all$type[sn.all$type == "land use"]  <- "landuse"
sn.all$text_type[sn.all$text_type  == "Characteristics.and.indicators"]  <- "Character.indicators"

# join
n.all <- rbind(sn.all, tn.all)

# clean
n.all$text <- gsub("\n", " ", n.all$text)
n.all$text <- gsub(paste(c("[(]", "[)]"), collapse = "|"), " ",  n.all$text)
n.all$text <- gsub("[][!#$%()*,.:;<=>@^_|~.{}-]", " ", n.all$text)
n.all$text <- gsub("/", " ", n.all$text)
n.all$text <- str_squish(n.all$text)
n.all$text <- tolower(n.all$text )
n.all <- n.all[!(n.all$text == ""),]
n.all <- unique(n.all)

# save
write.csv(n.all,file="D:/FY2024/project/EDITfire/EDIT_text_all_20240105.csv")

# end