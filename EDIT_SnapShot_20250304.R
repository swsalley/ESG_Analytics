# EDIT snapshot, 20250305

# class features

ClassFeatures <- c("class-list","climatic-features","landforms",
                   "physiographic-interval-properties","physiographic-nominal-properties",
                   "physiographic-ordinal-properties","annual-production","forest-overstory",
                   "forest-understory","rangeland-plant-composition","soil-surface-cover",
                   "soil-parent-material","soil-interval-properties","soil-nominal-properties",
                   "soil-ordinal-properties","soil-profile-properties","soil-surface-textures",
                   "model-state-narratives","model-transition-narratives")

# api function

cf <-  function(mlras, i, ClassFeatures, j){
  cat(mlras[i,1],"  ")
  l <-  paste0("https://edit.jornada.nmsu.edu/services/downloads/esd/",mlras[i,1],"/",ClassFeatures[j], ".txt")
  read.table(l, sep="\t", quote = "\"", header=TRUE, skip=2)
}

# convey

t1 <- Sys.time()

mlras <- read.table("https://edit.jornada.nmsu.edu/services/downloads/esd/geo-unit-list.txt", 
                    sep="\t", quote = "\"", header=TRUE, skip=2) 

length(unique(cf.1$Ecological.site.ID)) # 7503 ES concepts

for (k in 1:length(ClassFeatures)){
  cf.list <- list()
  for (i in 1:nrow(mlras)) {cf.list[[i]] <- cf(mlras, i, ClassFeatures, k)} 
  assign(paste0("cf.", k), do.call("rbind", cf.list))
  cat(ClassFeatures[k],"  ", sep="\n")
}

t2 <- Sys.time()
t2-t1 # 22.2517 mins

# combine & save
EDIT.20250305 <- list(mlras, cf.1,  cf.2, cf.3, cf.4, cf.5, cf.6, cf.7, cf.8, cf.9, cf.10, 
                      cf.11, cf.12, cf.13, cf.14, cf.15, cf.16, cf.17, cf.18, cf.19)
names(EDIT.20250305) <- c("geo-unit-list", ClassFeatures)
saveRDS(EDIT.20250305, "EDIT/EDIT20250305.rds")

#end
