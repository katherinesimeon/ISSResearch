## Load Text Data
setwd('/Users/katherinesimeon/Documents/GitHub/ISSResearch/data/ISSPubs_txt/')

filelist <- list.files(path="/Users/katherinesimeon/Documents/GitHub/ISSResearch/data/ISSPubs_txt",all.files=TRUE)

lapply(filelist, FUN=txt, header=TRUE)
