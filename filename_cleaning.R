library(dplyr)
library(stringr)

file_names <- data.frame(list.files(path="/Users/katherinesimeon/Documents/GitHub/ISSResearch/data/ISSPubs_txt",all.files=TRUE))
dim(file_names)
file_names <- data.frame(file_names[3:1835,])


file_list <- data.frame(lapply(file_names, gsub, pattern='.txt$', replacement=''))
file_list

file_list_new <- data.frame(do.call('rbind', strsplit(as.character(file_list[,1]),'_[0-9]{4}.pdf$')))

years <- data.frame(str_extract(as.character(file_list[,1]),pattern= "[0-9]{4}(?=\\.pdf)"))
files <- data.frame(cbind(file_list_new,years))
filenames <- files

colnames(filenames) <- c("name","year")
head(filenames)

no_NAs <- subset(filenames,!is.na(filenames$year))

# view NAs
year_na <- subset(filenames,is.na(filenames$year))
dim(subset(filenames,is.na(filenames$year)))
year_na <- data.frame(lapply(year_na, gsub, pattern='.pdf$', replacement=''))


y_na <- data.frame(str_extract(as.character(year_na[,1]),pattern= "_2[0-9]{3}"))
y_na <- data.frame(lapply(y_na, gsub, pattern='^_', replacement=''))

years_with_NA <- cbind(year_na[,1],y_na)
colnames(years_with_NA) <- c("name","year")

files_full <- rbind(no_NAs,years_with_NA)

files_full
