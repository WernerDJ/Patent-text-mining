ibrary(XML)
library(stringr)
library('rvest')
library(readxl)
# excelfile <- read_excel("~resultList_long.xls", skip = 2) #~should be substituted by the path to your file
names(excelfile)[1]<- "Publication"
###
### The following is a list of words that are included in texts that the function confuses with abstracts
### usually, because what is retracted is a text with between 400 and 1600 characters
### what is obtained is the inventors address list, more rarely papers or conference info 
### modify the list if the abstract could contain any of those words
###
#
Abstract <- vector(mode = "character", length = nrow(excelfile))
for (i in 1:nrow(excelfile)) {
###################
# Start a don't stop-when-error function
##################
#
tryCatch({
#
###################
# Find the patent and input it into memory
##################
#
Free <- "http://www.freepatentsonline.com/"
patent <- as.character(excelfile$Publication[i])
online <- ".html"
Freepatent <- paste(Free, patent, sep ="")
Freepatentonline <- paste(Freepatent, online, sep ="")
webpage <- read_html(Freepatentonline)
results <- webpage %>% html_nodes(".disp_doc2") %>% html_text()
is_in <- grep("Abstract", results)
Abstract[i] <- str_trim(substr(results[is_in],20, nchar(results[is_in])))
###################
# End of jump the stop-when-error function
##################
#
}, error=function(e){})
}
#
# I find quite useful to save the data in csv for later analysis
# write.csv2(data.frame(excelfile[,-ncol(excelfile)], Abstract), "~/abstracts.csv")
