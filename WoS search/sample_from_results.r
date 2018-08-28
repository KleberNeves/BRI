###############################################################################################
# sample_from_results.r
# Takes a file containing results from a Web of Science search and returns a sample of the rows
###############################################################################################
##################  S C R I P T   S E T U P   A N D   U S E R   O P T I O N S #################
###############################################################################################

# Full path + filename of the file of the full merged table of search results
# Notice that it should use normal slashes (/) and not backslashes (\)
full.results.filename = "/home/kleber/Documents/BRI/shuffledresults - authorfiltered.csv"

# Full path + filename to export the sample articles
# Notice that it should use normal slashes (/) and not backslashes (\)
sample.filename = "/home/kleber/Documents/BRI/sample 1-1000.csv"

# Specify the rows you want (TRUE) or get a random sample (FALSE)?
specify.rows = TRUE

# IF SPECIFY ROWS
  wanted.rows = 1:1000

# IF RANDOM SAMPLE:
  # How many articles to sample?
  article.sample.size = 100

###############################################################################################
######### DON'T CHANGE ANYTHING BELOW THIS LINE UNLESS YOU KNOW WHAT YOU ARE DOING ############
###############################################################################################

library(dplyr)

# Reads the Web of Science results table
wdata = read.table(full.results.filename, sep = "\t", stringsAsFactors = F, header = T, colClasses = "character",
                        skipNul = T, fill = T, blank.lines.skip = T, check.names = F, quote = "", row.names = NULL)

# Subsets the data frame according to number of rows or random sample
if (specify.rows) {
  sdata = wdata[wanted.rows, ]
} else {
  sdata = wdata %>% sample_n(article.sample.size)
}

# Writes subsetted data frame to disk
f = file(paste(dirname(sample.filename),"/sample_DOIs.txt",sep=""))
writeLines(gsub(pattern = '\"', replacement = "", x = sdata$DI), f)
close(f)

write.table(sdata, sample.filename, quote = F, row.names = F, fileEncoding = "UTF-8", sep = "\t")
