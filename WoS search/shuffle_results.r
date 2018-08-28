###############################################################################################
# shuffle_results.r
# Takes a merged file (see merge_wos_results.r) containing results from a Web of Science search
# and generates a new csv file, in a random order, numbered, from which we can sample in order.
###############################################################################################
##################  S C R I P T   S E T U P   A N D   U S E R   O P T I O N S #################
###############################################################################################

# Full path + filename of the file of the full merged table of search results
# Notice that it should use normal slashes (/) and not backslashes (\)
full.results.filename = "/home/kleber/Documents/BRI/wosdf - full.csv"

###############################################################################################
######### DON'T CHANGE ANYTHING BELOW THIS LINE UNLESS YOU KNOW WHAT YOU ARE DOING ############
###############################################################################################

library(dplyr)

# Reads a Web of Science results table
wdata = read.table(full.results.filename, sep = "\t", stringsAsFactors = F, header = T, colClasses = "character",
                        skipNul = T, fill = T, blank.lines.skip = T, check.names = F, quote = "", row.names = NULL)

# Shuffles the rows
shuffle.order = runif(n = nrow(wdata), min = 0, max = 1)
colnames(wdata)[69] = "NA"

wdata = wdata %>% arrange(shuffle.order)

# Saves the shuffled file
output.path = dirname(full.results.filename)
shuffle.filename = paste(output.path, "/shuffledresults - full wos df.csv", sep = "")

write.table(wdata, shuffle.filename, quote = F, row.names = F, fileEncoding = "UTF-8", sep = "\t")
