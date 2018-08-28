###############################################################################################
# count_occurrences.r
# Takes a folder with search results and makes summary tables of the frequency of occurrences
# of the techniques within papers and of the terms for each technique.
###############################################################################################
##################  S C R I P T   S E T U P   A N D   U S E R   O P T I O N S #################
###############################################################################################

# Full path of the folder containing the search results.
# Notice that it should use normal slashes (/) and not backslashes (\)
results.folder = "/home/kleber/Dropbox/Scientific Research/Projects/Brazilian Reproducibility Initiative/Fulltext download/Search Results"

###############################################################################################
######### DON'T CHANGE ANYTHING BELOW THIS LINE UNLESS YOU KNOW WHAT YOU ARE DOING ############
###############################################################################################

library(plyr)
library(dplyr)
library(ggplot2)
library(glue)

# Function to count occurences of a technique (the length of the results data frame)
get.count.technique = function (fn) {
  df = read.table(glue("{results.folder}/{fn}"), header = T, sep = ";", stringsAsFactors = F)
  x = length(unique(df$DOI))
  tn = substr(fn,start = 1, stop = nchar(fn)-4)
  return (data.frame(Technique = tn, Count = x))
}

# Function to count occurences of a technique in each year (uses the Web of Science table)
get.count.technique.by.year = function (fn) {
  df = read.table(glue("{results.folder}/WoS/WoS {fn}"), sep = "\t", stringsAsFactors = F,
                  header = T, colClasses = "character", row.names = NULL,
                  skipNul = T, fill = T, blank.lines.skip = T, check.names = F, quote = "")
  
  rdf = df %>% group_by(PY) %>% summarise(Count = n())
  tn = substr(fn,start = 1, stop = nchar(fn)-4)
  rdf = rdf %>% mutate(Technique = tn, Year = as.numeric(gsub(pattern = '\"', replacement = "", x = PY))) %>% select(3,4,2)
  return (rdf)
}

# Functions to create data frames using the count functions above
make.technique.count.df = function () {
  df = ldply(results.list, get.count.technique, .progress = "text")
  return (df)
}

make.technique.count.df.by.year = function () {
  df = ldply(results.list, get.count.technique.by.year, .progress = "text")
  return (df)
}

# Makes a data frame of occurences of specific keywords within each result list
# (a search might contain more than one keyword)
make.keyword.count.df = function (fn) {
  df = read.table(glue("{results.folder}/{fn}"), header = T, sep = ";", stringsAsFactors = F)
  tn = substr(fn,start = 1, stop = nchar(fn)-4)
  s = df %>% group_by(Term) %>% summarise(Count = n()) %>% mutate(Technique = tn) %>% select(3,1,2)
  return (s)
}


# Uses the functions above to create tables with the summaries of occurrences
results.list = list.files(pattern = "csv$", path = results.folder)

output.dir = glue("{results.folder}/Graphs")
dir.create(output.dir, showWarnings = FALSE)

kw.df = ldply(results.list, make.keyword.count.df, .progress = "text")
tech.df = make.technique.count.df()
tech.df.year = make.technique.count.df.by.year()

write.table(x = kw.df, file = glue("{output.dir}/keyword_occurrences.csv"), sep = ";", row.names = F)
write.table(x = tech.df, file = glue("{output.dir}/technique_occurrences.csv"), sep = ";", row.names = F)
write.table(x = tech.df.year, file = glue("{output.dir}/technique_by_year_occurrences.csv"), sep = ";", row.names = F)