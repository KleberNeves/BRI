###############################################################################################
# get_results_sheet.r
# Takes a folder with search results and delivers one CSV file for each results file, in the
# same folder, with only the WoS results corresponding to the DOIs that match the results.
###############################################################################################
##################  S C R I P T   S E T U P   A N D   U S E R   O P T I O N S #################
###############################################################################################

# Full path of the folder containing the search results.
# Notice that it should use normal slashes (/) and not backslashes (\)
results.folder = "/home/kleber/Dropbox/Scientific Research/Projects/Brazilian Reproducibility Initiative/Fulltext download/Search Results"

# Full path of the folder containing the sample from the Web of Science search.
# Notice that it should use normal slashes (/) and not backslashes (\)
wos.file = "/home/kleber/Dropbox/Scientific Research/Projects/Brazilian Reproducibility Initiative/Fulltext download/sample 1-1000.csv"

###############################################################################################
######### DON'T CHANGE ANYTHING BELOW THIS LINE UNLESS YOU KNOW WHAT YOU ARE DOING ############
###############################################################################################

library(plyr)
library(dplyr)
library(glue)

# Reads the list of results
results.list = list.files(pattern = "csv$", path = results.folder)

# Function to clean DOIs from special characters (PDFs must be saved with the cleaned DOI as the filename)
clean.DOIs = function (dois) {
  dois = gsub(pattern = ".", replacement = "_", x = dois, fixed = T)
  dois = gsub(pattern = "/", replacement = "_", x = dois, fixed = T)
  dois = gsub(pattern = "-", replacement = "_", x = dois, fixed = T)
  dois = gsub(pattern = ";", replacement = "_", x = dois, fixed = T)
  dois = gsub(pattern = ":", replacement = "_", x = dois, fixed = T)
  dois = gsub(pattern = ",", replacement = "_", x = dois, fixed = T)
  return (dois)
}

# Function to extract the list of unique DOIs in the search results 
get.DOIs = function (fn) {
  df = read.table(glue("{results.folder}/{fn}"), header = T, sep = ";", stringsAsFactors = F)
  doi.vec = clean.DOIs(unique(df$DOI))
  return (c(fn, doi.vec))
}

dois.lists = llply(results.list, get.DOIs, .progress = "text")

# Reads the corresponding Web of Science results table
wos.df = read.table(wos.file, sep = "\t", stringsAsFactors = F, header = T, colClasses = "character",
                    skipNul = T, fill = T, blank.lines.skip = T, check.names = F, quote = "", row.names = NULL)

wos.df = wos.df[1:68]
wos.df$DOI = clean.DOIs(wos.df$DI)

# Subsets the table to contain only the DOIs that have matches in the search results
save.subdf = function(doi.list) {
  fn = glue("{results.folder}/WoS/WoS {doi.list[1]}")
  doi.filter = doi.list[2:length(doi.list)]
  sub.df = wos.df %>% filter(DOI %in% doi.filter)
  write.table(sub.df, fn, sep = "\t", row.names = F, quote = FALSE)
}

output.dir = glue("{results.folder}/WoS")
dir.create(output.dir, showWarnings = FALSE)

# Applies function to each list of results and saves
l_ply(dois.lists, save.subdf, .progress = "text")