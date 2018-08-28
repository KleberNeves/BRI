###############################################################################################
# search_text_files.r
# Takes a folder and a list of strings and searches for each one of them in the files contained
# in the folder. Only works for text files (TXT).
###############################################################################################
##################  S C R I P T   S E T U P   A N D   U S E R   O P T I O N S #################
###############################################################################################

# Full path + filename of the folder containing the text files.
# Notice that it should use normal slashes (/) and not backslashes (\)
text.folder = "/home/kleber/Dropbox/Scientific Research/Projects/Brazilian Reproducibility Initiative/Fulltext download/TXTs"

# Search terms, as an R character vector
search.terms = c(
  "Western Blot",
  "Immunoblot"
)

###############################################################################################
######### DON'T CHANGE ANYTHING BELOW THIS LINE UNLESS YOU KNOW WHAT YOU ARE DOING ############
###############################################################################################

library(plyr)
library(dplyr)

output.dir = paste(text.folder, "/../Search Results", sep = "")
dir.create(output.dir, showWarnings = FALSE)

# Function to make an external call to grep to search all files for matches with the keywords
search.string = function (sterm) {
  command = paste("grep -rnw \"", text.folder, "\" -i -e ", '\"', sterm, '\"', sep = "")
  grep.results = system(command, intern = T)
  break.result = function (res) {
    parts = strsplit(res, ":", fixed = T)[[1]]
    doi = tools::file_path_sans_ext(basename(parts[[1]][1]))
    ocline = parts[2]
    context = parts[3]
    d = data.frame(DOI = doi, Line = ocline, Context = context, Term = sterm)
    return (d)
  }
  if (length(grep.results) > 0) {
    search.results = ldply(grep.results, break.result)
  } else {
    search.results = data.frame(DOI = "", Line = 0, Context = "", Term = "")
  }
  return (search.results)
}

# Calls the search function for each term and aggregates results
final.results = ldply(search.terms, search.string, .progress = "text") %>%
  filter(Line != 0 | is.na(Line))

final.results$Context = gsub(x = final.results$Context, pattern = '\"', replacement = "")
final.results$Context = gsub(x = final.results$Context, pattern = "\'", replacement = "")

write.table(x = final.results, file = paste(output.dir, "/", search.terms[1], ".csv", sep = ""),
            row.names = F, sep = ";")
