###############################################################################################
# get_fulltext.r
# Takes a list of DOIs in a text file and tries to download all of the papers.
###############################################################################################
##################  S C R I P T   S E T U P   A N D   U S E R   O P T I O N S #################
###############################################################################################

# Full path + filename of the file containing the DOIs (one DOI per line).
# Notice that it should use normal slashes (/) and not backslashes (\)
doi.list.file = "/home/kleber/Documents/BRI/leftover DOIs0.txt"

output.dir = "/home/kleber/Documents/BRI/Downloaded Papers"

###############################################################################################
######### DON'T CHANGE ANYTHING BELOW THIS LINE UNLESS YOU KNOW WHAT YOU ARE DOING ############
###############################################################################################

library(fulltext)
library(plyr)
library(dplyr)

# Function that uses the fulltext package to download the PDF of paper, given a DOI number
get.fulltext = function(article.doi) {
  cat("\n")
  cat(article.doi)
  result = try(ft_get(article.doi, type = "pdf"), silent = F)
  if ( !("try-error" %in% class(result)) & length(result) > 0) {
    dl.path = result[[1]]$data$path[[1]]$path[[1]]
    if ( !is.null(dl.path) ) {
      if ( !grepl(pattern = "xml$", x = dl.path) ) {
        cp.path = paste(output.dir, "/", basename(dl.path), sep = "")
        file.rename(from = dl.path,  to = cp.path)  
        cat(" - Successful\n")
        df[i,"DOI"] <<- article.doi
        df[i,"status"] <<- "Downloaded"
      } else {
        cat(" - Failed\n")
        df[i,"DOI"] <<- article.doi
        df[i,"status"] <<- "Failed"
      }
    } else {
      cat(" - Failed\n")
      df[i,"DOI"] <<- article.doi
      df[i,"status"] <<- "Failed"
    }
  } else {
    cat(" - Failed\n")
    df[i,"DOI"] <<- article.doi
    df[i,"status"] <<- "Failed"
  }
  i <<- i + 1
}

# Reads DOI list and prepares a dataframe for tracking success
list.of.dois = readLines(doi.list.file)

n = length(list.of.dois)
df = data.frame(DOI = character(n), status = character(n))
df$DOI = as.character(df$DOI)
df$status = as.character(df$status)

dir.create(output.dir, showWarnings = FALSE)

# Attempts to download all papers while keeping track of the ones that failed
# to be downloaded manually afterwards
i = 1
l_ply(list.of.dois, get.fulltext, .progress = "text")

leftovers = unique((df %>% filter(status == "Failed"))$DOI)

output.fn = paste(dirname(doi.list.file), "/leftover DOIs.txt", sep = "")
f = file(output.fn)
writeLines(leftovers, f)
close(f)

output.fn = paste(dirname(doi.list.file), "/fulltext_log.csv", sep = "")
write.table(x = df, file = output.fn, sep = ";", row.names = F)
