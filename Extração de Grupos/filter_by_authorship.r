###############################################################################################
# filter_by_authorship.r
# Takes a WoS spreadsheet (.csv) and saves new tables with information about authors, like how
# many are located in Brazil and how many groups there are. This is saved in two tables.
###############################################################################################
##################  S C R I P T   S E T U P   A N D   U S E R   O P T I O N S #################
###############################################################################################

# Full path to the folder containing the spreadsheet (a .txt file downloaded from WoS)
# Notice that it should use normal slashes (/) and not backslashes (\)

table.path = "/home/kleber/Documents/BRI/fullsavedresults.csv"

# A preffix to be appended to the output filename

preffix.output = ""

###############################################################################################
######### DON'T CHANGE ANYTHING BELOW THIS LINE UNLESS YOU KNOW WHAT YOU ARE DOING ############
###############################################################################################

library(reshape2)
library(plyr)
library(dplyr)
library(pbapply)
library(data.table)

# Reads the table containing all results
wos.df = read.table(table.path, sep = "\t", stringsAsFactors = F, header = T, colClasses = "character",
                    skipNul = T, fill = T, blank.lines.skip = T, check.names = F, quote = "", row.names = NULL)

# Removes last column then remove quotation marks
# This is necessary for reasons unknown (probably has to do with the encoding of the
# data frame strings when saving the file)
a = colnames(wos.df)[1:69]
a = gsub(pattern = '\"', replacement = "", x = a)
colnames(wos.df) = a
rm(a)

# Function to reverse a string and return it
rev.str.col = function (a.col) {
  return (sapply(strsplit(a.col, ""), function(i) paste(rev(i), collapse="")))
}

# Function to extract and return a data frame of all authors and respective affiliations
# associated with one article
# This function is tailored to the specific format for the Authors field (C1) in the Web of
# Science results.
get.affiliation.df = function (papers.row) {
  aff.str = papers.row$C1
  
  pairs = strsplit(aff.str, split = "[[]")
  if (length(pairs[[1]]) > 1) {
    pairs = pairs[[1]][-1]
    
    affs = lapply(pairs, strsplit, split = "]")
    
    get.aff.rows = function (aff) {
      authors = strsplit(aff[[1]][[1]],"; ")[[1]]
      country = strsplit(aff[[1]][[2]],", ")[[1]]
      country = country[[length(country)]]
      country = sub(pattern = "; ", x = country, replacement = "")
      if (grepl("USA",country)) {
        country = "USA"
      }
      df = expand.grid(authors, country)
      colnames(df) = c("Author","Country")
      return (df)
    }
    
    aff.df = do.call(rbind, lapply(affs, get.aff.rows))
    
  } else {
    
    authors = strsplit(papers.row$AF[[1]],"; ")[[1]]
    authors = gsub(pattern = '\"', replacement = "", x = authors)
    
    country = strsplit(pairs[[1]],", ")[[1]]
    country = country[[length(country)]]
    country = gsub(pattern = "; ", x = country, replacement = "")
    country = gsub(pattern = '\"', x = country, replacement = "")
    if (grepl("USA",country)) {
      country = "USA"
    }
    
    aff.df = expand.grid(authors, country)
    colnames(aff.df) = c("Author","Country")
    
  }
  
  return (aff.df)
  
}

# Function to get the country of affiliation of an author (depends on get.affiliation.df)
get.author.country = function (an.author, doi) {
  i <<- i + 1
  if (i %% 100 == 0) {
    cat(i)
    cat("\n")
  }

  paper.aff = affiliations %>% filter(DI == doi)
  paper.aff = paper.aff %>% filter(Author == an.author)
  this.aff = as.character(unique(paper.aff$Country))
  return (this.aff)
}

# Function to get the number of authors in a paper
# Uses data.table (faster)
get.author.number.dt = function (doi) {
  i <<- i + 1
  if (i %% 100 == 0) {
    cat(i)
    cat("\n")
  }
  paper.aff = affiliations.dt[DI == doi]
  a = length(unique(paper.aff$Author))
  paper.aff = paper.aff[Country == "Brazil"]
  b = length(unique(paper.aff$Author))
  return (data.frame(doi = doi, total = a, br = b))
}

# Function to get the number of authors in a paper
# Uses dplyr (slower)
get.author.number = function (doi) {
  i <<- i + 1
  if (i %% 100 == 0) {
    cat(i)
    cat("\n")
  }
  paper.aff = affiliations %>% filter(DI == doi)
  a = length(unique(paper.aff$Author))
  paper.aff = paper.aff %>% filter(Country == "Brazil")
  b = length(unique(paper.aff$Author))
  return (data.frame(doi = doi, total = a, br = b))
}

# Cleaning DOI field from quotations and removing missing DOIs
wos.df$DI = gsub(pattern = '\"', replacement = "", x = wos.df$DI)
wos.df = wos.df %>% filter(DI != "")

# Extract affiliations as structured data for each paper
print("Extraindo afiliações ...")
affiliations = ddply(wos.df, "DI", get.affiliation.df, .progress = "text")
affiliations$Country = gsub(pattern = '\"', replacement = "", x = affiliations$Country)
affiliations$DI = gsub(pattern = '\"', replacement = "", x = affiliations$DI)

# Extracts info about the last author
wos.df$last.author = rev.str.col(sub(pattern = " ;.*", x = rev.str.col(wos.df$AF), replacement = ""))
wos.df$last.author = gsub(pattern = '\"', replacement = "", x = wos.df$last.author)

# Extracts information about the last author's country
print("Extraindo afiliações dos últimos autores ...")
i = 0
wos.df$last.author.country = mapply(get.author.country, wos.df$last.author, wos.df$DI)
wos.df = wos.df %>% mutate(last.br = (last.author.country == "Brazil"))

# Extracts information on the number of authors, and how many have Brazilian affiliation
print("Calculando número de autores ...")
wos.df$DI = gsub(pattern = '\"', replacement = "", x = wos.df$DI)

affiliations.dt = data.table(affiliations)
total.authors = affiliations.dt[, .(total = .N), by = DI]
br.authors = affiliations.dt[Country == "Brazil", .(br = .N), by = DI]
doi.df = data.frame(DI = wos.df$DI)
doi.df = merge(doi.df, total.authors, all.x = T, by = "DI")
doi.df = merge(doi.df, br.authors, all.x = T, by = "DI")
doi.df[which(is.na(doi.df$total)),"total"] = 0
doi.df[which(is.na(doi.df$br)),"br"] = 0

wos.df$author.number = doi.df$total
wos.df$br.author.number = doi.df$br
rm(doi.df)

print("Calculando número de autores com afiliação brasileira ...")
wos.df$perc.br.authors = as.numeric(wos.df$br.author.number) / as.numeric(wos.df$author.number)
wos.df$half.br = (wos.df$perc.br.authors >= 0.5)

output.path = dirname(table.path)

# Creates CSV file with only the DOIs to be included
include.df = wos.df %>% filter(half.br, last.br)
include.df = include.df[1:69]
write.table(include.df, paste(output.path, "/", preffix.output, "authorfilteredresults.csv", sep = ""),
            sep = "\t", row.names = F, quote = FALSE)
