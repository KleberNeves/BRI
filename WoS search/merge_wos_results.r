###############################################################################################
# merge_wos_results.r
# Takes an input folder containing Web of Science search results and merge all the .txt files into a single .csv file
###############################################################################################
##################  S C R I P T   S E T U P   A N D   U S E R   O P T I O N S #################
###############################################################################################

# Full path to the folder containing the search results (all the different .txt files downloaded from WoS)
# Notice that it should use normal slashes (/) and not backslashes (\)

results.folder = "/home/kleber/Documents/BRI/WoS UTF8"


###############################################################################################
######### DON'T CHANGE ANYTHING BELOW THIS LINE UNLESS YOU KNOW WHAT YOU ARE DOING ############
###############################################################################################

setwd(results.folder)

library(readr)
library(pbapply)

# Reads a csv file as downloaded from Web of Science search results
read.wos.table = function(fn) {
  
  cat(basename(fn))
  cat("\n")
  
  df = read.table(fn, sep = "\t", stringsAsFactors = F, header = T, colClasses = "character", skipNul = T,
                  fill = T, blank.lines.skip = T, check.names = F, quote = "", row.names = NULL)
  
  colnames(df) = c("PT", "AU", "BA", "BE", "GP", "AF", "BF", "CA", "TI", "SO", "SE", "BS", "LA", "DT", "CT", "CY", "CL",
                   "SP", "HO", "DE", "ID", "AB", "C1", "RP", "EM", "RI", "OI", "FU", "FX", "CR", "NR", "TC", "Z9", "U1",
                   "U2", "PU", "PI", "PA", "SN", "EI", "BN", "J9", "JI", "PD", "PY", "VL", "IS", "PN", "SU", "SI", "MA",
                   "BP", "EP", "AR", "DI", "D2", "EA", "EY", "PG", "WC", "SC", "GA", "UT", "PM", "OA", "HC", "HP", "DA")
  
  return (df)
}

# Reads all files, then bind them in one data frame and saves to disk
file.list = list.files(pattern = "txt$", path = results.folder)
k = pblapply(file.list, read.wos.table)
full.df = do.call(rbind, k)

write.table(full.df, "fullsavedresults.csv", row.names = F, fileEncoding = "UTF-8", sep = "\t")
