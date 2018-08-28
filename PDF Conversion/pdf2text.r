###############################################################################################
# pdf2text.r
# Takes a folder and converts all PDFs in the folder to txt files in a subfolder.
###############################################################################################
##################  S C R I P T   S E T U P   A N D   U S E R   O P T I O N S #################
###############################################################################################

# Full path + filename of the folder containing the PDFs.
# Notice that it should use normal slashes (/) and not backslashes (\)
pdf.folder = "/home/kleber/Dropbox/Brazilian Reproducibility Initiative/Revisão Sistemática Inicial - Fulltext/Amostra WoS 2/Todos"

###############################################################################################
######### DON'T CHANGE ANYTHING BELOW THIS LINE UNLESS YOU KNOW WHAT YOU ARE DOING ############
###############################################################################################

library(pbapply)
library(tools)

# Creates folder to store the text files
output.dir = paste(pdf.folder, "/Text", sep = "")
dir.create(output.dir, showWarnings = FALSE)

# Function to make an external call to pdftotext
convert.pdf2txt = function (fn) {
  origin = paste('\"', pdf.folder, "/", fn, '\"', sep = "")
  dest = paste('\"', output.dir, "/", file_path_sans_ext(fn), '.txt\"', sep = "")
  system2("pdftotext", args = c(origin, dest))
}

# Lists the files and applies the conversion function to each one
pdf.list = list.files(pattern = "pdf$", path = pdf.folder)
pblapply(pdf.list, convert.pdf2txt)
