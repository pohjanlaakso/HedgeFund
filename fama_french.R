
# ken french installer
library(stringr)
url <- 'https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip'

# extract data
# idea based on this: https://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
temp <- tempfile()
download.file(url, temp)

# possible solution: https://stackoverflow.com/questions/29749734/assign-error-statement-to-variable-in-r
n_rows <- 0
res <- try(data <- read.table(unz(temp, 'F-F_Research_Data_Factors.csv'), sep=",", header=T, skip=3))
if(inherits(res, 'try-error')) {
  sentence <- print(conditionMessage(attr(res, 'condition')))
  n_rows <- as.numeric(word(sentence, 2))
}
n_rows <- n_rows - 2 # this is the right number of rows before it reaches the problematic part in the .csv-file

# finish the download 
data <- read.table(unz(temp, 'F-F_Research_Data_Factors.CSV'), sep=",", header=T, skip=3, nrows=n_rows) # throwns an error message
unlink(temp)
