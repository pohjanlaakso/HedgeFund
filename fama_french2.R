


# ken french installer 2
library(stringr)
url <- 'https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip'


download.file(url, destfile = "fama_french_data.zip", mode = "wb")

# 2. Unzip the file
unzip("fama_french_data.zip", exdir = "data_folder") # Extracts to a new folder named 'data_folder'

# 3. Read the CSV from the unzipped location
data <- read.csv("data_folder/F-F_Research_Data_Factors.CSV", skip = 3)
sapply(data, class)

RF <- as.numeric(data$RF)[1:1185]
plot.ts(RF)
