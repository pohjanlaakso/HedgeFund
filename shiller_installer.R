
# packages
library(stringr)
library(readxl)

# # https://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
url <- 'https://img1.wsimg.com/blobby/go/e5e77e0b-59d1-44d9-ab25-4763ac982e53/downloads/423556dc-a15c-4714-b32e-de9e2cb8173d/ie_data.xls?ver=1735912576408'
temp <- tempfile()
download.file(url, temp)
data <- read_excel(temp, sheet = 5, skip = 7)

# visual checks
plot(data$P, type = 'l')
plot(data$E, type = 'l')
plot(data$D, type = 'l')
plot(data$CAPE, type = 'l'); 
plot(1/as.numeric(data$CAPE), type = 'l') # inverse CAPE, or CAPE yield



