library(stringr)

pythonNoAnswer <- read.csv("E:/sta160/pythonNoAnswer.csv")
rNoAnswer <- read.csv("E:/sta160/
#find the most frequent tags

pythontags = pythonNoAnswer['tags']
pythontags = as.character(unlist(pythontags))
sepython = unlist(sapply(pythontags, function(x) strsplit(x,"\\|")))
bc = table(sepython)
sort(bc, decreasing = TRUE)[1:5]



rtags = rNoAnswer['tags']
rtags = as.character(unlist(rtags))
sepr = unlist(sapply(rtags, function(x) strsplit(x,"\\|")))
ab = table(sepr)
sort(ab, decreasing = TRUE)[1:5]
