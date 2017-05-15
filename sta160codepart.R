library(stringr)

pythonNoAnswer <- read.csv("E:/sta160/pythonNoAnswer.csv")
rNoAnswer <- read.csv("E:/sta160/
#find the most frequent tags

pythontags = pythonNoAnswer['tags']
pythontags = as.character(unlist(pythontags))

#separate all the tag
sepython = unlist(sapply(pythontags, function(x) strsplit(x,"\\|")))
#calculate the total of each tags
bc = table(sepython)
sort(bc, decreasing = TRUE)[1:5]



rtags = rNoAnswer['tags']
rtags = as.character(unlist(rtags))
sepr = unlist(sapply(rtags, function(x) strsplit(x,"\\|")))
ab = table(sepr)
sort(ab, decreasing = TRUE)[1:5]


#checking correlation between number of tags and views(in progress)
rtags = rNoAnswer['tags']
rtags = as.character(unlist(rtags))
sepr = lapply(rtags, function(x) strsplit(x,"\\|"))
rcountoftags = lapply(sepr, function(x)length(unlist(x)))
rviews = rNoAnswer['view_count']

plot(rcountoftags),unlist(rviews))
