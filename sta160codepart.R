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

combine = cbind(rviews,rcountoftags)
mean(combine[combine["rcountoftags"] ==1])
#62
mean(combine[combine["rcountoftags"] ==2])
#81
mean(combine[combine["rcountoftags"] ==3])
#78
mean(combine[combine["rcountoftags"] ==4])
#88
mean(combine[combine["rcountoftags"] ==5])
#91

#people views post more when there are more tags

#for python
pythontags = pythonNoAnswer['tags']
pythontags = as.character(unlist(pythontags))
sepython = lapply(pythontags, function(x) strsplit(x,"\\|"))
pythoncountoftags = sapply(sepython, function(x)length(unlist(x)))
pythonviews = pythonNoAnswer['view_count']
combinepy = cbind(pythonviews,pythoncountoftags)
mean(combinepy[combinepy["pythoncountoftags"] ==1])
#39
mean(combinepy[combinepy["pythoncountoftags"] ==2])
#63
mean(combinepy[combinepy["pythoncountoftags"] ==3])
#77
mean(combinepy[combinepy["pythoncountoftags"] ==4])
#83
mean(combinepy[combinepy["pythoncountoftags"] ==5])
#88

#investigating user id:
ruserid = table(rNoAnswer["owner_user_id"])
topruser = sort(ruserid, decreasing = TRUE)[1:5]
names(topruser)[1]
#compare with tags
na.omit(rNoAnswer["tags"][rNoAnswer["owner_user_id"] == names(topruser)[1]])
#compare with scores
na.omit(rNoAnswer["score"][rNoAnswer["owner_user_id"] == names(topruser)[1]])
