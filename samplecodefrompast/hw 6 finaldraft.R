#Soloman Wong
#ID:999042564




#who posted it,
#get user profile url
library(RCurl)
library(XML)
library(stringr)

url = 'http://stackoverflow.com/questions/tagged/r?page=1&sort=newest&pagesize=50'

#below are multiple function that can help getting information wanted from the summary
poster = function(html){
  
        path = "//div[@class = 'user-details']"
        nodes = getNodeSet(html,path)
        posters = xmlSApply(nodes,function(x)xmlValue(xmlChildren(x)$a))
        #reference from post 1468 in piazza
}

timer= function(html){
        #for date xmlGetAttr
        #when it was post
        timepath = "//div[@class = 'user-action-time']/span/@title"
        timer = xpathSApply(html, timepath, `[[`, 1)
}



#title of the post
title= function(html){
        titlepath = "//a[@class = 'question-hyperlink']/text()"
        nodes = getNodeSet(html,titlepath)
        titler = sapply(nodes, xmlValue)
  
}

#score/reputation
score = function(html){
        reputationscore = "//div[@class = '-flair']"
        nodes = getNodeSet(html,reputationscore)
        score = xmlSApply(nodes,function(x)xmlValue(xmlChildren(x)$span))
}


#view
view = function(html){
        viewpath = "//div[starts-with(@class, 'views ')]/@title"
        viewers = xpathSApply(html, viewpath, `[[`, 1)
        viewers2= gsub(" views","",viewers)
}


#number answer
answer= function(html){
        numberanswer = "//div[@class = 'status unanswered' or 'status answered']/strong"
        nodes = getNodeSet(html,numberanswer)
        answers = sapply(nodes, xmlValue)
}

#vote
vote = function(html){
        vote = "//span[@class = 'vote-count-post ']/strong"
        nodes = getNodeSet(html,vote)
        voters = sapply(nodes, xmlValue)
}



#url for the page
urlpage= function(html){
        url = "//div[@class = 'summary']/h3/a/@href"
        urllink = xpathSApply(html, url, `[[`, 1)
        urllink2= paste("http://stackoverflow.com",urllink,sep = "")
}

#id
id = function(html){
        id = "//div[@class='question-summary']/@id"
        id2 = xpathSApply(html, id, `[[`, 1)
        id3 = gsub("question-summary-", "", id2)
}

#tags
tags= function(html){
          tags = "//div[starts-with(@class, 'tags')]"
          tags2 = getNodeSet(html,tags)
          tags3= sapply(tags2,xmlValue)
          regenx = "[:alpha:]+[([:space:]{1})([:graph:]+)]{0,}([:alpha:]){1}"
          tags5 = str_extract_all(tags3, regenx)
          tags6 = sapply(tags5,`[`,1)
          tags7 = gsub(" ", "; ", tags6)
}




page= function(url){
          doc= getURLContent(url)
          html = htmlParse(doc, asText =TRUE)
  
          #obtain variables from the post
          
          #who postit
          whopost= poster(html)
          
          
          #when it was post
          whenpost = timer(html)
          
          #title of the post
          titles = title(html)
          
          #reputation level
          reputationposter = score(html)
          
          #numbers of views
          numview = view(html)
          
          #numbers of answers
          numans = answer(html)
          
          #vote 'score'
          votescore = vote(html)
          
          #url page
          urlpages = urlpage(html)
          
          #unique id
          uniqueid = id(html)
          
          #tags of post
          tagss = tags(html)
          
          #combine all variables into a single frame
          
          frame = data.frame(id = uniqueid, date = whenpost, tags = tagss, 
                             title = titles,
                             url = urlpages, views = numview, votes = votescore,
                             answer = numans, users = whopost, 
                             reputation = reputationposter,stringsAsFactors=FALSE)
         #during the degbugging step, I find out there are some NA in tags column
         #because the function cannot take in tags that only have 1 character
         #so I chagge all the NA into r and r is the only tags that have 1 character
          frame[["tags"]][is.na(frame[["tags"]])] <- "r"
          return (frame)
}

#get frame for current page
frame = page(url)

#use a for loop to extract information from other page
#put the information to frame that have the info of first page
#create a frame that contains the first 180 pages

for(i in 2:(180)){
        #get the url, content for the next page
        a = "//a[@rel = 'next']/@href"
        doc= getURLContent(url)
        html = htmlParse(doc, asText =TRUE)
        otherpage = xpathSApply(html, a, `[[`, 1)
        url = paste("http://stackoverflow.com",otherpage,sep = "")
        
        
        
        #frame
        
        frame2 = page(url)
        frame = rbind(frame,frame2)
        i=i+1
      }



#part3

rqa <- load("C:/Users/Soloman/Desktop/uc davis/2015 fall quarter/sta 141/hw6/rQAs.rda")

#1
#get rows that have only "answer" in type from dataframe

rqaanswer = subset(rQAs, rQAs$type == "answer")

#calculate the count of questions that every person answer
countrqaanswer = table(rqaanswer$user)
#check the frequency of questions count answered per person
numberfrequency = table(countrqaanswer)

#find the range of numbers of question that people answer
min(countrqaanswer)
max(countrqaanswer)




#create a plot to show the distribution
plot(numberfrequency, main = "frequency of questions that people answer", 
     xlab = "number of questions each person answers", 
     ylab = "Frequency")
#2
#use the frame to answer this question

tagsofpost = frame$tags
#take every tags out of evey posts
everytags = strsplit(tagsofpost, "; ")
totaltags = unlist(everytags)
#calculate sum of every tags
sumtags = table(totaltags)
#find the most frequent tags
ordertags = sort(sumtags, decreasing = TRUE)
ordertags[1:3]


#3
rqaquestion = subset(rQAs, rQAs$type == "question")
text= rqaquestion$text
regenx = "ggplot"
text2 = gregexpr(regenx, text, perl=TRUE, ignore.case=TRUE)
text3 = regmatches(text, text2, invert = FALSE)
text4 = sapply(text3,function(i){if (length(i)>0)TRUE else NA})
table(text4)
#there are 959 posts include word "ggplot" out of 9000 posts



#4

text= rQAs$text
#terms that we need to find text column
regenx = "(xml|html|web scrapping)"
text2 = gregexpr(regenx, text, perl=TRUE, ignore.case=TRUE)
text3 = regmatches(text, text2, invert = FALSE)
#set text 4 = True if the post involve with regenx and NA for post that does not
text4 = sapply(text3,function(i){if (length(i)>0)TRUE else NA})
#create a data frame that have qid and text4
text5 = data.frame(qid = rQAs$qid, involve= text4)
#extract the post that have regenx which include both comment, question, and anwswer
text6 = subset(text5, text5$involve == TRUE)
#calculating the total post that involve with with terms based on unique id
uniqueqid = length(unique(text6$qid))
#there are 1327 questions involved with xml,html, and webscrapping




#5

regexxxx = "[a-zA-Z]+\\("
title = frame$title
#find out function that follow format above
functionname4 = gregexpr(regexxxx, title, perl=TRUE, ignore.case=TRUE)
functionname5 = regmatches(title, functionname4, invert = FALSE)

#claculate frequency of each function
functionname6 = sapply(functionname5,function(i){if (length(i)>0)i else NA})
numberfunction = table(unlist(functionname6))
numberfunction2 = sort(numberfunction, decreasing = TRUE)


#6 
#set accepted answers and comments as score is greater than or equal to 0

answercomment = subset(rQAs, rQAs$type != 'question' & rQAs$score >= 0)
regexxxx = "[a-zA-Z]+\\("
#match the text with regenxxxx to find the comment
text = answercomment$text
text2 = gregexpr(regexxxx, text, perl=TRUE, ignore.case=TRUE)
text3 = regmatches(text, text2, invert = FALSE)
text4 = sapply(text3,function(i){if (length(i)>0)i else NA})
#check the number of each elements and calculate the counts and the most frequent one
numbertext = table(unlist(text4))
numbertext2 = sort(numbertext, decreasing = TRUE)
numbertext2[1:10]
