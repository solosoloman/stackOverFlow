library(RCurl)
library(XML)
library(stringr)


url = 'http://stackoverflow.com/questions/tagged/r?page=1&sort=newest&pagesize=50'

url2 = 'http://stackoverflow.com/questions/tagged/python?page=1&sort=newest&pagesize=50'

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
