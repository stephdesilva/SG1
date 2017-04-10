####################################################
# Title:SG1 shiny app
# Author: Steph de Silva.
# Email: steph@rex-analytics.com
# Date created: 07/04/17
# Date last altered: 10/04/17
# Attributions and acknowledgment of derivation:
# This script is due to information, tips and advice given in:
### Julia Silge is amazing: http://juliasilge.com/blog/Life-Changing-Magic/
## Alot of this is directly due to her work
# (1)  http://www.rdatamining.com/examples/text-mining
#     Accessed: 21/05/16
# Along with helpful code fixes and tweaks from:
# (1)  http://stackoverflow.com/questions/25069798/r-tm-in-mclapplycontentx-fun-all-scheduled-cores-encountered-errors
# (2)  http://www.inside-r.org/packages/cran/tm/docs/as.TermDocumentMatrix
# (3)  https://stat.ethz.ch/pipermail/r-help/2012-May/313013.html
# (4)  http://stackoverflow.com/questions/29358571/termdocumentmatrix-raises-error-in-r
# All accessed 21/05/16
# Also: 
# https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
# Handy tute link: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know#text-transformation
# Also colour and font handling as per:  http://www.r-bloggers.com/word-cloud-in-r/
# Adding a title to the wordcloud fix from here: http://stackoverflow.com/questions/15224913/r-add-title-to-wordcloud-graphics-png
### locations useful info: http://statistics.berkeley.edu/computing/r-reading-webpages
## fix from: https://www.r-bloggers.com/reading-html-pages-in-r-for-text-processing/
# Removing bits before or after an expression: http://stackoverflow.com/questions/12297859/find-and-replace-characters-before
# https://stevencarlislewalker.wordpress.com/2013/02/13/remove-or-replace-everything-before-or-after-a-specified-character-in-r-strings/
# Fix from: http://stackoverflow.com/questions/29358571/termdocumentmatrix-raises-error-in-r

#
#
# Purpose: This script is intended build the server for an SG1 explorer app.
#########################################################################
# Data Used: Screenplays of SG1 from IMSDB
# Source: http://www.imsdb.com/transcripts/Stargate-SG1-Children-Of-The-Gods.html
# Specifically:
# Translation by:
# Date Accessed: Accessed at time of run
# Gutenberg Number: NA
#########################################################################
# Script Outline:
# 1. Load Libraries, 
# 2. Create Server
#########################################################################
# 1. Load libraries, load data
#########################################################################
rm(list=ls(all=TRUE))

# The Server
library(shinydashboard)
library(shiny)
library(tidytext)
library(tm)
library(XML)
library(xlsx)
library(NLP)
library(SnowballC)
library(rJava)
library(RWekajars)
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(stringr)
library(tidytext)
library(dplyr)
library(tidyr)
library(viridis)
library(igraph)
library(ggraph)
set.seed(4567)
# setwd("~/Documents/Rex Analytics/Blog/SG1 screenplay/SG1 app")
Locations<- read.xlsx("Locations.xlsx", sheetName="Sheet1", startIndex=1, startRow=1, as.data.frame=TRUE, header=TRUE)
Locations$Title<-as.character(Locations$Title)
Locations$URL <-as.character(Locations$URL)
n = nrow(Locations) # episode numbers

colour.vec <- c( "Greens", "BuGn", "GnBu","Blues", "BuPu", "PuBu", "Purples", "PuRd", "Reds")
colour_vec_solid<-c("darkgreen", "darkturquoise", "steelblue4", "skyblue4", "mediumpurple4", "orchid4", "plum4", "violetred4", "red4")
# big.corpus <- as.data.frame(Locations)
# big.corpus$Text <- NA
# 
# for (i in 1:n){
#   
#   title = Locations[i,3]
#   season <-Locations[i,2]
#   colours <- colour.vec[season]
#   #destfile1=paste(cname,"/",title, ".html", sep="")
#   # script <- download.file(url=Locations[i,4], destfile=destfile1)
#   ## fix from: https://www.r-bloggers.com/reading-html-pages-in-r-for-text-processing/
#   script.html<- htmlTreeParse(Locations[i,4],
#                               useInternal = TRUE)
#   script.text <-  unlist(xpathApply(script.html, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue))
#   script.text = gsub('\\n', ' ', script.text) ### Fix from https://tonybreyal.wordpress.com/2011/11/18/htmltotext-extracting-text-from-html-via-xpath/
#   script.text = paste(script.text, collapse = ' ')
#   # start.location <- grep("STARGATE SG1", script.text)
#   #  end.location <- grep("Writers :", script.text)
#   SG1source <- as.list(script.text)
#   SG1source <- str_replace_all(SG1source, "Â", "")
#   SG1source <- str_replace_all(SG1source, "\r", "")
#   SG1source <- str_replace_all(SG1source, "\t", "")
#   SG1source <- str_replace_all(SG1source, "\r", "")
#   
#   # Removing bits before or after an expression: http://stackoverflow.com/questions/12297859/find-and-replace-characters-before
#   SG1source <- gsub(".*STARGATE SG1","",SG1source)
#   SG1source <- sub('THE END.*', '', SG1source) # https://stevencarlislewalker.wordpress.com/2013/02/13/remove-or-replace-everything-before-or-after-a-specified-character-in-r-strings/
#   
#   big.corpus$Text[i] <- SG1source
# }    
# save(big.corpus, file="SG1corpus.RData")

load("SG1corpus.RData")

tidy_SG1<- unnest_tokens(big.corpus, word, Text)

# let's tidy this up!
data("stop_words")
tidy_SG1 <- tidy_SG1%>%
  anti_join(stop_words)

tidy_SG1.episode<-tidy_SG1
tidy_SG1.series <- tidy_SG1

n.series <-length(unique(big.corpus$Series))

tidy_SG1.series$Index.Series <- NA
len <- vector(mode = "numeric", length = n.series)
for (i in 1:n.series){
  x <- subset(tidy_SG1.series, tidy_SG1.series$number.series==i)
  len[i] <- nrow(x)
}
len.present <-1
tidy_SG1.series$Index.Series[1:len[1]]=seq(from=1,to=len[1],by=1)
for (i in 2:n.series){
  len.present= len.present +len[i]
  A <- seq(1, len[i], 1)
  tidy_SG1.series$Index.Series[len.present:(len.present+len[i]-1)]=A
}

###

tidy_SG1.episode$Index.Episode <- NA
len <- vector(mode = "numeric", length = n)
for (i in 1:n){
  x <- subset(tidy_SG1.episode, tidy_SG1.episode$Number==i)
  len[i] <- nrow(x)
}
len.present = 1
tidy_SG1.episode$Index.Episode[1:len[1]]=seq(from=1,to=len[1],by=1)
for (i in 2:n){
  len.present = len.present +len[i]
  A <- seq(1, len[i], 1)
  tidy_SG1.episode[len.present:(len.present+len[i]-1),8]=A
}
##

bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(-score)

SG1.sentiment.episode <- tidy_SG1.episode %>%
  inner_join(bing) %>% 
  count(Number, index = Index.Episode%/%40, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

SG1.sentiment.season <- tidy_SG1.series %>%
  inner_join(bing) %>% 
  count(number.series, index = Index.Series%/%300, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
#########################################################################
# 3. Server Construction
#########################################################################


server <- function(input, output) {
  
  
  output$third_plot <- renderPlot({
    pos <- grep(input$title, Locations$Title)
    title = Locations[pos,3]
    season <-Locations[pos,5]
    colours1 <- colour.vec[season]
    colours2 <- colour_vec_solid[season]
    ggdata <- as.data.frame(subset(SG1.sentiment.episode, SG1.sentiment.episode$Number==pos))
    
    ggplot(ggdata)+
      geom_bar(aes(index, sentiment), stat = "identity", fill=NA, colour=colours2)+
      theme_minimal(base_size=13)+ 
      scale_x_discrete(expand=c(0.02,0)) +
      theme(strip.text=element_text(hjust=0)) +
      theme(strip.text = element_text(face = "italic")) +
      theme(axis.title.x=element_blank()) +
      theme(axis.ticks.x=element_blank()) +
      theme(axis.text.x=element_blank())
    
  })
  

  
output$first_plot <- renderPlot({  
  pos <- grep(input$title, Locations$Title)
  title = Locations[pos,3]
  season <-Locations[pos,2]
  colours2 <- colour.vec[season]
  colours1 <- colour_vec_solid[season]
  SG1source<-big.corpus$Text[pos]
  docs <- Corpus(VectorSource(SG1source)) 
  
 # myCorpus <- tm_map(docs,
 #                   content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')))
  myCorpus <- tm_map(docs, tolower)
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myStopwords <- stopwords('english')
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  myCorpus<-tm_map(myCorpus, stripWhitespace)
  
  #x    myCorpus <- tm_map(myCorpus, stemDocument)
  
  # Fix from: http://stackoverflow.com/questions/29358571/termdocumentmatrix-raises-error-in-r
  myCorpus<- tm_map(myCorpus, PlainTextDocument)
  
  
  mydtm<-TermDocumentMatrix(myCorpus,control=list(minWordLength=1))
  m <- as.matrix(mydtm)
  # calculate the frequency of words
  v <- sort(rowSums(m), decreasing=TRUE)
  myNames <- names(v)
  d <- data.frame(word=myNames, freq=v)
  pal <- brewer.pal(9, colours2)
  pal <- pal[-(1:4)]
#  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  wordcloud(d$word, d$freq, scale=c(7,0.7), min.freq=5,max.words=75, random.order=FALSE, rot.per=0, 
            colors=pal, use.r.layout=FALSE, fixed.asp=FALSE)
  
  })

output$first_plot_season <- renderPlot({ 
  pos <- grep(input$season, Locations$Series)
  pos<-pos[1]
  i <- big.corpus$number.series[pos]
  SG1source <- subset(big.corpus$Text, big.corpus$number.series==i)
  colour.graph1<-colour_vec_solid[i]
  colour.graph2 <- colour.vec[i]
  title <- paste ("Season ", i, "")
  SG1source <- str_replace_all(SG1source, "Â", "")
  
  docs <- Corpus(VectorSource(SG1source)) 
  
 # myCorpus <- tm_map(docs,
 #                    content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')))
  myCorpus <- tm_map(docs, tolower)
  myCorpus <- tm_map(myCorpus, removePunctuation)
  #  myCorpus <- tm_map(myCorpus, removeNumbers)
  myStopwords <- stopwords('english')
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  # myCorpus<-tm_map(myCorpus, stripWhitespace)
  
  #x    myCorpus <- tm_map(myCorpus, stemDocument)
  
  # Fix from: http://stackoverflow.com/questions/29358571/termdocumentmatrix-raises-error-in-r
  # myCorpus<- tm_map(myCorpus, PlainTextDocument)
  
  mydtm<-TermDocumentMatrix(myCorpus,control=list(minWordLength=1))
  m <- as.matrix(mydtm)
  # calculate the frequency of words
  v <- sort(rowSums(m), decreasing=TRUE)
  myNames <- names(v)
  d <- data.frame(word=myNames, freq=v)
  pal <- brewer.pal(9, colour.graph2)
  pal <- pal[-(1:4)]
 # layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=c(0,0,0,0))
  plot.new()
  wordcloud(d$word, d$freq, scale=c(7,0.7),min.freq=5,max.words=75, random.order=FALSE, rot.per=0, fixed.asp=FALSE,
            colors=pal, use.r.layout=FALSE)
  
  
  })


output$third_plot_season <- renderPlot({ 
  pos <- grep(input$season, Locations$Series)
  pos<-pos[1]
  i <- big.corpus$number.series[pos]
  SG1source <- subset(SG1.sentiment.season, SG1.sentiment.season$number.series==i)
  colour.graph1<-colour_vec_solid[i]
  colour.graph2 <- colour.vec[i]
  title <- paste ("Season ", i, "")
  ggplot(SG1source, aes(index, sentiment, fill=NA))+
    geom_bar(stat = "identity", show.legend=FALSE, fill=NA, colour=colour_vec_solid[i])+
    theme_minimal(base_size=13)+ 
    scale_x_discrete(expand=c(0.02,0)) +
    theme(strip.text=element_text(hjust=0)) +
    theme(strip.text = element_text(face = "italic")) +
    theme(axis.title.x=element_blank()) +
    theme(axis.ticks.x=element_blank()) +
    theme(axis.text.x=element_blank())
  
})

 
}




