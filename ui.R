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
# 2. Create UI
#########################################################################
# 1. Load libraries, load data
#########################################################################


# The UI
library(shinydashboard)
library(shiny)
library(xlsx)
# setwd("~/Documents/Rex Analytics/Blog/SG1 screenplay/SG1 app")
Locations<- read.xlsx("Locations.xlsx", sheetName="Sheet1", startIndex=1, startRow=1, as.data.frame=TRUE, header=TRUE)
Locations$Title<-as.character(Locations$Title)
Locations$URL <-as.character(Locations$URL)

#########################################################################
# 2. UI construction
#########################################################################


ui <- navbarPage("SG1: Text Mining",
                 tabPanel("Episodes",               
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("title", "Choose a title:", Locations$Title)
                            ),
                            mainPanel(
                              plotOutput("first_plot"),
                              plotOutput("third_plot")
                            )
                          )
                 ),
                 tabPanel("Seasons",               
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("season", "Select a season:", Locations$Series)
                            ),
                            mainPanel(
                              plotOutput("first_plot_season"),
                              plotOutput("third_plot_season")
                            )
                          )
                 )
                 
)



