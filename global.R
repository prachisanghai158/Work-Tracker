.libPaths(c('./lib',.libPaths()))
#setwd("C:/Users/pullarkd/OneDrive - Merck Sharp & Dohme, Corp/Documents/Work Tracker in R/20190809")
#options(shiny.trace = TRUE)
dropdowns<-read.csv("./data/dropdowns.csv", header = TRUE, stringsAsFactors = FALSE)
dropdowns[is.na(dropdowns) | dropdowns=="" ]<-"None"
# #READ IN FILES
country_info<-read.csv("./data/Country_Info.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(country_info)<-c("Country","Region","SubRegion","Cluster","MerckTopMarketFlag")

holiday_list<-read.csv("./data/List of Holidays.csv", header=TRUE, stringsAsFactors = FALSE)

team_information <- read.csv("./data/Employee_list.csv", header = TRUE, stringsAsFactors = FALSE)



#library(timeDate) #used for function to find end of month
library(shinycssloaders) #used for loading icon
library(DT)
#library(Rcpp)
library(shiny)
#library(qpcR) #used for cbind.na
#library(dqshiny) #used for autocomplete_input
#library(date)
library(reshape2)
#library(tidyselect)
#library(plotly)
#library(shinyjqui)
library(shinydashboard)
#library(shinydashboardPlus)
#library(readxl)
library(timevis)
library(sqldf) #used for conditional joins
library(readr) #used for write_csv
library(data.table)
library(lubridate) #used for time functions
library(shinyWidgets) #used for checkboxGroupButton
library(plyr) #used for descending
library(stringr) #used from str_wrap
library(dplyr)
library(ggplot2)
library(tidyr) #used for separate_rows
#library(DescTools)#used for %like any%
#library(shinyjs)
library(htmltools)
#jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

library(rhandsontable)

pM<-projectsMain[projectsMain$Project_Name!="Adhoc",]

numberOfDays <- function(date) 
{
  m <- format(date, format="%m")
  while (format(date, format="%m") == m) 
  {
    date <- date + 1
  }  
  return(as.integer(format(date - 1, format="%d")))
}
outputId<<-""

#FUNCTION TO CALCULATE # OF WEEKENDS
Nweekends <- Vectorize(function(a,b) 
  sum(weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday")))

#FUNCTION TO CALCULATE # OF HOLIDAYS
Nholidays <- Vectorize(function(a, b, MSD_AR) 
  sum((seq(a, b, "days")) %in% as.Date(holiday_list[holiday_list$MSD.AR==MSD_AR,1],"%m/%d/%Y")))

cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}


monthoutput <- function(x){
  
  
 n<-which(x==month_list_scope)
 return(month_list_scope2[n])
  
}












#DEFINE OPTIONS FOR SELECTINPUT
geography_list<-unique(dropdowns$Geography)
task_type <- c("Engagement", "Analytics", "Modeling", "Self-Development / Internal Initiatives")
Targetaudience <- c("Global", "Regional", "Country", "GCF Internal")
Stakeholder <- c("MMD", "MARKETING", "CORE", "FINANCE", "GCA", "GCI", "GCF Internal")
status <- c("Completed", "Yet to Start", "Deferred", "In-Progress")
Projectclassification <- c("Priority Planning", "Investment Decisions", "Capabilities")
brand <- unique(dropdowns$Brand)
TA <- unique(dropdowns$TA)
Planningcycle <- c("2020 5YP", "2020 Nov FC", "2020 August FC", "2021 5YP" ,"2019 5YP", "2019 Nov FC", "2019 August FC",  "Binding FC")
stakeholdernameList<- unique(dropdowns$Stakeholders)
status<-c("Completed", "In Progress" , "Yet to start" , "De-prioritized")
priority<-c("High","Medium","Low")

########Information relating to teams 
team_names <- data.frame("Team"= c(unique(team_information$Team)))

#DATES and DATE SEQUENCES
year       <- year(format(Sys.Date(),tz="Asia/Kolkata",usetz=TRUE))
month      <- month(format(Sys.Date(),tz="Asia/Kolkata",usetz=TRUE))
#month_p    <- month(month(format(Sys.Date(),tz="Asia/Kolkata",usetz=TRUE))+1)
#endmonth   <- as.Date(paste(year,month(format(Sys.Date(),tz="Asia/Kolkata",usetz=TRUE))+1,1,sep="-"))

projectsMain <-read.csv("./data/Projects.csv",header = TRUE, stringsAsFactors = FALSE)
#*****
# PS CREATING MONTH LIST FOR SELECTION
d <- as.POSIXlt(as.Date(Sys.Date()))

d1 <- d
d2<-as.POSIXlt(as.Date("2019-01-01 UTC"))
d1$year <- d1$year+1

dat <- format(as.Date(seq(d,by="months",length= 12)),"%b %Y")
datpast<-format(as.Date(seq(d2,d,by="months")),"%b %Y")
month_list_scope<-c(datpast,dat)
month_list_scope<-unique(month_list_scope)



dat2 <- format(as.Date(seq(d,by="months",length= 12)),"%B")
dat2<-dat2[2:length(dat2)]
datpast2<-format(as.Date(seq(d2,d,by="months")),"%B")

month_list_scope2<-c(datpast2,dat2)





class(d1)





fte<-0
fteactual<-0
fteplanned<-0

options(spinner.color.background="#F5F5F5")