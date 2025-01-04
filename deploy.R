
library(shiny)
library(DT)
library(tidyverse)
library(rsconnect)

##############

if(file.exists('keys.R') == T){
  
  source('keys.R')
  
  print("Connecting to account using local keys")
    rsconnect::setAccountInfo(name = shiny_acc,
                  token = shiny_token,
                  secret = shiny_secret)
   print("Connect Successfull")
}

if(file.exists('keys.R') == F){
  print("Connecting to account using git secrets")
    rsconnect::setAccountInfo(name = Sys.getenv("shiny_acc"),
                  token = Sys.getenv("shiny_token"),
                  secret = Sys.getenv("shiny_secret"))
   print("Connect Successfull")
}
    
 # remove.packages("knitr"): removing a package this way causes a process to just terminate with no clear error code
deployApp(
  appFiles = c( 
    # data files  for the app 
    'scores.csv', 
    
    # scripts and R files 
    "app.R"
    ), 
  appName = 'augsburg_rec', 
  forceUpdate = T
  )

     