
# Set Libraries -----------------------------------------------------------
# rm(list=ls())
# gc()
library("shiny")
library('shinyjs')
library("shinyWidgets")
library("shinydashboard")
# library('keyring')
library("htmltools")
library("shinythemes")
library("shinyBS")
library("DT")
# library("magrittr")
# library("rvest")
library("quantmod")
library("tidyquant")
library("readxl")
library("dplyr")
library("tidyr")
library("plyr")
# library("maps")
# library("ggplot2")
# library("gganimate")
# library('av')
library("reshape2")
# library("ggiraph")
library("RColorBrewer")
# library("leaflet")
library("plotly")
# library("geojsonio")
library('stringr')
library('stringi')
# library('pool')
library('forecast')
library('tstools')
library('tsutils')
# library('MASS')
# library('foreign')
# library('dplyr.snowflakedb')
# library('odbc')
# library('RODBC')
# library('rJava')
# library('RJDBC')
# library('lpmodeler')
library('drc')
library('LS2Wstat')
library('rhandsontable')
# library('BBmisc')
# library("jsonlite")
library("data.table")
library('forecast')
# library('MLmetrics')
# library('astsa')
library('lubridate')
# library('paws')
# install.packages('quantmod')
# library('quantmod')
# library('tidyquant')
library('purrr')
library('RcppRoll')
# library('RPostgres')
library('httr')
library('jsonlite')
library("googlesheets4")
library('pool')
library('RPostgreSQL')
library('IBrokers')
library('shinyauthr')
library('png')
library('reticulate')
library('sparkline')
library('mRpostman')
library('RCurl')
# library('scales')

jscode <- "shinyjs.refresh = function() { history.go(0); }"

# create a not in function
'%notin%' <- function(x,y)!('%in%'(x,y))

format.money  <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}

# Colour generation for charts --------------------------------------------

# For consistency across charts, we want to use the same colour for each item
# ideally with adjacent services being obviously different colours
# RGB values below 100,100,100 are all very dark and hard to distinguish
# R=G=B results in shades of grey - will try to have at least one value 20% different to the others
# max is 255
# view RGB as a XxX cube of smaller cubes, ignoring the 'grey' smaller cubes across the diagonal
# of which there would be a number equal to the length of one of the sides of the cube
# pick the centre of each cube to be the distinct colour
# cube must be of size: numItems == RxGxB - R === R^3 - R


colourTable <- function(inDset){
  # solver cubic equation for size of cube
  a <- suppressWarnings(as.integer(cubic(1,0,-1,-as.integer(count(inDset)))[c(1)]))
  b <- suppressWarnings(as.integer(cubic(1,0,-1,-as.integer(count(inDset)))[c(2)]))
  c <- suppressWarnings(as.integer(cubic(1,0,-1,-as.integer(count(inDset)))[c(3)]))
  # set cube to be one larger than largest cubic solution
  d <- max(a,b,c) + 1
  
  # start at max of 255 and go down to min of 100, gap is 155
  colourStep <- floor(155/d)
  
  # go downwards to use deeper colours first
  r <- c(255 - (colourStep * 1:d))
  g <- c(255 - (colourStep * 1:d))
  b <- c(255 - (colourStep * 1:d))
  mm <- rep(1,d)
  
  rR <- data.frame(r, mm)
  gG <- data.frame(g, mm)
  bB <- data.frame(b, mm)
  
  rgbVals <- rR %>%
    left_join(gG,
              by = 'mm') %>%
    left_join(bB,
              by = 'mm') %>%
    mutate(grey = if_else((r == g & r == b),1,0)) %>%
    filter(grey != 1) %>%
    select(-mm,-grey)
  
  
  # split 10 was for the rolling count
  
  rollCountNum <- ceiling(as.integer(count(rgbVals))/10)
  
  rgbVals <- rgbVals %>%
    mutate(rowNum = row_number(),
           rollCount = ((rowNum - 1)*rollCountNum + 1) %% as.integer(count(rgbVals)+1))
  
  inDset <- inDset %>%
    mutate(rollCount = row_number()) %>%
    left_join(rgbVals,
              by = c('rollCount')) %>%
    mutate(colour = paste0('rgb(',r,',',g,',',b,')')) %>%
    select(-rollCount, -rowNum, -r, -g, -b)
}

# Set Colours
iOGreen <- 'rgb(134,201,84,1)'
iOBlack <- 'rgb(25,25,25,1)'
iORed <- 'rgb(189,0,35,1)'
iOBlue <- 'rgb(0,31,83)'
falconBlue <- 'rgb(31, 119, 180)'
falconBlueHex <- '#1f76b4'
lightBlue <- '#a7bad9'
lightGreen <- '#bcd9a7'
lightRed <- '#d9a7a7'
burntOrange <- '#CC5500'
lighterOrange <- '#FFA006'


colourWeek1 <- 'rgb(134, 196, 169)'
colourWeek2 <- 'rgb(101, 158, 133)'
colourWeek3 <- 'rgb(61, 112, 90)'
colourWeek4 <- 'rgb(32, 66, 51)'



            
borderStyle <- "border: 2px solid #000000; border-radius: 8px"
borderStyleFine <- "border: 1px solid #000000; border-radius: 6px; padding: 10px"

font_style <- list(
  family = "Lao MN",
  # size = 14,
  color = 'black')


user_base <- tibble::tibble(
  user = c("AR", "JJ", "TB"),
  password = sapply(c("Perry", "Perry", "Perry"), sodium::password_store),
  permissions = c("admin", "admin", "admin"),
  name = c("AR", "JJ", "TB")
)


pool <- dbPool(
  drv = PostgreSQL(),
  dbname = "falcon",
  host = "68.183.42.25",
  port = 5432
  ,
  user = "aarogers",
  password = "falcon11uk"
)

onStop(function() {
  poolClose(pool)
})

apiKey <- "8e37fbcec2msh38a0590a582c462p10962djsndb9ccf6425dc"

source(file.path("global_scripts", "global_signal_generation.R"),  local = TRUE, encoding = 'UTF-8')$value
source(file.path("global_scripts", "global_email_ingest.R"),  local = TRUE, encoding = 'UTF-8')$value
# source(file.path("global_scripts", "global_dashboard.R"),  local = TRUE, encoding = 'UTF-8')$value
# source(file.path("global_scripts", "blacklists.R"),  local = TRUE, encoding = 'UTF-8')$value
# source(file.path("global_scripts", "nav_info.R"),  local = TRUE, encoding = 'UTF-8')$value
# source(file.path("global_scripts", "buy_basket_modelling.R"),  local = TRUE, encoding = 'UTF-8')$value
# source(file.path("global_scripts", "buy_auto.R"),  local = TRUE, encoding = 'UTF-8')$value
# source(file.path("global_scripts", "sell_auto.R"),  local = TRUE, encoding = 'UTF-8')$value





