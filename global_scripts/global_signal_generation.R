

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


# function to pull ticker data
pull_all_tickers <- function(){
  url <- paste0('https://twelve-data1.p.rapidapi.com/stocks')
  
  raw_data <- GET(url,
                  query = list(
                    # start_date = date,
                    # end_date = date+8,
                    # interval = '1day',
                    # exchange = 'NASDAQ',
                    # symbol = 'GSPC',
                    format = "json"
                  ),
                  add_headers('X-RapidAPI-Key' = apiKey,
                              'X-RapidAPI-Host' = 'twelve-data1.p.rapidapi.com'),
                  content_type("application/octet-stream")
  )
  
  response<-content(raw_data, as = "text", encoding = "UTF-8")
  
  dfMarkets <- jsonlite::fromJSON(response, flatten = TRUE)
  
  tickers_df <- dfMarkets$data
  
  tickersUS <- tickers_df %>%
    dplyr::filter(country == 'United States',
                  exchange %in% c('NYSE','NASDAQ'))
  
  return(tickersUS)
}




