


readmsg <- function(fname){
  l <- readLines(fname)
  subj <- grep("Subject: ", l, value=TRUE)
  subj <- gsub("Subject: ", "", subj)
  date <- grep("Date: ", l, value=TRUE)
  date <- gsub("Date: ", "", date)
  text1 <- tail(l, 3)[1]
  text2 <- tail(l, 3)[2]
  return(c(subj, date, text1, text2))
}

min_end_func <- function(position){
  end_pos <- link_end %>%
    dplyr::filter(start > position) %>%
    head(1)
  return(end_pos$end)
}

get_links <- function(fname){
  l <- readLines(fname)
  
  lll <- paste(l,collapse=" ")
  lll <- gsub("= ", "", lll)
  
  link_start <- as.data.frame(str_locate_all(lll,pattern = "<a href"))
  link_end <- as.data.frame(str_locate_all(lll, pattern = '</a>'))
  
  link_testing <- link_start %>%
    rowwise %>%
    mutate(link_stop = min_end_func(end)) %>%
    mutate(string = substr(lll, start, link_stop)) %>%
    mutate(to_quote = str_locate(string,pattern = '"')) %>%
    mutate(to_quote2 = gsub('^([^"]*"[^"]*)".*$', "\\1", string)) %>%
    mutate(between2quotes = substr(to_quote2,to_quote+9,nchar(to_quote2))) %>%
    mutate(to_https = str_locate(between2quotes,pattern = 'https')[1]) %>%
    dplyr::filter(!is.na(to_https)) %>%
    mutate(from_https = substr(between2quotes,to_https,nchar(between2quotes))) %>%
    mutate(to_html = str_locate(from_https,pattern = '.html')[1]) %>%
    mutate(html_trim = ifelse(is.na(to_html),from_https,substr(from_https,1,to_html+4))) %>%
    mutate(to_amp = str_locate(html_trim,pattern = '&')[1])  %>%
    mutate(link_to_amp = ifelse(is.na(to_amp),html_trim, substr(html_trim,1, to_amp -1))) 
  
  just_links <- link_testing %>%
    dplyr::select(link_to_amp) %>%
    unique()
  
  return(just_links)
}

# createLink <- function(val) {
#   paste0('<a href="',val,'" target="_blank" class="btn btn-primary">',val,'</a>')
# }

createLink <- function(val) {
  sprintf('<a href="%s" target="_blank" class="btn btn-primary">Go To Link</a>',val)
}

all_tickers <- pull_all_tickers()

links_output <- get_links('data/Google Alert - Stock Picks (1).eml')
UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36"

links_output2 <- links_output
links_output2$linkButton <- createLink(links_output2$link_to_amp)

# signals_master = data.frame(ticker = character(),
#                             source_link = character())
# 
# for(i in 1:nrow(links_output)){
#   print(paste0(i," of ",nrow(links_output),": ",links_output$link_to_amp[i]))
#   thisURL <- tryCatch({getURL(links_output$link_to_amp[i], useragent = UA, ssl.verifypeer=FALSE, timeout = 10)},
#                         error = function(error_condition) {
#                           'bad url connection'
#                         })
#   
#   if(thisURL == 'bad url connection'){
#     print(paste0(links_output$link_to_amp[i]," : connection refused"))
#   } else {
#     # data1 <- as.data.frame(strsplit(thisURL, "\n")[[1]])
#     
#     for(j in 1:nrow(all_tickers)){
#       if(j/1000 == round(j/1000)){
#         print(paste0(j," of ",nrow(all_tickers)," tickers checked"))
#       }
#       
#       # start <- Sys.time()
#       hasSignal <- str_detect(thisURL, regex(paste0("[^a-zA-Z0-9_]",all_tickers$symbol[j],"[^a-zA-Z0-9_]"), ignore_case=FALSE)) + 
#         str_detect(thisURL, regex(paste0("[^a-zA-Z0-9_]",all_tickers$name[j],"[^a-zA-Z0-9_]"), ignore_case=FALSE))
#       # stop <- Sys.time()
#       # diff <- start-stop
#       
#       if(sum(hasSignal) > 0){
#         signal_to_add <- data.frame(ticker = all_tickers$symbol[j],
#                                     source_link = links_output$link_to_amp[i])
#         
#         signals_master <- signals_master %>%
#           bind_rows(signal_to_add)
#       }
#     }
#   }
# }
# 
# 
# length(unique(signals_master$ticker))


tableHeaderLinksTables <- htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      # th(colspan = 8, paste0(rollsumChar(), " day rolling")),
      th('#', style=paste0("background-color:", iOBlack, ";text-align:center;color:white")),
      th('Address', style=paste0("background-color:", iOBlack, ";text-align:center;color:white")),
      th('Button', style=paste0("background-color:", iOBlack, ";text-align:center;color:white"))
    )
  )
))

