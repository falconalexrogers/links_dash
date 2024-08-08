
library(tm.plugin.mail)

# con <- configure_imap(url="imaps://imap.gmail.com",
#                       username="falcontestemailuk@gmail.com",
#                       password=rstudioapi::askForPassword())
# 
# con$select_folder(name = "INBOX")
# 
# res1 <- con$search_string(expr = "@k-state.edu", where = "FROM")



mbf <- "mboxfile"
convert_mbox_eml(mbf, "emlfile2")

maildir <- "data"
mailfiles <- dir(maildir, full.names=TRUE)

readmsg <- function(fname) {
  l <- readLines(fname)
  subj <- grep("Subject: ", l, value=TRUE)
  subj <- gsub("Subject: ", "", subj)
  date <- grep("Date: ", l, value=TRUE)
  date <- gsub("Date: ", "", date)
  text1 <- tail(l, 3)[1]
  text2 <- tail(l, 3)[2]
  return(c(subj, date, text1, text2))
}

mdf <- as.data.frame(do.call(rbind, lapply(mailfiles, readmsg)))

l <- readLines(mailfiles)

l <- readLines(mailfiles[1])

lll <- paste(l,collapse=" ")
lll <- gsub("= ", "", lll)

link_start <- as.data.frame(str_locate_all(lll,pattern = "<a href"))
link_end <- as.data.frame(str_locate_all(lll, pattern = '</a>'))

min_end_func <- function(position){
  end_pos <- link_end %>%
    dplyr::filter(start > position) %>%
    head(1)
  return(end_pos$end)
}

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
  
write.csv(just_links,'data/output/test_output.csv')


thepage <- readLines(links_output$link_to_amp[1])

library('RCurl')
page <- getURL(links_output$link_to_amp[1], useragent = UA)
page <- getURLContent('https://www.forbes.com/sites/greatspeculations/2024/07/24/should-you-pick-united-airlines-stock-after-a-mixed-q2/', useragent = UA, http_version=2)

page <- getURLContent(links_output$link_to_amp[10], config(verbose=TRUE, http_version=2, forbid_reuse=T), useragent = UA, timeout(10), verbose())
pageContent <- as.data.frame(page$content)

x <- readLines('http://www.imdb.com/chart/')

# UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36"
# UA <- "libcurl/7.88.1 r-curl/5.0.0 httr/1.4.6"

UAs <- GET('https://httpbin.org/headers')
UA <- UAs$request$options$useragent

res   <- GET('http://www.imdb.com/chart/', config(verbose=TRUE, http_version=2, forbid_reuse=T), add_headers(`Connection` = "keep-alive", `User-Agent` = UA))
res   <- GET(links_output$link_to_amp[10], config(verbose=TRUE, http_version=2, forbid_reuse=T), add_headers(`Connection` = "keep-alive", `User-Agent` = UA))

data1 <- strsplit(page, "\n")[[1]]

head(data1, 10) 

rlines <- readLines(links_output$link_to_amp[10], options(timeout = 10))

content <- as.data.frame(rlines)

head(content,100)



ur <- links_output$link_to_amp[1]

HEAD(ur, config(sslversion=4, http_version=2)) %>%  ## <- this is the magic line
  .$headers %>%
  as_data_frame() %>%
  glimpse()


page <- getURL(links_output$link_to_amp[10], config(http_version=2), useragent = UA, timeout = 10)
thisURL <- getURL(links_output$link_to_amp[i], http_version=2, useragent = UA, timeout = 10)


subsPage <- substr(page,1,500)





library(shiny)

# createLink <- function(val) {
#   sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary" /a>',val)
# }

createLink <- function(val) {
  sprintf('<a href="%s" target="_blank" class="btn btn-primary">%s</a>',val,val)
}

ui <- fluidPage(  
  titlePanel("Table with Links!"),
  sidebarLayout(
    sidebarPanel(
      h4("Click the link in the table to see
         a google search for the car.")
    ),
    mainPanel(
      dataTableOutput('table1')
    )
  )
)

server <- function(input, output) {
  
  output$table1 <- renderDataTable({
    
    my_table <- links_output
    
    my_table$link <- createLink(my_table$link_to_amp)
    return(my_table)
    
  }, escape = FALSE)
}

shinyApp(ui, server)




