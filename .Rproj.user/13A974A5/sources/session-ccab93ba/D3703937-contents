
server = function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  output$dashboardBody <- renderUI({
    
    # Show only when authenticated
    req(credentials()$user_auth)
    
    tabItems(
      tabItem(tabName = "landingPage",
              fluidRow(h2('Falcon Dashboard Landing Page'), align = 'center')
      ),
      source(file.path("ui_scripts", "ui_links.R"),  local = TRUE, encoding = 'UTF-8')$value
    )
    
  })
  
  source(file.path("server_scripts", "server_dashboard_main.R"),  local = TRUE, encoding = 'UTF-8')$value
  
  
  
}
