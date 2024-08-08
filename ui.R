dashboardPage( skin = 'black',
               title = 'FALCON',
               dashboardHeader(title = div(img(src="Falcon Long Logo dark png.png",
                                               width = 200,
                                               align = 'center'
                                               ),
                                           style='background-color:black;
                                     height: 100px;'),
                               tags$li(
                                 class = "dropdown",
                                 style = "padding: 8px;",
                                 shinyauthr::logoutUI("logout")
                               )
                               ),
  

  
               
               # logout button
               # div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
               
  # login section
  # shinyauthr::loginUI(id = "login"),
  
  # Sidebar to show user info after login

  # uiOutput("sidebarpanel"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Landing Page", tabName = "landingPage", icon = icon("dashboard")),
      menuItem("Links", tabName = "linksPage", icon = icon("link", lib = 'glyphicon'))
    )
  ),
  
  # Body to show user info after login
  # uiOutput("dashboardBody")
  

  
  dashboardBody(
    # login section
    shinyauthr::loginUI(id = "login"),
    tags$head(
      tags$style(HTML('
        /* logo */
        .skin-black .main-header .logo {
                              background-color: #000000;
                              }

        /* logo when hovered */
        .skin-black .main-header .logo:hover {
                              background-color: #1f76b4;
        }
        
          /* indiv buy button */
        .indiv-buy-button-js {
          display: inline-block;
          padding: 6px 12px;
          margin-bottom: 0;
          font-size: 14px;
          line-height: 1.42857143;
          text-align: center;
          white-space: nowrap;
          vertical-align: middle;
          cursor: pointer;
          border: 1px solid transparent;
          border-radius: 4px;
        }

        /* navbar (rest of the header) */
        .skin-black .main-header .navbar {
                              background-color: #000000;
        }

        /* body */
        .content-wrapper, .right-side {
                                background-color: white;
        }

        .navbar-nav > li > a, .navbar-brand {
                   padding-top:4px !important;
                   padding-bottom:0 !important;
                   height: 60px;
                 }
                 .navbar {min-height:60px !important;}
                              ')),
      tags$link(rel="shortcut icon", href="favicon.ico"),
              tags$style(HTML('* {font-family: "Lao MN"};
                              .tabbable > .nav > li > a {background-color: white;  color:black; width: 100VW;};
                               .tab-pane { background-color: white; width: 100%; }'))),
    uiOutput("dashboardBody"),
    # tabItems(
    #   tabItem(tabName = "landingPage",
    #           fluidRow(h2('Falcon Dashboard Landing Page'), align = 'center')
    #   ),
    #   source(file.path("ui_scripts", "ui_links.R"),  local = TRUE, encoding = 'UTF-8')$value
    # ),
    absolutePanel(id = "logo", class = "card", bottom = 20, right = 60, width = "auto", fixed=TRUE, draggable = FALSE, height = "40",
                  tags$a(href='https://www.falconxcglobal.com', tags$img(src='Falcon Long Logo.jpg',height='40',width="auto"))
    )
  )
)
