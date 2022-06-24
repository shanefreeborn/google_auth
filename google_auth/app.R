library(shiny)
library(googleAuthR)
library(httr)
library(shiny)
library(shinyjs)
library(googleAuthR)
library(googleID)
library(pool)
library(odbc)
library(tidyverse)
library(DT)
library(DBI)
library(glue)
library(shinyWidgets)
library(shinycssloaders)

# OAuth setup --------------------------------------------------------

# Most OAuth applications require that you redirect to a fixed and known
# set of URLs. Many only allow you to redirect to a single URL: if this
# is the case for, you'll need to create an app for testing with a localhost
# url, and an app for your deployed app.

if (interactive()) {
  # testing url
  options(shiny.port = 8100)
  APP_URL <- "http://localhost:8100/"
} else {
  # deployed URL
  #APP_URL <- "https://innovation.aguafria.org/google_oauth_template/"
}

# Note that secret is not really secret, and it's fine to include inline
# key and secret are under sfreeborn@aguafria.org test project. To create for a project,
# it must be created in Google first
app <- oauth_app("google",
                 key = "430836752453-nnlm5v7735sc6toloa2nf9sj1u3q09d1.apps.googleusercontent.com",
                 secret = "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",
                 redirect_uri = "http://localhost:8100/" #Redirect URI needs to be added to Google. hardcoded for now
)

# Here I'm using a canned endpoint, but you can create with oauth_endpoint()
api <- oauth_endpoints("google")

# Always request the minimal scope needed. For github, an empty scope
# gives read-only access to public info
scope <- "https://www.googleapis.com/auth/userinfo.email"

# Shiny -------------------------------------------------------------------

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth codes are present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}

ui <- fluidPage(
  # Your regular UI goes here, for when everything is properly auth'd
  verbatimTextOutput("code")
)

# A little-known feature of Shiny is that the UI can be a function, not just
# objects. You can use this to dynamically render the UI based on the request.
# We're going to pass this uiFunc, not ui, to shinyApp(). If you're using
# ui.R/server.R style files, that's fine too--just make this function the last
# expression in your ui.R file.
app_name <- "OAuth Test"
main_ui <- fluidPage(
  setBackgroundColor(
    color = c("#F1F1F1", "#F1F1F1"),
    gradient = "linear",
    direction = "bottom"
  ),
  titlePanel(app_name),
  windowTitle = app_name)
  #tags$style(HTML(read_file("www/custom_css.css"))),
  
  fluidRow(
    column(
      wellPanel(
        id = "global",
        # textOutput("panel")
        h4("App Controls")
      ), # End of the Global Controls 
      
      # Create a panel for page specific controls
      wellPanel(
        id = "page_controls",
        # textOutput("panel2")
        h4("Page Controls")
      ), # End of the Global Controls 
      width = 3 # Width of the sidebar
    ),
    mainPanel(
      id = "main_panel",
      tabsetPanel(
        tabPanel(
          "Table", 
          p("Testing"),
          withSpinner(dataTableOutput("act_table"), image = "animated.gif", image.width = "300")
        ),
        tabPanel("panel 2", "two"),
        tabPanel("Logout" ,
                 useShinyjs(),
                 sidebarLayout(
                   sidebarPanel = sidebarPanel(
                     h2(glue("Log out of {app_name}")),
                     googleAuthUI("gauth_login"),
                     # googleAuthUI("gauth_login"),
                     textOutput("display_username")), 
                   mainPanel = mainPanel()
                 )
        )
      ), 
      width = 9 # Width of the main panel
    )
  )

server <- function(input, output, session) {
  params <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(params)) {
    return()
  }
  
  # Manually create a token
  token <- oauth2.0_token(
    app = app,
    endpoint = api,
    credentials = oauth2.0_access_token(api, app, params$code),
    cache = FALSE
  )
  
  resp <- GET("https://www.googleapis.com/userinfo/v2/%s", config(token = token))
  # TODO: check for success/failure here
  
  output$code <- renderText(content(resp, "text"))
  
  # Innovation database connection
  af_connection <- function(){
    
    if(Sys.info()['sysname'] == "Darwin"){
      dev <- TRUE
      driver <- 'libtdsodbc.so'
      db_connection <- dbConnect(
        drv = odbc(),
        Driver = driver,
        Server = "10.220.3.147",
        Database = "assessment",
        UID = keyring::key_get("afuhsd_database_username", "assessment"),
        PWD = keyring::key_get("afuhsd_database", "assessment"),
        Port = 1433
      )
      
      return(db_connection)
    }else{
      driver <- readLines("/opt/microsoft/current_driver.txt")
      db_user <- Sys.getenv("db_user")
      
      db_password <- Sys.getenv("db_password")
      
      db_connection <- dbPool(
        drv = odbc(),
        Driver = driver,
        Server = "10.220.3.147",
        Database = "assessment",
        UID = Sys.getenv("db_user"),
        PWD = db_password,
        Port = 1433
      )
      
      return(db_connection)
    }
  }
  
  
  
  # DataTable options ----
  dt_options <- list(
    dom = 'Bfrtip', 
    buttons = 
      c('csv', 'excel', 'pdf', 'print', 'colvis')
    # c('excel','pdf','print','colvis')
    # list('colvis', list(extend = "csv", text = "CSV", filename = "data",
    #                     exportOptions = list(
    #                         modifier = list(page = "all")
    #                     )))
    , 
    # initComplete = DT::JS("function(settings, json) {",
    #                       "$(this.api().table().header()).css({'background-color': '#369BE9', 'color': '#fff'});",
    #                       "}"), 
    scrollX = TRUE, 
    scrollY = '40vh', 
    pageLength = 12000 #,
    # scrollCollapse = TRUE
  )
  
  
  # Conditionally define the UI for logged in/logged out users.
  
  # This is the ui for the Login page -----
  login_ui <- fluidPage(
    windowTitle = app_name,
    setBackgroundImage(src = "spotlight2.png"),
    tags$style(HTML(read_file("www/custom_css.css"))),
    fluidRow(
      useShinyjs(),
      column(3),
      column(
        6,
        # Add logo image and center it.
        HTML('<center><img src="logo_center2.png" alt="Agua Fria" width="65%" height="65%"></center>'),
        fluidRow(
          # inputPanel( # This can be uncommented to put a border and panel around login button
          # googleAuthUI("gauth_login"),
          googleAuthUI("gauth_login"),
          h3(glue("{app_name}"), align = "center"),
          textOutput("display_username"), # This is necessary for login to register and app to work
          align = "center"
          # )
        )
      ),
      column(3)
    )
  ) # End of login ui definition
}

# Note that we're using uiFunc, not ui!
shinyApp(main_ui, server)

