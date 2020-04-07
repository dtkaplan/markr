library("googledrive")
library("googlesheets4") # I am using the developing version 0.1.0.9000
library("shiny")

# You want to deploy an app in Shinyapps.io or other server
# FIRST STEP----
# Get the token an store it in a cache folder embedded in your app directory
# designate project-specific cache
options(gargle_oauth_cache = ".secrets")
# options(gargle_quiet = FALSE) # So you can know what is happening
# Authenticate in interactive mode (run the app) ONCE and check if the token
# has been stored inside the .secrets folder, after that just comment this line
drive_auth() # Authenticate to produce the token in the cache folder
# Grant permission to googlesheets to access to the token produced
sheets_auth(token = drive_token())

# SECOND STEP----
# Comment lines 10, 13 and 15 and uncomment lines 21 and 22
# You tell gargle to search the token in the secrets folder and to look
# for an auth given to a certain email (enter your email linked to googledrive!)
drive_auth(cache = ".secrets", email = "dtkaplan@gmail.com")
sheets_auth(token = drive_token())

# THIRD STEP---
# Now you can deploy your app in shinyapps.io!!
# Test if your app runs properly in the local version
# Authenticate in ShinyApps.io
# rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
# setwd() in your App directory
# library(rsconnect)
# deployApp()
# Enjoy your new App!!

ui <- # Define UI for application that plots random distributions
  fluidPage(

    # Application title
    titlePanel("Hello Shiny!"),

    # Sidebar with a slider input for number of observations
    sidebarLayout(
      sidebarPanel(
        sliderInput("obs",
                    "Number of observations:",
                    min = 1,
                    max = 1000,
                    value = 500),
        actionButton(
          "add",
          "Add new entry")
      ),

      # Show a plot of the generated distribution
      mainPanel(
        "Check your googlesheet!!"
      )
    )
  )

server <- function(input, output, session) {
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  #
  observeEvent(input$add, {
    # You should have or create a googlesheets through google drive with
    # the name "example_shiny"
    wb <- drive_get("example_shiny")
    dt <- read_sheet(wb)
    new_entry <-
      data.frame(ID = tail(dt$ID, 1) + 1, NAME = "new",
                 OBSERVATION = input$obs)
    sheets_append(new_entry, wb)
  })
}

shinyApp(ui, server)
