getwd()
setwd("app/")

rsconnect::setAccountInfo(
  name='simonpcouch',
  token=Sys.getenv("RSC_TOKEN"),
  secret=Sys.getenv("RSC_SECRET")
  # server = "posit.cloud"
)

# shiny::runApp()
rsconnect::deployApp(
  appName = "deere",
  envVars = "OPENAI_API_KEY"
)
