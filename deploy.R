# Deploys the app to shinyapps.io. Used by the ci workflow; locally it works
# the same once SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN and SHINYAPPS_SECRET are set.

account <- Sys.getenv("SHINYAPPS_ACCOUNT", "arcadefantasy")
token <- Sys.getenv("SHINYAPPS_TOKEN")
secret <- Sys.getenv("SHINYAPPS_SECRET")
if (!nzchar(token) || !nzchar(secret)) stop("SHINYAPPS_TOKEN and SHINYAPPS_SECRET must be set")

if (!requireNamespace("rsconnect", quietly = TRUE)) renv::install("rsconnect", prompt = FALSE)
rsconnect::setAccountInfo(name = account, token = token, secret = secret)

rsconnect::deployApp(
  appDir = ".",
  appFiles = c("app.R", list.files("R", full.names = TRUE), list.files("www", full.names = TRUE), "renv.lock"),
  appName = "Arcade_Basic_Stats",
  account = account,
  server = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = FALSE
)
