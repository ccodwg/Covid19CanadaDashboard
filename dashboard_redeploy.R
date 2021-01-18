# load libraries
library(rsconnect)

# set account info
rsconnect::setAccountInfo(
  name = "art-bd",
  token = readLines("deploy_token/.token.txt"),
  secret = readLines("deploy_token/.secret.txt")
)

# redeploy app
deployApp(
  appName = "covid19canada",
  account = "art-bd",
  launch.browser = FALSE,
  forceUpdate = TRUE
)