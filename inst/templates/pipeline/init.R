# INSTALL INTERNAL TOOLS ---------------------------
#
# devtools::install_github(
#   repo = "happycabbage/pipelinetools",
#   auth_token = "c350fc0203984e38432dea3ca2f3d0763f4db508",
#   quiet = TRUE,
#   upgrade = FALSE
# )


# LOAD LIB required to execute run.R -----------------------------
#
library(pipelinetools)


# DECLARE GLOBAL variables used within multiple source functions ----------
#
connectServer <- Sys.getenv("CONNECT_SERVER")
connectAPIKey <- Sys.getenv("CONNECT_API_KEY")


# DECLARE UTILS used by source funcs -----------------------------
#
#

