
#install packages not included in shiny-verse image layer
install.packages('pak')
packages = c(
  "jsonlite","shinyWidgets", "shinydashboard", "data.table", "juicyjuice",
  "shinyFeedback", "shinycssloaders", "shinyBS", "shinyhelper", "shinyjs",
  "openxlsx", "gt", "ggthemes", "webshot"
)
pak::pkg_install(packages, lib = tail(.libPaths(), 1))

#install any packages included in verse image layer, for dev use
if(interactive()) {
  dev_packages = c(
    "shiny", "dplyr", "glue", "readr", "purrr"
  )
  pak::pkg_install(dev_packages, lib = tail(.libPaths(), 1))
}
