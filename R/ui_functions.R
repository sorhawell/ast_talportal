# Inputs ------------------------------------------------------------------

inputpicker_options <- function (a, b, c) {
  pickerOptions(
    actionsBox = TRUE, # giver vælg/fravælg knapperne
    deselectAllText  = "Fravælg alle",
    #selectAllText  = "Vælg alle udsægte" ,
    selectAllText  = "Vælg alle",
    noneSelectedText = paste("Søg på",a),
    liveSearch = TRUE,
    liveSearchPlaceholder = paste("Søg på",b),
    liveSearchStyle = c # bestemmer om den kun søger fra starten (startswith) eller om søgeordet blot skal være indeholdt
  )
}

# Bokse etc. --------------------------------------------------------------

blaa_boks <- function(a, b, ...) {
  box(title = a,
      collapsible = TRUE,
      collapsed = b,
      width = NULL,
      status = "primary",
      solidHeader = TRUE,
      ...
  )
}

graa_boks <- function(a, b, ...) {
  box(title = a,
      collapsible = TRUE,
      collapsed = b,
      width = NULL,
      status = "danger",
      solidHeader = TRUE,
      ...
  )
}