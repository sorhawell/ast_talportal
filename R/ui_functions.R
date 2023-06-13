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


################# UI #################

# UI-functions ------------------------------------------------------------

KPI_boks_uden_nogletal <- function (titel, tekst, inputid, antal_sager, subtitle, inputid2, link, link_tekst) {
  box(title = titel,
    collapsible = FALSE,
    collapsed = FALSE,
    width = NULL,
    status = "primary",
    solidHeader = TRUE,
    fluidRow(
      column(6,
             html(tekst)
      ),
      column(6,
             fluidRow(
               column(12,
                      valueBox(
                        value = actionLink(
                          inputId = inputid,
                          label = div(antal_sager, style = "color: white; font-size: 60%")
                        )
                        ,
                        subtitle = tagList(
                          subtitle
                        ),
                        icon = tags$i(class = "fas fa-folder-open", style="font-size: 44px"),
                        color = "orange",
                        width = NULL
                      )
               ))
             # ,
             # fluidRow(
             #   column(12,
             #          valueBox(
             #            value = actionLink(
             #              inputId = inputid2,
             #              label = div("28,9 %", style = "color: white; font-size: 60%")
             #            )
             #            ,
             #            subtitle = tagList(
             #              "omgørelsesprocent i 2020"
             #            ),
             #            icon = tags$i(class = "fas fa-percent", style="font-size: 44px"),
             #            color = "light-blue",
             #            width = NULL
             #          )))
      )),
    actionLink(link, link_tekst)
)
}

KPI_boks_med_nogletal <- function (titel, tekst, inputid, antal_sager, periode, inputid2, noegletal, link, link_tekst) {
  box(title = titel,
      collapsible = FALSE,
      collapsed = FALSE,
      width = NULL,
      status = "primary",
      solidHeader = TRUE,
      fluidRow(
        column(6,
               p(tekst)
        ),
        column(6,
               fluidRow(
                 column(12,
                        valueBox(
                          value = actionLink(
                            inputId = inputid,
                            label = div(antal_sager, style = "color: white; font-size: 60%")
                          )
                          ,
                          subtitle = tagList(
                            paste("antal sager i", periode)
                          ),
                          icon = tags$i(class = "fas fa-folder-open", style="font-size: 44px"),
                          color = "orange",
                          width = NULL
                        )
                 ))
                ,
                fluidRow(
                  column(12,
                         valueBox(
                           value = actionLink(
                             inputId = inputid2,
                             label = div(noegletal, style = "color: white; font-size: 60%")
                           )
                           ,
                           subtitle = tagList(
                             paste("omgørelsesprocent i", periode)
                           ),
                           icon = tags$i(class = "fas fa-percent", style="font-size: 44px"),
                           color = "light-blue",
                           width = NULL
                         )))
        )),
      actionLink(link, link_tekst)
  )
}




# > Inputfelter -------------------------------------------------------------

make_pickerInput <- function(inputId, label, sagsemner, lovgrundlag, soeg_paa, soeg_paa2, soegetype, choiceOpt) {
   pickerInput(
       inputId = inputId,
       label = label,
       choices = lapply(split(as.character(sagsemner), lovgrundlag), as.list),
       multiple = TRUE,
       options = inputpicker_options(soeg_paa, soeg_paa2, soegetype),
       choicesOpt = list(tokens = choiceOpt)
   )
}

make_materialSwitch <- function(inputId) {
  materialSwitch(
    inputId = inputId,
    label = "Tilføj landstotal",
    value = FALSE,
    status = "primary"
  )
}

# make_radioButtons <- function(inputId, label, valg1, valg2) {
#   radioButtons(inputId, label = label,
#                choices = list(valg1 = 1,
#                               valg2 = 2)
#   )
# }


# > Knapper -------------------------------------------------------------

make_nulstil_knap <- function(inputId) {
  span(actionButton(inputId, "Nulstil filtre", icon = icon("eraser")), style = "postition:absolute;right:2em;")

}


# > Valgbokse -------------------------------------------------------------

make_vaelg_emne <- function(size, placering, type, inputId1, inputId2, choicesOpt) {

  column(size,
         box(
           width = NULL,
           solidHeader = TRUE,
           status = 'primary',
           title = span(paste0(placering,": Vælg ",type), icon("search")),

           column(4,
                  radioButtons(inputId1, label = "Udvælg",
                               choices = list("Paragraffer" = 1,
                                              "Lovgivninger" = 2)
                  )
           ),
           column(8,
                  make_pickerInput(inputId2, "Lovgivning", NULL, NULL, "lovgivning", "lovgivning, navn eller paragraf", "contains", choicesOpt)
           )
         )
  )

}

make_vaelg_emne_alt <- function(size, placering, type, inputId, choices_2) {

  column(size,
         box(
           width = NULL,
           solidHeader = TRUE,
           status = 'primary',
           title = span(paste0(placering,": Vælg ",type)),
           column(12,
                  awesomeCheckboxGroup(inputId,
                                              label = NULL,
                                              choices = choices_2
                                     )
           )
         )
  )
}

make_vaelg_emne_alt2 <- function(size, placering, type, inputId, choicesOpt) {

  column(size,
         box(
           width = NULL,
           solidHeader = TRUE,
           status = 'primary',
           title = span(paste0(placering,": Vælg ",type), icon("search")),
           column(12,
                  make_pickerInput(inputId, "Lovgivning", NULL, NULL, "lovgivning", "lovgivning, navn eller paragraf", "contains", choicesOpt)
           )
         )
  )
}

make_vaelg_instans <- function(size, placering, instanstype, inputId1, inputId2, kommuner, regioner) {
  column(size,
    box(
      width = NULL,
      solidHeader = TRUE,
      status = 'primary',
      title = span(paste0(placering,": Vælg ",instanstype), icon("search")),
      make_pickerInput(inputId1, NULL, kommuner, regioner, "kommune", "kommune eller region", "startsWith", regioner),
      make_materialSwitch(inputId2)
    )
  )
}

make_valg_aar <- function(size, placering, inputId) {
  column(size,
     box(
        width = NULL,
        solidHeader = TRUE,
        status = 'primary',
        title = span(paste0(placering,': Vælg periode'), icon("search")),
        column(12,
               make_pickerInput(inputId, "Periode", NULL, NULL, "år", "år", "contains", NULL)
        )
       )
      )
}

make_valg_kvartal <- function(size, placering, inputId) {
  column(size,
         box(
           width = NULL,
           solidHeader = TRUE,
           status = 'primary',
           title = span(paste0(placering,': Vælg periode'), icon("search")),
           column(12,
                  make_pickerInput(inputId, "Periode", NULL, NULL, "kvartal", "kvartal", "contains", NULL)
           )
         )
  )
}

make_valg_periode <- function(size, placering, inputId1, inputId2) {
  column(size,
         box(
           width = NULL,
           solidHeader = TRUE,
           status = 'primary',
           title = span(paste0(placering,': Vælg periode'), icon("search")),
           column(4,
                  radioButtons(inputId1, label = "Periode",
                               choices = list("År" = 1,
                                              "Kvartal" = 2)
                  )),
           column(8,
                  make_pickerInput(inputId2, "Periode", NULL, NULL, "periode", "år eller kvartal", "contains", NULL)
           )
         )
  )
}




make_fyldt_blaa_boks <- function(...) {
  box(
    width = NULL,
    background = "light-blue",
    ...)
}

# > Tilpas tabel ----------------------------------------------------------

make_tilpas_tabellen <- function(...) {
   graa_boks("Tilpas tabellen",
             FALSE,
             ...)
 }

make_som_raekker <- function(inputId, prevalg) {
               radioButtons(inputId, 'Som rækker',
                            choices = c('Kommune - herunder lovgivning' = 1,
                                        'Lovgivning - herunder kommune' = 2),
                            selected = prevalg)
}

make_som_kolonner <- function(inputId) {

  radioButtons(inputId, 'Som kolonner',
               choices = c('Nøgletal - herunder år' = 1,
                           'År - herunder nøgletal' = 2))
}

make_vises_fodnoter <- function(inputId) {
               radioButtons(inputId, 'Skal der vises fodnoter?',
                            choices = c('Ja' = TRUE,
                                        'Nej' = FALSE))
}

make_vises_i_alt <- function(inputId) {
               radioButtons(inputId, 'Vis i alt',
                            choices = c('Ja' = TRUE,
                                        'Nej' = FALSE))
}


# > Tilpas graf -----------------------------------------------------------

make_hvordan_skal_grafen_se_ud <- function(inputId) {
  box(
    title = "Hvordan skal grafen se ud?",
    width = NULL,
    status = "primary",
    solidHeader = TRUE,
    radioButtons(inputId, 'Opdel på kommune eller lovgivning',
                 choices = c('Flere kommuner i samme graf' = 1,
                             'Flere lovgivninger i samme graf' = 2))
  )
}

make_eksport_graf <- function (inputId) {
box(title = "Eksport af graf som billede",
    width = NULL,
    status = "primary",
    solidHeader = TRUE,
    downloadButton(inputId, "Hent PNG-fil")
    )
}
