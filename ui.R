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

source("./ui_functions.R", local = TRUE) # definerer andre filer. Jeg er ikke sikker på, at den skal stå her 

# Header ---------------------------------------------

header <- dashboardHeader(
                          #disable = TRUE,
                          #title = "Ankestyrelsens talportal", 
                          #titleWidth = 290,
                          titleWidth = 0,
                          tags$li(class = "dropdown", actionLink("header_knap_forside", "FORSIDE", icon = icon("home"), class = "my_class")),
                          tags$li(class = "dropdown", actionLink("om_portalen", "OM TALPORTALEN / KONTAKT", icon = icon("info"), class = "my_class")),                          
                          tags$li(a(href = 'http://www.ast.dk', target="_blank",
                                            img(src = 'hvidtlogo2.png',
                                            title = "Link til ast.dk", height = "40px"),
                                            style = "padding-top:5px; padding-bottom:5px;"),
                                            class = "dropdown")
                          #tags$li(class = "dropdown", actionLink("header_knap_forside", "Forside", icon = icon("home"), class = "my_class")), 
                          #tags$li(class = "dropdown", actionLink("header_knap_sagsbehandlingstider", "Underside om sagsbehandlingstider", icon = icon("clock"), class = "my_class"))
                          #tags$li(class = "dropdown", actionLink("header_knap_sagsudfald", "Sagsudfald", icon = icon("percent"), class = "my_class")),
                          #tags$li(class = "dropdown", actionLink("om_portalen", "Om", class = "my_class"))
                          )

# Sidebar (venstremenu) ----------------------------------------

sidebar <- dashboardSidebar(
  disable = FALSE,
  collapsed = TRUE,
  width = 290,
  sidebarMenu(id = "tabs",
              
     menuItem("Forside", tabName = "forside", icon = icon("home")),
     menuItem("Sagsudfald og omgørelsesprocenter", tabName = "Sagsudfald", icon = icon("percent"),
             menuSubItem("KSB-sager", tabName = "subitem6"),
             menuSubItem("UDK-sager", tabName = "subitem7"),
             menuSubItem("Arbejdsskadesager", tabName = "subitem10"),
             menuSubItem("Tilsynssager", tabName = "Tilsynssager"),
             menuSubItem("Mellemkommunal", tabName = "Mellemkommunal"),
             menuSubItem("Underretningssager", tabName = "Underretningssager"),
             menuSubItem("Tvangsbortadoption", tabName = "Tvangsbortadoption"),
             menuSubItem("Anbringelse", tabName = "Anbringelse")
             )     
  )
)

# Body (hovedvindue) ---------------------------------------------

body <-  dashboardBody(
  tags$head(tags$style(HTML(
    '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Poppins",sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
  tags$head(tags$title("Ankestyrelsens talportal")),
  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Ankestyrelsens talportal </span>\');
      })
     ')),
  useShinyFeedback(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"), # henviser til css-filen med yderligere styling
    tags$style(HTML("
                      .my_class {
                      font-weight: bold;
                      color:white;
                      }"))
  ),
  tabItems(
    
 # Forside -----------------------------------------------------------------

 tabItem(tabName = "forside",
         fluidPage(
           fluidRow(
             column(8, offset = 2, align="center",
                    box(
                      title = span("Fremsøg statistik", icon("search")),
                      collapsible = FALSE,
                      collapsed = FALSE,
                      width = NULL,
                      solidHeader = TRUE,
                      p("Her kan du søge på alle paragraffer, emner og lovgivninger, som Ankestyrelsen behandler sager om. Du kan derefter gå direkte til den relevante statistik."),
                      pickerInput(
                        inputId = 'soegning_forside', 
                        label = '',
                        selected = NULL,
                        choices = lapply(split(as.character(Sagsemner_forside$Sagsemne), Sagsemner_forside$Omraade), as.list),
                        multiple = TRUE,
                        choicesOpt = list(#tokens = Sagsemner_forside$X,
                          subtext = paste(Sagsemner_forside$Lovgrundlag,Sagsemner_forside$Paragraffer),
                          style = rep(("color: black; font-weight: bold;"),450)),
                        pickerOptions(
                          deselectAllText  = "Fravælg alle",
                          selectAllText  = "Vælg alle",
                          noneSelectedText = paste("Søg efter paragraf, emne eller lovgivning"),
                          liveSearch = TRUE,                                                       
                          liveSearchPlaceholder = paste("Søg efter paragraf, emne eller lovgivning"),
                          liveSearchStyle = "contains",
                          maxOptions = 1
                        )
                      ),
                      uiOutput("Knap_paa_forsiden"),
                      uiOutput("ovrige_paa_forsiden")
                    ))),
           box(
             width = NULL,
             tabBox(
               title = tagList( " "),
               width = NULL,
               tabPanel("Arbejdsskadesager",
                        fluidRow(
                          column(12, align="left",
                                 fluidRow(
                                   column(4, 
                                          KPI_boks_med_nogletal(titel = "Arbejdsmarkedets Erhvervssikring",
                                                                tekst = html("Sager, hvor Arbejdsmarkedets Erhvervssikring (AES) har truffet den oprindelige afgørelse. Det er sager inden for arbejdsskadeloven."),
                                                                inputid = "forside_ask_kpi_antal_sager",
                                                                antal_sager = "9.883",
                                                                periode = "2022",
                                                                inputid2 = "forside_ask_kpi_omgor",
                                                                noegletal = "28,6 %",
                                                                link = "ASk_knap_forside_ny",
                                                                link_tekst = "Gå til arbejdsskadesager")
                                   ),
                                 )
                          ),                       
                        )
               ),
               tabPanel("Kommunale sager",
                        fluidRow(
                          column(12, align = "left",
                                 fluidRow(
                                   column(4, align = "left",
                                          KPI_boks_med_nogletal(titel = "Kommunale sager (beskæftigelses- og socialområdet)",
                                                                tekst = html("Sager, hvor en kommune har truffet den oprindelige afgørelse. Det er bl.a. sager inden for serviceloven, aktivloven og lov om aktiv beskæftigelsesindsats.
                                                                <br>
                                                                <br>
                                                                OBS: Sager, hvor det er kommunale børn- og ungeudvalg, der har truffet den oprindelige afgørelse, indgår <u>ikke</u> i statistikken."),
                                                                inputid = "forside_ksb_kpi_antal_sager",
                                                                antal_sager = "18.420",
                                                                periode = "2022",
                                                                inputid2 = "forside_ksb_kpi_omgor",
                                                                noegletal = "30,2 %",
                                                                link = "KSB_knap_forside_ny",
                                                                link_tekst = "Gå til kommunale sager")
                                   ),
                                    column(4,
                                           KPI_boks_uden_nogletal(titel = "Sager om uenighed mellem kommunerne",
                                                                 tekst = "Hvis to eller flere kommuner er uenige om deres forpligtelser, kan de indbringe sager om opholdskommune, pligten til at yde hjælp og mellemkommunal refusion for Ankestyrelsen.
                                                                         <br>
                                                                         <br>",
                                                                 inputid = "mellemkommunal_id1",
                                                                 antal_sager = "666",
                                                                 subtitle = "antal sager i 2022",
                                                                 link = "mellemkommunal_knap_forside_ny", 
                                                                 link_tekst = "Gå til sager om mellemkommunal uenighed")
                         )
                        )
                        )
                        )
               ),
               tabPanel("Udbetaling Danmark-sager",
                        fluidRow(
                          column(4, align = "left",
                                 KPI_boks_med_nogletal(titel = "Udbetaling Danmark-sager",
                                                       tekst = "Sager, hvor Udbetaling Danmark har truffet den oprindelige afgørelse. Det er sager om bl.a. sociale ydelser, pension, boligstøtte, familieydelser og barselsdagpenge.",
                                                       inputid = "forside_udk_kpi_antal_sager",
                                                       antal_sager = "6.488",
                                                       periode = "2022",
                                                       inputid2 = "forside_udk_kpi_omgor",
                                                       noegletal = "10,5 %",
                                                       link = "UDK_knap_forside_ny", 
                                                       link_tekst = "Gå til Udbetaling Danmark-sager")
                          )
                        )
                        
               ),
               tabPanel("Børne- og ungesager",
                        fluidRow(
                          column(4, align = "left",
                                 KPI_boks_uden_nogletal(titel = "Underretningssager",
                                                        tekst = "Sager, hvor en borger eller en fagperson kontakter Ankestyrelsen om bekymring for et barn eller en ung.
                                                                 <br>
                                                                 <br>
                                                                 Det kan også være sager, hvor Ankestyrelsen på anden måde bliver opmærksom på bekymrende oplysninger om et barn eller en ung, fx på baggrund af medieomtale.
                                                                 <br>
                                                                 <br>",
                                                        inputid = "underretning_id1",
                                                        antal_sager = "1.373",
                                                        subtitle = "antal sager i 2022",
                                                        link = "underretning_knap_forside_ny", 
                                                        link_tekst = "Gå til underretningssager")),
                          column(4, align = "left",
                                 KPI_boks_uden_nogletal(titel = "Tvangsmæssige foranstaltninger",
                                                        tekst = "Sager om klager over tvangsmæssige foranstaltninger overfor børn og unge under 18 år, hvor kommunernes børn og unge-udvalg har truffet afgørelse i 1. instans.
                                                                <br>
                                                                <br>
                                                                Tvangsmæssige foranstaltninger kan bl.a. være anbringelsessager eller afgørelser der releterer sig til anbringelser. 
                                                                <br>
                                                                <br>",
                                                        inputid = "anbringelse_id1",
                                                        antal_sager = "1.545",
                                                        subtitle = "antal sager i 2022",
                                                        link = "anbringelse_knap_forside_ny", 
                                                        link_tekst = "Gå til tvangsmæssige foranstaltninger")          
                          ),
                          column(4, align = "left",
                                 KPI_boks_uden_nogletal(titel = "Sager om tvangsadoption (frigivelse)",
                                                        tekst = "Sager, hvor Ankestyrelsen har truffet afgørelse om adoption uden samtykke (frigivelse).
                                                                 <br>
                                                                 <br>
                                                                 Sagerne har forinden været behandlet i et kommunalt børn og unge-udvalg, der har indstillet til Ankestyrelsen at træffe afgørelse om bortadoption.
                                                                 <br>
                                                                 <br>",
                                                        inputid = "tvang_id1",
                                                        antal_sager = "45",
                                                        subtitle = "antal sager i 2022",
                                                        link = "tvang_knap_forside_ny", 
                                                        link_tekst = "Gå til sager om tvangsadoption")                             
                          )
                        )
               ),              
               tabPanel("Tilsynssager",
                        fluidRow(
                          column(4,
                                 KPI_boks_uden_nogletal(titel = "Sager behandlet af det kommunale og regionale tilsyn",
                                                        tekst = "Det kommunale og regionale tilsyn fører tilsyn med kommuner og regioner samt kommunale og regionale fællesskaber.
                                                                <br>
                                                                <br>Der bliver ført tilsyn med, at kommuner og regioner overholder den lovgivning, der særligt gælder for offentlige myndigheder.
                                                                <br>
                                                                <br>",
                                                        inputid = "forside_tilsyn_kpi_antal_sager",
                                                        antal_sager = "1.408",
                                                        subtitle = "antal sager i  2022",
                                                        link = "tilsyn_knap_forside", 
                                                        link_tekst = "Gå til tilsynssager")
                          )
                        )
                        ),
               tabPanel("Øvrige sager",
                        box(title = "Øvrige sager",
                            collapsible = FALSE,
                            collapsed = FALSE,
                            width = NULL,
                            #status = "primary",
                            solidHeader = TRUE,
                            tagList(
                              p("Ankestyrelsen behandler også sager om de sociale tilsyn, arbejdsløshedsdagpenge m.v. Statistik om disse sager er endnu ikke på talportalen. Se underside om publikationer på ast.dk eller kontakt os på statistik@ast.dk"),
                              tags$a(href="https://ast.dk/publikationer/emneopdelte-publikationer", "Publikationer", target="_blank")
                            )
                            
                        )
                        
                        
                        )
             )
             )
         )
 ),

# KSB --------------------------------------------------------

    tabItem(tabName = "subitem6",
            fluidRow(
    # Den totale bredde af applikationen
             column(12,
                    fluidRow(
                            column(12,
                                    # giver det mening af tilknytte subtext (eks. med antal sager)? Eller en subtext med, hvornår til og fra, at sagsemner er brugt
                                 blaa_boks("Klager over kommunernes afgørelser inden for social- og beskæftigelsesområdet", FALSE,
                                 fluidRow(
                                   column(4,
                                          box(
                                            width = NULL,
                                            solidHeader = TRUE,
                                            #height = 145,
                                            status = 'primary',
                                            title = span("1: Vælg lovgivning", icon("search")),
                                            
                                            column(4, 
                                                   radioButtons("lov_el_paragraf", label = "Udvælg",
                                                                choices = list("Paragraffer" = 1,
                                                                               "Lovgivninger" = 2)
                                                   )
                                                   ),
                                            column(8,
                                            pickerInput(
                                              inputId = 'in_lovgivning', 
                                              label = 'Lovgivning',
                                              choices = lapply(split(as.character(Sagsemner$Sagsemne), Sagsemner$Lovgivning), as.list),
                                              multiple=TRUE,
                                              options = inputpicker_options("lovgivning", "lovgivning, navn eller paragraf", "contains"),
                                              choicesOpt = list(tokens = Sagsemner$X
                                                                
                                                                #subtext = c('Indgår i danmarkskortet - Børnehandicap','Indgår i danmarkskortet - Børnehandicap')
                                              )
                                              
                                            )
                                            
                                          )
                                   )
                                   ),
                                   column(4,
                                         box(
                                         width = NULL,
                                         # background = "light-blue",
                                         solidHeader = TRUE,
                                         status = 'primary',
                                         # title = '2: Vælg kommune',
                                         title = span("2: Vælg kommune", icon("search")),
                                         pickerInput(
                                           inputId = 'in_kommune', 
                                           #label = 'Kommune',
                                           choices = lapply(split(as.character(kommuner$Kommune), kommuner$Region), as.list), # laver opdeling af kommuner på regioner
                                           #choices = lapply(split(glue(as.character(kommuner$Kommune), "=",as.character(kommuner$Kommune)), kommuner$Region), as.list), 
                                           multiple=TRUE, 
                                           #selected = list('Københavns Kommune', 'Frederiksberg Kommune'),
                                           options = inputpicker_options("kommune", "kommune eller region", "startsWith"),
                                           choicesOpt = list(tokens = kommuner$Region
                                                             #subtext = c('Indgår i danmarkskortet - Børnehandicap','Indgår i danmarkskortet - Børnehandicap')
                                                             )
                                           ),
                                         materialSwitch(
                                           inputId = "in_landstotal",
                                           label = "Tilføj landstotal", 
                                           value = FALSE,
                                           status = "primary"
                                         
                                         )
                                         # ,
                                        #  verbatimTextOutput("procent_kolonner_test"),
                                         # verbatimTextOutput("mangler_der_data")
                                         )
                                  )
                                  
                                 
                                              

                                 ,
                                 
                                 column(4,
                                  box(
                                    width = NULL,
                                    solidHeader = TRUE,
                                    # title = '3: Vælg periode',
                                    title = span('3: Vælg periode', icon("search")),
                                    status = 'primary',
                                   column(4, 
                                        radioButtons("aar_el_kvartal", label = "Periode",
                                                     choices = list("År" = 1,
                                                                    "Kvartal" = 2)
                                        )
                                        
                                        # radioGroupButtons(
                                        #   inputId = "aar_el_kvartal",
                                        #   label = "Periode", 
                                        #   choices = list("År" = 1,
                                        #                            "Kvartal" = 2),
                                        #   status = "primary"
                                        # )
                                        
                                   
                                        
                                        
                                 ),
                                 
                                 
                                  column(8,
                                         
                                         pickerInput('in_periode', 'Periode', 
                                                     choices = "",                                                    
                                                     multiple=TRUE, 
                                                     options = inputpicker_options("periode", "år eller kvartal", "contains")
                                                    )
                                        )#,
                                 # column(12,
                                 # p(html("Senest opdateret: <br>
                                 #   År: 2020, Kvartal: 2. kvt. 2021")))
                                  )
                                 
                                 
                                 
                                 )
                                       )
                                 ,
                                 fluidRow(
                                   column(12, align = "right",
                                          actionButton("dk_kort", "Info om SÆM's danmarkskort", icon = icon("map")),
                                          span(actionButton("nulstil_filtre", "Nulstil filtre", icon = icon("eraser")), style = "postition:absolute;right:2em;")
                                          )),
                                 fluidRow(align = "center",
                                          uiOutput("ikke_nok_valg"),
                                          uiOutput("download_stor"),
                                          uiOutput("for_mange_valgt")
                                          )
                                 
                                 # herfra
                                 
                                 # fluidRow(
                                 # column(4,
                                 #        # box(
                                 #        # width = NULL,
                                 #        # h4("Realitetsbehandlede sager")
                                 #        # ,
                                 #        box(
                                 #        background = "light-blue",
                                 #        width = NULL,
                                 #        #span(h4("Antal sager"), icon("question")),
                                 #        #h2(""),
                                 #        
                                 #        h4(HTML("Antal sager  ", "<font size='2'>",
                                 #                as.character(actionLink(inputId = "show", 
                                 #                                        label = "", 
                                 #                                        icon = icon("info"))), "</font>")),
                                 #        
                                 #        #actionButton("show", "Info"),
                                 #        checkboxGroupInput("antal_sager1",
                                 #                           label = NULL,
                                 #                           choices = list("Realitetsbehandlede sager i alt" = 'Realitetsbehandlede sager i alt'),
                                 #                           selected = 'Realitetsbehandlede sager i alt'
                                 #        ),
                                 #        tags$div(id = 'my_div2',
                                 #                 class = 'uncle',
                                 #        checkboxGroupInput("antal_sager2",
                                 #                           label = NULL,
                                 #                           #label = h4("Realitetsbehandlede sager"), 
                                 #                           choices = list("Stadfæstede sager" = "Stadfæstede sager", 
                                 #                                          "Ændrede/ophævede sager" = "Ændrede/ophævede sager",
                                 #                                          "Hjemviste sager" = "Hjemviste sager")
                                 #                           )
                                 #        
                                 #        )
                                 #        #bsTooltip(id = "nogletal_info2", "Omgørelsesprocenten er antallet af hjemviste og ændrede sager set i forhold til alle realitetsbehandlede sager.", "bottom", options = NULL)
                                 #        ,
                                 #        checkboxGroupInput("antal_sager3",
                                 #                           label = NULL,
                                 #                           #label = h4("Afviste sager"), 
                                 #                           choices = list("Afviste sager" = "Afviste sager")
                                 #        )
                                 #        # ,
                                 #        # verbatimTextOutput("antal_sager_valgte"),
                                 #        # verbatimTextOutput("antal_sager_valgte_periode")
                                 #        ),
                                 #        ),
                                 # column(4,
                                 #        box(
                                 #        background = "light-blue",
                                 #        width = NULL,
                                 #        #heigth = 100,
                                 #        # h4("Nøgletal"),
                                 #        # h2(""),
                                 #        h4(HTML("Nøgletal  ", "<font size='2'>",
                                 #                as.character(actionLink(inputId = "nogletal", 
                                 #                                        label = "", 
                                 #                                        icon = icon("info"))), "</font>")),
                                 #        #actionButton("nogletal", "Info"),
                                 #        checkboxGroupInput("nogletal1",
                                 #                           label = NULL,
                                 #                           choices = list("Stadfæstelsesprocent (%)" = "Stadfæstelsesprocent",
                                 #                                          "Omgørelsesprocent (%)" = "Omgørelsesprocent"),
                                 #                           #selected = "Omgørelsesprocent"
                                 #                           ),
                                 #        tags$div(id = 'my_div3',
                                 #                 class = 'uncle',
                                 #        checkboxGroupInput("nogletal2",
                                 #                           label = NULL,
                                 #                           choices = list("Ændrings-/ophævelsesprocent (%)" = "Ændrings-/ophævelsesprocent",
                                 #                                          "Hjemvisningsprocent (%)" = "Hjemvisningsprocent")
                                 #                           ))
                                 #        ,
                                 #        # checkboxGroupInput("nogletal3",
                                 #        #                    label = NULL,
                                 #        #                    choices = list("Sager pr. 10.000 indbyggere" = "Sager pr. 10.000 indbyggere")
                                 #        # )
                                 #        # ,
                                 #        # verbatimTextOutput("nogletal_valgte_32"),
                                 #        # verbatimTextOutput("nogletal_valgte_32_periode")
                                 #        #bsTooltip(id = "nogletal_info", "Omgørelsesprocenten er antallet af hjemviste og ændrede sager set i forhold til alle realitetsbehandlede sager.", "bottom", options = NULL)
                                 # ))
                                 # ,
                                 # column(4,
                                 # 
                                 #        # textOutput("selected_var"),
                                 #        # textOutput("selected_var_antal_raekke")
                                 #        #h4("Info"),
                                 #        actionButton("dk_kort", "Info om danmarkskortene", icon = icon("map")),
                                 #        span(actionButton("nulstil_filtre", "Nulstil filtre", icon = icon("eraser")), style = "postition:absolute;right:2em;")
                                 #        #verbatimTextOutput("antal_kombinationer")
                                 #        #actionButton("refresh", "Nulstil filtre")
                                 #        #actionButton("show", "Om antal sager"),
                                 #        #actionButton("nogletal", "Om nøgletal"),
                                 #        
                                 #        )
                                 # )
                                 
                                 # hertil
                                 
                                 
                                 )
                                 
                                  
                             )
                    
                    )
                  
                  , fluidRow(  
                      column(12, 
                        tabBox(
                               id = 'ksb_tabs',
                               title = tagList( " "),
                               width = NULL,
                                  tabPanel("Tabel",
                                           fluidPage(
                                             column(10,
                                                    
                                                    fluidRow(
                                                       box(
                                                       width = NULL,
                                                       background = "light-blue",
                                                       #status = "primary",
                                                       #solidHeader = TRUE,
                                                       #title = "4: Hvad skal vises?",
                                                      # status = "primary",
                                                      # title = "Hvad skal vises?",
                                                      # solidHeader = TRUE,
                                                      column(4,
                                                             #box(
                                                               #background = "light-blue",
                                                               #width = NULL,
                                                               #span(h4("Antal sager"), icon("question")),
                                                               #h2(""),
                                                               
                                                               h4(HTML("Antal sager", "<font size='2'>",
                                                                       as.character(actionLink(inputId = "show", 
                                                                                               label = "", 
                                                                                               icon = icon("info"))), "</font>")),
                                                               
                                                               #actionButton("show", "Info"),
                                                               checkboxGroupInput("antal_sager1",
                                                                                  label = NULL,
                                                                                  choices = list("Realitetsbehandlede sager i alt" = 'Realitetsbehandlede sager i alt'),
                                                                                  selected = 'Realitetsbehandlede sager i alt'
                                                               ),
                                                               tags$div(id = 'my_div2',
                                                                        class = 'uncle',
                                                                        checkboxGroupInput("antal_sager2",
                                                                                           label = NULL,
                                                                                           #label = h4("Realitetsbehandlede sager"), 
                                                                                           choices = list("Stadfæstede sager" = "Stadfæstede sager", 
                                                                                                          "Ændrede/ophævede sager" = "Ændrede/ophævede sager",
                                                                                                          "Hjemviste sager" = "Hjemviste sager")
                                                                        )
                                                                        
                                                               )
                                                               #bsTooltip(id = "nogletal_info2", "Omgørelsesprocenten er antallet af hjemviste og ændrede sager set i forhold til alle realitetsbehandlede sager.", "bottom", options = NULL)
                                                               ,
                                                               checkboxGroupInput("antal_sager3",
                                                                                  label = NULL,
                                                                                  #label = h4("Afviste sager"), 
                                                                                  choices = list("Afviste sager" = "Afviste sager")
                                                               )
                                                               # ,
                                                               # verbatimTextOutput("antal_sager_valgte"),
                                                               # verbatimTextOutput("antal_sager_valgte_periode")
                                                             #)
                                                             ),
                                                      column(4,
                                                             #box(
                                                               #background = "light-blue",
                                                               #width = NULL,
                                                               #heigth = 100,
                                                               # h4("Nøgletal"),
                                                               # h2(""),
                                                               h4(HTML("Nøgletal  ", "<font size='2'>",
                                                                       as.character(actionLink(inputId = "nogletal", 
                                                                                               label = "", 
                                                                                               icon = icon("info"))), "</font>")),
                                                               #actionButton("nogletal", "Info"),
                                                               checkboxGroupInput("nogletal1",
                                                                                  label = NULL,
                                                                                  choices = list("Stadfæstelsesprocent (%)" = "Stadfæstelsesprocent",
                                                                                                 "Omgørelsesprocent (%)" = "Omgørelsesprocent"),
                                                                                  #selected = "Omgørelsesprocent"
                                                               ),
                                                               tags$div(id = 'my_div3',
                                                                        class = 'uncle',
                                                                        checkboxGroupInput("nogletal2",
                                                                                           label = NULL,
                                                                                           choices = list("Ændrings-/ophævelsesprocent (%)" = "Ændrings-/ophævelsesprocent",
                                                                                                          "Hjemvisningsprocent (%)" = "Hjemvisningsprocent")
                                                                        ))
                                                               ,
                                                               # checkboxGroupInput("nogletal3",
                                                               #                    label = NULL,
                                                               #                    choices = list("Sager pr. 10.000 indbyggere" = "Sager pr. 10.000 indbyggere")
                                                               # )
                                                               # ,
                                                               # verbatimTextOutput("nogletal_valgte_32"),
                                                               # verbatimTextOutput("nogletal_valgte_32_periode")
                                                               #bsTooltip(id = "nogletal_info", "Omgørelsesprocenten er antallet af hjemviste og ændrede sager set i forhold til alle realitetsbehandlede sager.", "bottom", options = NULL)
                                                             #)
                                                             
                                                      )
                                                      # column(4,
                                                      #        h4(HTML("Sagsbehandlingstider  ", "<font size='2'>",
                                                      #                as.character(actionLink(inputId = "sagsbehandlingstider", 
                                                      #                                        label = "", 
                                                      #                                        icon = icon("info"))), "</font>")),
                                                      #        checkboxGroupInput("sagsbehandlingstider1",
                                                      #                           label = NULL,
                                                      #                           choices = list("Sagsbehandlingstider (antal uger)" = "Sagsbehandlingstider")
                                                      #                           
                                                      #                           
                                                      #        )
                                                      #        )
                                                         
                                                             )
                                                      
                                                    ),
                                                    fluidRow(
                                                      column(12,
                                                             #uiOutput("ikke_nok_valg"),
                                                             withSpinner(gt_output('table2'), type = 8, proxy.height = 150),
                                                             #uiOutput("download_stor"),
                                                             #uiOutput("for_mange_valgt"),
                                                             )
                                                    )
                                                    ),
                                             column(2,
                                                    # actionButton("dk_kort", "Info om danmarkskortene", icon = icon("map")),
                                                    # h4(""),
                                                    # span(actionButton("nulstil_filtre", "Nulstil filtre", icon = icon("eraser")), style = "postition:absolute;right:2em;"),
                                                    # h4(""),
                                                    uiOutput("moreControls"),
                                                    # box(title = "Eksport af tabel/data",
                                                    #     width = NULL,
                                                    #     status = "danger",
                                                    #     solidHeader = TRUE,
                                                    #     collapsible = TRUE,
                                                    #     collapsed = TRUE,
                                                    #     downloadButton("downloadData_2", "PNG (billede)"),
                                                    #     #downloadButton("downloadData", "CSV"),
                                                    #     downloadButton("downloadData_xlsx", "XLSX (Excel)"),
                                                    #     # actionGroupButtons(
                                                    #     #   inputIds = c("btn1", "btn2", "btn3"),
                                                    #     #   labels = list("PNG (billede)", "CSV", tags$span(icon("file-excel"), "XLSX (Excel)")),
                                                    #     #   status = "primary"
                                                    #     # )
                                                    #     # actionButton("btn3", "Download tabel som billede"),
                                                    #     # actionButton("bt4", "Download data (Excel el. CSV)")
                                                    # ),
                                                    graa_boks("Tilpas tabellen", FALSE,
                                                              radioButtons('in_pivot', 'Som kolonner', 
                                                                           choices = c('Nøgletal - herunder år' = 1,
                                                                                       'År - herunder nøgletal' = 2)),
                                                              radioButtons('in_pivot2', 'Som rækker', 
                                                                           choices = c('Kommune - herunder lovgivning' = 2,
                                                                                       'Lovgivning - herunder kommune' = 1)
                                                              ),
                                                              radioButtons('noter_ja_nej', 'Skal der vises fodnoter?',
                                                                           choices = c('Ja', 'Nej')),
                                                              radioButtons('i_alt', 'Vis i alt',
                                                                           choices = c('Ja' = 1,
                                                                                       'Nej' = 2))
                                                              # ,
                                                              # actionButton("show", "Show modal dialog")
                                                    )
                                                    )
                                                    )
                                             
                                             
                                             
                                           # ,
                                           # 
                                           # fluidRow(
                                           #   column(4,
                                                    # box(
                                                    # width = NULL,
                                                    # h4("Realitetsbehandlede sager")
                                                    # ,
                                                    # box(
                                                    #   background = "light-blue",
                                                    #   width = NULL,
                                                    #   #span(h4("Antal sager"), icon("question")),
                                                    #   #h2(""),
                                                    #   
                                                    #   h4(HTML("Antal sager  ", "<font size='2'>",
                                                    #           as.character(actionLink(inputId = "show", 
                                                    #                                   label = "", 
                                                    #                                   icon = icon("info"))), "</font>")),
                                                    #   
                                                    #   #actionButton("show", "Info"),
                                                    #   checkboxGroupInput("antal_sager1",
                                                    #                      label = NULL,
                                                    #                      choices = list("Realitetsbehandlede sager i alt" = 'Realitetsbehandlede sager i alt'),
                                                    #                      selected = 'Realitetsbehandlede sager i alt'
                                                    #   ),
                                                    #   tags$div(id = 'my_div2',
                                                    #            class = 'uncle',
                                                    #            checkboxGroupInput("antal_sager2",
                                                    #                               label = NULL,
                                                    #                               #label = h4("Realitetsbehandlede sager"), 
                                                    #                               choices = list("Stadfæstede sager" = "Stadfæstede sager", 
                                                    #                                              "Ændrede/ophævede sager" = "Ændrede/ophævede sager",
                                                    #                                              "Hjemviste sager" = "Hjemviste sager")
                                                    #            )
                                                    #            
                                                    #   )
                                                    #   #bsTooltip(id = "nogletal_info2", "Omgørelsesprocenten er antallet af hjemviste og ændrede sager set i forhold til alle realitetsbehandlede sager.", "bottom", options = NULL)
                                                    #   ,
                                                    #   checkboxGroupInput("antal_sager3",
                                                    #                      label = NULL,
                                                    #                      #label = h4("Afviste sager"), 
                                                    #                      choices = list("Afviste sager" = "Afviste sager")
                                                    #   )
                                                    #   # ,
                                                    #   # verbatimTextOutput("antal_sager_valgte"),
                                                    #   # verbatimTextOutput("antal_sager_valgte_periode")
                                                    # )
                                                    
                                                    
                                                    
                                             # ),
                                             # column(4,
                                                    # box(
                                                    #   background = "light-blue",
                                                    #   width = NULL,
                                                    #   #heigth = 100,
                                                    #   # h4("Nøgletal"),
                                                    #   # h2(""),
                                                    #   h4(HTML("Nøgletal  ", "<font size='2'>",
                                                    #           as.character(actionLink(inputId = "nogletal", 
                                                    #                                   label = "", 
                                                    #                                   icon = icon("info"))), "</font>")),
                                                    #   #actionButton("nogletal", "Info"),
                                                    #   checkboxGroupInput("nogletal1",
                                                    #                      label = NULL,
                                                    #                      choices = list("Stadfæstelsesprocent (%)" = "Stadfæstelsesprocent",
                                                    #                                     "Omgørelsesprocent (%)" = "Omgørelsesprocent"),
                                                    #                      #selected = "Omgørelsesprocent"
                                                    #   ),
                                                    #   tags$div(id = 'my_div3',
                                                    #            class = 'uncle',
                                                    #            checkboxGroupInput("nogletal2",
                                                    #                               label = NULL,
                                                    #                               choices = list("Ændrings-/ophævelsesprocent (%)" = "Ændrings-/ophævelsesprocent",
                                                    #                                              "Hjemvisningsprocent (%)" = "Hjemvisningsprocent")
                                                    #            ))
                                                    #   ,
                                                    #   # checkboxGroupInput("nogletal3",
                                                    #   #                    label = NULL,
                                                    #   #                    choices = list("Sager pr. 10.000 indbyggere" = "Sager pr. 10.000 indbyggere")
                                                    #   # )
                                                    #   # ,
                                                    #   # verbatimTextOutput("nogletal_valgte_32"),
                                                    #   # verbatimTextOutput("nogletal_valgte_32_periode")
                                                    #   #bsTooltip(id = "nogletal_info", "Omgørelsesprocenten er antallet af hjemviste og ændrede sager set i forhold til alle realitetsbehandlede sager.", "bottom", options = NULL)
                                                    # )
                                                    
                                                    
                                                    
                                                    # )
                                             #,
                                             #column(4,
                                                    
                                                    # textOutput("selected_var"),
                                                    # textOutput("selected_var_antal_raekke")
                                                    #h4("Info"),
                                                    # actionButton("dk_kort", "Info om danmarkskortene", icon = icon("map")),
                                                    # span(actionButton("nulstil_filtre", "Nulstil filtre", icon = icon("eraser")), style = "postition:absolute;right:2em;")
                                                    #verbatimTextOutput("antal_kombinationer")
                                                    #actionButton("refresh", "Nulstil filtre")
                                                    #actionButton("show", "Om antal sager"),
                                                    #actionButton("nogletal", "Om nøgletal"),
                                                    
                                             #)
                                           # ),
                                           
                                           
                                           
                                           
                                           # uiOutput("moreControls"),
                                           
                                           
                                           # fluidRow(
                                           #   column(4, 
                                           #          graa_boks("Tilpas tabellen", TRUE,
                                           #                   radioButtons('in_pivot', 'Som kolonner', 
                                           #                                choices = c('Nøgletal - herunder år' = 1,
                                           #                                            'År - herunder nøgletal' = 2)),
                                           #                   radioButtons('in_pivot2', 'Som rækker', 
                                           #                                choices = c('Kommune - herunder lovgivning' = 1,
                                           #                                            'Lovgivning - herunder kommune' = 2)
                                           #                   ),
                                           #                   radioButtons('noter_ja_nej', 'Skal der vises fodnoter?',
                                           #                                choices = c('Ja', 'Nej')),
                                           #                   radioButtons('i_alt', 'Vis i alt',
                                           #                                choices = c('Ja' = 1,
                                           #                                            'Nej' = 2))
                                           #                   # ,
                                           #                   # actionButton("show", "Show modal dialog")
                                           #          )
                                           #   )
                                           #   ,
                                           #   column(4, 
                                           #          box(title = "Tilføj til tabellen",
                                           #              collapsible = TRUE,
                                           #              collapsed = TRUE,
                                           #              width = NULL,
                                           #              status = "danger",
                                           #              solidHeader = TRUE,
                                           #              
                                           #              pickerInput('benchmark_test', 'Sammenlign med', 
                                           #                          choices= list(
                                           #                            'Gennemsnit' = c('Landsgennemsnit','Region midt','Region syd', 'Region midt-nord'),
                                           #                            'Alle kommmuner i:' = c('Region midt',
                                           #                                                    'Region syd',
                                           #                                                    'Region midt-nord')
                                           #                          ),
                                           #                          multiple=TRUE, 
                                           #                          #selectize=TRUE,
                                           #                          selected = list('2017','2018','2019'),
                                           #                          options = inputpicker_options("sammenligning", "region", "contains")
                                           #              ),
                                           #              radioButtons('farver', 'Tilføj farver til nøgletal',
                                           #                           choices = c('Ja', 'Nej'),
                                           #                           selected = 'Nej')
                                           #              
                                           #              
                                           #              
                                           #          )),
                                           #   column(4,
                                           #          box(title = "Eksport af tabel/data", 
                                           #              width = NULL,
                                           #              status = "danger",
                                           #              solidHeader = TRUE,
                                           #              collapsible = TRUE,
                                           #              collapsed = TRUE,
                                           #              downloadButton("downloadData_2", "PNG (billede)"),
                                           #              downloadButton("downloadData", "CSV"),
                                           #              downloadButton("downloadData_xlsx", "XLSX")
                                           #          )
                                           #   )
                                           #  )
                                           
                                           
                                           # her
                                           
                                           
                                  
                                  # fluidRow(         
                                  #      column(10,
                                                # align = "center",
                                                # actionButton("export", "Eksport tabel/data"),
                                                # h4(""),
                                                # h4(""),
                                                # h4(""),
                                                # withSpinner(gt_output('table2'), type = 8, proxy.height = 150)
                                        #      )
                                       
                                       # ,
                                       # 
                                       # 
                                       # column(2,
                                       #        # uiOutput("moreControls"),
                                              # # box(title = "Eksport af tabel/data",
                                              # #     width = NULL,
                                              # #     status = "danger",
                                              # #     solidHeader = TRUE,
                                              # #     collapsible = TRUE,
                                              # #     collapsed = TRUE,
                                              # #     downloadButton("downloadData_2", "PNG (billede)"),
                                              # #     #downloadButton("downloadData", "CSV"),
                                              # #     downloadButton("downloadData_xlsx", "XLSX (Excel)"),
                                              # #     # actionGroupButtons(
                                              # #     #   inputIds = c("btn1", "btn2", "btn3"),
                                              # #     #   labels = list("PNG (billede)", "CSV", tags$span(icon("file-excel"), "XLSX (Excel)")),
                                              # #     #   status = "primary"
                                              # #     # )
                                              # #     # actionButton("btn3", "Download tabel som billede"),
                                              # #     # actionButton("bt4", "Download data (Excel el. CSV)")
                                              # # ),
                                              # graa_boks("Tilpas tabellen", FALSE,
                                              #           radioButtons('in_pivot', 'Som kolonner', 
                                              #                        choices = c('Nøgletal - herunder år' = 1,
                                              #                                    'År - herunder nøgletal' = 2)),
                                              #           radioButtons('in_pivot2', 'Som rækker', 
                                              #                        choices = c('Kommune - herunder lovgivning' = 2,
                                              #                                    'Lovgivning - herunder kommune' = 1)
                                              #           ),
                                              #           radioButtons('noter_ja_nej', 'Skal der vises fodnoter?',
                                              #                        choices = c('Ja', 'Nej')),
                                              #           radioButtons('i_alt', 'Vis i alt',
                                              #                        choices = c('Ja' = 1,
                                              #                                    'Nej' = 2))
                                              #           # ,
                                              #           # actionButton("show", "Show modal dialog")
                                              # )
                                  #             
                                  #             )
                                  # 
                                  # )
                                  # , fluidRow(
                                  #   # (column(12, gt_output('total_tabel')))
                                  #   # verbatimTextOutput("strfile"),
                                  #   # verbatimTextOutput("strfile2"),
                                  #   # verbatimTextOutput("strfile3")
                                  # )
                                      )
                              ,
                              tabPanel("Graf",
                                fluidRow(
                                  column(9,
                                          pickerInput('input til graf', 'Vælg indhold af graf:',
                                                                  choices= list(
                                                                    'Antal sager' = c("Realitetsbehandlede sager i alt","Stadfæstede sager","Ændrede/ophævede sager", "Hjemviste sager", "Afviste sager"),
                                                                    'Nøgletal' = c('Stadfæstelsesprocent',
                                                                                            'Omgørelsesprocent',
                                                                                            'Ændrings-/ophævelsesprocent',
                                                                                            'Hjemvisningsprocent')),
                                                                   #,
                                                                    #'Sagsbehandlingstid' = 'Sagsbehandlingstid (uger)'
                                                                  #),
                                                                  multiple=FALSE,
                                                                  #selectize=TRUE,
                                                                  selected = ('Realitetsbehandlede sager i alt'),
                                                                  #options = inputpicker_options("sammenligning", "region", "contains")
                                                      ),

                                         
                                         
                                         
                                         
                                         
                                         
                                         #plotOutput("plot_omg"),
                                         htmlOutput("Vaelg_mindst_to_perioder"),
                                         uiOutput("sized_plot"),
                                         
                                  ),
                                  column(3,
                                         uiOutput("advarsel_ask"),
                                         box(
                                           title = "Hvordan skal grafen se ud?",
                                           width = NULL,
                                           status = "primary",
                                           solidHeader = TRUE,
                                           radioButtons('Pivot_graf_kommune_el_lovgivning', 'Opdel på kommune eller lovgivning',
                                                        choices = c('Flere kommuner i samme graf' = 1,
                                                                    'Flere lovgivninger i samme graf' = 2))
                                           # radioButtons('in_pivot_3', 'Kommuner',
                                           #                    choices = c('Alle i samme graf' = 1,
                                           #                                'Opdelte grafer' = 2)),
                                           # #uiOutput("in_pivot_3_x"),
                                           # radioButtons('in_pivot2_4', 'Lovgivning', 
                                           #                   choices = c('Alle i samme graf','Opdelte grafer'))
                                           ),
                                           box(title = "Eksport af graf som billede",
                                               width = NULL,
                                               status = "primary",
                                               solidHeader = TRUE,
                                           downloadButton("downloadData_2_alt", "Hent PNG-fil")                                           
                                         )
                                         )
                                )
                              )
                              )
                            )
                        )
                    )
                  )
          )
    ,

# UDK --------------------------------------------------

tabItem(tabName = "subitem7",
        # Den totale bredde af applikationen
        fluidRow(
        column(12,
               fluidRow(
                 column(12,
                        # giver det mening af tilknytte subtext (eks. med antal sager)? Eller en subtext med, hvornår til og fra, at sagsemner er brugt
                        blaa_boks("Udbetaling Danmark-sager: klager over Udbetaling Danmarks afgørelser", FALSE,
                                  fluidRow(
                                    column(6,
                                           box(
                                             width = NULL,
                                             solidHeader = TRUE,
                                             #height = 145,
                                             status = 'primary',
                                             title = span("1: Vælg lovgivning", icon("search")),

                                             column(4,
                                                    radioButtons("udk_lov_el_paragraf", label = "Udvælg",
                                                                 choices = list("Sagsområder" = 1,
                                                                                "Lovgivninger" = 2
                                                                                #,
                                                                                #"Sagsområder" = 3
                                                                 )
                                                    )
                                             ),
                                             column(8,
                                                    pickerInput(
                                                      inputId = 'udk_in_lovgivning',
                                                      label = 'Lovgivning',
                                                      choices = lapply(split(as.character(Sagsemner_udk$Sagsemne), Sagsemner_udk$Lovgrundlag), as.list),
                                                      multiple=TRUE,
                                                      options = inputpicker_options("lovgivning", "lovgivning, navn eller paragraf", "contains"),
                                                      choicesOpt = list(
                                                      subtext = Sagsemner_udk$Paragraffer
                                                      )
                                                      
                                                    )
                                                    #,
                                                    #verbatimTextOutput("udk_test_samling_2"),
                                                    #verbatimTextOutput("udk_test_af_dropdown2")
                                                    
                                                    
                                                    
                                                    
                                                    

                                             )
                                           )),
                                    column(6,
                                           box(
                                             width = NULL,
                                             solidHeader = TRUE,
                                             # title = '3: Vælg periode',
                                             title = span('2: Vælg periode', icon("search")),
                                             status = 'primary',
                                             column(4,
                                                    radioButtons("udk_aar_el_kvartal", label = "Periode",
                                                                 choices = list("År" = 1,
                                                                                "Kvartal" = 2)
                                                    )

                                                    # radioGroupButtons(
                                                    #   inputId = "aar_el_kvartal",
                                                    #   label = "Periode",
                                                    #   choices = list("År" = 1,
                                                    #                            "Kvartal" = 2),
                                                    #   status = "primary"
                                                    # )




                                             ),


                                             column(8,

                                                    pickerInput('udk_in_periode', 'Periode',
                                                                #choices = unique(ANKEAST5_spread_test$Aar),
                                                                # choices= list(
                                                                #   'Hele år' = c('2019' = '5. kvartal 2019',
                                                                #                 '2018' = '5. kvartal 2018',
                                                                #                 '2017' = '5. kvartal 2017'),
                                                                #   'Kvartaler' = c('4. kvartal 2019',
                                                                #                   '3. kvartal 2019',
                                                                #                   '2. kvartal 2019',
                                                                #                   '1. kvartal 2019',
                                                                #                   '4. kvartal 2018',
                                                                #                   '3. kvartal 2018',
                                                                #                   '2. kvartal 2018',
                                                                #                   '1. kvartal 2018',
                                                                #                   '4. kvartal 2017',
                                                                #                   '3. kvartal 2017',
                                                                #                   '2. kvartal 2017',
                                                                #                   '1. kvartal 2017')
                                                                # )
                                                                choices = "",
                                                                multiple=TRUE,
                                                                #selectize=TRUE,
                                                                #selected = list('2017','2018','2019'),
                                                                options = inputpicker_options("periode", "år eller kvartal", "contains")
                                                    )
                                             )
                                           ))
                                  )
                                  ,
                                  fluidRow(
                                    column(12, align = "right",
                                           #actionButton("dk_kort", "Info om SÆM's danmarkskort", icon = icon("map")),
                                           span(actionButton("udk_nulstil_filtre", "Nulstil filtre", icon = icon("eraser")), style = "postition:absolute;right:2em;")
                                    )),
                                  fluidRow(align = "center",
                                           uiOutput("ikke_nok_valg_udk")
                                           #,
                                           #uiOutput("udk_download_stor"),
                                           #uiOutput("udk_for_mange_valgt")
                                  )

                                  # herfra

                                  # fluidRow(
                                  # column(4,
                                  #        # box(
                                  #        # width = NULL,
                                  #        # h4("Realitetsbehandlede sager")
                                  #        # ,
                                  #        box(
                                  #        background = "light-blue",
                                  #        width = NULL,
                                  #        #span(h4("Antal sager"), icon("question")),
                                  #        #h2(""),
                                  #
                                  #        h4(HTML("Antal sager  ", "<font size='2'>",
                                  #                as.character(actionLink(inputId = "show",
                                  #                                        label = "",
                                  #                                        icon = icon("info"))), "</font>")),
                                  #
                                  #        #actionButton("show", "Info"),
                                  #        checkboxGroupInput("antal_sager1",
                                  #                           label = NULL,
                                  #                           choices = list("Realitetsbehandlede sager i alt" = 'Realitetsbehandlede sager i alt'),
                                  #                           selected = 'Realitetsbehandlede sager i alt'
                                  #        ),
                                  #        tags$div(id = 'my_div2',
                                  #                 class = 'uncle',
                                  #        checkboxGroupInput("antal_sager2",
                                  #                           label = NULL,
                                  #                           #label = h4("Realitetsbehandlede sager"),
                                  #                           choices = list("Stadfæstede sager" = "Stadfæstede sager",
                                  #                                          "Ændrede/ophævede sager" = "Ændrede/ophævede sager",
                                  #                                          "Hjemviste sager" = "Hjemviste sager")
                                  #                           )
                                  #
                                  #        )
                                  #        #bsTooltip(id = "nogletal_info2", "Omgørelsesprocenten er antallet af hjemviste og ændrede sager set i forhold til alle realitetsbehandlede sager.", "bottom", options = NULL)
                                  #        ,
                                  #        checkboxGroupInput("antal_sager3",
                                  #                           label = NULL,
                                  #                           #label = h4("Afviste sager"),
                                  #                           choices = list("Afviste sager" = "Afviste sager")
                                  #        )
                                  #        # ,
                                  #        # verbatimTextOutput("antal_sager_valgte"),
                                  #        # verbatimTextOutput("antal_sager_valgte_periode")
                                  #        ),
                                  #        ),
                                  # column(4,
                                  #        box(
                                  #        background = "light-blue",
                                  #        width = NULL,
                                  #        #heigth = 100,
                                  #        # h4("Nøgletal"),
                                  #        # h2(""),
                                  #        h4(HTML("Nøgletal  ", "<font size='2'>",
                                  #                as.character(actionLink(inputId = "nogletal",
                                  #                                        label = "",
                                  #                                        icon = icon("info"))), "</font>")),
                                  #        #actionButton("nogletal", "Info"),
                                  #        checkboxGroupInput("nogletal1",
                                  #                           label = NULL,
                                  #                           choices = list("Stadfæstelsesprocent (%)" = "Stadfæstelsesprocent",
                                  #                                          "Omgørelsesprocent (%)" = "Omgørelsesprocent"),
                                  #                           #selected = "Omgørelsesprocent"
                                  #                           ),
                                  #        tags$div(id = 'my_div3',
                                  #                 class = 'uncle',
                                  #        checkboxGroupInput("nogletal2",
                                  #                           label = NULL,
                                  #                           choices = list("Ændrings-/ophævelsesprocent (%)" = "Ændrings-/ophævelsesprocent",
                                  #                                          "Hjemvisningsprocent (%)" = "Hjemvisningsprocent")
                                  #                           ))
                                  #        ,
                                  #        # checkboxGroupInput("nogletal3",
                                  #        #                    label = NULL,
                                  #        #                    choices = list("Sager pr. 10.000 indbyggere" = "Sager pr. 10.000 indbyggere")
                                  #        # )
                                  #        # ,
                                  #        # verbatimTextOutput("nogletal_valgte_32"),
                                  #        # verbatimTextOutput("nogletal_valgte_32_periode")
                                  #        #bsTooltip(id = "nogletal_info", "Omgørelsesprocenten er antallet af hjemviste og ændrede sager set i forhold til alle realitetsbehandlede sager.", "bottom", options = NULL)
                                  # ))
                                  # ,
                                  # column(4,
                                  #
                                  #        # textOutput("selected_var"),
                                  #        # textOutput("selected_var_antal_raekke")
                                  #        #h4("Info"),
                                  #        actionButton("dk_kort", "Info om danmarkskortene", icon = icon("map")),
                                  #        span(actionButton("nulstil_filtre", "Nulstil filtre", icon = icon("eraser")), style = "postition:absolute;right:2em;")
                                  #        #verbatimTextOutput("antal_kombinationer")
                                  #        #actionButton("refresh", "Nulstil filtre")
                                  #        #actionButton("show", "Om antal sager"),
                                  #        #actionButton("nogletal", "Om nøgletal"),
                                  #
                                  #        )
                                  # )

                                  # hertil


                        )


                 )

               )

               , fluidRow(
                 column(12,
                        tabBox(
                          id = 'udk_tabs',
                          title = tagList( " "),
                          width = NULL,
                          tabPanel("Tabel",


                                   fluidPage(
                                     column(10,

                                            fluidRow(
                                              box(
                                                width = NULL,
                                                background = "light-blue",
                                                #status = "primary",
                                                #solidHeader = TRUE,
                                                #title = "4: Hvad skal vises?",
                                                # status = "primary",
                                                # title = "Hvad skal vises?",
                                                # solidHeader = TRUE,
                                                column(4,
                                                       #box(
                                                       #background = "light-blue",
                                                       #width = NULL,
                                                       #span(h4("Antal sager"), icon("question")),
                                                       #h2(""),

                                                       h4(HTML("Antal sager", "<font size='2'>",
                                                               as.character(actionLink(inputId = "udk_show",
                                                                                       label = "",
                                                                                       icon = icon("info"))), "</font>")),

                                                       #actionButton("show", "Info"),
                                                       checkboxGroupInput("udk_antal_sager1",
                                                                          label = NULL,
                                                                          choices = list("Realitetsbehandlede sager i alt" = 'Realitetsbehandlede sager i alt'),
                                                                          selected = 'Realitetsbehandlede sager i alt'
                                                       ),
                                                       tags$div(id = 'my_div2',
                                                                class = 'uncle',
                                                                checkboxGroupInput("udk_antal_sager2",
                                                                                   label = NULL,
                                                                                   #label = h4("Realitetsbehandlede sager"),
                                                                                   choices = list("Stadfæstede sager" = "Stadfæstede sager",
                                                                                                  "Ændrede/ophævede sager" = "Ændrede/ophævede sager",
                                                                                                  "Hjemviste sager" = "Hjemviste sager")
                                                                )

                                                       )
                                                       #bsTooltip(id = "nogletal_info2", "Omgørelsesprocenten er antallet af hjemviste og ændrede sager set i forhold til alle realitetsbehandlede sager.", "bottom", options = NULL)
                                                       ,
                                                       checkboxGroupInput("udk_antal_sager3",
                                                                          label = NULL,
                                                                          #label = h4("Afviste sager"),
                                                                          choices = list("Afviste sager" = "Afviste sager")
                                                       )
                                                       # ,
                                                       # verbatimTextOutput("antal_sager_valgte"),
                                                       # verbatimTextOutput("antal_sager_valgte_periode")
                                                       #)
                                                ),
                                                column(4,
                                                       #box(
                                                       #background = "light-blue",
                                                       #width = NULL,
                                                       #heigth = 100,
                                                       # h4("Nøgletal"),
                                                       # h2(""),
                                                       h4(HTML("Nøgletal  ", "<font size='2'>",
                                                               as.character(actionLink(inputId = "udk_nogletal",
                                                                                       label = "",
                                                                                       icon = icon("info"))), "</font>")),
                                                       #actionButton("nogletal", "Info"),
                                                       checkboxGroupInput("udk_nogletal1",
                                                                          label = NULL,
                                                                          choices = list("Stadfæstelsesprocent (%)" = "Stadfæstelsesprocent",
                                                                                         "Omgørelsesprocent (%)" = "Omgørelsesprocent"),
                                                                          #selected = "Omgørelsesprocent"
                                                       ),
                                                       tags$div(id = 'my_div3',
                                                                class = 'uncle',
                                                                checkboxGroupInput("udk_nogletal2",
                                                                                   label = NULL,
                                                                                   choices = list("Ændrings-/ophævelsesprocent (%)" = "Ændrings-/ophævelsesprocent",
                                                                                                  "Hjemvisningsprocent (%)" = "Hjemvisningsprocent")
                                                                ))
                                                )
                                                #,
                                                # column(4,
                                                #        h4(HTML("Sagsbehandlingstider  ", "<font size='2'>",
                                                #                as.character(actionLink(inputId = "udk_sagsbehandlingstider",
                                                #                                        label = "",
                                                #                                        icon = icon("info"))), "</font>")),
                                                #        checkboxGroupInput("udk_sagsbehandlingstider1",
                                                #                           label = NULL,
                                                #                           choices = list("Sagsbehandlingstider (antal uger)" = "Sagsbehandlingstid")
                                                # 
                                                # 
                                                #        )
                                                # 
                                                # )
                                              )

                                            ),
                                            fluidRow(
                                              column(12,
                                                     #uiOutput("ikke_nok_valg"),
                                                     withSpinner(gt_output('table2_udk'), type = 8, proxy.height = 150),
                                                     #uiOutput("download_stor"),
                                                     #uiOutput("for_mange_valgt"),
                                              )
                                            )
                                     ),
                                     column(2,
                                            # actionButton("dk_kort", "Info om danmarkskortene", icon = icon("map")),
                                            # h4(""),
                                            # span(actionButton("nulstil_filtre", "Nulstil filtre", icon = icon("eraser")), style = "postition:absolute;right:2em;"),
                                            # h4(""),
                                            uiOutput("udk_moreControls"),
                                            # box(title = "Eksport af tabel/data",
                                            #     width = NULL,
                                            #     status = "danger",
                                            #     solidHeader = TRUE,
                                            #     collapsible = TRUE,
                                            #     collapsed = TRUE,
                                            #     downloadButton("downloadData_2", "PNG (billede)"),
                                            #     #downloadButton("downloadData", "CSV"),
                                            #     downloadButton("downloadData_xlsx", "XLSX (Excel)"),
                                            #     # actionGroupButtons(
                                            #     #   inputIds = c("btn1", "btn2", "btn3"),
                                            #     #   labels = list("PNG (billede)", "CSV", tags$span(icon("file-excel"), "XLSX (Excel)")),
                                            #     #   status = "primary"
                                            #     # )
                                            #     # actionButton("btn3", "Download tabel som billede"),
                                            #     # actionButton("bt4", "Download data (Excel el. CSV)")
                                            # ),
                                            graa_boks("Tilpas tabellen", FALSE,
                                                      radioButtons('udk_in_pivot', 'Som kolonner',
                                                                   choices = c('Nøgletal - herunder år' = 1,
                                                                               'År - herunder nøgletal' = 2)),
                                                      #radioButtons('udk_in_pivot2', 'Som rækker',
                                                      #             choices = c('Kommune - herunder lovgivning' = 2,
                                                      #                         'Lovgivning - herunder kommune' = 1)
                                                      #),
                                                      #radioButtons('udk_noter_ja_nej', 'Skal der vises fodnoter?',
                                                      #             choices = c('Ja', 'Nej')),
                                                      radioButtons('udk_i_alt', 'Vis i alt',
                                                                   choices = c('Ja' = 1,
                                                                               'Nej' = 2))
                                                      # ,
                                                      # actionButton("show", "Show modal dialog")
                                            )
                                     )
                                   )
                          )
                          ,
                          tabPanel("Graf",
                                   fluidRow(
                                     column(9,
                                            pickerInput('udk_input til graf', 'Vælg indhold af graf:',
                                                        choices= list(
                                                          'Antal sager' = c("Realitetsbehandlede sager i alt","Stadfæstede sager","Ændrede/ophævede sager", "Hjemviste sager", "Afviste sager"),
                                                          'Nøgletal' = c('Stadfæstelsesprocent',
                                                                         'Omgørelsesprocent',
                                                                         'Ændrings-/ophævelsesprocent',
                                                                         'Hjemvisningsprocent')
                                                          #,
                                                          #'Sagsbehandlingstid (uger)' = 'Sagsbehandlingstid'
                                                        ),
                                                        multiple=FALSE,
                                                        #selectize=TRUE,
                                                        selected = ('Realitetsbehandlede sager i alt'),
                                                        #options = inputpicker_options("sammenligning", "region", "contains")
                                            ),







                                            plotOutput("udk_plot_omg"),
                                            htmlOutput("udk_Vaelg_mindst_to_perioder")
                                            #uiOutput("udk_sized_plot"),

                                     ),
                                     column(3,
                                            uiOutput("udk_advarsel"),
                                            # box(
                                            #   title = "Hvordan skal grafen se ud?",
                                            #   width = NULL,
                                            #   status = "primary",
                                            #   solidHeader = TRUE,
                                            #   radioButtons('udk_Pivot_graf_kommune_el_lovgivning', 'Opdel på kommune eller lovgivning',
                                            #                choices = c('Flere kommuner i samme graf' = 1,
                                            #                            'Flere lovgivninger i samme graf' = 2))
                                            #   # radioButtons('in_pivot_3', 'Kommuner',
                                            #   #                    choices = c('Alle i samme graf' = 1,
                                            #   #                                'Opdelte grafer' = 2)),
                                            #   # #uiOutput("in_pivot_3_x"),
                                            #   # radioButtons('in_pivot2_4', 'Lovgivning',
                                            #   #                   choices = c('Alle i samme graf','Opdelte grafer'))
                                            # ),
                                            box(title = "Eksport af graf som billede",
                                                width = NULL,
                                                status = "primary",
                                                solidHeader = TRUE,
                                                downloadButton("udk_downloadData_2_alt", "Hent PNG-fil")
                                            )
                                     )
                                   )
                          )
                        )
                 )
               )
        )

)),


# ASK ------------------------------------------

tabItem(tabName = "subitem10",
        fluidRow(
        # Den totale bredde af applikationen
        column(12,
               fluidRow(
                 column(12,
                        blaa_boks("Arbejdsskadesager: klager over Arbejdsmarkedets Erhverssikrings afgørelser", FALSE,
                                  fluidRow(
                                    column(6,
                                           box(
                                             width = NULL,
                                             solidHeader = TRUE,
                                             status = 'primary',
                                             title = span("1: Vælg sagsområde", icon("search")),
                                             column(12,
                                                    pickerInput(
                                                      inputId = 'ask_in_lovgivning',
                                                      label = 'Sagsområde',
                                                      choices = lapply(split(as.character(Sagsemner_ask$Sagsemne), Sagsemner_ask$Lovgrundlag), as.list),
                                                      multiple=TRUE,
                                                      options = inputpicker_options("sagsområde", "sagsområde", "contains"),
                                                      choicesOpt = list(
                                                      subtext = Sagsemner_ask$Paragraffer
                                                      )

                                                    )
                                             )
                                           )),
                                    column(6,
                                           box(
                                             width = NULL,
                                             solidHeader = TRUE,
                                             title = span('2: Vælg periode', icon("search")),
                                             status = 'primary',
                                             column(4,
                                                    radioButtons("ask_aar_el_kvartal", label = "Periode",
                                                                 choices = list("År" = 1,
                                                                                "Kvartal" = 2)
                                                    )
                                             ),
                                             column(8,
                                                    pickerInput('ask_in_periode', 'Periode',
                                                                choices = "",
                                                                multiple=TRUE,
                                                                options = inputpicker_options("periode", "år eller kvartal", "contains")
                                                    )
                                             )
                                           ))
                                  )
                                  ,
                                  fluidRow(
                                    column(12, align = "right",
                                           span(actionButton("ask_nulstil_filtre", "Nulstil filtre", icon = icon("eraser")), style = "postition:absolute;right:2em;")
                                    )),
                                  fluidRow(align = "center",
                                           uiOutput("ikke_nok_valg_ask")
                                  )
                        )
                 )
               )
               , fluidRow(
                 column(12,
                        tabBox(
                          id = "ask_tabs",
                          title = tagList( " "),
                          width = NULL,
                          tabPanel("Tabel",


                                   fluidPage(
                                     column(10,

                                            fluidRow(
                                              box(
                                                width = NULL,
                                                background = "light-blue",
                                                column(4,
                                                       h4(HTML("Antal sager", "<font size='2'>",
                                                               as.character(actionLink(inputId = "ask_show",
                                                                                       label = "",
                                                                                       icon = icon("info"))), "</font>")),
                                                       checkboxGroupInput("ask_antal_sager1",
                                                                          label = NULL,
                                                                          choices = list("Realitetsbehandlede sager i alt" = 'Realitetsbehandlede sager i alt'),
                                                                          selected = 'Realitetsbehandlede sager i alt'
                                                       ),
                                                       tags$div(id = 'my_div2',
                                                                class = 'uncle',
                                                                checkboxGroupInput("ask_antal_sager2",
                                                                                   label = NULL,
                                                                                   #label = h4("Realitetsbehandlede sager"),
                                                                                   choices = list("Stadfæstede sager" = "Stadfæstede sager",
                                                                                                  "Ændrede sager" = "Ændrede sager",
                                                                                                  "Hjemviste sager" = "Hjemviste sager",
                                                                                                  "Ophævede sager" = "Ophævede sager"
                                                                                   )
                                                                )

                                                       )
                                                       #bsTooltip(id = "nogletal_info2", "Omgørelsesprocenten er antallet af hjemviste og ændrede sager set i forhold til alle realitetsbehandlede sager.", "bottom", options = NULL)
                                                       ,
                                                       checkboxGroupInput("ask_antal_sager3",
                                                                          label = NULL,
                                                                          #label = h4("Afviste sager"),
                                                                          choices = list("Afviste sager" = "Afviste sager")
                                                       )
                                                       # ,
                                                       # verbatimTextOutput("antal_sager_valgte"),
                                                       # verbatimTextOutput("antal_sager_valgte_periode")
                                                       #)
                                                ),
                                                column(4,
                                                       #box(
                                                       #background = "light-blue",
                                                       #width = NULL,
                                                       #heigth = 100,
                                                       # h4("Nøgletal"),
                                                       # h2(""),
                                                       h4(HTML("Nøgletal  ", "<font size='2'>",
                                                               as.character(actionLink(inputId = "ask_nogletal",
                                                                                       label = "",
                                                                                       icon = icon("info"))), "</font>")),
                                                       #actionButton("nogletal", "Info"),
                                                       checkboxGroupInput("ask_nogletal1",
                                                                          label = NULL,
                                                                          choices = list("Stadfæstelsesprocent (%)" = "Stadfæstelsesprocent",
                                                                                         "Omgørelsesprocent (%)" = "Omgørelsesprocent"),
                                                                          #selected = "Omgørelsesprocent"
                                                       ),
                                                       tags$div(id = 'my_div3',
                                                                class = 'uncle',
                                                                checkboxGroupInput("ask_nogletal2",
                                                                                   label = NULL,
                                                                                   choices = list("Ændringsprocent (%)" = "Ændringsprocent",
                                                                                                  "Hjemvisningsprocent (%)" = "Hjemvisningsprocent",
                                                                                                  "Ophævelsesprocent (%)" = "Ophævelsesprocent")
                                                                ))
                                                )
                                                #,
                                                # column(4,
                                                #        h4(HTML("Sagsbehandlingstider  ", "<font size='2'>",
                                                #                as.character(actionLink(inputId = "ask_sagsbehandlingstider",
                                                #                                        label = "",
                                                #                                        icon = icon("info"))), "</font>")),
                                                #        checkboxGroupInput("ask_sagsbehandlingstider1",
                                                #                           label = NULL,
                                                #                           choices = list("Gns. sagsbehandlingstid (antal uger)" = "Sagsbehandlingstid")
                                                # 
                                                # 
                                                #        )
                                                # 
                                                # )
                                              )

                                            ),
                                            fluidRow(
                                              column(12,
                                                     #uiOutput("ikke_nok_valg"),
                                                     withSpinner(gt_output('ask_table2'), type = 8, proxy.height = 150),
                                                     html("<br>
                                                          <br>")
                                                     #,
                                                     #uiOutput("ask_note_boks"),
                                                     #verbatimTextOutput("kommentarer_rownames")
                                                     
                                                     #uiOutput("download_stor"),
                                                     #uiOutput("for_mange_valgt"),
                                              )
                                            )
                                     ),
                                     column(2,
                                            # actionButton("dk_kort", "Info om danmarkskortene", icon = icon("map")),
                                            # h4(""),
                                            # span(actionButton("nulstil_filtre", "Nulstil filtre", icon = icon("eraser")), style = "postition:absolute;right:2em;"),
                                            # h4(""),
                                            uiOutput("ask_moreControls"),
                                            # box(title = "Eksport af tabel/data",
                                            #     width = NULL,
                                            #     status = "danger",
                                            #     solidHeader = TRUE,
                                            #     collapsible = TRUE,
                                            #     collapsed = TRUE,
                                            #     downloadButton("downloadData_2", "PNG (billede)"),
                                            #     #downloadButton("downloadData", "CSV"),
                                            #     downloadButton("downloadData_xlsx", "XLSX (Excel)"),
                                            #     # actionGroupButtons(
                                            #     #   inputIds = c("btn1", "btn2", "btn3"),
                                            #     #   labels = list("PNG (billede)", "CSV", tags$span(icon("file-excel"), "XLSX (Excel)")),
                                            #     #   status = "primary"
                                            #     # )
                                            #     # actionButton("btn3", "Download tabel som billede"),
                                            #     # actionButton("bt4", "Download data (Excel el. CSV)")
                                            # ),
                                            graa_boks("Tilpas tabellen", FALSE,
                                                      radioButtons('ask_in_pivot', 'Som kolonner',
                                                                   choices = c('Nøgletal - herunder år' = 1,
                                                                               'År - herunder nøgletal' = 2)),
                                                      #radioButtons('ask_in_pivot2', 'Som rækker',
                                                      #             choices = c('Kommune - herunder lovgivning' = 2,
                                                      #                         'Lovgivning - herunder kommune' = 1)
                                                      #),
                                                      radioButtons('ask_noter_ja_nej', 'Skal der vises fodnoter?',
                                                                   choices = c('Ja', 'Nej')),
                                                      radioButtons('ask_i_alt', 'Vis i alt',
                                                                   choices = c('Ja' = 1,
                                                                               'Nej' = 2))
                                                      # ,
                                                      # actionButton("show", "Show modal dialog")
                                            )
                                     )
                                   )
                          )
                          ,
                          tabPanel("Graf",
                                   fluidRow(
                                     column(9,
                                            pickerInput('ask_input til graf', 'Vælg indhold af graf:',
                                                        choices= list(
                                                          'Antal sager' = c("Realitetsbehandlede sager i alt","Stadfæstede sager","Ændrede sager", "Hjemviste sager", "Ophævede sager", "Afviste sager"),
                                                          'Nøgletal' = c('Stadfæstelsesprocent',
                                                                         'Omgørelsesprocent',
                                                                         'Ændringsprocent',
                                                                         'Hjemvisningsprocent',
                                                                         'Ophævelsesprocent')
                                                          #,
                                                          #'Sagsbehandlingstid (uger)' = 'Sagsbehandlingstid'
                                                        ),
                                                        multiple=FALSE,
                                                        #selectize=TRUE,
                                                        selected = ('Realitetsbehandlede sager i alt'),
                                                        #options = inputpicker_options("sammenligning", "region", "contains")
                                            ),







                                            plotOutput("ask_plot_omg"),
                                            htmlOutput("ask_Vaelg_mindst_to_perioder"),
                                            #uiOutput("ask_sized_plot"),

                                     ),
                                     column(3,
                                            uiOutput("ask_advarsel"),
                                            # box(
                                            #   title = "Hvordan skal grafen se ud?",
                                            #   width = NULL,
                                            #   status = "primary",
                                            #   solidHeader = TRUE,
                                            #   radioButtons('ask_Pivot_graf_kommune_el_lovgivning', 'Opdel på kommune eller lovgivning',
                                            #                choices = c('Flere kommuner i samme graf' = 1,
                                            #                            'Flere lovgivninger i samme graf' = 2))
                                            #   # radioButtons('in_pivot_3', 'Kommuner',
                                            #   #                    choices = c('Alle i samme graf' = 1,
                                            #   #                                'Opdelte grafer' = 2)),
                                            #   # #uiOutput("in_pivot_3_x"),
                                            #   # radioButtons('in_pivot2_4', 'Lovgivning',
                                            #   #                   choices = c('Alle i samme graf','Opdelte grafer'))
                                            # ),
                                            box(title = "Eksport af graf som billede",
                                                width = NULL,
                                                status = "primary",
                                                solidHeader = TRUE,
                                                downloadButton("ask_downloadData_2_alt", "Hent PNG-fil")
                                            )
                                     )
                                   )
                          )
                        )
                 )
               )
        )

)),


# Tilsynssager ------------------------------------------------------------


tabItem(tabName = "Tilsynssager",
        fluidRow(
          column(12,
                blaa_boks("Sager behandlet af det kommunale og regionale tilsyn", FALSE,
                          fluidRow(
                             make_vaelg_emne_alt2(4, 1, "emne", "tilsyn_lovgrundlag", NULL),
                             make_vaelg_instans(4, 2, "instans", "tilsyn_kommune", "tilsyn_landstotal", instanser$fuldt_navn, instanser$Region),
                             make_valg_periode(4, 3, "tilsyn_aar_eller_kvartal", "tilsyn_periode")
                             #make_valg_kvartal(4, 3, "tilsyn_periode")
                           ),
                          fluidRow(
                            column(12, 
                                   align = "right",
                                   make_nulstil_knap("nulstil_filtre_tilsyn")
                            )
                          ),
                          fluidRow(
                            align = "center",
                            uiOutput("ikke_nok_valg_tilsyn")
                          )
                          ),
                tabBox(
                       id = "tilsyn_tabs",
                       title = tagList( " "),
                       width = NULL,
                       tabPanel("Tabel",
                                fluidPage(
                                  column(10,
                                         fluidRow(
                                         make_fyldt_blaa_boks(      
                                           column(4,
                                                  h4(HTML("Antal sager", "<font size='2'>",
                                                          as.character(actionLink(inputId = "info_antal_sager_tilsyn", 
                                                                                  label = "", 
                                                                                  icon = icon("info"))), "</font>")),
                                                  checkboxGroupInput("antal_sager1_tilsyn",
                                                                     label = NULL,
                                                                     choices = list("Behandlede sager i alt" = 'Behandlede sager i alt'),
                                                                     selected = 'Behandlede sager i alt'
                                                                     
                                                  ),
                                                  tags$div(id = 'my_div2_tilsyn',
                                                           class = 'uncle',
                                                           checkboxGroupInput("antal_sager2_tilsyn",
                                                                              label = NULL,
                                                                              choices = list("Udtalelse/godkendelse (§ 50)" = "Udtalelse",
                                                                                             "Afvisning (§ 48 a)" = "Afvisning", 
                                                                                             "Sanktion (§ 50 d-51)" = "Sanktion",
                                                                                             "Forhåndsudtalelse" = "Forhåndsudtalelse",
                                                                                             "Fogedsag" = "Fogedsag")
                                                           )
                                                  )
                                                  ,
                                                  checkboxGroupInput("antal_sager3_tilsyn",
                                                                     label = NULL,
                                                                       choices = list("Ikke-behandlede sager" = "Sag ikke behandlet")
                                                  )
                                           )
                                          ) 
                                         ),
                                         fluidRow(
                                           column(12,
                                                  withSpinner(gt_output("tabel_tilsyn"), type = 8, proxy.height = 150)
                                                  )
                                         )
                                         ),
                                  column(2,
                                         uiOutput("eksport_boks_tilsyn"),
                                         make_tilpas_tabellen(
                                           make_som_kolonner("som_kolonner_tilsyn"),
                                           make_som_raekker("som_raekker_tilsyn", 1),
                                           make_vises_fodnoter("vises_fodnoter_tilsyn"),
                                           make_vises_i_alt("vises_i_alt_tilsyn")
                                         )
                                         )
                                )
                                )
                       # ,
                       # tabPanel("Graf",
                       #          fluidRow(
                       #            column(9,
                       #                   pickerInput('input til graf_tilsyn', 'Vælg indhold af graf:',
                       #                               choices= list(
                       #                                 'Antal sager' = c("Realitetsbehandlede sager i alt","Stadfæstede sager","Ændrede/ophævede sager", "Hjemviste sager", "Afviste sager"),
                       #                                 'Nøgletal' = c('Stadfæstelsesprocent',
                       #                                                'Omgørelsesprocent',
                       #                                                'Ændrings-/ophævelsesprocent',
                       #                                                'Hjemvisningsprocent')),
                       #                               multiple=FALSE,
                       #                               selected = ('Realitetsbehandlede sager i alt'),
                       #                   ),
                       #                   #htmlOutput("Vaelg_mindst_to_perioder"),
                       #                   plotOutput("tilsyn_plot_omg"),
                       #            ),
                       #            column(3,
                       #                   #uiOutput("advarsel_ask"),
                       #                   make_hvordan_skal_grafen_se_ud("Pivot_graf_kommune_el_lovgivning_tilsyn"),
                       #                   make_eksport_graf("eksport_graf_tilsyn")
                       #            )
                       #          )
                       #          )
                       )
                 )
                 )),


# Mellemkommunal ----------------------------------------------------------

tabItem(tabName = "Mellemkommunal",
        fluidRow(
          column(12,
                 blaa_boks("Mellemkommunal uenighed", FALSE,
                           fluidRow(
                             make_vaelg_emne_alt(4, 1, "emne", "mellemk_lovgrundlag", list("Mellemkommunal refusion (retsikkerhedslovens § 9c)" = "Mellemkommunal refusion - § 9C",
                                                                                          "Opholdskommune (retsikkerhedslovens §§ 9-9b og 9d)" = "Opholdskommune - § 9-9b og 9d")
                                                        ),
                             make_vaelg_instans(4, 2, "kommune", "mellemk_kommune", "mellemk_landstotal", kommuner$fuldt_navn, kommuner$Region),
                             make_valg_periode(4, 3, "mellemk_aar_eller_kvartal", "mellemk_periode")
                             #make_valg_kvartal(4, 3, "mellemk_periode")
                           ),
                           fluidRow(
                             column(12, 
                                    align = "right",
                                    make_nulstil_knap("nulstil_filtre_mellemk")
                             )
                           ),
                           fluidRow(
                             align = "center",
                             uiOutput("ikke_nok_valg_mellemk")
                           )
                 ),
                 tabBox(
                   id = "mellemk_tabs",
                   title = tagList( " "),
                   width = NULL,
                   tabPanel("Tabel",
                            fluidPage(
                              column(10,
                                     fluidRow(
                                       make_fyldt_blaa_boks(      
                                         column(4,
                                                h4(HTML("Antal sager")
                                                        #, "<font size='2'>",
                                                        #as.character(actionLink(inputId = "info_antal_sager_mellemk", 
                                                        #                        label = "", 
                                                        #                        icon = icon("info"))), "</font>"
                                                        #)
                                                   ),
                                                checkboxGroupInput("antal_sager1_mellemk",
                                                                   label = NULL,
                                                                   choices = list("Behandlede sager i alt" = 'Realitetsbehandlede sager i alt'),
                                                                   selected = 'Realitetsbehandlede sager i alt'
                                                ),
                                                # tags$div(id = 'my_div2_mellemk',
                                                #          class = 'uncle',
                                                #          checkboxGroupInput("antal_sager2_mellemk",
                                                #                             label = NULL,
                                                #                             choices = list("Henlagt" = "Henlagt",
                                                #                                            "Bortfald" = "Bortfald", 
                                                #                                            "Realitetsbehandlet" = "Realitetsbehandlet")
                                                #          )
                                                # )
                                                # ,
                                                checkboxGroupInput("antal_sager2_mellemk",
                                                                   label = NULL,
                                                                   choices = list("Ikke-behandlede sager" = "Ikke-behandlede sager")
                                                )
                                         )
                                       ) 
                                     ),
                                     fluidRow(
                                       column(12,
                                              withSpinner(gt_output("tabel_mellemk"), type = 8, proxy.height = 150)
                                       )
                                     )
                              ),
                              column(2,
                                     uiOutput("eksport_boks_mellemk"),
                                     make_tilpas_tabellen(
                                       make_som_kolonner("som_kolonner_mellemk"),
                                       make_som_raekker("som_raekker_mellemk", 2),
                                       make_vises_fodnoter("vises_fodnoter_mellemk"),
                                       make_vises_i_alt("vises_i_alt_mellemk")
                                     )
                              )
                            )
                   )
                   #,
                   # tabPanel("Graf",
                   #          fluidRow(
                   #            column(9,
                   #                   pickerInput('input til graf_mellemk', 'Vælg indhold af graf:',
                   #                               choices= list(
                   #                                 'Antal sager' = c("Realitetsbehandlede sager i alt","Stadfæstede sager","Ændrede/ophævede sager", "Hjemviste sager", "Afviste sager"),
                   #                                 'Nøgletal' = c('Stadfæstelsesprocent',
                   #                                                'Omgørelsesprocent',
                   #                                                'Ændrings-/ophævelsesprocent',
                   #                                                'Hjemvisningsprocent')),
                   #                               multiple=FALSE,
                   #                               selected = ('Realitetsbehandlede sager i alt'),
                   #                   ),
                   #                   #htmlOutput("Vaelg_mindst_to_perioder"),
                   #                   plotOutput("mellemk_plot_omg"),
                   #            ),
                   #            column(3,
                   #                   #uiOutput("advarsel_ask"),
                   #                   make_hvordan_skal_grafen_se_ud("Pivot_graf_kommune_el_lovgivning_mellemk"),
                   #                   make_eksport_graf("eksport_graf_mellemk")
                   #            )
                   #          )
                   # )
                 )
          )
        )
),


# Underretningssager ------------------------------------------------------

tabItem(tabName = "Underretningssager",
        fluidRow(
          column(12,
                 blaa_boks("Underretningssager", FALSE,
                           fluidRow(
                             #make_vaelg_emne(4, 1, "underretning_lov_eller_paragraf", "underretning_lovgrundlag", Sagsemner$X),
                             #make_vaelg_instans(6, 1, "kommune", "underretning_kommune", "underretning_landstotal", kommuner$fuldt_navn, kommuner$Region),
                             #make_valg_periode(6, 1, "underretning_aar_eller_kvartal", "underretning_periode")
                             column(4,
                             box(title = "1. Lovgrundlag", 
                                 width = NULL, 
                                 solidHeader = TRUE, 
                                 status = "danger",
                                 selectInput("underretning_lovgrundlag_valgt",
                                             "Valgt lovgrundlag:",
                                             "Underretningssager",
                                             selected = "Hele landet"
                                 )
                             )),
                             column(4,
                             box(title = "2. Kommune", 
                                 width = NULL, 
                                 solidHeader = TRUE, 
                                 status = "danger",
                                 selectInput("underretning_kommune_valgt",
                                             "Valgt kommune:",
                                             "Hele landet",
                                             selected = "Hele landet"
                                             ))),
                             make_valg_aar(4, 3, "underretning_periode")
                           ),
                           fluidRow(
                             column(12, 
                                    align = "right",
                                    make_nulstil_knap("nulstil_filtre_underretning")
                             )
                           ),
                           fluidRow(
                             align = "center",
                             uiOutput("ikke_nok_valg_underretning")
                           )                           
                 ),
                 
                 tabBox(
                   id = "underretningssager_tabs",
                   title = tagList( " "),
                   width = NULL,
                   tabPanel("Tabel",
                            fluidPage(
                              column(10,
                                     fluidRow(
                                       make_fyldt_blaa_boks(      
                                         column(4,
                                                h4(HTML("Antal sager", "<font size='2'>",
                                                        as.character(actionLink(inputId = "info_antal_sager_underretning", 
                                                                                label = "", 
                                                                                icon = icon("info"))), "</font>")),
                                                checkboxGroupInput("antal_sager1_underretning",
                                                                   label = NULL,
                                                                   choices = list("Mødebehandlet" = "Mødebehandlet",
                                                                                      "Ikke-mødebehandlet" = "Ikke-mødebehandlet"),
                                                                   selected = list('Mødebehandlet',
                                                                                   "Ikke-mødebehandlet½")),
                                                 # tags$div(id = 'my_div2_mellemk',
                                                 #          class = 'uncle',
                                                 #          checkboxGroupInput("antal_sager2_underretning",
                                                 #                             label = NULL,
                                                 #                             choices = list("Mødebehandlet" = "Mødebehandlet",
                                                 #                                            "Ikke-mødebehandlet" = "Ikke.mødebehandlet") 
                                                 #          )
                                                 # )
                                                # ,
                                                #checkboxGroupInput("antal_sager3_underretning",
                                                #                   label = NULL,
                                                #                   choices = list("Ikke-behandlede sager" = "Ikke.realitetsbehandlet")
                                                #)
                                         )
                                       ) 
                                     ),
                                     fluidRow(
                                       column(12,
                                              withSpinner(gt_output("tabel_underretning"), type = 8, proxy.height = 150)
                                       )
                                     )
                              ),
                              column(2,
                                     uiOutput("eksport_boks_underretning"),
                                     make_tilpas_tabellen(
                                       make_som_kolonner("som_kolonner_underretning"),
                                       make_vises_fodnoter("vises_fodnoter_underretning")
                                       #,
                                       #make_vises_i_alt("vises_i_alt_underretning")
                                     )
                              )
                            )
                   )
                   # ,
                   # tabPanel("Graf",
                   #          fluidRow(
                   #            column(9,
                   #                   pickerInput('input til graf_underretning', 'Vælg indhold af graf:',
                   #                               choices= list(
                   #                                 'Antal sager' = c("Realitetsbehandlede sager i alt","Stadfæstede sager","Ændrede/ophævede sager", "Hjemviste sager", "Afviste sager"),
                   #                                 'Nøgletal' = c('Stadfæstelsesprocent',
                   #                                                'Omgørelsesprocent',
                   #                                                'Ændrings-/ophævelsesprocent',
                   #                                                'Hjemvisningsprocent')),
                   #                               multiple=FALSE,
                   #                               selected = ('Realitetsbehandlede sager i alt'),
                   #                   )
                   #                   #,
                   #                   #htmlOutput("Vaelg_mindst_to_perioder"),
                   #                   #plotOutput("mellemk_plot_omg"),
                   #            ),
                   #            column(3,
                   #                   #uiOutput("advarsel_ask"),
                   #                   make_hvordan_skal_grafen_se_ud("Pivot_graf_kommune_el_lovgivning_underretning"),
                   #                   make_eksport_graf("eksport_graf_underretning")
                   #            )
                   #          )
                   # )
                 )
                 
                 
                 
                 
                 
                 
                 
                 
                 
          
          )          
        )
),


# Tvangsbortadoption -----------------------------------------

tabItem(tabName = "Tvangsbortadoption",
        fluidRow(
          column(12,
                 blaa_boks("Sager om tvangsbortadoption", FALSE,
                           fluidRow(
                             make_vaelg_emne_alt(6, 1,"emne", "tvang_lovgrundlag", list("Tvangsadoption af børn (adoptionsloven § 9 stk. 3)" = "Tvangsbortadoption § 9, stk. 3",
                                                                                        "Tvangsadoption af anbragte børn (adoptionsloven § 9 stk. 4)" = "Tvangsbortadoption § 9, stk. 4")
                             ),
                             #make_vaelg_instans(4, 2, "kommune", "tvang_kommune", "tvang_landstotal", kommuner$fuldt_navn, kommuner$Region),
                             #make_valg_periode(6, 2, "tvang_aar_eller_kvartal", "tvang_periode"),
                             make_valg_aar(6, 2, "tvang_periode")
                           ),
                           fluidRow(
                             column(12, 
                                    align = "right",
                                    make_nulstil_knap("nulstil_filtre_tvang")
                             )
                           ),
                           fluidRow(
                             align = "center",
                             uiOutput("ikke_nok_valg_tvang")
                           )
                 ),
                 tabBox(
                   id = "tvangsbortadoption_tabs",
                   title = tagList( " "),
                   width = NULL,
                   tabPanel("Tabel",
                            fluidPage(
                              column(10,
                                     fluidRow(
                                       make_fyldt_blaa_boks(      
                                         column(4,
                                                h4(HTML("Antal sager", "<font size='2'>",
                                                        as.character(actionLink(inputId = "info_antal_sager_tvang", 
                                                                                label = "", 
                                                                                icon = icon("info"))), "</font>")),
                                                checkboxGroupInput("antal_sager1_tvang",
                                                                   label = NULL,
                                                                   choices = list("Tvangsadoption" = 'Tvangsbortadoption',
                                                                                  #"Afslag på tvangsadoption" = "Afslag.på.tvangsbortadoption",
                                                                                  "Afslag på tvangsadoption" = "Afslag på tvangsadoption",
                                                                                  "Hjemvisning" = "Hjemvisning"
                                                                                  ),
                                                                   selected = 'Tvangsbortadoption'
                                                )
                                         )
                                       ) 
                                     ),
                                     fluidRow(
                                       column(12,
                                              withSpinner(gt_output("tabel_tvang"), type = 8, proxy.height = 150)
                                       )
                                     )
                              ),
                              column(2,
                                     uiOutput("eksport_boks_tvang"),
                                     make_tilpas_tabellen(
                                       make_som_kolonner("som_kolonner_tvang"),
                                       #make_som_raekker("som_raekker_tvang", 2),
                                       #make_vises_fodnoter("vises_fodnoter_tvang"),
                                       make_vises_i_alt("vises_i_alt_tvang")
                                     )
                              )
                            )
                   )
                   # ,
                   # 
                   # tabPanel("Graf",
                   #          fluidRow(
                   #            column(9,
                   #                   pickerInput('input til graf_tvang', 'Vælg indhold af graf:',
                   #                               choices= list(
                   #                                 'Antal sager' = c("Realitetsbehandlede sager i alt","Stadfæstede sager","Ændrede/ophævede sager", "Hjemviste sager", "Afviste sager"),
                   #                                 'Nøgletal' = c('Stadfæstelsesprocent',
                   #                                                'Omgørelsesprocent',
                   #                                                'Ændrings-/ophævelsesprocent',
                   #                                                'Hjemvisningsprocent')),
                   #                               multiple=FALSE,
                   #                               selected = ('Realitetsbehandlede sager i alt'),
                   #                   )
                   #                   #,
                   #                   #htmlOutput("Vaelg_mindst_to_perioder"),
                   #                   #plotOutput("mellemk_plot_omg"),
                   #            ),
                   #            column(3,
                   #                   #uiOutput("advarsel_ask"),
                   #                   make_hvordan_skal_grafen_se_ud("Pivot_graf_kommune_el_lovgivning_tvang"),
                   #                   make_eksport_graf("eksport_graf_tvang")
                   #            )
                   #          )
                   # )
                 )
                 
          )
        )
),


# Anbringelsessager -------------------------------------------------------------

tabItem(tabName = "Anbringelse",
        fluidRow(
          column(12,
                 blaa_boks("Sager om tvangsmæssige foranstaltninger", FALSE,
                           fluidRow(
                             #make_vaelg_emne_alt(4, 1,"emne", "anbringelse_lovgrundlag", list("Tvangsadoption af børn (adoptionsloven § 9 stk. 3)" = "Tvangsbortadoption § 9, stk. 3",
                             #                                                            "Tvangsadoption af anbragte børn (adoptionsloven § 9 stk. 4)" = "Tvangsbortadoption § 9, stk. 4")
                             # ),
                             make_vaelg_instans(6, 1, "kommune", "anbringelse_kommune", "anbringelse_landstotal", kommuner$fuldt_navn, kommuner$Region),
                             #make_valg_periode(6, 2, "anbringelse_aar_eller_kvartal", "anbringelse_periode")
                             make_valg_aar(6, 2, "anbringelse_periode")
                           ),
                           fluidRow(
                             column(12, 
                                    align = "right",
                                    make_nulstil_knap("nulstil_filtre_anbringelse")
                             )
                           ),
                           fluidRow(
                             align = "center",
                             uiOutput("ikke_nok_valg_anbringelse")
                           )
                 ),
                 tabBox(
                   id = "anbringelse_tabs",
                   title = tagList( " "),
                   width = NULL,
                   tabPanel("Tabel",
                            fluidPage(
                              column(10,
                                     # fluidRow(
                                     #   make_fyldt_blaa_boks(      
                                     #     column(4,
                                     #            h4(HTML("Antal sager", "<font size='2'>",
                                     #                    as.character(actionLink(inputId = "info_antal_sager_anbringelse", 
                                     #                                            label = "", 
                                     #                                            icon = icon("info"))), "</font>")),
                                     #            checkboxGroupInput("antal_sager1_anbringelse",
                                     #                               label = NULL,
                                     #                               choices = list("Mødebehandlet" = 'Mødebehandlet'
                                     #                                              #,
                                     #                                              #"Ikke-behandlet" = "Ikke_mødebehandlet"
                                     #                               ),
                                     #                               selected = 'Mødebehandlet'
                                     #            )
                                     #     )
                                     #   ) 
                                     # ),
                                     fluidRow(
                                       column(12,
                                              withSpinner(gt_output("tabel_anbringelse"), type = 8, proxy.height = 150)
                                       )
                                     )
                              ),
                              column(2,
                                     uiOutput("eksport_boks_anbringelse"),
                                     make_tilpas_tabellen(
                                       make_som_kolonner("som_kolonner_anbringelse"),
                                       make_vises_fodnoter("vises_fodnoter_anbringelse")
                                     )
                              )
                            )
                   )
                   # ,
                   # tabPanel("Graf",
                   #          fluidRow(
                   #            column(9,
                   #                   pickerInput('input til graf_anbringelse', 'Vælg indhold af graf:',
                   #                               choices= list(
                   #                                 'Antal sager' = c("Realitetsbehandlede sager i alt","Stadfæstede sager","Ændrede/ophævede sager", "Hjemviste sager", "Afviste sager"),
                   #                                 'Nøgletal' = c('Stadfæstelsesprocent',
                   #                                                'Omgørelsesprocent',
                   #                                                'Ændrings-/ophævelsesprocent',
                   #                                                'Hjemvisningsprocent')),
                   #                               multiple=FALSE,
                   #                               selected = ('Realitetsbehandlede sager i alt'),
                   #                   )
                   #                   #,
                   #                   #htmlOutput("Vaelg_mindst_to_perioder"),
                   #                   #plotOutput("mellemk_plot_omg"),
                   #            ),
                   #            column(3,
                   #                   #uiOutput("advarsel_ask"),
                   #                   make_hvordan_skal_grafen_se_ud("Pivot_graf_kommune_el_lovgivning_anbringelse"),
                   #                   make_eksport_graf("eksport_graf_anbringelse")
                   #            )
                   #          )
                   # )
                 )   
                ) 
        )
),


tabItem(tabName = "Sagsudfald",
       fluidRow(

         )
  )





  )
)


# Samler header, sidebar og body -------------------------------------------------

ShinyUI <- dashboardPage(skin = "blue",
                    header,
                    sidebar,
                    body
)