################## SERVER ##################

# Umiddelbart er min tanke, at de serverelementer, der skal bruges flere steder, får sin egen reactive-
# element, hvorimod dem der knytter sig op til et specifikt element (eks. en tabel el. figur) er indeholdt
# i eks. render table

# Server functions --------------------------------------------------------

# Beregning af nøgletal

tilfoj_realitetsbehandlede_sager <- function (.) {
  mutate(.,`Realitetsbehandlede sager i alt` = (Antal_sager_ialt -  Afvisning.Henvisning))
}

filtrer_data <- function (data, instanser, sagsemner, perioder) {
  data %>%
  filter(Instans %in% all_of(instanser) & Sagsemne %in% all_of(sagsemner) & periode %in% all_of(perioder))
}

pivoter_data <- function (data, valgte_maal, pivot) {
  data %>%
  pivot_wider(
    names_from = 'periode',
    values_from = valgte_maal,
    names_glue = if (pivot == 2) "{periode};{.value}" else "{.value};{periode}")
}

tilfoej_i_alt_raekker <- function (data, datakolonner, input_lovgrundlag, vis_i_alt) {
      if(length(input_lovgrundlag) > 1 && vis_i_alt == TRUE) {
      i_alt_raekker <- aggregate(data[,datakolonner], by=list(Instans = data$Instans,
                                    periode = data$periode), FUN=sum, na.rm=TRUE) %>%
      mutate(Sagsemne = 'I alt')
      bind_rows(data, i_alt_raekker)
      } else
      data
}

tilfoej_i_alt_raekker_test <- function (data, input_lovgrundlag, vis_i_alt, landstotal) {
  if(length(input_lovgrundlag) > 1 && vis_i_alt == TRUE && !landstotal) {
    total <- data %>%
      group_by(Instans, periode) %>%
      summarise(across(-Sagsemne, sum)) %>%
      mutate(Sagsemne = 'I alt')
    bind_rows(data, total )
  } else
  data
  }

tilfoej_i_alt_raekker_alt <- function (data, datakolonner, input_sagsemne, landstotal, vis_i_alt) {
    if(length(input_sagsemne) > 1 && vis_i_alt == TRUE && !landstotal) {
    i_alt_raekker <- aggregate(data[,datakolonner], by=list(periode = data$periode), FUN=sum, na.rm=TRUE) %>%
      mutate(Instans = 'I alt')
    bind_rows(data, i_alt_raekker)
  } else
    data
}

dan_gt_tabel <- function (data, raekke, gruppe, titel, undertitel, fodnote) {
  #data_sorteret <- data[,c("Sagsemne", sortering)]
  gt(data, rowname_col = raekke, groupname_col = gruppe) %>%
    tab_spanner_delim(";") %>%
    tab_header(
      title = md(paste(titel,undertitel))
    ) %>%
    tab_source_note(md(paste("Kilde: Ankestyrelsen (",format(Sys.Date(), "%Y"),")"))) %>%
    tab_source_note(html(fodnote)) %>%
    cols_align(align = "right",
               columns = everything()) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_spanners(spanners = TRUE) # OBS: i koden til KSB er der et if-statement, som jeg ikke forstår
    ) %>%
    tab_options(
      row_group.font.weight = "bold"
    ) %>%
    fmt_missing (
      columns = everything(),
      missing_text = ".")
}

dan_graf <- function (data, input_til_graf, aar_eller_kvartal, pivot) {
    valg_til_graf <- as.character(input_til_graf)
    maks_vaerdi <- as.double(max(data$data_kolonne))
    ggplot(data, aes(x=as.factor(periode), y=data_kolonne, group = if(pivot == 1) Instans else Sagsemne)) +
      geom_line(aes(color= Instans), size = 1, na.rm = TRUE) +
      geom_point(aes(color= Instans), size = 3, na.rm = TRUE) +
      ylim(0, case_when(
        valg_til_graf == 'Omgørelsesprocent' ~ 100,
        valg_til_graf == 'Stadfæstelsesprocent' ~ 100,
        valg_til_graf == 'Hjemvisningsprocent' ~ 100,
        valg_til_graf == 'Ændrings-/ophævelsesprocent' ~ 100,
        TRUE ~ maks_vaerdi)) +
      theme_minimal() +
      labs(title = case_when(
        valg_til_graf == 'Omgørelsesprocent' ~ 'Andel omgjorte sager',
        valg_til_graf == 'Stadfæstelsesprocent' ~ 'Andel stadfæstede sager',
        valg_til_graf == 'Hjemvisningsprocent' ~ 'Andel hjemviste sager',
        valg_til_graf == 'Ændrings-/ophævelsesprocent' ~ 'Andel ændrede/ophævede sager',
        valg_til_graf == 'Realitetsbehandlede sager i alt' ~ 'Antal realitetsbehandlede sager i alt',
        valg_til_graf == 'Stadfæstede sager' ~ 'Antal stadfæstede sager',
        valg_til_graf == 'Ændrede/ophævede sager' ~ 'Antal ændrede/ophævede sager',
        valg_til_graf == 'Hjemviste sager' ~ 'Antal hjemviste sager',
        valg_til_graf == 'Afviste sager' ~ 'Antal afviste sager',
        TRUE ~ 'Øvrigt'),
        x = case_when(
          aar_eller_kvartal == 1 ~ "År",
          aar_eller_kvartal == 2 ~ "Kvartal",
          TRUE ~ "Periode"),
        y = valg_til_graf,
        color = if(pivot == 1) "Instans" else "Sagsemne") +
      theme(legend.position = "right",
            strip.text.x = element_text(size = 12, face = "bold"),
            text = element_text(size = 16),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap( ~ if(pivot == 1) Sagsemne else Instans)
}

tilfoej_nogletal <- function (data, noegletal) {
   switch(noegletal,
          "sager_i_alt" = mutate(data, samlet = Afvisning + Udtalelse),
          "sager_i_alt_100" = mutate(data, samlet = (Afvisning + Udtalelse)/100),
          "sager_u_alt" = mutate(data, samlet_u = Afvisning - Udtalelse),
          "sager_u_alt_100" = mutate(data, samlet_u = (Afvisning - Udtalelse)/100),
          "Realitetsbehandlede sager" = mutate(.,`Realitetsbehandlede sager i alt` = (Antal_sager_ialt -  Afvisning.Henvisning)),
          "Omgørelsesprocent" = mutate(.,Omgørelsesprocent = (replace_na(Ændring.Ophævelse,0) + replace_na(Hjemvisning,0)) / replace_na(`Realitetsbehandlede sager i alt`,0)),
          "Hjemvisningsprocent" = mutate(.,Hjemvisningsprocent = replace_na(Hjemvisning,0) / replace_na(`Realitetsbehandlede sager i alt`,0)),
          "Afvisningprocent" = mutate(.,Afvisningsprocent = replace_na(Afvisning.Henvisning,0) / replace_na(`Realitetsbehandlede sager i alt`,0)),
          "Stadfæstelsesprocent" = mutate(.,Stadfæstelsesprocent = replace_na(Stadfæstelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0)),
          "Ændrings-/ophævelsesprocent" = mutate(.,'Ændrings-/ophævelsesprocent' = replace_na(Ændring.Ophævelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0)),
          "Behandlede sager i alt" = mutate(data, "Behandlede sager i alt" = (Afvisning + Udtalelse + Forhåndsudtalelse + Fogedsag + Sanktion)),
          data # hvis nøgletal ikke matches
          )
 }

tilfoej_dimensionsvaerdier <- function (data, dimensionstabel, lov_eller_paragraf) {
  if(lov_eller_paragraf == 1) {
  data %>%
      inner_join(dimensionstabel, by = c("Sagsemne" = "Sorteringskode")) %>%
      select(-Sagsemne) %>%
      mutate(Sagsemne = paste(Sagsemne.y, " - ", Lovgrundlag, Paragraffer)) %>%
      #select(Sagsemne, Aar, Kvartal, Afvisning.Henvisning, Hjemvisning, Antal_sager_ialt, Sagsbehandlingstid, Stadfæstelse, Ændring.Ophævelse, periode) %>%
      select(-c("Sagsemne.y",Lovgrundlag, Paragraffer)) %>%
      arrange(Sagsemne)
  } else data
}

opslag_sortering <- function (maal, valgte_perioder, som_kolonner, antal_kolonner, procent_kolonner) {
    samlet_liste <- tibble(anker = paste(rep(if (som_kolonner == 2) rev(valgte_perioder) else maal,
                                              each = length(if (som_kolonner == 2) maal else valgte_perioder)),
                                          if (som_kolonner == 2) maal else rev(valgte_perioder),
                                          sep = ";")) %>%
    filter(anker %in% antal_kolonner | anker %in% procent_kolonner) %>%
    pull(anker)
}

kombiner_maal_og_periode <- function(maal, som_kolonne, perioder) {
  if (length(maal) > 0) {
    paste(rep(if (som_kolonne == 2) perioder else maal, each = length(perioder)), if (som_kolonne == 2) maal else perioder, sep = ";")
  } else {FALSE}
}

# ui-elementer

eksportboks <- function(id) {
box(title = "Eksport af tabel/data",
    width = NULL,
    status = "danger",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = FALSE,
    actionButton(id, "Vælg download", icon = icon("download"))
)
}

eksportmodal <- function(id, id2) {
  showModal(modalDialog(
    title = html("<strong>Eksport af tabel/data</strong>"),
    html("Vælg filtype:
    <br>
    "),
    downloadButton(id, "XLSX (Excel)"),
    downloadButton(id2, "CSV-fil"),
    html("<br>
          <br>
         <strong>OBS: det kan tage et øjeblik, før filen er hentet.</strong>"),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Annuller (ESC)")
    )
  ))
}

dan_excel_fil <- function(data, som_raekker, som_kolonner, periode, valgte_maal_og_noegletal, sortering) {
  #downloadHandler(
    #filename = "excelfil.xlsx",
    content = function(file) {
      Sys.sleep(0.3)
      removeModal()
      shiny::withProgress(message = 'Downloader excel-fil. Det kan tage et øjeblik.',
                            value = 0,   {

                            shiny::incProgress(1/10)
                            Sys.sleep(1)
                            shiny::incProgress(5/10)

                            wb <- createWorkbook()

                            # Danner fane
                            addWorksheet(wb, "Som tabel")


                            sortering1 <- if (som_raekker == 1) "Instans" else "Sagsemne"
                            sortering2 <- if (som_raekker == 1) "Sagsemne" else "Instans"

                            # Sorterer rækkefølgende af kolonnerne

                            grund_tabel <- data %>%
                              select("Sagsemne", "Instans", all_of(sortering)) %>%
                              select(all_of(sortering2), all_of(sortering1), everything()) %>%
                              arrange(.[,1])

                            grund_tabel[is.na(grund_tabel)] <- 0

                            x <- grund_tabel

                            # Hjælpevariable til opsætning af ark
                            antal_raekker2 <- if (som_kolonner == 1) {
                              length(periode)
                            } else {
                              length(valgte_maal_og_noegletal)
                            }

                            startkolonne <- 3

                            writeData(wb, "Som tabel", x, startCol = 1, startRow = 2, rowNames = FALSE) # indsætter data i fanen
                            setColWidths(wb, sheet = "Som tabel", cols = 1:ncol(x), widths = "auto") # Justerer kolonnebredden

                            # Laver kolonneoverskrifter og underoverskrifter
                            kolonnenavne <- colnames(x) # tager kolonnenavnene fra datasætter
                            kolonnenavne <- kolonnenavne[startkolonne:length(kolonnenavne)] # fjerner de første to
                            kolonne_niveau_1 <- matrix(sub(".*[;]", "", kolonnenavne), nrow = 1) # fjerner tekst før punktum
                            kolonne_niveau_2 <- matrix(sub("[;].*", "", kolonnenavne), nrow = 1) # fjerner tekst efter punktum

                            # Skriver overskrifterne på de to første rækker
                            writeData(wb, "Som tabel", kolonne_niveau_1, startCol = startkolonne, startRow = 2, colNames = FALSE)
                            writeData(wb, "Som tabel", kolonne_niveau_2, startCol = startkolonne, startRow = 1, colNames = FALSE)

                            # Merger celler i den første række
                            for(i in seq(startkolonne,ncol(x),antal_raekker2)){ # loop fra startkolonne til sidste kolonne der stiger med antal værdier
                              mergeCells(wb, sheet = "Som tabel", cols = i:(i+(antal_raekker2-1)), rows = 1)
                            }

                            # kildeangivelse
                            kilde <- paste("Kilde: Ankestyrelsen (",format(Sys.Date(), "%Y"),")")
                            writeData(wb, "Som tabel", kilde, startCol = 1, startRow = nrow(x) + 4) # der er tre rækker før data og en række mellemrum

                            # definerer og tilføjer styling
                            centerStyle <- createStyle(halign = "center")
                            underline_style <- createStyle(
                              border = "Bottom"
                            )
                            venstre_style <- createStyle(
                              border = "Left"
                            )

                            styleT <- createStyle

                            addStyle(wb, "Som tabel", centerStyle, rows = 1, cols = startkolonne:ncol(x))
                            addStyle(wb, "Som tabel", centerStyle, rows = 2, cols = startkolonne:ncol(x))
                            addStyle(wb, "Som tabel", underline_style, rows = 1, cols = startkolonne:ncol(x), stack = TRUE)

                            saveWorkbook(wb, file, overwrite = TRUE)
                          }
      )
    }
  #)
}

server <- function(input, output, session) {

# Tilsyn ------------------------------------------------------------------

# Links på forsiden

links_til_tilsyn <- reactive ({
  list(input$tilsyn_knap_forside, input$forside_tilsyn_kpi_antal_sager, input$forside_tilsyn_kpi_omgor)
})

# observeEvent(links_til_tilsyn(), {
#   updateTabItems(session = session, "tabs", "Tilsynssager")
# })

# Variable

#lovgrundlag_tilsyn <- reactive(c(input$tilsyn_kommune, input$tilsyn_landstotal))  # Skal rettes
lovgrundlag_tilsyn <- reactive(input$tilsyn_lovgrundlag)  # Skal rettes

tilsyn_landstotal <- reactive (if(input$tilsyn_landstotal) "Hele landet" else NULL)

valgte_instanser_tilsyn <- reactive(
  #c(input$tilsyn_kommune, input$tilsyn_landstotal)
  c(input$tilsyn_kommune, tilsyn_landstotal())
)

valgte_maal_tilsyn <- reactive(
  #c('Afvisning','Udtalelse', 'Forhåndsudtalelse', 'Sag.ikke.behandlet','Fogedsag')
  c(input$antal_sager1_tilsyn, input$antal_sager2_tilsyn, input$antal_sager3_tilsyn)
)

antal_sager_samlet_tilsyn <- reactive(c(input$antal_sager1_tilsyn, input$antal_sager2_tilsyn, input$antal_sager3_tilsyn))

nogletal_samlet_tilsyn <- reactive(c(input$nogletal1_tilsyn, input$nogletal2_tilsyn))

valgte_maal_og_noegletal_tilsyn <- reactive(c(valgte_maal_tilsyn()))

input_til_graf_tilsyn <- reactive ({
  "Afvisning"
})

procent_kolonner_tilsyn2 <- reactive ({
  #kombiner_maal_og_periode(procent_kolonner_tilsyn(), input$som_kolonner_tilsyn, input$tilsyn_periode)
  kombiner_maal_og_periode(input$som_kolonner_tilsyn, input$tilsyn_periode)
})

antal_kolonner_tilsyn2 <- reactive ({
  kombiner_maal_og_periode(valgte_maal_tilsyn(), input$som_kolonner_tilsyn, input$tilsyn_periode)
})

len_valg_tilsyn <- reactive ({
  length(valgte_instanser_tilsyn()) * length(input$tilsyn_lovgrundlag) * length(input$tilsyn_periode) * (length(input$antal_sager1_tilsyn) + length(input$antal_sager2_tilsyn) + length(input$antal_sager3_tilsyn))
})

# Sortering af kolonnner

sortering_kolonner_tilsyn <- reactive ({
  mulige_maal <- c("Behandlede sager i alt",
                   "Afvisning",
                   "Udtalelse",
                   "Forhåndsudtalelse",
                   "Sag ikke behandlet",
                   "Fogedsag",
                   "Sanktion"
                   )
  valgte_perioder_2 <- input$tilsyn_periode
  opslag_sortering(maal = mulige_maal,
                   valgte_perioder = valgte_perioder_2,
                   som_kolonner = input$som_kolonner_tilsyn,
                   antal_kolonner = antal_kolonner_tilsyn2(),
                   #procent_kolonner = procent_kolonner_tilsyn2()
                   procent_kolonner = NULL
                   )
})

# grundbearbejdning af data

tilsyn_data <- reactive({

  data_tilsyn_spread %>%
  select(-c(Aar, Kvartal)) %>%
  rename("Sag ikke behandlet" = Sag.ikke.behandlet) %>%
  rename('Afvisning' = 'X..48.a.afvisning',                                              # skal slettes
           'Udtalelse' = 'X..50.Udtalelse.godkendelse') %>%                              # skal slettes
  filtrer_data(.,valgte_instanser_tilsyn(), input$tilsyn_lovgrundlag, input$tilsyn_periode) %>%
  mutate('Behandlede sager i alt' = (Afvisning + Udtalelse + Forhåndsudtalelse + Fogedsag + Sanktion)) %>%
  select(Sagsemne, Instans, valgte_maal_og_noegletal_tilsyn(), periode) %>%
  #tilfoej_dimensionsvaerdier(., Sagsemner_udk, input$tilsyn_lov_eller_paragraf) %>%
  tilfoej_i_alt_raekker_test(., lovgrundlag_tilsyn(), input$vises_i_alt_tilsyn, input$tilsyn_landstotal)
})

# dan tabel

gt_tabl_tilsyn <- reactive({
tilsyn_data() %>%
  pivoter_data(.,valgte_maal_og_noegletal_tilsyn(), input$som_kolonner_tilsyn) %>%
  select("Sagsemne", "Instans", all_of(sortering_kolonner_tilsyn())) %>% # Sorterer rækkefølge på valgte maal
  dan_gt_tabel(titel = "**Klager behandlet af det kommunale og regionale tilsyn**",
               undertitel = NULL,
               raekke = if(input$som_raekker_tilsyn == 1) "Sagsemne" else "Instans",
               gruppe = if(input$som_raekker_tilsyn == 1) "Instans" else "Sagsemne",
               fodnote = NULL)
})

output$tabel_tilsyn <- render_gt({
  req(input$tilsyn_lovgrundlag, valgte_instanser_tilsyn(), input$tilsyn_periode, antal_sager_samlet_tilsyn()) # der genereres ikke en tabel, før der er valg i de nøvendige filtre
  gt_tabl_tilsyn()
})

# dan graf

tilsyn_plot_omg_input <- reactive(
tilsyn_data() %>%
  select(Instans, Sagsemne, periode, input_til_graf_tilsyn()) %>%
  rename(data_kolonne = 4) %>%
  dan_graf(input_til_graf_tilsyn(), 2, input$Pivot_graf_kommune_el_lovgivning_tilsyn)
)

output$tilsyn_plot_omg <- renderPlot({
  #req(input$ask_in_lovgivning, length(input$ask_in_periode) > 1, len_valg_graf_ask() < 2000, length(input$ask_in_lovgivning) < 10)
  print(tilsyn_plot_omg_input())
})

# Eksport graf

output$eksport_graf_tilsyn <- downloadHandler(
  filename = "graf.png",
  content = function(file) {
    shiny::withProgress(message = 'Downloader billede. Det kan tage et øjeblik.',
                        value = 0,
                        {
                          shiny::incProgress(1/10)
                          Sys.sleep(1)
                          shiny::incProgress(5/10)
                          ggsave(file, plot = tilsyn_plot_omg_input(), device = "png", width = 20)
                        }
    )
  }
)

# Dan excel

grundtabel_exl_tilsyn <- reactive({

tilsyn_data() %>%
    #select(Sagsemne, Instans, valgte_maal_og_noegletal_tilsyn(), periode) %>%
    pivoter_data(valgte_maal_og_noegletal_tilsyn(), input$som_kolonner_tilsyn)

})

# output$excel_tilsyn <- dan_excel_fil(data = grundtabel_exl_tilsyn(),
#                                     som_raekker = input$som_raekker_tilsyn,
#                                     som_kolonner = input$som_kolonner_tilsyn,
#                                     periode = input$tilsyn_periode,
#                                     valgte_maal_og_noegletal = valgte_maal_og_noegletal_tilsyn(),
#                                     sortering = sortering_kolonner_tilsyn()
# )

output$excel_tilsyn <- downloadHandler(
    filename = "excelfil.xlsx",
    content = function(file) {
      Sys.sleep(0.3)
      removeModal()
      shiny::withProgress(message = 'Downloader excel-fil. Det kan tage et øjeblik.',
                          value = 0,   {
                             shiny::incProgress(1/10)
                             Sys.sleep(1)
                             shiny::incProgress(5/10)

                            wb <- createWorkbook()

                            # Danner fane
                            addWorksheet(wb, "Som tabel")


                            sortering1 <- if (input$som_raekker_tilsyn == 1) "Sagsemne" else "Instans"
                            sortering2 <- if (input$som_raekker_tilsyn == 1) "Instans" else "Sagsemne"

                            # Sorterer rækkefølgende af kolonnerne

                            grund_tabel <- grundtabel_exl_tilsyn() %>%
                              select("Sagsemne", "Instans", all_of(sortering_kolonner_tilsyn())) %>%
                              select(all_of(sortering2), all_of(sortering1), everything()) %>%
                              arrange(.[,1])

                            grund_tabel[is.na(grund_tabel)] <- 0

                            x <- grund_tabel

                            # Hjælpevariable til opsætning af ark
                            antal_raekker2 <- if (input$som_kolonner_tilsyn == 1) {
                              length(input$tilsyn_periode)
                            } else {
                              length(valgte_maal_og_noegletal_tilsyn())
                            }

                            startkolonne <- 3

                            writeData(wb, "Som tabel", x, startCol = 1, startRow = 2, rowNames = FALSE) # indsætter data i fanen
                            setColWidths(wb, sheet = "Som tabel", cols = 1:ncol(x), widths = "auto") # Justerer kolonnebredden

                            # Laver kolonneoverskrifter og underoverskrifter
                            kolonnenavne <- colnames(x) # tager kolonnenavnene fra datasætter
                            kolonnenavne <- kolonnenavne[startkolonne:length(kolonnenavne)] # fjerner de første to
                            kolonne_niveau_1 <- matrix(sub(".*[;]", "", kolonnenavne), nrow = 1) # fjerner tekst før punktum
                            kolonne_niveau_2 <- matrix(sub("[;].*", "", kolonnenavne), nrow = 1) # fjerner tekst efter punktum

                            # Skriver overskrifterne på de to første rækker
                            writeData(wb, "Som tabel", kolonne_niveau_1, startCol = startkolonne, startRow = 2, colNames = FALSE)
                            writeData(wb, "Som tabel", kolonne_niveau_2, startCol = startkolonne, startRow = 1, colNames = FALSE)

                            # Merger celler i den første række
                            for(i in seq(startkolonne,ncol(x),antal_raekker2)){ # loop fra startkolonne til sidste kolonne der stiger med antal værdier
                              mergeCells(wb, sheet = "Som tabel", cols = i:(i+(antal_raekker2-1)), rows = 1)
                            }

                            # kildeangivelse
                            kilde <- paste("Kilde: Ankestyrelsen (",format(Sys.Date(), "%Y"),")")
                            writeData(wb, "Som tabel", kilde, startCol = 1, startRow = nrow(x) + 4) # der er tre rækker før data og en række mellemrum

                            # definerer og tilføjer styling
                            centerStyle <- createStyle(halign = "center")
                            underline_style <- createStyle(
                              border = "Bottom"
                            )
                            venstre_style <- createStyle(
                              border = "Left"
                            )

                            styleT <- createStyle

                            addStyle(wb, "Som tabel", centerStyle, rows = 1, cols = startkolonne:ncol(x))
                            addStyle(wb, "Som tabel", centerStyle, rows = 2, cols = startkolonne:ncol(x))
                            addStyle(wb, "Som tabel", underline_style, rows = 1, cols = startkolonne:ncol(x), stack = TRUE)

                            saveWorkbook(wb, file, overwrite = TRUE)
                          }
      )
    }
  )

# Dan csv

 grund_tabel_csv_tilsyn <- reactive ({
   tilsyn_data() %>%
   select(Instans, Sagsemne, periode, all_of(valgte_maal_og_noegletal_tilsyn()))
   #%>%
   #left_join(Sagsemner, by = 'Sagsemne') %>%
  #   select(-c(Sorteringskode, X))
 })

 output$download_csv_tilsyn <- downloadHandler(
   filename = function() {
     removeModal()
     paste("data", ".csv", sep = "")
   },
   content = function(file) {
     write.table(grund_tabel_csv_tilsyn(), file, row.names = FALSE, sep = ";",dec = ",", fileEncoding = 'latin1')
   }
 )

# UI-elementer

output$eksport_boks_tilsyn <- renderUI({
  req(len_valg_tilsyn() >= 1, len_valg_tilsyn() < 2000)
  eksportboks("download_data_modal_tilsyn")
})

# modaler

observeEvent(input$download_data_modal_tilsyn, {
 eksportmodal("excel_tilsyn", "download_csv_tilsyn")
})

observeEvent(input$info_antal_sager_tilsyn, {
  showModal(modalDialog(
    title = html("<strong>Det regionale og kommune tilsyns afgørelsestyper</strong>"),
    h3("Behandlede sager"),
    html("
    <strong>Udtalelse/godkendelse</strong> (kommunestyrelseslovens § 50) betyder, at Ankestyrelsen har udtalt sig vejledende om en kommunes, regions eller kommunalt fællesskabs beslutninger eller praksis.
    Ankestyrelsen kan også have truffet en afgørelse, f.eks. om samtykke til at sælge fast ejendom uden offentligt udbud.
    <br>
    <br>
    <strong>Afvisning</strong> (kommunestyrelseslovens § 48 a) betyder, at Ankestyrelsen har besluttet, at der ikke er anledning til at rejse en tilsynssag.
    <br>
    <br>
    <strong>Sanktion</strong> (kommunestyrelseslovens §§ 50 d-51) betyder, at Ankestyrelsen har taget stilling til, om der skal anvendes en sanktion over for en kommune, region eller kommunalt fællesskab.
    <br>
    <br>
    <strong>Forhåndsudtalelse</strong> betyder, at Ankestyrelsen efter anmodning fra en kommune, region eller kommunalt fællesskab har afgivet en forudgående vejledende udtalelse om lovligheden af en disposition, inden den er foretaget.
    <br>
    <br>
    <strong>Fogedsag</strong> betyder, at Ankestyrelsen har behandlet en sag, hvor spørgsmålet er, om en kommune, region eller kommunalt fællesskab har undladt at efterleve en bindende afgørelse fra en rekurs- eller sektortilsynsmyndighed.
    Sagerne kan have forskelligt udfald: Ankestyrelsen kan beslutte at rejse en tilsynssag eller ikke at rejse en tilsynssag.
    <br>
    <br>"),
    h3("Ikke-behandlede sager"),
    html("
    <strong>Ikke-behandlede sager</strong> er sager, som Ankestyrelsen ikke har behandlet. Det kan eksempelvis være, fordi Ankestyrelsen ikke har kompetence til at behandle sagen, eller fordi sagen henlægges.
    "),

    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})


observeEvent(input$info_nogletal_tilsyn, {
  showModal(modalDialog(
    title = html("<strong>Nøgletal</strong>"),
    html("<strong>Stadfæstelsesprocenten</strong> er antallet af stadfæstede sager set i forhold til alle realitetsbehandlede sager.
    <br>
    <br>
    <strong>Omgørelsesprocenten</strong> er antallet af hjemviste og ændrede/ophævede sager set i forhold til alle realitetsbehandlede sager.
    <br>
    <br>
    <strong>Ændrings-/ophævelsesprocenten</strong> er antallet af ændrede/ophævede sager set i forhold til alle realitetsbehandlede sager.
    <br>
    <br>
    <strong>Hjemvisningsprocenten</strong> er antallet af hjemviste sager set i forhold til alle realitetsbehandlede sager.
    "),

    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})

# Nulstil filtre

observeEvent(input$nulstil_filtre_tilsyn, {
  updateMaterialSwitch(session, "tilsyn_landstotal",
                       value = FALSE)
  updatePickerInput(session, "tilsyn_periode",
                    selected = FALSE)
  updatePickerInput(session, "tilsyn_lovgrundlag",
                    selected = FALSE)
  updatePickerInput(session, "tilsyn_kommune",
                    selected = FALSE)
  updateRadioButtons(session, "som_kolonner_tilsyn",
                            selected = 1)
  updateRadioButtons(session, "som_raekker_tilsyn",
                            selected = 1)
  updateRadioButtons(session, "vises_fodnoter_tilsyn",
                            selected = TRUE)
  updateRadioButtons(session, "vises_i_alt_tilsyn",
                            selected = TRUE)
  updateRadioButtons(session, "tilsyn_aar_eller_kvartal",
                     selected = 1)

})


output$ikke_nok_valg_tilsyn <- renderUI({
  req(length(input$tilsyn_lovgrundlag) < 1 || length(valgte_instanser_tilsyn()) < 1 || length(input$tilsyn_periode) < 1)
  h4("Data vises, når du har valgt mindst ét emne, én kommune og én periode.")
}
)



# Underretningssager ------------------------------------------------------

# lovgrundlag_underretning <- reactive(input$underretning_lovgrundlag)  # Skal rettes
lovgrundlag_underretning <- reactive(c('Underretning'))  # Skal rettes

valgte_instanser_underretning <- reactive(
  'Hele landet'
  )

valgte_maal_underretning <- reactive(
  #c('Afvisning','Udtalelse', 'Forhåndsudtalelse', 'Sag.ikke.behandlet','Fogedsag')
  c(input$antal_sager1_underretning)
)

valgte_maal_og_noegletal_underretning <- reactive(c(valgte_maal_underretning()))

len_valg_underretning <- reactive ({
  length(input$underretning_periode) * length(input$antal_sager1_underretning)
})

# grundbearbejdning af data

underretning_data <- reactive({

  data_underretning_spread %>%
    select(-c(Aar, NULL.)) %>%
    rename("Ikke-mødebehandlet" = Ikke.mødebehandlet) %>%
    filtrer_data(.,valgte_instanser_underretning(), lovgrundlag_underretning(), input$underretning_periode) %>%
    select(Instans, valgte_maal_og_noegletal_underretning(), periode)
    #%>%
    #tilfoej_dimensionsvaerdier(., Sagsemner_udk, input$tilsyn_lov_eller_paragraf)
    #%>% #tilfoej_i_alt_raekker_alt(data = ., all_of(valgte_maal_underretning()), input$underretning_kommune, input$underretning_landstotal, input$vises_i_alt_underretning)
})

# dan tabel

gt_tabl_underretning <- reactive({
  underretning_data() %>%
    #tilfoej_nogletal(., "Behandlede sager i alt") %>% # Fejler, når man vælger flere sagsemner
    #tilfoej_nogletal(., "sager_i_alt") %>% # Dette skal erstattes af map el. lignende, så der kun beregnes de relevante nøgletal
    #tilfoej_nogletal(., "sager_u_alt") %>%
    #select(Instans, valgte_maal_og_noegletal_underretning(), periode) %>%
    pivoter_data(.,valgte_maal_og_noegletal_underretning(), input$som_kolonner_underretning) %>%
    #select("Instans", all_of(sortering_kolonner_tilsyn())) %>% # Sorterer rækkefølge på valgte maal
    dan_gt_tabel(titel = "**Underretningssager**",
                 undertitel = NULL,
                 #raekke = if(input$som_raekker_mellemk == 1) "Sagsemne" else "Instans",
                 raekke = "Instans",
                 #gruppe = if(input$som_raekker_mellemk == 1) "Instans" else "Sagsemne"
                 gruppe = NULL,
                 fodnote = NULL
                 )
})

output$tabel_underretning <- render_gt({
  req(input$underretning_periode, valgte_maal_underretning()) # der genereres ikke en tabel, før der er valg i de nøvendige filtre
  gt_tabl_underretning()
})


# Dan excel

grundtabel_exl_underretning <- reactive({

  underretning_data() %>%
    pivoter_data(valgte_maal_og_noegletal_underretning(), input$som_kolonner_underretning)

})

output$excel_underretning <- downloadHandler(
  filename = "excelfil.xlsx",
  content = function(file) {
    Sys.sleep(0.3)
    removeModal()
    shiny::withProgress(message = 'Downloader excel-fil. Det kan tage et øjeblik.',
                        value = 0,   {
                          shiny::incProgress(1/10)
                          Sys.sleep(1)
                          shiny::incProgress(5/10)

                          wb <- createWorkbook()

                          # Danner fane
                          addWorksheet(wb, "Som tabel")

                          #sortering1 <- if (input$som_raekker_tilsyn == 1) "Sagsemne" else "Instans"
                          #sortering2 <- if (input$som_raekker_tilsyn == 1) "Instans" else "Sagsemne"

                          # Sorterer rækkefølgende af kolonnerne

                          grund_tabel <- grundtabel_exl_underretning() %>%
                            #select("Sagsemne", "Instans", all_of(sortering_kolonner_tilsyn())) %>%
                            select("Instans", everything()) %>%
                            arrange(.[,1])

                          grund_tabel[is.na(grund_tabel)] <- 0

                          x <- grund_tabel

                          # Hjælpevariable til opsætning af ark
                          antal_raekker2 <- if (input$som_kolonner_underretning == 1) {
                            length(input$underretning_periode)
                          } else {
                            length(valgte_maal_og_noegletal_underretning())
                          }

                          startkolonne <- 2

                          writeData(wb, "Som tabel", x, startCol = 1, startRow = 2, rowNames = FALSE) # indsætter data i fanen
                          setColWidths(wb, sheet = "Som tabel", cols = 1:ncol(x), widths = "auto") # Justerer kolonnebredden

                          # Laver kolonneoverskrifter og underoverskrifter
                          kolonnenavne <- colnames(x) # tager kolonnenavnene fra datasætter
                          kolonnenavne <- kolonnenavne[startkolonne:length(kolonnenavne)] # fjerner de første to
                          kolonne_niveau_1 <- matrix(sub(".*[;]", "", kolonnenavne), nrow = 1) # fjerner tekst før punktum
                          kolonne_niveau_2 <- matrix(sub("[;].*", "", kolonnenavne), nrow = 1) # fjerner tekst efter punktum

                          # Skriver overskrifterne på de to første rækker
                          writeData(wb, "Som tabel", kolonne_niveau_1, startCol = startkolonne, startRow = 2, colNames = FALSE)
                          writeData(wb, "Som tabel", kolonne_niveau_2, startCol = startkolonne, startRow = 1, colNames = FALSE)

                          # Merger celler i den første række
                          for(i in seq(startkolonne,ncol(x),antal_raekker2)){ # loop fra startkolonne til sidste kolonne der stiger med antal værdier
                            mergeCells(wb, sheet = "Som tabel", cols = i:(i+(antal_raekker2-1)), rows = 1)
                          }

                          # kildeangivelse
                          kilde <- paste("Kilde: Ankestyrelsen (",format(Sys.Date(), "%Y"),")")
                          writeData(wb, "Som tabel", kilde, startCol = 1, startRow = nrow(x) + 4) # der er tre rækker før data og en række mellemrum

                          # definerer og tilføjer styling
                          centerStyle <- createStyle(halign = "center")
                          underline_style <- createStyle(
                            border = "Bottom"
                          )
                          venstre_style <- createStyle(
                            border = "Left"
                          )

                          styleT <- createStyle

                          addStyle(wb, "Som tabel", centerStyle, rows = 1, cols = startkolonne:ncol(x))
                          addStyle(wb, "Som tabel", centerStyle, rows = 2, cols = startkolonne:ncol(x))
                          addStyle(wb, "Som tabel", underline_style, rows = 1, cols = startkolonne:ncol(x), stack = TRUE)

                          saveWorkbook(wb, file, overwrite = TRUE)
                        }
    )
  }
)


# Dan csv

grund_tabel_csv_underretning <- reactive ({
  underretning_data() %>%
    select(Instans, periode, all_of(valgte_maal_og_noegletal_underretning()))
})

output$download_csv_underretning <- downloadHandler(
  filename = function() {
    removeModal()
    paste("data", ".csv", sep = "")
  },
  content = function(file) {
    write.table(grund_tabel_csv_underretning(), file, row.names = FALSE, sep = ";",dec = ",", fileEncoding = 'latin1')
  }
)

observe({
  x <-
    #if (input$underretning_aar_eller_kvartal == 1) {
      c('2022','2021','2020', '2019', '2018')
    # } else {
    #   list(
    #     '2021' = c('4. kvartal 2021' = '2021 - 4. kvartal',
    #                 '3. kvartal 2021' = '2021 - 3. kvartal',
    #                '2. kvartal 2021' = '2021 - 2. kvartal',
    #                '1. kvartal 2021' = '2021 - 1. kvartal'),
    #      '2020' = c('4. kvartal 2020' = '2020 - 4. kvartal',
    #                 '3. kvartal 2020' = '2020 - 3. kvartal',
    #                 '2. kvartal 2020' = '2020 - 2. kvartal',
    #                 '1. kvartal 2020' = '2020 - 1. kvartal'),
    #      '2019' = c('4. kvartal 2019' = '2019 - 4. kvartal',
    #                 '3. kvartal 2019' = '2019 - 3. kvartal',
    #                 '2. kvartal 2019' = '2019 - 2. kvartal',
    #                 '1. kvartal 2019' = '2019 - 1. kvartal'),
    #      '2018' = c('4. kvartal 2018' = '2018 - 4. kvartal',
    #                 '3. kvartal 2018' = '2018 - 3. kvartal',
    #                 '2. kvartal 2018' = '2018 - 2. kvartal',
    #                 '1. kvartal 2018' = '2018 - 1. kvartal')
    #     # ,
    #     # '2017' = c('4. kvartal 2017' = '2017 - 4. kvartal',
    #     #            '3. kvartal 2017' = '2017 - 3. kvartal',
    #     #            '2. kvartal 2017' = '2017 - 2. kvartal',
    #     #            '1. kvartal 2017' = '2017 - 1. kvartal'),
    #     # '2016' = c('4. kvartal 2016' = '2016 - 4. kvartal',
    #     #            '3. kvartal 2016' = '2016 - 3. kvartal',
    #     #            '2. kvartal 2016' = '2016 - 2. kvartal',
    #     #            '1. kvartal 2016' = '2016 - 1. kvartal'),
    #     # '2015' = c('4. kvartal 2015' = '2015 - 4. kvartal',
    #     #            '3. kvartal 2015' = '2015 - 3. kvartal',
    #     #            '2. kvartal 2015' = '2015 - 2. kvartal',
    #     #            '1. kvartal 2015' = '2015 - 1. kvartal'),
    #     # '2014' = c('4. kvartal 2014' = '2014 - 4. kvartal',
    #     #            '3. kvartal 2014' = '2014 - 3. kvartal',
    #     #            '2. kvartal 2014' = '2014 - 2. kvartal',
    #     #            '1. kvartal 2014' = '2014 - 1. kvartal'),
    #     # '2013' = c('4. kvartal 2013' = '2013 - 4. kvartal',
    #     #            '3. kvartal 2013' = '2013 - 3. kvartal')
    #   )
    # }
  y <- 'Vælg år'
  updatePickerInput(session, "underretning_periode",choices = x, label = y
                    #selected = list('2017','2018','2019')
  )
})

# UI-elementer

output$eksport_boks_underretning <- renderUI({
  req(len_valg_underretning() >= 1, len_valg_underretning() < 2000)
  eksportboks("download_data_modal_underretning")
})

# modaler

observeEvent(input$download_data_modal_underretning, {
  eksportmodal("excel_underretning", "download_csv_underretning")
})

observeEvent(input$info_antal_sager_underretning, {
  showModal(modalDialog(
    title = html("<strong>Sagsudfald for underretningssager</strong>"),
    html("
    <strong>Mødebehandlede sager</strong> er sager, hvor Ankestyrelsen på baggrund af oplysninger fra kommunen, indkalder forældremyndighedsindehavere og børn over 12 år til møde i Ankestyrelsen. Når sagen behandles på møde, har Ankestyrelsen kompetence til at træffe afgørelse af egen drift om fx anbringelse uden samtykke. Ankestyrelsen har også kompetence til at pålægge kommunen at foretage undersøgelser eller iværksætte andre foranstaltninger.
    <br>
    <br>
    <strong>Ikke-mødebehandlede sager</strong> er sager, hvor Ankestyrelsen på baggrund af en udtalelse og akter fra kommunen vurderer, at kommunen handler relevant og tilstrækkeligt. Sagen behandles administrativt.
    <br>
    <br>"),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})

# Nulstil filtre

observeEvent(input$nulstil_filtre_underretning, {
  updatePickerInput(session, "underretning_periode",
                    selected = FALSE)
  updateRadioButtons(session, "som_kolonner_underretning",
                     selected = 1)
  updateRadioButtons(session, "vises_fodnoter_underretning",
                     selected = TRUE)
})

output$ikke_nok_valg_underretning <- renderUI({
  req(length(input$underretning_periode) < 1)
  h4("Data vises, når du har valgt mindst én periode.")
}
)

# Mellemkommunal ----------------------------------------------------------


# Links på forsiden

# links_til_tilsyn <- reactive ({
#   list(input$tilsyn_knap_forside, input$forside_tilsyn_kpi_antal_sager, input$forside_tilsyn_kpi_omgor)
# })
#
# observeEvent(links_til_tilsyn(), {
#   updateTabItems(session = session, "tabs", "Tilsynssager")
# })

# Variable
#
# #lovgrundlag_tilsyn <- reactive(c(input$tilsyn_kommune, input$tilsyn_landstotal))  # Skal rettes

lovgrundlag_mellemkommunal <- reactive(input$mellemk_lovgrundlag)  # Skal rettes

mellemkommunale_landstotal <- reactive (if(input$mellemk_landstotal) "Hele landet" else NULL)

valgte_instanser_mellemkommunal <- reactive(
  #c(input$mellemk_kommune, input$mellemk_landstotal)
  c(input$mellemk_kommune, mellemkommunale_landstotal())
)
#
valgte_maal_mellemkommunal <- reactive(
   #c('Afvisning','Udtalelse', 'Forhåndsudtalelse', 'Sag.ikke.behandlet','Fogedsag')
   c(input$antal_sager1_mellemk, input$antal_sager2_mellemk)
 )

valgte_maal_og_noegletal_mellemkommunal <- reactive(c(valgte_maal_mellemkommunal()))

antal_kolonner_mellemkommunal <- reactive ({
  kombiner_maal_og_periode(valgte_maal_mellemkommunal(), input$som_kolonner_mellemk, input$mellemk_periode)
})

len_valg_mellemkommunal <- reactive ({
  length(valgte_instanser_mellemkommunal()) * length(input$mellemk_lovgrundlag) * length(input$mellemk_periode) * (length(input$antal_sager1_mellemk) + length(input$antal_sager2_mellemk))
})

# Sortering af kolonnner

sortering_kolonner_mellemk <- reactive ({
  mulige_maal <- c("Realitetsbehandlede sager i alt",
                   "Ikke-behandlede sager"
  )
  valgte_perioder_2 <- input$mellemk_periode
  opslag_sortering(maal = mulige_maal,
                   valgte_perioder = valgte_perioder_2,
                   som_kolonner = input$som_kolonner_mellemk,
                   antal_kolonner = antal_kolonner_mellemkommunal(),
                   #procent_kolonner = procent_kolonner_tilsyn2()
                   procent_kolonner = NULL
  )
})

# grundbearbejdning af data

mellemkommunal_data <- reactive({

  data_mellemkommunal_spread %>%
    select(-c(Aar, Kvartal)) %>%                                                    # skal rettes
    rename('Ikke-behandlede sager' = Ikke.behandlede.sager,
           'Realitetsbehandlede sager i alt' = Realitetsbehandlede.sager.i.alt) %>%
    #rename('Afvisning' = 'X..48.a.afvisning',                                              # skal slettes
    #       'Udtalelse' = 'X..50.Udtalelse.godkendelse') %>%                              # skal slettes
    filtrer_data(.,valgte_instanser_mellemkommunal(), input$mellemk_lovgrundlag, input$mellemk_periode)
    #tilfoej_dimensionsvaerdier(., Sagsemner_udk, input$tilsyn_lov_eller_paragraf) %>%
    #tilfoej_i_alt_raekker(data = ., all_of(valgte_maal_mellemkommunal()), lovgrundlag_mellemkommunal(), input$vises_i_alt_mellemk)
    #tilfoej_i_alt_raekker_test(., lovgrundlag_mellemkommunal(), input$vises_i_alt_mellemkommunal, input$mellemk_landstotal)

})

# dan tabel

gt_tabl_mellemkommunal <- reactive({
  mellemkommunal_data() %>%
    #tilfoej_nogletal(., "Behandlede sager i alt") %>% # Fejler, når man vælger flere sagsemner
    #tilfoej_nogletal(., "sager_i_alt") %>% # Dette skal erstattes af map el. lignende, så der kun beregnes de relevante nøgletal
    #tilfoej_nogletal(., "sager_u_alt") %>%
    select(Sagsemne, Instans, valgte_maal_og_noegletal_mellemkommunal(), periode) %>%
    pivoter_data(.,valgte_maal_og_noegletal_mellemkommunal(), input$som_kolonner_mellemk) %>%
    #select("Sagsemne", "Instans", all_of(sortering_kolonner_tilsyn())) %>% # Sorterer rækkefølge på valgte maal
    dan_gt_tabel(titel = "**Klager om kommunal uenighed**",
                 undertitel = NULL,
                 raekke = if(input$som_raekker_mellemk == 1) "Sagsemne" else "Instans",
                 gruppe = if(input$som_raekker_mellemk == 1) "Instans" else "Sagsemne",
                 fodnote = "Sager om mellemkommunal vil typisk have to involverede kommuner. I statistikken tælles begge kommuner med, og antallet af sager vil således være det dobbelte af, hvad der andre steder rapporteres i forhold til antallet af sager om mellemkommunal uenighed.

                            Grundet en ekstraordinær kvalitetssikring, er data for de mellemkommunale sager for 2021 blevet justeret den 9.januar 2022")
})

output$tabel_mellemk <- render_gt({
  req(input$mellemk_lovgrundlag, valgte_instanser_mellemkommunal(), input$mellemk_periode, input$antal_sager1_mellemk) # der genereres ikke en tabel, før der er valg i de nøvendige filtre
  gt_tabl_mellemkommunal()
})

# dan excel

grundtabel_exl_mellemkommunal <- reactive({

  mellemkommunal_data() %>%
    pivoter_data(valgte_maal_og_noegletal_mellemkommunal(), input$som_kolonner_mellemk)

})

output$excel_mellemk <- downloadHandler(
  filename = "excelfil.xlsx",
  content = function(file) {
    Sys.sleep(0.3)
    removeModal()
    shiny::withProgress(message = 'Downloader excel-fil. Det kan tage et øjeblik.',
                        value = 0,   {
                          shiny::incProgress(1/10)
                          Sys.sleep(1)
                          shiny::incProgress(5/10)

                          wb <- createWorkbook()

                          # Danner fane
                          addWorksheet(wb, "Som tabel")


                          sortering1 <- if (input$som_raekker_mellemk == 1) "Sagsemne" else "Instans"
                          sortering2 <- if (input$som_raekker_mellemk == 1) "Instans" else "Sagsemne"

                          # Sorterer rækkefølgende af kolonnerne

                          grund_tabel <- grundtabel_exl_mellemkommunal() %>%
                            select("Sagsemne", "Instans", all_of(sortering_kolonner_mellemk())) %>%
                            select(all_of(sortering2), all_of(sortering1), everything()) %>%
                            arrange(.[,1])

                          grund_tabel[is.na(grund_tabel)] <- 0

                          x <- grund_tabel

                          # Hjælpevariable til opsætning af ark
                          antal_raekker2 <- if (input$som_kolonner_mellemk == 1) {
                            length(input$mellemk_periode)
                          } else {
                            length(valgte_maal_og_noegletal_mellemkommunal())
                          }

                          startkolonne <- 3

                          writeData(wb, "Som tabel", x, startCol = 1, startRow = 2, rowNames = FALSE) # indsætter data i fanen
                          setColWidths(wb, sheet = "Som tabel", cols = 1:ncol(x), widths = "auto") # Justerer kolonnebredden

                          # Laver kolonneoverskrifter og underoverskrifter
                          kolonnenavne <- colnames(x) # tager kolonnenavnene fra datasætter
                          kolonnenavne <- kolonnenavne[startkolonne:length(kolonnenavne)] # fjerner de første to
                          kolonne_niveau_1 <- matrix(sub(".*[;]", "", kolonnenavne), nrow = 1) # fjerner tekst før punktum
                          kolonne_niveau_2 <- matrix(sub("[;].*", "", kolonnenavne), nrow = 1) # fjerner tekst efter punktum

                          # Skriver overskrifterne på de to første rækker
                          writeData(wb, "Som tabel", kolonne_niveau_1, startCol = startkolonne, startRow = 2, colNames = FALSE)
                          writeData(wb, "Som tabel", kolonne_niveau_2, startCol = startkolonne, startRow = 1, colNames = FALSE)

                          # Merger celler i den første række
                          for(i in seq(startkolonne,ncol(x),antal_raekker2)){ # loop fra startkolonne til sidste kolonne der stiger med antal værdier
                            mergeCells(wb, sheet = "Som tabel", cols = i:(i+(antal_raekker2-1)), rows = 1)
                          }

                          # kildeangivelse
                          kilde <- paste("Kilde: Ankestyrelsen (",format(Sys.Date(), "%Y"),")")
                          writeData(wb, "Som tabel", kilde, startCol = 1, startRow = nrow(x) + 4) # der er tre rækker før data og en række mellemrum

                          # definerer og tilføjer styling
                          centerStyle <- createStyle(halign = "center")
                          underline_style <- createStyle(
                            border = "Bottom"
                          )
                          venstre_style <- createStyle(
                            border = "Left"
                          )

                          styleT <- createStyle

                          addStyle(wb, "Som tabel", centerStyle, rows = 1, cols = startkolonne:ncol(x))
                          addStyle(wb, "Som tabel", centerStyle, rows = 2, cols = startkolonne:ncol(x))
                          addStyle(wb, "Som tabel", underline_style, rows = 1, cols = startkolonne:ncol(x), stack = TRUE)

                          saveWorkbook(wb, file, overwrite = TRUE)
                        }
    )
  }
)


# Dan csv

grund_tabel_csv_mellemk <- reactive ({
  mellemkommunal_data() %>%
    select(Instans, Sagsemne, periode, all_of(valgte_maal_og_noegletal_mellemkommunal()))
  #%>%
  #left_join(Sagsemner, by = 'Sagsemne') %>%
  #   select(-c(Sorteringskode, X))
})

output$download_csv_mellemk <- downloadHandler(
  filename = function() {
    removeModal()
    paste("data", ".csv", sep = "")
  },
  content = function(file) {
    write.table(grund_tabel_csv_mellemk(), file, row.names = FALSE, sep = ";",dec = ",", fileEncoding = 'latin1')
  }
)


# observe({
#   x <- list(
#   if (input$mellemk_aar_eller_kvartal == 1) {
#     c('2022','2021')
#   } else {
#         '2021' = c('4. kvartal 2021' = '2021 - 4. kvartal',
#                    '3. kvartal 2021' = '2021 - 3. kvartal',
#                    '2. kvartal 2021' = '2021 - 2. kvartal',
#                    '1. kvartal 2021' = '2021 - 1. kvartal')
#       )
#   y <- 'Vælg kvartal'
#   updatePickerInput(session, "mellemk_periode",choices = x, label = y
#   )
# })

observe({
  x <-
    if (input$mellemk_aar_eller_kvartal == 1) {
      c('2022','2021')
    } else {
      list('2022' = c('4. kvartal 2022' = '2022 - 4. kvartal',
                      '3. kvartal 2022' = '2022 - 3. kvartal',
                      '2. kvartal 2022' = '2022 - 2. kvartal',
                      '1. kvartal 2022' = '2022 - 1. kvartal'),
        '2021' = c('4. kvartal 2021' = '2021 - 4. kvartal',
                   '3. kvartal 2021' = '2021 - 3. kvartal',
                   '2. kvartal 2021' = '2021 - 2. kvartal',
                   '1. kvartal 2021' = '2021 - 1. kvartal')
        )
    }
  y <-
    if (input$mellemk_aar_eller_kvartal == 1) {
      'Vælg år' } else {'Vælg kvartal'}
  updatePickerInput(session, "mellemk_periode",choices = x, label = y
  )
})






# Nulstil filtre

observeEvent(input$nulstil_filtre_mellemk, {
  updateMaterialSwitch(session, "mellemk_landstotal",
                       value = FALSE)
  updatePickerInput(session, "mellemk_periode",
                    selected = FALSE)
  updatePickerInput(session, "mellemk_lovgrundlag",
                    selected = FALSE)
  updatePickerInput(session, "mellemk_kommune",
                     selected = 1)
  updateRadioButtons(session, "som_kolonner_mellemk",
                     selected = 1)
  updateRadioButtons(session, "som_raekker_mellemk",
                     selected = 2)
  updateRadioButtons(session, "vises_fodnoter_mellemk",
                     selected = TRUE)
  updateRadioButtons(session, "vises_i_alt_mellemk",
                     selected = TRUE)
})

# UI-elementer

output$eksport_boks_mellemk <- renderUI({
  req(len_valg_mellemkommunal() >= 1, len_valg_mellemkommunal() < 2000)
  eksportboks("download_data_modal_mellemk")
})

# Modaler

observeEvent(input$download_data_modal_mellemk, {
  eksportmodal("excel_mellemk", "download_csv_mellemk")
})

observeEvent(input$info_antal_sager_mellemk, {
  showModal(modalDialog(
    title = html("<strong>Sager om mellemkommunal uenighed</strong>"),
    h3("Behandlede sager"),
    html("
    <strong>Udtalelse/godkendelse</strong> (kommunestyrelseslovens § 50) betyder, at Ankestyrelsen har udtalt sig vejledende om en kommunes, regions eller kommunalt fællesskabs beslutninger eller praksis.
    Ankestyrelsen kan også have truffet en afgørelse, f.eks. om samtykke til at sælge fast ejendom uden offentligt udbud.
    <br>
    <br>
    <strong>Afvisning</strong> (kommunestyrelseslovens § 48 a) betyder, at Ankestyrelsen har besluttet, at der ikke er anledning til at rejse en tilsynssag.
    <br>
    <br>
    <strong>Sanktion</strong> (kommunestyrelseslovens §§ 50 d-51) betyder, at Ankestyrelsen har taget stilling til, om der skal anvendes en sanktion over for en kommune, region eller kommunalt fællesskab.
    <br>
    <br>
    <strong>Forhåndsudtalelse</strong> betyder, at Ankestyrelsen efter anmodning fra en kommune, region eller kommunalt fællesskab har afgivet en forudgående vejledende udtalelse om lovligheden af en disposition, inden den er foretaget.
    <br>
    <br>
    <strong>Fogedsag</strong> betyder, at Ankestyrelsen har behandlet en sag, hvor spørgsmålet er, om en kommune, region eller kommunalt fællesskab har undladt at efterleve en bindende afgørelse fra en rekurs- eller sektortilsynsmyndighed.
    Sagerne kan have forskelligt udfald: Ankestyrelsen kan beslutte at rejse en tilsynssag eller ikke at rejse en tilsynssag.
    <br>
    <br>"),
    h3("Ikke-behandlede sager"),
    html("
    <strong>Ikke-behandlede sager</strong> er sager, som Ankestyrelsen ikke har behandlet. Det kan eksempelvis være, fordi Ankestyrelsen ikke har kompetence til at behandle sagen, eller fordi sagen henlægges.
    "),

    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})

output$ikke_nok_valg_mellemk <- renderUI({
  req(length(input$mellemk_lovgrundlag) < 1 || length(valgte_instanser_mellemkommunal()) < 1 || length(input$mellemk_periode) < 1)
  h4("Data vises, når du har valgt mindst et emne, en kommue og en periode.")
}
)


# Tvangsbortadoption ------------------------------------------------------

lovgrundlag_tvang <- reactive(input$tvang_lovgrundlag)  # Skal rettes
#lovgrundlag_tvang <- reactive("Landstotal")  # Skal rettes

valgte_instanser_tvang <- reactive(
  c("Landstotal")
)
#
valgte_maal_tvang <- reactive(
  #c('Afvisning','Udtalelse', 'Forhåndsudtalelse', 'Sag.ikke.behandlet','Fogedsag')
  input$antal_sager1_tvang
)

antal_kolonner_tvang <- reactive ({
  kombiner_maal_og_periode(valgte_maal_tvang(), input$som_kolonner_tvang, input$tvang_periode)
})

valgte_maal_og_noegletal_tvang <- reactive(c(valgte_maal_tvang()))

# grundbearbejdning af data

tvangsadoption_data <- reactive({

 data_tvangsadoption_spread %>%
    select(-c(Aar)) %>% # skal rettes
    rename("Afslag på tvangsadoption" = Afslag.på.tvangsbortadoption) %>%
    filtrer_data(.,valgte_instanser_tvang(), input$tvang_lovgrundlag, input$tvang_periode) %>%
    #tilfoej_dimensionsvaerdier(., Sagsemner_udk, input$tilsyn_lov_eller_paragraf) %>%
    tilfoej_i_alt_raekker_test(., lovgrundlag_tvang(), input$vises_i_alt_tvang, FALSE)
})

# Sortering af kolonnner

sortering_kolonner_tvang <- reactive ({
  mulige_maal <- c("Tvangsbortadoption",
                   "Afslag på tvangsadoption",
                   "Hjemvisning"
  )
  valgte_perioder_2 <- input$tvang_periode
  opslag_sortering(maal = mulige_maal,
                   valgte_perioder = valgte_perioder_2,
                   som_kolonner = input$som_kolonner_tvang,
                   antal_kolonner = antal_kolonner_tvang(),
                   #procent_kolonner = procent_kolonner_tilsyn2()
                   procent_kolonner = NULL
  )
})

# dan tabel

gt_tabl_tvangsadoption <- reactive({
  tvangsadoption_data() %>%
    #tilfoej_nogletal(., "Behandlede sager i alt") %>% # Fejler, når man vælger flere sagsemner
    #tilfoej_nogletal(., "sager_i_alt") %>% # Dette skal erstattes af map el. lignende, så der kun beregnes de relevante nøgletal
    #tilfoej_nogletal(., "sager_u_alt") %>%
    select(Sagsemne, Instans, valgte_maal_og_noegletal_tvang(), periode) %>%
    pivoter_data(.,valgte_maal_og_noegletal_tvang(), input$som_kolonner_tvang) %>%
    #select("Sagsemne", "Instans", all_of(sortering_kolonner_tilsyn())) %>% # Sorterer rækkefølge på valgte maal
    dan_gt_tabel(titel = "**Sager om tvangsadoption (frigivelse)**",
                 undertitel = NULL,
                 raekke = "Sagsemne",
                 gruppe = "Instans",
                 fodnote = "Note: OBS indtil 30/6-2019 traf Ankestyrelsen alene en procesledende beslutning, mens styrelsen fra 1/7-2019 har haft kompetence til at træffe afgørelse om frigivelse til bortadoption uden samtykke.")
})

output$tabel_tvang <- render_gt({
  req(input$tvang_lovgrundlag, input$tvang_periode, input$antal_sager1_tvang) #, faelles_maal)  # der genereres ikke en tabel, før der er valg i de nøvendige filtre
  gt_tabl_tvangsadoption ()
})


# Dan excel

grundtabel_exl_tvang <- reactive({

  tvangsadoption_data() %>%
    #select(Instans, valgte_maal_og_noegletal_anbringelse(), periode) %>%
    pivoter_data(valgte_maal_og_noegletal_tvang(), input$som_kolonner_tvang)
})

output$excel_tvang <- downloadHandler(
  filename = "excelfil.xlsx",
  content = function(file) {
    Sys.sleep(0.3)
    removeModal()
    shiny::withProgress(message = 'Downloader excel-fil. Det kan tage et øjeblik.',
                        value = 0,   {
                          shiny::incProgress(1/10)
                          Sys.sleep(1)
                          shiny::incProgress(5/10)

                          wb <- createWorkbook()

                          # Danner fane
                          addWorksheet(wb, "Som tabel")

                          #sortering1 <- if (input$som_raekker_tilsyn == 1) "Sagsemne" else "Instans"
                          #sortering2 <- if (input$som_raekker_tilsyn == 1) "Instans" else "Sagsemne"

                          # Sorterer rækkefølgende af kolonnerne

                          grund_tabel <- grundtabel_exl_tvang() %>%
                            select(-"Sagsemne") %>%
                            select("Instans", all_of(sortering_kolonner_tvang())) %>%
                            #select(all_of(sortering2), all_of(sortering1), everything()) %>%
                            arrange(.[,1])

                          grund_tabel[is.na(grund_tabel)] <- 0

                          x <- grund_tabel

                          # Hjælpevariable til opsætning af ark
                          antal_raekker2 <- if (input$som_kolonner_tvang == 1) {
                            length(input$tvang_periode)
                          } else {
                            length(valgte_maal_og_noegletal_tvang())
                          }

                          startkolonne <- 2

                          writeData(wb, "Som tabel", x, startCol = 1, startRow = 2, rowNames = FALSE) # indsætter data i fanen
                          setColWidths(wb, sheet = "Som tabel", cols = 1:ncol(x), widths = "auto") # Justerer kolonnebredden

                          # Laver kolonneoverskrifter og underoverskrifter
                          kolonnenavne <- colnames(x) # tager kolonnenavnene fra datasætter
                          kolonnenavne <- kolonnenavne[startkolonne:length(kolonnenavne)] # fjerner de første to
                          kolonne_niveau_1 <- matrix(sub(".*[;]", "", kolonnenavne), nrow = 1) # fjerner tekst før punktum
                          kolonne_niveau_2 <- matrix(sub("[;].*", "", kolonnenavne), nrow = 1) # fjerner tekst efter punktum

                          # Skriver overskrifterne på de to første rækker
                          writeData(wb, "Som tabel", kolonne_niveau_1, startCol = startkolonne, startRow = 2, colNames = FALSE)
                          writeData(wb, "Som tabel", kolonne_niveau_2, startCol = startkolonne, startRow = 1, colNames = FALSE)

                          # Merger celler i den første række
                          for(i in seq(startkolonne,ncol(x),antal_raekker2)){ # loop fra startkolonne til sidste kolonne der stiger med antal værdier
                            mergeCells(wb, sheet = "Som tabel", cols = i:(i+(antal_raekker2-1)), rows = 1)
                          }

                          # kildeangivelse
                          kilde <- paste("Kilde: Ankestyrelsen (",format(Sys.Date(), "%Y"),")")
                          writeData(wb, "Som tabel", kilde, startCol = 1, startRow = nrow(x) + 4) # der er tre rækker før data og en række mellemrum

                          # definerer og tilføjer styling
                          centerStyle <- createStyle(halign = "center")
                          underline_style <- createStyle(
                            border = "Bottom"
                          )
                          venstre_style <- createStyle(
                            border = "Left"
                          )

                          styleT <- createStyle

                          addStyle(wb, "Som tabel", centerStyle, rows = 1, cols = startkolonne:ncol(x))
                          addStyle(wb, "Som tabel", centerStyle, rows = 2, cols = startkolonne:ncol(x))
                          addStyle(wb, "Som tabel", underline_style, rows = 1, cols = startkolonne:ncol(x), stack = TRUE)

                          saveWorkbook(wb, file, overwrite = TRUE)
                        }
    )
  }
)

observe({
  x <- c('2022','2021','2020','2019','2018')
  y <-'Vælg år'
  updatePickerInput(session, "tvang_periode",choices = x, label = y
                    #selected = list('2017','2018','2019')
  )
})

# Nulstil filtre

observeEvent(input$nulstil_filtre_tvang, {
  updatePickerInput(session, "tvang_periode",
                    selected = FALSE)
  updatePickerInput(session, "tvang_lovgrundlag",
                    selected = FALSE)
  updateRadioButtons(session, "som_kolonner_tvang",
                     selected = 1)
  updateRadioButtons(session, "vises_fodnoter_tvang",
                     selected = TRUE)
  updateRadioButtons(session, "vises_i_alt_tvang",
                     selected = TRUE)
})

# UI-elementer

output$eksport_boks_tvang <- renderUI({
  #req(len_valg_anbringelse() >= 1)
  eksportboks("download_data_modal_tvang")
})

# Modaler

observeEvent(input$download_data_modal_tvang, {
  eksportmodal("excel_tvang", "download_csv_anbringelse2")
})


observeEvent(input$info_antal_sager_tvang, {
  showModal(modalDialog(
    title = html("<strong>Sager om tvangsadoption</strong>"),
    #h3("Behandlede sager"),
    html("
    <strong>Tvangsadoption</strong> betyder, at Ankestyrelsen har vurderet, at betingelserne efter adoptionsloven for adoption uden samtykke er opfyldt. Barnet er &quot;frigivet til adoption&quot;.
    <br>
    <br>
    <strong>Afslag på tvangsadoption</strong> betyder, at Ankestyrelsen har vurderet, at betingelserne efter adoptionsloven for adoption uden samtykke ikke er opfyldt.
    <br>
    <br>
    <strong>Hjemvisning</strong> betyder, at Ankestyrelsen sender sagen tilbage til kommunen, som har indstillet sagen til det kommunale børn og unge-udvalg. Kommunen skal behandle sagen på ny og evt. indhente supplerende materiale og vurdere, om den skal forelægges for børn og unge-udvalg på ny.
    <br>
    <br>"),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})

output$ikke_nok_valg_tvang <- renderUI({
  req(length(input$tvang_lovgrundlag) < 1 || length(input$tvang_periode) < 1)
  h4("Data vises, når du har valgt mindst ét emne og én periode.")
}
)

# Anbringelse -------------------------------------------------------------


#lovgrundlag_tvang <- reactive(input$tvang_lovgrundlag)  # Skal rettes
lovgrundlag_anbringelse <- reactive("SEL Klagesager, anbringelse børn")  # Skal rettes

anbringelse_landstotal <- reactive (if(input$anbringelse_landstotal) "Hele landet" else NULL)

valgte_instanser_anbringelse <- reactive(
  #c(input$anbringelse_kommune, input$anbringelse_landstotal)
  c(input$anbringelse_kommune, anbringelse_landstotal())
)
#
valgte_maal_anbringelse <- reactive(
  "Mødebehandlet"
)

valgte_maal_og_noegletal_anbringelse <- reactive(c(valgte_maal_anbringelse()))

antal_kolonner_anbringelse <- reactive ({
  kombiner_maal_og_periode(valgte_maal_anbringelse(), input$som_kolonner_anbringelse, input$anbringelse_periode)
})

len_valg_anbringelse <- reactive ({
  length(valgte_instanser_anbringelse()) * length(input$anbringelse_periode) * 1
})

# Sortering af kolonnner

sortering_kolonner_anbringelse <- reactive ({
  mulige_maal <- c("Mødebehandlet"
  )
  valgte_perioder_2 <- input$anbringelse_periode
  opslag_sortering(maal = mulige_maal,
                   valgte_perioder = valgte_perioder_2,
                   som_kolonner = input$som_kolonner_anbringelse,
                   antal_kolonner = antal_kolonner_anbringelse(),
                   #procent_kolonner = procent_kolonner_tilsyn2()
                   procent_kolonner = NULL
  )
})

# grundbearbejdning af data

anbringelse_data <- reactive({

  data_anbringelse_spread %>%
    select(-c(Aar)) %>%
    filtrer_data(.,valgte_instanser_anbringelse(), lovgrundlag_anbringelse(), input$anbringelse_periode)
    #%>%
    #tilfoej_dimensionsvaerdier(., Sagsemner_udk, input$tilsyn_lov_eller_paragraf) %>%
    #tilfoej_i_alt_raekker(data = ., all_of(valgte_maal_tvang()), lovgrundlag_tvang(), input$vises_i_alt_tvang)
})

# dan tabel

gt_tabl_anbringelse <- reactive({
  anbringelse_data() %>%
    select(Instans, valgte_maal_og_noegletal_anbringelse(), periode) %>%
    pivoter_data(.,valgte_maal_og_noegletal_anbringelse(), input$som_kolonner_anbringelse) %>%
    #select("Sagsemne", "Instans", all_of(sortering_kolonner_tilsyn())) %>% # Sorterer rækkefølge på valgte maal
    dan_gt_tabel(titel = "**Sager om tvangsmæssige foranstaltninger**",
                 undertitel = NULL,
                 raekke = "Instans",
                 gruppe = NULL,
                 fodnote = "I sager om tvangsmæssige foranstaltninger oprettes en sag pr. barn/ung. Sagen kan indeholde flere afgørelser om tvangsmæssige foranstaltninger, men vil kun tælle med én gang i statistikken.
                 <br>
                 Felter med under tre sager er diskretioneret og markeret med et punktum.")
})

output$tabel_anbringelse <- render_gt({
  req(valgte_instanser_anbringelse(), input$anbringelse_periode) # der genereres ikke en tabel, før der er valg i de nøvendige filtre
  gt_tabl_anbringelse()
})


# Dan excel

grundtabel_exl_anbringelse <- reactive({

  anbringelse_data() %>%
    #select(Instans, valgte_maal_og_noegletal_anbringelse(), periode) %>%
    pivoter_data(valgte_maal_og_noegletal_anbringelse(), input$som_kolonner_anbringelse)

})

output$excel_anbringelse <- downloadHandler(
  filename = "excelfil.xlsx",
  content = function(file) {
    Sys.sleep(0.3)
    removeModal()
    shiny::withProgress(message = 'Downloader excel-fil. Det kan tage et øjeblik.',
                        value = 0,   {
                          shiny::incProgress(1/10)
                          Sys.sleep(1)
                          shiny::incProgress(5/10)

                          wb <- createWorkbook()

                          # Danner fane
                          addWorksheet(wb, "Som tabel")

                          #sortering1 <- if (input$som_raekker_tilsyn == 1) "Sagsemne" else "Instans"
                          #sortering2 <- if (input$som_raekker_tilsyn == 1) "Instans" else "Sagsemne"

                          # Sorterer rækkefølgende af kolonnerne

                          grund_tabel <- grundtabel_exl_anbringelse() %>%
                            select(-"Sagsemne") %>%
                            select("Instans", all_of(sortering_kolonner_anbringelse())) %>%
                            #select(all_of(sortering2), all_of(sortering1), everything()) %>%
                            arrange(.[,1])

                          grund_tabel[is.na(grund_tabel)] <- 0

                          x <- grund_tabel

                          # Hjælpevariable til opsætning af ark
                          antal_raekker2 <- if (input$som_kolonner_anbringelse == 1) {
                            length(input$anbringelse_periode)
                          } else {
                            length(valgte_maal_og_noegletal_anbringelse())
                          }

                          startkolonne <- 2

                          writeData(wb, "Som tabel", x, startCol = 1, startRow = 2, rowNames = FALSE) # indsætter data i fanen
                          setColWidths(wb, sheet = "Som tabel", cols = 1:ncol(x), widths = "auto") # Justerer kolonnebredden

                          # Laver kolonneoverskrifter og underoverskrifter
                          kolonnenavne <- colnames(x) # tager kolonnenavnene fra datasætter
                          kolonnenavne <- kolonnenavne[startkolonne:length(kolonnenavne)] # fjerner de første to
                          kolonne_niveau_1 <- matrix(sub(".*[;]", "", kolonnenavne), nrow = 1) # fjerner tekst før punktum
                          kolonne_niveau_2 <- matrix(sub("[;].*", "", kolonnenavne), nrow = 1) # fjerner tekst efter punktum

                          # Skriver overskrifterne på de to første rækker
                          writeData(wb, "Som tabel", kolonne_niveau_1, startCol = startkolonne, startRow = 2, colNames = FALSE)
                          writeData(wb, "Som tabel", kolonne_niveau_2, startCol = startkolonne, startRow = 1, colNames = FALSE)

                          # Merger celler i den første række
                          for(i in seq(startkolonne,ncol(x),antal_raekker2)){ # loop fra startkolonne til sidste kolonne der stiger med antal værdier
                            mergeCells(wb, sheet = "Som tabel", cols = i:(i+(antal_raekker2-1)), rows = 1)
                          }

                          # kildeangivelse
                          kilde <- paste("Kilde: Ankestyrelsen (",format(Sys.Date(), "%Y"),")")
                          writeData(wb, "Som tabel", kilde, startCol = 1, startRow = nrow(x) + 4) # der er tre rækker før data og en række mellemrum

                          # definerer og tilføjer styling
                          centerStyle <- createStyle(halign = "center")
                          underline_style <- createStyle(
                            border = "Bottom"
                          )
                          venstre_style <- createStyle(
                            border = "Left"
                          )

                          styleT <- createStyle

                          addStyle(wb, "Som tabel", centerStyle, rows = 1, cols = startkolonne:ncol(x))
                          addStyle(wb, "Som tabel", centerStyle, rows = 2, cols = startkolonne:ncol(x))
                          addStyle(wb, "Som tabel", underline_style, rows = 1, cols = startkolonne:ncol(x), stack = TRUE)

                          saveWorkbook(wb, file, overwrite = TRUE)
                        }
    )
  }
)


# Dan csv

grund_tabel_csv_anbringelse <- reactive ({
  anbringelse_data() %>%
    select(Instans,periode, all_of(valgte_maal_og_noegletal_anbringelse()))
  #%>%
  #left_join(Sagsemner, by = 'Sagsemne') %>%
  #   select(-c(Sorteringskode, X))
})

output$download_csv_anbringelse <- downloadHandler(
  filename = function() {
    removeModal()
    paste("data", ".csv", sep = "")
  },
  content = function(file) {
    write.table(grund_tabel_csv_anbringelse(), file, row.names = FALSE, sep = ";",dec = ",", fileEncoding = 'latin1')
  }
)

observe({
  x <- c('2022','2021','2020','2019','2018')
    # } else {
    #   list(
    #     '2021' = c('3. kvartal 2021' = '2021 - 3. kvartal',
    #                '2. kvartal 2021' = '2021 - 2. kvartal',
    #                '1. kvartal 2021' = '2021 - 1. kvartal'),
    #     '2020' = c('4. kvartal 2020' = '2020 - 4. kvartal',
    #                '3. kvartal 2020' = '2020 - 3. kvartal',
    #                '2. kvartal 2020' = '2020 - 2. kvartal',
    #                '1. kvartal 2020' = '2020 - 1. kvartal'),
    #     '2019' = c('4. kvartal 2019' = '2019 - 4. kvartal',
    #                '3. kvartal 2019' = '2019 - 3. kvartal',
    #                '2. kvartal 2019' = '2019 - 2. kvartal',
    #                '1. kvartal 2019' = '2019 - 1. kvartal'),
    #     '2018' = c('4. kvartal 2018' = '2018 - 4. kvartal',
    #                '3. kvartal 2018' = '2018 - 3. kvartal',
    #                '2. kvartal 2018' = '2018 - 2. kvartal',
    #                '1. kvartal 2018' = '2018 - 1. kvartal')
    #     # ,
    #     # '2017' = c('4. kvartal 2017' = '2017 - 4. kvartal',
    #     #            '3. kvartal 2017' = '2017 - 3. kvartal',
    #     #            '2. kvartal 2017' = '2017 - 2. kvartal',
    #     #            '1. kvartal 2017' = '2017 - 1. kvartal'),
    #     # '2016' = c('4. kvartal 2016' = '2016 - 4. kvartal',
    #     #            '3. kvartal 2016' = '2016 - 3. kvartal',
    #     #            '2. kvartal 2016' = '2016 - 2. kvartal',
    #     #            '1. kvartal 2016' = '2016 - 1. kvartal'),
    #     # '2015' = c('4. kvartal 2015' = '2015 - 4. kvartal',
    #     #            '3. kvartal 2015' = '2015 - 3. kvartal',
    #     #            '2. kvartal 2015' = '2015 - 2. kvartal',
    #     #            '1. kvartal 2015' = '2015 - 1. kvartal'),
    #     # '2014' = c('4. kvartal 2014' = '2014 - 4. kvartal',
    #     #            '3. kvartal 2014' = '2014 - 3. kvartal',
    #     #            '2. kvartal 2014' = '2014 - 2. kvartal',
    #     #            '1. kvartal 2014' = '2014 - 1. kvartal'),
    #     # '2013' = c('4. kvartal 2013' = '2013 - 4. kvartal',
    #     #            '3. kvartal 2013' = '2013 - 3. kvartal')
    #   )
    # }
  y <- 'Vælg år'
  updatePickerInput(session, "anbringelse_periode",choices = x, label = y
                    #selected = list('2017','2018','2019')
  )
})

# Nulstil filtre

observeEvent(input$nulstil_filtre_anbringelse, {
  updateMaterialSwitch(session, "anbringelse_landstotal",
                       value = FALSE)
  updatePickerInput(session, "anbringelse_periode",
                    selected = FALSE)
  updatePickerInput(session, "anbringelse_kommune",
                    selected = FALSE)
  updateRadioButtons(session, "som_kolonner_anbringelse",
                     selected = 1)
  updateRadioButtons(session, "vises_fodnoter_anbringelse",
                     selected = TRUE)
})

# UI-elementer

output$eksport_boks_anbringelse <- renderUI({
  req(len_valg_anbringelse() >= 1)
  eksportboks("download_data_modal_anbringelse")
})

# Modaler

observeEvent(input$download_data_modal_anbringelse, {
  eksportmodal("excel_anbringelse", "download_csv_anbringelse")
})

observeEvent(input$info_antal_sager_anbringelse, {
  showModal(modalDialog(
    title = html("<strong>Sager om tvangsmæssige foranstaltninger</strong>"),
    #h3("Behandlede sager"),
    html("
    <strong>Tvangsadoption</strong> betyder, at Ankestyrelsen er kommet frem til...
    <br>
    <br>
    <strong>Afslag på tvangsadoption</strong> betyder, at Ankestyrelsen...
    <br>
    <br>
    <strong>Hjemvisning</strong> betyder, at Ankestyrelsen har hjemvist sagen...
    <br>
    <br>"),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})

output$ikke_nok_valg_anbringelse <- renderUI({
  req(length(valgte_instanser_anbringelse()) < 1 || length(input$anbringelse_periode) < 1)
  h4("Data vises, når du har valgt mindst én kommune og periode.")
}
)


# Nyt sektion -------------------------------------------------------------

ask_test_samling <- reactive({
   maal_ask_til_liste <- c("Realitetsbehandlede sager i alt",
                           "Stadfæstede sager",
                           "Ændrede sager",
                           "Hjemviste sager",
                           "Ophævede sager",
                           "Afviste sager",
                           "Stadfæstelsesprocent",
                           "Omgørelsesprocent",
                           "Ændringsprocent",
                           "Hjemvisningsprocent",
                           "Ophævelsesprocent")
    # samlet_liste <- tibble(anker = paste(rep(maal_ask_til_liste,
    #                                          each = length(input$ask_in_periode)),
    #                                          rev(input$ask_in_periode),
    #                                      sep = ";")) %>%
    samlet_liste <- tibble(anker = paste(rep(if (input$ask_in_pivot == 2) rev(input$ask_in_periode) else maal_ask_til_liste,
                                             each = length(if (input$ask_in_pivot == 2) maal_ask_til_liste else input$ask_in_periode)),
                                             if (input$ask_in_pivot == 2) maal_ask_til_liste else rev(input$ask_in_periode),
                                         sep = ";")) %>%
    filter(anker %in% antal_kolonner_ask() | anker %in% procent_kolonner_ask()) %>%
    pull(anker)
})


udk_test_samling <- reactive({
   maal_udk_til_liste <- c("Realitetsbehandlede sager i alt",
                           "Stadfæstede sager",
                           "Ændrede/ophævede sager",
                           "Hjemviste sager",
                           "Afviste sager",
                           "Stadfæstelsesprocent",
                           "Omgørelsesprocent",
                           "Ændrings-/ophævelsesprocent",
                           "Hjemvisningsprocent")
    samlet_liste <- tibble(anker = paste(rep(if (input$udk_in_pivot == 2) rev(input$udk_in_periode) else maal_udk_til_liste,
                                             each = length(if (input$udk_in_pivot == 2) maal_udk_til_liste else input$udk_in_periode)),
                                             if (input$udk_in_pivot == 2) maal_udk_til_liste else rev(input$udk_in_periode),
                                         sep = ";")) %>%
    filter(anker %in% antal_kolonner_udk() | anker %in% procent_kolonner_udk()) %>%
    pull(anker)
})

ksb_test_samling <- reactive({
  maal_ksb_til_liste <- c("Realitetsbehandlede sager i alt",
                          "Stadfæstede sager",
                          "Ændrede/ophævede sager",
                          "Hjemviste sager",
                          "Afviste sager",
                          "Stadfæstelsesprocent",
                          "Omgørelsesprocent",
                          "Ændrings-/ophævelsesprocent",
                          "Hjemvisningsprocent")
  samlet_liste <- tibble(anker = paste(rep(if (input$in_pivot == 2) rev(input$in_periode) else maal_ksb_til_liste,
                                           each = length(if (input$in_pivot == 2) maal_ksb_til_liste else input$in_periode)),
                                       if (input$in_pivot == 2) maal_ksb_til_liste else rev(input$in_periode),
                                       sep = ";")) %>%
    filter(anker %in% antal_kolonner() | anker %in% procent_kolonner()) %>%
    pull(anker)
})

udk_in_lovgivning_id <- reactive ({

  if (input$udk_lov_el_paragraf == 1) {
  Sagsemner_udk %>%
    filter(Sagsemne %in% input$udk_in_lovgivning) %>%
    pull(Sorteringskode)
  }
  else input$udk_in_lovgivning
})

ask_in_lovgivning_id <- reactive ({
  Sagsemner_ask %>%
    filter(Sagsemne %in% input$ask_in_lovgivning) %>%
    pull(Sorteringskode)

})

  tekst_til_forside_sog <- reactive ({
    Sagsemner_forside %>%
      filter(Sagsemne == input$soegning_forside) %>%
      select (Omraade) %>%
      pull ()
  })

  output$Knap_paa_forsiden <- renderUI({
    req(length(input$soegning_forside) > 0, tekst_til_forside_sog() != 'Øvrige')
    tagList(
      actionBttn("forside_knap_ny_test",
                 div(paste("Gå til", tolower(input$soegning_forside)," under ", if (tekst_til_forside_sog() == 'Udbetaling Danmark-sager') tekst_til_forside_sog() else tolower(tekst_til_forside_sog())), icon("arrow-right")),
                 style = "simple",
                 block = TRUE,
                 #icon = icon("arrow-right"),
                 color = "success"
                 #icon = icon("sliders")
      ))
  })

  output$forside_link_tekst <- renderText(tekst_til_forside_sog())


  output$ovrige_paa_forsiden <- renderUI({
    req(length(input$soegning_forside) > 0, tekst_til_forside_sog() == 'Øvrige')
    tagList(
      h3("Om sager i kategorien øvrige"),
      p("Der er endnu ikke data for det valgte sagsemne på talportalen. Se listen over publikationer på ast.dk eller kontakt os på statistik@ast.dk, hvis du har spørgsmål angående data inden for disse områder."),
      tags$a(href="https://ast.dk/publikationer/emneopdelte-publikationer", "Publikationer", target="_blank")
    )


  })

  observeEvent(input$soegning_forside, {
    updateRadioButtons(session, "udk_lov_el_paragraf",
                       selected = 1)
    updateRadioButtons(session, "lov_el_paragraf",
                       selected = 1)
    updateRadioButtons(session, "ask_lov_el_paragraf",
                       selected = 1)
  })

  observeEvent(input$forside_knap_ny_test, {
    updateTabItems(session = session, "tabs",
                   if (tekst_til_forside_sog() == 'Kommunale sager')
                     "subitem6"
                   else if(tekst_til_forside_sog() == 'Udbetaling Danmark-sager')
                     "subitem7"
                   else if (tekst_til_forside_sog() == 'Arbejdsskadesager')
                     "subitem10"
                   else if (tekst_til_forside_sog() == 'Tilsyn')
                     "Tilsynssager"
                   else if (tekst_til_forside_sog() == 'Mellemkommunal uenighed')
                     "Mellemkommunal"
                   else if (tekst_til_forside_sog() == 'Underretning')
                     "Underretningssager"
                   else if (tekst_til_forside_sog() == 'Tvangsmæssige foranstaltninger')
                     "Anbringelse"
                   else if (tekst_til_forside_sog() == 'Tvangsbortadoption')
                     "Tvangsbortadoption"
                   else "subitem4"
    )

    updatePickerInput(session = session,
                      if (tekst_til_forside_sog() == 'Kommunale sager')
                        "in_lovgivning"
                      else if(tekst_til_forside_sog() == 'Udbetaling Danmark-sager')
                        "udk_in_lovgivning"
                      else if (tekst_til_forside_sog() == 'Arbejdsskadesager')
                        "ask_in_lovgivning"
                      else if (tekst_til_forside_sog() == 'Tilsyn')
                        "tilsyn_lovgrundlag"
                      else NULL,
                      selected = input$soegning_forside)

    # updateAwesomeCheckboxGroup(session = session,
    #                   if (tekst_til_forside_sog() == 'Mellemkommunal uenighed')
    #                     "mellemk_lovgrundlag"
    #                   else NULL,
    #                   selected = input$soegning_forside)

  })


  observeEvent(input$forside_ksb_kpi_antal_sager, {
    updateTabItems(session = session, "tabs", "subitem6")

  })

  observeEvent(input$forside_ksb_kpi_omgor, {
    updateTabItems(session = session, "tabs", "subitem6")

  })

  observeEvent(input$forside_ask_kpi_antal_sager, {
    updateTabItems(session = session, "tabs", "subitem10")

  })

  observeEvent(input$forside_ask_kpi_omgor, {
    updateTabItems(session = session, "tabs", "subitem10")

  })

  observeEvent(input$forside_udk_kpi_antal_sager, {
    updateTabItems(session = session, "tabs", "subitem7")

  })

  observeEvent(input$forside_udk_kpi_omgor, {
    updateTabItems(session = session, "tabs", "subitem7")

  })


  in_mall_1 <- reactive(c(input$antal_sager1,input$antal_sager2,input$antal_sager3))
  output$value <- renderPrint(checkGroup())

  in_mall_2 <- reactive(c(input$nogletal1,input$nogletal2))


  in_mall_1_udk <- reactive(c(input$udk_antal_sager1,input$udk_antal_sager2,input$udk_antal_sager3))
  #output$value <- renderPrint(checkGroup())

  in_mall_2_udk <- reactive(c(input$udk_nogletal1,input$udk_nogletal2))


  in_mall_1_ask <- reactive(c(input$ask_antal_sager1,input$ask_antal_sager2,input$ask_antal_sager3))
  #output$value <- renderPrint(checkGroup())

  in_mall_2_ask <- reactive(c(input$ask_nogletal1,input$ask_nogletal2))

  landstotal_ny <- reactive(if (input$in_landstotal) {'Hele landet'} else {})

  in_geografi <- reactive(c(input$in_kommune,landstotal_ny()))

  ## Her indlæses kvartaler for KSB!
  observe({
    x <-
    if (input$aar_el_kvartal == 1) {
     c('2022','2021','2020','2019','2018','2017','2016','2015','2014')
    } else {
      list('2022' = c('4. kvartal 2022' = '2022 - 4. kvartal',
                      '3. kvartal 2022' = '2022 - 3. kvartal',
                      '2. kvartal 2022' = '2022 - 2. kvartal',
                      '1. kvartal 2022' = '2022 - 1. kvartal'),
           '2021' = c('4. kvartal 2021' = '2021 - 4. kvartal',
                      '3. kvartal 2021' = '2021 - 3. kvartal',
                      '2. kvartal 2021' = '2021 - 2. kvartal',
                      '1. kvartal 2021' = '2021 - 1. kvartal'),
           '2020' = c('4. kvartal 2020' = '2020 - 4. kvartal',
                      '3. kvartal 2020' = '2020 - 3. kvartal',
                      '2. kvartal 2020' = '2020 - 2. kvartal',
                      '1. kvartal 2020' = '2020 - 1. kvartal'),
           '2019' = c('4. kvartal 2019' = '2019 - 4. kvartal',
                      '3. kvartal 2019' = '2019 - 3. kvartal',
                      '2. kvartal 2019' = '2019 - 2. kvartal',
                      '1. kvartal 2019' = '2019 - 1. kvartal'),
           '2018' = c('4. kvartal 2018' = '2018 - 4. kvartal',
                      '3. kvartal 2018' = '2018 - 3. kvartal',
                      '2. kvartal 2018' = '2018 - 2. kvartal',
                      '1. kvartal 2018' = '2018 - 1. kvartal'),
           '2017' = c('4. kvartal 2017' = '2017 - 4. kvartal',
                      '3. kvartal 2017' = '2017 - 3. kvartal',
                      '2. kvartal 2017' = '2017 - 2. kvartal',
                      '1. kvartal 2017' = '2017 - 1. kvartal'),
           '2016' = c('4. kvartal 2016' = '2016 - 4. kvartal',
                      '3. kvartal 2016' = '2016 - 3. kvartal',
                      '2. kvartal 2016' = '2016 - 2. kvartal',
                      '1. kvartal 2016' = '2016 - 1. kvartal'),
           '2015' = c('4. kvartal 2015' = '2015 - 4. kvartal',
                      '3. kvartal 2015' = '2015 - 3. kvartal',
                      '2. kvartal 2015' = '2015 - 2. kvartal',
                      '1. kvartal 2015' = '2015 - 1. kvartal'),
           '2014' = c('4. kvartal 2014' = '2014 - 4. kvartal',
                      '3. kvartal 2014' = '2014 - 3. kvartal',
                      '2. kvartal 2014' = '2014 - 2. kvartal',
                      '1. kvartal 2014' = '2014 - 1. kvartal'),
           '2013' = c('4. kvartal 2013' = '2013 - 4. kvartal',
                      '3. kvartal 2013' = '2013 - 3. kvartal')





      )
    }
    y <-
      if (input$aar_el_kvartal == 1) {
        'Vælg år' } else {'Vælg kvartal'}
    updatePickerInput(session, "in_periode",choices = x, label = y
                      #selected = list('2017','2018','2019')
                      )
  })



  observe({
    x <-
      if (input$udk_aar_el_kvartal == 1) {
        c('2022','2021','2020','2019','2018','2017','2016','2015')
      } else {
        list(
          '2022' = c('4. kvartal 2022' = '2022 - 4. kvartal',
                     '3. kvartal 2022' = '2022 - 3. kvartal',
                     '2. kvartal 2022' = '2022 - 2. kvartal',
                     '1. kvartal 2022' = '2022 - 1. kvartal'),
          '2021' = c('4. kvartal 2021' = '2021 - 4. kvartal',
                     '3. kvartal 2021' = '2021 - 3. kvartal',
                     '2. kvartal 2021' = '2021 - 2. kvartal',
                     '1. kvartal 2021' = '2021 - 1. kvartal'),
          '2020' = c('4. kvartal 2020' = '2020 - 4. kvartal',
                     '3. kvartal 2020' = '2020 - 3. kvartal',
                     '2. kvartal 2020' = '2020 - 2. kvartal',
                     '1. kvartal 2020' = '2020 - 1. kvartal'),
          '2019' = c('4. kvartal 2019' = '2019 - 4. kvartal',
                     '3. kvartal 2019' = '2019 - 3. kvartal',
                     '2. kvartal 2019' = '2019 - 2. kvartal',
                     '1. kvartal 2019' = '2019 - 1. kvartal'),
          '2018' = c('4. kvartal 2018' = '2018 - 4. kvartal',
                     '3. kvartal 2018' = '2018 - 3. kvartal',
                     '2. kvartal 2018' = '2018 - 2. kvartal',
                     '1. kvartal 2018' = '2018 - 1. kvartal'),
          '2017' = c('4. kvartal 2017' = '2017 - 4. kvartal',
                      '3. kvartal 2017' = '2017 - 3. kvartal',
                      '2. kvartal 2017' = '2017 - 2. kvartal',
                      '1. kvartal 2017' = '2017 - 1. kvartal'),
          '2016' = c('4. kvartal 2016' = '2016 - 4. kvartal',
                      '3. kvartal 2016' = '2016 - 3. kvartal',
                      '2. kvartal 2016' = '2016 - 2. kvartal',
                      '1. kvartal 2016' = '2016 - 1. kvartal'),
          '2015' = c('4. kvartal 2015' = '2015 - 4. kvartal',
                      '3. kvartal 2015' = '2015 - 3. kvartal',
                      '2. kvartal 2015' = '2015 - 2. kvartal',
                      '1. kvartal 2015' = '2015 - 1. kvartal')
        )
      }
    y <-
      if (input$udk_aar_el_kvartal == 1) {
        'Vælg år' } else {'Vælg kvartal'}
    updatePickerInput(session, "udk_in_periode",choices = x, label = y
                      #selected = list('2017','2018','2019')
    )
  })


  observe({
    x <-
      if (input$ask_aar_el_kvartal == 1) {
        c('2022','2021', '2020','2019','2018')
      } else {
        list(
          '2022' = c('4. kvartal 2022' = '2022 - 4. kvartal',
                     '3. kvartal 2022' = '2022 - 3. kvartal',
                     '2. kvartal 2022' = '2022 - 2. kvartal',
                     '1. kvartal 2022' = '2022 - 1. kvartal'),
          '2021' = c('4. kvartal 2021' = '2021 - 4. kvartal',
                     '3. kvartal 2021' = '2021 - 3. kvartal',
                     '2. kvartal 2021' = '2021 - 2. kvartal',
                     '1. kvartal 2021' = '2021 - 1. kvartal'),
          '2020' = c('4. kvartal 2020' = '2020 - 4. kvartal',
                     '3. kvartal 2020' = '2020 - 3. kvartal',
                     '2. kvartal 2020' = '2020 - 2. kvartal',
                     '1. kvartal 2020' = '2020 - 1. kvartal'),
          '2019' = c('4. kvartal 2019' = '2019 - 4. kvartal',
                     '3. kvartal 2019' = '2019 - 3. kvartal',
                     '2. kvartal 2019' = '2019 - 2. kvartal',
                     '1. kvartal 2019' = '2019 - 1. kvartal'),
          '2018' = c('4. kvartal 2018' = '2018 - 4. kvartal',
                     '3. kvartal 2018' = '2018 - 3. kvartal',
                     '2. kvartal 2018' = '2018 - 2. kvartal',
                     '1. kvartal 2018' = '2018 - 1. kvartal')
        )
      }
    y <-
      if (input$udk_aar_el_kvartal == 1) {
        'Vælg år' } else {'Vælg kvartal'}
    updatePickerInput(session, "ask_in_periode",choices = x, label = y
                      #selected = list('2017','2018','2019')
    )
  })



  observe({
    x <-
      if (input$tilsyn_aar_eller_kvartal == 1) {
        c('2022','2021')
      } else {
        list('2022' = c('4. kvartal 2022' = '2022 - 4. kvartal',
                        '3. kvartal 2022' = '2022 - 3. kvartal',
                        '2. kvartal 2022' = '2022 - 2. kvartal',
                        '1. kvartal 2022' = '2022 - 1. kvartal'),
          '2021' = c('4. kvartal 2021' = '2021 - 4. kvartal',
                     '3. kvartal 2021' = '2021 - 3. kvartal',
                     '2. kvartal 2021' = '2021 - 2. kvartal',
                     '1. kvartal 2021' = '2021 - 1. kvartal'))
      }
    y <-
      if (input$tilsyn_aar_eller_kvartal == 1) {
        'Vælg år' } else {'Vælg kvartal'}
    updatePickerInput(session, "tilsyn_periode",choices = x, label = y
    )
  })

  observe({
    x <-
      if (input$lov_el_paragraf == 1) {

        lapply(split(as.character(Sagsemner$Sagsemne), Sagsemner$Lovgivning), as.list)


      } else {
        list(
          'Alle paragraffer - serviceloven',
          'Alle paragraffer - andre love - tidligere nævnsområder',
          'Alle paragraffer - dagtilbudsloven',
          'Alle paragraffer - almenboligloven',
          'Alle paragraffer - boligstøtteloven',
          'Alle paragraffer - sygedagpengeloven',
          'Alle paragraffer - Lov om højeste, mellemste førtidspension m.fl.',
          'Alle paragraffer - Lov om social pension',
          'Alle paragraffer - aktivloven',
          'Alle paragraffer - tidligere lov om aktiv beskæftigelsesindsats (til og med 2019)',
          'Alle paragraffer - lov om aktiv beskæftigelsesindsats (fra 2020)',
          'Alle paragraffer - seniorjobloven',
          'Alle paragraffer - integrationsloven',
          'Alle paragraffer - repatrieringsloven',
          'Alle paragraffer - lov om kompensation til handicappede i erhverv',
          'Alle paragraffer - fleksydelsesloven',
          'Alle paragraffer - kontantydelsesloven',
          'Alle paragraffer - lov om forsøg med socialt frikort',
          'Alle paragraffer - lov om anvendelse af tvang i psykiatrien m.v.',
          'Alle paragraffer - friplejeboligloven'

        )
      }
    y <-
      if (input$lov_el_paragraf == 1) {
        'Vælg paragraffer' } else {'Vælg lovgivning'}
    z <- if(input$lov_el_paragraf == 1) {
      list(tokens = Sagsemner$X)} else {NULL}
    updatePickerInput(session, "in_lovgivning",choices = x, label = y, choicesOpt = z

    )
  })



  observe({
    x <-
        #lapply(split(as.character(Sagsemner_tilsyn_tb$Sagsemner_tilsyn), Sagsemner_tilsyn_tb$Lovgrundlag), as.list)
       as.character(Sagsemner_tilsyn)
   y <- NULL
    updatePickerInput(session, "tilsyn_lovgrundlag",choices = x, choicesOpt = list(subtext = y)

    )
  })



  observe({
    x <-
      if (input$udk_lov_el_paragraf == 1) {

        lapply(split(as.character(Sagsemner_udk$Sagsemne), Sagsemner_udk$Lovgrundlag), as.list)
        #setNames(as.character(Sagsemner_udk$Sorteringskode), Sagsemner_udk$Sagsemne)

      } else if (input$udk_lov_el_paragraf == 2) {
        # list(
        #   'Alle paragraffer - aktivloven',
        #   'Alle paragraffer - fleksydelsesloven',
        #   'Alle paragraffer - Pensionsloven',
        #   'Alle paragraffer - bidragsopkrævningsloven',
        #   'Alle paragraffer - boligstøtteloven',
        #   'Alle paragraffer - sygedagpengeloven',
        #   'Alle paragraffer - Lov om højeste, mellemste førtidspension m.fl.',
        #   'Alle paragraffer - Lov om social pension',
        #   'Alle paragraffer - aktivloven',
        #   'Alle paragraffer - tidligere lov om aktiv beskæftigelsesindsats (til og med 2019)',
        #   'Alle paragraffer - lov om aktiv beskæftigelsesindsats (fra 2020)',
        #   'Alle paragraffer - seniorjobloven',
        #   'Alle paragraffer - integrationsloven',
        #   'Alle paragraffer - repatrieringsloven',
        #   'Alle paragraffer - lov om kompensation til handicappede i erhverv',
        #   'Alle paragraffer - fleksydelsesloven',
        #   'Alle paragraffer - kontantydelsesloven'
        # )
        unique(as.character(Sagsemner_udk$Lovgrundlag))
      }

    else {
      lapply(split(as.character(Sagsemner_udk_sagsomr$Sagsemne), Sagsemner_udk_sagsomr$Ydelseskategori), as.list)
    }
    y <- if (input$udk_lov_el_paragraf == 1) Sagsemner_udk$Paragraffer else NULL
    #updatePickerInput(session, "udk_in_lovgivning",choices = x, choicesOpt = list(subtext = Sagsemner_udk$Paragraffer) #label = y #, choicesOpt = z
    updatePickerInput(session, "udk_in_lovgivning",choices = x, choicesOpt = list(subtext = y) #label = y #, choicesOpt = z

    )
  })








# Feedback ----------------------------------------------------------------

# observeEvent(input$sagsbehandlingstid_ksb,
#                green_feedback("sagsbehandlingstid_ksb", ignoreNULL = FALSE))

# Sagsudfald KSB

# observeEvent(input$in_kommune, {
#   if (length(input$in_kommune) > 0) {
#     showFeedbackSuccess(
#       inputId = "in_kommune",
#       icon = NULL
#     )
#     } else {
#     hideFeedback("in_kommune")
#     }
#
#   }, ignoreNULL = FALSE) # normalt vil observeEvent ikke blive triggered, hvis der ikke er valgt værdier (null) i filteret. Men i dette tilfælde er der brug for det for at fjerne feedback, når der ikke er valgt en værdi
#
# observeEvent(input$in_lovgivning, {
#   if (length(input$in_lovgivning) != 0) {
#      showFeedbackSuccess(
#         inputId = "in_lovgivning",
#         icon = NULL
#       )
#     } else {
#       hideFeedback("in_lovgivning")
#     }
#
#   }, ignoreNULL = FALSE)
#
# observeEvent(input$in_periode, {
#   if (length(input$in_periode) > 0) {
#     showFeedbackSuccess(
#       inputId = "in_periode",
#       icon = NULL
#     )
#   } else {
#     hideFeedback("in_periode")
#   }
#
# }, ignoreNULL = FALSE)

# Sagsbehandlingstid - ASK

observeEvent(input$sagsbehandlingstid_ASK, {
  if (length(input$sagsbehandlingstid_ASK) > 0) {
    showFeedbackSuccess(
      inputId = "sagsbehandlingstid_ASK"
    )
  } else {
    hideFeedback("sagsbehandlingstid_ASK")
  }

}, ignoreNULL = FALSE) # normalt vil observeEvent ikke blive triggered, hvis der ikke er valgt værdier (null) i filteret. Men i dette tilfælde er der brug for det for at fjerne feedback, når der ikke er valgt en værdi

observeEvent(input$sagsbehandlingstid_ASK2, {
  if (length(input$sagsbehandlingstid_ASK2) != 0) {
    showFeedbackSuccess(
      inputId = "sagsbehandlingstid_ASK2"
    )
  } else {
    hideFeedback("sagsbehandlingstid_ASK2")
  }

}, ignoreNULL = FALSE)

observeEvent(input$sagsbehandlingstid_ASK3, {
  if (length(input$sagsbehandlingstid_ASK3) > 0) {
    showFeedbackSuccess(
      inputId = "sagsbehandlingstid_ASK3"
    )
  } else {
    hideFeedback("sagsbehandlingstid_ASK3")
  }

}, ignoreNULL = FALSE)

# Sagsbehandlingstid - KSB

# observeEvent(input$sagsbehandlingstid_ksb, {
#   if (length(input$sagsbehandlingstid_ksb) > 0) {
#     showFeedbackSuccess(
#       inputId = "sagsbehandlingstid_ksb"
#     )
#   } else {
#     hideFeedback("sagsbehandlingstid_ksb")
#   }
#
# }, ignoreNULL = FALSE) # normalt vil observeEvent ikke blive triggered, hvis der ikke er valgt værdier (null) i filteret. Men i dette tilfælde er der brug for det for at fjerne feedback, når der ikke er valgt en værdi
#
# observeEvent(input$sagsbehandlingstid_ksb2, {
#   if (length(input$sagsbehandlingstid_ksb2) != 0) {
#     showFeedbackSuccess(
#       inputId = "sagsbehandlingstid_ksb2"
#     )
#   } else {
#     hideFeedback("sagsbehandlingstid_ksb2")
#   }
#
# }, ignoreNULL = FALSE)

# Links -------------------------------------------------------------------

# Header

observeEvent(input$header_knap_forside, {

  updateTabsetPanel(session = session, "ksb_tabs",
                    selected = "Tabel")

  updateTabsetPanel(session = session, "udk_tabs",
                    selected = "Tabel")

  updateTabsetPanel(session = session, "ask_tabs",
                    selected = "Tabel")

  updateTabItems(session = session, "tabs", "forside")

  updatePickerInput(session, "soegning_forside",
                    selected = FALSE)

    updateMaterialSwitch(session, "in_landstotal",
                         value = FALSE)
    updatePickerInput(session, "in_periode",
                      selected = FALSE)
    updatePickerInput(session, "in_lovgivning",
                      selected = FALSE)
    updatePickerInput(session, "in_kommune",
                      selected = FALSE)
    updateRadioButtons(session, "aar_el_kvartal",
                       selected = 1)
    updateCheckboxGroupInput(session, "antal_sager2",
                             selected = FALSE)
    updateCheckboxGroupInput(session, "antal_sager3",
                             selected = FALSE)
    updateCheckboxGroupInput(session, "nogletal1",
                             selected = FALSE)
    updateCheckboxGroupInput(session, "nogletal2",
                             selected = FALSE)
    updateCheckboxGroupInput(session, "sagsbehandlingstider1",
                             selected = FALSE)
    updateRadioButtons(session, "lov_el_paragraf",
                       selected = 1)
    updatePickerInput(session, "udk_in_periode",
                      selected = FALSE)
    updatePickerInput(session, "udk_in_lovgivning",
                      selected = FALSE)
    updateRadioButtons(session, "udk_aar_el_kvartal",
                       selected = 1)
    updateCheckboxGroupInput(session, "udk_antal_sager2",
                             selected = FALSE)
    updateCheckboxGroupInput(session, "udk_antal_sager3",
                             selected = FALSE)
    updateCheckboxGroupInput(session, "udk_nogletal1",
                             selected = FALSE)
    updateCheckboxGroupInput(session, "udk_nogletal2",
                             selected = FALSE)
    updateCheckboxGroupInput(session, "udk_sagsbehandlingstider1",
                             selected = FALSE)
    updateRadioButtons(session, "udk_lov_el_paragraf",
                       selected = 1)
    updatePickerInput(session, "ask_in_periode",
                      selected = FALSE)
    updatePickerInput(session, "ask_in_lovgivning",
                      selected = FALSE)
    updateRadioButtons(session, "ask_aar_el_kvartal",
                       selected = 1)
    updateCheckboxGroupInput(session, "ask_antal_sager2",
                             selected = FALSE)
    updateCheckboxGroupInput(session, "ask_antal_sager3",
                             selected = FALSE)
    updateCheckboxGroupInput(session, "ask_nogletal1",
                             selected = FALSE)
    updateCheckboxGroupInput(session, "ask_nogletal2",
                             selected = FALSE)
    updateCheckboxGroupInput(session, "ask_sagsbehandlingstider1",
                             selected = FALSE)
    updateRadioButtons(session, "ask_lov_el_paragraf",
                       selected = 1)

})

observeEvent(input$header_knap_sagsbehandlingstider, {
  updateTabItems(session = session, "tabs", "subitem0")
})

observeEvent(input$header_knap_sagsudfald, {
  updateTabItems(session = session, "tabs", "subitem5")
})

observeEvent(input$header_knap_om, {
  updateTabItems(session = session, "tabs", "om")
})

# Forside

observeEvent(input$forside_knap_sagsudfald_oversigt, {
    updateTabItems(session = session, "tabs", "subitem5")
  })

observeEvent(input$forside_knap_sagsudfald_ksb, {
  updateTabItems(session = session, "tabs", "subitem6")
})

observeEvent(input$forside_knap_sagsbehandlingstider_oversigt, {
  updateTabItems(session = session, "tabs", "subitem0")
})

observeEvent(input$forside_knap_sagsbehandlingstider_ksb, {
  updateTabItems(session = session, "tabs", "subitem1")
})

observeEvent(input$forside_knap_sagsbehandlingstider_arbejdsskade, {
  updateTabItems(session = session, "tabs", "subitem_sagsbehandlingstider_ask")
})


# nye

observeEvent(input$KSB_knap_forside_ny, {
  updateTabItems(session = session, "tabs", "subitem6")
})

observeEvent(input$UDK_knap_forside_ny, {
  updateTabItems(session = session, "tabs", "subitem7")
})

observeEvent(input$ASk_knap_forside_ny, {
  updateTabItems(session = session, "tabs", "subitem10")
})

observeEvent(input$underretning_knap_forside_ny, {
  updateTabItems(session = session, "tabs", "Underretningssager")
})

observeEvent(input$tvang_knap_forside_ny, {
  updateTabItems(session = session, "tabs", "Tvangsbortadoption")
})

observeEvent(input$anbringelse_knap_forside_ny, {
  updateTabItems(session = session, "tabs", "Anbringelse")
})

observeEvent(input$mellemkommunal_knap_forside_ny, {
  updateTabItems(session = session, "tabs", "Mellemkommunal")
})

observeEvent(input$tilsyn_knap_forside, {
  updateTabItems(session = session, "tabs", "Tilsynssager")
})

# Sagsudfald - oversigt

observeEvent(input$sagsudfald_oversigt_link_KSB, {
  updateTabItems(session = session, "tabs", "subitem6")
})

# Sagsbehandlingstid - oversigt

observeEvent(input$sagsbehandlingstider_oversigt_link_ASK, {
  updateTabItems(session = session, "tabs", "subitem_sagsbehandlingstider_ask")
})

observeEvent(input$sagsbehandlingstider_oversigt_link_KSB, {
  updateTabItems(session = session, "tabs", "subitem1")
})

# definerer rækker og kolonner på baggrund af valg i pivot ----------------

v_rownames <- reactive ({
  if (input$in_pivot2 == 1) "Kommune" else "Sagsemne"
  })

v_groupnames <- reactive ({
  if (input$in_pivot2 == 1) "Sagsemne" else "Kommune"
})



# v_columns <- reactive ({
#   if (input$in_pivot == "Lovgivning") {
#       input$in_lovgivning}
#       else {
#       input$in_periode
#   }
#   })

v_titel <-  reactive ({
  if (length(input$in_kommune) == 1) {
      input$in_kommune}
      else {
      "Flere kommuner"
      }
  })

v_note <- reactive ({
  if ("2017" %in% input$in_periode) {
    "Her er en note der kun bliver vist, hvis 2017 er valgt"
     } else {
      "asdasd"
    }
  })

v_stubhead <- reactive ({
  if (length(input$in_kommune) == 1) {
    NULL
    } else {"Kommune"
    }
})

# input fra de to målingsfelter. Bør måske omskrives. ---------------------

valgte_maal1 <- reactive ({
 #input$in_mall_1
  in_mall_1()
})

valgte_maal2 <- reactive ({
  in_mall_2()
})

faelles_maal <- reactive ({
 # c(input$in_mall_1,input$in_mall_2)
  c(in_mall_1(),in_mall_2())
})

faelles_laenge <- reactive ({
  # sum(length(input$in_mall_1),length(input$in_mall_2))
  sum(length(in_mall_1()),length(in_mall_2()))
})

# variable med kolonnenavne, disse bruges til styling af tabellen efterfølgende --------

# procent_kolonner <- reactive ({
#   paste(rep(input$in_mall_2, each = length(input$in_periode)), input$in_periode, sep = ".")
# })

procent_kolonner <- reactive ({
  if (length(in_mall_2()) > 0) {
  paste(rep(if (input$in_pivot == 2) input$in_periode else in_mall_2(), each = length(input$in_periode)), if (input$in_pivot == 2) in_mall_2() else input$in_periode, sep = ";")
  } else {FALSE}
})

antal_kolonner <- reactive ({
  if(length(in_mall_1()) > 0) {
    paste(rep(if (input$in_pivot == 2) input$in_periode else in_mall_1(), each = length(input$in_periode)), if (input$in_pivot == 2) in_mall_1() else input$in_periode, sep = ";")
  } else {FALSE}
    })


procent_kolonner_udk <- reactive ({
  if (length(in_mall_2_udk()) > 0) {
    paste(rep(if (input$udk_in_pivot == 2) input$udk_in_periode else in_mall_2_udk(), each = length(input$udk_in_periode)), if (input$udk_in_pivot == 2) in_mall_2_udk() else input$udk_in_periode, sep = ";")
  } else {FALSE}
})

antal_kolonner_udk <- reactive ({
  if(length(in_mall_1()) > 0) {
    paste(rep(if (input$udk_in_pivot == 2) input$udk_in_periode else in_mall_1_udk(), each = length(input$udk_in_periode)), if (input$udk_in_pivot == 2) in_mall_1_udk() else input$udk_in_periode, sep = ";")
  } else {FALSE}
})

procent_kolonner_ask <- reactive ({
  if (length(in_mall_2_ask()) > 0) {
    paste(rep(if (input$ask_in_pivot == 2) input$ask_in_periode else in_mall_2_ask(), each = length(input$ask_in_periode)), if (input$ask_in_pivot == 2) in_mall_2_ask() else input$ask_in_periode, sep = ";")
  } else {FALSE}
})

antal_kolonner_ask <- reactive ({
  if(length(in_mall_1_ask()) > 0) {
    paste(rep(if (input$ask_in_pivot == 2) input$ask_in_periode else in_mall_1_ask(), each = length(input$ask_in_periode)), if (input$ask_in_pivot == 2) in_mall_1_ask() else input$ask_in_periode, sep = ";")
  } else {FALSE}
})

uge_kolonner_ask <- reactive ({
  if(length(input$ask_sagsbehandlingstider1) > 0) {
    paste(rep(if (input$ask_in_pivot == 2) input$ask_in_periode else input$ask_sagsbehandlingstider1, each = length(input$ask_in_periode)), if (input$ask_in_pivot == 2) input$ask_sagsbehandlingstider1 else input$ask_in_periode, sep = ";")
  } else {FALSE}
})

uge_kolonner_udk <- reactive ({
  if(length(input$udk_sagsbehandlingstider1) > 0) {
    paste(rep(if (input$udk_in_pivot == 2) input$udk_in_periode else input$udk_sagsbehandlingstider1, each = length(input$udk_in_periode)), if (input$udk_in_pivot == 2) input$udk_sagsbehandlingstider1 else input$udk_in_periode, sep = ";")
  } else {FALSE}
})



# Hvad laver denne?


ksb_kommune_valg_2 <- reactive ({

if (length(input$in_kommune) == 0) 'Hele landet' else input$in_kommune

})



# input fra de to målingsfelter UDK -sager ---------------------

valgte_maal1_udk <- reactive ({
  #input$in_mall_1
  in_mall_1_udk()
})

valgte_maal2_udk <- reactive ({
  in_mall_2_udk()
})

valgte_maal1_ask <- reactive ({
  #input$in_mall_1
  in_mall_1_ask()
})

valgte_maal2_ask <- reactive ({
  in_mall_2_ask()
})


faelles_maal_udk <- reactive ({
  # c(input$in_mall_1,input$in_mall_2)
  c(in_mall_1_udk(),in_mall_2_udk())
})

faelles_laenge_udk <- reactive ({
  # sum(length(input$in_mall_1),length(input$in_mall_2))
  sum(length(in_mall_1()),length(in_mall_2()))
})

faelles_maal_ask <- reactive ({
  # c(input$in_mall_1,input$in_mall_2)
  c(in_mall_1_ask(),in_mall_2_ask())
})

faelles_laenge_ask <- reactive ({
  # sum(length(input$in_mall_1),length(input$in_mall_2))
  sum(length(in_mall_1_ask()),length(in_mall_2_ask()))
})

# variable med kolonnenavne, disse bruges til styling af tabellen efterfølgende --------

# procent_kolonner <- reactive ({
#   paste(rep(input$in_mall_2, each = length(input$in_periode)), input$in_periode, sep = ".")
# })

procent_kolonner <- reactive ({
  if (length(in_mall_2()) > 0) {
    paste(rep(if (input$in_pivot == 2) input$in_periode else in_mall_2(), each = length(input$in_periode)), if (input$in_pivot == 2) in_mall_2() else input$in_periode, sep = ";")
  } else {FALSE}
})

antal_kolonner <- reactive ({
  if(length(in_mall_1()) > 0) {
    paste(rep(if (input$in_pivot == 2) input$in_periode else in_mall_1(), each = length(input$in_periode)), if (input$in_pivot == 2) in_mall_1() else input$in_periode, sep = ";")
  } else {FALSE}
})



# Forsøg med tekst --------------------------------------------------------


len_valg <- reactive ({

  # length(input$in_kommune) * length(input$in_lovgivning) * length(input$in_periode) * length(input$in_mall_1) * length(input$in_mall_2)
  length(in_geografi()) * length(input$in_lovgivning) * length(input$in_periode) * (length(in_mall_1()) + length(in_mall_2()))
})

len_valg_graf <- reactive ({

  # length(input$in_kommune) * length(input$in_lovgivning) * length(input$in_periode) * length(input$in_mall_1) * length(input$in_mall_2)
  length(in_geografi()) * length(input$in_lovgivning) * length(input$in_periode)
})

len_valg_udk <- reactive ({

  # length(input$in_kommune) * length(input$in_lovgivning) * length(input$in_periode) * length(input$in_mall_1) * length(input$in_mall_2)
  length(input$udk_in_lovgivning) * length(input$udk_in_periode) * (length(in_mall_1_udk()) + length(in_mall_2_udk()))
})

len_valg_graf_udk <- reactive ({

  # length(input$in_kommune) * length(input$in_lovgivning) * length(input$in_periode) * length(input$in_mall_1) * length(input$in_mall_2)
  length(input$udk_in_lovgivning) * length(input$udk_in_periode)
})

len_valg_graf_ask <- reactive ({

  # length(input$in_kommune) * length(input$in_lovgivning) * length(input$in_periode) * length(input$in_mall_1) * length(input$in_mall_2)
  length(input$ask_in_lovgivning) * length(input$ask_in_periode)
})

len_valg_ask <- reactive ({

  # length(input$in_kommune) * length(input$in_lovgivning) * length(input$in_periode) * length(input$in_mall_1) * length(input$in_mall_2)
  length(input$ask_in_lovgivning) * length(input$ask_in_periode) * (length(in_mall_1_ask()) + length(in_mall_2_ask()) + length(input$ask_sagsbehandlingstider1))
})




output$selected_var <- renderText({

  paste("Antal valg:",len_valg())

})

# danner datasæt på baggrund af filtervalg --------------------------------

# alle mål bliver beregnet. Under 'select' vælges, hvilke der tages med videre på baggrund af valg i filte
# det kan overvejes af performanceovervejelser, om målene først skal beregnes, når brugeren har valgt disse i
# brugergrænsefladen. Evt. at omgørelsesprocent og antal er beregnet på forhånd, men at de øvrige mål beregnes
# hvis de vælges i brugergrænsefladen.
# som det er nu, så bliver koden genkørt, hver gang der sker ændringer til mål-filtrene
# det kan med fordel ændres, så der kun bliver genkørt, når der ændres i lovgivning, år eller kommune


# Data - sagsudfald - KSB -------------------------------------------------

grund_tabel_valgte_lovgivning <- reactive({
  data_ksb_spread %>%
    filter(Sagsemne %in% input$in_lovgivning)
})

grund_tabel_valgte_kommune <- reactive({
  grund_tabel_valgte_lovgivning() %>%
    filter(Kommune %in% in_geografi())
})

grund_tabel_valgte <- reactive({
  grund_tabel_valgte_kommune() %>%
    filter(periode %in% input$in_periode)
})

generer_totaler <- reactive ({

if (input$in_pivot2 == 2) {
  xyz <- aggregate(grund_tabel_valgte()[,6:10], by=list(Kommune = grund_tabel_valgte()$Kommune,
                                              #Aar = grund_tabel_valgte()$Aar,
                                              periode = grund_tabel_valgte()$periode,
                                              Aar = grund_tabel_valgte()$Aar), FUN=sum, na.rm=TRUE) %>%
    mutate(Sagsemne = 'I alt') %>%
    #mutate(periode = '4. kvartal 2019') %>%
    mutate(Kvartal = 4)

  xyz$Kvartal <- as.integer(xyz$Kvartal)
  xyz$periode <- as.factor(xyz$periode)
  xyz$Sagsemne <- as.factor(xyz$Sagsemne)

} else {

  xyz <- aggregate(grund_tabel_valgte()[,6:10], by=list(Sagsemne = grund_tabel_valgte()$Sagsemne,
                                                       #Aar = grund_tabel_valgte()$Aar,
                                                       periode = grund_tabel_valgte()$periode,
                                                       Aar = grund_tabel_valgte()$Aar), FUN=sum, na.rm=TRUE) %>%
    mutate(Kommune = 'I alt') %>%
    #mutate(periode = '4. kvartal 2019') %>%
    mutate(Kvartal = 4)

  xyz$Kvartal <- as.integer(xyz$Kvartal)
  xyz$periode <- as.factor(xyz$periode)
  xyz$Sagsemne <- as.factor(xyz$Sagsemne)
  xyz$Kommune <- as.factor(xyz$Kommune)

}

xyz

})

samlet_tbl <- reactive ({
  if((length(input$in_lovgivning ) > 1 && input$i_alt == 1 && input$in_pivot2 == 2) || (length(in_geografi() ) > 1 && input$i_alt == 1 && input$in_pivot2 == 1 && input$in_landstotal == FALSE))  {
  bind_rows(grund_tabel_valgte(), generer_totaler())
  } else {
    grund_tabel_valgte()
  }
})

tilføj_nøgletal <- reactive({

samlet_tbl() %>%
  tilfoj_realitetsbehandlede_sager() %>%
  mutate(.,Omgørelsesprocent = (replace_na(Ændring.Ophævelse,0) + replace_na(Hjemvisning,0)) / replace_na(`Realitetsbehandlede sager i alt`,0)) %>%
  tilfoj_hjemvisningsprocent() %>%
  tilfoj_afvisningsprocent() %>%
  mutate(.,Stadfæstelsesprocent = replace_na(Stadfæstelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0)) %>%
  mutate(.,'Ændrings-/ophævelsesprocent' = replace_na(Ændring.Ophævelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0)) %>%
  rename("Afgjorte sager" = Antal_sager_ialt) %>%
  rename("Stadfæstede sager" = Stadfæstelse) %>%
  rename("Ændrede/ophævede sager" = Ændring.Ophævelse) %>%
  rename("Hjemviste sager" = Hjemvisning) %>%
  rename("Afviste sager" = Afvisning.Henvisning)
})

grund_tabel_valgte2 <- reactive ({
  tilføj_nøgletal() %>%
    select (Kommune, Sagsemne, periode, valgte_maal1(), valgte_maal2())
})

grund_tabel <- reactive ({

grund_tabel_valgte2() %>%
    pivot_wider(
      names_from = 'periode',
      values_from = c(valgte_maal1(),valgte_maal2()),
      names_glue = if (input$in_pivot == 2) "{periode};{.value}" else "{.value};{periode}"
    )
})


# Data - sagsudfald - UDK -------------------------------------------------

grund_tabel_valgte_lovgivning_UDK <- reactive({

  if(input$udk_lov_el_paragraf == 1) {
  data_udk_spread %>%
    #filter(Sagsemne %in% input$udk_in_lovgivning) %>%
    filter(Sagsemne %in% udk_in_lovgivning_id()) %>%
    inner_join(Sagsemner_udk, by = c("Sagsemne" = "Sorteringskode")) %>%
    select(-Sagsemne) %>%
    mutate(Sagsemne = paste(Sagsemne.y, " - ", Lovgrundlag, Paragraffer)) %>%
    select(Sagsemne, Aar, Kvartal, Afvisning.Henvisning, Hjemvisning, Antal_sager_ialt, Sagsbehandlingstid, Stadfæstelse, Ændring.Ophævelse, periode) %>%
    arrange(Sagsemne)
  } else {
  data_udk_spread %>%
    filter(Sagsemne %in% udk_in_lovgivning_id()) %>%
    select(Sagsemne, Aar, Kvartal, Afvisning.Henvisning, Hjemvisning, Antal_sager_ialt, Sagsbehandlingstid, Stadfæstelse, Ændring.Ophævelse, periode)
  }



  })

grund_tabel_valgte_udk <- reactive({

  grund_tabel_valgte_lovgivning_UDK() %>%
    filter(periode %in% input$udk_in_periode)
}
)



generer_totaler_udk <- reactive ({

  xyz <- grund_tabel_valgte_udk() %>%
    mutate(Sagsbehandlingstid2 = (replace_na(Sagsbehandlingstid,0)*(replace_na(Antal_sager_ialt,0)-(replace_na(Afvisning.Henvisning,0)))
    )
    )


    xyz <- aggregate(xyz[,c(4:9,11)], by=list(periode = xyz$periode,
                                                             Aar = xyz$Aar), FUN=sum, na.rm=TRUE) %>%
      mutate(Sagsemne = 'I alt') %>%
      #mutate(periode = '4. kvartal 2019') %>%
      mutate(Kvartal = 4)

    xyz$Kvartal <- as.integer(xyz$Kvartal)
    xyz$periode <- as.factor(xyz$periode)
    xyz$Sagsemne <- as.factor(xyz$Sagsemne)


    xyz <- xyz %>%
      mutate(Sagsbehandlingstid = replace_na(Sagsbehandlingstid2,0)/(replace_na(Antal_sager_ialt,0)-(replace_na(Afvisning.Henvisning,0)))) %>%
      select(-Sagsbehandlingstid2)


    xyz

})

samlet_tbl_udk <- reactive ({
  if(length(input$udk_in_lovgivning ) > 1 && input$udk_i_alt == 1)   {
  bind_rows(grund_tabel_valgte_udk(), generer_totaler_udk())
   } else {
    grund_tabel_valgte_udk()
   }
})

tilføj_nøgletal_udk <- reactive({

  samlet_tbl_udk() %>%
    tilfoj_realitetsbehandlede_sager() %>%
    mutate(.,Omgørelsesprocent = (replace_na(Ændring.Ophævelse,0) + replace_na(Hjemvisning,0)) / replace_na(`Realitetsbehandlede sager i alt`,0)) %>%
    tilfoj_hjemvisningsprocent() %>%
    tilfoj_afvisningsprocent() %>%
    mutate(.,Stadfæstelsesprocent = replace_na(Stadfæstelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0)) %>%
    mutate(.,'Ændrings-/ophævelsesprocent' = replace_na(Ændring.Ophævelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0)) %>%
    rename("Afgjorte sager" = Antal_sager_ialt) %>%
    rename("Stadfæstede sager" = Stadfæstelse) %>%
    rename("Ændrede/ophævede sager" = Ændring.Ophævelse) %>%
    rename("Hjemviste sager" = Hjemvisning) %>%
    rename("Afviste sager" = Afvisning.Henvisning)
})

grund_tabel_valgte2_udk <- reactive ({
  tilføj_nøgletal_udk() %>%
    select (Sagsemne, periode, valgte_maal1_udk(), valgte_maal2_udk(), input$udk_sagsbehandlingstider1
    )
})

grund_tabel_udk <- reactive ({

  grund_tabel_valgte2_udk() %>%
    pivot_wider(
      names_from = 'periode',
      values_from = c(valgte_maal1_udk(),valgte_maal2_udk(),input$udk_sagsbehandlingstider1),
      names_glue = if (input$udk_in_pivot == 2) "{periode};{.value}" else "{.value};{periode}"
    )
})


# Data - sagsudfald - ASK -------------------------------------------------

grund_tabel_valgte_lovgivning_ask <- reactive({

  data_ask_spread %>%
    #filter(Sagsemne %in% input$ask_in_lovgivning)

  filter(Sagsemne %in% ask_in_lovgivning_id()) %>%
    inner_join(Sagsemner_ask, by = c("Sagsemne" = "Sorteringskode")) %>%
    select(-Sagsemne) %>%
    mutate(Sagsemne = paste(Sagsemne.y, " - ", Lovgrundlag, replace_na(Paragraffer,""))) %>%
    select(Sagsemne, Aar, Kvartal, Afvisning.Henvisning, Hjemvisning, Antal_sager_ialt, Sagsbehandlingstid, Stadfæstelse, Ændring, Ophævelse, periode) %>%
    arrange(Sagsemne)

})

grund_tabel_valgte_ask <- reactive({

  grund_tabel_valgte_lovgivning_ask() %>%
    filter(periode %in% input$ask_in_periode)
}
)



generer_totaler_ask <- reactive ({
    xyz <- grund_tabel_valgte_ask() %>%
      mutate(.,Sagsbehandlingstid2 = replace_na(Sagsbehandlingstid,0)*(replace_na(Antal_sager_ialt,0)-(replace_na(Afvisning.Henvisning,0))))
      #mutate(.,Sagsbehandlingstid2 = Sagsbehandlingstid)

    xyz2 <- aggregate(xyz[,c(4:10,12)], by=list(periode = xyz$periode,
                                                             Aar = xyz$Aar), FUN=sum, na.rm=TRUE) %>%
      mutate(Sagsemne = 'I alt') %>%
      #mutate(periode = '4. kvartal 2019') %>%
      mutate(Kvartal = 4)

    xyz2$Kvartal <- as.integer(xyz2$Kvartal)
    xyz2$periode <- as.factor(xyz2$periode)
    xyz2$Sagsemne <- as.factor(xyz2$Sagsemne)


    xyz3 <- xyz2 %>%
      mutate(.,Sagsbehandlingstid = replace_na(Sagsbehandlingstid2,0)/(replace_na(Antal_sager_ialt,0)-(replace_na(Afvisning.Henvisning,0)))) %>%
      #mutate(.,Sagsbehandlingstid = Sagsbehandlingstid) %>%
      select(-Sagsbehandlingstid2)

    xyz3

})

samlet_tbl_ask <- reactive ({
  if(length(input$ask_in_lovgivning ) > 1 && input$ask_i_alt == 1)  {
  bind_rows(grund_tabel_valgte_ask(), generer_totaler_ask())
   } else {
    grund_tabel_valgte_ask()
   }
})

tilføj_nøgletal_ask <- reactive({

  samlet_tbl_ask() %>%
    tilfoj_realitetsbehandlede_sager() %>%
    #mutate(.,Omgørelsesprocent = (replace_na(Ændring.Ophævelse,0) + replace_na(Hjemvisning,0)) / replace_na(`Realitetsbehandlede sager i alt`,0)) %>%
    mutate(.,Omgørelsesprocent = (replace_na(Ændring,0) + replace_na(Ophævelse,0) + replace_na(Hjemvisning,0)) / replace_na(`Realitetsbehandlede sager i alt`,0)) %>%
    tilfoj_hjemvisningsprocent() %>%
    tilfoj_afvisningsprocent() %>%
    mutate(.,Stadfæstelsesprocent = replace_na(Stadfæstelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0)) %>%
    #mutate(.,'Ændrings-/ophævelsesprocent' = replace_na(Ændring.Ophævelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0)) %>%
    mutate(.,'Ændringsprocent' = replace_na(Ændring,0) / replace_na(`Realitetsbehandlede sager i alt`,0)) %>%
    mutate(.,'Ophævelsesprocent' = replace_na(Ophævelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0)) %>%
    rename("Afgjorte sager" = Antal_sager_ialt) %>%
    rename("Stadfæstede sager" = Stadfæstelse) %>%
    #rename("Ændrede/ophævede sager" = Ændring.Ophævelse) %>%
    rename("Ændrede sager" = Ændring) %>%
    rename("Ophævede sager" = Ophævelse) %>%
    rename("Hjemviste sager" = Hjemvisning) %>%
    rename("Afviste sager" = Afvisning.Henvisning)
})

grund_tabel_valgte2_ask <- reactive ({
  tilføj_nøgletal_ask() %>%
    select (Sagsemne, periode, valgte_maal1_ask(), valgte_maal2_ask(), input$ask_sagsbehandlingstider1
    )
})

grund_tabel_ask <- reactive ({

  grund_tabel_valgte2_ask() %>%
    pivot_wider(
      names_from = 'periode',
      values_from = c(valgte_maal1_ask(),valgte_maal2_ask(),input$ask_sagsbehandlingstider1),
      names_glue = if (input$ask_in_pivot == 2) "{periode};{.value}" else "{.value};{periode}"
    )
})

# test af rækkefølge af kolonner






# Test-tabeller -----------------------------------------------------------

output$strfile2 <- renderPrint({str(grund_tabel_valgte())})

output$strfile <- renderPrint({str(generer_totaler())})

output$strfile3 <- renderPrint({str(samlet_tbl())})

output$antal_sager_valgte <- renderText({in_mall_1()})

output$nogletal_valgte_32 <- renderText({in_mall_2()})

output$antal_sager_valgte_periode <- renderText({antal_kolonner()})

output$nogletal_valgte_32_periode <- renderText({procent_kolonner()})

output$antal_kombinationer <- renderText(len_valg())

# Excel - KSB -------------------------------------------------------------------

tilføj_nøgletal_exl <- reactive({

  samlet_tbl() %>%
    tilfoj_realitetsbehandlede_sager() %>%
    #tilfoj_omgorelsesprocent_exl() %>%
    mutate(.,Omgørelsesprocent = ((replace_na(Ændring.Ophævelse,0) + replace_na(Hjemvisning,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    tilfoj_hjemvisningsprocent_exl() %>%
    #tilfoj_stadfaestelsesprocent_exl() %>%
    mutate(.,Stadfæstelsesprocent = ((replace_na(Stadfæstelse,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    #tilfoj_aendrings_ophaevelsesprocent_exl() %>%
    mutate(.,'Ændrings-/ophævelsesprocent' = ((replace_na(Ændring.Ophævelse,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    rename("Afgjorte sager" = Antal_sager_ialt) %>%
    rename("Stadfæstede sager" = Stadfæstelse) %>%
    rename("Ændrede/ophævede sager" = Ændring.Ophævelse) %>%
    rename("Hjemviste sager" = Hjemvisning) %>%
    rename("Afviste sager" = Afvisning.Henvisning)
})

grund_tabel_valgte2_exl <- reactive ({
  tilføj_nøgletal_exl() %>%
    select (Kommune, Sagsemne, periode, valgte_maal1(), valgte_maal2())

})

grund_tabel_exl <- reactive ({

  grund_tabel_valgte2_exl() %>%
    pivot_wider(
      names_from = 'periode',
      values_from = c(valgte_maal1(),valgte_maal2()),
      names_glue = if (input$in_pivot == 2) "{periode};{.value}" else "{.value};{periode}"
    )
})


# Excel - UDK -------------------------------------------------------------------

tilføj_nøgletal_exl_udk <- reactive({

  samlet_tbl_udk() %>%
    tilfoj_realitetsbehandlede_sager() %>%
    #tilfoj_omgorelsesprocent_exl() %>%
    mutate(.,Omgørelsesprocent = ((replace_na(Ændring.Ophævelse,0) + replace_na(Hjemvisning,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    tilfoj_hjemvisningsprocent_exl() %>%
    #tilfoj_stadfaestelsesprocent_exl() %>%
    mutate(.,Stadfæstelsesprocent = ((replace_na(Stadfæstelse,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    #tilfoj_aendrings_ophaevelsesprocent_exl() %>%
    mutate(.,'Ændrings-/ophævelsesprocent' = ((replace_na(Ændring.Ophævelse,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    rename("Afgjorte sager" = Antal_sager_ialt) %>%
    rename("Stadfæstede sager" = Stadfæstelse) %>%
    rename("Ændrede/ophævede sager" = Ændring.Ophævelse) %>%
    rename("Hjemviste sager" = Hjemvisning) %>%
    rename("Afviste sager" = Afvisning.Henvisning)
})

grund_tabel_valgte2_exl_udk <- reactive ({
  tilføj_nøgletal_exl_udk() %>%
    select (Sagsemne, periode, valgte_maal1_udk(), valgte_maal2_udk(),input$udk_sagsbehandlingstider1)

})

grund_tabel_exl_udk <- reactive ({

  grund_tabel_valgte2_exl_udk() %>%
    pivot_wider(
      names_from = 'periode',
      values_from = c(valgte_maal1_udk(),valgte_maal2_udk(),input$udk_sagsbehandlingstider1),
      names_glue = if (input$udk_in_pivot == 2) "{periode};{.value}" else "{.value};{periode}"
    )
})



# Excel - ASK -------------------------------------------------------------------

tilføj_nøgletal_exl_ask <- reactive({

  samlet_tbl_ask() %>%
    tilfoj_realitetsbehandlede_sager() %>%
    #tilfoj_omgorelsesprocent_exl() %>%
    mutate(.,Omgørelsesprocent = ((replace_na(Ændring,0) + replace_na(Hjemvisning,0) + replace_na(Ophævelse,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    tilfoj_hjemvisningsprocent_exl() %>%
    #tilfoj_stadfaestelsesprocent_exl() %>%
    mutate(.,Stadfæstelsesprocent = ((replace_na(Stadfæstelse,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    #tilfoj_aendrings_ophaevelsesprocent_exl() %>%
    mutate(.,'Ændringsprocent' = ((replace_na(Ændring,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    mutate(.,'Ophævelsesprocent' = ((replace_na(Ophævelse,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    rename("Afgjorte sager" = Antal_sager_ialt) %>%
    rename("Stadfæstede sager" = Stadfæstelse) %>%
    rename("Ændrede sager" = Ændring) %>%
    rename("Ophævede sager" = Ophævelse) %>%
    rename("Hjemviste sager" = Hjemvisning) %>%
    rename("Afviste sager" = Afvisning.Henvisning)
})

grund_tabel_valgte2_exl_ask <- reactive ({
  tilføj_nøgletal_exl_ask() %>%
    select (Sagsemne, periode, valgte_maal1_ask(), valgte_maal2_ask(),input$ask_sagsbehandlingstider1)
    #select (Sagsemne, periode, ask_test_samling())

})

grund_tabel_exl_ask <- reactive ({

  grund_tabel_valgte2_exl_ask() %>%
    pivot_wider(
      names_from = 'periode',
      values_from = c(valgte_maal1_ask(),valgte_maal2_ask(),input$ask_sagsbehandlingstider1),
      names_glue = if (input$ask_in_pivot == 2) "{periode};{.value}" else "{.value};{periode}"
    )
})


#grund_tabel2 <- grund_tabel_ask()[,c("Sagsemne", ask_test_samling())]


# Laver CSV-datasæt der ikke er pivoteret

grund_tabel_csv <- reactive ({
  left_join(grund_tabel_valgte2_exl(),Sagsemner, by = 'Sagsemne') %>%
    select(-c(Sorteringskode, X)) %>%
    select(Kommune, Lovgivning, Sagsemne, periode, valgte_maal1(), valgte_maal2())
})


# Laver CSV-datasæt der ikke er pivoteret - UDK

grund_tabel_csv_udk <- reactive ({
  left_join(grund_tabel_valgte2_exl_udk(),Sagsemner_udk, by = 'Sagsemne') %>%
    #select(-c(Sorteringskode, X)) %>%
    select(Lovgrundlag, Sagsemne, periode, valgte_maal1_udk(), valgte_maal2_udk(),input$udk_sagsbehandlingstider1)
})


# Laver CSV-datasæt der ikke er pivoteret - ASK

grund_tabel_csv_ask <- reactive ({
  left_join(grund_tabel_valgte2_exl_ask(),Sagsemner_ask, by = 'Sagsemne') %>%
    #select(-c(Sorteringskode, X)) %>%
    select(Lovgrundlag, Sagsemne, periode, valgte_maal1_ask(), valgte_maal2_ask(),input$ask_sagsbehandlingstider1)
})



# Datasæt til graf

cd3 <- reactive ({
  data_ksb_spread %>%
    filter(Sagsemne %in% input$in_lovgivning & periode %in% input$in_periode & Kommune %in% in_geografi()) %>%
    tilfoj_realitetsbehandlede_sager() %>%
    #tilfoj_omgorelsesprocent2() %>%
    mutate(.,Omgørelsesprocent = ((replace_na(Ændring.Ophævelse,0) + replace_na(Hjemvisning,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    tilfoj_hjemvisningsprocent2() %>%
    mutate(.,Stadfæstelsesprocent = (replace_na(Stadfæstelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    #tilfoj_aendrings_ophaevelsesprocent() %>%
    mutate(.,'Ændrings-/ophævelsesprocent' = (replace_na(Ændring.Ophævelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    rename("Afgjorte sager" = Antal_sager_ialt) %>%
    rename("Stadfæstede sager" = Stadfæstelse) %>%
    rename("Ændrede/ophævede sager" = Ændring.Ophævelse) %>%
    rename("Hjemviste sager" = Hjemvisning) %>%
    rename("Afviste sager" = Afvisning.Henvisning) %>%
    #select (Kommune, Sagsemne, Aar, valgte_maal1(), valgte_maal2())
    select (Kommune, Sagsemne, periode, input$`input til graf`) %>%
    rename(data_kolonne = 4)
})

cd3_udk <- reactive ({

  if(input$udk_lov_el_paragraf == 1) {
  data_udk_spread %>%
    filter(Sagsemne %in% udk_in_lovgivning_id() & periode %in% input$udk_in_periode) %>%
    inner_join(Sagsemner_udk, by = c("Sagsemne" = "Sorteringskode")) %>%
    select(-Sagsemne) %>%
    mutate(Sagsemne = paste(Sagsemne.y, " - ", Lovgrundlag)) %>%
    select(Sagsemne, Aar, Kvartal, Afvisning.Henvisning, Hjemvisning, Antal_sager_ialt, Sagsbehandlingstid, Stadfæstelse, Ændring.Ophævelse, periode) %>%
    tilfoj_realitetsbehandlede_sager() %>%
    #tilfoj_omgorelsesprocent2() %>%
    mutate(.,Omgørelsesprocent = ((replace_na(Ændring.Ophævelse,0) + replace_na(Hjemvisning,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    tilfoj_hjemvisningsprocent2() %>%
    mutate(.,Stadfæstelsesprocent = (replace_na(Stadfæstelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    #tilfoj_aendrings_ophaevelsesprocent() %>%
    mutate(.,'Ændrings-/ophævelsesprocent' = (replace_na(Ændring.Ophævelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    rename("Afgjorte sager" = Antal_sager_ialt) %>%
    rename("Stadfæstede sager" = Stadfæstelse) %>%
    rename("Ændrede/ophævede sager" = Ændring.Ophævelse) %>%
    rename("Hjemviste sager" = Hjemvisning) %>%
    rename("Afviste sager" = Afvisning.Henvisning) %>%
    #select (Kommune, Sagsemne, Aar, valgte_maal1(), valgte_maal2())
    select (Sagsemne, periode, input$`udk_input til graf`) %>%
    rename(data_kolonne = 3)
  } else {

    data_udk_spread %>%
    filter(Sagsemne %in% udk_in_lovgivning_id() & periode %in% input$udk_in_periode) %>%
    select(Sagsemne, Aar, Kvartal, Afvisning.Henvisning, Hjemvisning, Antal_sager_ialt, Sagsbehandlingstid, Stadfæstelse, Ændring.Ophævelse, periode) %>%
    tilfoj_realitetsbehandlede_sager() %>%
    #tilfoj_omgorelsesprocent2() %>%
    mutate(.,Omgørelsesprocent = ((replace_na(Ændring.Ophævelse,0) + replace_na(Hjemvisning,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    tilfoj_hjemvisningsprocent2() %>%
    mutate(.,Stadfæstelsesprocent = (replace_na(Stadfæstelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    #tilfoj_aendrings_ophaevelsesprocent() %>%
    mutate(.,'Ændrings-/ophævelsesprocent' = (replace_na(Ændring.Ophævelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    rename("Afgjorte sager" = Antal_sager_ialt) %>%
    rename("Stadfæstede sager" = Stadfæstelse) %>%
    rename("Ændrede/ophævede sager" = Ændring.Ophævelse) %>%
    rename("Hjemviste sager" = Hjemvisning) %>%
    rename("Afviste sager" = Afvisning.Henvisning) %>%
    #select (Kommune, Sagsemne, Aar, valgte_maal1(), valgte_maal2())
    select (Sagsemne, periode, input$`udk_input til graf`) %>%
    rename(data_kolonne = 3)
  }
})

cd3_ask <- reactive ({
  data_ask_spread %>%
    #filter(Sagsemne %in% input$ask_in_lovgivning & periode %in% input$ask_in_periode) %>%
    filter(Sagsemne %in% ask_in_lovgivning_id() & periode %in% input$ask_in_periode) %>%
    inner_join(Sagsemner_ask, by = c("Sagsemne" = "Sorteringskode")) %>%
    select(-Sagsemne) %>%
    mutate(Sagsemne = paste(Sagsemne.y, " - ", Lovgrundlag)) %>%
    select(Sagsemne, Aar, Kvartal, Afvisning.Henvisning, Hjemvisning, Antal_sager_ialt, Sagsbehandlingstid, Stadfæstelse, Ændring, Ophævelse, periode) %>%
    tilfoj_realitetsbehandlede_sager() %>%
    #tilfoj_omgorelsesprocent2() %>%
    mutate(.,Omgørelsesprocent = ((replace_na(Ændring,0) + replace_na(Hjemvisning,0) + replace_na(Ophævelse,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    tilfoj_hjemvisningsprocent2() %>%
    mutate(.,Stadfæstelsesprocent = (replace_na(Stadfæstelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    #tilfoj_aendrings_ophaevelsesprocent() %>%
    mutate(.,'Ændringsprocent' = (replace_na(Ændring,0) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    mutate(.,'Ophævelsesprocent' = (replace_na(Ophævelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0))*100) %>%
    rename("Afgjorte sager" = Antal_sager_ialt) %>%
    rename("Stadfæstede sager" = Stadfæstelse) %>%
    rename("Ændrede sager" = Ændring) %>%
    rename("Ophævede sager" = Ophævelse) %>%
    rename("Hjemviste sager" = Hjemvisning) %>%
    rename("Afviste sager" = Afvisning.Henvisning) %>%
    #select (Kommune, Sagsemne, Aar, valgte_maal1(), valgte_maal2())
    select (Sagsemne, periode, input$`ask_input til graf`) %>%
    rename(data_kolonne = 3)
})


sagsbehandlingstid_data <- reactive ({
sagsbehandlingstid_samlet
})

antal_kommuner <- reactive ({
  length(unique(cd3()$Kommune))
})

# afgør om der vises år eller kvartaler i periodefilteret på baggrund af input i checkbox (Id007)

# datovalg <- reactive({
#   switch(input$Id007,
#          Aar = list('2017','2018','2019'),
#          Kvartal = list('Q1-2018', 'Q2-2018')
#   )
# })
#
# observe({
#   updateSelectInput(session = session, inputId = "in_periode", choices = datovalg())
# })

# Tabel - sagsudfald - KSB ------------------------------------------------

gt_tabl <- reactive ({

  grund_tabel2 <- grund_tabel()[, order(names(grund_tabel()))] # sorterer kolonner, så tab_spanner_delim virker
  grund_tabel2 <- grund_tabel()[, c("Sagsemne", "Kommune", ksb_test_samling())] # sorterer kolonner, så tab_spanner_delim virker

  grund_tabel2 %>%
  gt(rowname_col = v_rownames(), groupname_col = v_groupnames()) %>%
  tab_spanner_delim(";") %>%
     row_group_order(
       groups = if (input$in_pivot2 == 2) ksb_kommune_valg_2() else input$in_lovgivning # landstotal kommer til at stå til sidst
        ) %>%
  tab_header(
     title = md(paste("**Ankestatistik**",v_titel()))
     ) %>%
  tab_stubhead(v_stubhead()) %>%
  fmt_missing (
    columns = if(is.logical(procent_kolonner())) c() else procent_kolonner(),
    missing_text = "--") %>%
  fmt_missing (
    columns = if(is.logical(antal_kolonner())) c() else antal_kolonner(),
    missing_text = "0") %>%
   fmt_percent (
     columns = if(is.logical(procent_kolonner())) c() else procent_kolonner(),
     decimals = 1,
     incl_space = TRUE,
     dec_mark = ",",
     sep_mark = ".") %>%
   fmt_number(
     columns = if(is.logical(antal_kolonner())) c() else antal_kolonner(),
     decimals = 0,
     dec_mark = ',',
     sep_mark = "."
     ) %>%
 tab_source_note(md(paste("Kilde: Ankestyrelsen (",format(Sys.Date(), "%Y"),")"))) %>%
 tab_style(
     style = cell_text(size = "14px"),
     location = cells_column_labels(everything())
   )  %>%
 tab_options(
    data_row.padding = px(5)
   ) %>%
 tab_options(
     summary_row.padding = px(5)
   ) %>%
 tab_options(
  row_group.padding = px(9)
   ) %>%
 tab_style(
     style = cell_text(weight = "bold"),
     locations = if (faelles_laenge() > 0) {
       cells_column_spanners(spanners = TRUE)
       } else NULL
     ) %>%
 tab_style(
   style = cell_text(whitespace = "nowrap"),
   locations = cells_body(columns = everything())
   ) %>%
 tab_style(
       style = cell_borders(
         sides = c("top"),
         color = "#BBBBBB",
         weight = px(2.0),
         style = "solid"
       ),
       locations = cells_body(
         columns = everything(),
         rows = if (input$in_pivot2 == 1) {Sagsemne == "I alt"} else {Kommune == "I alt"}
         )) %>%
 tab_options(
     row_group.font.weight = "bold"
   ) %>%
 tab_footnote(
       footnote = md("Fra 2019 er sagsemnet *Voksne - botilbud - §§ 107-110* opdelt i sagsemnerne: <br>
                     *Voksne - midlertidigt botilbud - § 107*, <br>
                     *Voksne - længerevarende botilbud - § 108* og <br>
                     *Voksne - krisecentre og forsorgshjem - §§ 109 og 110*"),
       if (input$noter_ja_nej == "Ja" && "2019" %in% input$in_periode && "Voksne - botilbud - §§ 107-110 (2013-2018)" %in% input$in_lovgivning) {
       locations = cells_column_labels(
         columns = contains('2019'))
       } else NULL
     )
 })

output$table2 <- render_gt({
  #req(input$in_kommune, input$in_lovgivning, input$in_periode, faelles_maal)  # der genereres ikke en tabel, før der er valg i de nøvendige filtre
  req(len_valg() >= 1, len_valg() <= 2000)
  gt_tabl()
})

output$download_stor <- renderUI({
 req(len_valg() > 2000, len_valg() <= 20000)
 tagList(
 h4(paste("OBS: Du har valgt:",format(len_valg(), big.mark = ".", decimal.mark = ","),"kombinationer af lovgivning, kommune og periode. Der kan maksimalt vises 2.000 kombinationer som tabel.")),
 h4("Du har dog mulighed for at downloade en CSV-fil med data for op til 20.000 kombinationer herunder:"),
 actionButton("test_download_modal2", "Download data", icon = icon("download"))
 )
}
)

output$for_mange_valgt <- renderUI({
  req(len_valg() > 20000)
  tagList(
    h4(paste("OBS: Du har valgt:",format(len_valg(), big.mark = ".", decimal.mark = ","),"kombinationer af lovgivning, kommune og periode.")),
    h4("Der kan maksimalt vælges 20.000 kombinationer af data.")
  )
}
)

output$ikke_nok_valg <- renderUI({
  req(length(in_geografi()) < 1 || length(input$in_lovgivning) < 1 || length(input$in_periode) < 1)
  h4("Data vises, når du har valgt mindst én lovgivning, kommune og periode.")
}
)

# Tabel - sagsudfald - UDK ------------------------------------------------

gt_tabl_udk <- reactive ({

  #grund_tabel2 <- grund_tabel_udk()[, order(names(grund_tabel_udk()))] # sorterer kolonner, så tab_spanner_delim virker
  grund_tabel2 <- grund_tabel_udk()[,c("Sagsemne", udk_test_samling())]

  grund_tabel2 %>%
    #gt() %>%
    #gt(rowname_col = v_rownames(), groupname_col = v_groupnames()) %>%
    gt(rowname_col = "Sagsemne") %>%
    tab_spanner_delim(";") %>%
    tab_header(
      title = md(paste("**Klager over Udbetaling Danmarks afgørelser**"))
    ) %>%
    #   tab_stubhead(v_stubhead()) %>%
    fmt_missing (
      columns = if(is.logical(procent_kolonner_udk())) c() else procent_kolonner_udk(),
      missing_text = "--") %>%
    fmt_missing (
      columns = if(is.logical(antal_kolonner_udk())) c() else antal_kolonner_udk(),
      missing_text = "0") %>%
    fmt_percent (columns = if(is.logical(procent_kolonner_udk())) c() else procent_kolonner_udk(),
                 decimals = 1,
                 incl_space = TRUE,
                 dec_mark = ",",
                 sep_mark = ".") %>%
    fmt_number(
      columns = if(is.logical(antal_kolonner_udk())) c() else antal_kolonner_udk(),
      decimals = 0,
      dec_mark = ',',
      sep_mark = "."
    ) %>%
    #   cols_align(align = "right",
    #              columns = antal_kolonner()) %>%
    fmt_number(
      columns = if(is.logical(uge_kolonner_udk())) c() else uge_kolonner_udk(),
      decimals = 1,
      dec_mark = ',',
      sep_mark = "."
    ) %>%
    fmt_missing (
      columns = if(is.logical(uge_kolonner_udk())) c() else uge_kolonner_udk,
      missing_text = "--") %>%
    tab_source_note(md(paste("Kilde: Ankestyrelsen (",format(Sys.Date(), "%Y"),")"))) %>%
    tab_style(
      style = cell_text(size = "14px"),
      location = cells_column_labels(everything())
    )  %>%
    tab_style(
      style = cell_text(whitespace = "nowrap"),
      locations = cells_body(columns = everything())
    ) %>%
    tab_options(
      data_row.padding = px(5)
    ) %>%
  #   tab_options(
  #     summary_row.padding = px(5)
  #   ) %>%
    cols_align(align = "right",
               columns = everything()) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = if (faelles_laenge_udk() > 0) {
        cells_column_spanners(spanners = TRUE)
      } else NULL
    )
  #   tab_options(
  #     row_group.padding = px(9)
  #   ) %>%
  #   tab_style(
  #     style = cell_text(weight = "bold"),
  #     #locations = cells_column_spanners(spanners = TRUE)
  #     locations = if (faelles_laenge() > 0) {
  #       cells_column_spanners(spanners = TRUE)
  #     } else NULL
  #   ) %>%
  #   tab_style(
  #     style = cell_borders(
  #       sides = c("top"),
  #       color = "#BBBBBB",
  #       weight = px(2.0),
  #       style = "solid"
  #     ),
  #     locations = cells_body(
  #       columns = everything(),
  #       rows = if (input$in_pivot2 == 1) {Sagsemne == "I alt"} else {Kommune == "I alt"}
  #     )) %>%
  #   tab_options(
  #     row_group.font.weight = "bold"
  #   )
  #   # tab_footnote(
  #   #   footnote = "Data fra 2017 indeholder kun sager fra 2. halvår",
  #   #   if (input$noter_ja_nej == 'Ja' && '2017' %in% input$in_periode) {
  #   #   locations = cells_column_labels(
  #   #     #columns = vars('Omgørelsesprocent.2017','Afgjorte sager.2017'))}
  #   #     columns = contains('2017'))
  #   #   } else NULL
  #   # ) %>%
  # # %>%
  # #   {if (length(input$in_lovgivning) > 1 && input$i_alt == 1) {
  # #   summary_rows(., groups = TRUE,
  # #                columns = antal_kolonner(),
  # #                decimals = 0,
  # #                fns = list(
  # #                  'I alt' =  ~sum(., na.rm = TRUE)
  # #                )
  # #   )} else . }
  # # %>%
  # #   {if (length(input$in_lovgivning) > 1 && input$i_alt == 1) {
  # #   (function(x) {
  # #     res <- function() x$`_data` %>%
  # #       dplyr::summarize(.,`Omgørelsesprocent;2019` = sum(`Afgjorte sager;2019`)) %>%
  # #       dplyr::pull(.,.data$`Omgørelsesprocent;2019`)
  # #
  # #     summary_rows(x, groups = TRUE, fns = list('I alt' = ~ res()), columns = vars(`Omgørelsesprocent;2019`))
  # #
  # #   })
  # #   } else . }
  # # {if (length(input$in_lovgivning) > 1) {
  # # summary_rows(., groups = TRUE,
  # #              columns = procent_kolonner(),
  # #              decimals = 0,
  # #              fns = list(
  # #                'I alt' =  ~mean(., na.rm = TRUE) * 100
  # #              )
  # # )} else . }
})

output$table2_udk <- render_gt({
  req(input$udk_in_lovgivning, input$udk_in_periode, faelles_maal_udk)  # der genereres ikke en tabel, før der er valg i de nøvendige filtre
  #req(len_valg() >= 1, len_valg() <= 2000)
  gt_tabl_udk()
})

output$download_stor_udk <- renderUI({
  req(len_valg() > 2000, len_valg() <= 20000)
  tagList(
    h4(paste("OBS: Du har valgt:",format(len_valg(), big.mark = ".", decimal.mark = ","),"kombinationer af lovgivning, kommune og periode. Der kan maksimalt vises 2.000 kombinationer som tabel.")),
    h4("Du har dog mulighed for at downloade en CSV-fil med data for op til 20.000 kombinationer herunder:"),
    actionButton("test_download_modal2", "Download data", icon = icon("download"))
  )
}
)

output$for_mange_valgt_udk <- renderUI({
  req(len_valg() > 20000)
  tagList(
    h4(paste("OBS: Du har valgt:",format(len_valg(), big.mark = ".", decimal.mark = ","),"kombinationer af lovgivning, kommune og periode.")),
    h4("Der kan maksimalt vælges 20.000 kombinationer af data.")
  )
}
)

output$ikke_nok_valg_udk <- renderUI({
  req(length(input$udk_in_lovgivning) < 1 || length(input$udk_in_periode) < 1)
  h4("Data vises, når du har valgt mindst én lovgivning og periode.")
}
)

output$ikke_nok_valg_ask <- renderUI({
  req(length(input$ask_in_lovgivning) < 1 || length(input$ask_in_periode) < 1)
  h4("Data vises, når du har valgt mindst én lovgivning og periode.")
}
)

# Tabel - sagsudfald - ASK ------------------------------------------------

gt_tabl_ask <- reactive ({

  #grund_tabel2 <- grund_tabel_ask()[, order(names(grund_tabel_ask()))] # sorterer kolonner, så tab_spanner_delim virker
  grund_tabel2 <- grund_tabel_ask()[,c("Sagsemne", ask_test_samling())] # sorterer kolonner, så tab_spanner_delim virker

  #grund_tabel2 <- grund_tabel_ask() %>%
  #  select(antal_kolonner_ask(),procent_kolonner_ask(), everything())

  note <- Sagsemner_ask[2,8]

  #note <- Sagsemner_ask$Kommentar

  grund_tabel2 %>%
    gt(rowname_col = "Sagsemne") %>%
    tab_spanner_delim(";") %>%
    tab_header(
      title = md(paste("**Klager over AES' afgørelser**"))
    ) %>%
    # #   tab_stubhead(v_stubhead()) %>%
    fmt_missing (
      columns = if(is.logical(antal_kolonner_ask())) c() else antal_kolonner_ask(),
      missing_text = "--") %>%
    fmt_missing (
      columns = if(is.logical(procent_kolonner_ask())) c() else procent_kolonner_ask(),
      missing_text = "--") %>%
    fmt_percent (columns = if(is.logical(procent_kolonner_ask())) c() else procent_kolonner_ask(),
                 decimals = 1,
                 incl_space = TRUE,
                 dec_mark = ",",
                 sep_mark = ".") %>%
    fmt_number(
      columns = if(is.logical(antal_kolonner_ask())) c() else antal_kolonner_ask(),
      decimals = 0,
      dec_mark = ',',
      sep_mark = "."
    ) %>%
    fmt_number(
      columns = if(is.logical(uge_kolonner_ask())) c() else uge_kolonner_ask(),
      decimals = 1,
      dec_mark = ',',
      sep_mark = "."
    ) %>%
    fmt_missing (
      columns = if(is.logical(uge_kolonner_ask())) c() else uge_kolonner_ask(),
      missing_text = "--") %>%
    cols_align(align = "right",
              columns = everything()) %>%
    tab_source_note(md(paste("Kilde: Ankestyrelsen (",format(Sys.Date(), "%Y"),")"))) %>%
    #tilfoj_note(., note, "2019") %>%
    tab_footnote(
      footnote = sagsemner_ask_kommentarer(),
      #locations = cells_body(rows = 3, columns = 1)
      if (input$ask_noter_ja_nej == "Ja") {
      locations = cells_stub(rows = contains(sagsemner_ask_kommentarer2()))
      } else NULL
      ) %>%
    tab_style(
      style = cell_text(size = "14px"),
      location = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = cell_text(whitespace = "nowrap"),
      locations = cells_body(columns = everything())
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = if (faelles_laenge_ask() > 0) {
        cells_column_spanners(spanners = TRUE)
      } else NULL
    ) %>%
    tab_style(
      style = cell_text(whitespace = "nowrap"),
      locations = cells_column_spanners(spanners = TRUE)
    ) %>%
    tab_style(
      style = cell_text(whitespace = "nowrap"),
      locations = cells_column_labels(everything())
    )
})

output$ask_table2 <- render_gt({
  req(input$ask_in_lovgivning, input$ask_in_periode, faelles_maal_ask)  # der genereres ikke en tabel, før der er valg i de nøvendige filtre
  #req(len_valg() >= 1, len_valg() <= 2000)
  gt_tabl_ask()
})

# output$download_stor_ask <- renderUI({
#   req(len_valg() > 2000, len_valg() <= 20000)
#   tagList(
#     h4(paste("OBS: Du har valgt:",format(len_valg(), big.mark = ".", decimal.mark = ","),"kombinationer af lovgivning, kommune og periode. Der kan maksimalt vises 2.000 kombinationer som tabel.")),
#     h4("Du har dog mulighed for at downloade en CSV-fil med data for op til 20.000 kombinationer herunder:"),
#     actionButton("test_download_modal2", "Download data", icon = icon("download"))
#   )
# }
# )

# output$for_mange_valgt_udk <- renderUI({
#   req(len_valg() > 20000)
#   tagList(
#     h4(paste("OBS: Du har valgt:",format(len_valg(), big.mark = ".", decimal.mark = ","),"kombinationer af lovgivning, kommune og periode.")),
#     h4("Der kan maksimalt vælges 20.000 kombinationer af data.")
#   )
# }
# )

# output$ikke_nok_valg_udk <- renderUI({
#   req(length(in_geografi()) < 1 || length(input$in_lovgivning) < 1 || length(input$in_periode) < 1)
#   h4("Data vises, når du har valgt mindst én lovgivning, kommune og periode.")
# }
# )

# Noter til tabel - ASK

sagsemner_ask_kommentarer <- reactive ({
  req(length(input$ask_in_periode) > 0)
  ask_datoer <- datohierarki %>%
      filter(Periode %in% input$ask_in_periode) %>%
      pull(Dato)

  min_ask <- min(ask_datoer)
  maks_ask <- max(ask_datoer)

  Sagsemner_ask %>%
  filter(Sorteringskode %in% ask_in_lovgivning_id() & !is.na(Kommentar) & (min_ask < Valid_from | maks_ask > Valid_to)) %>%
  pull(Kommentar)

})

sagsemner_ask_kommentarer2 <- reactive ({
  req(length(input$ask_in_periode) > 0)
  ask_datoer <- datohierarki %>%
    filter(Periode %in% input$ask_in_periode) %>%
    pull(Dato)

  min_ask <- min(ask_datoer)
  maks_ask <- max(ask_datoer)

  Sagsemner_ask %>%
    filter(Sorteringskode %in% ask_in_lovgivning_id() & !is.na(Kommentar) & (min_ask < Valid_from | maks_ask > Valid_to)) %>%
    pull(Sagsemne)

})




# output$ask_kommentar <- renderTable (
#   sagsemner_ask_kommentarer(), colnames = FALSE
# )
#
# output$ask_note_boks <- renderUI({
#   req(length(sagsemner_ask_kommentarer()) > 0, length(input$ask_in_periode) > 0)
#   graa_boks("Noter til tabellen", FALSE,
#             tableOutput("ask_kommentar")
#   )
#
# })


output$kommentarer_rownames <- renderText ({
  sagsemner_ask_kommentarer2()
  #gt_tabl_ask() %>%
  #filter(Sagsemne %in% sagsemner_ask_kommentarer2())
  #%>%
  #rownames() %>%
  #as.numeric()
})

# Sagsbehandlingstid ------------------------------------------------------

sagsbehandlingstid_samlet_data <- reactive ({

sagsbehandlingstid_samlet %>%
  select(-Omraade) %>%
  mutate(`Forskel (uger)` = Juli_September_2020.Sagbehandlingstid - Juli_September_2019.Sagbehandlingstid) %>%
  mutate(Udvikling = NA) %>%
  #mutate(`2019` = paste(Juli_September_2019.Sagbehandlingstid, " (",Juli_September_2019.Afgjorte_sager,")" )) %>%
  select(Sagstype, Udvikling, everything()) %>%
  arrange(if (input$sagsbehandlingstid_sortering == 1) {`Sagstype`}
            else if (input$sagsbehandlingstid_sortering == 2) {desc(`Forskel (uger)`)}
            else if (input$sagsbehandlingstid_sortering == 3) {`Forskel (uger)`}
            else if (input$sagsbehandlingstid_sortering == 4) {desc(`Juli_September_2019.Afgjorte_sager`)}
            else {`Juli_September_2019.Afgjorte_sager`}
    )

  })

# Tabel - sagsbehandlingstider overblik - 3 måneder ---------------------

output$table_sagsbehandlingstid <- render_gt({

  #mins <- group_by(d, Crime.Type) %>% slice(which.min(Crime.Rate))
  mins <- sagsbehandlingstider_udvikling %>%
    filter(Omraaede3 == "Udbetaling Danmark") %>%
    slice_min(Antal.uger)

  maks <- sagsbehandlingstider_udvikling %>%
    filter(Omraaede3 == "Udbetaling Danmark") %>%
    slice_max(Antal.uger)

  udk_graf <- sagsbehandlingstider_udvikling %>%
    filter(Omraaede3 == "Udbetaling Danmark") %>%
    ggplot(aes(x = Dato, y = Antal.uger, group = 1)) +
    geom_rect(ymin = 0, ymax = Inf,
              xmin = '2018-07', xmax = '2018-09', fill = 'azure2') +
    geom_rect(ymin = 0, ymax = Inf,
              xmin = '2019-07', xmax = '2019-09', fill = 'bisque') +
    geom_line(size = 2.5) +
    geom_point(data = mins, aes(x = Dato, y = Antal.uger), color = 'blue', size = 30) +
    geom_point(data = maks, aes(x = Dato, y = Antal.uger), color = 'red', size = 30) +
    geom_text(data = maks, aes(label = Antal.uger), vjust = 1, size = 50) +
    geom_text(data = mins, aes(label = Antal.uger), vjust = -1, size = 50) +
    #expand_limits(x = (maks$Antal.uger + (0.35 * (maks$Antal.uger - maks$Antal.uger)))) +
    theme(axis.title=element_blank(),
          axis.text.y = element_blank(), axis.ticks = element_blank()) +
    theme(strip.text.y = element_text(angle = 0, vjust=0.2, hjust=0)) +
    theme_void()

    sagsbehandlingstid_samlet_data() %>%
    gt(rowname_col = 'Sagstype') %>%
    tab_spanner_delim(".") %>%
    cols_move(
      columns = vars(Juli_September_2020.Sagbehandlingstid),
      after = vars(Juli_September_2019.Afgjorte_sager)
    ) %>%
    cols_move(
      columns = vars(Juli_September_2020.Afgjorte_sager),
      after = vars(Juli_September_2020.Sagbehandlingstid)
    ) %>%
    tab_header(
      title = md("**Udvikling gennemsnit sagsbehandlingstid (uger)**"),
      subtitle = md("Seneste *tre* måneder sammenholdt med *samme periode sidste år*")
    ) %>%
    tab_style(
      style = cell_text(size = "14px"),
      location = cells_column_labels(everything())
    )  %>%
    # data_color(
    #   columns = vars(`Forskel (uger)`),
    #   colors = scales::col_numeric(
    #     palette = c(
    #       "green", "red"),
    #     domain = c(-40,40)
    #   )
    #   )
    tab_style(
      style = list(
        cell_text(color = "green")
      ),
      locations = cells_body(
        columns = vars(`Forskel (uger)`),
        rows = `Forskel (uger)` < 0)
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "red")
      ),
      locations = cells_body(
        columns = vars(`Forskel (uger)`),
        rows = `Forskel (uger)` > 0)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "bisque")
      ),
      locations = cells_column_spanners(spanners = "Juli_September_2020")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "azure2")
      ),
      locations = cells_column_spanners(spanners = "Juli_September_2019")
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "grey")
      ),
      locations = cells_column_labels(
        columns = vars(Juli_September_2019.Afgjorte_sager)
    )) %>%
    tab_style(
      style = list(
        cell_text(color = "grey")
      ),
      locations = cells_column_labels(
        columns = vars(Juli_September_2020.Afgjorte_sager)
      )) %>%
    tab_style(
      style = list(
        cell_text(color = "grey")
      ),
      locations = cells_body(
        columns = vars(Juli_September_2020.Afgjorte_sager))
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "grey")
      ),
      locations = cells_body(
        columns = vars(Juli_September_2019.Afgjorte_sager))
    ) %>%
    cols_align(align = "right",
               columns = TRUE) %>%
    cols_label(
      Juli_September_2020.Sagbehandlingstid = "Sagsbehandlingstid",
      Juli_September_2020.Afgjorte_sager = "Afgjorte sager",
      Juli_September_2019.Sagbehandlingstid = "Sagsbehandlingstid",
      Juli_September_2019.Afgjorte_sager = "Afgjorte sager"
    ) %>%
    text_transform(
      locations = cells_body(vars(Udvikling)),
      fn = function(x) {
        udk_graf %>%
          ggplot_image(height = px(30), aspect_ratio = 7)
      }
    ) %>%
    fmt_number(
      columns = vars(Juli_September_2020.Sagbehandlingstid,
                     Juli_September_2019.Sagbehandlingstid,
                     `Forskel (uger)`),
      decimals = 1,
      dec_mark = ',',
      sep_mark = "."
    )
})


# Tabel - sagsbehandlingstider overblik - 6 måneder -----------------------

output$table_sagsbehandlingstid_6 <- render_gt({

  udk_graf <- sagsbehandlingstider_udvikling %>%
    filter(Omraaede3 == "Udbetaling Danmark") %>%
    ggplot(aes(x = Dato, y = Antal.uger, group = 1)) +
    geom_rect(ymin = 0, ymax = Inf,
              xmin = '2018-04', xmax = '2018-09', fill = 'azure2') +
    geom_rect(ymin = 0, ymax = Inf,
              xmin = '2019-04', xmax = '2019-09', fill = 'bisque') +
    geom_line(size = 2.5) +
    theme(axis.title=element_blank(),
          axis.text.y = element_blank(), axis.ticks = element_blank()) +
    theme(strip.text.y = element_text(angle = 0, vjust=0.2, hjust=0)) +
    theme_void()

  sagsbehandlingstid_samlet_data() %>%
    gt(rowname_col = 'Sagstype') %>%
    tab_spanner_delim(".") %>%
    cols_move(
      columns = vars(Juli_September_2020.Sagbehandlingstid),
      after = vars(Juli_September_2019.Afgjorte_sager)
    ) %>%
    cols_move(
      columns = vars(Juli_September_2020.Afgjorte_sager),
      after = vars(Juli_September_2020.Sagbehandlingstid)
    ) %>%
    tab_header(
      title = md("**Udvikling gennemsnit sagsbehandlingstid (uger)**"),
      subtitle = md("Seneste *seks* måneder sammenholdt med *samme periode sidste år*")
    ) %>%
    tab_style(
      style = cell_text(size = "14px"),
      location = cells_column_labels(everything())
    )  %>%
    # data_color(
    #   columns = vars(`Forskel (uger)`),
    #   colors = scales::col_numeric(
    #     palette = c(
    #       "green", "red"),
    #     domain = c(-40,40)
    #   )
    #   )
    tab_style(
      style = list(
        cell_text(color = "green")
      ),
      locations = cells_body(
        columns = vars(`Forskel (uger)`),
        rows = `Forskel (uger)` < 0)
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "red")
      ),
      locations = cells_body(
        columns = vars(`Forskel (uger)`),
        rows = `Forskel (uger)` > 0)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "bisque")
      ),
      locations = cells_column_spanners(spanners = "Juli_September_2020")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "azure2")
      ),
      locations = cells_column_spanners(spanners = "Juli_September_2019")
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "grey")
      ),
      locations = cells_column_labels(
        columns = vars(Juli_September_2019.Afgjorte_sager)
      )) %>%
    tab_style(
      style = list(
        cell_text(color = "grey")
      ),
      locations = cells_column_labels(
        columns = vars(Juli_September_2020.Afgjorte_sager)
      )) %>%
    tab_style(
      style = list(
        cell_text(color = "grey")
      ),
      locations = cells_body(
        columns = vars(Juli_September_2020.Afgjorte_sager))
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "grey")
      ),
      locations = cells_body(
        columns = vars(Juli_September_2019.Afgjorte_sager))
    ) %>%
    cols_align(align = "right",
               columns = TRUE) %>%
    cols_label(
      Juli_September_2020.Sagbehandlingstid = "Sagsbehandlingstid",
      Juli_September_2020.Afgjorte_sager = "Afgjorte sager",
      Juli_September_2019.Sagbehandlingstid = "Sagsbehandlingstid",
      Juli_September_2019.Afgjorte_sager = "Afgjorte sager"
    ) %>%
    text_transform(
      locations = cells_body(vars(Udvikling)),
      fn = function(x) {
        udk_graf %>%
          ggplot_image(height = px(30), aspect_ratio = 7)
      }
    ) %>%
    fmt_number(
      columns = vars(Juli_September_2020.Sagbehandlingstid,
                     Juli_September_2019.Sagbehandlingstid,
                     `Forskel (uger)`),
      decimals = 1,
      dec_mark = ',',
      sep_mark = "."
    )
})


# Tabel - sagsudfald - oversigt -------------------------------------------

output$table_sagsudfald_oversigt_tabel <- render_gt({

udk_graf <- sagsbehandlingstider_udvikling %>%
  filter(Omraaede3 == "Udbetaling Danmark") %>%
  ggplot(aes(x = Dato, y = Antal.uger, group = 1)) +
  # geom_rect(ymin = 0, ymax = Inf,
  #           xmin = '2018-04', xmax = '2018-09', fill = 'azure2') +
  # geom_rect(ymin = 0, ymax = Inf,
  #           xmin = '2019-04', xmax = '2019-09', fill = 'bisque') +
  geom_bar(stat = "identity") +
  theme(axis.title=element_blank(),
        axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(strip.text.y = element_text(angle = 0, vjust=0.2, hjust=0)) +
  theme_void()

sagsbehandlingstid_samlet_data() %>%
  select(-c(Juli_September_2020.Sagbehandlingstid,Juli_September_2019.Sagbehandlingstid)) %>%
  gt(rowname_col = 'Sagstype') %>%
  tab_header(
    title = md("**Udvikling gennemsnit sagsbehandlingstid (uger)**"),
    subtitle = md("Seneste *seks* måneder sammenholdt med *samme periode sidste år*")
  ) %>%
  tab_style(
    style = cell_text(size = "14px"),
    location = cells_column_labels(everything())
  )  %>%
  tab_style(
    style = list(
      cell_text(color = "green")
    ),
    locations = cells_body(
      columns = vars(`Forskel (uger)`),
      rows = `Forskel (uger)` < 0)
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "red")
    ),
    locations = cells_body(
      columns = vars(`Forskel (uger)`),
      rows = `Forskel (uger)` > 0)
  ) %>%
  text_transform(
    locations = cells_body(vars(Udvikling)),
    fn = function(x) {
      udk_graf %>%
        ggplot_image(height = px(30), aspect_ratio = 7)
    }
  )
})

# Download af CSV-fil -----------------------------------------------------

output$downloadData_csv <- downloadHandler(
  filename = function() {
    removeModal()
    paste("data", ".csv", sep = "")
  },
  content = function(file) {
    write.table(grund_tabel_csv(), file, row.names = FALSE, sep = ";",dec = ",", fileEncoding = 'latin1')
  }
)

# Download af CSV-fil - UDK -----------------------------------------------------

output$downloadData_csv_udk <- downloadHandler(
  filename = function() {
    removeModal()
    paste("data", ".csv", sep = "")
  },
  content = function(file) {
    write.table(grund_tabel_csv_udk(), file, row.names = FALSE, sep = ";",dec = ",", fileEncoding = 'latin1')
  }
)

# Download af CSV-fil - ASK -----------------------------------------------------

output$downloadData_csv_ask <- downloadHandler(
  filename = function() {
    removeModal()
    paste("data", ".csv", sep = "")
  },
  content = function(file) {
    write.table(grund_tabel_csv_ask(), file, row.names = FALSE, sep = ";",dec = ",", fileEncoding = 'latin1')
  }
)

# Download af excel-fil ---------------------------------------------------

 #output$downloadData_xlsx <- downloadHandler(
 output$downloadData_xlsx22 <- downloadHandler(

  filename = "excelfil.xlsx",
  content = function(file) {
  Sys.sleep(0.3)
  removeModal()
  shiny::withProgress(message = 'Downloader excel-fil. Det kan tage et øjeblik.',
                      value = 0,   {

  shiny::incProgress(1/10)
  Sys.sleep(1)
  shiny::incProgress(5/10)

    wb <- createWorkbook()

    # Danner fane
    addWorksheet(wb, "Som tabel")


    sortering1 <- if (input$in_pivot2 == 1) "Kommune" else "Sagsemne"
    sortering2 <- if (input$in_pivot2 == 1) "Sagsemne" else "Kommune"

    # Sorterer rækkefølgende af kolonnerne
    #grund_tabel2 <- grund_tabel_exl()[, order(names(grund_tabel()))]
    grund_tabel2 <- grund_tabel_exl()[, c("Sagsemne", "Kommune", ksb_test_samling())]
    grund_tabel2 <- grund_tabel2 %>%
                    select(sortering2, sortering1, everything())
                    #arrange([[1]])
                    #arrange(sortering1,sortering2)

    grund_tabel3 <- grund_tabel2 %>%
      arrange(.[,1])

    grund_tabel3[is.na(grund_tabel3)] <- 0

    x <- grund_tabel3

    # Hjælpevariable til opsætning af ark
    antal_raekker2 <- if (input$in_pivot == 1) {
       length(input$in_periode)
       } else {
         # length(input$in_mall_1) + length(input$in_mall_2)
         length(in_mall_1()) + length(in_mall_2())
       }

    startkolonne <- 3

    writeData(wb, "Som tabel", x, startCol = 1, startRow = 2, rowNames = FALSE) # indsætter data i fanen
    setColWidths(wb, sheet = "Som tabel", cols = 1:ncol(x), widths = "auto") # Justerer kolonnebredden

    # Laver kolonneoverskrifter og underoverskrifter
    kolonnenavne <- colnames(x) # tager kolonnenavnene fra datasætter
    kolonnenavne <- kolonnenavne[startkolonne:length(kolonnenavne)] # fjerner de første to
    kolonne_niveau_1 <- matrix(sub(".*[;]", "", kolonnenavne), nrow = 1) # fjerner tekst før punktum
    kolonne_niveau_2 <- matrix(sub("[;].*", "", kolonnenavne), nrow = 1) # fjerner tekst efter punktum

    # Skriver overskrifterne på de to første rækker
    writeData(wb, "Som tabel", kolonne_niveau_1, startCol = startkolonne, startRow = 2, colNames = FALSE)
    writeData(wb, "Som tabel", kolonne_niveau_2, startCol = startkolonne, startRow = 1, colNames = FALSE)

    # Merger celler i den første række
    for(i in seq(startkolonne,ncol(x),antal_raekker2)){ # loop fra startkolonne til sidste kolonne der stiger med antal værdier
      mergeCells(wb, sheet = "Som tabel", cols = i:(i+(antal_raekker2-1)), rows = 1)
    }

    # kildeangivelse
    kilde <- paste("Kilde: Ankestyrelsen (",format(Sys.Date(), "%Y"),")")
    writeData(wb, "Som tabel", kilde, startCol = 1, startRow = nrow(x) + 4) # der er tre rækker før data og en række mellemrum

    # definerer og tilføjer styling
    centerStyle <- createStyle(halign = "center")
    underline_style <- createStyle(
    border = "Bottom"
    )
    venstre_style <- createStyle(
      border = "Left"
    )

    styleT <- createStyle

    addStyle(wb, "Som tabel", centerStyle, rows = 1, cols = startkolonne:ncol(x))
    addStyle(wb, "Som tabel", centerStyle, rows = 2, cols = startkolonne:ncol(x))
    addStyle(wb, "Som tabel", underline_style, rows = 1, cols = startkolonne:ncol(x), stack = TRUE)
    #addStyle(wb, "Som tabel", venstre_style, rows = 1, cols = seq((startkolonne+antal_raekker2),ncol(x),antal_raekker2), stack = TRUE)

    saveWorkbook(wb, file, overwrite = TRUE)
                      }
  )
  }
)

# Download af excel-fil UDK ---------------------------------------------------

output$downloadData_xlsx22_udk <- downloadHandler(

  filename = "excelfil.xlsx",
  content = function(file) {
    Sys.sleep(0.3)
    removeModal()
    shiny::withProgress(message = 'Downloader excel-fil. Det kan tage et øjeblik.',
                        value = 0,   {

                          shiny::incProgress(1/10)
                          Sys.sleep(1)
                          shiny::incProgress(5/10)

                          wb <- createWorkbook()

                          # Danner fane
                          addWorksheet(wb, "Som tabel")


                          #sortering1 <- if (input$in_pivot2 == 1) "Kommune" else "Sagsemne"
                          #sortering2 <- if (input$in_pivot2 == 1) "Sagsemne" else "Kommune"

                          # Sorterer rækkefølgende af kolonnerne
                          #grund_tabel2 <- grund_tabel_exl_udk()[, order(names(grund_tabel_udk()))]
                          grund_tabel2 <- grund_tabel_exl_udk()[,c("Sagsemne", udk_test_samling())]
                          grund_tabel2 <- grund_tabel2 %>%
                            select(Sagsemne, everything())
                          #arrange([[1]])
                          #arrange(sortering1,sortering2)

                          grund_tabel3 <- grund_tabel2
                          # %>%
                            #arrange(.[,1])
                          #  arrange(.[,2])

                          grund_tabel3[is.na(grund_tabel3)] <- 0

                          x <- grund_tabel3

                          # Hjælpevariable til opsætning af ark
                          antal_raekker2 <- if (input$udk_in_pivot == 1) {
                            length(input$udk_in_periode)
                          } else {
                            # length(input$in_mall_1) + length(input$in_mall_2)
                            length(in_mall_1_udk()) + length(in_mall_2_udk())
                          }

                          startkolonne <- 2

                          writeData(wb, "Som tabel", x, startCol = 1, startRow = 2, rowNames = FALSE) # indsætter data i fanen
                          setColWidths(wb, sheet = "Som tabel", cols = 1:ncol(x), widths = "auto") # Justerer kolonnebredden

                          # Laver kolonneoverskrifter og underoverskrifter
                          kolonnenavne <- colnames(x) # tager kolonnenavnene fra datasætter
                          kolonnenavne <- kolonnenavne[startkolonne:length(kolonnenavne)] # fjerner de første to
                          kolonne_niveau_1 <- matrix(sub(".*[;]", "", kolonnenavne), nrow = 1) # fjerner tekst før punktum
                          kolonne_niveau_2 <- matrix(sub("[;].*", "", kolonnenavne), nrow = 1) # fjerner tekst efter punktum

                          # Skriver overskrifterne på de to første rækker
                          writeData(wb, "Som tabel", kolonne_niveau_1, startCol = startkolonne, startRow = 2, colNames = FALSE)
                          writeData(wb, "Som tabel", kolonne_niveau_2, startCol = startkolonne, startRow = 1, colNames = FALSE)

                          # Merger celler i den første række
                          for(i in seq(startkolonne,ncol(x),antal_raekker2)){ # loop fra startkolonne til sidste kolonne der stiger med antal værdier
                            mergeCells(wb, sheet = "Som tabel", cols = i:(i+(antal_raekker2-1)), rows = 1)
                          }

                          # kildeangivelse
                          kilde <- paste("Kilde: Ankestyrelsen (",format(Sys.Date(), "%Y"),")")
                          writeData(wb, "Som tabel", kilde, startCol = 1, startRow = nrow(x) + 4) # der er tre rækker før data og en række mellemrum

                          # definerer og tilføjer styling
                          centerStyle <- createStyle(halign = "center")
                          underline_style <- createStyle(
                            border = "Bottom"
                          )
                          venstre_style <- createStyle(
                            border = "Left"
                          )

                          styleT <- createStyle

                          addStyle(wb, "Som tabel", centerStyle, rows = 1, cols = startkolonne:ncol(x))
                          addStyle(wb, "Som tabel", centerStyle, rows = 2, cols = startkolonne:ncol(x))
                          addStyle(wb, "Som tabel", underline_style, rows = 1, cols = startkolonne:ncol(x), stack = TRUE)
                          #addStyle(wb, "Som tabel", venstre_style, rows = 1, cols = seq((startkolonne+antal_raekker2),ncol(x),antal_raekker2), stack = TRUE)

                          saveWorkbook(wb, file, overwrite = TRUE)
                        }
    )
  }
)


# Download af excel-fil - ASK ---------------------------------------------------

output$downloadData_xlsx22_ask <- downloadHandler(


  #grund_tabel2 <- grund_tabel_ask()[,c("Sagsemne", ask_test_samling())]


  filename = "excelfil.xlsx",
  content = function(file) {
    Sys.sleep(0.3)
    removeModal()
    shiny::withProgress(message = 'Downloader excel-fil. Det kan tage et øjeblik.',
                        value = 0,   {

                          shiny::incProgress(1/10)
                          Sys.sleep(1)
                          shiny::incProgress(5/10)

                          wb <- createWorkbook()

                          # Danner fane
                          addWorksheet(wb, "Som tabel")


                          #sortering1 <- "Sagsemne"
                          #sortering2 <- "Sagsemne"

                          # Sorterer rækkefølgende af kolonnerne
                          grund_tabel2 <- grund_tabel_exl_ask()[, c("Sagsemne", ask_test_samling())]
                          grund_tabel2 <- grund_tabel2 %>%
                            select(Sagsemne, everything())
                          #arrange([[1]])
                          #arrange(sortering1,sortering2)

                          grund_tabel3 <- grund_tabel2
                          #%>%
                          #  arrange(.[,1])

                          grund_tabel3[is.na(grund_tabel3)] <- 0

                          x <- grund_tabel3

                          # Hjælpevariable til opsætning af ark
                          antal_raekker2 <- if (input$ask_in_pivot == 1) {
                            length(input$ask_in_periode)
                          } else {
                            # length(input$in_mall_1) + length(input$in_mall_2)
                            length(in_mall_1_ask()) + length(in_mall_2_ask())
                          }

                          startkolonne <- 2

                          writeData(wb, "Som tabel", x, startCol = 1, startRow = 2, rowNames = FALSE) # indsætter data i fanen
                          setColWidths(wb, sheet = "Som tabel", cols = 1:ncol(x), widths = "auto") # Justerer kolonnebredden

                          # Laver kolonneoverskrifter og underoverskrifter
                          kolonnenavne <- colnames(x) # tager kolonnenavnene fra datasætter
                          kolonnenavne <- kolonnenavne[startkolonne:length(kolonnenavne)] # fjerner de første to
                          kolonne_niveau_1 <- matrix(sub(".*[;]", "", kolonnenavne), nrow = 1) # fjerner tekst før punktum
                          kolonne_niveau_2 <- matrix(sub("[;].*", "", kolonnenavne), nrow = 1) # fjerner tekst efter punktum

                          # Skriver overskrifterne på de to første rækker
                          writeData(wb, "Som tabel", kolonne_niveau_1, startCol = startkolonne, startRow = 2, colNames = FALSE)
                          writeData(wb, "Som tabel", kolonne_niveau_2, startCol = startkolonne, startRow = 1, colNames = FALSE)

                          # Merger celler i den første række
                          for(i in seq(startkolonne,ncol(x),antal_raekker2)){ # loop fra startkolonne til sidste kolonne der stiger med antal værdier
                            mergeCells(wb, sheet = "Som tabel", cols = i:(i+(antal_raekker2-1)), rows = 1)
                          }

                          # kildeangivelse
                          kilde <- paste("Kilde: Ankestyrelsen (",format(Sys.Date(), "%Y"),")")
                          writeData(wb, "Som tabel", kilde, startCol = 1, startRow = nrow(x) + 4) # der er tre rækker før data og en række mellemrum

                          # definerer og tilføjer styling
                          centerStyle <- createStyle(halign = "center")
                          underline_style <- createStyle(
                            border = "Bottom"
                          )
                          venstre_style <- createStyle(
                            border = "Left"
                          )

                          styleT <- createStyle

                          addStyle(wb, "Som tabel", centerStyle, rows = 1, cols = startkolonne:ncol(x))
                          addStyle(wb, "Som tabel", centerStyle, rows = 2, cols = startkolonne:ncol(x))
                          addStyle(wb, "Som tabel", underline_style, rows = 1, cols = startkolonne:ncol(x), stack = TRUE)
                          #addStyle(wb, "Som tabel", venstre_style, rows = 1, cols = seq((startkolonne+antal_raekker2),ncol(x),antal_raekker2), stack = TRUE)

                          saveWorkbook(wb, file, overwrite = TRUE)
                        }
    )
  }
)







# Download af billede - PNG -----------------------------------------------

  output$downloadData_2 <- downloadHandler(
    filename = "Tabel.png",
    content = function(file) {
      Sys.sleep(0.3)
      removeModal()
      shiny::withProgress(message = 'Downloader billede. Det kan tage et øjeblik.',
                          value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(5/10)
          gtsave(gt_tabl(), file, expand = 10, zoom = 2)
        }
      )

    }
  )


# Download af graf - PNG -----------------------------------------------

output$downloadData_2_alt <- downloadHandler(
  filename = "graf.png",
  content = function(file) {
    shiny::withProgress(message = 'Downloader billede. Det kan tage et øjeblik.',
                        value = 0,
                        {
                          shiny::incProgress(1/10)
                          Sys.sleep(1)
                          shiny::incProgress(5/10)
                          ggsave(file, plot = plot_omg_input(), device = "png", width = 20)
                        }
    )
  }
)

# Download af graf - PNG - ASK -----------------------------------------------

output$ask_downloadData_2_alt <- downloadHandler(
  filename = "graf.png",
  content = function(file) {
    shiny::withProgress(message = 'Downloader billede. Det kan tage et øjeblik.',
                        value = 0,
                        {
                          shiny::incProgress(1/10)
                          Sys.sleep(1)
                          shiny::incProgress(5/10)
                          ggsave(file, plot = ask_plot_omg_input(), device = "png", width = 20)
                        }
    )
  }
)
# Download af graf - PNG - UDK -----------------------------------------------

output$udk_downloadData_2_alt <- downloadHandler(
  filename = "graf.png",
  content = function(file) {
    shiny::withProgress(message = 'Downloader billede. Det kan tage et øjeblik.',
                        value = 0,
                        {
                          shiny::incProgress(1/10)
                          Sys.sleep(1)
                          shiny::incProgress(5/10)
                          ggsave(file, plot = udk_plot_omg_input(), device = "png", width = 20)
                        }
    )
  }
)



# Graf - sagsudfald - KSB --------------------------------------------------------

output$plot_omg <- renderPlot({
  req(input$in_kommune, input$in_lovgivning, length(input$in_periode) > 1, len_valg_graf() < 2000, length(input$in_lovgivning) < 10, length(input$in_kommune) < 10)
  print(plot_omg_input())
})

plot_omg_input <- reactive({
  #req(input$in_kommune, input$in_lovgivning, length(input$in_periode) > 1, len_valg_graf() < 2000, length(input$in_lovgivning) < 10, length(input$in_kommune) < 10)
  valg_til_graf <- as.character(input$`input til graf`)
  #valgte_kommuner <- Kommune
  maks_vaerdi <- as.double(max(cd3()$data_kolonne))


  # if(input$Pivot_graf_kommune_el_lovgivning == 1) Kommune else Sagsemne
  # if(input$Pivot_graf_kommune_el_lovgivning == 1) Sagsemne else Kommune

  #browser()
  #req(length(input$in_periode) > 1, input$in_lovgivning, input$in_kommune)
  #req(len_valg() > 1, len_valg() < 200)
  ggplot(data=cd3(), aes(x=as.factor(periode), y=data_kolonne, group = if(input$Pivot_graf_kommune_el_lovgivning == 1) Kommune else Sagsemne)) +
  #ggplot(data=cd3(), aes_string(x=as.factor("Aar"), y=valg_til_graf, group = "Kommune")) +
  geom_line(aes(color=if(input$Pivot_graf_kommune_el_lovgivning == 1) Kommune else Sagsemne), size = 1, na.rm = TRUE) +
  geom_point(aes(color=if(input$Pivot_graf_kommune_el_lovgivning == 1) Kommune else Sagsemne), size = 3, na.rm = TRUE) +
  #scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  ylim(0, case_when(
            valg_til_graf == 'Omgørelsesprocent' ~ 100,
            valg_til_graf == 'Stadfæstelsesprocent' ~ 100,
            valg_til_graf == 'Hjemvisningsprocent' ~ 100,
            valg_til_graf == 'Ændrings-/ophævelsesprocent' ~ 100,
            #TRUE ~ maks_vaerdi*1.1)) +
            TRUE ~ maks_vaerdi)) +
  theme_minimal() +
  labs(title = case_when(
         valg_til_graf == 'Omgørelsesprocent' ~ 'Andel omgjorte sager',
         valg_til_graf == 'Stadfæstelsesprocent' ~ 'Andel stadfæstede sager',
         valg_til_graf == 'Hjemvisningsprocent' ~ 'Andel hjemviste sager',
         valg_til_graf == 'Ændrings-/ophævelsesprocent' ~ 'Andel ændrede/ophævede sager',
         valg_til_graf == 'Realitetsbehandlede sager i alt' ~ 'Antal realitetsbehandlede sager i alt',
         valg_til_graf == 'Stadfæstede sager' ~ 'Antal stadfæstede sager',
         valg_til_graf == 'Ændrede/ophævede sager' ~ 'Antal ændrede/ophævede sager',
         valg_til_graf == 'Hjemviste sager' ~ 'Antal hjemviste sager',
         valg_til_graf == 'Afviste sager' ~ 'Antal afviste sager',
         TRUE ~ 'Øvrigt'),
       x = case_when(
            input$aar_el_kvartal == 1 ~ "År",
            input$aar_el_kvartal == 2 ~ "Kvartal",
            TRUE ~ "Periode"),
       y = valg_til_graf,
       color = if(input$Pivot_graf_kommune_el_lovgivning == 1) "Kommune" else "Sagsemne") +
  #theme(legend.position = if(length(input$in_kommune) > 5) "none" else "right") +
  #theme(legend.position = if(input$in_pivot_3 == 2) "none" else "right",
  theme(legend.position = "right",
        strip.text.x = element_text(size = 12, face = "bold"),
        text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  # facet_wrap(if(length(input$in_kommune) > 5) {
  #               Kommune~Lovgivning
  #               } else ~Lovgivning,
  #            nrow = 2
  # facet_wrap(if(input$in_pivot_3 == 2) {
  #   Kommune~Sagsemne
  #   } else ~Sagsemne,
  #   nrow = 2
  #
  # )
  # facet_wrap(if(input$in_pivot_3 == 2) {
  #   if(input$Pivot_graf_kommune_el_lovgivning == 1) Kommune else Sagsemne~if(input$Pivot_graf_kommune_el_lovgivning == 1) Sagsemne else Kommune
  # } else ~if(input$Pivot_graf_kommune_el_lovgivning == 1) Sagsemne else Kommune,
  # nrow = 2
  #
  # )
  facet_wrap( ~ if(input$Pivot_graf_kommune_el_lovgivning == 1) Sagsemne else Kommune)


  }

  #, height = 500
     #height =exprToFunction(input$plotY)
)


# set_shiny_plot_height_with_respects_to_width <- function(session, plot_omg){
#   width <- function() {
#     session$clientData[[plot_omg]]
#   }
#   # Do something with the width
#   width / 2
# }

# output$bredde_af_graf <- renderText({
#   session$clientData$output_plot_omg_width
# })


mangler_der_tal_i_graf <- reactive({
    any(is.na(cd3()$data_kolonne))
})


output$advarsel <- renderUI({
req(mangler_der_tal_i_graf())
#if(mangler_der_tal_i_graf()) {
box(id = "boks_der_skjules",
      title= "OBS",
      width = NULL,
      status = "warning",
      solidHeader = TRUE,
      p("Du har valgt et nøgletal, hvor der inden for nogle af de valgte kommuner og lovområder ikke er behandlet sager. Grafen kan derfor være tom, eller der kan mangle punkter på grafen."))

#} else NULL
}
)

mangler_der_tal_i_graf_ask <- reactive({
  any(is.na(cd3_ask()$data_kolonne))
})


output$advarsel_ask <- renderUI({
  req(mangler_der_tal_i_graf_ask())
  #if(mangler_der_tal_i_graf()) {
  box(id = "boks_der_skjules_ask",
      title= "OBS",
      width = NULL,
      status = "warning",
      solidHeader = TRUE,
      p("Du har valgt et nøgletal, hvor der inden for nogle af de valgte kommuner og lovområder ikke er behandlet sager. Grafen kan derfor være tom, eller der kan mangle punkter på grafen."))

  #} else NULL
}
)

output$Vaelg_mindst_to_perioder <- renderUI({
  #req(length(input$in_periode) == 1)
  if(length(input$in_periode) == 1) {
  html("<strong>OBS: Vælg mindst to år eller kvartaler i feltet periode.</strong>")
  } else if (length(input$in_kommune) > 9) {
  html("<strong>OBS: Vælg maksimalt ni kommuner.</strong>")
  } else if (length(input$in_lovgivning) > 9) {
  html("<strong>OBS: Vælg maksimalt ni valg under lovgivning.</strong>")
  } else NULL
}
)

bredden_af_graf_var <- reactive({
  #session$clientData$output_plot_omg_width
  session$clientData[['output_plot_omg_width']]
})

output$sized_plot <- renderUI({
  plotOutput("plot_omg", height = if(input$Pivot_graf_kommune_el_lovgivning == 1 && length(input$in_lovgivning) > 3) 800 else 400)
  #plotOutput("plot_omg", height = laengden_af_grafen())
  #plotOutput("plot_omg", height = set_shiny_plot_height_with_respects_to_width())
})


# Graf - sagsudfald - UDK --------------------------------------------------------

output$udk_plot_omg <- renderPlot({
  req(input$udk_in_lovgivning, length(input$udk_in_periode) > 1, len_valg_graf_udk() < 2000, length(input$udk_in_lovgivning) < 10)
  print(udk_plot_omg_input())
})

udk_plot_omg_input <- reactive({
  #req(input$in_kommune, input$in_lovgivning, length(input$in_periode) > 1, len_valg_graf() < 2000, length(input$in_lovgivning) < 10, length(input$in_kommune) < 10)
  valg_til_graf_udk <- as.character(input$`udk_input til graf`)
  #valgte_kommuner <- Kommune
  maks_vaerdi <- as.double(max(cd3_udk()$data_kolonne))


  # if(input$Pivot_graf_kommune_el_lovgivning == 1) Kommune else Sagsemne
  # if(input$Pivot_graf_kommune_el_lovgivning == 1) Sagsemne else Kommune

  #browser()
  #req(length(input$in_periode) > 1, input$in_lovgivning, input$in_kommune)
  #req(len_valg() > 1, len_valg() < 200)
  ggplot(data=cd3_udk(), aes(x=as.factor(periode), y=data_kolonne, group = Sagsemne)) +
    #ggplot(data=cd3(), aes_string(x=as.factor("Aar"), y=valg_til_graf, group = "Kommune")) +
    geom_line(aes(color= Sagsemne), size = 1, na.rm = TRUE) +
    geom_point(aes(color= Sagsemne), size = 3, na.rm = TRUE) +
    #scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
    ylim(0, case_when(
      valg_til_graf_udk == 'Omgørelsesprocent' ~ 100,
      valg_til_graf_udk == 'Stadfæstelsesprocent' ~ 100,
      valg_til_graf_udk == 'Hjemvisningsprocent' ~ 100,
      valg_til_graf_udk == 'Ændrings-/ophævelsesprocent' ~ 100,
      #TRUE ~ maks_vaerdi*1.1)) +
      TRUE ~ maks_vaerdi)) +
    theme_minimal() +
    labs(title = case_when(
      valg_til_graf_udk == 'Omgørelsesprocent' ~ 'Andel omgjorte sager',
      valg_til_graf_udk == 'Stadfæstelsesprocent' ~ 'Andel stadfæstede sager',
      valg_til_graf_udk == 'Hjemvisningsprocent' ~ 'Andel hjemviste sager',
      valg_til_graf_udk == 'Ændrings-/ophævelsesprocent' ~ 'Andel ændrede/ophævede sager',
      valg_til_graf_udk == 'Realitetsbehandlede sager i alt' ~ 'Antal realitetsbehandlede sager i alt',
      valg_til_graf_udk == 'Stadfæstede sager' ~ 'Antal stadfæstede sager',
      valg_til_graf_udk == 'Ændrede/ophævede sager' ~ 'Antal ændrede/ophævede sager',
      valg_til_graf_udk == 'Hjemviste sager' ~ 'Antal hjemviste sager',
      valg_til_graf_udk == 'Afviste sager' ~ 'Antal afviste sager',
      valg_til_graf_udk == 'Sagsbehandlingstid' ~ 'Sagsbehandlingstid (antal uger)',
      TRUE ~ 'Øvrigt'),
      x = case_when(
        input$udk_aar_el_kvartal == 1 ~ "År",
        input$udk_aar_el_kvartal == 2 ~ "Kvartal",
        TRUE ~ "Periode"),
      y = valg_til_graf_udk,
      color = "Sagsemne") +
    #theme(legend.position = if(length(input$in_kommune) > 5) "none" else "right") +
    #theme(legend.position = if(input$in_pivot_3 == 2) "none" else "right",
    theme(legend.position = "right",
          strip.text.x = element_text(size = 12, face = "bold"),
          text = element_text(size = 16),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    # facet_wrap(if(length(input$in_kommune) > 5) {
    #               Kommune~Lovgivning
    #               } else ~Lovgivning,
    #            nrow = 2
    # facet_wrap(if(input$in_pivot_3 == 2) {
    #   Kommune~Sagsemne
    #   } else ~Sagsemne,
    #   nrow = 2
    #
    # )
    # facet_wrap(if(input$in_pivot_3 == 2) {
  #   if(input$Pivot_graf_kommune_el_lovgivning == 1) Kommune else Sagsemne~if(input$Pivot_graf_kommune_el_lovgivning == 1) Sagsemne else Kommune
  # } else ~if(input$Pivot_graf_kommune_el_lovgivning == 1) Sagsemne else Kommune,
  # nrow = 2
  #
  # )
  #facet_wrap( ~ Sagsemne)


}

#, height = 500
#height =exprToFunction(input$plotY)
)


# set_shiny_plot_height_with_respects_to_width <- function(session, plot_omg){
#   width <- function() {
#     session$clientData[[plot_omg]]
#   }
#   # Do something with the width
#   width / 2
# }

# output$bredde_af_graf <- renderText({
#   session$clientData$output_plot_omg_width
# })

# Hvor står nedenstående to to gange i koden?

# mangler_der_tal_i_graf <- reactive({
#   any(is.na(cd3()$data_kolonne))
# })
#
#
# output$advarsel <- renderUI({
#   req(mangler_der_tal_i_graf())
#   #if(mangler_der_tal_i_graf()) {
#   box(id = "boks_der_skjules",
#       title= "OBS",
#       width = NULL,
#       status = "warning",
#       solidHeader = TRUE,
#       p("Du har valgt et nøgletal, hvor der inden for nogle af de valgte kommuner og lovområder ikke er behandlet sager. Grafen kan derfor være tom, eller der kan mangle punkter på grafen."))
#
#   #} else NULL
# }
# )


output$udk_Vaelg_mindst_to_perioder <- renderUI({
  #req(length(input$in_periode) == 1)
  if(length(input$udk_in_periode) == 1) {
    html("<strong>OBS: Vælg mindst to år eller kvartaler i feltet periode.</strong>")
    # } else if (length(input$in_kommune) > 9) {
    #   html("<strong>OBS: Vælg maksimalt ni kommuner.</strong>")
  } else if (length(input$udk_in_lovgivning) > 9) {
    html("<strong>OBS: Vælg maksimalt ni valg under lovgivning.</strong>")
  } else NULL
}
)




# Graf - sagsudfald - ASK --------------------------------------------------------

output$ask_plot_omg <- renderPlot({
  req(input$ask_in_lovgivning, length(input$ask_in_periode) > 1, len_valg_graf_ask() < 2000, length(input$ask_in_lovgivning) < 10)
  print(ask_plot_omg_input())
})

ask_plot_omg_input <- reactive({
  #req(input$in_kommune, input$in_lovgivning, length(input$in_periode) > 1, len_valg_graf() < 2000, length(input$in_lovgivning) < 10, length(input$in_kommune) < 10)
  valg_til_graf_ask <- as.character(input$`ask_input til graf`)
  #valgte_kommuner <- Kommune
  maks_vaerdi <- as.double(max(cd3_ask()$data_kolonne))


  # if(input$Pivot_graf_kommune_el_lovgivning == 1) Kommune else Sagsemne
  # if(input$Pivot_graf_kommune_el_lovgivning == 1) Sagsemne else Kommune

  #browser()
  #req(length(input$in_periode) > 1, input$in_lovgivning, input$in_kommune)
  #req(len_valg() > 1, len_valg() < 200)
  ggplot(data=cd3_ask(), aes(x=as.factor(periode), y=data_kolonne, group = Sagsemne)) +
    #ggplot(data=cd3(), aes_string(x=as.factor("Aar"), y=valg_til_graf, group = "Kommune")) +
    geom_line(aes(color= Sagsemne), size = 1, na.rm = TRUE) +
    geom_point(aes(color= Sagsemne), size = 3, na.rm = TRUE) +
    #scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
    ylim(0, case_when(
      valg_til_graf_ask == 'Omgørelsesprocent' ~ 100,
      valg_til_graf_ask == 'Stadfæstelsesprocent' ~ 100,
      valg_til_graf_ask == 'Hjemvisningsprocent' ~ 100,
      valg_til_graf_ask == 'Ændringsprocent' ~ 100,
      valg_til_graf_ask == 'Ophævelsesprocent' ~ 100,
      #TRUE ~ maks_vaerdi*1.1)) +
      TRUE ~ maks_vaerdi)) +
    theme_minimal() +
    labs(title = case_when(
      valg_til_graf_ask == 'Omgørelsesprocent' ~ 'Andel omgjorte sager',
      valg_til_graf_ask == 'Stadfæstelsesprocent' ~ 'Andel stadfæstede sager',
      valg_til_graf_ask == 'Hjemvisningsprocent' ~ 'Andel hjemviste sager',
      valg_til_graf_ask == 'Ændringsprocent' ~ 'Andel ændrede sager',
      valg_til_graf_ask == 'Ophævelsesprocent' ~ 'Andel ophævede sager',
      valg_til_graf_ask == 'Realitetsbehandlede sager i alt' ~ 'Antal realitetsbehandlede sager i alt',
      valg_til_graf_ask == 'Stadfæstede sager' ~ 'Antal stadfæstede sager',
      valg_til_graf_ask == 'Ændrede sager' ~ 'Antal ændrede sager',
      valg_til_graf_ask == 'Ophævede sager' ~ 'Antal ophævede sager',
      valg_til_graf_ask == 'Hjemviste sager' ~ 'Antal hjemviste sager',
      valg_til_graf_ask == 'Afviste sager' ~ 'Antal afviste sager',
      valg_til_graf_ask == 'Sagsbehandlingstid' ~ 'Sagsbehandlingstid (uger)',
      TRUE ~ 'Øvrigt'),
      x = case_when(
        input$ask_aar_el_kvartal == 1 ~ "År",
        input$ask_aar_el_kvartal == 2 ~ "Kvartal",
        TRUE ~ "Periode"),
      y = valg_til_graf_ask,
      color = "Sagsemne") +
    #theme(legend.position = if(length(input$in_kommune) > 5) "none" else "right") +
    #theme(legend.position = if(input$in_pivot_3 == 2) "none" else "right",
    theme(legend.position = "right",
          strip.text.x = element_text(size = 12, face = "bold"),
          text = element_text(size = 16),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  #+
    # facet_wrap(if(length(input$in_kommune) > 5) {
    #               Kommune~Lovgivning
    #               } else ~Lovgivning,
    #            nrow = 2
    # facet_wrap(if(input$in_pivot_3 == 2) {
    #   Kommune~Sagsemne
    #   } else ~Sagsemne,
    #   nrow = 2
    #
    # )
    # facet_wrap(if(input$in_pivot_3 == 2) {
  #   if(input$Pivot_graf_kommune_el_lovgivning == 1) Kommune else Sagsemne~if(input$Pivot_graf_kommune_el_lovgivning == 1) Sagsemne else Kommune
  # } else ~if(input$Pivot_graf_kommune_el_lovgivning == 1) Sagsemne else Kommune,
  # nrow = 2
  #
  # )
  #facet_wrap( ~ Sagsemne)


}

#, height = 500
#height =exprToFunction(input$plotY)
)


# set_shiny_plot_height_with_respects_to_width <- function(session, plot_omg){
#   width <- function() {
#     session$clientData[[plot_omg]]
#   }
#   # Do something with the width
#   width / 2
# }

# output$bredde_af_graf <- renderText({
#   session$clientData$output_plot_omg_width
# })

# Hvor står nedenstående to to gange i koden?

# mangler_der_tal_i_graf <- reactive({
#   any(is.na(cd3()$data_kolonne))
# })
#
#
# output$advarsel <- renderUI({
#   req(mangler_der_tal_i_graf())
#   #if(mangler_der_tal_i_graf()) {
#   box(id = "boks_der_skjules",
#       title= "OBS",
#       width = NULL,
#       status = "warning",
#       solidHeader = TRUE,
#       p("Du har valgt et nøgletal, hvor der inden for nogle af de valgte kommuner og lovområder ikke er behandlet sager. Grafen kan derfor være tom, eller der kan mangle punkter på grafen."))
#
#   #} else NULL
# }
# )


output$ask_Vaelg_mindst_to_perioder <- renderUI({
  #req(length(input$in_periode) == 1)
  if(length(input$ask_in_periode) == 1) {
    html("<strong>OBS: Vælg mindst to år eller kvartaler i feltet periode.</strong>")
  # } else if (length(input$in_kommune) > 9) {
  #   html("<strong>OBS: Vælg maksimalt ni kommuner.</strong>")
  } else if (length(input$ask_in_lovgivning) > 9) {
    html("<strong>OBS: Vælg maksimalt ni valg under lovgivning.</strong>")
  } else NULL
}
)

# bredden_af_graf_var <- reactive({
#   #session$clientData$output_plot_omg_width
#   session$clientData[['output_plot_omg_width']]
# })
#
# output$sized_plot <- renderUI({
#   plotOutput("plot_omg", height = if(input$Pivot_graf_kommune_el_lovgivning == 1 && length(input$in_lovgivning) > 3) 800 else 400)
#   #plotOutput("plot_omg", height = laengden_af_grafen())
#   #plotOutput("plot_omg", height = set_shiny_plot_height_with_respects_to_width())
# })











# Modal -------------------------------------------------------------------

# Om afgørelsestyper

observeEvent(input$show, {
  showModal(modalDialog(
    title = html("<strong>Ankestyrelsens afgørelsestyper</strong>"),
    html("
    <strong>Realitetsbehandlede sager</strong> er hjemviste, ændrede/ophævede og stadfæstede sager. Her tæller afviste/henviste sager altså ikke med.
    <br>
    <br>
    <strong>Stadfæstelse</strong> betyder, at Ankestyrelsen er enig i kommunens afgørelse.
    <br>
    <br>
    <strong>Ændring/ophævelse</strong> betyder, at afgørelsen er forkert, og at Ankestyrelsen har ophævet kommunens afgørelse.
    <br>
    <br>
    <strong>Hjemvisning</strong> betyder, at Ankestyrelsen sender sagen tilbage til kommunen, som er førsteinstans. Kommunen skal behandle sagen og afgøre den igen. Grunden til, at vi hjemviser en sag, kan for eksempel være, at der mangler oplysninger, eller at der er sket alvorlige sagsbehandlingsfejl.
    <br>
    <br>
    <strong>Afvisning/henvisning</strong>  betyder, at Ankestyrelsen ikke behandler sagen. Det kan for eksempel være, fordi borgeren har klaget for sent eller beslutter at opgive klagen. Eller fordi Ankestyrelsen ikke er den kompetente myndighed, og derfor sender klagen videre til en anden instans.
    "),

    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})

observeEvent(input$udk_show, {
  showModal(modalDialog(
    title = html("<strong>Ankestyrelsens afgørelsestyper</strong>"),
    html("
    <strong>Realitetsbehandlede sager</strong> er stadfæstede, ændrede/ophævede og hjemviste sager. Her tæller afviste/henviste sager altså ikke med.
    <br>
    <br>
    <strong>Stadfæstelse</strong> betyder, at Ankestyrelsen er enig i Udbetaling Danmarks afgørelse.
    <br>
    <br>
    <strong>Ændring/ophævelse</strong> betyder, at afgørelsen er forkert, og at Ankestyrelsen har ophævet Udbetaling Danmarks afgørelse.
    <br>
    <br>
    <strong>Hjemvisning</strong> betyder, at Ankestyrelsen sender sagen tilbage til Udbetaling Danmark. Udbetaling Danmark skal behandle sagen og afgøre den igen. Grunden til, at vi hjemviser en sag, kan for eksempel være, at der mangler oplysninger, eller at der er sket alvorlige sagsbehandlingsfejl.
    <br>
    <br>
    <strong>Afvisning/henvisning</strong>  betyder, at Ankestyrelsen ikke behandler sagen. Det kan for eksempel være, fordi borgeren har klaget for sent eller beslutter at opgive klagen. Eller fordi Ankestyrelsen ikke er den kompetente myndighed, og derfor sender klagen videre til en anden instans.
    "),

    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})



observeEvent(input$ask_show, {
  showModal(modalDialog(
    title = html("<strong>Ankestyrelsens afgørelsestyper</strong>"),
    h3("Sagsudfald"),
    html("
    <strong>Realitetsbehandlede sager</strong> er stadfæstede, ændrede, hjemviste og ophævede sager. Her tæller afviste/henviste sager altså ikke med.
    <br>
    <br>
    <strong>Stadfæstelse</strong> betyder, at Ankestyrelsen kommer frem til samme resultat som AES.
    <br>
    <br>
    <strong>Ændring</strong> betyder, at Ankestyrelsen sætter sin egen afgørelse med et andet resultat i stedet for AES' afgørelse.
    <br>
    <br>
    <strong>Hjemvisning</strong> betyder, at Ankestyrelsen sender sagen tilbage til AES som førsteinstans. AES skal behandle sagen og afgøre den igen. Årsagen til, at vi hjemviser en sag, kan for eksempel være, at sagen ikke er tilstrækkeligt oplyst.
    <br>
    <br>
    <strong>Ophævelse</strong> betyder, at AES’ afgørelse ikke længere gælder, typisk som en retsvirkning af en anden afgørelse. Et eksempel er, hvis Ankestyrelsen ændrer en afgørelse om anerkendelse til afslag på anerkendelse. Her vil Ankestyrelsen ophæve afgørelserne om ydelser efter loven, hvis der også er påklaget spørgsmål herom.
    <br>
    <br>
    <strong>Afvisning/henvisning</strong>  betyder, , at Ankestyrelsen ikke behandler sagen. Det kan for eksempel være, fordi parten har klaget for sent eller trækker klagen tilbage, eller fordi Ankestyrelsen ikke er den kompetente myndighed.
    "),
    h3("Om antallet af sager"),
    html("
    Alle delafgørelser, som Ankestyrelsen har efterprøvet, indgår i opgørelsen. Hvis Ankestyrelsen for eksempel i samme sag efterprøver AES’ afgørelser om anerkendelse og varigt mén, er der tale om to afgørelser, som hver især indgår i opgørelsen.
    <br>
    <br>
    Udgangspunktet er, at Ankestyrelsen kun efterprøver de (del)afgørelser, der er klaget over. Ankestyrelsen kan dog udvide prøvelsen til andet end det, der er klaget over.
    "),

    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})


observeEvent(input$nogletal, {
  showModal(modalDialog(
    title = html("<strong>Ankestyrelsens afgørelsestyper</strong>"),
    html("<strong>Stadfæstelsesprocenten</strong> er antallet af stadfæstede sager set i forhold til alle realitetsbehandlede sager.
    <br>
    <br>
    <strong>Omgørelsesprocenten</strong> er antallet af hjemviste og ændrede/ophævede sager set i forhold til alle realitetsbehandlede sager.
    <br>
    <br>
    <strong>Ændrings-/ophævelsesprocenten</strong> er antallet af ændrede/ophævede sager set i forhold til alle realitetsbehandlede sager.
    <br>
    <br>
    <strong>Hjemvisningsprocenten</strong> er antallet af hjemviste sager set i forhold til alle realitetsbehandlede sager.
    "),

    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})

observeEvent(input$udk_nogletal, {
  showModal(modalDialog(
    title = html("<strong>Ankestyrelsens afgørelsestyper</strong>"),
    html("<strong>Stadfæstelsesprocenten</strong> er antallet af stadfæstede sager set i forhold til alle realitetsbehandlede sager.
    <br>
    <br>
    <strong>Omgørelsesprocenten</strong> er antallet af hjemviste og ændrede/ophævede sager set i forhold til alle realitetsbehandlede sager.
    <br>
    <br>
    <strong>Ændrings-/ophævelsesprocenten</strong> er antallet af ændrede/ophævede sager set i forhold til alle realitetsbehandlede sager.
    <br>
    <br>
    <strong>Hjemvisningsprocenten</strong> er antallet af hjemviste sager set i forhold til alle realitetsbehandlede sager.
    "),

    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})

observeEvent(input$ask_nogletal, {
  showModal(modalDialog(
    title = html("<strong>Nøgletal</strong>"),
    html("<strong>Stadfæstelsesprocenten</strong> er antallet af stadfæstede sager set i forhold til alle realitetsbehandlede sager.
    <br>
    <br>
    <strong>Omgørelsesprocenten</strong> er antallet af hjemviste, ændrede og ophævede sager set i forhold til alle realitetsbehandlede sager.
    <br>
    <br>
    <strong>Ændringsprocenten</strong> er antallet af ændrede sager set i forhold til alle realitetsbehandlede sager.
    <br>
    <br>
    <strong>Hjemvisningsprocenten</strong> er antallet af hjemviste sager set i forhold til alle realitetsbehandlede sager.
    <br>
    <br>
    <strong>Ophævelsesprocenten</strong> er antallet af ophævede sager set i forhold til alle realitetsbehandlede sager.
    "),

    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})

observeEvent(input$udk_sagsbehandlingstider, {
  showModal(modalDialog(
    title = html("<strong>Om sagsbehandlingstider</strong>"),
    html("Sagsbehandlingstiden opgøres som perioden mellem, at Ankestyrelsen har modtaget klagen til at sagen er afgjort."),

    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})

observeEvent(input$ask_sagsbehandlingstider, {
  showModal(modalDialog(
    title = html("<strong>Om sagsbehandlingstider</strong>"),
    html("Sagsbehandlingstiden opgøres som perioden mellem, at Ankestyrelsen har modtaget klagen til at sagen er afgjort."),

    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})


# Om ankeportalen

observeEvent(input$om_portalen, {
  showModal(modalDialog(
    title = html("<strong>Om tal og statistik fra Ankestyrelsen</strong>"),
    html("Denne dataportal er udarbejdet og vedligeholdes af Ankestyrelsen. Portalen opdateres løbende med nye data.
    <br>
    <br>
    Har du spørgsmål til portalen eller data er du velkommen til at kontakte statistik@ast.dk.
    "),

    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})

# Om danmarkskortene

observeEvent(input$dk_kort, {
  showModal(modalDialog(
    title = html("<strong>Om Social- og Ældreministeriets danmarkskort</strong>"),
    html("Social- og Ældreministeriet publicerer hvert år tre danmarkskort med omgørelsesprocenter. Ankestyrelsen leverer data om kommunernes omgørelser til udarbejdelsen af kortene. Kortene kan findes her:"), a("sm.dk/danmarkskort", href="https://sm.dk/danmarkskort/", target="_blank"), html("
    <br>
    <br>
    <strong>Kortet for socialområdet generelt</strong>
    <br>
    Her indgår alle bestemmelser i serviceloven, hvor der er kommunal klageadgang.
    Dvs. både børneområdet og voksenområdet. Kortet for socialområdet generelt indeholder således de bestemmelser, der indgår i de specifikke danmarkskort for hhv.
    voksenhandicapområdet og børnehandicapområdet, men derudover indgår en række
    andre bestemmelser på tværs af børne- og voksenområdet.
    <br>
    <br>
    "),
    actionButton("fsfsdf", "Vælg paragraffer"),
    html("
    <br>
    <br>
    <strong>Kortet specifikt for børnehandicapområdet</strong>
    <br>
    I dette kort indgår følgende bestemmelser:
    <ul>
      <li>Pasningstilbud, hjemmetræning m.m. §§ 32, 32a, 36, 39-40</li>
      <li>Merudgiftsydelse § 41</li>
      <li>Tabt arbejdsfortjeneste §§ 42- 43</li>
      <li>Personlig hjælp og ledsagelse §§ 44 - 45</li>
    </ul>
    "),
    actionButton("fsfsdf2", "Vælg paragraffer"),
    html(
    "
    <br>
    <br>
    <strong>Kortet specifikt for særlige bestemmelser på voksenhandicapområdet</strong>
    <br>
    I dette kort indgår følgende bestemmelser:
    <ul>
      <li>Voksne - kontante tilskud § 95</li>
      <li>Voksne - borgerstyret personlig assistance § 96</li>
      <li>Voksne - ledsageordning § 97</li>
      <li>Voksne - merudgifter § 100 </li>
    </ul>
    "),
    actionButton("fsfsdf3", "Vælg paragraffer")
    ,

    easyClose = TRUE,
    footer = tagList(
      modalButton("Skjul (ESC)")
    )
  ))
})


observeEvent(input$test_download_modal, {
  showModal(modalDialog(
    title = html("<strong>Eksport af tabel/data</strong>"),
    html("Download excel-fil:
    <br>
    "),
    downloadButton("downloadData_xlsx22", "XLSX (Excel)"),
    downloadButton("downloadData_csv", "CSV-fil"),
    html("<br>
          <br>
         <strong>OBS: det kan tage et øjeblik, før filen er hentet.</strong>"),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Annuller (ESC)")
    )
  ))
})

observeEvent(input$test_download_modal_ask, {
  showModal(modalDialog(
    title = html("<strong>Eksport af tabel/data</strong>"),
    html("Download excel-fil:
    <br>
    "),
    downloadButton("downloadData_xlsx22_ask", "XLSX (Excel)"),
    downloadButton("downloadData_csv_ask", "CSV-fil"),
    html("<br>
          <br>
         <strong>OBS: det kan tage et øjeblik, før filen er hentet.</strong>"),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Annuller (ESC)")
    )
  ))
})

observeEvent(input$test_download_modal_udk, {
  showModal(modalDialog(
    title = html("<strong>Eksport af tabel/data</strong>"),
    html("Download excel-fil:
    <br>
    "),
    downloadButton("downloadData_xlsx22_udk", "XLSX (Excel)"),
    downloadButton("downloadData_csv_udk", "CSV-fil"),
    html("<br>
          <br>
         <strong>OBS: det kan tage et øjeblik, før filen er hentet.</strong>"),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Annuller (ESC)")
    )
  ))
})

observeEvent(input$test_download_modal2, {
  showModal(modalDialog(
    title = html("<strong>Eksport af data</strong>"),
    html("Download CSV-fil:
    <br>
    "),
    downloadButton("downloadData_csv", "CSV-fil"),
    html("<br>
          <br>
         <strong>OBS: det kan tage et øjeblik, før filen er hentet.</strong>"),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Annuller (ESC)")
    )
  ))
})

servicelov_alle <- reactive ({
  Sagsemner %>%
    filter(Lovgivning == 'Serviceloven') %>%
    pull(Sagsemne)
})

observeEvent(input$fsfsdf, {

  updatePickerInput(session, "in_lovgivning",
                     selected = servicelov_alle())

  })

observeEvent(input$fsfsdf2, {
  updatePickerInput(session, "in_lovgivning",
                    selected = list('Pasningstilbud, hjemmetræning m.m. §§32, 32a, 36, 39-40 + tilbagebet.',
                                    'Merudgiftydelse §41 + tilbagebetaling',
                                    'Tabt arbejdsfortjeneste §§ 42,43 + tilbagebetaling',
                                    'Personlig hjælp og ledsagelse §§44, 45 + tilbagebetaling'
                                    ))
})

observeEvent(input$fsfsdf3, {
  updatePickerInput(session, "in_lovgivning",
                    selected = list('Voksne - kontante tilskud § 95',
                                    'Voksne - borgerstyret personlig assistance - § 96',
                                    'Voksne - ledsageordning - § 97',
                                    'Voksne - merudgifter - § 100'))
})


observeEvent(input$fsfsdf, {
  removeModal()
})

observeEvent(input$fsfsdf2, {
  removeModal()
})

observeEvent(input$fsfsdf3, {
  removeModal()
})

# observeEvent(input$downloadData_2, {
#   removeModal()
# })



# Nulstil filtre ----------------------------------------------------------

observeEvent(input$nulstil_filtre, {
  updateMaterialSwitch(session, "in_landstotal",
                       value = FALSE)
  updatePickerInput(session, "in_periode",
                    selected = FALSE)
  updatePickerInput(session, "in_lovgivning",
                    selected = FALSE)
  updatePickerInput(session, "in_kommune",
                    selected = FALSE)
  updateRadioButtons(session, "aar_el_kvartal",
                  selected = 1)
  updateCheckboxGroupInput(session, "antal_sager2",
                           selected = FALSE)
  updateCheckboxGroupInput(session, "antal_sager3",
                           selected = FALSE)
  updateCheckboxGroupInput(session, "nogletal1",
                           selected = FALSE)
  updateCheckboxGroupInput(session, "nogletal2",
                           selected = FALSE)
  updateCheckboxGroupInput(session, "sagsbehandlingstider1",
                           selected = FALSE)
  updateRadioButtons(session, "lov_el_paragraf",
                     selected = 1)

})

observeEvent(input$udk_nulstil_filtre, {
  updatePickerInput(session, "udk_in_periode",
                    selected = FALSE)
  updatePickerInput(session, "udk_in_lovgivning",
                    selected = FALSE)
  updateRadioButtons(session, "udk_aar_el_kvartal",
                     selected = 1)
  updateCheckboxGroupInput(session, "udk_antal_sager2",
                           selected = FALSE)
  updateCheckboxGroupInput(session, "udk_antal_sager3",
                           selected = FALSE)
  updateCheckboxGroupInput(session, "udk_nogletal1",
                           selected = FALSE)
  updateCheckboxGroupInput(session, "udk_nogletal2",
                           selected = FALSE)
  updateCheckboxGroupInput(session, "udk_sagsbehandlingstider1",
                           selected = FALSE)
  updateRadioButtons(session, "udk_lov_el_paragraf",
                     selected = 1)

})

observeEvent(input$ask_nulstil_filtre, {
  updatePickerInput(session, "ask_in_periode",
                    selected = FALSE)
  updatePickerInput(session, "ask_in_lovgivning",
                    selected = FALSE)
  updateRadioButtons(session, "ask_aar_el_kvartal",
                     selected = 1)
  updateCheckboxGroupInput(session, "ask_antal_sager2",
                           selected = FALSE)
  updateCheckboxGroupInput(session, "ask_antal_sager3",
                           selected = FALSE)
  updateCheckboxGroupInput(session, "ask_nogletal1",
                           selected = FALSE)
  updateCheckboxGroupInput(session, "ask_nogletal2",
                           selected = FALSE)
  updateCheckboxGroupInput(session, "ask_sagsbehandlingstider1",
                           selected = FALSE)
  updateRadioButtons(session, "ask_lov_el_paragraf",
                     selected = 1)

})



# Test af renderUI --------------------------------------------------------


output$moreControls <- renderUI({
req(len_valg() >= 1, len_valg() < 2000)
# fluidRow(
#   column(12,
         box(title = "Eksport af tabel/data",
             width = NULL,
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = FALSE,
             #downloadButton("downloadData_2", "PNG (billede)"),
             #downloadButton("downloadData", "CSV"),
             #downloadButton("downloadData_xlsx", "XLSX (Excel)"),
             actionButton("test_download_modal", "Vælg download", icon = icon("download"))
             # actionGroupButtons(
             #   inputIds = c("btn1", "btn2", "btn3"),
             #   labels = list("PNG (billede)", "CSV", tags$span(icon("file-excel"), "XLSX (Excel)")),
             #   status = "primary"
             # )
             # actionButton("btn3", "Download tabel som billede"),
             # actionButton("bt4", "Download data (Excel el. CSV)")
         )
})

output$udk_moreControls <- renderUI({
  req(len_valg_udk() >= 1, len_valg_udk() < 2000)
  # fluidRow(
  #   column(12,
  box(title = "Eksport af tabel/data",
      width = NULL,
      status = "danger",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = FALSE,
      #downloadButton("downloadData_2", "PNG (billede)"),
      #downloadButton("downloadData", "CSV"),
      #downloadButton("downloadData_xlsx", "XLSX (Excel)"),
      actionButton("test_download_modal_udk", "Vælg download", icon = icon("download"))
      # actionGroupButtons(
      #   inputIds = c("btn1", "btn2", "btn3"),
      #   labels = list("PNG (billede)", "CSV", tags$span(icon("file-excel"), "XLSX (Excel)")),
      #   status = "primary"
      # )
      # actionButton("btn3", "Download tabel som billede"),
      # actionButton("bt4", "Download data (Excel el. CSV)")
  )
})

output$ask_moreControls <- renderUI({
  req(len_valg_ask() >= 1, len_valg_ask() < 2000)
  # fluidRow(
  #   column(12,
  box(title = "Eksport af tabel/data",
      width = NULL,
      status = "danger",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = FALSE,
      #downloadButton("downloadData_2", "PNG (billede)"),
      #downloadButton("downloadData", "CSV"),
      #downloadButton("downloadData_xlsx", "XLSX (Excel)"),
      actionButton("test_download_modal_ask", "Vælg download", icon = icon("download"))
      # actionGroupButtons(
      #   inputIds = c("btn1", "btn2", "btn3"),
      #   labels = list("PNG (billede)", "CSV", tags$span(icon("file-excel"), "XLSX (Excel)")),
      #   status = "primary"
      # )
      # actionButton("btn3", "Download tabel som billede"),
      # actionButton("bt4", "Download data (Excel el. CSV)")
  )
})

}
