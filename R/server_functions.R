# Beregnede variable -------------------------------------------------------------

tilfoj_realitetsbehandlede_sager <- function (.) {
  mutate(.,`Realitetsbehandlede sager i alt` = (Antal_sager_ialt -  Afvisning.Henvisning))
}

# tilfoj_omgorelsesprocent <- function (.) {
#   mutate(.,Omg?relsesprocent = (replace_na(?ndring.Oph?velse,0) + replace_na(Hjemvisning,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))
# }

#format(round(1.1234, 2), nsmall = 2)

tilfoj_hjemvisningsprocent <- function (.) {
  mutate(.,Hjemvisningsprocent = replace_na(Hjemvisning,0) / replace_na(`Realitetsbehandlede sager i alt`,0))
}

tilfoj_afvisningsprocent <- function (.) {
  mutate(.,Afvisningsprocent = replace_na(Afvisning.Henvisning,0) / replace_na(`Realitetsbehandlede sager i alt`,0))
}

# tilfoj_stadfaestelsesprocent <- function (.) { 
#   mutate(.,Stadf?stelsesprocent = replace_na(Stadf?stelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0))
# }

# tilfoj_aendrings_ophaevelsesprocent <- function (.) {
#   mutate(.,'?ndrings-/oph?velsesprocent' = replace_na(?ndring.Oph?velse,0) / replace_na(`Realitetsbehandlede sager i alt`,0))
# }
# 
# tilfoj_omgorelsesprocent2 <- function (.) {
#   mutate(.,Omg?relsesprocent = ((replace_na(?ndring.Oph?velse,0) + replace_na(Hjemvisning,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100)
# }

tilfoj_hjemvisningsprocent2 <- function (.) {
  mutate(.,Hjemvisningsprocent = (replace_na(Hjemvisning,0) / replace_na(`Realitetsbehandlede sager i alt`,0))*100)
}
# 
# tilf?j_afvisningsprocent2 <- function (.) {
#   mutate(.,Afvisningsprocent = (replace_na(Afvisning.Henvisning,0) / replace_na(`Realitetsbehandlede sager i alt`,0))*100)
# }
# 
# tilf?j_stadf?stelsesprocent2 <- function (.) { 
#   mutate(.,Stadf?stelsesprocent = (replace_na(Stadf?stelse,0) / replace_na(`Realitetsbehandlede sager i alt`,0))*100)
# }
# 
# tilf?j_?ndrings_oph?velsesprocent2 <- function (.) {
#   mutate(.,'?ndrings-/oph?velsesprocent' = (replace_na(?ndring.Oph?velse,0) / replace_na(`Realitetsbehandlede sager i alt`,0))*100)
# }

# Eksport af data ---------------------------------------------------------

# tilfoj_omgorelsesprocent_exl <- function (.) {
#   mutate(.,Omg?relsesprocent = ((replace_na(?ndring.Oph?velse,0) + replace_na(Hjemvisning,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100)
# }

tilfoj_hjemvisningsprocent_exl <- function (.) {
  mutate(.,Hjemvisningsprocent = ((replace_na(Hjemvisning,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100)
}

# tilfoj_stadfaestelsesprocent_exl <- function (.) { 
#   mutate(.,Stadf?stelsesprocent = ((replace_na(Stadf?stelse,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100)
# }

# tilfoj_aendrings_ophaevelsesprocent_exl <- function (.) {
#   mutate(.,'?ndrings-/oph?velsesprocent' = ((replace_na(?ndring.Oph?velse,0)) / replace_na(`Realitetsbehandlede sager i alt`,0))*100)
# }

# Noter til tabeller

tilfoj_note <- function (., note, placering) {
  tab_footnote(.,
    footnote = md(note),
      locations = cells_column_labels(
        columns = contains(placering))

  ) 
}


