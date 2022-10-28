library(tidyverse)
library(janitor)
library(readxl)
library(jsonlite)
library(googlesheets4)
library(googledrive)
library(htmlwidgets)
library(data.table)
library(htmltools)
library(fontawesome)

# google authentication

#GOOGLE_JSON <- Sys.getenv("GOOGLE_JSON")
#drive_auth(cache = ".secrets")
#drive_auth(token = GOOGLE_JSON)
#drive_auth(path = "/Users/runner/work/KGO-Evacuations/KGO-Evacuations/google_service_account.json")
#gs4_auth(token = GS_API)

# loading in each google sheet and adding county + state columns

alameda <- read_csv("https://docs.google.com/spreadsheets/d/1yUNrLhrJ1ijTqHlde_6Ls4HV0YgodL80vKDa-ddCn-Q/gviz/tq?tqx=out:csv&gid=0")

alameda['County'] = 'Alameda'
alameda['State'] = 'California'

contra_costa <- read_csv("https://docs.google.com/spreadsheets/d/1yUNrLhrJ1ijTqHlde_6Ls4HV0YgodL80vKDa-ddCn-Q/gviz/tq?tqx=out:csv&gid=1620495568")

contra_costa['County'] = 'Contra Costa'
contra_costa['State'] = 'California'

lake <- read_csv("https://docs.google.com/spreadsheets/d/1yUNrLhrJ1ijTqHlde_6Ls4HV0YgodL80vKDa-ddCn-Q/gviz/tq?tqx=out:csv&gid=800178297")

lake['County'] = 'Lake'
lake['State'] = 'California'

marin <- read_csv("https://docs.google.com/spreadsheets/d/1yUNrLhrJ1ijTqHlde_6Ls4HV0YgodL80vKDa-ddCn-Q/gviz/tq?tqx=out:csv&gid=368196789")

marin['County'] = 'Marin'
marin['State'] = 'California'

merced <- read_csv("https://docs.google.com/spreadsheets/d/1yUNrLhrJ1ijTqHlde_6Ls4HV0YgodL80vKDa-ddCn-Q/gviz/tq?tqx=out:csv&gid=878671387")

merced['County'] = 'Merced'
merced['State'] = 'California'

napa <- read_csv("https://docs.google.com/spreadsheets/d/1yUNrLhrJ1ijTqHlde_6Ls4HV0YgodL80vKDa-ddCn-Q/gviz/tq?tqx=out:csv&gid=71542859")

napa['County'] = 'Napa'
napa['State'] = 'California'

san_mateo <- read_csv("https://docs.google.com/spreadsheets/d/1yUNrLhrJ1ijTqHlde_6Ls4HV0YgodL80vKDa-ddCn-Q/gviz/tq?tqx=out:csv&gid=1836818617")

san_mateo['County'] = 'San Mateo'
san_mateo['State'] = 'California'

santa_clara <- read_csv("https://docs.google.com/spreadsheets/d/1yUNrLhrJ1ijTqHlde_6Ls4HV0YgodL80vKDa-ddCn-Q/gviz/tq?tqx=out:csv&gid=2115273005")

santa_clara['County'] = 'Santa Clara'
santa_clara['State'] = 'California'

santa_cruz <- read_csv("https://docs.google.com/spreadsheets/d/1yUNrLhrJ1ijTqHlde_6Ls4HV0YgodL80vKDa-ddCn-Q/gviz/tq?tqx=out:csv&gid=1378970754")

santa_cruz['County'] = 'Santa Cruz'
santa_cruz['State'] = 'California'

solano <- read_csv("https://docs.google.com/spreadsheets/d/1yUNrLhrJ1ijTqHlde_6Ls4HV0YgodL80vKDa-ddCn-Q/gviz/tq?tqx=out:csv&gid=282322297")

solano['County'] = 'Solano'
solano['State'] = 'California'

sonoma <- read_csv("https://docs.google.com/spreadsheets/d/1yUNrLhrJ1ijTqHlde_6Ls4HV0YgodL80vKDa-ddCn-Q/gviz/tq?tqx=out:csv&gid=583273920")

sonoma['County'] = 'Sonoma'
sonoma['State'] = 'California'


# individual county tables

# Alameda

alameda1 <- alameda %>%
  select("Type", "Description")

alameda_table <- DT::datatable(alameda1, width = "100%", height = NULL, rownames = FALSE, class = 'hover table-bordered table-condensed cell-border', extensions = "FixedColumns", options = list(pageLength = 15, language = list(searchPlaceholder = "Enter your location", lengthMenu = "_MENU_", dom = 'tr', zeroRecords = " "), fixedColumns = list(leftColumns = 1), lengthChange = FALSE, scrollX = TRUE,  initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  "}")
))

alameda_table1 = alameda_table %>% prependContent(
  tags$head(
    tags$style(
      "body {
  padding-top: 0px!important;
}
        "
    )))

saveWidget(alameda_table1, 'alameda_table.html', selfcontained = TRUE)


# Contra Costa

contra_costa1 <- contra_costa %>%
  select("Type", "Description")

contra_costa_table <- DT::datatable(contra_costa1, width = "100%", height = NULL, rownames = FALSE, class = 'hover table-bordered table-condensed cell-border', extensions = "FixedColumns", options = list(pageLength = 15, language = list(searchPlaceholder = "Enter your location", lengthMenu = "_MENU_", dom = 'tr', zeroRecords = " "), fixedColumns = list(leftColumns = 1), lengthChange = FALSE, scrollX = TRUE,  initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  "}")
))

contra_costa_table1 = contra_costa_table %>% prependContent(
  tags$head(
    tags$style(
      "body {
  padding-top: 0px!important;
}
        "
    )))

saveWidget(contra_costa_table1, 'contra_costa_table.html', selfcontained = TRUE)

# Lake

lake1 <- lake %>%
  select("Type", "Description")

lake_table <- DT::datatable(lake1, width = "100%", height = NULL, rownames = FALSE, class = 'hover table-bordered table-condensed cell-border', extensions = "FixedColumns", options = list(pageLength = 15, language = list(searchPlaceholder = "Enter your location", lengthMenu = "_MENU_", dom = 'tr', zeroRecords = " "), fixedColumns = list(leftColumns = 1), lengthChange = FALSE, scrollX = TRUE,  initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  "}")
))

lake_table1 = lake_table %>% prependContent(
  tags$head(
    tags$style(
      "body {
  padding-top: 0px!important;
}
        "
    )))

saveWidget(lake_table1, 'lake_table.html', selfcontained = TRUE)

# Marin

marin1 <- marin %>%
  select("Type", "Description")

marin_table <- DT::datatable(marin1, width = "100%", height = NULL, rownames = FALSE, class = 'hover table-bordered table-condensed cell-border', extensions = "FixedColumns", options = list(pageLength = 15, language = list(searchPlaceholder = "Enter your location", lengthMenu = "_MENU_", dom = 'tr', zeroRecords = " "), fixedColumns = list(leftColumns = 1), lengthChange = FALSE, scrollX = TRUE,  initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  "}")
))

marin_table1 = marin_table %>% prependContent(
  tags$head(
    tags$style(
      "body {
  padding-top: 0px!important;
}
        "
    )))

saveWidget(marin_table1, 'marin_table.html', selfcontained = TRUE)

# Merced

merced1 <- merced %>%
  select("Type", "Description")

merced_table <- DT::datatable(merced1, width = "100%", height = NULL, rownames = FALSE, class = 'hover table-bordered table-condensed cell-border', extensions = "FixedColumns", options = list(pageLength = 15, language = list(searchPlaceholder = "Enter your location", lengthMenu = "_MENU_", dom = 'tr', zeroRecords = " "), fixedColumns = list(leftColumns = 1), lengthChange = FALSE, scrollX = TRUE,  initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  "}")
))

merced_table1 = merced_table %>% prependContent(
  tags$head(
    tags$style(
      "body {
  padding-top: 0px!important;
}
        "
    )))

saveWidget(merced_table1, 'merced_table.html', selfcontained = TRUE)

# Napa

napa1 <- napa %>%
  select("Type", "Description")

napa_table <- DT::datatable(napa1, width = "100%", height = NULL, rownames = FALSE, class = 'hover table-bordered table-condensed cell-border', extensions = "FixedColumns", options = list(pageLength = 15, language = list(searchPlaceholder = "Enter your location", lengthMenu = "_MENU_", dom = 'tr', zeroRecords = " "), fixedColumns = list(leftColumns = 1), lengthChange = FALSE, scrollX = TRUE,  initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  "}")
))

napa_table1 = napa_table %>% prependContent(
  tags$head(
    tags$style(
      "body {
  padding-top: 0px!important;
}
        "
    )))

saveWidget(napa_table1, 'napa_table.html', selfcontained = TRUE)

# San Mateo

san_mateo1 <- san_mateo %>%
  select("Type", "Description")

san_mateo_table <- DT::datatable(san_mateo1, width = "100%", height = NULL, rownames = FALSE, class = 'hover table-bordered table-condensed cell-border', extensions = "FixedColumns", options = list(pageLength = 15, language = list(searchPlaceholder = "Enter your location", lengthMenu = "_MENU_", dom = 'tr', zeroRecords = " "), fixedColumns = list(leftColumns = 1), lengthChange = FALSE, scrollX = TRUE,  initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  "}")
))

san_mateo_table1 = san_mateo_table %>% prependContent(
  tags$head(
    tags$style(
      "body {
  padding-top: 0px!important;
}
        "
    )))

saveWidget(san_mateo_table1, 'san_mateo_table.html', selfcontained = TRUE)

# Santa Clara

santa_clara1 <- santa_clara %>%
  select("Type", "Description")

santa_clara_table <- DT::datatable(santa_clara1, width = "100%", height = NULL, rownames = FALSE, class = 'hover table-bordered table-condensed cell-border', extensions = "FixedColumns", options = list(pageLength = 15, language = list(searchPlaceholder = "Enter your location", lengthMenu = "_MENU_", dom = 'tr', zeroRecords = " "), fixedColumns = list(leftColumns = 1), lengthChange = FALSE, scrollX = TRUE,  initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  "}")
))

santa_clara_table1 = santa_clara_table %>% prependContent(
  tags$head(
    tags$style(
      "body {
  padding-top: 0px!important;
}
        "
    )))

saveWidget(santa_clara_table1, 'santa_clara_table.html', selfcontained = TRUE)

# Santa Cruz

santa_cruz1 <- santa_cruz %>%
  select("Type", "Description")

santa_cruz_table <- DT::datatable(santa_cruz1, width = "100%", height = NULL, rownames = FALSE, class = 'hover table-bordered table-condensed cell-border', extensions = "FixedColumns", options = list(pageLength = 15, language = list(searchPlaceholder = "Enter your location", lengthMenu = "_MENU_", dom = 'tr', zeroRecords = " "), fixedColumns = list(leftColumns = 1), lengthChange = FALSE, scrollX = TRUE,  initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  "}")
))

santa_cruz_table1 = santa_cruz_table %>% prependContent(
  tags$head(
    tags$style(
      "body {
  padding-top: 0px!important;
}
        "
    )))

saveWidget(santa_cruz_table1, 'santa_cruz_table.html', selfcontained = TRUE)

# Solano

solano1 <- solano %>%
  select("Type", "Description")

solano_table <- DT::datatable(solano1, width = "100%", height = NULL, rownames = FALSE, class = 'hover table-bordered table-condensed cell-border', extensions = "FixedColumns", options = list(pageLength = 15, language = list(searchPlaceholder = "Enter your location", lengthMenu = "_MENU_", dom = 'tr', zeroRecords = " "), fixedColumns = list(leftColumns = 1), lengthChange = FALSE, scrollX = TRUE,  initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  "}")
))

solano_table1 = solano_table %>% prependContent(
  tags$head(
    tags$style(
      "body {
  padding-top: 0px!important;
}
        "
    )))

saveWidget(solano_table1, 'solano_table.html', selfcontained = TRUE)

# Sonoma

sonoma1 <- sonoma %>%
  select("Type", "Description")

sonoma_table <- DT::datatable(sonoma1, width = "100%", height = NULL, rownames = FALSE, class = 'hover table-bordered table-condensed cell-border', extensions = "FixedColumns", options = list(pageLength = 15, language = list(searchPlaceholder = "Enter your location", lengthMenu = "_MENU_", dom = 'tr', zeroRecords = " "), fixedColumns = list(leftColumns = 1), lengthChange = FALSE, scrollX = TRUE,  initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  "}")
))

sonoma_table1 = sonoma_table %>% prependContent(
  tags$head(
    tags$style(
      "body {
  padding-top: 0px!important;
}
        "
    )))

saveWidget(sonoma_table1, 'sonoma_table.html', selfcontained = TRUE)

# all evacs table

all_evacs <- rbind(alameda, contra_costa, lake, marin, merced, napa, san_mateo, santa_clara, santa_cruz, solano, sonoma)

all_evacs1 <- all_evacs %>%
  select("Type", "Description", "County")

all_evacs_table <- DT::datatable(all_evacs1, width = "100%", height = NULL, rownames = FALSE, class = 'hover table-bordered table-condensed cell-border', extensions = "FixedColumns", options = list(pageLength = 15, language = list(searchPlaceholder = "Enter your location", lengthMenu = "_MENU_", dom = 'tr', zeroRecords = " "), fixedColumns = list(leftColumns = 1), lengthChange = FALSE, scrollX = TRUE,  initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  "}")
))

all_evacs_table1 = all_evacs_table %>% prependContent(
  tags$head(
    tags$style(
      "body {
  padding-top: 0px!important;
}
        "
    )))

saveWidget(all_evacs_table1, 'all_evacs_table.html', selfcontained = TRUE)

