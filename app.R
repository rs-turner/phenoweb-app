# ---- auto-install and load libraries ----
required_packages <- c("shiny", "shinydashboard", "readxl", "tidyverse", "DT", "leaflet")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(new_packages)) {
  install.packages(new_packages, repos = "https://cran.rstudio.com/")
}

library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(DT)
library(leaflet)

# ---- settings ----
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# ---- utility functions ----
get_dropbox_path <- function() {
  paths <- c(Sys.getenv("DROPBOX"),
             file.path(Sys.getenv("USERPROFILE"), "Dropbox"), 
             file.path(Sys.getenv("HOME"), "Dropbox"))
  valid_path <- paths[paths != "" & dir.exists(paths)]
  if (length(valid_path) == 0) stop("Dropbox not found.")
  return(valid_path[1])
}

find_file_in_dropbox <- function(filename) {
  path <- list.files(get_dropbox_path(), pattern = paste0("^", filename, "$"), 
                     recursive = TRUE, full.names = TRUE)
  if (length(path) == 0) stop("File not found: ", filename)
  return(path[1])
}

load_bird_data <- function(path, region_name) {
  read_excel(path) %>%
    mutate(sitebox = str_c(site, box, sep = " "), region = region_name)
}

# ---- ui ----
ui <- dashboardPage(
  dashboardHeader(
    title = paste("Phenoweb Bird Phenology", current_year), 
    titleWidth = 300,
    tags$li(class = "dropdown", 
            actionButton("refresh_data", "Refresh Data", icon = icon("sync"), 
                         style = "margin-top: 8px; margin-right: 10px;"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dove")),
      selectInput("region_filter", "Region:", choices = "All"),
      selectInput("site_filter", "Site:", choices = "All"),
      selectInput("species_filter", "Species:", choices = "All")
    )
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("nests_initiated", width = 3),
      valueBoxOutput("avg_lay_date", width = 3),
      valueBoxOutput("avg_clutch_size", width = 3),
      valueBoxOutput("total_fledge", width = 3) 
    ),
    fluidRow(
      valueBoxOutput("hist_avg_lay", width = 6),
      valueBoxOutput("hist_avg_cs", width = 6)
    ),
    fluidRow(
      tabBox(title = "Exploration Tools", width = 12, id = "tab_main",
             tabPanel("Nest Summary Table", icon = icon("table"), DTOutput("bird_table")),
             tabPanel("Find My Bird", icon = icon("search"), DTOutput("ringing_table")),
             tabPanel("Map View", icon = icon("map-marker-alt"), leafletOutput("nest_map", height = 600))
      )
    )
  )
)

# ---- server ----
server <- function(input, output, session) {
  
  vals <- reactiveValues(master_current = NULL, historic = NULL, ringing_data = NULL, coords = NULL)
  
  load_all_data <- function() {
    withProgress(message = 'Syncing Dropbox...', value = 0.5, {
      
      # 1. Load Current Year Data
      south_file <- str_c("Bird_Phenology_", current_year, "_south.xlsx")
      north_file <- str_c("Bird_Phenology_", current_year, "_north.xlsx")
      
      current <- bind_rows(
        load_bird_data(find_file_in_dropbox(south_file), "south"),
        load_bird_data(find_file_in_dropbox(north_file), "north")
      ) %>%
        mutate(
          Region = str_to_title(region), Site = site, Box = as.character(box), 
          Species = case_match(species, "bluti" ~ "Blue Tit", "coati" ~ "Coal Tit", "greti" ~ "Great Tit", .default = species),
          `Lay Date` = if_else(is.na(latest_fed), "", format(as.Date(round(latest_fed) - 1, origin = str_c(current_year, "-01-01")), "%d %B %Y")),
          `Hatch Date` = if_else(is.na(`day hatching first observed`), "", format(as.Date(round(`day hatching first observed`) - 1, origin = str_c(current_year, "-01-01")), "%d %B %Y")),
          `Clutch Size` = as.numeric(cs), `Brood Size` = as.numeric(v1alive), `Number Fledged` = as.numeric(suc),
          lay_date_numeric = latest_fed
        )
      
      site_region_map <- current %>% select(Site, Region) %>% distinct()
      
      # 2. Cache Coordinates
      if (is.null(vals$coords)) {
        vals$coords <- read_csv(find_file_in_dropbox("nest_coords.csv"), show_col_types = FALSE) %>%
          mutate(Box = as.character(nest_number)) %>% select(site, Box, lon, lat)
      }
      
      # 3. Cache Historical Data
      if (is.null(vals$historic)) {
        vals$historic <- read_csv(find_file_in_dropbox("Bird_Phenology.csv"), show_col_types = FALSE) %>%
          mutate(Site = site, Species = case_match(species, "bluti" ~ "Blue Tit", "coati" ~ "Coal Tit", "greti" ~ "Great Tit", .default = species),
                 lay_date_historic = coalesce(fed, latestfed)) %>%
          left_join(site_region_map, by = "Site") %>%
          mutate(Region = replace_na(Region, "Unknown"))
      }
      
      # 4. Cache Ringing Data
      if (is.null(vals$ringing_data)) {
        adults <- read_csv(find_file_in_dropbox("Adults.csv"), show_col_types = FALSE) %>%
          transmute(Ring = ring, Date = as.Date(round(date) - 1, origin = str_c(year, "-01-01")),
                    Site = site, Box = as.character(box), Age = as.character(age),
                    Sex = case_match(sex, "M" ~ "Male", "F" ~ "Female", .default = sex), 
                    Wing = wing, Weight = mass, Ringer = ringer, Species = "Blue Tit", Fledged = NA_character_)
        
        nestlings <- read_csv(find_file_in_dropbox("Nestlings.csv"), show_col_types = FALSE) %>%
          filter(ring != "unringed") %>%
          mutate(fledged = if_else(fledged == 1, "Yes", "No")) %>%
          transmute(Ring = ring, Date = as.Date(round(v2date) - 1, origin = str_c(year, "-01-01")),
                    Site = site, Box = as.character(box), Age = "1", Sex = NA_character_, 
                    Wing = v2wing, Weight = v2mass, Ringer = v1ringer, Species = "Blue Tit", Fledged = fledged)
        
        vals$ringing_data <- bind_rows(adults, nestlings) %>%
          left_join(site_region_map, by = "Site") %>%
          mutate(Region = replace_na(Region, "Unknown")) %>%
          select(Region, Site, Box, Species, Date, Ring, Age, Sex, Wing, Weight, Fledged, Ringer)
      }
      
      vals$master_current <- current %>% left_join(vals$coords, by = c("Site" = "site", "Box" = "Box"))
    })
  }
  
  observeEvent(input$refresh_data, { load_all_data() }, ignoreNULL = FALSE)
  
  observe({
    req(vals$master_current)
    data <- vals$master_current
    updateSelectInput(session, "region_filter", choices = c("All", sort(unique(data$Region))), selected = input$region_filter)
    choices_site <- if(input$region_filter == "All") data else data %>% filter(Region == input$region_filter)
    updateSelectInput(session, "site_filter", choices = c("All", sort(unique(choices_site$Site))), selected = input$site_filter)
    updateSelectInput(session, "species_filter", choices = c("All", sort(unique(data$Species))), selected = input$species_filter)
  })
  
  filtered_all <- reactive({
    req(vals$master_current)
    vals$master_current %>%
      filter(if(input$region_filter != "All") Region == input$region_filter else TRUE) %>%
      filter(if(input$site_filter != "All") Site == input$site_filter else TRUE) %>%
      filter(if(input$species_filter != "All") Species == input$species_filter else TRUE)
  })
  
  filtered_init <- reactive({ filtered_all() %>% filter(!is.na(n1)) })
  
  filtered_hist <- reactive({
    req(vals$historic)
    vals$historic %>%
      filter(if(input$region_filter != "All") Region == input$region_filter else TRUE) %>%
      filter(if(input$site_filter != "All") Site == input$site_filter else TRUE) %>%
      filter(if(input$species_filter != "All") Species == input$species_filter else TRUE)
  })
  
  # --- Value Boxes ---
  output$nests_initiated <- renderValueBox({
    n_init <- nrow(filtered_init()); total <- nrow(filtered_all())
    perc <- if(total > 0) round((n_init / total) * 100, 1) else 0
    valueBox(paste0(n_init, " (", perc, "%)"), "Nests Initiated", icon = icon("feather-alt"), color = "blue")
  })
  
  output$avg_lay_date <- renderValueBox({
    valid <- filtered_init() %>% filter(!is.na(lay_date_numeric))
    n_count <- nrow(valid)
    mean_val <- mean(valid$lay_date_numeric, na.rm = TRUE)
    display <- if(is.nan(mean_val)) "N/A" else format(as.Date(round(mean_val)-1, origin=str_c(current_year, "-01-01")), "%d %B")
    valueBox(display, paste0(current_year, " Avg Lay Date (n = ", n_count, ")"), icon = icon("calendar"), color = "green")
  })
  
  output$avg_clutch_size <- renderValueBox({
    valid <- filtered_init() %>% filter(!is.na(`Clutch Size`))
    n_count <- nrow(valid)
    val <- mean(valid$`Clutch Size`, na.rm = TRUE)
    valueBox(if(is.nan(val)) "N/A" else round(val, 1), paste0(current_year, " Avg Clutch Size (n = ", n_count, ")"), icon = icon("egg"), color = "purple")
  })
  
  output$total_fledge <- renderValueBox({
    valid <- filtered_init() %>% filter(!is.na(`Number Fledged`))
    n_count <- nrow(valid)
    total_val <- sum(valid$`Number Fledged`, na.rm = TRUE)
    valueBox(total_val, paste0(current_year, " Total Fledglings (n = ", n_count, ")"), icon = icon("dove"), color = "yellow")
  })
  
  output$hist_avg_lay <- renderValueBox({
    valid <- filtered_hist() %>% filter(!is.na(lay_date_historic))
    n_count <- nrow(valid)
    hist_ord <- mean(valid$lay_date_historic, na.rm = TRUE)
    display <- if(is.nan(hist_ord)) "N/A" else format(as.Date(round(hist_ord)-1, origin=str_c(current_year, "-01-01")), "%d %B")
    valueBox(display, paste0("Historic Avg Lay Date (n = ", n_count, ")"), icon = icon("history"), color = "teal")
  })
  
  output$hist_avg_cs <- renderValueBox({
    valid <- filtered_hist() %>% filter(!is.na(cs))
    n_count <- nrow(valid)
    val <- mean(valid$cs, na.rm = TRUE)
    valueBox(if(is.nan(val)) "N/A" else round(val, 1), paste0("Historic Avg Clutch Size (n = ", n_count, ")"), icon = icon("history"), color = "maroon")
  })
  
  # --- Tables & Map ---
  output$bird_table <- renderDT({
    display_df <- filtered_all() %>% 
      select(Region, Site, Box, Species, `Lay Date`, `Clutch Size`, `Hatch Date`, `Brood Size`, `Number Fledged`) %>%
      mutate(across(everything(), ~replace_na(as.character(.x), "")))
    datatable(display_df, filter = 'top', options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  output$ringing_table <- renderDT({
    req(vals$ringing_data)
    ringing_df <- vals$ringing_data %>%
      filter(if(input$region_filter != "All") Region == input$region_filter else TRUE) %>%
      filter(if(input$site_filter != "All") Site == input$site_filter else TRUE) %>%
      mutate(sort_date = Date, Date = format(Date, "%d %B %Y")) %>%
      relocate(sort_date, .after = last_col())
    
    sort_col_idx <- ncol(ringing_df) - 1
    datatable(ringing_df, filter = 'top', rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE, order = list(list(sort_col_idx, 'asc')),
                             columnDefs = list(list(visible = FALSE, targets = sort_col_idx))))
  })
  
  output$nest_map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(baseGroups = c("Satellite", "Street Map"), options = layersControlOptions(collapsed = FALSE)) %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street Map")
  })
  
  observe({
    map_df <- filtered_all() %>% filter(!is.na(lat) & !is.na(lon))
    leafletProxy("nest_map", data = map_df) %>% clearMarkers() %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, radius = 7, color = "white", weight = 1,
                       fillColor = ~if_else(!is.na(n1), "#2196F3", "#F44336"), fillOpacity = 0.8,
                       popup = ~paste0("<b>Box ID:</b> ", sitebox, "<br><b>Status:</b> ", if_else(!is.na(n1), "Occupied", "Empty"), "<br><b>Species:</b> ", replace_na(as.character(Species), ""), "<br><b>Lay Date:</b> ", replace_na(as.character(`Lay Date`), ""), "<br><b>Clutch Size:</b> ", replace_na(as.character(`Clutch Size`), ""), "<br><b>Hatch Date:</b> ", replace_na(as.character(`Hatch Date`), ""), "<br><b>Brood Size:</b> ", replace_na(as.character(`Brood Size`), ""), "<br><b>Number Fledged:</b> ", replace_na(as.character(`Number Fledged`), "")),
                       label = ~sitebox)
  })
}

shinyApp(ui, server)