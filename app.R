# ---- auto-install and load libraries ----
required_packages <- c("shiny", "shinydashboard", "readxl", "tidyverse", "DT", "leaflet")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(new_packages)) {
  message("Installing missing packages: ", paste(new_packages, collapse = ", "))
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
  c(Sys.getenv("DROPBOX"),
    file.path(Sys.getenv("USERPROFILE"), "Dropbox"), 
    file.path(Sys.getenv("HOME"), "Dropbox")) %>% 
    discard(~ .x == "" || !dir.exists(.x)) %>%
    first() %>%
    { if (is.na(.)) stop("Dropbox not found.") else . }
}

find_file_in_dropbox <- function(filename) {
  get_dropbox_path() %>%
    list.files(
      pattern = str_c("^", filename, "$"),
      recursive = TRUE,
      full.names = TRUE
    ) %>%
    { if (length(.) == 0) stop("File not found: ", filename) else . } %>%
    first()
}

load_bird_data <- function(path, region_name) {
  read_excel(path) %>%
    mutate(sitebox = str_c(site, box, sep = " "),
           region  = region_name)
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
  
  vals <- reactiveValues(master_current = NULL, historic = NULL, ringing_data = NULL)
  
  load_all_data <- function() {
    withProgress(message = 'Updating from Dropbox...', value = 0, {
      
      # 1. Load Current Year Data
      south_file <- str_c("Bird_Phenology_", current_year, "_south.xlsx")
      north_file <- str_c("Bird_Phenology_", current_year, "_north.xlsx")
      
      current <- bind_rows(
        load_bird_data(find_file_in_dropbox(south_file), "south"),
        load_bird_data(find_file_in_dropbox(north_file), "north")
      ) %>%
        mutate(
          Region = str_to_title(region),
          Site = site,
          Box = as.character(box), 
          Species = case_match(species, "bluti" ~ "Blue Tit", "coati" ~ "Coal Tit", "greti" ~ "Great Tit", .default = species),
          `Lay Date` = if_else(is.na(latest_fed), "", format(as.Date(round(latest_fed) - 1, origin = str_c(current_year, "-01-01")), "%d %B %Y")),
          `Hatch Date` = if_else(is.na(`day hatching first observed`), "", format(as.Date(round(`day hatching first observed`) - 1, origin = str_c(current_year, "-01-01")), "%d %B %Y")),
          `Clutch Size` = as.numeric(cs), 
          `Brood Size` = as.numeric(v1alive), 
          `Number Fledged` = as.numeric(suc),
          lay_date_numeric = latest_fed
        )
      
      site_region_map <- current %>% select(Site, Region) %>% distinct()
      
      coords_data <- read_csv(find_file_in_dropbox("nest_coords.csv"), show_col_types = FALSE) %>%
        mutate(Box = as.character(nest_number)) %>%
        select(site, Box, lon, lat)
      
      current <- current %>% left_join(coords_data, by = c("Site" = "site", "Box" = "Box"))
      
      # 4. Historical
      historic <- read_csv(find_file_in_dropbox("Bird_Phenology.csv"), show_col_types = FALSE) %>%
        mutate(Site = site,
               Species = case_match(species, "bluti" ~ "Blue Tit", "coati" ~ "Coal Tit", "greti" ~ "Great Tit", .default = species),
               lay_date_historic = coalesce(fed, latestfed)) %>%
        left_join(site_region_map, by = "Site") %>%
        mutate(Region = replace_na(Region, "Unknown"))
      
      # 5. Ringing Data
      adults_raw <- read_csv(find_file_in_dropbox("Adults.csv"), show_col_types = FALSE)
      nestlings_raw <- read_csv(find_file_in_dropbox("Nestlings.csv"), show_col_types = FALSE)
      
      adults <- adults_raw %>%
        transmute(Ring = ring, 
                  Date = as.Date(round(date) - 1, origin = str_c(year, "-01-01")),
                  Site = site, 
                  Box = as.character(box),
                  Age = as.character(age),
                  Sex = case_match(sex, "M" ~ "Male", "F" ~ "Female", .default = sex), 
                  Wing = wing, 
                  Weight = mass, 
                  Ringer = ringer, 
                  Species = "Blue Tit")
      
      nestlings <- nestlings_raw %>%
        filter(fledged == 1) %>%
        transmute(Ring = ring, 
                  Date = as.Date(round(v2date) - 1, origin = str_c(year, "-01-01")),
                  Site = site, 
                  Box = as.character(box), 
                  Age = "1", 
                  Sex = NA_character_, 
                  Wing = v2wing, 
                  Weight = v2mass, 
                  Ringer = v1ringer, 
                  Species = "Blue Tit")
      
      ringing_combined <- bind_rows(adults, nestlings) %>%
        left_join(site_region_map, by = "Site") %>%
        mutate(Region = replace_na(Region, "Unknown")) %>%
        select(Region, Site, Box, Species, Date, Ring, Age, Sex, Wing, Weight, Ringer)
      
      vals$master_current <- current
      vals$historic <- historic
      vals$ringing_data <- ringing_combined
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
  
  filtered_all_boxes <- reactive({
    req(vals$master_current)
    vals$master_current %>%
      filter(if(input$region_filter != "All") Region == input$region_filter else TRUE) %>%
      filter(if(input$site_filter != "All") Site == input$site_filter else TRUE) %>%
      filter(if(input$species_filter != "All") Species == input$species_filter else TRUE)
  })
  
  filtered_initiated <- reactive({ filtered_all_boxes() %>% filter(!is.na(n1)) })
  
  filtered_historic <- reactive({
    req(vals$historic)
    vals$historic %>%
      filter(if(input$region_filter != "All") Region == input$region_filter else TRUE) %>%
      filter(if(input$site_filter != "All") Site == input$site_filter else TRUE) %>%
      filter(if(input$species_filter != "All") Species == input$species_filter else TRUE)
  })
  
  output$nests_initiated <- renderValueBox({
    n_init <- nrow(filtered_initiated()); total <- nrow(filtered_all_boxes())
    perc <- if(total > 0) round((n_init / total) * 100, 1) else 0
    valueBox(paste0(n_init, " (", perc, "%)"), "Nests Initiated", icon = icon("feather-alt"), color = "blue")
  })
  
  output$avg_lay_date <- renderValueBox({
    valid_data <- filtered_initiated() %>% filter(!is.na(lay_date_numeric))
    n_count <- nrow(valid_data); mean_ord <- mean(valid_data$lay_date_numeric, na.rm = TRUE)
    val_display <- if(is.nan(mean_ord)) "N/A" else format(as.Date(round(mean_ord)-1, origin=str_c(current_year, "-01-01")), "%d %B")
    valueBox(val_display, paste0(current_year, " Avg Lay Date (n = ", n_count, ")"), icon = icon("calendar"), color = "green")
  })
  
  output$avg_clutch_size <- renderValueBox({
    valid_data <- filtered_initiated() %>% filter(!is.na(`Clutch Size`))
    n_count <- nrow(valid_data); val <- mean(valid_data$`Clutch Size`, na.rm = TRUE)
    valueBox(if(is.nan(val)) "N/A" else round(val, 1), paste0(current_year, " Avg Clutch Size (n = ", n_count, ")"), icon = icon("egg"), color = "purple")
  })
  
  output$total_fledge <- renderValueBox({
    valid_data <- filtered_initiated() %>% filter(!is.na(`Number Fledged`))
    n_count <- nrow(valid_data)
    total_val <- sum(valid_data$`Number Fledged`, na.rm = TRUE)
    valueBox(if(n_count == 0) "0" else total_val, paste0(current_year, " Total Fledglings (n = ", n_count, ")"), icon = icon("dove"), color = "yellow")
  })
  
  output$hist_avg_lay <- renderValueBox({
    valid_data <- filtered_historic() %>% 
      filter(!is.na(lay_date_historic)) %>%
      pull(lay_date_historic)
    
    if (length(valid_data) == 0) {
      val_display <- "N/A"
      n_count <- 0
    } else {
      hist_ord <- mean(valid_data, na.rm = TRUE)
      n_count <- length(valid_data)
      val_display <- if(is.nan(hist_ord)) "N/A" else format(as.Date(round(hist_ord)-1, origin=str_c(current_year, "-01-01")), "%d %B")
    }
    valueBox(val_display, paste0("Historic Avg Lay Date (n = ", n_count, ")"), icon = icon("history"), color = "teal")
  })
  
  output$hist_avg_cs <- renderValueBox({
    valid_data <- filtered_historic() %>% filter(!is.na(cs))
    n_count <- nrow(valid_data); val <- mean(valid_data$cs, na.rm = TRUE)
    valueBox(if(n_count == 0 || is.nan(val)) "N/A" else round(val, 1), paste0("Historic Avg Clutch Size (n = ", n_count, ")"), icon = icon("history"), color = "maroon")
  })
  
  output$bird_table <- renderDT({
    display_df <- filtered_all_boxes() %>% 
      select(Region, Site, Box, Species, `Lay Date`, `Clutch Size`, `Hatch Date`, `Brood Size`, `Number Fledged`) %>%
      mutate(across(everything(), ~replace_na(as.character(.x), "")))
    datatable(display_df, 
              filter = 'top', 
              options = list(pageLength = 15, scrollX = TRUE), 
              rownames = FALSE)
  })
  
  output$ringing_table <- renderDT({
    req(vals$ringing_data)
    
    ringing_df <- vals$ringing_data %>%
      filter(if(input$region_filter != "All") Region == input$region_filter else TRUE) %>%
      filter(if(input$site_filter != "All") Site == input$site_filter else TRUE) %>%
      mutate(
        sort_date = Date, # Proxy column for true chronological sorting
        Date = format(Date, "%d %B %Y")
      ) %>%
      relocate(sort_date, .after = last_col())
    
    datatable(ringing_df, 
              filter = 'top', 
              options = list(
                pageLength = 15, 
                scrollX = TRUE, 
                # Sort by sort_date (index 11) ascending (oldest to newest)
                order = list(list(11, 'asc')), 
                # Hide the sort_date column from view
                columnDefs = list(list(visible = FALSE, targets = 11))
              ), 
              rownames = FALSE)
  })
  
  output$nest_map <- renderLeaflet({
    map_df <- filtered_all_boxes() %>% filter(!is.na(lat) & !is.na(lon))
    if(nrow(map_df) == 0) return(NULL)
    leaflet(map_df) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat, radius = 7, color = "white", weight = 1,
        fillColor = ~if_else(!is.na(n1), "#2196F3", "#F44336"), fillOpacity = 0.8,
        popup = ~paste0("<b>Box ID:</b> ", sitebox, "<br><b>Status:</b> ", if_else(!is.na(n1), "Occupied", "Empty"), "<br><b>Species:</b> ", replace_na(as.character(Species), ""), "<br><b>Lay Date:</b> ", replace_na(as.character(`Lay Date`), ""), "<br><b>Clutch Size:</b> ", replace_na(as.character(`Clutch Size`), ""), "<br><b>Hatch Date:</b> ", replace_na(as.character(`Hatch Date`), ""), "<br><b>Brood Size:</b> ", replace_na(as.character(`Brood Size`), ""), "<br><b>Number Fledged:</b> ", replace_na(as.character(`Number Fledged`), "")),
        label = ~sitebox 
      ) %>%
      addLayersControl(baseGroups = c("Satellite", "Street Map"), options = layersControlOptions(collapsed = FALSE))
  })
}

# ---- run app ----
shinyApp(ui, server)