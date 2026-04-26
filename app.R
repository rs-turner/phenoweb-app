# ---- load libraries ----
library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(DT)
library(leaflet)

# ---- settings ----
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# ---- function to locate users dropbox path ----
get_dropbox_path <- function() {
  c(Sys.getenv("DROPBOX"),
    file.path(Sys.getenv("USERPROFILE"), "Dropbox"), 
    file.path(Sys.getenv("HOME"), "Dropbox")) %>% 
    discard(~ .x == "" || !dir.exists(.x)) %>%
    first() %>%
    { if (is.na(.)) stop("Dropbox not found.") else . }
}

# ---- function to find bird phenology files within dropbox ----
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

# ---- function to load excel data ----
load_bird_data <- function(path, region_name) {
  read_excel(path) %>%
    mutate(sitebox = str_c(site, box, sep = " "),
           region  = region_name)
}

# ---- data preparation ----
south_file <- str_c("Bird_Phenology_", current_year, "_south.xlsx")
north_file <- str_c("Bird_Phenology_", current_year, "_north.xlsx")

master_current_data <- bind_rows(
  load_bird_data(find_file_in_dropbox(south_file), "south"),
  load_bird_data(find_file_in_dropbox(north_file), "north")
) %>%
  mutate(
    Region = str_to_title(region),
    Site = site,
    Box = as.character(box), 
    Species = case_match(species,
                         "bluti" ~ "Blue Tit",
                         "coati" ~ "Coal Tit",
                         "greti" ~ "Great Tit",
                         .default = species),
    
    `Lay Date` = if_else(is.na(latest_fed), "", 
                         format(as.Date(round(latest_fed) - 1, origin = str_c(current_year, "-01-01")), "%d %B %Y")),
    
    `Hatch Date` = if_else(is.na(`day hatching first observed`), "", 
                           format(as.Date(round(`day hatching first observed`) - 1, origin = str_c(current_year, "-01-01")), "%d %B %Y")),
    
    `Clutch Size` = cs,
    `Brood Size` = v1alive,
    `Number Fledged` = suc,
    lay_date_numeric = latest_fed
  )

coords_data <- read_csv(find_file_in_dropbox("nest_coords.csv")) %>%
  mutate(Box = as.character(nest_number)) %>%
  select(site, Box, lon, lat)

master_current_data <- master_current_data %>%
  left_join(coords_data, by = c("Site" = "site", "Box" = "Box"))

historic_data <- read_csv(find_file_in_dropbox("Bird_Phenology.csv")) %>%
  mutate(
    Species = case_match(species,
                         "bluti" ~ "Blue Tit",
                         "coati" ~ "Coal Tit",
                         "greti" ~ "Great Tit",
                         .default = species),
    lay_date_historic = coalesce(fed, latestfed)
  )

# ---- ui ----
ui <- dashboardPage(
  dashboardHeader(title = paste("Phenoweb Bird Phenology", current_year), titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dove")),
      selectInput("region_filter", "Region:", 
                  choices = c("All", sort(unique(master_current_data$Region)))),
      selectInput("site_filter", "Site:", choices = "All"),
      selectInput("species_filter", "Species:", 
                  choices = c("All", sort(unique(master_current_data$Species))))
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
             tabPanel("Summary Table", icon = icon("table"),
                      DTOutput("bird_table")),
             tabPanel("Map View", icon = icon("map-marker-alt"),
                      leafletOutput("nest_map", height = 600))
      )
    )
  )
)

# ---- server ----
server <- function(input, output, session) {
  
  observe({
    req(input$region_filter)
    choices_site <- master_current_data
    if (input$region_filter != "All") choices_site <- choices_site %>% filter(Region == input$region_filter)
    updateSelectInput(session, "site_filter", choices = c("All", sort(unique(choices_site$Site))))
  })
  
  filtered_all_boxes <- reactive({
    data <- master_current_data
    if (input$region_filter != "All") data <- data %>% filter(Region == input$region_filter)
    if (input$site_filter != "All")   data <- data %>% filter(Site == input$site_filter)
    if (input$species_filter != "All") data <- data %>% filter(Species == input$species_filter)
    data
  })
  
  filtered_initiated <- reactive({
    filtered_all_boxes() %>% filter(!is.na(n1))
  })
  
  filtered_historic <- reactive({
    data <- historic_data
    if (input$site_filter != "All")   data <- data %>% filter(site == input$site_filter)
    if (input$species_filter != "All") data <- data %>% filter(Species == input$species_filter)
    data
  })
  
  output$nests_initiated <- renderValueBox({
    n_init <- nrow(filtered_initiated())
    total <- nrow(filtered_all_boxes())
    perc <- if(total > 0) round((n_init / total) * 100, 1) else 0
    valueBox(paste0(n_init, " (", perc, "%)"), "Nests Initiated", icon = icon("feather-alt"), color = "blue")
  })
  
  output$avg_lay_date <- renderValueBox({
    valid_data <- filtered_initiated() %>% filter(!is.na(lay_date_numeric))
    n_count <- nrow(valid_data)
    mean_ord <- mean(valid_data$lay_date_numeric, na.rm = TRUE)
    val_display <- if(is.nan(mean_ord)) "N/A" else format(as.Date(round(mean_ord)-1, origin=str_c(current_year, "-01-01")), "%d %B")
    valueBox(val_display, paste0(current_year, " Avg Lay Date (n = ", n_count, ")"), icon = icon("calendar"), color = "green")
  })
  
  output$avg_clutch_size <- renderValueBox({
    valid_data <- filtered_initiated() %>% filter(!is.na(`Clutch Size`))
    n_count <- nrow(valid_data)
    val <- mean(valid_data$`Clutch Size`, na.rm = TRUE)
    val_display <- if(is.nan(val)) "N/A" else round(val, 1)
    valueBox(val_display, paste0(current_year, " Avg Clutch Size (n = ", n_count, ")"), icon = icon("egg"), color = "purple")
  })
  
  output$total_fledge <- renderValueBox({
    valid_data <- filtered_initiated() %>% filter(!is.na(`Number Fledged`))
    n_count <- nrow(valid_data)
    total_val <- sum(valid_data$`Number Fledged`, na.rm = TRUE)
    valueBox(if(n_count == 0) "0" else total_val, paste0(current_year, " Total Fledglings (n = ", n_count, ")"), icon = icon("dove"), color = "yellow")
  })
  
  output$hist_avg_lay <- renderValueBox({
    valid_data <- filtered_historic() %>% filter(!is.na(lay_date_historic))
    n_count <- nrow(valid_data)
    hist_ord <- mean(valid_data$lay_date_historic, na.rm = TRUE)
    val_display <- if(n_count == 0) "N/A" else format(as.Date(round(hist_ord)-1, origin=str_c(current_year, "-01-01")), "%d %B")
    valueBox(val_display, paste0("Historic Avg Lay Date (n = ", n_count, ")"), icon = icon("history"), color = "teal")
  })
  
  output$hist_avg_cs <- renderValueBox({
    valid_data <- filtered_historic() %>% filter(!is.na(cs))
    n_count <- nrow(valid_data)
    val <- mean(valid_data$cs, na.rm = TRUE)
    val_display <- if(n_count == 0) "N/A" else round(val, 1)
    valueBox(val_display, paste0("Historic Avg Clutch Size (n = ", n_count, ")"), icon = icon("history"), color = "maroon")
  })
  
  output$bird_table <- renderDT({
    display_df <- filtered_all_boxes() %>% 
      select(Region, Site, Box, Species, `Lay Date`, `Clutch Size`, `Hatch Date`, `Brood Size`, `Number Fledged`) %>%
      mutate(across(everything(), ~replace_na(as.character(.x), "")))
    datatable(display_df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  output$nest_map <- renderLeaflet({
    map_df <- filtered_all_boxes() %>% filter(!is.na(lat) & !is.na(lon))
    if(nrow(map_df) == 0) return(NULL)
    
    leaflet(map_df) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 7,
        color = "white", weight = 1,
        fillColor = ~if_else(!is.na(n1), "#2196F3", "#F44336"),
        fillOpacity = 0.8,
        popup = ~paste0(
          "<b>Box ID:</b> ", sitebox, "<br>",
          "<b>Status:</b> ", if_else(!is.na(n1), "Occupied", "Empty"), "<br>",
          "<b>Species:</b> ", replace_na(as.character(Species), ""), "<br>",
          "<b>Lay Date:</b> ", replace_na(as.character(`Lay Date`), ""), "<br>",
          "<b>Clutch Size:</b> ", replace_na(as.character(`Clutch Size`), ""), "<br>",
          "<b>Hatch Date:</b> ", replace_na(as.character(`Hatch Date`), ""), "<br>",
          "<b>Brood Size:</b> ", replace_na(as.character(`Brood Size`), ""), "<br>",
          "<b>Number Fledged:</b> ", replace_na(as.character(`Number Fledged`), "")
        ),
        label = ~sitebox 
      ) %>%
      addLayersControl(
        baseGroups = c("Satellite", "Street Map"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

# ---- run app ----
shinyApp(ui, server)