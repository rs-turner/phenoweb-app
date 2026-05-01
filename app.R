# ---- Auto-install and load required libraries ----
required_packages <- c("shiny", "shinydashboard", "readxl", "tidyverse", "DT", "leaflet", "viridis", "shinyBS")
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
library(shinyBS) 

# ---- Global Settings & Metadata ----
current_year <- as.numeric(format(Sys.Date(), "%Y"))

task_levels <- c("N1 Check", "NL Check", "First Egg Check", 
                 "First Incubation Check", "Confirm Incubation Check",
                 "Hatch Check", "V1", "Catch Male", "Catch Female", "V2", "Fledge Check")

task_colors <- c(
  "N1 Check"                 = "#E41A1C", 
  "NL Check"                 = "#377EB8", 
  "First Egg Check"          = "#4DAF4A", 
  "First Incubation Check"   = "#984EA3", 
  "Confirm Incubation Check" = "#FF7F00", 
  "Hatch Check"              = "#FDB462", 
  "V1"                       = "#A65628", 
  "Catch Male"               = "#F781BF", 
  "Catch Female"             = "#999999", 
  "V2"                       = "#66C2A5", 
  "Fledge Check"             = "#8DA0CB"  
)

# ---- Utility Functions for Data Access ----
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
    mutate(sitebox = str_c(site, box, sep = " "), 
           region = region_name)
}

# ---- User Interface (UI) ----
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(paste("Phenoweb"), 
                     style = "text-align: left; padding-left: 1px; width: 100%; font-weight: bold;"),
    titleWidth = 150, 
    tags$li(class = "dropdown", 
            actionButton("refresh_data", "Refresh Data", icon = icon("sync"), 
                         style = "margin-top: 8px; margin-right: 10px;"))
  ),
  dashboardSidebar(
    width = 150, 
    sidebarMenu(
      menuItem("Birds", tabName = "bird_data", icon = icon("dove")),
      menuItem("Trees", tabName = "tree_data", icon = icon("tree"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        text-align: left !important;
        padding: 0 0 0 15px !important;
      }
      .planner-sidebar {
        font-size: 11px;
        padding: 5px !important; 
      }
      .planner-sidebar h4 {
        font-size: 14px;
        font-weight: bold;
        margin-top: 1px;
        margin-bottom: 1px;
      }
      .planner-sidebar .control-label {
        font-size: 11px;
        margin-bottom: 4px;
      }
      .planner-sidebar .checkbox label {
        font-size: 11px;
      }
      .planner-sidebar .form-group {
        margin-bottom: 4px;
      }
      .tight-row > .col-sm-2 {
        padding-right: 1px !important;
      }
      .tight-row > .col-sm-10 {
        padding-left: 1px !important;
      }
    '))),
    
    tabItems(
      # ---- APP 1: BIRD DATA ----
      tabItem(tabName = "bird_data",
              fluidRow(
                box(title = "Filters", width = 12, status = "primary", solidHeader = TRUE,
                    column(4, selectInput("region_filter", "Region:", choices = "All")),
                    column(4, selectInput("site_filter", "Site:", choices = "All")),
                    column(4, selectInput("species_filter", "Species:", choices = "All"))
                )
              ),
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
                       
                       # Day Planner with internal Widget Filters
                       tabPanel("Day Planner", icon = icon("calendar-check"), 
                                fluidRow(class = "tight-row",                            column(width = 2,
                                         wellPanel(class = "planner-sidebar", # Added class here
                                                   h4(""),
                                                   numericInput("task_date", "Ordinal Day:", value = as.numeric(format(Sys.Date(), "%j")), min = 91, max = 183, width = "100px"),                                                   checkboxGroupInput("task_types", "Tasks to Show:", 
                                                                      choices = task_levels, 
                                                                      selected = task_levels[1:11])
                                         )
                                  ),
                                  column(width = 10,
                                         plotOutput("task_plot", height = "650px")
                                  )
                                )
                       ),
                       
                       tabPanel("Find My Bird", icon = icon("search"), DTOutput("ringing_table")),
                       tabPanel("Map View", icon = icon("map-marker-alt"), leafletOutput("nest_map", height = 600))
                )
              )
      ),
      
      # ---- APP 2: TREE DATA ----
      tabItem(tabName = "tree_data",
              fluidRow(
                box(title = "Tree Phenology", width = 12, status = "success",
                    p("Coming soon (probably).")
                )
              )
      )
    ),
    
    bsModal(id = "history_modal", title = "Nest Box History", trigger = "none", size = "large",
            DTOutput("history_table"))
  )
)

# ---- Server Logic ----
server <- function(input, output, session) {
  
  vals <- reactiveValues(master_current = NULL, historic = NULL, ringing_data = NULL, coords = NULL, adult_rings = NULL)
  
  load_all_data <- function() {
    withProgress(message = 'Syncing Dropbox...', value = 0.5, {
      
      south_file <- str_c("Bird_Phenology_", current_year, "_south.xlsx")
      north_file <- str_c("Bird_Phenology_", current_year, "_north.xlsx")
      
      raw_adults <- read_csv(find_file_in_dropbox("Adults.csv"), show_col_types = FALSE)
      
      vals$adult_rings <- raw_adults %>%
        filter(!is.na(sex)) %>%
        group_by(year, site, box, sex) %>%
        summarise(Ring = paste(unique(ring), collapse = ", "), .groups = "drop") %>%
        pivot_wider(names_from = sex, values_from = Ring, names_prefix = "Parent_") %>%
        rename(Male = Parent_M, Female = Parent_F) %>%
        mutate(box = as.character(box))
      
      current <- bind_rows(
        load_bird_data(find_file_in_dropbox(south_file), "south"),
        load_bird_data(find_file_in_dropbox(north_file), "north")
      ) %>%
        mutate(
          Region = str_to_title(region), Site = site, Box = as.character(box), 
          Species = case_match(species, "bluti" ~ "Blue Tit", "coati" ~ "Coal Tit", "greti" ~ "Great Tit", .default = species),
          `Lay Date` = if_else(is.na(latest_fed), "", format(as.Date(round(latest_fed), origin = str_c(current_year - 1, "-12-31")), "%d %B %Y")),
          `Hatch Date` = if_else(is.na(`day hatching first observed`), "", format(as.Date(round(`day hatching first observed`), origin = str_c(current_year - 1, "-12-31")), "%d %B %Y")),
          `Clutch Size` = as.numeric(cs), `Brood Size` = as.numeric(v1alive), `Number Fledged` = as.numeric(suc),
          lay_date_numeric = latest_fed,
          Male = NA_character_, 
          Female = NA_character_
        )
      
      site_region_map <- current %>% select(Site, Region) %>% distinct()
      
      if (is.null(vals$coords)) {
        vals$coords <- read_csv(find_file_in_dropbox("nest_coords.csv"), show_col_types = FALSE) %>%
          mutate(Box = as.character(nest_number)) %>% select(site, Box, lon, lat)
      }
      
      if (is.null(vals$historic)) {
        vals$historic <- read_csv(find_file_in_dropbox("Bird_Phenology.csv"), show_col_types = FALSE) %>%
          mutate(Site = site, Species = case_match(species, "bluti" ~ "Blue Tit", "coati" ~ "Coal Tit", "greti" ~ "Great Tit", .default = species),
                 lay_date_historic = coalesce(fed, latestfed),
                 box = as.character(box)) %>%
          left_join(vals$adult_rings, by = c("year", "site", "box")) %>%
          left_join(site_region_map, by = "Site") %>%
          mutate(Region = replace_na(Region, "Unknown"))
      }
      
      if (is.null(vals$ringing_data)) {
        adults <- raw_adults %>%
          transmute(Ring = ring, Date = as.Date(round(date), origin = str_c(year - 1, "-12-31")),
                    Site = site, Box = as.character(box), 
                    Status = "Adult",
                    `Age Code` = as.character(age),
                    Sex = case_match(sex, "M" ~ "Male", "F" ~ "Female", .default = sex), 
                    Wing = wing, Weight = mass, Ringer = ringer, Species = "Blue Tit", Fledged = NA_character_)
        
        nestlings <- read_csv(find_file_in_dropbox("Nestlings.csv"), show_col_types = FALSE) %>%
          mutate(fledged = if_else(fledged == 1, "Yes", "No")) %>%
          filter(ring != "unringed") %>%
          transmute(Ring = ring, Date = as.Date(round(v1date), origin = str_c(year - 1, "-12-31")),
                    Site = site, Box = as.character(box), 
                    Status = "Pullus",
                    `Age Code` = "1",
                    Sex = NA_character_, 
                    Wing = v2wing, Weight = v2mass, Ringer = v1ringer, Species = "Blue Tit", Fledged = fledged)
        
        vals$ringing_data <- bind_rows(adults, nestlings) %>%
          left_join(site_region_map, by = "Site") %>%
          mutate(Region = replace_na(Region, "Unknown")) %>%
          select(Region, Site, Box, Species, Date, Ring, Status, `Age Code`, Sex, Wing, Weight, Fledged, Ringer)
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
  
  output$task_plot <- renderPlot({
    req(filtered_all(), input$task_date, length(input$task_types) > 0)
    
    tasks_df <- filtered_all() %>%
      mutate(
        trip_type = case_match(region,
                               "south" ~ if_else(input$task_date %% 2 == 0, "D", "A"),
                               "north" ~ if_else(input$task_date %% 2 == 0, "B", "C"),
                               .default = NA_character_)
      ) %>%
      filter(trip == trip_type, is.na(suc) | (as.numeric(suc) != 0)) %>%
      mutate(
        "N1 Check"                 = is.na(n1),
        "NL Check"                 = !is.na(n1) & is.na(nl),
        "First Egg Check"          = !is.na(nl) & is.na(eggsfirstrecorded),
        "First Incubation Check"   = !is.na(eggsfirstrecorded) & is.na(fki),
        "Confirm Incubation Check" = !is.na(fki) & (input$task_date == (fki + 2) | (input$task_date > (fki + 2) & is.na(cs))),
        "Hatch Check"              = (fki + 10) <= input$task_date & is.na(`day hatching first observed`),
        "V1"                       = (`day hatching first observed` + 6) <= input$task_date & is.na(v1alive),
        "Fledge Check"             = (`day hatching first observed` + 18) <= input$task_date & is.na(suc) & (v2alive > 0 | is.na(v2alive)),
        "Catch Male"               = (v1date + 4) <= input$task_date & is.na(male) & v1alive > 0 & is.na(suc),
        "Catch Female"             = (v1date + 4) <= input$task_date & is.na(female) & v1alive > 0 & is.na(suc),
        "V2"                       = (v1date + 6) <= input$task_date & is.na(v2alive) & v1alive > 0 & is.na(suc)
      ) %>%
      pivot_longer(cols = any_of(task_levels), names_to = "Task", values_to = "is_due") %>%
      filter(is_due == TRUE, Task %in% input$task_types)
    
    validate(need(nrow(tasks_df) > 0, "No tasks due for the selected criteria."))
    
    tasks_df <- tasks_df %>%
      mutate(Task = factor(Task, levels = task_levels),
             sitebox = reorder(sitebox, `N to S`))
    
    current_limits <- task_levels[task_levels %in% input$task_types]
    
    ggplot(tasks_df, aes(sitebox, Task, fill = Task)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_y_discrete(limits = rev(current_limits)) + 
      scale_fill_manual(values = task_colors, drop = FALSE) + 
      labs(title = paste("Day Planner - Day", input$task_date), x = "", y = "") +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
        legend.position = "none",
        panel.grid.major.y = element_line(color = "#f5f5f5")
      )
  })
  
  output$nests_initiated <- renderValueBox({
    n_init <- nrow(filtered_init()); total <- nrow(filtered_all())
    perc <- if(total > 0) round((n_init / total) * 100, 1) else 0
    valueBox(paste0(n_init, " (", perc, "%)"), "Nests Initiated", icon = icon("feather-alt"), color = "blue")
  })
  
  output$avg_lay_date <- renderValueBox({
    valid <- filtered_init() %>% filter(!is.na(lay_date_numeric))
    mean_val <- mean(valid$lay_date_numeric, na.rm = TRUE)
    display <- if(is.nan(mean_val)) "N/A" else format(as.Date(round(mean_val), origin = str_c(current_year - 1, "-12-31")), "%d %B")
    valueBox(display, paste0(current_year, " Avg Lay Date (n = ", nrow(valid), ")"), icon = icon("calendar"), color = "green")
  })
  
  output$avg_clutch_size <- renderValueBox({
    valid <- filtered_init() %>% filter(!is.na(`Clutch Size`))
    val <- mean(valid$`Clutch Size`, na.rm = TRUE)
    valueBox(if(is.nan(val)) "N/A" else round(val, 1), paste0(current_year, " Avg Clutch Size (n = ", nrow(valid), ")"), icon = icon("egg"), color = "purple")
  })
  
  output$total_fledge <- renderValueBox({
    valid <- filtered_init() %>% filter(!is.na(`Number Fledged`))
    total_val <- sum(valid$`Number Fledged`, na.rm = TRUE)
    valueBox(total_val, paste0(current_year, " Total Fledglings (n = ", nrow(valid), ")"), icon = icon("dove"), color = "yellow")
  })
  
  output$hist_avg_lay <- renderValueBox({
    valid <- filtered_hist() %>% filter(!is.na(lay_date_historic))
    hist_ord <- mean(valid$lay_date_historic, na.rm = TRUE)
    display <- if(is.nan(hist_ord)) "N/A" else format(as.Date(round(hist_ord), origin = str_c(current_year - 1, "-12-31")), "%d %B")
    valueBox(display, paste0("Historic Avg Lay Date (n = ", nrow(valid), ")"), icon = icon("history"), color = "teal")
  })
  
  output$hist_avg_cs <- renderValueBox({
    valid <- filtered_hist() %>% filter(!is.na(cs))
    val <- mean(as.numeric(valid$cs), na.rm = TRUE)
    valueBox(if(is.nan(val)) "N/A" else round(val, 1), paste0("Historic Avg Clutch Size (n = ", nrow(valid), ")"), icon = icon("history"), color = "maroon")
  })
  
  output$bird_table <- renderDT({
    display_df <- filtered_all() %>% 
      select(Region, Site, Box, Species, Male, Female, `Lay Date`, `Clutch Size`, `Hatch Date`, `Brood Size`, `Number Fledged`) %>%
      mutate(across(everything(), ~replace_na(as.character(.x), "")))
    datatable(display_df, selection = 'single', filter = 'top', options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  observeEvent(input$bird_table_rows_selected, {
    req(vals$historic, vals$master_current)
    
    selected_row <- filtered_all()[input$bird_table_rows_selected, ]
    selected_site <- selected_row$Site
    selected_box  <- selected_row$Box
    
    current_box_data <- selected_row %>%
      mutate(
        Year = as.character(current_year),
        `Lay Date` = as.character(`Lay Date`),
        `Hatch Date` = as.character(`Hatch Date`),
        `Clutch Size` = as.numeric(`Clutch Size`),
        `Brood Size` = as.numeric(`Brood Size`),
        `Number Fledged` = as.numeric(`Number Fledged`)
      ) %>%
      select(Year, Species, Male, Female, `Lay Date`, `Clutch Size`, `Hatch Date`, `Brood Size`, `Number Fledged`)
    
    history_data <- vals$historic %>%
      filter(Site == selected_site, as.character(box) == selected_box) %>%
      mutate(
        `Lay Date` = if_else(is.na(lay_date_historic), "", 
                             format(as.Date(round(lay_date_historic), origin = str_c(year - 1, "-12-31")), "%d %B %Y")),
        `Hatch Date` = if_else(is.na(hatching_first_recorded), "", 
                               format(as.Date(round(hatching_first_recorded), origin = str_c(year - 1, "-12-31")), "%d %B %Y")),
        `Clutch Size` = as.numeric(cs),
        `Brood Size` = as.numeric(v1alive),
        `Number Fledged` = as.numeric(suc),
        Year = as.character(year)
      ) %>%
      select(Year, Species, Male, Female, `Lay Date`, `Clutch Size`, `Hatch Date`, `Brood Size`, `Number Fledged`)
    
    full_history <- bind_rows(current_box_data, history_data) %>%
      arrange(desc(Year))
    
    output$history_table <- renderDT({
      datatable(full_history, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
    })
    
    toggleModal(session, "history_modal", toggle = "open")
    selectRows(dataTableProxy("bird_table"), NULL)
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
    map_df <- filtered_all() %>% filter(!is.na(lat) & !is.na(lon))
    if(nrow(map_df) == 0) return(NULL)
    
    leaflet(map_df) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat, radius = 7, color = "white", weight = 1,
        fillColor = ~if_else(!is.na(n1), "#2196F3", "#F44336"), fillOpacity = 0.8,
        popup = ~paste0("<b>Box ID:</b> ", sitebox, 
                        "<br><b>Status:</b> ", if_else(!is.na(n1), "Occupied", "Empty"), 
                        "<br><b>Species:</b> ", replace_na(as.character(Species), ""), 
                        "<br><b>Male:</b> ", replace_na(as.character(Male), ""),
                        "<br><b>Female:</b> ", replace_na(as.character(Female), ""),
                        "<br><b>Lay Date:</b> ", replace_na(as.character(`Lay Date`), ""), 
                        "<br><b>Clutch Size:</b> ", replace_na(as.character(`Clutch Size`), ""), 
                        "<br><b>Hatch Date:</b> ", replace_na(as.character(`Hatch Date`), ""), 
                        "<br><b>Brood Size:</b> ", replace_na(as.character(`Brood Size`), ""), 
                        "<br><b>Number Fledged:</b> ", replace_na(as.character(`Number Fledged`), "")),
        label = ~sitebox 
      ) %>%
      addLayersControl(baseGroups = c("Satellite", "Street Map"), options = layersControlOptions(collapsed = FALSE))
  })
}

# Launch Application
shinyApp(ui, server)