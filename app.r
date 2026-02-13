library(shiny)
library(bslib)
library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(pool) #

# --- 1. PERSISTENT DATABASE POOL ---
db_pool <- dbPool(
  RPostgres::Postgres(),
  dbname   = 'postgres',
  host     = 'aws-1-ap-south-1.pooler.supabase.com',
  port     = 5432,
  user     = 'postgres.wwyjvsakzaiwstwqyxqk',
  password = 'Anrw1024098#'
)
onStop(function() {
  poolClose(db_pool)
})

# --- 2. UI STRUCTURE ---
ui_main_app <- function(role) {
  tagList(
    useShinyjs(),
    navbarPage(
      title = "ProTrack Manufacturing",
      theme = bs_theme(bootswatch = "flatly", primary = "#2c3e50"),
    
    
    # DASHBOARD PAGE
    tabPanel("Dashboard",
      fluidRow(
        column(3, uiOutput("dash_total_cases")),
        column(3, uiOutput("dash_avg_ne")),
        column(3, uiOutput("dash_avg_te")),
        column(3, uiOutput("dash_active_lines"))
      ),
      hr(),
      fluidRow(
        column(8, plotOutput("daily_trend_plot")),
        column(4, h4("Last Hour Status"), DTOutput("status_table_mini"))
      )
    ),
    
    # HOURLY ENTRY PAGE
    tabPanel("Hourly Entry",
      sidebarLayout(
        sidebarPanel(
          h4("Shift Header"),
          selectInput("in_line", "Line:", choices = c("Line-1", "Line-2", "Line-3", "Line-4", "Line-5", "Line-6")),
          selectInput("in_shift", "Shift:", choices = c("A", "B", "C")),
          dateInput("in_date", "Date:", value = Sys.Date()),
          selectInput("in_eng", "Shift Engineer:", choices = NULL),
          hr(),
          h4("Production Data"),
          selectInput("in_time", "Time Duration:", choices = NULL),
          selectInput("in_sku", "SKU Name:", choices = NULL),
          numericInput("in_qty", "Actual Production (Cases):", value = 0),
          actionButton("submit_btn", "Submit Hourly Report", class = "btn-success", width = "100%")
        ),
        mainPanel(
          h4("Breakdowns in this Hour"),
          fluidRow(
            column(4, selectInput("bd_machine", "Machine", choices = NULL)), # Populated by server
            column(4, selectizeInput(
              inputId = "bd_rsn", 
              label = "Reason", 
              choices = NULL, # Populated dynamically from DB
              options = list(
                placeholder = 'Type or select reason...',
                create = TRUE,       # Allows typing a brand new reason
                maxOptions = 10,     # Keeps the list manageable
                openOnFocus = TRUE   # Shows suggestions immediately on click
              )
            )),
            column(4, selectInput("bd_tp", "Type", choices = c("Operational", "Maintenance", "ChangeOver", "Utility")))
          ),
          fluidRow(
            column(3, numericInput("bd_min", "Duration (Min)", 0, min = 0)),
            column(2, actionButton("add_bd", "+", class = "btn-info", style = "margin-top: 25px;")),
            column(7, checkboxInput("no_bd", "No Breakdown for this hour", value = FALSE))
          ),
          hr(),
          h4("Staged Breakdowns"),
          tableOutput("temp_bd_list"),
          hr(),
          uiOutput("live_calc_ui")
        )
      )
    ),
    tabPanel("Shift Data Check",
      tabsetPanel(
        tabPanel("Production & Breakdowns",
          fluidPage(
            h3("Last Shift Review (Coordinator View)"),
            wellPanel(
              fluidRow(
                column(6, 
                  actionButton("refresh_check", "Refresh Data", icon = icon("sync")),
                  actionButton("finalize_shift", "Finalize & Save All", class = "btn-success", icon = icon("check-double"))
                ),
                column(6, align = "right",
                  downloadButton("download_shift_pdf", "Download PDF Summary", class = "btn-danger")
                )
              )
            ),
            h4("1. Production Quantities"),
            DTOutput("table_shift_check"),
            hr(),
            h4("2. Breakdown Records"),
            DTOutput("table_breakdown_check")
          )
        ),
        tabPanel("Material & Utility Usage",
          fluidPage(
            h4("Shift Material Usage"),
            wellPanel(
              fluidRow(
                column(4, numericInput("mat_preform", "Preform Usage (pcs)", 0)),
                column(4, numericInput("mat_closure", "Closure Usage (pcs)", 0)),
                column(4, numericInput("mat_syrup", "Syrup Usage (Liter)", 0, step = 0.1))
              ),
              fluidRow(
                column(4, numericInput("mat_label", "Label Usage (KG)", 0, step = 0.01)),
                column(4, numericInput("mat_co2", "CO2 Usage (Kg)", 0, step = 0.1)),
                column(4, numericInput("mat_fill_err", "High/Low Fill (pcs)", 0))
              ),
              actionButton("save_materials", "Save Material Usage", class = "btn-primary")
            ),
            hr(),
            h4("Daily Power Usage (24 Hours)"),
            wellPanel(
              fluidRow(
                column(6, dateInput("power_date", "Date", value = Sys.Date())),
                column(6, numericInput("power_kwh", "Power Usage (KWH)", 0, step = 1))
              ),
              actionButton("save_power", "Save Power Record", class = "btn-warning"),
              p(style = "color: gray; margin-top: 10px;", 
                "Note: Only one power record is required per day for yield/efficiency calculation.")
            )
          )
        )
      )
    ),
    # PAGE 3: REPORTS
    tabPanel("Reports",
            fluidRow(
              column(4, dateRangeInput("rep_date", "Select Range:", start = Sys.Date()-7, end = Sys.Date())),
              column(4, selectInput("rep_line", "Line:", choices = c("All", "Line-1", "Line-2", "Line-3", "Line-4", "Line-5", "Line-6")))
            ),
            DTOutput("full_report_table")
    ),

    # CONFIGURATION PAGE
    tabPanel("Configuration",
      navlistPanel(
        # Admin Only: User Management
        if(role == "Admin") tabPanel("User Management", 
          wellPanel(
            h4("User Administration"),
            fluidRow(
              column(4, textInput("new_u_name", "Username")),
              column(4, textInput("new_u_pass", "Password")),
              column(4, selectInput("new_u_role", "Role", choices = c("Engineer", "Admin")))
            ),
            actionButton("save_user", "Create/Update User", class = "btn-success")
          ),
          DTOutput("table_users")
        ),
        tabPanel("Engineers Database",
          textInput("new_eng", "Engineer Name:"),
          actionButton("save_eng", "Add Engineer"),
          DTOutput("table_eng")
        ),
        tabPanel("Machine Database",
          wellPanel(
            h4("Manage Line Machines"),
            fluidRow(
              column(4, selectInput("m_line", "Select Line", choices = c("Line-1", "Line-2", "Line-3", "Line-4", "Line-5", "Line-6"))),
              column(6, textInput("m_name", "Machine Name (e.g., Filler, Labeller)")),
              column(2, actionButton("save_machine", "Add", class = "btn-success", style = "margin-top: 25px;"))
            ),
            hr(),
            DTOutput("table_machines")
          )
        ),
        tabPanel("SKU Database",
          wellPanel(
            h4("Manage SKU Master Data"),
            fluidRow(
              column(4, textInput("sku_n", "SKU Name")),
              column(4, numericInput("sku_v", "Volume (ml)", 250)),
              column(4, numericInput("sku_bpc", "Bottles/Case", 12))
            ),
            actionButton("save_sku", "Save SKU", class = "btn-primary")
          ),
          DTOutput("table_sku_master")
        ),
        tabPanel("Line-SKU Mapping",
          wellPanel(
            h4("Streamline Line Capacity"),
            fluidRow(
              column(4, 
                selectInput("map_line", "Select Line", 
                            choices = c("Line-1", "Line-2", "Line-3", "Line-4", "Line-5", "Line-6"))
              ),
              column(8,
                h5("Currently Mapped SKUs:"),
                uiOutput("current_mapping_badges") 
              )
            ),
            hr(),
            fluidRow(
              column(12,
                selectizeInput(
                  inputId = "map_skus",
                  label = "Select Available SKUs for this Line:",
                  choices = NULL,
                  multiple = TRUE,
                  width = "100%",
                  options = list(
                    placeholder = 'No SKUs Assigned - Select Line First',
                    plugins = list('remove_button')
                  )
                )
              )
            ),
            fluidRow(
              column(12, style = "margin-bottom: 10px;",
                actionButton("select_all_skus", "Select All", class = "btn-sm btn-default"),
                actionButton("deselect_all_skus", "Clear All", class = "btn-sm btn-default")
              )
            ),
            actionButton("save_mapping", "Update Line Mapping", class = "btn-warning"),
            actionButton("reset_all_config", "Reset All Line Configurations", 
                        class = "btn-danger", icon = icon("trash-alt"), style = "margin-left: 10px;")
          ),
          hr(),
          DTOutput("table_line_sku_view")
        ),
        tabPanel("Line Capacity (BPM)",
          wellPanel(
            h4("Record Line Speeds"),
            fluidRow(
              column(4, selectInput("cap_line", "Select Line", 
                                    choices = c("Line-1", "Line-2", "Line-3", "Line-4", "Line-5", "Line-6"))),
              column(4, selectInput("cap_sku", "Select SKU", choices = NULL)), 
              column(4, numericInput("cap_bpm", "BPM (Speed)", 0))
            ),
            actionButton("save_cap", "Update BPM", class = "btn-success")
          ),
          hr(),
          h4("Current Line Speeds per SKU"),
          DTOutput("table_bpm_view")
        ),
      )
    ),

      header = tags$div(style="position: absolute; right: 20px; top: 10px; z-index: 1000;", 
                        actionButton("logout_btn", "Logout", class="btn-sm btn-outline-danger"))
    ))
}

# The actual UI that Shiny loads initially
ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = bs_theme(bootswatch = "flatly"),
  uiOutput("page_content")
)

# --- 3. SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Reactive values for session management
  auth <- reactiveValues(logged_in = FALSE, user = NULL, role = NULL)
  refresh_users <- reactiveVal(0)
  refresh_data <- reactiveVal(0)
  refresh_cfg <- reactiveVal(0)
  is_editing_user <- reactiveVal(NULL)
  temp_bds <- reactiveVal(data.frame(Reason=character(), Min=numeric(), Type=character()))

  # --- CENTRALIZED DASHBOARD DATA (CACHED) ---
  dash_metrics <- reactive({
    refresh_data() 
    query <- "
      SELECT 
        COUNT(DISTINCT line_no) as active_lines,
        SUM(actual_qty) as total_cases,
        AVG(CASE WHEN theo_output_val > 0 THEN (actual_qty / theo_output_val) * 100 END) as avg_ne,
        AVG(CASE WHEN theo_output_val > 0 THEN (actual_qty / (theo_output_val * 0.85)) * 100 END) as avg_te
      FROM production_logs 
      WHERE prod_date = CURRENT_DATE
    "
    dbGetQuery(db_pool, query)
  }) %>% bindCache(refresh_data())

  # --- DASHBOARD OUTPUTS (Using bslib value_box) ---
  output$dash_total_cases <- renderUI({
    val <- dash_metrics()$total_cases[1]
    value_box(
      title = "Total Cases (Today)",
      value = if(is.na(val)) 0 else format(val, big.mark=","),
      showcase = icon("box"),
      theme = "primary"
    )
  })

  output$dash_avg_ne <- renderUI({
    val <- dash_metrics()$avg_ne[1]
    value_box(
      title = "Avg NE%",
      value = paste0(if(is.na(val)) 0 else round(val, 1), "%"),
      showcase = icon("chart-line"),
      theme = "info"
    )
  })

  output$dash_avg_te <- renderUI({
    val <- dash_metrics()$avg_te[1]
    value_box(
      title = "Avg TE%",
      value = paste0(if(is.na(val)) 0 else round(val, 1), "%"),
      showcase = icon("gauge-high"),
      theme = "success"
    )
  })

  output$dash_active_lines <- renderUI({
    val <- dash_metrics()$active_lines[1]
    value_box(
      title = "Active Lines",
      value = if(is.na(val)) 0 else val,
      showcase = icon("industry"),
      theme = "secondary"
    )
  })

  # --- TREND PLOT (CACHED) ---
  output$daily_trend_plot <- renderCachedPlot({
    refresh_data()
    df <- dbGetQuery(db_pool, "
      SELECT prod_date, SUM(actual_qty) as daily_total 
      FROM production_logs 
      GROUP BY prod_date ORDER BY prod_date DESC LIMIT 14
    ")
    
    ggplot(df, aes(x = as.Date(prod_date), y = daily_total)) +
      geom_area(fill = "#2c3e50", alpha = 0.2) +
      geom_line(color = "#2c3e50", size = 1) +
      theme_minimal() +
      labs(title = "14-Day Production Trend", x = "Date", y = "Total Cases")
  }, cacheKeyExpr = { list(refresh_data()) })

  # 1. LOGIN UI GATEKEEPER
  output$page_content <- renderUI({
    if (!auth$logged_in) {
      tagList(
        fluidRow(
          column(4, offset = 4, style = "margin-top: 100px;",
            wellPanel(
              h3("ProTrack Login", align = "center"),
              textInput("login_user", "Username"),
              passwordInput("login_pass", "Password"),
              actionButton("login_btn", "Login", class = "btn-primary", width = "100%")
            )
          )
        )
      )
    } else {
      ui_main_app(auth$role)
    }
  })

  # 2. LOGIN AUTHENTICATION
  observeEvent(input$login_btn, {
    user_data <- dbGetQuery(db_pool, sprintf(
          "SELECT username, role FROM app_users WHERE username = '%s' AND password = '%s'",
      input$login_user, input$login_pass
    ))

    if (nrow(user_data) == 1) {
      auth$logged_in <- TRUE
      auth$user <- user_data$username[1]
      auth$role <- user_data$role[1]
      showNotification(paste("Welcome,", auth$user), type = "message")
    } else {
      showNotification("Invalid Username or Password", type = "error")
    }
  })

  # --- LOGOUT LOGIC ---
  observeEvent(input$logout_btn, {
    removeModal()
    
    # 2. Reset local reactive states (Clean up R side)
    auth$logged_in <- FALSE
    auth$user <- NULL
    auth$role <- NULL
    
    # 3. Force a hard browser reload (Clean up JavaScript/UI side)
    # This completely kills the "dimmed" screen and clears navigation cache
    shinyjs::runjs("location.reload();") 
  })

  # 3. USER MANAGEMENT (Admin Only)
  # --- USER MANAGEMENT TABLE (ADMIN ONLY) ---
  output$table_users <- renderDT({
    req(auth$role == "Admin")
    refresh_users()
    df <- dbGetQuery(db_pool, "SELECT id, username, role FROM app_users")
    
    if(nrow(df) > 0) {
      df$Actions <- paste0(
        '<button class="btn btn-info btn-sm" onclick="Shiny.setInputValue(\'edit_user_id\', ', df$id, ', {priority: \'event\'})">Edit</button> ',
        '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'delete_user_id\', ', df$id, ', {priority: \'event\'})">Delete</button>'
      )
    }
    datatable(df, escape = FALSE, selection = 'none',
              colnames = c("ID", "Username", "Role", "Actions"))
  },server = TRUE)

  # User Edit Logic (Administrator Function)
  observeEvent(input$edit_user_id, {
    req(auth$role == "Admin")
    user <- dbGetQuery(db_pool, sprintf("SELECT * FROM app_users WHERE id = %s", input$edit_user_id))
    if(nrow(user) > 0) {
      updateTextInput(session, "new_u_name", value = user$username[1])
      updateTextInput(session, "new_u_pass", value = user$password[1])
      updateSelectInput(session, "new_u_role", selected = user$role[1])
      # Set the reactive state to track that we are in "Update" mode rather than "Create"
      is_editing_user(input$edit_user_id)
    }
  })

  output$user_admin_panel <- renderUI({
    req(auth$role == "Admin") # Restrict access to Admins
    tagList(
      wellPanel(
        h4("Create New User"),
        fluidRow(
          column(4, textInput("new_u_name", "Username")),
          column(4, textInput("new_u_pass", "Password")),
          column(4, selectInput("new_u_role", "Role", choices = c("Engineer", "Admin")))
        ),
        actionButton("save_user", "Add User", class = "btn-success")
      ),
      DTOutput("table_users")
    )
  })

  # Logic to save user
  observeEvent(input$save_user, {
    req(auth$role == "Admin", input$new_u_name, input$new_u_pass)
    
    tryCatch({
      # Use db_pool directly. The pool automatically manages the connection lifecycle.
      if (is.null(is_editing_user())) {
        # Create New User
        dbExecute(db_pool, 
                  "INSERT INTO app_users (username, password, role) VALUES ($1, $2, $3)",
                  list(input$new_u_name, input$new_u_pass, input$new_u_role))
        showNotification("New User Created", type = "message")
      } else {
        # Update Existing User
        dbExecute(db_pool, 
                  "UPDATE app_users SET username=$1, password=$2, role=$3 WHERE id=$4",
                  list(input$new_u_name, input$new_u_pass, input$new_u_role, is_editing_user()))
        showNotification("User Updated", type = "message")
        is_editing_user(NULL) # Reset editing state
      }
      
      # Clear Inputs and Refresh
      updateTextInput(session, "new_u_name", value = "")
      updateTextInput(session, "new_u_pass", value = "")
      refresh_users(refresh_users() + 1)
      
    }, error = function(e) {
      showNotification(paste("Database Error:", e$message), type = "error")
    })
  })

  # User Delete Logic (Admin Only)
  # 1. Trigger the confirmation modal
  observeEvent(input$delete_user_id, {
    req(auth$role == "Admin")
    # No database connection needed just to show the modal
    showModal(modalDialog(
      title = "Confirm User Deletion",
      "Are you sure you want to permanently delete this user? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_user_btn", "Yes, Delete User", class = "btn-danger")
      )
    ))
  })

  # 2. Execute the deletion once confirmed
  observeEvent(input$confirm_delete_user_btn, {
    req(auth$role == "Admin")
    removeModal()
    
    tryCatch({
      # Use db_pool directly. 
      # This avoids the 300-500ms SSL handshake delay required for new connections.
      dbExecute(db_pool, "DELETE FROM app_users WHERE id = $1", list(input$delete_user_id))
      
      # Trigger reactive refresh for the user table
      refresh_users(refresh_users() + 1)
      showNotification("User deleted successfully.", type = "warning")
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # (Include all your existing production and config logic here)

  # Reactive triggers
  refresh_data <- reactiveVal(0)
  refresh_cfg <- reactiveVal(0)
  temp_bds <- reactiveVal(data.frame(Reason=character(), Min=numeric(), Type=character()))

  # --- CONFIGURATION LOGIC ---
  cached_config <- reactive({
    refresh_cfg()
    list(
      engs = dbGetQuery(db_pool, "SELECT name FROM config_engineers")$name,
      skus = dbGetQuery(db_pool, "SELECT sku_name FROM config_skus")$sku_name,
      vols <- dbGetQuery(db_pool, "SELECT DISTINCT sku_volume_ml FROM config_skus")$sku_volume_ml
    )
  }) %>% bindCache(refresh_cfg())

  observe({
    refresh_cfg()
    refresh_data()
    req(auth$logged_in)
    engs <- dbGetQuery(db_pool, "SELECT name FROM config_engineers")$name
    skus <- dbGetQuery(db_pool, "SELECT sku_name FROM config_skus")$sku_name
    vols <- dbGetQuery(db_pool, "SELECT DISTINCT sku_volume_ml FROM config_skus")$sku_volume_ml
    
    updateSelectInput(session, "in_eng", 
                      choices = engs, 
                      selected = auth$user) # Defaults to the person logged in
    updateSelectInput(session, "in_sku", choices = skus)
    updateSelectInput(session, "cap_vol", choices = vols)
  })

  # --- UNIFIED LINE-SKU MAPPING LOGIC (SELECTIZE VERSION) ---
  observe({
    # Reactive dependencies
    input$map_line
    refresh_data()
    refresh_cfg()
    
    # Only proceed if authenticated and line selected
    req(auth$logged_in, input$map_line)
    
    cat("=== Updating SKU Selectize for", input$map_line, "===\n")
    
    # 1. Fetch ALL available SKUs from database
    all_skus <- tryCatch({
      skus <- dbGetQuery(db_pool, "SELECT sku_name FROM config_skus ORDER BY sku_name")$sku_name
      cat("Fetched", length(skus), "SKUs from database\n")
      skus
    }, error = function(e) {
      cat("ERROR fetching SKUs:", e$message, "\n")
      showNotification(paste("Database error:", e$message), type = "error")
      character(0)
    })
    
    # 2. Fetch SKUs currently assigned to the selected line
    current_mapping <- tryCatch({
      mapped <- dbGetQuery(db_pool, sprintf(
        "SELECT sku_name FROM config_line_sku_mapping WHERE line_no = '%s'", 
        input$map_line
      ))$sku_name
      cat("Current mapping:", length(mapped), "SKUs\n")
      mapped
    }, error = function(e) {
      cat("ERROR fetching mapping:", e$message, "\n")
      character(0)
    })
    
    # 3. Update the selectizeInput
    updateSelectizeInput(
      session = session,
      inputId = "map_skus",
      choices = all_skus,
      selected = current_mapping,
      server = TRUE
    )
    
    cat("Selectize updated with", length(all_skus), "choices,", length(current_mapping), "selected\n\n")
  })
  
  # Populate Breakdown Reason suggestions from history
  observe({
    refresh_data() # Trigger update when new data is logged
    req(auth$logged_in)
    
    # Fetch unique reasons previously entered in the system
    historical_reasons <- tryCatch({
      dbGetQuery(db_pool, "SELECT DISTINCT reason FROM breakdowns ORDER BY reason")$reason
    }, error = function(e) character(0))
    
    # Update the selectizeInput with historical data
    updateSelectizeInput(
      session = session,
      inputId = "bd_rsn",
      choices = historical_reasons,
      server = TRUE
    )
  })

  # Select All button
  observeEvent(input$select_all_skus, {
    req(auth$logged_in)
    all_skus <- dbGetQuery(db_pool, "SELECT sku_name FROM config_skus ORDER BY sku_name")$sku_name
    updateSelectizeInput(session, "map_skus", selected = all_skus)
    cat("Selected all SKUs\n")
  })
  
  # Deselect All button
  observeEvent(input$deselect_all_skus, {
    updateSelectizeInput(session, "map_skus", selected = character(0))
    cat("Cleared all selections\n")
  })

# --- CONFIG: ADD ENGINEER ---
  observeEvent(input$save_eng, {
    req(input$new_eng) # Don't run if empty
    
    tryCatch({
      dbExecute(db_pool, "INSERT INTO config_engineers (name) VALUES ($1) ON CONFLICT (name) DO NOTHING", 
                list(input$new_eng))
      
      # SUCCESS FEEDBACK
      showNotification(paste("Engineer", input$new_eng, "Added!"), type = "message")
      updateTextInput(session, "new_eng", value = "") # Clear input
      refresh_data(refresh_data() + 1)               # Trigger UI refresh
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  # --- MACHINE DATABASE LOGIC ---

  # 1. Save Machine to Database
  observeEvent(input$save_machine, {
    req(input$m_line, input$m_name)
    
    tryCatch({
      dbExecute(db_pool, 
                "INSERT INTO config_machines (line_no, machine_name) VALUES ($1, $2)",
                list(input$m_line, input$m_name))
      
      showNotification(paste("Machine Added to", input$m_line), type = "message")
      updateTextInput(session, "m_name", value = "") # Clear input after save
      refresh_cfg(refresh_cfg() + 1) # Trigger table refresh
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # 2. Render the Machine Table with Delete Functionality
  output$table_machines <- renderDT({
    refresh_cfg()
    req(input$m_line)
    # Fetch data based on filter
    query <- if(input$m_line == "All") {
      "SELECT id, line_no, machine_name FROM config_machines ORDER BY line_no, machine_name"
    } else {
      sprintf("SELECT id, line_no, machine_name FROM config_machines WHERE line_no = '%s' ORDER BY machine_name", 
              input$m_line)
    }
    
    df <- dbGetQuery(db_pool, query)
    
    if(nrow(df) > 0) {
      df$Actions <- paste0(
        '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'delete_m_id\', ', df$id, ', {priority: \'event\'})">Delete</button>'
      )
    }
    
    datatable(df, escape = FALSE, selection = 'none', 
              colnames = c("ID", "Line Number", "Machine Name", "Actions"),
              options = list(pageLength = 10))
  })

  # 3. Handle Machine Deletion
  observeEvent(input$delete_m_id, {
    showModal(modalDialog(
      title = "Confirm Deletion",
      "Are you sure you want to remove this machine from the configuration?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_m_del", "Yes, Delete", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_m_del, {
    removeModal()
    req(input$delete_m_id)
    
    tryCatch({
      dbExecute(db_pool, "DELETE FROM config_machines WHERE id = $1", list(input$delete_m_id))
      showNotification("Machine Deleted", type = "warning")
      refresh_cfg(refresh_cfg() + 1)
    }, error = function(e) {
      showNotification(e$message, type = "error")
    })
  })
  # --- CONFIG: ADD SKU ---
  observeEvent(input$save_sku, {
    req(input$sku_n, input$sku_v)
    
    tryCatch({
      dbExecute(db_pool, "INSERT INTO config_skus (sku_name, sku_volume_ml, bottles_per_case) 
                      VALUES ($1, $2, $3) 
                      ON CONFLICT (sku_name) DO UPDATE SET 
                      sku_volume_ml = EXCLUDED.sku_volume_ml, 
                      bottles_per_case = EXCLUDED.bottles_per_case", 
                list(input$sku_n, input$sku_v, input$sku_bpc))
      
      showNotification("SKU Saved/Updated Successfully!", type = "message")
      refresh_data(refresh_data() + 1)
      refresh_cfg(refresh_cfg() + 1)  # Also trigger config refresh for Line-SKU mapping
      
    }, error = function(e) {
      showNotification(paste("Database Error:", e$message), type = "error")
    })
  })

  # --- SKU MASTER TABLE ---
  output$table_sku_master <- renderDT({
    refresh_data()
    # Fetch master SKU data
    df <- dbGetQuery(db_pool, "SELECT id, sku_name, sku_volume_ml, bottles_per_case FROM config_skus ORDER BY sku_name")
    
    if(nrow(df) > 0) {
      df$Actions <- paste0(
        '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'delete_sku_id\', ', df$id, ', {priority: \'event\'})">Delete</button>'
      )
    }
    datatable(df, escape = FALSE, selection = 'none', 
              colnames = c("ID", "SKU Name", "Volume (ml)", "BPC", "Actions"))
  },server = TRUE)

  # 1. Update SKU dropdown based on selected line in the BPM tab
  observeEvent(input$cap_line, {
    # Only show SKUs mapped to this line for capacity setting
    mapped_skus <- dbGetQuery(db_pool, sprintf(
      "SELECT sku_name FROM config_line_sku_mapping WHERE line_no = '%s'", input$cap_line
    ))$sku_name
    
    updateSelectInput(session, "cap_sku", choices = mapped_skus)
  })

  # 2. Updated BPM Table View (showing Line, SKU Name, and Speed)
  output$table_bpm_view <- renderDT({
    refresh_data()
    # Fetch data directly from the capacity table
    df <- dbGetQuery(db_pool, "
      SELECT 
        id,
        line_no AS \"Line\", 
        sku_name AS \"SKU\", 
        sku_volume_ml AS \"Volume (ml)\",
        bpm AS \"Speed (BPM)\"
      FROM config_line_capacity
      ORDER BY line_no, sku_name
    ")
   
    if(nrow(df) > 0) {
      df$Actions <- paste0(
        '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'delete_bpm_id\', ', df$id, ', {priority: \'event\'})">Delete</button>'
      )
    }
    datatable(df, escape = FALSE, selection = 'none', options = list(pageLength = 10))
  }, server = TRUE)

  # 1. Confirmation Modal for Reset
  observeEvent(input$reset_all_config, {
    req(auth$role == "Admin")
    showModal(modalDialog(
      title = "Warning: Full Configuration Reset",
      "This will delete ALL SKU-to-Line assignments AND all recorded BPM speeds. 
       The SKU Database will remain intact, but lines will be unconfigured.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_full_reset", "Yes, Reset Everything", class = "btn-danger")
      )
    ))
  })

  # 2. Execution Logic (Clears both tables)
  observeEvent(input$confirm_full_reset, {
    req(auth$role == "Admin")
    removeModal()
    
    tryCatch({
      # 1. Borrow a specific connection from the pool for this multi-step operation
      con <- poolCheckout(db_pool)
      
      # 2. Start a Transaction to ensure both deletes succeed together
      dbBegin(con)
      
      # Step 1: Clear the Line-SKU Mappings
      dbExecute(con, "DELETE FROM config_line_sku_mapping")
      
      # Step 2: Clear the BPM Speeds
      dbExecute(con, "DELETE FROM config_line_capacity")
      
      # 3. Commit the changes and return the connection to the pool
      dbCommit(con)
      poolReturn(con)
      
      # Refresh all related UI tables
      refresh_data(refresh_data() + 1)
      showNotification("Line mappings and BPM settings cleared successfully.", type = "error")
      
    }, error = function(e) {
      # If an error occurs, undo any partial deletions and return the connection
      if(exists("con")) {
        dbRollback(con)
        poolReturn(con)
      }
      showNotification(paste("Error during reset:", e$message), type = "error")
    })
  })

  observeEvent(input$save_cap, {
    req(input$cap_line, input$cap_sku, input$cap_bpm)
    showNotification("Updating BPM record...", id = "bpm_load", duration = NULL)
    
    tryCatch({
      # 1. Borrow a specific connection from the pool for this sequence of operations
      con <- poolCheckout(db_pool)
      
      # 2. Fetch the volume for the selected SKU name
      sku_info <- dbGetQuery(con, sprintf(
        "SELECT sku_volume_ml FROM config_skus WHERE sku_name = '%s'", 
        input$cap_sku
      ))
      
      if(nrow(sku_info) == 0) {
        poolReturn(con) # Always return connection before stopping
        stop("SKU volume not found in database.")
      }
      target_vol <- sku_info$sku_volume_ml[1]
      
      # 3. Execute the UPSERT (Insert or Update on conflict)
      dbExecute(con, 
            "INSERT INTO config_line_capacity (line_no, sku_name, sku_volume_ml, bpm) 
            VALUES ($1, $2, $3, $4) 
            ON CONFLICT (line_no, sku_name) 
            DO UPDATE SET bpm = EXCLUDED.bpm, sku_volume_ml = EXCLUDED.sku_volume_ml", 
            list(input$cap_line, input$cap_sku, target_vol, input$cap_bpm)
      )
      
      # 4. Return the connection to the pool
      poolReturn(con)
      
      removeNotification("bpm_load")
      showNotification(paste("Speed updated for", input$cap_sku), type = "message")
      refresh_data(refresh_data() + 1)
      
    }, error = function(e) {
      if(exists("con")) poolReturn(con)
      removeNotification("bpm_load")
      showNotification(paste("Update Error:", e$message), type = "error")
    })
  })
  
  # --- HOURLY SUBMISSION ---
  # --- STEP 1: VALIDATE AND SHOW MODAL ---
  observeEvent(input$submit_btn, {
    # Requirement: Must have recorded breakdowns OR "No Breakdown" checked
    has_records <- nrow(temp_bds()) > 0
    is_no_bd <- input$no_bd
    
    if (!has_records && !is_no_bd) {
      showNotification("Error: You must record at least one breakdown or check 'No Breakdown' before submitting.", type = "error")
      return()
    }
    
    # If valid, show final summary modal
    showModal(modalDialog(
      title = "Final Review of Hourly Report",
      h4("Production Summary"),
      p(paste("Line:", input$in_line, "| SKU:", input$in_sku, "| Qty:", input$in_qty)),
      hr(),
      h4("Breakdown Summary"),
      if(is_no_bd) p("Status: No breakdowns recorded for this hour.") else tableOutput("modal_bd_table"),
      footer = tagList(
        modalButton("Back to Edit"),
        actionButton("final_confirm_btn", "Confirm & Save to Database", class = "btn-success")
      )
    ))
  })

  output$modal_bd_table <- renderTable({ temp_bds() })

  # --- STEP 2: ACTUAL DATABASE INSERTION ---
  observeEvent(input$final_confirm_btn, {
    removeModal()
    shinyjs::disable("submit_btn")
    showNotification("Saving to database...", id = "saving", duration = NULL)
    
    tryCatch({
      con <- poolCheckout(db_pool)
      dbBegin(con)
      
      # 1. Calculate Theoretical Value
      res_theo <- dbGetQuery(con, sprintf(
        "SELECT c.bpm, s.bottles_per_case FROM config_line_capacity c 
        JOIN config_skus s ON c.sku_name = s.sku_name 
        WHERE c.line_no = '%s' AND s.sku_name = '%s'", 
        input$in_line, input$in_sku
      ))
      theo_val <- if(nrow(res_theo) > 0) (res_theo$bpm[1] * 60) / res_theo$bottles_per_case[1] else 0
      
      # 2. Insert Production Log and get ID
      insert_sql <- "INSERT INTO production_logs (line_no, shift, prod_date, engineer, time_slot, sku, actual_qty, theo_output_val) 
                    VALUES ($1, $2, $3, $4, $5, $6, $7, $8) RETURNING id"
      res <- dbSendQuery(con, insert_sql)
      dbBind(res, list(input$in_line, input$in_shift, as.character(input$in_date), 
                      input$in_eng, input$in_time, input$in_sku, 
                      as.numeric(input$in_qty), theo_val))
      log_id <- dbFetch(res)$id[1]
      dbClearResult(res)
      
      # 3. Insert Breakdowns if they exist
      bds <- temp_bds()
      if(nrow(bds) > 0) {
        for(i in 1:nrow(bds)) {
          dbExecute(con, "INSERT INTO breakdowns (log_id, machine_name, reason, duration_min, bd_type) 
                          VALUES ($1, $2, $3, $4, $5)", 
                    list(log_id, bds$Machine[i], bds$Reason[i], bds$Min[i], bds$Type[i]))
        }
      }
      
      dbCommit(con)
      poolReturn(con)
      
      # UI Cleanup
      removeNotification("saving")
      showNotification("Report Successfully Logged!", type = "message")
      updateNumericInput(session, "in_qty", value = 0)
      temp_bds(data.frame(Machine=character(), Reason=character(), Min=numeric(), Type=character(), stringsAsFactors = FALSE))
      updateCheckboxInput(session, "no_bd", value = FALSE)
      refresh_data(refresh_data() + 1)
      shinyjs::enable("submit_btn")
      
    }, error = function(e) {
      if(exists("con")) { dbRollback(con); poolReturn(con) }
      removeNotification("saving")
      shinyjs::enable("submit_btn")
      showNotification(paste("Database Error:", e$message), type = "error")
    })
  })

  # --- ENTRY LOGIC ---
  # --- SHIFT-BASED TIME SELECTION ---
  observeEvent(input$in_shift, {
    shift_times <- switch(input$in_shift,
      "A" = c("07:00 - 08:00", "08:00 - 09:00", "09:00 - 10:00", "10:00 - 11:00", 
              "11:00 - 12:00", "12:00 - 13:00", "13:00 - 14:00", "14:00 - 15:00"),
      "B" = c("15:00 - 16:00", "16:00 - 17:00", "17:00 - 18:00", "18:00 - 19:00", 
              "19:00 - 20:00", "20:00 - 21:00", "21:00 - 22:00"),
      "C" = c("22:00 - 23:00", "23:00 - 00:00", "00:00 - 01:00", "01:00 - 02:00", "02:00 - 03:00", 
              "03:00 - 04:00", "04:00 - 05:00", "05:00 - 06:00", "06:00 - 07:00")
    )
    
    updateSelectInput(session, "in_time", choices = shift_times)
  })

  # --- FIXED ADD BREAKDOWN LOGIC ---
  observeEvent(input$add_bd, {
    # 1. Validation: Ensure all fields are filled before adding to list
    req(input$bd_machine, input$bd_rsn != "", input$bd_min > 0, input$bd_tp)
    
    # 2. Uncheck "No Breakdown" if a user manually adds a breakdown
    updateCheckboxInput(session, "no_bd", value = FALSE)
    
    # 3. Create the new record
    new_row <- data.frame(
      Machine = input$bd_machine, 
      Reason  = input$bd_rsn, 
      Min     = input$bd_min,
      Type    = input$bd_tp,
      stringsAsFactors = FALSE
    )
    
    # 4. Append to the existing reactive data frame
    current_list <- temp_bds()
    temp_bds(rbind(current_list, new_row))
    
    # 5. Reset inputs for the next entry
    updateTextInput(session, "bd_rsn", value = "")
    updateNumericInput(session, "bd_min", value = 0)
    
    showNotification("Breakdown added to staged list.", type = "message")
  })
  
  output$temp_bd_list <- renderTable({ temp_bds() })

  # 1. Create a debounced version of the quantity input
  qty_debounced <- reactive({ input$in_qty }) %>% debounce(500)
  # Live KPI Calculation
  output$live_calc_ui <- renderUI({
    req(input$in_line, input$in_sku, input$in_qty)
    # Fetch BPM for current selection
    res <- dbGetQuery(db_pool, sprintf(
      "SELECT c.bpm, s.bottles_per_case FROM config_line_capacity c 
       JOIN config_skus s ON c.sku_volume_ml = s.sku_volume_ml 
       WHERE c.line_no = '%s' AND s.sku_name = '%s'", input$in_line, input$in_sku
    ))
    
    if(nrow(res) == 0) return(p("Please set BPM for this SKU/Line in Config first."))
    
    theo_hr <- (res$bpm[1] * 60) / res$bottles_per_case[1]
    ne <- (qty_debounced() / theo_hr) * 100
    # Handle breakdown calculation safely
    bd_mins <- sum(temp_bds()$Min, na.rm = TRUE)
    te <- (qty_debounced() / (theo_hr * ((60 - bd_mins)/60))) * 100
    
    wellPanel(
      h4("Current Performance"),
      p(paste("Theo Output/Hr:", round(theo_hr, 0), "Cases")),
      p(paste("NE%:", round(ne, 1), "%")),
      p(paste("TE%:", round(te, 1), "%"))
    )
  }) %>% bindCache(input$in_line, input$in_sku, temp_bds())

  # 1. Display Current Mapped SKUs as Graphical Badges
output$current_mapping_badges <- renderUI({
  req(input$map_line)
  current <- dbGetQuery(db_pool, sprintf(
    "SELECT sku_name FROM config_line_sku_mapping WHERE line_no = '%s'", input$map_line
  ))$sku_name
  
  if(length(current) == 0) return(span("No SKUs currently assigned.", style="color: gray; font-style: italic;"))
  
  # Create a badge for each SKU
  lapply(current, function(sku) {
    span(sku, class = "badge rounded-pill bg-primary", style = "margin-right: 5px; padding: 8px 12px; font-size: 0.9em;")
  })
})

  # 2. Save Mapping Logic
  observeEvent(input$save_mapping, {
    req(input$map_line)
    showNotification("Updating mappings...", id = "map_load", duration = NULL)
    tryCatch({
      # 1. Borrow a specific connection from the pool for this transaction
      con <- poolCheckout(db_pool)
      # 2. Start a Transaction (Atomic operation)
      dbBegin(con)
      # 3. Clear existing mapping for this line
      dbExecute(con, "DELETE FROM config_line_sku_mapping WHERE line_no = $1", list(input$map_line))
      # Insert new mapping
      if(!is.null(input$map_skus)){
        for(s in input$map_skus){
          dbExecute(con, "INSERT INTO config_line_sku_mapping (line_no, sku_name) VALUES ($1, $2)", 
                    list(input$map_line, s))
        }
      }
      # 5. Commit changes and return connection to the pool
      dbCommit(con)
      poolReturn(con)
      
      removeNotification("map_load")
      showNotification("Line Mapping Updated Successfully", type = "message")
      refresh_data(refresh_data() + 1)
      
    }, error = function(e) {
      # If anything fails, rollback the deletion so you don't lose data
      if(exists("con")) {
        dbRollback(con)
        poolReturn(con)
      }
      removeNotification("map_load")
      showNotification(paste("Mapping Error:", e$message), type = "error")
    })
  })

  # 1. Initialize Breakdown Data with Machine and Type
  temp_bds <- reactiveVal(data.frame(Machine=character(), Reason=character(), Min=numeric(), Type=character()))
  # 3. THE STREAMLINE FILTER (For Hourly Entry Page)
  observeEvent(input$in_line, {
    req(auth$logged_in)
    m_choices <- dbGetQuery(db_pool, sprintf(
    "SELECT machine_name FROM config_machines WHERE line_no = '%s' ORDER BY machine_name", 
    input$in_line
    ))$machine_name
    updateSelectInput(session, "bd_machine", choices = m_choices)
    
    # Only fetch SKUs assigned to the selected line
    available_skus <- dbGetQuery(db_pool, sprintf(
      "SELECT sku_name FROM config_line_sku_mapping WHERE line_no = '%s'", input$in_line
    ))$sku_name
    
    if(length(available_skus) == 0) {
      updateSelectInput(session, "in_sku", choices = "No SKUs Mapped")
    } else {
      updateSelectInput(session, "in_sku", choices = available_skus)
    }
  })

  # 1. Render the Capacity Table with a Delete Button
  output$table_sku_list <- renderDT({
    refresh_data()
    # Fetch ID so we know which row to delete
    df <- dbGetQuery(db_pool, "SELECT id, line_no, sku_volume_ml, bpm FROM config_line_capacity")
    
    # Create the Action Button HTML for each row
    if(nrow(df) > 0) {
      df$Actions <- paste0(
        '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'delete_bpm_id\', ', df$id, ', {priority: \'event\'})">
          <i class="glyphicon glyphicon-trash"></i> Delete
        </button>'
      )
    }
    
    # IMPORTANT: escape = FALSE allows the HTML in the 'Actions' column to render as a button
    datatable(df, 
              escape = FALSE, 
              selection = 'none', 
              options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  },server = TRUE)

  # 2. Logic to handle the Deletion when a button is clicked
  observeEvent(input$delete_bpm_id, {
    target_id <- input$delete_bpm_id
    
    # Ask for confirmation (Optional but recommended)
    showModal(modalDialog(
      title = "Confirm Deletion",
      paste("Are you sure you want to delete capacity entry ID:", target_id, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_btn", "Yes, Delete", class = "btn-danger")
      )
    ))
  })

  # 3. Final Execution of Delete after Confirmation
  observeEvent(input$confirm_delete_btn, {
    removeModal()
    req(input$delete_bpm_id)
    target_id <- input$delete_bpm_id
    
    tryCatch({
      dbExecute(db_pool, "DELETE FROM config_line_capacity WHERE id = $1", list(target_id))
      
      showNotification("Entry deleted successfully.", type = "warning")
      refresh_data(refresh_data() + 1) # Refresh the table
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # 1. Listen for the button click and show a confirmation popup
  observeEvent(input$delete_bpm_id, {
    showModal(modalDialog(
      title = "Confirm Deletion",
      "Are you sure you want to delete this capacity entry?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_btn", "Yes, Delete", class = "btn-danger")
      )
    ))
  })

  # 2. Execute the SQL Delete after the user confirms
  observeEvent(input$confirm_delete_btn, {
    removeModal()
    target_id <- input$delete_bpm_id
    
    tryCatch({
      dbExecute(db_pool, "DELETE FROM config_line_capacity WHERE id = $1", list(target_id))
     
      showNotification("Entry Deleted", type = "warning")
      refresh_data(refresh_data() + 1) # Force table to redraw
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # 1. Save Shift Material Usage
  observeEvent(input$save_materials, {
    # We fetch the latest shift/date from the production logs to stay in sync
    last_shift <- dbGetQuery(db_pool, "SELECT prod_date, shift, line_no FROM production_logs ORDER BY id DESC LIMIT 1")
    req(nrow(last_shift) > 0)
    
    tryCatch({
      dbExecute(db_pool, 
        "INSERT INTO material_usage (prod_date, shift, line_no, preform_qty_pcs, closure_qty_pcs, syrup_qty_liter, label_qty_kg, co2_qty_kg, high_low_fill_pcs) 
        VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)",
        list(last_shift$prod_date[1], last_shift$shift[1], last_shift$line_no[1],
            input$mat_preform, input$mat_closure, input$mat_syrup, 
            input$mat_label, input$mat_co2, input$mat_fill_err)
      )
      showNotification("Material usage recorded for current shift.", type = "message")
    }, error = function(e) showNotification(e$message, type = "error"))
  })

  # 2. Save Daily Power Usage
  observeEvent(input$save_power, {
    tryCatch({
      dbExecute(db_pool, 
        "INSERT INTO power_usage (usage_date, kwh_value) VALUES ($1, $2) 
        ON CONFLICT (usage_date) DO UPDATE SET kwh_value = EXCLUDED.kwh_value",
        list(as.character(input$power_date), input$power_kwh)
      )
      showNotification("Power usage record updated for the day.", type = "message")
    }, error = function(e) showNotification(e$message, type = "error"))
  })

  # --- ENGINEER TABLE WITH DELETE ---
  output$table_eng <- renderDT({
    refresh_data()
    df <- dbGetQuery(db_pool, "SELECT id, name FROM config_engineers ORDER BY id DESC")
    
    if(nrow(df) > 0) {
      df$Actions <- paste0(
        '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'delete_eng_id\', ', df$id, ', {priority: \'event\'})">Delete</button>'
      )
    }
    datatable(df, escape = FALSE, selection = 'none')
  },server = TRUE)

  # Engineer Delete Observer
  observeEvent(input$delete_eng_id, {
    showModal(modalDialog(
      title = "Confirm Deletion",
      "Delete this engineer from the configuration?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_eng", "Yes, Delete", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_delete_eng, {
    removeModal()
    tryCatch({
      dbExecute(db_pool, "DELETE FROM config_engineers WHERE id = $1", list(input$delete_eng_id))
      showNotification("Engineer Deleted", type = "warning")
      refresh_data(refresh_data() + 1)
    }, error = function(e) { showNotification(e$message, type = "error") })
  })

  # --- SKU TABLE WITH DELETE ---
  output$table_sku_list <- renderDT({
    refresh_data()
    df <- dbGetQuery(db_pool, "SELECT id, sku_name, sku_volume_ml, bottles_per_case FROM config_skus ORDER BY sku_name")
    
    if(nrow(df) > 0) {
      df$Actions <- paste0(
        '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'delete_sku_id\', ', df$id, ', {priority: \'event\'})">Delete</button>'
      )
    }
    datatable(df, escape = FALSE, selection = 'none')
  },server = TRUE)

  # --- DISPLAY LINE-WISE SKU MAPPING ---
  output$table_line_sku_view <- renderDT({
    refresh_data() # Table refreshes whenever you save a new mapping
    # Fetch the mapping joined with SKU details for a better view
    df <- dbGetQuery(db_pool, "
      SELECT 
        m.line_no AS \"Line\", 
        m.sku_name AS \"SKU Name\", 
        s.sku_volume_ml AS \"Volume (ml)\"
      FROM config_line_sku_mapping m
      JOIN config_skus s ON m.sku_name = s.sku_name
      ORDER BY m.line_no, s.sku_volume_ml
    ")
    
    datatable(df, 
              filter = 'top', # Adds search boxes per column
              options = list(pageLength = 10, autoWidth = TRUE),
              rownames = FALSE)
  },server = TRUE)

  # SKU Delete Observer
  observeEvent(input$delete_sku_id, {
    showModal(modalDialog(
      title = "Confirm Deletion",
      "Are you sure? This may affect existing Line Capacity mappings.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_sku", "Yes, Delete", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_delete_sku, {
    removeModal()
    tryCatch({
      dbExecute(db_pool, "DELETE FROM config_skus WHERE id = $1", list(input$delete_sku_id))
      showNotification("SKU Deleted", type = "warning")
      refresh_data(refresh_data() + 1)
      refresh_cfg(refresh_cfg() + 1)  # Also trigger config refresh
    }, error = function(e) { 
      showNotification("Cannot delete SKU: It is currently mapped to a Line Capacity entry.", type = "error") 
    })
  })

  # --- SHIFT DATA CHECK SERVER LOGIC ---

  # 1. Reactive container for the editable data
  shift_check_data <- reactiveVal(NULL)

  # 2. Fetch data for the 'Last Shift'
  observe({
    req(auth$logged_in)
    input$refresh_check # Trigger on button press
    
    # This query identifies the most recent date/shift combination in the logs
    query <- "
      WITH last_s AS (
        SELECT prod_date, shift FROM production_logs 
        ORDER BY prod_date DESC, shift DESC LIMIT 1
      )
      SELECT id, line_no, shift, prod_date, time_slot, sku, actual_qty 
      FROM production_logs 
      WHERE (prod_date, shift) IN (SELECT prod_date, shift FROM last_s)
      ORDER BY line_no, time_slot
    "
    df <- dbGetQuery(db_pool, query)
    shift_check_data(df)
  })

  # 3. Render the Editable Table
  output$table_shift_check <- renderDT({
    datatable(
      shift_check_data(),
      editable = list(target = "cell", disable = list(columns = c(0:5))), # Only allow editing qty
      selection = 'none',
      options = list(pageLength = 25)
    )
  })

  # 4. Handle User Edits
  observeEvent(input$table_shift_check_cell_edit, {
    info <- input$table_shift_check_cell_edit
    df <- shift_check_data()
    
    # Update the reactive value with the new entry
    df[info$row, info$col] <- as.numeric(info$value)
    shift_check_data(df)
  })

  # 5. Finalize Changes to Database
  observeEvent(input$finalize_shift, {
    req(shift_check_data())
    df <- shift_check_data()
    
    showModal(modalDialog(
      title = "Confirm Updates",
      "This will update the production quantities in the database. Continue?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_shift_update", "Confirm Update", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$confirm_shift_update, {
    removeModal()
    df <- shift_check_data()
    
    tryCatch({
      con <- poolCheckout(db_pool)
      dbBegin(con)
      
      # Loop through all rows and update actual_qty by ID
      for(i in 1:nrow(df)) {
        dbExecute(con, 
                  "UPDATE production_logs SET actual_qty = $1 WHERE id = $2",
                  list(df$actual_qty[i], df$id[i]))
      }
      
      dbCommit(con)
      poolReturn(con)
      showNotification("Shift data finalized and updated successfully.", type = "message")
      refresh_data(refresh_data() + 1) # Update dashboard and reports
      
    }, error = function(e) {
      if(exists("con")) { dbRollback(con); poolReturn(con) }
      showNotification(paste("Update Error:", e$message), type = "error")
    })
  })
  # Reactive container for breakdowns
  shift_bd_data <- reactiveVal(NULL)

  # Fetch Breakdowns for the Last Shift
  observe({
    req(auth$logged_in)
    input$refresh_check
    
    query <- "
      SELECT b.id, p.line_no, b.machine_name, b.reason, b.duration_min, b.bd_type
      FROM breakdowns b
      JOIN production_logs p ON b.log_id = p.id
      WHERE (p.prod_date, p.shift) IN (
        SELECT prod_date, shift FROM production_logs ORDER BY prod_date DESC, shift DESC LIMIT 1
      )
    "
    shift_bd_data(dbGetQuery(db_pool, query))
  })

  # Render Editable Breakdown Table
  output$table_breakdown_check <- renderDT({
    datatable(shift_bd_data(), 
              editable = list(target = "cell", disable = list(columns = c(0, 1))), 
              selection = 'none')
  })

  # Handle Breakdown Edits
  observeEvent(input$table_breakdown_check_cell_edit, {
    info <- input$table_breakdown_check_cell_edit
    df <- shift_bd_data()
    df[info$row, info$col] <- info$value
    shift_bd_data(df)
  })
  output$download_shift_pdf <- downloadHandler(
    filename = function() {
      paste0("Shift_Report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # 1. Create a notification
      id <- showNotification("Generating PDF...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)

      # 2. Copy template to a temp directory to avoid permission issues
      tempReport <- file.path(tempdir(), "shift_report.Rmd")
      file.copy("shift_report.Rmd", tempReport, overwrite = TRUE)

      # 3. Prepare clean data frames (remove any non-serializable objects)
      p_data <- as.data.frame(shift_check_data())
      b_data <- as.data.frame(shift_bd_data())

      # 4. Render the report
      tryCatch({
        rmarkdown::render(tempReport, output_file = file,
                          params = list(prod = p_data, bds = b_data),
                          envir = new.env(parent = globalenv()))
      }, error = function(e) {
        showNotification(paste("PDF Error:", e$message), type = "error", duration = 10)
      })
    }
  )

  observeEvent(input$confirm_shift_update, {
    removeModal()
    df_p <- shift_check_data()
    df_b <- shift_bd_data()
    
    tryCatch({
      con <- poolCheckout(db_pool)
      dbBegin(con)
      
      # Update Production Logs
      for(i in 1:nrow(df_p)) {
        dbExecute(con, "UPDATE production_logs SET actual_qty = $1 WHERE id = $2",
                  list(df_p$actual_qty[i], df_p$id[i]))
      }
      
      # Update Breakdowns
      for(i in 1:nrow(df_b)) {
        dbExecute(con, "UPDATE breakdowns SET machine_name = $1, reason = $2, duration_min = $3, bd_type = $4 WHERE id = $5",
                  list(df_b$machine_name[i], df_b$reason[i], df_b$duration_min[i], df_b$bd_type[i], df_b$id[i]))
      }
      
      dbCommit(con)
      poolReturn(con)
      showNotification("All shift data updated successfully.", type = "message")
    }, error = function(e) {
      if(exists("con")) { dbRollback(con); poolReturn(con) }
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
}
shinyApp(ui, server)

