#' Create the Shiny UI for Documentation Assistant (Functional Version)
#'
#' Creates the user interface for the R Documentation Assistant Shiny application
#' updated to work with the new functional architecture.
#'
#' @return Shiny UI object
#' @export
create_doc_ui <- function() {
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "VibeCheck: R Package Assistant"),
    
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Functions", tabName = "functions", icon = shiny::icon("code")),
        shinydashboard::menuItem("Dependencies", tabName = "dependencies", icon = shiny::icon("puzzle-piece")),
        shinydashboard::menuItem("Namespace", tabName = "namespace", icon = shiny::icon("link")),
        shinydashboard::menuItem("R CMD Check", tabName = "cmdcheck", icon = shiny::icon("check-circle")),
        shinydashboard::menuItem("Settings", tabName = "settings", icon = shiny::icon("cog"))
      ),
      
      shiny::br(),
      shiny::div(style = "margin: 10px;",
        shiny::h4("Package Info"),
        shiny::textInput("package_path", "Package Path:", value = ".", width = "100%"),
        shiny::actionButton("reload_package", "Reload Package", class = "btn-primary", width = "100%"),
        shiny::br(), shiny::br(),
        shiny::verbatimTextOutput("package_summary")
      )
    ),
    
    shinydashboard::dashboardBody(
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .ace_editor { font-size: 12px !important; }
          .content-wrapper, .right-side { background-color: #f4f4f4; }
          .small-box { margin-bottom: 10px; }
        "))
      ),
      
      shinydashboard::tabItems(
        
        # Functions Tab
        shinydashboard::tabItem(tabName = "functions",
          shiny::fluidRow(
            shinydashboard::box(title = "Functions Overview", status = "primary", solidHeader = TRUE, width = 12,
              DT::dataTableOutput("functions_table")
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Function Details", status = "info", solidHeader = TRUE, width = 6,
              shinyAce::aceEditor("function_code", 
                       mode = "r", 
                       theme = "github",
                       height = "200px",
                       readOnly = TRUE,
                       fontSize = 12),
              shiny::br(),
              shiny::h5("Bulk Documentation"),
              shiny::checkboxInput("save_bulk_docs", "Save templates to files", value = FALSE),
              shiny::actionButton("generate_bulk_docs", "Generate All Missing Docs", 
                          class = "btn-info", icon = shiny::icon("magic"))
            ),
            
            shinydashboard::box(title = "Documentation Editor", status = "warning", solidHeader = TRUE, width = 6,
              shinyAce::aceEditor("docs_editor", 
                       mode = "text", 
                       theme = "github",
                       height = "350px",
                       fontSize = 12),
              shiny::br(),
              shiny::div(style = "text-align: right;",
                shiny::actionButton("save_docs", "Save Documentation", 
                            class = "btn-success", icon = shiny::icon("save")),
                shiny::actionButton("auto_template", "Generate Template", 
                            class = "btn-info", icon = shiny::icon("magic"))
              )
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Parameter Suggestions", status = "success", solidHeader = TRUE, width = 12,
              shiny::verbatimTextOutput("param_suggestions")
            )
          )
        ),
        
        # Dependencies Tab  
        shinydashboard::tabItem(tabName = "dependencies",
          shiny::fluidRow(
            shinydashboard::box(title = "Dependencies Summary", status = "primary", solidHeader = TRUE, width = 12,
              shiny::verbatimTextOutput("dependencies_summary")
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Missing Packages", status = "danger", solidHeader = TRUE, width = 12,
              shiny::verbatimTextOutput("missing_packages"),
              shiny::br(),
              shiny::div(style = "text-align: center;",
                shiny::actionButton("install_missing", "Install Missing Packages", 
                            class = "btn-danger", icon = shiny::icon("download"))
              )
            )
          )
        ),
        
        # Namespace Tab
        shinydashboard::tabItem(tabName = "namespace",
          shiny::fluidRow(
            shinydashboard::box(title = "Namespace Conversion Overview", status = "primary", solidHeader = TRUE, width = 12,
              shiny::verbatimTextOutput("namespace_summary"),
              shiny::br(),
              shiny::div(style = "text-align: center;",
                shiny::actionButton("apply_namespace", "Apply Namespace Conversion", 
                            class = "btn-primary", icon = shiny::icon("link"))
              )
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Detailed Namespace Analysis", status = "info", solidHeader = TRUE, width = 12,
              shiny::verbatimTextOutput("namespace_report")
            )
          )
        ),
        
        # R CMD Check Tab
        shinydashboard::tabItem(tabName = "cmdcheck",
          shiny::fluidRow(
            shinydashboard::box(title = "Fix Global Variables", status = "warning", solidHeader = TRUE, width = 6,
              shiny::p(shiny::textOutput("global_vars_help")),
              shiny::textAreaInput("global_vars_input", 
                          label = NULL,
                          placeholder = "Paste R CMD check output here...",
                          height = "150px",
                          width = "100%"),
              shiny::checkboxInput("preview_global_vars", "Preview only", value = TRUE),
              shiny::actionButton("fix_global_vars", "Fix Global Variables", 
                          class = "btn-warning", icon = shiny::icon("wrench"))
            ),
            
            shinydashboard::box(title = "Fix Non-ASCII Characters", status = "danger", solidHeader = TRUE, width = 6,
              shiny::p(shiny::textOutput("non_ascii_help")),
              shiny::textAreaInput("non_ascii_input", 
                          label = NULL,
                          placeholder = "Paste R CMD check output here...",
                          height = "150px",
                          width = "100%"),
              shiny::checkboxInput("preview_non_ascii", "Preview only", value = TRUE),
              shiny::actionButton("fix_non_ascii", "Fix Non-ASCII Characters", 
                          class = "btn-danger", icon = shiny::icon("wrench"))
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "R CMD Check Help", status = "info", solidHeader = TRUE, width = 12,
              shiny::h4("How to use R CMD Check fixes:"),
              shiny::tags$ol(
                shiny::tags$li("Run ", shiny::code("R CMD check"), " on your package"),
                shiny::tags$li("Copy the relevant error output"),
                shiny::tags$li("Paste it into the appropriate box above"),
                shiny::tags$li("Use 'Preview only' to see what would be changed"),
                shiny::tags$li("Uncheck 'Preview only' and click the fix button to apply changes")
              ),
              shiny::br(),
              shiny::h5("Supported fixes:"),
              shiny::tags$ul(
                shiny::tags$li(shiny::strong("Global Variables:"), " Fixes 'no visible binding for global variable' errors"),
                shiny::tags$li(shiny::strong("Non-ASCII Characters:"), " Fixes 'non-ASCII characters' errors"),
                shiny::tags$li(shiny::strong("Empty Examples:"), " Use ", shiny::code("fix_empty_examples()"), " function directly")
              )
            )
          )
        ),
        
        # Settings Tab
        shinydashboard::tabItem(tabName = "settings",
          shiny::fluidRow(
            shinydashboard::box(title = "Package Statistics", status = "primary", solidHeader = TRUE, width = 6,
              shiny::verbatimTextOutput("detailed_stats")
            ),
            
            shinydashboard::box(title = "Available Functions", status = "info", solidHeader = TRUE, width = 6,
              shiny::h5("Core Analysis Functions:"),
              shiny::tags$ul(
                shiny::tags$li(shiny::code("analyze_package()")),
                shiny::tags$li(shiny::code("scan_r_functions()")),
                shiny::tags$li(shiny::code("analyze_package_dependencies()")),
                shiny::tags$li(shiny::code("build_parameter_history()"))
              ),
              
              shiny::h5("Documentation Functions:"),
              shiny::tags$ul(
                shiny::tags$li(shiny::code("generate_roxygen_template()")),
                shiny::tags$li(shiny::code("save_function_docs()")),
                shiny::tags$li(shiny::code("generate_bulk_documentation()"))
              ),
              
              shiny::h5("R CMD Check Fixes:"),
              shiny::tags$ul(
                shiny::tags$li(shiny::code("fix_global_variables()")),
                shiny::tags$li(shiny::code("fix_non_ascii_characters()")),
                shiny::tags$li(shiny::code("fix_empty_examples()"))
              ),
              
              shiny::h5("Namespace Functions:"),
              shiny::tags$ul(
                shiny::tags$li(shiny::code("analyze_namespace_usage()")),
                shiny::tags$li(shiny::code("apply_namespace_conversion()"))
              )
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Quick Actions", status = "success", solidHeader = TRUE, width = 12,
              shiny::p("Use these functions directly in the R console for command-line usage:"),
              shiny::br(),
              shiny::div(style = "font-family: monospace; background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
                "# Complete package analysis", shiny::br(),
                "vibecheck()", shiny::br(), shiny::br(),
                
                "# Quick check without fixes", shiny::br(),
                "quick_package_check()", shiny::br(), shiny::br(),
                
                "# Individual analyses", shiny::br(),
                "pkg_info <- analyze_package()", shiny::br(),
                "deps <- analyze_package_dependencies('.')", shiny::br(),
                "namespace_opps <- analyze_namespace_usage('.')", shiny::br(), shiny::br(),
                
                "# Generate documentation", shiny::br(),
                "template <- generate_roxygen_template('my_func', list(x = NULL))", shiny::br(),
                "bulk_docs <- generate_bulk_documentation(pkg_info)"
              )
            )
          )
        )
      )
    )
  )
}

#' Launch the Documentation Assistant App (Updated)
#'
#' Launches the updated Shiny application using the functional architecture.
#'
#' @param package_path Character. Path to package (default: ".")
#' @param port Integer. Port number (default: auto-select)
#' @param host Character. Host address (default: "127.0.0.1")
#' @param launch_browser Logical. Open browser automatically (default: TRUE)
#'
#' @return Starts Shiny application
#' @export
launch_doc_app <- function(package_path = ".", port = NULL, host = "127.0.0.1", launch_browser = TRUE) {
  
  # Check required packages
  required_packages <- c("shiny", "shinyAce", "shinydashboard", "DT", "stringr", "purrr")
  missing_packages <- c()
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  if (length(missing_packages) > 0) {
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "), 
         "\nPlease install them with: install.packages(c(", 
         paste(paste0("'", missing_packages, "'"), collapse = ", "), "))")
  }
  
  # Load required libraries
  library(shiny)
  library(shinydashboard)
  library(shinyAce)
  library(DT)
  library(stringr)
  library(purrr)
  
  cat("🚀 Launching VibeCheck Documentation Assistant...\n")
  cat("Package path:", package_path, "\n\n")
  
  # Run quick check first
  cat("Running quick package check...\n")
  quick_package_check(package_path)
  
  # Create UI and server
  ui <- create_doc_ui()
  server <- create_doc_server()
  
  # Launch options
  options <- list(
    host = host,
    launch.browser = launch_browser
  )
  
  if (!is.null(port)) {
    options$port <- port
  }
  
  shiny::shinyApp(ui = ui, server = server, options = options)
}