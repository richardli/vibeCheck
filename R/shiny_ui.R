#' Create the Shiny UI for Documentation Assistant
#'
#' Creates the user interface for the R Documentation Assistant Shiny application.
#'
#' @return Shiny UI object
#' @export
create_doc_ui <- function() {
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "R Package Documentation Assistant"),
    
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Functions", tabName = "functions", icon = shiny::icon("code")),
        shinydashboard::menuItem("Dependencies", tabName = "dependencies", icon = shiny::icon("puzzle-piece")),
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
        "))
      ),
      
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "functions",
          shiny::fluidRow(
            shinydashboard::box(title = "Functions", status = "primary", solidHeader = TRUE, width = 12,
              DT::dataTableOutput("functions_table")
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Function Code", status = "info", solidHeader = TRUE, width = 6,
              shinyAce::aceEditor("function_code", 
                       mode = "r", 
                       theme = "github",
                       height = "400px",
                       readOnly = TRUE,
                       fontSize = 12)
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
                shiny::actionButton("auto_template", "Auto Template", 
                            class = "btn-info", icon = shiny::icon("magic"))
              )
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Parameter Suggestions", status = "success", solidHeader = TRUE, width = 6,
              shiny::verbatimTextOutput("param_suggestions")
            ),
            
            shinydashboard::box(title = "Function Dependencies", status = "primary", solidHeader = TRUE, width = 6,
              shiny::verbatimTextOutput("function_dependencies")
            )
          )
        ),
        
        shinydashboard::tabItem(tabName = "dependencies",
          shiny::fluidRow(
            shinydashboard::box(title = "Package Dependencies Overview", status = "primary", solidHeader = TRUE, width = 12,
              shiny::verbatimTextOutput("dependencies_summary")
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Missing/Uninstalled Packages", status = "danger", solidHeader = TRUE, width = 6,
              shiny::verbatimTextOutput("missing_packages"),
              shiny::br(),
              shiny::div(style = "text-align: center;",
                shiny::actionButton("install_missing", "Install Missing Packages", 
                            class = "btn-danger", icon = shiny::icon("download"))
              )
            ),
            
            shinydashboard::box(title = "DESCRIPTION File Analysis", status = "warning", solidHeader = TRUE, width = 6,
              shiny::verbatimTextOutput("description_analysis")
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Dependency Details by Detection Method", status = "info", solidHeader = TRUE, width = 12,
              shiny::tabsetPanel(
                shiny::tabPanel("Library/Require Calls", shiny::verbatimTextOutput("library_calls")),
                shiny::tabPanel("Namespace Calls (::)", shiny::verbatimTextOutput("namespace_calls")), 
                shiny::tabPanel("Suspected Usage", shiny::verbatimTextOutput("suspected_usage")),
                shiny::tabPanel("Roxygen Imports", shiny::verbatimTextOutput("roxygen_imports")),
                shiny::tabPanel("Function-Level Details", DT::dataTableOutput("function_deps_table"))
              )
            )
          )
        ),
        
        shinydashboard::tabItem(tabName = "settings",
          shiny::fluidRow(
            shinydashboard::box(title = "Settings", status = "primary", solidHeader = TRUE, width = 12,
              shiny::h4("Package Statistics"),
              shiny::verbatimTextOutput("detailed_stats"),
              shiny::br(),
              shiny::h4("Parameter History"),
              shiny::verbatimTextOutput("param_history_display")
            )
          )
        )
      )
    )
  )
}
