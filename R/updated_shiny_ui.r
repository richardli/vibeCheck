#' Create Updated Shiny UI for Documentation Assistant
#'
#' Creates the user interface for the R Documentation Assistant Shiny application
#' updated to work with the current function architecture and optimized performance.
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
        shinydashboard::menuItem("Documentation", tabName = "documentation", icon = shiny::icon("file-text")),
        shinydashboard::menuItem("Package Analysis", tabName = "analysis", icon = shiny::icon("chart-bar")),
        shinydashboard::menuItem("R CMD Check", tabName = "cmdcheck", icon = shiny::icon("check-circle")),
        shinydashboard::menuItem("Settings", tabName = "settings", icon = shiny::icon("cog"))
      ),
      
      shiny::br(),
      shiny::div(style = "margin: 10px;",
        shiny::h4("Package Controls"),
        shiny::textInput("package_path", "Package Path:", value = ".", width = "100%"),
        shiny::div(style = "display: flex; gap: 5px; margin-bottom: 10px;",
          shiny::actionButton("quick_check", "Quick Check", class = "btn-warning btn-sm", style = "flex: 1;"),
          shiny::actionButton("reload_package", "Full Analysis", class = "btn-primary btn-sm", style = "flex: 1;")
        ),
        shiny::checkboxInput("fast_mode", "Fast Mode (no dependencies)", value = TRUE),
        shiny::checkboxInput("skip_dependencies", "Skip Dependencies (faster)", value = FALSE),
        shiny::br(),
        shiny::verbatimTextOutput("package_summary")
      )
    ),
    
    shinydashboard::dashboardBody(
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .ace_editor { font-size: 12px !important; }
          .content-wrapper, .right-side { background-color: #f4f4f4; }
          .small-box { margin-bottom: 10px; }
          .btn-sm { padding: 4px 8px; font-size: 12px; }
          .status-good { color: #00a65a; font-weight: bold; }
          .status-warning { color: #f39c12; font-weight: bold; }
          .status-error { color: #dd4b39; font-weight: bold; }
        "))
      ),
      
      shinydashboard::tabItems(
        
        # Functions Tab
        shinydashboard::tabItem(tabName = "functions",
          shiny::fluidRow(
            shinydashboard::box(title = "Functions Overview", status = "primary", solidHeader = TRUE, width = 12,
              DT::dataTableOutput("functions_table"),
              shiny::br(),
              shiny::div(style = "text-align: right;",
                shiny::actionButton("refresh_functions", "Refresh", class = "btn-info btn-sm", icon = shiny::icon("refresh"))
              )
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Function Code & Documentation Viewer", status = "info", solidHeader = TRUE, width = 8,
              shiny::h5("Selected Function Code:"),
              shinyAce::aceEditor("code_viewer", 
                       mode = "r", 
                       theme = "github",
                       height = "300px",
                       fontSize = 11,
                       readOnly = TRUE,
                       placeholder = "Select a function to view its code..."),
              shiny::br(),
              shiny::h5("Existing Documentation:"),
              shinyAce::aceEditor("existing_docs_viewer", 
                       mode = "text", 
                       theme = "github",
                       height = "200px",
                       fontSize = 11,
                       readOnly = TRUE,
                       placeholder = "Existing roxygen documentation will appear here...")
            ),
            
            shinydashboard::box(title = "Documentation Editor", status = "warning", solidHeader = TRUE, width = 4,
              shiny::verbatimTextOutput("selected_function_info"),
              shiny::br(),
              shiny::h5("Quick Actions"),
              shiny::div(style = "display: flex; flex-direction: column; gap: 5px;",
                shiny::actionButton("generate_template", "Generate Template", class = "btn-info btn-sm", width = "100%"),
                shiny::actionButton("save_selected_docs", "Save Documentation", class = "btn-success btn-sm", width = "100%")
              ),
              shiny::br(),
              shinyAce::aceEditor("docs_editor", 
                       mode = "text", 
                       theme = "github",
                       height = "350px",
                       fontSize = 12,
                       placeholder = "Select a function to edit documentation..."),
              shiny::br(),
              shiny::div(style = "text-align: right;",
                shiny::checkboxInput("create_backup", "Create backup", value = TRUE, width = "auto")
              )
            )
          )
        ),
        
        # Dependencies Tab  
        shinydashboard::tabItem(tabName = "dependencies",
          shiny::fluidRow(
            shinydashboard::box(title = "Quick Dependency Check", status = "primary", solidHeader = TRUE, width = 12,
              shiny::div(style = "display: flex; gap: 10px; align-items: center; margin-bottom: 10px;",
                shiny::actionButton("check_dependencies", "Check Dependencies", class = "btn-primary"),
                shiny::actionButton("detailed_dep_analysis", "Detailed Analysis", class = "btn-info"),
                shiny::checkboxInput("include_suggests", "Include Suggests", value = FALSE)
              ),
              shiny::verbatimTextOutput("dependencies_summary")
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Missing Packages", status = "danger", solidHeader = TRUE, width = 6,
              shiny::verbatimTextOutput("missing_packages"),
              shiny::br(),
              shiny::div(style = "text-align: center;",
                shiny::actionButton("install_missing", "Install Missing", class = "btn-danger", icon = shiny::icon("download"))
              )
            ),
            
            shinydashboard::box(title = "Package Usage Analysis", status = "info", solidHeader = TRUE, width = 6,
              shiny::selectInput("analyze_package", "Analyze package usage:", choices = c("Select package..." = ""), width = "100%"),
              shiny::actionButton("analyze_pkg_usage", "Analyze Usage", class = "btn-info"),
              shiny::br(), shiny::br(),
              shiny::verbatimTextOutput("package_usage_report")
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Namespace Conversion", status = "success", solidHeader = TRUE, width = 12,
              shiny::div(style = "display: flex; gap: 10px; align-items: center; margin-bottom: 10px;",
                shiny::actionButton("check_namespace", "Check Opportunities", class = "btn-info"),
                shiny::actionButton("preview_namespace", "Preview Changes", class = "btn-warning"),
                shiny::actionButton("apply_namespace", "Apply Conversion", class = "btn-success")
              ),
              shiny::verbatimTextOutput("namespace_report")
            )
          )
        ),
        
        # Documentation Tab
        shinydashboard::tabItem(tabName = "documentation",
          shiny::fluidRow(
            shinydashboard::box(title = "Bulk Documentation Generation", status = "primary", solidHeader = TRUE, width = 12,
              shiny::h4("Generate Documentation Templates"),
              shiny::div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-bottom: 15px;",
                shiny::div(
                  shiny::h5("Basic Options"),
                  shiny::checkboxInput("save_bulk_docs", "Save to files", value = FALSE),
                  shiny::checkboxInput("exported_only", "Exported functions only", value = TRUE),
                  shiny::checkboxInput("backup_files", "Create backups", value = TRUE)
                ),
                shiny::div(
                  shiny::h5("Template Type"),
                  shiny::radioButtons("template_type", NULL,
                    choices = list(
                      "Standard export" = "exported",
                      "Internal function" = "internal",
                      "Basic template" = "standard"
                    ),
                    selected = "exported"
                  )
                )
              ),
              shiny::actionButton("generate_bulk_docs", "Generate All Missing Docs", 
                        class = "btn-primary", icon = shiny::icon("magic")),
              shiny::br(), shiny::br(),
              shiny::verbatimTextOutput("bulk_docs_result")
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Custom Example Templates", status = "warning", solidHeader = TRUE, width = 12,
              shiny::h4("Add Custom Examples to Functions"),
              shiny::p("Use [FUNCNAME] as placeholder for the function name in your template."),
              shiny::textAreaInput("example_template", 
                          "Example Template:",
                          value = "data <- ...\nresult <- [FUNCNAME](data)",
                          height = "100px",
                          width = "100%"),
              shiny::div(style = "display: flex; gap: 10px; align-items: center; margin-bottom: 10px;",
                shiny::textInput("specific_functions", "Specific functions (comma-separated, leave empty for all):", 
                        placeholder = "func1, func2, func3", width = "300px"),
                shiny::checkboxInput("preview_examples", "Preview only", value = TRUE)
              ),
              shiny::actionButton("add_custom_examples", "Add Examples", class = "btn-warning", icon = shiny::icon("plus")),
              shiny::br(), shiny::br(),
              shiny::verbatimTextOutput("examples_result")
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Documentation Analysis", status = "info", solidHeader = TRUE, width = 12,
              shiny::div(style = "display: flex; gap: 10px; margin-bottom: 10px;",
                shiny::actionButton("check_missing_examples", "Check Missing Examples", class = "btn-info"),
                shiny::actionButton("check_export_status", "Check Export Status", class = "btn-info")
              ),
              shiny::verbatimTextOutput("doc_analysis_result")
            )
          )
        ),
        
        # Package Analysis Tab
        shinydashboard::tabItem(tabName = "analysis",
          shiny::fluidRow(
            shinydashboard::box(title = "Package Statistics", status = "primary", solidHeader = TRUE, width = 6,
              shiny::verbatimTextOutput("detailed_stats")
            ),
            
            shinydashboard::box(title = "Analysis Options", status = "info", solidHeader = TRUE, width = 6,
              shiny::h5("Analysis Mode"),
              shiny::radioButtons("analysis_mode", NULL,
                choices = list(
                  "Ultra Fast (basic checks only)" = "ultra_fast",
                  "Fast (optimized analysis)" = "fast", 
                  "Full (comprehensive analysis)" = "full"
                ),
                selected = "fast"
              ),
              shiny::br(),
              shiny::actionButton("run_analysis", "Run Analysis", class = "btn-primary"),
              shiny::br(), shiny::br(),
              shiny::h5("Quick Actions"),
              shiny::div(style = "display: flex; flex-direction: column; gap: 5px;",
                shiny::actionButton("extract_functions_action", "Extract Package Functions", class = "btn-info btn-sm"),
                shiny::actionButton("analyze_dependencies_action", "Analyze Dependencies", class = "btn-info btn-sm"),
                shiny::actionButton("build_param_history", "Build Parameter History", class = "btn-info btn-sm")
              )
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Analysis Results", status = "success", solidHeader = TRUE, width = 12,
              shiny::verbatimTextOutput("analysis_results")
            )
          )
        ),
        
        # R CMD Check Tab
        shinydashboard::tabItem(tabName = "cmdcheck",
          shiny::fluidRow(
            shinydashboard::box(title = "Global Variables Fix", status = "warning", solidHeader = TRUE, width = 6,
              shiny::p("Paste R CMD check output containing 'no visible binding for global variable' errors:"),
              shiny::textAreaInput("global_vars_input", 
                          label = NULL,
                          placeholder = "myfunction: no visible binding for global variable 'x'\nUndefined global functions or variables:\n  x y_var weighted_value",
                          height = "150px",
                          width = "100%"),
              shiny::div(style = "display: flex; gap: 10px; align-items: center;",
                shiny::checkboxInput("preview_global_vars", "Preview only", value = TRUE),
                shiny::actionButton("fix_global_vars", "Fix Global Variables", class = "btn-warning")
              ),
              shiny::verbatimTextOutput("global_vars_result")
            ),
            
            shinydashboard::box(title = "Custom Examples Management", status = "info", solidHeader = TRUE, width = 6,
              shiny::h5("Detect Missing Examples"),
              shiny::actionButton("detect_missing_examples", "Check Missing Examples", class = "btn-info"),
              shiny::br(), shiny::br(),
              shiny::h5("Fix Empty Examples"),
              shiny::p("Use the Documentation tab for adding custom examples to functions."),
              shiny::verbatimTextOutput("examples_check_result")
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "R CMD Check Help", status = "success", solidHeader = TRUE, width = 12,
              shiny::h4("How to use R CMD Check fixes:"),
              shiny::tags$ol(
                shiny::tags$li("Run ", shiny::code("R CMD check"), " on your package"),
                shiny::tags$li("Copy the relevant error output"),
                shiny::tags$li("Paste it into the appropriate box above"),
                shiny::tags$li("Use 'Preview only' to see what would be changed"),
                shiny::tags$li("Uncheck 'Preview only' and click the fix button to apply changes")
              ),
              shiny::br(),
              shiny::h5("Available fixes:"),
              shiny::tags$ul(
                shiny::tags$li(shiny::strong("Global Variables:"), " Automatically creates utils::globalVariables() declarations"),
                shiny::tags$li(shiny::strong("Missing Examples:"), " Use the Documentation tab to add examples"),
                shiny::tags$li(shiny::strong("Export Status:"), " Check which functions are exported in NAMESPACE")
              ),
              shiny::br(),
              shiny::h5("Command Line Usage:"),
              shiny::div(style = "font-family: monospace; background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
                "# Quick package check", shiny::br(),
                "quick_package_check()", shiny::br(), shiny::br(),
                "# Full interactive analysis", shiny::br(),
                "vibecheck()", shiny::br(), shiny::br(),
                "# Fix global variables", shiny::br(),
                'extract_and_update_globals(check_output)', shiny::br()
              )
            )
          )
        ),
        
        # Settings Tab
        shinydashboard::tabItem(tabName = "settings",
          shiny::fluidRow(
            shinydashboard::box(title = "Performance Settings", status = "primary", solidHeader = TRUE, width = 6,
              shiny::h5("Analysis Performance"),
              shiny::radioButtons("default_analysis_mode", "Default analysis mode:",
                choices = list(
                  "Ultra Fast" = "ultra_fast",
                  "Fast (Recommended)" = "fast", 
                  "Full Analysis" = "full"
                ),
                selected = "fast"
              ),
              shiny::checkboxInput("auto_dependency_check", "Auto-check dependencies", value = FALSE),
              shiny::checkboxInput("verbose_output", "Verbose output", value = TRUE),
              shiny::br(),
              shiny::actionButton("save_settings", "Save Settings", class = "btn-primary")
            ),
            
            shinydashboard::box(title = "Package Information", status = "info", solidHeader = TRUE, width = 6,
              shiny::h5("Current Package:"),
              shiny::verbatimTextOutput("current_package_info"),
              shiny::br(),
              shiny::h5("Available Functions"),
              shiny::p("You can use these functions directly in the R console:"),
              shiny::tags$ul(
                shiny::tags$li(shiny::code("vibecheck()"), " - Complete package analysis"),
                shiny::tags$li(shiny::code("quick_package_check()"), " - Fast package check"),
                shiny::tags$li(shiny::code("analyze_package_fast()"), " - Optimized analysis"),
                shiny::tags$li(shiny::code("launch_doc_app()"), " - Launch this Shiny app")
              )
            )
          ),
          
          shiny::fluidRow(
            shinydashboard::box(title = "Function Reference", status = "success", solidHeader = TRUE, width = 12,
              shiny::h5("Core Analysis Functions:"),
              shiny::div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
                shiny::div(
                  shiny::h6("Fast Functions (Recommended):"),
                  shiny::tags$ul(
                    shiny::tags$li(shiny::code("analyze_package_fast()")),
                    shiny::tags$li(shiny::code("analyze_dependencies_fast()")),
                    shiny::tags$li(shiny::code("quick_package_check()")),
                    shiny::tags$li(shiny::code("parse_function_docs_fast()"))
                  )
                ),
                shiny::div(
                  shiny::h6("Full Functions:"),
                  shiny::tags$ul(
                    shiny::tags$li(shiny::code("analyze_package()")),
                    shiny::tags$li(shiny::code("analyze_package_dependencies()")),
                    shiny::tags$li(shiny::code("extract_package_functions()")),
                    shiny::tags$li(shiny::code("generate_package_usage_report()"))
                  )
                )
              ),
              
              shiny::h5("Documentation Functions:"),
              shiny::div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
                shiny::div(
                  shiny::tags$ul(
                    shiny::tags$li(shiny::code("generate_roxygen_template()")),
                    shiny::tags$li(shiny::code("generate_bulk_documentation()")),
                    shiny::tags$li(shiny::code("add_custom_examples()")),
                    shiny::tags$li(shiny::code("save_function_docs()"))
                  )
                ),
                shiny::div(
                  shiny::tags$ul(
                    shiny::tags$li(shiny::code("detect_missing_examples()")),
                    shiny::tags$li(shiny::code("update_examples_only()")),
                    shiny::tags$li(shiny::code("check_export_status()")),
                    shiny::tags$li(shiny::code("get_exported_functions()"))
                  )
                )
              ),
              
              shiny::h5("Utility Functions:"),
              shiny::div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
                shiny::div(
                  shiny::tags$ul(
                    shiny::tags$li(shiny::code("extract_and_update_globals()")),
                    shiny::tags$li(shiny::code("analyze_namespace_usage()")),
                    shiny::tags$li(shiny::code("apply_namespace_conversion()"))
                  )
                ),
                shiny::div(
                  shiny::tags$ul(
                    shiny::tags$li(shiny::code("install_missing_packages()")),
                    shiny::tags$li(shiny::code("scan_r_functions()")),
                    shiny::tags$li(shiny::code("detect_package_root()"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}