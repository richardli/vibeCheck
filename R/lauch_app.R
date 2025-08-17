#' Launch the R Documentation Assistant Shiny App (SIMPLE FIXED VERSION)
#'
#' @param package_path Character. Path to package (default: auto-detect)
#' @param port Integer. Port number (default: auto-select)
#' @param host Character. Host address (default: "127.0.0.1")
#' @param launch_browser Logical. Launch browser (default: TRUE)
#'
#' @export
launch_doc_app <- function(package_path = NULL, port = NULL, host = "127.0.0.1", launch_browser = TRUE) {
  
  # Smart path detection
  if (is.null(package_path)) {
    package_path <- smart_detect_package_path()
  }
  
  # Check required packages
  required_packages <- c("shiny", "shinyAce", "shinydashboard", "DT", "stringr", "purrr", "R6")
  missing_packages <- c()
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  if (length(missing_packages) > 0) {
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "), 
         "\nPlease install them with: utils::install.packages(c(", 
         paste(paste0("'", missing_packages, "'"), collapse = ", "), "))")
  }
  
  # Load required libraries
  library(shiny)
  library(shinydashboard)
  library(shinyAce)
  library(DT)
  library(stringr)
  library(purrr)
  library(R6)
  
  cat("🚀 Launching VibeCheck Documentation Assistant\n")
  cat("==============================================\n")
  cat("Package path:", package_path, "\n")
  cat("Host:", host, "\n")
  if (!is.null(port)) cat("Port:", port, "\n")
  cat("\n")
  
  # Run quick diagnostic
  cat("Running quick package check...\n")
  tryCatch({
    pkg_info <- analyze_package(package_path, include_dependencies = FALSE, verbose = FALSE)
    cat("✅ Found", pkg_info$stats$total_functions, "functions in", pkg_info$stats$total_files, "files\n")
  }, error = function(e) {
    cat("Warning: Quick check failed -", e$message, "\n")
    cat("The app will still launch, but check your package path.\n")
  })
  
  cat("\nStarting Shiny application...\n\n")
  
  # Create the UI and server
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
  
  # Start the app
  shiny::shinyApp(ui = ui, server = server, options = options)
}