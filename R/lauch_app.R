#' Launch the R Documentation Assistant Shiny App
#'
#' Launches an interactive Shiny application for analyzing R package documentation,
#' dependencies, and generating roxygen2 documentation templates.
#'
#' @param package_path Character. Path to the R package directory (default: ".")
#' @param port Integer. Port number for the Shiny app (default: auto-select)
#' @param host Character. Host address (default: "127.0.0.1")
#' @param launch_browser Logical. Whether to launch browser automatically (default: TRUE)
#'
#' @return Starts a Shiny application
#'
#' @examples
#' \dontrun{
#' # Launch app for current directory
#' launch_doc_app()
#' 
#' # Launch app for specific package
#' launch_doc_app("/path/to/my/package")
#' 
#' # Launch on specific port
#' launch_doc_app(port = 3838)
#' 
#' # Launch without opening browser
#' launch_doc_app(launch_browser = FALSE)
#' }
#'
#' @export
launch_doc_app <- function(package_path = ".", port = NULL, host = "127.0.0.1", launch_browser = TRUE) {
  
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
  
  cat("Launching R Documentation Assistant...\n")
  cat("Package path:", package_path, "\n\n")
  
  # First, run diagnostic
  check_r_files(package_path)
  
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
  
  shiny::shinyApp(ui = ui, server = server, options = options)
}

#' Check R Files Diagnostic
#'
#' Diagnostic function to check what R files are available in a directory
#' and provide information about package structure.
#'
#' @param path Character. Path to check (default: ".")
#'
#' @examples
#' \dontrun{
#' # Check current directory
#' check_r_files()
#' 
#' # Check specific package
#' check_r_files("/path/to/package")
#' }
#'
#' @export
check_r_files <- function(path = ".") {
  cat("=== R FILES DIAGNOSTIC ===\n")
  cat("Checking path:", normalizePath(path, mustWork = FALSE), "\n\n")
  
  # Check different possible locations
  locations <- c(
    "R" = file.path(path, "R"),
    "Current dir" = path,
    "src" = file.path(path, "src"),
    "scripts" = file.path(path, "scripts")
  )
  
  for (name in names(locations)) {
    dir_path <- locations[[name]]
    cat("Checking", name, "directory:", dir_path, "\n")
    
    if (dir.exists(dir_path)) {
      r_files <- list.files(dir_path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
      cat("  ? Directory exists\n")
      cat("  ? Found", length(r_files), "R files\n")
      
      if (length(r_files) > 0) {
        cat("  Files:\n")
        for (f in utils::head(r_files, 10)) {  # Show first 10 files
          cat("    -", basename(f), "\n")
        }
        if (length(r_files) > 10) cat("    ... and", length(r_files) - 10, "more\n")
      }
    } else {
      cat("  ? Directory does not exist\n")
    }
    cat("\n")
  }
  
  # Check if this looks like an R package
  has_description <- file.exists(file.path(path, "DESCRIPTION"))
  has_namespace <- file.exists(file.path(path, "NAMESPACE"))
  
  cat("Package indicators:\n")
  cat("  DESCRIPTION file:", if(has_description) "?" else "?", "\n")
  cat("  NAMESPACE file:", if(has_namespace) "?" else "?", "\n")
  
  if (has_description && has_namespace) {
    cat("  ? This looks like an R package\n")
  } else {
    cat("  ? This doesn't look like a standard R package\n")
  }
  
  invisible(NULL)
}
