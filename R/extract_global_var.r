#' Extract and Update Global Variables from R CMD Check Output
#'
#' This function parses R CMD check output to identify global variable warnings
#' and automatically updates the package's check_global.R file with the appropriate
#' utils::globalVariables() declarations. It reads existing global variables
#' and merges them with newly found ones.
#'
#' @param check_output_string Character string containing R CMD check output,
#'   typically copy-pasted from the console or log file.
#' @param package_dir Character string specifying the path to the package directory.
#'   Defaults to current directory (".").
#'
#' @return A list containing:
#'   \item{new_variables}{Character vector of newly found global variables}
#'   \item{all_variables}{Character vector of all global variables (existing + new)}
#'   \item{file_path}{Character string path to the check_global.R file}
#'
#' @details
#' The function searches for two types of global variable warnings in the check output:
#' \enumerate{
#'   \item Individual warnings: "no visible binding for global variable 'variable_name'"
#'   \item Summary section: "Undefined global functions or variables:"
#' }
#'
#' The function automatically:
#' \itemize{
#'   \item Creates the R/ directory if it doesn't exist
#'   \item Reads existing global variables from check_global.R if present
#'   \item Merges new variables with existing ones (removing duplicates)
#'   \item Writes the updated utils::globalVariables() declaration
#'   \item Filters out function names and keeps only variable-like identifiers
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with R CMD check output
#' check_output <- "myfunction: no visible binding for global variable 'x'
#'    Undefined global functions or variables:
#'      x y_var weighted_value"
#' 
#' extract_and_update_globals(check_output)
#' 
#' # Specify different package directory
#' extract_and_update_globals(check_output, package_dir = "path/to/package")
#' }
#'
#' @seealso \code{\link[utils]{globalVariables}} for more information about
#'   suppressing global variable warnings.
#'
#' @export
extract_and_update_globals <- function(check_output_string, package_dir = ".") {
  
  # Initialize vector to store found variables
  global_vars <- character(0)
  
  # Split input into lines
  lines <- unlist(strsplit(check_output_string, "\n"))
  
  # Pattern 1: "no visible binding for global variable 'variable_name'"
  pattern1 <- "no visible binding for global variable '([^']+)'"
  
  for (line in lines) {
    matches <- regmatches(line, gregexpr(pattern1, line))
    if (length(matches[[1]]) > 0 && matches[[1]][1] != "character(0)") {
      for (match in matches[[1]]) {
        var <- gsub(".*'([^']+)'.*", "\\1", match)
        global_vars <- c(global_vars, var)
      }
    }
  }
  
  # Pattern 2: Extract from "Undefined global functions or variables:" section
  undefined_start <- grep("Undefined global functions or variables:", lines)
  
  if (length(undefined_start) > 0) {
    start_idx <- undefined_start[1] + 1
    
    if (start_idx <= length(lines)) {
      for (i in start_idx:length(lines)) {
        line <- trimws(lines[i])
        if (line == "" || !grepl("^\\s", lines[i])) {
          break
        }
        
        words <- unlist(strsplit(line, "\\s+"))
        words <- trimws(words)
        words <- words[words != ""]
        
        for (word in words) {
          if (word != "" && !grepl("\\.", word) && !word %in% c("c", "list", "data", "function")) {
            global_vars <- c(global_vars, word)
          }
        }
      }
    }
  }
  
  # Determine the correct path to check_global.R
  # Check if we're already in the R/ directory or at package root
  current_dir <- normalizePath(package_dir, winslash = "/")
  
  if (basename(current_dir) == "R") {
    # We're already in the R/ directory
    globals_file <- file.path(package_dir, "check_global.R")
    r_dir <- package_dir
  } else {
    # We're at package root (or somewhere else), look for R/ subdirectory
    r_dir <- file.path(package_dir, "R")
    globals_file <- file.path(r_dir, "check_global.R")
  }
  
  # Read existing global variables if file exists
  existing_globals <- character(0)
  
  if (file.exists(globals_file)) {
    content <- readLines(globals_file, warn = FALSE)
    globals_lines <- grep("utils::globalVariables", content)
    
    if (length(globals_lines) > 0) {
      start_line <- globals_lines[1]
      full_call <- character(0)
      paren_count <- 0
      
      for (i in start_line:length(content)) {
        line <- content[i]
        full_call <- c(full_call, line)
        paren_count <- paren_count + lengths(gregexpr("\\(", line)) - lengths(gregexpr("\\)", line))
        
        if (paren_count <= 0 && grepl("\\)", line)) {
          break
        }
      }
      
      full_call_text <- paste(full_call, collapse = " ")
      c_pattern <- "c\\s*\\(([^)]+)\\)"
      c_match <- regmatches(full_call_text, regexpr(c_pattern, full_call_text))
      
      if (length(c_match) > 0) {
        vars_content <- gsub("c\\s*\\(([^)]+)\\)", "\\1", c_match)
        vars_raw <- unlist(strsplit(vars_content, ","))
        vars_clean <- trimws(vars_raw)
        vars_clean <- gsub('^["\']|["\']$', '', vars_clean)
        vars_clean <- vars_clean[vars_clean != "" & !is.na(vars_clean)]
        existing_globals <- vars_clean
      }
    }
  } else {
    # Only create R/ directory if we're at package root and it doesn't exist
    if (basename(current_dir) != "R" && !dir.exists(r_dir)) {
      dir.create(r_dir, recursive = TRUE)
    }
  }
  
  # Clean up new variables and merge with existing
  global_vars <- unique(global_vars)
  global_vars <- global_vars[global_vars != ""]
  global_vars <- global_vars[!is.na(global_vars)]
  
  all_globals <- unique(c(existing_globals, global_vars))
  all_globals <- sort(all_globals[all_globals != ""])
  new_vars <- setdiff(global_vars, existing_globals)
  
  # Write the updated file
  if (length(all_globals) > 0) {
    header <- c(
      "# Global Variables Declaration",
      "# This file declares global variables to avoid R CMD check warnings",
      "# Auto-generated and updated by extract_and_update_globals()",
      ""
    )
    
    formatted_vars <- paste0('  "', all_globals, '"', collapse = ",\n")
    globals_call <- paste0("utils::globalVariables(c(\n", formatted_vars, "\n))")
    
    file_content <- c(header, globals_call)
    writeLines(file_content, globals_file)
    
    return(list(
      new_variables = new_vars,
      all_variables = all_globals,
      file_path = globals_file
    ))
  } else {
    return(list(
      new_variables = character(0),
      all_variables = character(0),
      file_path = globals_file
    ))
  }
}