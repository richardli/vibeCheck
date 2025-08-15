#' Fix Global Variable Binding Issues from R CMD check
#'
#' Parses R CMD check output for "no visible binding for global variable" errors
#' and automatically adds them to a utils::globalVariables() call in check_global.R
#'
#' @param check_output Character. The copied text from R CMD check output
#' @param file_path Character. Path to the check_global.R file (default: "R/check_global.R")
#' @param preview_only Logical. If TRUE, shows what would be added without writing (default: FALSE)
#' @param backup Logical. Create backup of existing file (default: TRUE)
#'
#' @return List with information about variables found and file operations
#'
#' @examples
#' \dontrun{
#' # Copy-paste your R CMD check output
#' check_text <- "
#' checking R code for possible problems ... [21s/21s] NOTE
#'   AH_TOBC_W_OTH: no visible binding for global variable 'v149'
#'   AH_TOBC_W_OTH: no visible binding for global variable 'rc_edu'
#' "
#' 
#' # Preview what would be added
#' fix_global_variables(check_text, preview_only = TRUE)
#' 
#' # Apply the fix
#' fix_global_variables(check_text)
#' }
#'
#' @export
fix_global_variables <- function(check_output, 
                                 file_path = "R/check_global.R",
                                 preview_only = FALSE,
                                 backup = FALSE) {
  
  # Smart path handling - detect if we're in R/ directory
  final_file_path <- determine_correct_file_path(file_path)
  
  # Parse the check output to extract variable names
  variables <- extract_global_variables(check_output)
  
  if (length(variables) == 0) {
    message("No global variable binding issues found in the provided text.")
    return(list(
      variables_found = character(0),
      file_existed = file.exists(final_file_path),
      action_taken = "none"
    ))
  }
  
  message("Found ", length(variables), " global variable binding issues:")
  for (var in variables) {
    message("  - ", var)
  }
  
  # Check if file exists and read existing variables
  file_existed <- file.exists(final_file_path)
  existing_variables <- c()
  
  if (file_existed) {
    message("\nFile ", final_file_path, " exists. Reading existing variables...")
    existing_variables <- extract_existing_global_variables(final_file_path)
    message("Found ", length(existing_variables), " existing variables in file")
  } else {
    message("\nFile ", final_file_path, " does not exist. Will create new file.")
  }
  
  # Combine and deduplicate variables
  all_variables <- unique(c(existing_variables, variables))
  new_variables <- setdiff(variables, existing_variables)
  
  message("\nSummary:")
  message("  New variables to add: ", length(new_variables))
  message("  Total variables after update: ", length(all_variables))
  
  if (length(new_variables) == 0) {
    message("All variables are already in the globalVariables() call. No changes needed.")
    return(list(
      variables_found = variables,
      variables_added = character(0),
      file_existed = file_existed,
      action_taken = "no_changes_needed"
    ))
  }
  
  # Generate the new file content
  new_content <- generate_global_variables_file(all_variables)
  
  if (preview_only) {
    message("\n=== PREVIEW MODE ===")
    message("Would write to: ", final_file_path)
    message("\nNew file content:")
    cat(new_content, sep = "\n")
    return(list(
      variables_found = variables,
      variables_added = new_variables,
      file_existed = file_existed,
      action_taken = "preview_only",
      preview_content = new_content
    ))
  }
  
  # Create backup if file exists and backup is requested
  if (file_existed && backup) {
    backup_path <- paste0(final_file_path, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(final_file_path, backup_path)
    message("Backup created: ", backup_path)
  }
  
  # Ensure directory exists
  dir.create(dirname(final_file_path), showWarnings = FALSE, recursive = TRUE)
  
  # Write the file
  writeLines(new_content, final_file_path)
  
  action <- if (file_existed) "updated" else "created"
  message("\nFile ", action, ": ", final_file_path)
  message("Added ", length(new_variables), " new variables to globalVariables()")
  
  return(list(
    variables_found = variables,
    variables_added = new_variables,
    existing_variables = existing_variables,
    file_existed = file_existed,
    action_taken = action,
    file_path = final_file_path
  ))
}

#' Determine Correct File Path for check_global.R
#'
#' @param file_path Character. Initial file path
#' @return Character. Corrected file path
determine_correct_file_path <- function(file_path) {
  
  # If absolute path, use as is
  if (startsWith(file_path, "/") || startsWith(file_path, "~")) {
    return(file_path)
  }
  
  # Get current working directory
  cwd <- getwd()
  
  # Check if we're currently in an R/ directory
  if (basename(cwd) == "R") {
    # We're in R/ directory
    if (startsWith(file_path, "R/")) {
      # Remove the R/ prefix since we're already in R/
      file_path <- substr(file_path, 3, nchar(file_path))
    }
    return(file_path)
  } else {
    # We're in package root or other directory
    # Use the path as provided (likely R/check_global.R)
    return(file_path)
  }
}

#' Extract Global Variable Names from R CMD check Output
#'
#' @param check_output Character. Raw check output text
#' @return Character vector of variable names
extract_global_variables <- function(check_output) {
  
  # Multiple patterns to handle different formats
  patterns <- c(
    # Standard pattern: no visible binding for global variable 'variable_name'
    "no visible binding for global variable\\s+['\"]([^'\"]+)['\"]",
    
    # Alternative pattern without quotes
    "no visible binding for global variable\\s+([a-zA-Z_][a-zA-Z0-9_\\.]*)",
    
    # Pattern for: no visible binding for global variable variable_name
    "no visible binding for global variable\\s+([^\\s]+)",
    
    # Pattern that captures variables after colon
    ":\\s+no visible binding for global variable\\s+['\"]([^'\"]+)['\"]",
    ":\\s+no visible binding for global variable\\s+([a-zA-Z_][a-zA-Z0-9_\\.]*)"
  )
  
  all_variables <- c()
  
  # Try each pattern
  for (pattern in patterns) {
    matches <- stringr::str_match_all(check_output, pattern)[[1]]
    
    if (nrow(matches) > 0) {
      # Get the variable names (second column contains the captured group)
      variables <- matches[, 2]
      all_variables <- c(all_variables, variables)
    }
  }
  
  # Debug: show what we're looking for
  if (length(all_variables) == 0) {
    message("Debug: No matches found. Looking for patterns in text:")
    
    # Show first few lines of input for debugging
    lines <- strsplit(check_output, "\n")[[1]]
    message("Input preview (first 10 lines):")
    for (i in 1:min(10, length(lines))) {
      message("  ", i, ": ", lines[i])
    }
    
    # Check if text contains the key phrase at all
    if (grepl("no visible binding", check_output, ignore.case = TRUE)) {
      message("\nFound 'no visible binding' in text. Trying broader extraction...")
      
      # Extract all lines containing the phrase
      binding_lines <- lines[grepl("no visible binding", lines, ignore.case = TRUE)]
      message("Lines with 'no visible binding':")
      for (line in binding_lines) {
        message("  ", line)
      }
      
      # Try to extract anything that looks like a variable name from these lines
      for (line in binding_lines) {
        # Look for quoted strings that might be variable names
        quoted_matches <- stringr::str_extract_all(line, "['\"]([a-zA-Z_][a-zA-Z0-9_\\.]*)['\"]")[[1]]
        if (length(quoted_matches) > 0) {
          # Remove quotes
          vars <- gsub("['\"]", "", quoted_matches)
          all_variables <- c(all_variables, vars)
        }
        
        # Also try to find unquoted variable names at the end of lines
        unquoted_match <- stringr::str_extract(line, "[a-zA-Z_][a-zA-Z0-9_\\.]*$")
        if (!is.na(unquoted_match)) {
          all_variables <- c(all_variables, unquoted_match)
        }
      }
    } else {
      message("\n'no visible binding' phrase not found in text.")
      message("Please check that you copied the R CMD check output correctly.")
    }
  }
  
  # Remove duplicates, empty strings, and sort
  all_variables <- all_variables[!is.na(all_variables) & nchar(all_variables) > 0]
  all_variables <- unique(all_variables)
  all_variables <- sort(all_variables)
  
  return(all_variables)
}

#' Extract Existing Global Variables from check_global.R
#'
#' @param file_path Character. Path to existing file
#' @return Character vector of existing variable names
extract_existing_global_variables <- function(file_path) {
  
  if (!file.exists(file_path)) {
    return(character(0))
  }
  
  content <- readLines(file_path, warn = FALSE)
  content_text <- paste(content, collapse = "\n")
  
  # Pattern to match globalVariables(c("var1", "var2", ...))
  # This handles multi-line declarations
  pattern <- "globalVariables\\s*\\(\\s*c\\s*\\(([^)]+)\\)"
  
  matches <- stringr::str_match_all(content_text, pattern)[[1]]
  
  if (nrow(matches) == 0) {
    return(character(0))
  }
  
  # Extract the variable list (inside the c(...))
  var_list_text <- matches[1, 2]
  
  # Extract individual quoted variables
  var_pattern <- "['\"]([^'\"]+)['\"]"
  var_matches <- stringr::str_match_all(var_list_text, var_pattern)[[1]]
  
  if (nrow(var_matches) == 0) {
    return(character(0))
  }
  
  variables <- var_matches[, 2]
  return(sort(unique(variables)))
}

#' Generate Content for check_global.R File
#'
#' @param variables Character vector. All variable names to include
#' @return Character vector. Lines of file content
generate_global_variables_file <- function(variables) {
  
  if (length(variables) == 0) {
    variables <- character(0)
  }
  
  # Create header with only ASCII characters
  header <- c(
    "# Global Variables Declaration",
    "# This file declares global variables to avoid R CMD check NOTEs",
    "# about 'no visible binding for global variable'",
    "# Generated automatically by fix_global_variables()",
    "",
    "# Suppress R CMD check NOTEs about global variables"
  )
  
  if (length(variables) == 0) {
    return(c(header, "utils::globalVariables(character(0))"))
  }
  
  # Format variables for nice display
  # If there are many variables, format them nicely across multiple lines
  if (length(variables) <= 5) {
    # Short list - put on one line
    var_string <- paste0('"', variables, '"', collapse = ", ")
    global_vars_call <- paste0("utils::globalVariables(c(", var_string, "))")
  } else {
    # Long list - format across multiple lines with proper indentation
    quoted_vars <- paste0('"', variables, '"')
    
    # Create nicely formatted multi-line structure
    var_lines <- c()
    var_lines[1] <- "utils::globalVariables(c("
    
    # Add variables with proper indentation
    for (i in seq_along(quoted_vars)) {
      if (i == length(quoted_vars)) {
        # Last variable - no comma
        var_lines <- c(var_lines, paste0("  ", quoted_vars[i]))
      } else {
        # Add comma
        var_lines <- c(var_lines, paste0("  ", quoted_vars[i], ","))
      }
    }
    
    var_lines <- c(var_lines, "))")
    global_vars_call <- var_lines
  }
  
  # Combine header and variable declaration
  content <- c(header, "", global_vars_call)
  
  return(content)
}

#' Interactive Function to Fix Global Variables
#'
#' Prompts user to paste R CMD check output and processes it automatically
#'
#' @param file_path Character. Path to check_global.R file
#' @return Result from fix_global_variables()
#'
#' @examples
#' \dontrun{
#' # Interactive mode - will prompt for input
#' fix_global_variables_interactive()
#' }
#'
#' @export
fix_global_variables_interactive <- function(file_path = "R/check_global.R") {
  
  cat("=== Interactive Global Variables Fixer ===\n\n")
  cat("Please paste your R CMD check output below.\n")
  cat("Press Enter twice when done (empty line to finish):\n\n")
  
  # Read multi-line input
  input_lines <- c()
  repeat {
    line <- readline()
    if (nchar(line) == 0) {
      break
    }
    input_lines <- c(input_lines, line)
  }
  
  if (length(input_lines) == 0) {
    message("No input provided. Exiting.")
    return(invisible(NULL))
  }
  
  check_output <- paste(input_lines, collapse = "\n")
  
  cat("\n--- Processing input ---\n")
  
  # Show preview first
  result <- fix_global_variables(check_output, file_path, preview_only = TRUE)
  
  if (result$action_taken == "preview_only" && length(result$variables_added) > 0) {
    response <- readline("\nApply these changes? (y/n): ")
    if (tolower(substr(response, 1, 1)) == "y") {
      result <- fix_global_variables(check_output, file_path, preview_only = FALSE)
      cat("\nChanges applied successfully!\n")
    } else {
      cat("\nChanges not applied.\n")
    }
  }
  
  return(invisible(result))
}

#' Extract Global Variable Names from R CMD check Output
#'
#' @param check_output Character. Raw check output text
#' @return Character vector of variable names
extract_global_variables <- function(check_output) {
  
  # Multiple patterns to handle different formats
  patterns <- c(
    # Standard pattern: no visible binding for global variable 'variable_name'
    "no visible binding for global variable\\s+['\"]([^'\"]+)['\"]",
    
    # Alternative pattern without quotes
    "no visible binding for global variable\\s+([a-zA-Z_][a-zA-Z0-9_\\.]*)",
    
    # Pattern for: no visible binding for global variable variable_name
    "no visible binding for global variable\\s+([^\\s]+)",
    
    # Pattern that captures variables after colon
    ":\\s+no visible binding for global variable\\s+['\"]([^'\"]+)['\"]",
    ":\\s+no visible binding for global variable\\s+([a-zA-Z_][a-zA-Z0-9_\\.]*)"
  )
  
  all_variables <- c()
  
  # Try each pattern
  for (pattern in patterns) {
    matches <- stringr::str_match_all(check_output, pattern)[[1]]
    
    if (nrow(matches) > 0) {
      # Get the variable names (second column contains the captured group)
      variables <- matches[, 2]
      all_variables <- c(all_variables, variables)
    }
  }
  
  # Debug: show what we're looking for
  if (length(all_variables) == 0) {
    message("Debug: No matches found. Looking for patterns in text:")
    
    # Show first few lines of input for debugging
    lines <- strsplit(check_output, "\n")[[1]]
    message("Input preview (first 10 lines):")
    for (i in 1:min(10, length(lines))) {
      message("  ", i, ": ", lines[i])
    }
    
    # Check if text contains the key phrase at all
    if (grepl("no visible binding", check_output, ignore.case = TRUE)) {
      message("\nFound 'no visible binding' in text. Trying broader extraction...")
      
      # Extract all lines containing the phrase
      binding_lines <- lines[grepl("no visible binding", lines, ignore.case = TRUE)]
      message("Lines with 'no visible binding':")
      for (line in binding_lines) {
        message("  ", line)
      }
      
      # Try to extract anything that looks like a variable name from these lines
      for (line in binding_lines) {
        # Look for quoted strings that might be variable names
        quoted_matches <- stringr::str_extract_all(line, "['\"]([a-zA-Z_][a-zA-Z0-9_\\.]*)['\"]")[[1]]
        if (length(quoted_matches) > 0) {
          # Remove quotes
          vars <- gsub("['\"]", "", quoted_matches)
          all_variables <- c(all_variables, vars)
        }
        
        # Also try to find unquoted variable names at the end of lines
        unquoted_match <- stringr::str_extract(line, "[a-zA-Z_][a-zA-Z0-9_\\.]*$")
        if (!is.na(unquoted_match)) {
          all_variables <- c(all_variables, unquoted_match)
        }
      }
    } else {
      message("\n'no visible binding' phrase not found in text.")
      message("Please check that you copied the R CMD check output correctly.")
    }
  }
  
  # Remove duplicates, empty strings, and sort
  all_variables <- all_variables[!is.na(all_variables) & nchar(all_variables) > 0]
  all_variables <- unique(all_variables)
  all_variables <- sort(all_variables)
  
  return(all_variables)
}

#' Extract Existing Global Variables from check_global.R
#'
#' @param file_path Character. Path to existing file
#' @return Character vector of existing variable names
extract_existing_global_variables <- function(file_path) {
  
  if (!file.exists(file_path)) {
    return(character(0))
  }
  
  content <- readLines(file_path, warn = FALSE)
  content_text <- paste(content, collapse = "\n")
  
  # Pattern to match globalVariables(c("var1", "var2", ...))
  # This handles multi-line declarations
  pattern <- "globalVariables\\s*\\(\\s*c\\s*\\(([^)]+)\\)"
  
  matches <- stringr::str_match_all(content_text, pattern)[[1]]
  
  if (nrow(matches) == 0) {
    return(character(0))
  }
  
  # Extract the variable list (inside the c(...))
  var_list_text <- matches[1, 2]
  
  # Extract individual quoted variables
  var_pattern <- "['\"]([^'\"]+)['\"]"
  var_matches <- stringr::str_match_all(var_list_text, var_pattern)[[1]]
  
  if (nrow(var_matches) == 0) {
    return(character(0))
  }
  
  variables <- var_matches[, 2]
  return(sort(unique(variables)))
}

#' Generate Content for check_global.R File
#'
#' @param variables Character vector. All variable names to include
#' @return Character vector. Lines of file content
generate_global_variables_file <- function(variables) {
  
  if (length(variables) == 0) {
    variables <- character(0)
  }
  
  # Create header
  header <- c(
    "# Global Variables Declaration",
    "# This file declares global variables to avoid R CMD check NOTEs",
    "# about 'no visible binding for global variable'",
    "# Generated automatically by fix_global_variables()",
    "",
    "# Suppress R CMD check NOTEs about global variables"
  )
  
  if (length(variables) == 0) {
    return(c(header, "utils::globalVariables(character(0))"))
  }

  variables <- gsub("‘", "", variables)
  variables <- gsub("’", "", variables)
  
  # Format variables for nice display
  # If there are many variables, format them nicely across multiple lines
  if (length(variables) <= 5) {
    # Short list - put on one line
    var_string <- paste0('"', variables, '"', collapse = ", ")
    global_vars_call <- paste0("utils::globalVariables(c(", var_string, "))")
  } else {
    # Long list - format across multiple lines with proper indentation
    quoted_vars <- paste0('"', variables, '"')
    
    # Create nicely formatted multi-line structure
    var_lines <- c()
    var_lines[1] <- "utils::globalVariables(c("
    
    # Add variables with proper indentation
    for (i in seq_along(quoted_vars)) {
      if (i == length(quoted_vars)) {
        # Last variable - no comma
        var_lines <- c(var_lines, paste0("  ", quoted_vars[i]))
      } else {
        # Add comma
        var_lines <- c(var_lines, paste0("  ", quoted_vars[i], ","))
      }
    }
    
    var_lines <- c(var_lines, "))")
    global_vars_call <- var_lines
  }
  
  # Combine header and variable declaration
  content <- c(header, "", global_vars_call)
  
  return(content)
}

#' Interactive Function to Fix Global Variables
#'
#' Prompts user to paste R CMD check output and processes it automatically
#'
#' @param file_path Character. Path to check_global.R file
#' @return Result from fix_global_variables()
#'
#' @examples
#' \dontrun{
#' # Interactive mode - will prompt for input
#' fix_global_variables_interactive()
#' }
#'
#' @export
fix_global_variables_interactive <- function(file_path = "R/check_global.R") {
  
  cat("=== Interactive Global Variables Fixer ===\n\n")
  cat("Please paste your R CMD check output below.\n")
  cat("Press Enter twice when done (empty line to finish):\n\n")
  
  # Read multi-line input
  input_lines <- c()
  repeat {
    line <- readline()
    if (nchar(line) == 0) {
      break
    }
    input_lines <- c(input_lines, line)
  }
  
  if (length(input_lines) == 0) {
    message("No input provided. Exiting.")
    return(invisible(NULL))
  }
  
  check_output <- paste(input_lines, collapse = "\n")
  
  cat("\n--- Processing input ---\n")
  
  # Show preview first
  result <- fix_global_variables(check_output, file_path, preview_only = TRUE)
  
  if (result$action_taken == "preview_only" && length(result$variables_added) > 0) {
    response <- readline("\nApply these changes? (y/n): ")
    if (tolower(substr(response, 1, 1)) == "y") {
      result <- fix_global_variables(check_output, file_path, preview_only = FALSE)
      cat("\nChanges applied successfully!\n")
    } else {
      cat("\nChanges not applied.\n")
    }
  }
  
  return(invisible(result))
}