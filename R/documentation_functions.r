#' Generate Roxygen2 Documentation Template
#'
#' Creates a roxygen2 documentation template for a function with smart
#' parameter suggestions based on existing documentation patterns.
#'
#' @param function_name Character. Name of the function
#' @param args Named list. Function arguments (names and default values)
#' @param param_suggestions Named list. Parameter history for suggestions (optional)
#' @param template_type Character. Type of template ("standard", "exported", "internal")
#' @param add_examples Logical. Whether to include examples section
#'
#' @return Character. Roxygen2 documentation template
#'
#' @examples
#' \dontrun{
#' # Basic template
#' template <- generate_roxygen_template("my_function", 
#'                                      list(data = NULL, method = '"default"'))
#' 
#' # With parameter suggestions
#' pkg_info <- analyze_package()
#' template <- generate_roxygen_template("process_data", 
#'                                      list(data = NULL, verbose = "TRUE"),
#'                                      pkg_info$parameter_history)
#' 
#' # Internal function template
#' template <- generate_roxygen_template("helper_func", 
#'                                      list(x = NULL),
#'                                      template_type = "internal")
#' }
#'
#' @export
generate_roxygen_template <- function(function_name, 
                                     args, 
                                     param_suggestions = NULL,
                                     template_type = "exported",
                                     add_examples = TRUE) {
  
  # Start with title and description
  template <- paste0("#' ", function_name, "\n")
  template <- paste0(template, "#'\n")
  template <- paste0(template, "#' [Brief description of what the function does]\n")
  template <- paste0(template, "#'\n")
  
  # Add detailed description for exported functions
  if (template_type == "exported") {
    template <- paste0(template, "#' [Detailed description with more context and usage information]\n")
    template <- paste0(template, "#'\n")
  }
  
  # Add parameters
  if (length(args) > 0) {
    for (arg_name in names(args)) {
      param_desc <- suggest_parameter_description(arg_name, param_suggestions)
      template <- paste0(template, "#' @param ", arg_name, " ", param_desc, "\n")
    }
    template <- paste0(template, "#'\n")
  }
  
  # Add return value
  template <- paste0(template, "#' @return [Description of return value]\n")
  
  # Add examples for exported functions
  if (add_examples && template_type %in% c("exported", "standard")) {
    template <- paste0(template, "#'\n")
    template <- paste0(template, "#' @examples\n")
    template <- paste0(template, "#' \\dontrun{\n")
    
    # Generate smart example
    example <- generate_function_example(function_name, args)
    template <- paste0(template, "#' ", example, "\n")
    template <- paste0(template, "#' }\n")
  }
  
  # Add export tag for exported functions
  if (template_type == "exported") {
    template <- paste0(template, "#'\n")
    template <- paste0(template, "#' @export\n")
  }
  
  return(template)
}

#' Suggest Parameter Description
#'
#' @param param_name Character. Parameter name
#' @param param_suggestions Named list. Historical parameter descriptions
#' @return Character. Suggested description
suggest_parameter_description <- function(param_name, param_suggestions = NULL) {
  
  # If we have historical suggestions, use the most common one
  if (!is.null(param_suggestions) && param_name %in% names(param_suggestions)) {
    suggestions <- param_suggestions[[param_name]]
    if (length(suggestions) > 0) {
      return(suggestions[1])  # Return first (most recent) suggestion
    }
  }
  
  # Common parameter patterns
  common_descriptions <- list(
    "data" = "Data.frame or tibble containing the data to process",
    "x" = "Numeric vector or data input",
    "y" = "Numeric vector or response variable", 
    "file" = "Character. Path to file",
    "path" = "Character. Directory or file path",
    "verbose" = "Logical. Whether to print progress messages (default: TRUE)",
    "quiet" = "Logical. Whether to suppress messages (default: FALSE)",
    "debug" = "Logical. Whether to print debug information (default: FALSE)",
    "force" = "Logical. Whether to force operation (default: FALSE)",
    "recursive" = "Logical. Whether to operate recursively (default: FALSE)",
    "overwrite" = "Logical. Whether to overwrite existing files (default: FALSE)",
    "backup" = "Logical. Whether to create backup (default: TRUE)",
    "preview" = "Logical. Whether to preview changes only (default: FALSE)",
    "n" = "Integer. Number of items to process",
    "limit" = "Integer. Maximum number of items",
    "timeout" = "Numeric. Timeout in seconds",
    "method" = "Character. Method to use",
    "algorithm" = "Character. Algorithm to use",
    "format" = "Character. Output format",
    "encoding" = "Character. Text encoding (default: 'UTF-8')",
    "sep" = "Character. Field separator",
    "header" = "Logical. Whether data has header row",
    "skip" = "Integer. Number of lines to skip",
    "na" = "Character. How to handle missing values",
    "..." = "Additional arguments passed to other functions"
  )
  
  # Check for exact matches
  if (param_name %in% names(common_descriptions)) {
    return(common_descriptions[[param_name]])
  }
  
  # Pattern matching for common parameter types
  if (grepl("_path$|_file$", param_name)) {
    return("Character. File or directory path")
  }
  
  if (grepl("^is_|^has_|^use_|^enable_|^disable_", param_name)) {
    return("Logical. [Description]")
  }
  
  if (grepl("^n_|^max_|^min_|count$", param_name)) {
    return("Integer. [Description]")
  }
  
  if (grepl("name$|_name$", param_name)) {
    return("Character. Name of [item]")
  }
  
  # Default
  return("[Description]")
}

#' Generate Smart Function Example
#'
#' @param function_name Character. Function name
#' @param args Named list. Function arguments
#' @return Character. Example usage
generate_function_example <- function(function_name, args) {
  
  # Simple example with common parameter patterns
  if (length(args) == 0) {
    return(paste0("result <- ", function_name, "()"))
  }
  
  # Generate example arguments
  example_args <- character(0)
  
  for (arg_name in names(args)) {
    default_val <- args[[arg_name]]
    
    if (is.null(default_val)) {
      # Required argument - provide example
      if (arg_name %in% c("data", "df", "x")) {
        example_args <- c(example_args, paste0(arg_name, " = my_data"))
      } else if (grepl("file|path", arg_name)) {
        example_args <- c(example_args, paste0(arg_name, ' = "path/to/file"'))
      } else if (grepl("name", arg_name)) {
        example_args <- c(example_args, paste0(arg_name, ' = "my_name"'))
      } else {
        example_args <- c(example_args, paste0(arg_name, " = [value]"))
      }
    } else {
      # Optional argument - sometimes include in example
      if (arg_name %in% c("verbose", "debug")) {
        example_args <- c(example_args, paste0(arg_name, " = TRUE"))
      }
    }
  }
  
  if (length(example_args) > 0) {
    args_str <- paste(example_args, collapse = ", ")
    return(paste0("result <- ", function_name, "(", args_str, ")"))
  } else {
    return(paste0("result <- ", function_name, "()"))
  }
}

#' Save Function Documentation to File
#'
#' @param file_path Character. Path to R file
#' @param function_name Character. Name of function to document
#' @param documentation Character. Roxygen2 documentation
#' @param backup Logical. Whether to create backup (default: TRUE)
#'
#' @return List with operation results
#'
#' @examples
#' \dontrun{
#' # Save documentation for a function
#' docs <- generate_roxygen_template("my_func", list(x = NULL))
#' result <- save_function_docs("R/my_file.R", "my_func", docs)
#' }
#'
#' @export
save_function_docs <- function(file_path, function_name, documentation, backup = TRUE) {
  
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  # Read file content
  content <- readLines(file_path, warn = FALSE)
  file_text <- paste(content, collapse = "\n")
  
  # Find the function definition
  func_pattern <- paste0("((?:#'[^\n]*\n)*)(\\s*)(", function_name, ")\\s*<-\\s*function")
  func_match <- stringr::str_locate(file_text, func_pattern)
  
  if (is.na(func_match[1, 1])) {
    stop("Function '", function_name, "' not found in file: ", file_path)
  }
  
  # Extract current function definition
  start_pos <- func_match[1, 1]
  
  # Find where the new function definition should start (after existing docs)
  before_func <- substr(file_text, 1, start_pos - 1)
  
  # Find the actual function line (without existing docs)
  func_only_pattern <- paste0("(\\s*)(", function_name, ")\\s*<-\\s*function")
  func_only_match <- stringr::str_locate(file_text, func_only_pattern)
  
  if (is.na(func_only_match[1, 1])) {
    stop("Could not locate function definition for: ", function_name)
  }
  
  func_start <- func_only_match[1, 1]
  after_func <- substr(file_text, func_start, nchar(file_text))
  
  # Create backup if requested
  if (backup) {
    backup_path <- paste0(file_path, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(file_path, backup_path)
  }
  
  # Combine: before + new docs + function
  new_content <- paste0(before_func, documentation, "\n", after_func)
  
  # Write back to file
  writeLines(strsplit(new_content, "\n")[[1]], file_path)
  
  result <- list(
    file_path = file_path,
    function_name = function_name,
    backup_created = backup,
    success = TRUE
  )
  
  if (backup) {
    result$backup_path <- backup_path
  }
  
  return(result)
}

#' Get Parameter Suggestions for a Function
#'
#' @param package_data List. Result from analyze_package() (optional)
#' @param param_name Character. Parameter name to get suggestions for
#' @param function_name Character. Function name context (optional)
#'
#' @return Character vector of suggestions
#'
#' @examples
#' \dontrun{
#' # Get suggestions for a parameter
#' pkg_info <- analyze_package()
#' suggestions <- suggest_parameters(pkg_info, "data")
#' 
#' # Without package context
#' suggestions <- suggest_parameters(param_name = "verbose")
#' }
#'
#' @export
suggest_parameters <- function(package_data = NULL, param_name, function_name = NULL) {
  
  # Get suggestions from package data if available
  suggestions <- character(0)
  
  if (!is.null(package_data) && "parameter_history" %in% names(package_data)) {
    param_history <- package_data$parameter_history
    
    if (param_name %in% names(param_history)) {
      suggestions <- param_history[[param_name]]
    }
  }
  
  # If no package-specific suggestions, use common patterns
  if (length(suggestions) == 0) {
    fallback <- suggest_parameter_description(param_name)
    if (fallback != "[Description]") {
      suggestions <- fallback
    }
  }
  
  return(suggestions)
}

#' Update Function Documentation
#'
#' Replaces existing documentation for a function with new documentation.
#'
#' @param file_path Character. Path to R file
#' @param function_name Character. Function name
#' @param new_docs Character. New documentation
#' @param backup Logical. Create backup (default: TRUE)
#'
#' @return List with operation results
#' @export
update_function_docs <- function(file_path, function_name, new_docs, backup = TRUE) {
  
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  content <- readLines(file_path, warn = FALSE)
  file_text <- paste(content, collapse = "\n")
  
  # Pattern to match function with existing docs
  pattern <- paste0("((?:#'[^\n]*\n)*)(\\s*)(", function_name, "\\s*<-\\s*function)")
  
  match_result <- stringr::str_match(file_text, pattern)
  
  if (is.na(match_result[1, 1])) {
    stop("Function '", function_name, "' not found in file")
  }
  
  # Create backup
  if (backup) {
    backup_path <- paste0(file_path, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(file_path, backup_path)
  }
  
  # Replace the documentation part
  old_full_match <- match_result[1, 1]
  indentation <- match_result[1, 3]
  func_definition <- match_result[1, 4]
  
  new_full_match <- paste0(new_docs, "\n", indentation, func_definition)
  
  # Replace in file content
  new_file_text <- stringr::str_replace(file_text, stringr::fixed(old_full_match), new_full_match)
  
  # Write back
  writeLines(strsplit(new_file_text, "\n")[[1]], file_path)
  
  result <- list(
    file_path = file_path,
    function_name = function_name,
    backup_created = backup,
    success = TRUE
  )
  
  if (backup) {
    result$backup_path <- backup_path
  }
  
  return(result)
}

#' Generate Documentation for Multiple Functions
#'
#' @param package_data List. Result from analyze_package()
#' @param functions Character vector. Function names to document (optional, defaults to all undocumented)
#' @param template_type Character. Type of template to generate
#' @param save_to_files Logical. Whether to save directly to files (default: FALSE)
#' @param backup Logical. Create backups when saving (default: TRUE)
#' @param exported_only Logical. Only document functions that are exported in NAMESPACE (default: TRUE)
#'
#' @return List with generated documentation for each function
#'
#' @examples
#' \dontrun{
#' # Generate docs only for exported functions (default)
#' pkg_info <- analyze_package()
#' docs <- generate_bulk_documentation(pkg_info)
#' 
#' # Include all functions (internal and exported)
#' docs <- generate_bulk_documentation(pkg_info, exported_only = FALSE)
#' 
#' # Generate and save for specific functions
#' docs <- generate_bulk_documentation(pkg_info, 
#'                                    functions = c("func1", "func2"),
#'                                    save_to_files = TRUE)
#' }
#'
#' @export
generate_bulk_documentation <- function(package_data, 
                                       functions = NULL, 
                                       template_type = "exported",
                                       save_to_files = FALSE,
                                       backup = TRUE,
                                       exported_only = TRUE) {
  
  if (!"functions" %in% names(package_data)) {
    stop("package_data must contain 'functions' element. Use analyze_package() first.")
  }
  
  func_data <- package_data$functions
  docs_data <- package_data$documentation
  param_history <- package_data$parameter_history
  package_path <- package_data$package_path
  
  # Get list of exported functions from NAMESPACE
  exported_functions <- get_exported_functions(package_path)
  
  # If no specific functions provided, use all undocumented functions
  if (is.null(functions)) {
    undocumented <- !docs_data$has_docs
    candidate_functions <- func_data$function_name[undocumented]
    
    # Filter to only exported functions if requested
    if (exported_only && length(candidate_functions) > 0) {
      if (length(exported_functions) > 0) {
        # Use NAMESPACE to filter
        before_count <- length(candidate_functions)
        candidate_functions <- candidate_functions[candidate_functions %in% exported_functions]
        filtered_count <- before_count - length(candidate_functions)
        
        if (filtered_count > 0) {
          message("Filtered out ", filtered_count, " non-exported functions based on NAMESPACE")
        }
      } else {
        message("No NAMESPACE file found or no exported functions detected")
        message("Tip: Run devtools::document() to generate NAMESPACE, or use exported_only = FALSE")
      }
    }
    
    functions <- candidate_functions
  } else {
    # If specific functions provided, still filter by export status if requested
    if (exported_only && length(exported_functions) > 0) {
      non_exported <- functions[!functions %in% exported_functions]
      if (length(non_exported) > 0) {
        message("Warning: These functions are not exported in NAMESPACE: ", 
                paste(non_exported, collapse = ", "))
        functions <- functions[functions %in% exported_functions]
      }
    }
  }
  
  if (length(functions) == 0) {
    if (exported_only && length(exported_functions) == 0) {
      message("No functions to document. Try:")
      message("  1. Run devtools::document() first to generate NAMESPACE")
      message("  2. Add @export tags to your function documentation")
      message("  3. Use exported_only = FALSE to document all functions")
    } else {
      message("No functions to document")
    }
    return(list())
  }
  
  # Report what we're doing
  if (exported_only) {
    message("Generating documentation for ", length(functions), " exported functions...")
  } else {
    message("Generating documentation for ", length(functions), " functions...")
  }
  
  results <- list()
  
  for (func_name in functions) {
    # Find function data
    func_idx <- which(func_data$function_name == func_name)
    
    if (length(func_idx) == 0) {
      warning("Function '", func_name, "' not found")
      next
    }
    
    func_idx <- func_idx[1]  # Use first match
    
    # Get function arguments
    args <- docs_data$parsed_args[[func_idx]]
    
    # Generate documentation
    docs <- generate_roxygen_template(
      function_name = func_name,
      args = args,
      param_suggestions = param_history,
      template_type = template_type
    )
    
    results[[func_name]] <- list(
      function_name = func_name,
      file_path = func_data$file_path[func_idx],
      documentation = docs,
      generated = TRUE,
      is_exported = func_name %in% exported_functions
    )
    
    # Save to file if requested
    if (save_to_files) {
      tryCatch({
        save_result <- save_function_docs(
          file_path = func_data$file_path[func_idx],
          function_name = func_name,
          documentation = docs,
          backup = backup
        )
        
        results[[func_name]]$saved <- TRUE
        results[[func_name]]$save_result <- save_result
        
      }, error = function(e) {
        warning("Failed to save documentation for '", func_name, "': ", e$message)
        results[[func_name]]$saved <- FALSE
        results[[func_name]]$error <- e$message
      })
    }
  }
  
  message("Documentation generation complete!")
  message("  Generated: ", sum(sapply(results, function(x) x$generated)))
  if (save_to_files) {
    saved_count <- sum(sapply(results, function(x) isTRUE(x$saved)))
    message("  Saved to files: ", saved_count)
  }
  if (exported_only) {
    message("  (Only exported functions from NAMESPACE were processed)")
  }
  
  return(results)
}

#' Get Exported Functions from NAMESPACE File
#'
#' Reads the NAMESPACE file and extracts all exported function names.
#'
#' @param package_path Character. Path to package root
#' @return Character vector of exported function names
#' @export
get_exported_functions <- function(package_path) {
  
  namespace_file <- file.path(package_path, "NAMESPACE")
  
  if (!file.exists(namespace_file)) {
    return(character(0))
  }
  
  tryCatch({
    namespace_content <- readLines(namespace_file, warn = FALSE)
  }, error = function(e) {
    warning("Could not read NAMESPACE file: ", e$message)
    return(character(0))
  })
  
  if (length(namespace_content) == 0) {
    return(character(0))
  }
  
  exported_functions <- character(0)
  
  for (line in namespace_content) {
    line <- trimws(line)
    
    # Skip empty lines and comments
    if (nchar(line) == 0 || startsWith(line, "#")) {
      next
    }
    
    # Parse export() statements
    # Handles: export(function_name)
    if (grepl("^export\\s*\\(", line)) {
      # Extract function name from export(function_name)
      match <- stringr::str_match(line, "^export\\s*\\(\\s*([^)]+)\\s*\\)")
      if (!is.na(match[1, 2])) {
        func_name <- trimws(match[1, 2])
        # Remove quotes if present
        func_name <- gsub("^['\"]|['\"]$", "", func_name)
        exported_functions <- c(exported_functions, func_name)
      }
    }
    
    # Parse exportPattern() statements
    # Handles: exportPattern("^[[:alpha:]]+")
    if (grepl("^exportPattern\\s*\\(", line)) {
      # For patterns, we can't determine exact functions without scanning
      # the actual function definitions, so we'll note this but skip for now
      message("Note: Found exportPattern() in NAMESPACE - all matching functions will be exported")
    }
  }
  
  return(unique(exported_functions))
}

#' Check if Package Has Exported Functions
#'
#' Quick check to see if the package has any exported functions.
#'
#' @param package_path Character. Path to package root
#' @return List with export information
#' @export
check_export_status <- function(package_path = ".") {
  
  package_path <- detect_package_root(package_path, verbose = FALSE)
  exported_functions <- get_exported_functions(package_path)
  
  namespace_file <- file.path(package_path, "NAMESPACE")
  has_namespace <- file.exists(namespace_file)
  
  if (!has_namespace) {
    return(list(
      has_namespace = FALSE,
      exported_count = 0,
      exported_functions = character(0),
      message = "No NAMESPACE file found. Run devtools::document() to generate it."
    ))
  }
  
  return(list(
    has_namespace = TRUE,
    exported_count = length(exported_functions),
    exported_functions = exported_functions,
    message = if (length(exported_functions) == 0) {
      "NAMESPACE exists but no functions are exported. Add @export tags to your documentation."
    } else {
      paste("Found", length(exported_functions), "exported functions in NAMESPACE")
    }
  ))
}