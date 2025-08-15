#' Generate Roxygen2 Documentation Template
#'
#' Creates a roxygen2 documentation template for a function with smart
#' parameter suggestions based on existing documentation patterns.
#' Enhanced with custom example templates.
#'
#' @param function_name Character. Name of the function
#' @param args Named list. Function arguments (names and default values)
#' @param param_suggestions Named list. Parameter history for suggestions (optional)
#' @param template_type Character. Type of template ("standard", "exported", "internal")
#' @param add_examples Logical. Whether to include examples section
#' @param example_template Character. Custom example template with [FUNCNAME] placeholder (optional)
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
#' # With custom example template
#' custom_template <- "dhsData <- getDHSdata(country = \"Zambia\", indicator = \"[FUNCNAME]\", year = 2018)"
#' template <- generate_roxygen_template("get_mortality", 
#'                                      list(country = NULL),
#'                                      example_template = custom_template)
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
                                     add_examples = TRUE,
                                     example_template = NULL) {
  
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
    
    if (!is.null(example_template)) {
      # Use custom template with function name substitution
      custom_example <- generate_custom_example(function_name, example_template)
      template <- paste0(template, "#' \\dontrun{\n")
      template <- paste0(template, "#' ", custom_example, "\n")
      template <- paste0(template, "#' }\n")
    } else {
      # Generate smart example
      template <- paste0(template, "#' \\dontrun{\n")
      example <- generate_function_example(function_name, args)
      template <- paste0(template, "#' ", example, "\n")
      template <- paste0(template, "#' }\n")
    }
  }
  
  # Add export tag for exported functions
  if (template_type == "exported") {
    template <- paste0(template, "#'\n")
    template <- paste0(template, "#' @export\n")
  }
  
  return(template)
}

#' Generate Custom Example from Template
#'
#' Replaces [FUNCNAME] placeholder in template with actual function name.
#'
#' @param function_name Character. Function name
#' @param example_template Character. Template with [FUNCNAME] placeholder
#' @return Character. Custom example with function name substituted
#'
#' @examples
#' \dontrun{
#' template <- "dhsData <- getDHSdata(country = \"Zambia\", indicator = \"[FUNCNAME]\", year = 2018)"
#' example <- generate_custom_example("get_mortality", template)
#' # Returns: "dhsData <- getDHSdata(country = \"Zambia\", indicator = \"get_mortality\", year = 2018)"
#' }
#'
#' @export
generate_custom_example <- function(function_name, example_template) {
  
  if (is.null(example_template) || nchar(example_template) == 0) {
    return(paste0("result <- ", function_name, "()"))
  }
  
  # Replace [FUNCNAME] with actual function name
  custom_example <- stringr::str_replace_all(example_template, "\\[FUNCNAME\\]", function_name)
  
  return(custom_example)
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

#' Generate Documentation for Multiple Functions
#'
#' @param package_data List. Result from analyze_package()
#' @param functions Character vector. Function names to document (optional, defaults to all undocumented)
#' @param template_type Character. Type of template to generate
#' @param save_to_files Logical. Whether to save directly to files (default: FALSE)
#' @param backup Logical. Create backups when saving (default: TRUE)
#' @param exported_only Logical. Only document functions that are exported in NAMESPACE (default: TRUE)
#' @param example_template Character. Custom example template with [FUNCNAME] placeholder (optional)
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
#' # Generate and save for specific functions with custom examples
#' template <- "dhsData <- getDHSdata(country = \"Zambia\", indicator = \"[FUNCNAME]\", year = 2018)"
#' docs <- generate_bulk_documentation(pkg_info, 
#'                                    functions = c("func1", "func2"),
#'                                    save_to_files = TRUE,
#'                                    example_template = template)
#' }
#'
#' @export
generate_bulk_documentation <- function(package_data, 
                                       functions = NULL, 
                                       template_type = "exported",
                                       save_to_files = FALSE,
                                       backup = TRUE,
                                       exported_only = TRUE,
                                       example_template = NULL) {
  
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
  
  if (!is.null(example_template)) {
    message("Using custom example template: ", example_template)
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
      template_type = template_type,
      example_template = example_template
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

#' Detect Missing or Empty Examples
#'
#' Analyzes existing documentation to find functions with missing or empty examples.
#'
#' @param package_data List. Result from analyze_package()
#' @param check_empty_dontrun Logical. Whether to flag empty \\dontrun{} blocks (default: TRUE)
#'
#' @return List with information about missing examples
#'
#' @examples
#' \dontrun{
#' pkg_info <- analyze_package()
#' missing_examples <- detect_missing_examples(pkg_info)
#' 
#' # Check results
#' print(missing_examples$functions_needing_examples)
#' cat(missing_examples$summary)
#' }
#'
#' @export
detect_missing_examples <- function(package_data, check_empty_dontrun = TRUE) {
  
  if (!"functions" %in% names(package_data)) {
    stop("package_data must contain 'functions' element. Use analyze_package() first.")
  }
  
  func_data <- package_data$functions
  docs_data <- package_data$documentation
  
  functions_needing_examples <- character(0)
  example_status <- list()
  
  for (i in seq_len(nrow(func_data))) {
    func_name <- func_data$function_name[i]
    has_docs <- docs_data$has_docs[i]
    existing_docs <- docs_data$existing_docs[i]
    
    example_status[[func_name]] <- list(
      has_documentation = has_docs,
      has_examples = FALSE,
      examples_empty = FALSE,
      needs_examples = FALSE
    )
    
    if (has_docs && nchar(existing_docs) > 0) {
      # Check for @examples section
      has_examples <- stringr::str_detect(existing_docs, "#'\\s*@examples")
      example_status[[func_name]]$has_examples <- has_examples
      
      if (has_examples) {
        # Extract examples section
        examples_content <- extract_examples_section(existing_docs)
        
        if (check_empty_dontrun) {
          # Check if examples are just empty \dontrun{} blocks
          is_empty <- is_examples_section_empty(examples_content)
          example_status[[func_name]]$examples_empty <- is_empty
          
          if (is_empty) {
            functions_needing_examples <- c(functions_needing_examples, func_name)
            example_status[[func_name]]$needs_examples <- TRUE
          }
        }
      } else {
        # No examples section at all
        functions_needing_examples <- c(functions_needing_examples, func_name)
        example_status[[func_name]]$needs_examples <- TRUE
      }
    } else {
      # No documentation at all
      functions_needing_examples <- c(functions_needing_examples, func_name)
      example_status[[func_name]]$needs_examples <- TRUE
    }
  }
  
  # Generate summary
  total_functions <- nrow(func_data)
  documented_functions <- sum(docs_data$has_docs, na.rm = TRUE)
  functions_with_examples <- sum(sapply(example_status, function(x) x$has_examples && !x$examples_empty))
  
  summary_text <- paste0(
    "EXAMPLES ANALYSIS SUMMARY\n",
    "=========================\n",
    "Total functions: ", total_functions, "\n",
    "Documented functions: ", documented_functions, "\n",
    "Functions with examples: ", functions_with_examples, "\n",
    "Functions needing examples: ", length(functions_needing_examples), "\n\n",
    "Functions needing examples:\n",
    paste("  •", functions_needing_examples, collapse = "\n")
  )
  
  return(list(
    functions_needing_examples = functions_needing_examples,
    example_status = example_status,
    summary = summary_text,
    stats = list(
      total_functions = total_functions,
      documented_functions = documented_functions,
      functions_with_examples = functions_with_examples,
      functions_needing_examples = length(functions_needing_examples)
    )
  ))
}

#' Extract Examples Section from Documentation
#'
#' @param docs_text Character. Documentation text
#' @return Character. Examples section content
extract_examples_section <- function(docs_text) {
  
  # Pattern to match @examples section until next @ tag or end
  # This captures everything after @examples including empty lines
  examples_pattern <- "#'\\s*@examples\\s*([\\s\\S]*?)(?=#'\\s*@\\w+|$)"
  examples_match <- stringr::str_match(docs_text, examples_pattern)
  
  if (is.na(examples_match[1, 1])) {
    return("")
  }
  
  raw_content <- examples_match[1, 2]
  
  # Clean up the content - remove empty roxygen lines but preserve structure
  lines <- strsplit(raw_content, "\n")[[1]]
  
  # Process each line
  cleaned_lines <- character(0)
  for (line in lines) {
    # Remove leading/trailing whitespace
    clean_line <- trimws(line)
    
    # Skip completely empty lines
    if (nchar(clean_line) == 0) {
      next
    }
    
    # Skip lines that are just #' with whitespace
    if (grepl("^#'\\s*$", clean_line)) {
      next
    }
    
    # For lines with content, remove the #' prefix and keep the rest
    if (grepl("^#'", clean_line)) {
      content_part <- stringr::str_replace(clean_line, "^#'\\s*", "")
      cleaned_lines <- c(cleaned_lines, content_part)
    } else {
      # Line without #' prefix (shouldn't happen in roxygen, but handle it)
      cleaned_lines <- c(cleaned_lines, clean_line)
    }
  }
  
  # Join the cleaned lines back together
  result <- paste(cleaned_lines, collapse = "\n")
  
  return(trimws(result))
}

#' Check if Examples Section is Empty or Just Empty dontrun
#'
#' @param examples_content Character. Examples section content
#' @return Logical. TRUE if examples are empty or just empty dontrun blocks
is_examples_section_empty <- function(examples_content) {
  
  if (nchar(trimws(examples_content)) == 0) {
    return(TRUE)
  }
  
  # Remove comment markers and whitespace
  clean_content <- stringr::str_replace_all(examples_content, "#'\\s*", "")
  clean_content <- trimws(clean_content)
  
  if (nchar(clean_content) == 0) {
    return(TRUE)
  }
  
  # Check if there are \dontrun{} blocks
  has_dontrun <- stringr::str_detect(clean_content, "\\\\dontrun")
  
  if (has_dontrun) {
    # Extract content inside \dontrun{} blocks
    dontrun_content <- extract_dontrun_content(clean_content)
    
    if (nchar(trimws(dontrun_content)) == 0) {
      return(TRUE)  # Empty dontrun blocks
    }
    
    # Check if dontrun content is empty or just placeholders
    return(is_dontrun_content_empty(dontrun_content))
    
  } else {
    # No \dontrun{} blocks - check the raw content directly
    return(is_raw_content_empty(clean_content))
  }
}

#' Check if Raw Content (without dontrun) is Empty or Just Placeholders
#'
#' @param content Character. Clean content without comment markers
#' @return Logical. TRUE if content is empty or just placeholders
is_raw_content_empty <- function(content) {
  
  if (nchar(trimws(content)) == 0) {
    return(TRUE)
  }
  
  # Split into lines and check each line
  lines <- strsplit(content, "\n")[[1]]
  lines <- trimws(lines)
  
  # Remove empty lines
  lines <- lines[nchar(lines) > 0]
  
  if (length(lines) == 0) {
    return(TRUE)
  }
  
  # Patterns that indicate empty or placeholder content
  empty_patterns <- c(
    "^\\s*$",                    # Empty lines
    "^\\s*#\\s*$",               # Just # with whitespace
    "^\\s*\\[.*\\]\\s*$",        # [Description] placeholders
    "^\\s*# Add example.*$",     # Common placeholder comments
    "^\\s*# Example.*$",
    "^\\s*# TODO.*$",
    "^\\s*# Your code here.*$",
    "^\\s*# Insert example.*$",
    "^\\s*# \\.\\.\\.$"          # # ...
  )
  
  substantial_lines <- character(0)
  
  for (line in lines) {
    is_empty_line <- FALSE
    
    for (pattern in empty_patterns) {
      if (stringr::str_detect(line, pattern)) {
        is_empty_line <- TRUE
        break
      }
    }
    
    if (!is_empty_line) {
      substantial_lines <- c(substantial_lines, line)
    }
  }
  
  # If we have substantial lines, it's NOT empty
  if (length(substantial_lines) > 0) {
    return(FALSE)
  }
  
  # If no substantial lines found, it's empty
  return(TRUE)
}

#' Extract Content Inside \\dontrun{} Blocks
#'
#' @param content Character. Clean content without comment markers
#' @return Character. Content inside dontrun blocks
extract_dontrun_content <- function(content) {
  
  # Pattern to match \dontrun{...} blocks, handling nested braces
  dontrun_pattern <- "\\\\dontrun\\s*\\{([^{}]*(?:\\{[^{}]*\\}[^{}]*)*)\\}"
  
  matches <- stringr::str_match_all(content, dontrun_pattern)[[1]]
  
  if (nrow(matches) == 0) {
    return("")
  }
  
  # Combine all dontrun content
  all_content <- paste(matches[, 2], collapse = "\n")
  
  return(trimws(all_content))
}

#' Check if Dontrun Content is Empty or Just Placeholders
#'
#' @param dontrun_content Character. Content inside dontrun blocks
#' @return Logical. TRUE if content is empty or just placeholders
is_dontrun_content_empty <- function(dontrun_content) {
  
  if (nchar(trimws(dontrun_content)) == 0) {
    return(TRUE)
  }
  
  # Split into lines and check each line
  lines <- strsplit(dontrun_content, "\n")[[1]]
  lines <- trimws(lines)
  
  # Remove empty lines
  lines <- lines[nchar(lines) > 0]
  
  if (length(lines) == 0) {
    return(TRUE)
  }
  
  # Patterns that indicate empty or placeholder content
  empty_patterns <- c(
    "^\\s*$",                    # Empty lines
    "^\\s*#.*$",                 # Comment-only lines
    "^\\s*\\[.*\\]\\s*$",        # [Description] placeholders
    "^\\s*# Add example.*$",     # Common placeholder comments
    "^\\s*# Example.*$",
    "^\\s*# TODO.*$",
    "^\\s*# Your code here.*$",
    "^\\s*# Insert example.*$"
  )
  
  substantial_lines <- character(0)
  
  for (line in lines) {
    is_empty_line <- FALSE
    
    for (pattern in empty_patterns) {
      if (stringr::str_detect(line, pattern)) {
        is_empty_line <- TRUE
        break
      }
    }
    
    if (!is_empty_line) {
      substantial_lines <- c(substantial_lines, line)
    }
  }
  
  # If no substantial lines found, it's empty
  if (length(substantial_lines) == 0) {
    return(TRUE)
  }
  
  # Check if remaining content is just variable assignments without real code
  # This is more conservative - only flag truly minimal content
  all_substantial <- paste(substantial_lines, collapse = "\n")
  
  # Very minimal patterns that suggest placeholder content
  minimal_patterns <- c(
    "^\\s*x\\s*<-\\s*1\\s*$",           # x <- 1
    "^\\s*data\\s*<-\\s*NULL\\s*$",     # data <- NULL
    "^\\s*result\\s*<-\\s*NULL\\s*$",   # result <- NULL
    "^\\s*\\w+\\s*<-\\s*\\w+\\s*$"     # single assignment only
  )
  
  # Only flag as empty if it's REALLY minimal
  if (length(substantial_lines) == 1) {
    for (pattern in minimal_patterns) {
      if (stringr::str_detect(all_substantial, pattern)) {
        return(TRUE)
      }
    }
  }
  
  # If we get here, there's substantial content
  return(FALSE)
}

#' Extract Content Inside \\dontrun{} Blocks
#'
#' @param content Character. Clean content without comment markers
#' @return Character. Content inside dontrun blocks
extract_dontrun_content <- function(content) {
  
  # Pattern to match \dontrun{...} blocks, handling nested braces
  dontrun_pattern <- "\\\\dontrun\\s*\\{([^{}]*(?:\\{[^{}]*\\}[^{}]*)*)\\}"
  
  matches <- stringr::str_match_all(content, dontrun_pattern)[[1]]
  
  if (nrow(matches) == 0) {
    return("")
  }
  
  # Combine all dontrun content
  all_content <- paste(matches[, 2], collapse = "\n")
  
  return(trimws(all_content))
}

#' Check if Dontrun Content is Empty or Just Placeholders
#'
#' @param dontrun_content Character. Content inside dontrun blocks
#' @return Logical. TRUE if content is empty or just placeholders
is_dontrun_content_empty <- function(dontrun_content) {
  
  if (nchar(trimws(dontrun_content)) == 0) {
    return(TRUE)
  }
  
  # Split into lines and check each line
  lines <- strsplit(dontrun_content, "\n")[[1]]
  lines <- trimws(lines)
  
  # Remove empty lines
  lines <- lines[nchar(lines) > 0]
  
  if (length(lines) == 0) {
    return(TRUE)
  }
  
  # Patterns that indicate empty or placeholder content
  empty_patterns <- c(
    "^\\s*$",                    # Empty lines
    "^\\s*#.*$",                 # Comment-only lines
    "^\\s*\\[.*\\]\\s*$",        # [Description] placeholders
    "^\\s*# Add example.*$",     # Common placeholder comments
    "^\\s*# Example.*$",
    "^\\s*# TODO.*$",
    "^\\s*# Your code here.*$",
    "^\\s*# Insert example.*$"
  )
  
  substantial_lines <- character(0)
  
  for (line in lines) {
    is_empty_line <- FALSE
    
    for (pattern in empty_patterns) {
      if (stringr::str_detect(line, pattern)) {
        is_empty_line <- TRUE
        break
      }
    }
    
    if (!is_empty_line) {
      substantial_lines <- c(substantial_lines, line)
    }
  }
  
  # If no substantial lines found, it's empty
  if (length(substantial_lines) == 0) {
    return(TRUE)
  }
  
  # Check if remaining content is just variable assignments without real code
  # This is more conservative - only flag truly minimal content
  all_substantial <- paste(substantial_lines, collapse = "\n")
  
  # Very minimal patterns that suggest placeholder content
  minimal_patterns <- c(
    "^\\s*x\\s*<-\\s*1\\s*$",           # x <- 1
    "^\\s*data\\s*<-\\s*NULL\\s*$",     # data <- NULL
    "^\\s*result\\s*<-\\s*NULL\\s*$",   # result <- NULL
    "^\\s*\\w+\\s*<-\\s*\\w+\\s*$"     # single assignment only
  )
  
  # Only flag as empty if it's REALLY minimal
  if (length(substantial_lines) == 1) {
    for (pattern in minimal_patterns) {
      if (stringr::str_detect(all_substantial, pattern)) {
        return(TRUE)
      }
    }
  }
  
  # If we get here, there's substantial content
  return(FALSE)
}

#' Add Custom Examples to Functions
#'
#' Adds or replaces examples in function documentation using a custom template.
#'
#' @param package_data List. Result from analyze_package()
#' @param example_template Character. Custom example template with [FUNCNAME] placeholder
#' @param functions Character vector. Specific functions to update (optional, defaults to all needing examples)
#' @param save_to_files Logical. Whether to save changes to files (default: FALSE)
#' @param backup Logical. Create backups when saving (default: TRUE)
#' @param exported_only Logical. Only update exported functions (default: TRUE)
#'
#' @return List with update results
#'
#' @examples
#' \dontrun{
#' # Define custom template
#' template <- "dhsData <- getDHSdata(country = \"Zambia\", indicator = \"[FUNCNAME]\", year = 2018)"
#' 
#' # Add examples to all functions needing them
#' pkg_info <- analyze_package()
#' result <- add_custom_examples(pkg_info, template, save_to_files = TRUE)
#' 
#' # Add examples to specific functions only
#' result <- add_custom_examples(pkg_info, template, 
#'                              functions = c("func1", "func2"),
#'                              save_to_files = TRUE)
#' }
#'
#' @export
add_custom_examples <- function(package_data, 
                               example_template,
                               functions = NULL,
                               save_to_files = FALSE,
                               backup = TRUE,
                               exported_only = TRUE) {
  
  if (!"functions" %in% names(package_data)) {
    stop("package_data must contain 'functions' element. Use analyze_package() first.")
  }
  
  # Detect which functions need examples
  missing_examples <- detect_missing_examples(package_data)
  
  # If no specific functions provided, use all that need examples
  if (is.null(functions)) {
    functions <- missing_examples$functions_needing_examples
  } else {
    # Filter to only those that actually need examples
    functions <- intersect(functions, missing_examples$functions_needing_examples)
  }
  
  if (length(functions) == 0) {
    message("No functions need example updates")
    return(list(updated_functions = character(0), total_updates = 0))
  }
  
  # Get exported functions if filtering is requested
  if (exported_only) {
    exported_functions <- get_exported_functions(package_data$package_path)
    if (length(exported_functions) > 0) {
      functions <- intersect(functions, exported_functions)
    }
  }
  
  if (length(functions) == 0) {
    message("No exported functions need example updates")
    return(list(updated_functions = character(0), total_updates = 0))
  }
  
  message("Adding custom examples to ", length(functions), " functions...")
  
  func_data <- package_data$functions
  docs_data <- package_data$documentation
  results <- list()
  
  for (func_name in functions) {
    message("  Processing: ", func_name)
    
    # Find function data
    func_idx <- which(func_data$function_name == func_name)
    
    if (length(func_idx) == 0) {
      warning("Function '", func_name, "' not found")
      next
    }
    
    func_idx <- func_idx[1]
    file_path <- func_data$file_path[func_idx]
    
    tryCatch({
      # Generate new documentation with custom examples
      args <- docs_data$parsed_args[[func_idx]]
      
      new_docs <- generate_roxygen_template(
        function_name = func_name,
        args = args,
        param_suggestions = package_data$parameter_history,
        template_type = "exported",
        add_examples = TRUE,
        example_template = example_template
      )
      
      results[[func_name]] <- list(
        function_name = func_name,
        file_path = file_path,
        new_documentation = new_docs,
        updated = TRUE
      )
      
      # Save to file if requested
      if (save_to_files) {
        if (docs_data$has_docs[func_idx]) {
          # Update existing documentation
          save_result <- update_function_docs(
            file_path = file_path,
            function_name = func_name,
            new_docs = new_docs,
            backup = backup
          )
        } else {
          # Add new documentation
          save_result <- save_function_docs(
            file_path = file_path,
            function_name = func_name,
            documentation = new_docs,
            backup = backup
          )
        }
        
        results[[func_name]]$saved <- save_result$success
        results[[func_name]]$save_result <- save_result
      }
      
    }, error = function(e) {
      warning("Failed to update examples for '", func_name, "': ", e$message)
      results[[func_name]] <- list(
        function_name = func_name,
        updated = FALSE,
        error = e$message
      )
    })
  }
  
  updated_count <- sum(sapply(results, function(x) isTRUE(x$updated)))
  saved_count <- if (save_to_files) sum(sapply(results, function(x) isTRUE(x$saved))) else 0
  
  message("Custom examples update complete!")
  message("  Updated: ", updated_count, " functions")
  if (save_to_files) {
    message("  Saved to files: ", saved_count, " functions")
  }
  
  return(list(
    updated_functions = names(results),
    total_updates = updated_count,
    total_saved = saved_count,
    results = results,
    template_used = example_template
  ))
}

#' Update Examples Only (Preserve Other Documentation)
#'
#' Updates just the examples section of existing documentation, preserving
#' all other parts (title, description, parameters, etc.).
#'
#' @param file_path Character. Path to R file
#' @param function_name Character. Function name
#' @param example_template Character. Custom example template
#' @param backup Logical. Create backup (default: TRUE)
#'
#' @return List with operation results
#'
#' @examples
#' \dontrun{
#' template <- "dhsData <- getDHSdata(country = \"Zambia\", indicator = \"[FUNCNAME]\", year = 2018)"
#' result <- update_examples_only("R/my_file.R", "get_mortality", template)
#' }
#'
#' @export
update_examples_only <- function(file_path, function_name, example_template, backup = TRUE) {
  
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  content <- readLines(file_path, warn = FALSE)
  file_text <- paste(content, collapse = "\n")
  
  # Find the function with its documentation
  func_pattern <- paste0("((?:#'[^\n]*\n)*)(\\s*)(", stringr::str_escape(function_name), "\\s*<-\\s*function)")
  func_match <- stringr::str_match(file_text, func_pattern)
  
  if (is.na(func_match[1, 1])) {
    stop("Function '", function_name, "' not found in file")
  }
  
  existing_docs <- func_match[1, 2]
  indentation <- func_match[1, 3]
  func_definition <- func_match[1, 4]
  
  # Generate custom example
  custom_example <- generate_custom_example(function_name, example_template)
  
  # Update or add examples section
  updated_docs <- update_examples_in_docs(existing_docs, custom_example)
  
  # Create backup
  if (backup) {
    backup_path <- paste0(file_path, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(file_path, backup_path)
  }
  
  # Replace in file content
  old_full_match <- func_match[1, 1]
  new_full_match <- paste0(updated_docs, indentation, func_definition)
  
  new_file_text <- stringr::str_replace(file_text, stringr::fixed(old_full_match), new_full_match)
  
  # Write back
  writeLines(strsplit(new_file_text, "\n")[[1]], file_path)
  
  result <- list(
    file_path = file_path,
    function_name = function_name,
    backup_created = backup,
    success = TRUE,
    example_added = custom_example
  )
  
  if (backup) {
    result$backup_path <- backup_path
  }
  
  return(result)
}

#' Update Examples Section in Documentation Text
#'
#' @param docs_text Character. Existing documentation
#' @param custom_example Character. New example content
#' @return Character. Updated documentation
update_examples_in_docs <- function(docs_text, custom_example) {
  
  # Check if @examples section exists
  has_examples <- stringr::str_detect(docs_text, "#'\\s*@examples")
  
  if (has_examples) {
    # Replace existing examples section
    examples_pattern <- "(#'\\s*@examples\\s*[\\s\\S]*?)(?=#'\\s*@\\w+|$)"
    new_examples <- paste0("#' @examples\n#' \\dontrun{\n#' ", custom_example, "\n#' }")
    
    updated_docs <- stringr::str_replace(docs_text, examples_pattern, new_examples)
  } else {
    # Add new examples section before @export (if exists) or at end
    if (stringr::str_detect(docs_text, "#'\\s*@export")) {
      # Insert before @export
      export_pattern <- "(.*)(#'\\s*@export.*)"
      new_examples <- paste0("\\1#' @examples\n#' \\dontrun{\n#' ", custom_example, "\n#' }\n#'\n\\2")
      updated_docs <- stringr::str_replace(docs_text, export_pattern, new_examples)
    } else {
      # Add at end
      if (!stringr::str_detect(docs_text, "#'\\s*$")) {
        docs_text <- paste0(docs_text, "\n#'")
      }
      updated_docs <- paste0(docs_text, "\n#' @examples\n#' \\dontrun{\n#' ", custom_example, "\n#' }")
    }
  }
  
  return(updated_docs)
}

#' Bulk Examples Update with Custom Template
#'
#' Convenience function to detect and update all functions needing examples.
#'
#' @param package_path Character. Package directory path (default: ".")
#' @param example_template Character. Custom example template with [FUNCNAME] placeholder
#' @param save_to_files Logical. Whether to save changes to files (default: TRUE)
#' @param backup Logical. Create backups (default: TRUE)
#' @param exported_only Logical. Only update exported functions (default: TRUE)
#'
#' @return List with comprehensive results
#'
#' @examples
#' \dontrun{
#' # Define your custom template
#' template <- "dhsData <- getDHSdata(country = \"Zambia\", indicator = \"[FUNCNAME]\", year = 2018)"
#' 
#' # Update all functions that need examples
#' result <- bulk_examples_update(".", template, save_to_files = TRUE)
#' 
#' # Check results
#' cat(result$summary)
#' print(result$updated_functions)
#' }
#'
#' @export
bulk_examples_update <- function(package_path = ".", 
                                example_template,
                                save_to_files = TRUE,
                                backup = TRUE,
                                exported_only = TRUE) {
  
  message("Starting bulk examples update...")
  
  # Analyze package
  package_data <- analyze_package(package_path, verbose = FALSE)
  
  # Detect missing examples
  missing_analysis <- detect_missing_examples(package_data)
  
  message("Found ", length(missing_analysis$functions_needing_examples), " functions needing examples")
  
  if (length(missing_analysis$functions_needing_examples) == 0) {
    return(list(
      summary = "No functions need example updates",
      updated_functions = character(0),
      missing_analysis = missing_analysis
    ))
  }
  
  # Update examples
  update_result <- add_custom_examples(
    package_data = package_data,
    example_template = example_template,
    save_to_files = save_to_files,
    backup = backup,
    exported_only = exported_only
  )
  
  # Generate summary
  summary_text <- paste0(
    "BULK EXAMPLES UPDATE COMPLETE\n",
    "============================\n",
    "Template used: ", example_template, "\n",
    "Functions needing examples: ", length(missing_analysis$functions_needing_examples), "\n",
    "Functions updated: ", update_result$total_updates, "\n",
    if (save_to_files) paste0("Functions saved: ", update_result$total_saved, "\n") else "",
    "Export-only mode: ", exported_only, "\n\n",
    "Updated functions:\n",
    paste("  •", update_result$updated_functions, collapse = "\n")
  )
  
  return(list(
    summary = summary_text,
    updated_functions = update_result$updated_functions,
    missing_analysis = missing_analysis,
    update_result = update_result,
    template_used = example_template
  ))
}