#' Fix Empty Examples Sections from R CMD check
#'
#' Parses R CMD check output for "Dropping empty section \\examples" warnings
#' and automatically adds placeholder examples to the affected functions.
#'
#' @param check_output Character. The copied text from R CMD check output
#' @param example_template Character. Template for examples (default: "x <- FUNCNAME(data)")
#' @param package_path Character. Path to package directory (default: ".")
#' @param preview_only Logical. If TRUE, shows what would be added without writing (default: FALSE)
#' @param backup Logical. Create backup of modified files (default: TRUE)
#'
#' @return List with information about functions processed and file operations
#'
#' @examples
#' \dontrun{
#' # Copy-paste your R CMD check output
#' check_text <- "
#' prepare_Rd: AH_TOBC_W_OTH.Rd:18-21: Dropping empty section \\examples
#' prepare_Rd: calculate_stats.Rd:25-28: Dropping empty section \\examples
#' "
#' 
#' # Preview what would be added
#' fix_empty_examples(check_text, preview_only = TRUE)
#' 
#' # Apply the fix with custom template
#' fix_empty_examples(check_text, example_template = "result <- FUNCNAME(my_data)")
#' }
#'
#' @export
fix_empty_examples <- function(check_output, 
                              example_template = "x <- FUNCNAME(data)",
                              package_path = ".",
                              preview_only = FALSE,
                              backup = TRUE) {
  
  # Smart path detection
  package_path <- detect_package_root_standalone(package_path, verbose = FALSE)
  
  # Extract function names from check output
  function_names <- extract_empty_examples_functions(check_output)
  
  if (length(function_names) == 0) {
    message("No empty examples sections found in the provided text.")
    return(list(
      functions_found = character(0),
      files_modified = character(0),
      action_taken = "none"
    ))
  }
  
  message("Found ", length(function_names), " functions with empty examples sections:")
  for (func in function_names) {
    message("  - ", func)
  }
  
  # Find R files and locate the functions
  helper <- DocumentationHelper$new(package_path)
  
  if (is.null(helper$functions_data) || nrow(helper$functions_data) == 0) {
    stop("No functions found in package. Make sure you're in the correct directory.")
  }
  
  # Match function names to files
  function_file_map <- match_functions_to_files(function_names, helper$functions_data)
  
  if (length(function_file_map) == 0) {
    message("No matching functions found in R files.")
    return(list(
      functions_found = function_names,
      functions_matched = character(0),
      files_modified = character(0),
      action_taken = "no_matches_found"
    ))
  }
  
  message("\nMatched functions to files:")
  for (func in names(function_file_map)) {
    message("  ", func, " -> ", basename(function_file_map[[func]]))
  }
  
  # Process each file
  modifications <- list()
  files_to_modify <- unique(unlist(function_file_map))
  
  for (file_path in files_to_modify) {
    file_functions <- names(function_file_map)[function_file_map == file_path]
    
    result <- process_file_for_examples(
      file_path = file_path,
      target_functions = file_functions,
      example_template = example_template,
      helper = helper,
      preview_only = preview_only,
      backup = backup
    )
    
    modifications[[file_path]] <- result
  }
  
  # Summary
  total_functions_modified <- sum(sapply(modifications, function(x) length(x$functions_modified)))
  files_modified <- names(modifications)[sapply(modifications, function(x) x$file_modified)]
  
  message("\nSummary:")
  message("  Functions processed: ", length(function_names))
  message("  Functions modified: ", total_functions_modified)
  message("  Files modified: ", length(files_modified))
  
  if (preview_only) {
    message("\n*** PREVIEW MODE - No changes applied ***")
  }
  
  return(list(
    functions_found = function_names,
    functions_matched = names(function_file_map),
    files_modified = files_modified,
    modifications = modifications,
    action_taken = if (preview_only) "preview_only" else "applied"
  ))
}

#' Extract Function Names from Empty Examples Check Output
#'
#' @param check_output Character. Raw check output text
#' @return Character vector of function names
extract_empty_examples_functions <- function(check_output) {
  
  # Pattern to match "function_name.Rd:XX-YY: Dropping empty section \\examples"
  patterns <- c(
    # Standard pattern: prepare_Rd: FUNCNAME.Rd:18-21: Dropping empty section \\examples
    "prepare_Rd:\\s+([a-zA-Z_][a-zA-Z0-9_\\.]*)\\.Rd:\\d+-\\d+:\\s+Dropping empty section\\s+\\\\examples",
    
    # Alternative patterns
    "([a-zA-Z_][a-zA-Z0-9_\\.]*)\\.Rd:\\d+-\\d+:\\s+Dropping empty section\\s+\\\\examples",
    "Dropping empty section\\s+\\\\examples.*?([a-zA-Z_][a-zA-Z0-9_\\.]*)\\.Rd"
  )
  
  all_functions <- c()
  
  # Try each pattern
  for (pattern in patterns) {
    matches <- stringr::str_match_all(check_output, pattern)[[1]]
    
    if (nrow(matches) > 0) {
      # Get the function names (second column contains the captured group)
      functions <- matches[, 2]
      all_functions <- c(all_functions, functions)
    }
  }
  
  # Debug output if no matches found
  if (length(all_functions) == 0) {
    message("Debug: No function names extracted. Looking for patterns in text:")
    
    lines <- strsplit(check_output, "\n")[[1]]
    message("Input preview (first 10 lines):")
    for (i in 1:min(10, length(lines))) {
      message("  ", i, ": ", lines[i])
    }
    
    # Check for the key phrases
    if (grepl("Dropping empty section", check_output, ignore.case = TRUE)) {
      message("\nFound 'Dropping empty section' in text.")
      
      # Extract lines with this phrase
      empty_lines <- lines[grepl("Dropping empty section", lines, ignore.case = TRUE)]
      message("Lines with 'Dropping empty section':")
      for (line in empty_lines) {
        message("  ", line)
        
        # Try to extract .Rd filename
        rd_match <- stringr::str_extract(line, "[a-zA-Z_][a-zA-Z0-9_\\.]*\\.Rd")
        if (!is.na(rd_match)) {
          func_name <- gsub("\\.Rd$", "", rd_match)
          all_functions <- c(all_functions, func_name)
        }
      }
    }
  }
  
  # Remove duplicates and sort
  all_functions <- unique(all_functions[!is.na(all_functions)])
  all_functions <- sort(all_functions)
  
  return(all_functions)
}

#' Match Function Names to R Files
#'
#' @param function_names Character vector. Function names to find
#' @param functions_data Data frame. Function data from DocumentationHelper
#' @return Named character vector. Names are function names, values are file paths
match_functions_to_files <- function(function_names, functions_data) {
  
  function_file_map <- character(0)
  
  for (func_name in function_names) {
    # Look for exact matches first
    exact_matches <- which(functions_data$function_name == func_name)
    
    if (length(exact_matches) > 0) {
      # Use first match if multiple found
      function_file_map[func_name] <- functions_data$file_path[exact_matches[1]]
    } else {
      # Try partial matches (in case of slight name differences)
      partial_matches <- which(grepl(func_name, functions_data$function_name, fixed = TRUE))
      
      if (length(partial_matches) > 0) {
        function_file_map[func_name] <- functions_data$file_path[partial_matches[1]]
        message("  Note: Using partial match for '", func_name, "' -> '", 
                functions_data$function_name[partial_matches[1]], "'")
      } else {
        message("  Warning: Function '", func_name, "' not found in R files")
      }
    }
  }
  
  return(function_file_map)
}

#' Process a Single File to Add Examples
#'
#' @param file_path Character. Path to R file
#' @param target_functions Character vector. Function names to modify
#' @param example_template Character. Example template
#' @param helper DocumentationHelper. Helper object
#' @param preview_only Logical. Preview mode
#' @param backup Logical. Create backup
#' @return List with modification details
process_file_for_examples <- function(file_path, target_functions, example_template, 
                                     helper, preview_only, backup) {
  
  # Read file content
  original_content <- readLines(file_path, warn = FALSE)
  modified_content <- original_content
  
  functions_modified <- c()
  modifications <- list()
  file_modified <- FALSE
  
  # Get function data for this file
  file_functions <- helper$functions_data[helper$functions_data$file_path == file_path, ]
  
  for (func_name in target_functions) {
    # Find this function in the file data
    func_row <- which(file_functions$function_name == func_name)
    
    if (length(func_row) == 0) {
      message("  Warning: Function '", func_name, "' not found in ", basename(file_path))
      next
    }
    
    func_data <- file_functions[func_row[1], ]
    
    # Check if function already has examples
    if (has_examples_in_docs(func_data$existing_docs)) {
      message("  Skipping '", func_name, "' - already has examples")
      next
    }
    
    # Generate new documentation with examples
    new_docs <- add_examples_to_function_docs(
      existing_docs = func_data$existing_docs,
      function_name = func_name,
      args = func_data$parsed_args[[1]],
      example_template = example_template
    )
    
    # Replace function documentation in file content
    old_function_text <- func_data$full_function
    
    # Remove existing docs and add new docs
    func_without_docs <- stringr::str_replace(old_function_text, "^(#'[^\n]*\n)*", "")
    new_function_text <- paste0(new_docs, "\n", func_without_docs)
    
    # Find and replace in content
    content_text <- paste(modified_content, collapse = "\n")
    content_text <- stringr::str_replace(content_text, stringr::fixed(old_function_text), new_function_text)
    modified_content <- strsplit(content_text, "\n")[[1]]
    
    functions_modified <- c(functions_modified, func_name)
    file_modified <- TRUE
    
    modifications[[func_name]] <- list(
      function_name = func_name,
      old_docs = func_data$existing_docs,
      new_docs = new_docs
    )
    
    message("  Added example to '", func_name, "'")
  }
  
  # Apply changes if not preview mode
  if (!preview_only && file_modified) {
    # Create backup
    if (backup) {
      backup_path <- paste0(file_path, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
      file.copy(file_path, backup_path)
      message("  Backup created: ", backup_path)
    }
    
    # Write modified content
    writeLines(modified_content, file_path)
    message("  File updated: ", file_path)
  }
  
  return(list(
    file_path = file_path,
    functions_modified = functions_modified,
    file_modified = file_modified,
    modifications = modifications
  ))
}

#' Check if Documentation Already Has Examples
#'
#' @param docs Character. Existing documentation
#' @return Logical. TRUE if examples section exists and is not empty
has_examples_in_docs <- function(docs) {
  if (is.null(docs) || nchar(trimws(docs)) == 0) {
    return(FALSE)
  }
  
  # Look for @examples tag with content after it
  examples_pattern <- "@examples\\s*\n\\s*#'\\s*[^\\s]"
  has_examples <- grepl(examples_pattern, docs)
  
  return(has_examples)
}

#' Add Examples Section to Function Documentation
#'
#' @param existing_docs Character. Current documentation
#' @param function_name Character. Name of the function
#' @param args List. Function arguments
#' @param example_template Character. Template for examples
#' @return Character. Updated documentation
add_examples_to_function_docs <- function(existing_docs, function_name, args, example_template) {
  
  # Generate example by replacing FUNCNAME placeholder
  example_code <- stringr::str_replace_all(example_template, "FUNCNAME", function_name)
  
  # If no existing docs, create minimal template
  if (is.null(existing_docs) || nchar(trimws(existing_docs)) == 0) {
    template <- paste0("#' ", function_name, "\n#'\n#' [Description]\n#'\n")
    
    # Add parameters if any
    if (length(args) > 0) {
      for (arg_name in names(args)) {
        template <- paste0(template, "#' @param ", arg_name, " [Description]\n")
      }
    }
    
    template <- paste0(template, "#' @return [Description]\n")
    template <- paste0(template, "#' @examples\n")
    template <- paste0(template, "#' \\dontrun{\n")
    template <- paste0(template, "#' ", example_code, "\n")
    template <- paste0(template, "#' }\n")
    template <- paste0(template, "#' @export\n")
    
    return(template)
  }
  
  # If docs exist, check if they already have @examples
  if (grepl("@examples", existing_docs)) {
    # Replace empty examples section
    if (grepl("@examples\\s*\n\\s*(#'\\s*\n)*\\s*(#'\\s*@|#'\\s*$)", existing_docs)) {
      # Empty examples section - replace it
      new_examples <- paste0("@examples\n#' \\dontrun{\n#' ", example_code, "\n#' }")
      docs_with_examples <- stringr::str_replace(
        existing_docs,
        "@examples\\s*\n(\\s*#'\\s*\n)*",
        paste0(new_examples, "\n#' ")
      )
      return(docs_with_examples)
    } else {
      # Examples already exist - don't modify
      return(existing_docs)
    }
  } else {
    # Add examples section before @export or at the end
    if (grepl("@export", existing_docs)) {
      # Insert before @export
      new_examples <- paste0("#' @examples\n#' \\dontrun{\n#' ", example_code, "\n#' }\n#' ")
      docs_with_examples <- stringr::str_replace(
        existing_docs,
        "#'\\s*@export",
        paste0(new_examples, "@export")
      )
      return(docs_with_examples)
    } else {
      # Add at the end
      new_examples <- paste0("\n#' @examples\n#' \\dontrun{\n#' ", example_code, "\n#' }\n#' @export")
      return(paste0(existing_docs, new_examples))
    }
  }
}

#' Interactive Function to Fix Empty Examples
#'
#' @param example_template Character. Template for examples
#' @param package_path Character. Path to package
#' @return Result from fix_empty_examples()
#' @export
fix_empty_examples_interactive <- function(example_template = "x <- FUNCNAME(data)",
                                          package_path = ".") {
  
  cat("=== Interactive Empty Examples Fixer ===\n\n")
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
  result <- fix_empty_examples(check_output, example_template, package_path, preview_only = TRUE)
  
  if (result$action_taken == "preview_only" && length(result$functions_matched) > 0) {
    response <- readline("\nApply these changes? (y/n): ")
    if (tolower(substr(response, 1, 1)) == "y") {
      result <- fix_empty_examples(check_output, example_template, package_path, preview_only = FALSE)
      cat("\nChanges applied successfully!\n")
    } else {
      cat("\nChanges not applied.\n")
    }
  }
  
  return(invisible(result))
}