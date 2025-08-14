#' Build Function Mappings from DESCRIPTION File Dependencies
#'
#' @param package_path Character. Path to package directory (default: ".")
#' @param include_suggests Logical. Include Suggests packages (default: FALSE)
#' @param custom_mappings Named list. Additional custom function mappings
#' @param verbose Logical. Print progress messages (default: TRUE)
#' @export
build_mappings_from_description <- function(package_path = ".", 
                                           include_suggests = FALSE,
                                           custom_mappings = NULL,
                                           verbose = TRUE) {
  
  # Smart path detection
  package_path <- detect_package_root_standalone(package_path, verbose)
  
  desc_file <- file.path(package_path, "DESCRIPTION")
  if (!file.exists(desc_file)) {
    stop("DESCRIPTION file not found at: ", desc_file)
  }
  
  if (verbose) message("Reading dependencies from: ", desc_file)
  
  # Simple DESCRIPTION parsing
  desc_content <- readLines(desc_file, warn = FALSE)
  desc_text <- paste(desc_content, collapse = "\n")
  
  # Extract Imports
  imports_match <- stringr::str_match(desc_text, "Imports:\\s*([^\\n]*(?:\\n\\s+[^\\n]*)*)")
  target_packages <- c()
  
  if (!is.na(imports_match[1, 2])) {
    imports_text <- imports_match[1, 2]
    imports_text <- stringr::str_replace_all(imports_text, "\\s*\\([^)]*\\)", "")
    imports_text <- stringr::str_replace_all(imports_text, "\\s+", " ")
    packages <- stringr::str_split(imports_text, ",\\s*")[[1]]
    packages <- trimws(packages)
    target_packages <- packages[packages != "" & !is.na(packages)]
  }
  
  if (length(target_packages) == 0) {
    if (verbose) message("No packages found in Imports")
    return(if (!is.null(custom_mappings)) custom_mappings else list())
  }
  
  target_packages <- target_packages[target_packages != "R"]
  
  if (verbose) message("Building function mappings for ", length(target_packages), " packages...")
  
  mappings <- list()
  for (pkg in target_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      tryCatch({
        exports <- getNamespaceExports(pkg)
        user_functions <- exports[nchar(exports) >= 2 & !grepl("^\\.", exports)]
        
        for (func in user_functions) {
          if (!func %in% names(mappings)) {
            mappings[[func]] <- pkg
          }
        }
        if (verbose) message("  ", pkg, ": ", length(user_functions), " functions")
      }, error = function(e) {
        if (verbose) message("  Error processing ", pkg)
      })
    }
  }
  
  if (!is.null(custom_mappings)) {
    mappings <- c(mappings, custom_mappings)
  }
  
  if (verbose) message("Total function mappings created: ", length(mappings))
  return(mappings)
}

#' Detect Package Root Directory
#' @param initial_path Character. Initial path to check
#' @param verbose Logical. Print messages
detect_package_root_standalone <- function(initial_path, verbose = TRUE) {
  path <- normalizePath(initial_path, mustWork = FALSE)
  current_path <- path
  
  for (i in 1:3) {
    if (is_package_root_standalone(current_path)) {
      if (current_path != path && verbose) {
        message("Detected package root at: ", current_path, " (moved up from ", path, ")")
      }
      return(current_path)
    }
    
    parent_path <- dirname(current_path)
    if (parent_path == current_path) break
    current_path <- parent_path
  }
  
  return(path)
}

#' Check if Directory is Package Root
#' @param path Character. Path to check
is_package_root_standalone <- function(path) {
  has_description <- file.exists(file.path(path, "DESCRIPTION"))
  has_r_dir <- dir.exists(file.path(path, "R"))
  has_r_files <- length(list.files(path, pattern = "\\.R$")) > 0
  has_namespace <- file.exists(file.path(path, "NAMESPACE"))
  
  return(has_description && (has_r_dir || has_r_files || has_namespace))
}

#' Auto-convert function calls to namespace format
#' @param file_path Character. Path to R file to analyze and convert
#' @param backup Logical. Whether to create a backup of the original file
#' @param preview_only Logical. If TRUE, only shows changes without applying them
#' @param custom_mappings Named list. Custom function-to-package mappings to use
#' @param exclude_functions Character vector. Functions to exclude from conversion
#' @export
auto_namespace_functions <- function(file_path, 
                                   backup = TRUE, 
                                   preview_only = FALSE,
                                   custom_mappings = NULL,
                                   exclude_functions = c()) {
  
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  # Get package directory
  package_dir <- dirname(dirname(file_path))
  func_to_package <- build_mappings_from_description(package_dir, custom_mappings = custom_mappings, verbose = FALSE)
  
  if (length(func_to_package) == 0) {
    message("No function mappings available. Check your DESCRIPTION file.")
    return(list(
      file_path = file_path,
      total_lines = 0,
      lines_modified = 0,
      changes = list(),
      functions_converted = character(0),
      preview_only = preview_only
    ))
  }
  
  # Read and process file
  original_content <- readLines(file_path, warn = FALSE)
  modified_content <- original_content
  changes <- list()
  
  for (i in seq_along(modified_content)) {
    line <- modified_content[i]
    
    # Skip comments
    if (grepl("^\\s*#", line)) next
    
    # Process function calls
    modified_line <- line
    functions_changed <- c()
    
    for (func_name in names(func_to_package)) {
      if (func_name %in% exclude_functions) next
      
      package_name <- func_to_package[[func_name]]
      pattern <- paste0("\\b", func_name, "\\s*\\(")
      
      if (grepl(pattern, line) && !grepl(paste0(package_name, "::", func_name), line)) {
        replacement <- paste0(package_name, "::", func_name, "(")
        modified_line <- gsub(pattern, replacement, modified_line)
        functions_changed <- c(functions_changed, func_name)
      }
    }
    
    if (modified_line != line) {
      modified_content[i] <- modified_line
      changes[[length(changes) + 1]] <- list(
        line_number = i,
        original = line,
        modified = modified_line,
        functions_changed = functions_changed
      )
    }
  }
  
  # Results
  results <- list(
    file_path = file_path,
    total_lines = length(original_content),
    lines_modified = length(changes),
    changes = changes,
    functions_converted = unique(unlist(lapply(changes, function(x) x$functions_changed))),
    preview_only = preview_only
  )
  
  # Apply changes if not preview
  if (!preview_only && length(changes) > 0) {
    if (backup) {
      backup_path <- paste0(file_path, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
      file.copy(file_path, backup_path)
      results$backup_path <- backup_path
      message("Backup created: ", backup_path)
    }
    
    writeLines(modified_content, file_path)
    message("File modified: ", file_path)
    message("Lines changed: ", length(changes))
  }
  
  # Print summary
  cat("\n=== NAMESPACE CONVERSION SUMMARY ===\n")
  cat("File:", results$file_path, "\n")
  cat("Lines modified:", results$lines_modified, "\n")
  if (length(results$functions_converted) > 0) {
    cat("Functions converted:", paste(results$functions_converted, collapse = ", "), "\n")
  }
  if (results$preview_only) {
    cat("*** PREVIEW MODE - No changes applied ***\n")
  }
  cat("\n")
  
  return(results)
}