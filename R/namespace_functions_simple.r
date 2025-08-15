#' Analyze Namespace Usage Opportunities
#'
#' Analyzes R package code to find functions that could be converted from
#' bare function calls to explicit namespace calls (pkg::func format).
#'
#' @param package_path Character. Path to package directory (default: ".")
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @return List with namespace analysis results
#'
#' @examples
#' \dontrun{
#' # Analyze current package
#' analysis <- analyze_namespace_usage()
#' 
#' # Print summary
#' cat(analysis$summary)
#' 
#' # View opportunities by file
#' str(analysis$opportunities)
#' }
#'
#' @export
analyze_namespace_usage <- function(package_path = ".", verbose = TRUE) {
  
  package_path <- detect_package_root(package_path, verbose = FALSE)
  
  # Build function mappings from DESCRIPTION
  mappings <- build_function_mappings(package_path, verbose = verbose)
  
  if (length(mappings) == 0) {
    return(list(
      summary = "No dependencies found in DESCRIPTION file to analyze",
      mappings = list(),
      opportunities = list(),
      stats = list(total_opportunities = 0, files_affected = 0, packages_involved = character(0))
    ))
  }
  
  # Scan R files for conversion opportunities
  functions_data <- scan_r_functions(package_path, verbose = FALSE)
  r_files <- unique(functions_data$file_path)
  
  opportunities <- list()
  
  for (file_path in r_files) {
    if (!file.exists(file_path)) next
    
    content <- readLines(file_path, warn = FALSE)
    file_opps <- find_namespace_opportunities(content, mappings)
    
    if (length(file_opps) > 0) {
      opportunities[[basename(file_path)]] <- file_opps
    }
  }
  
  # Generate summary
  total_opportunities <- sum(sapply(opportunities, length))
  packages_involved <- unique(unlist(lapply(opportunities, function(x) sapply(x, function(y) y$package))))
  
  summary_text <- paste0(
    "NAMESPACE CONVERSION OPPORTUNITIES\n",
    "=====================================\n",
    "Dependencies in DESCRIPTION: ", length(unique(unlist(mappings))), " packages\n",
    "Available function mappings: ", length(mappings), " functions\n",
    "Files with opportunities: ", length(opportunities), "\n",
    "Total conversion opportunities: ", total_opportunities, "\n",
    "Packages involved: ", paste(packages_involved, collapse = ", "), "\n"
  )
  
  return(list(
    summary = summary_text,
    mappings = mappings,
    opportunities = opportunities,
    stats = list(
      total_opportunities = total_opportunities,
      files_affected = length(opportunities),
      packages_involved = packages_involved
    )
  ))
}

#' Build Function to Package Mappings from DESCRIPTION
#'
#' @param package_path Character. Package root path
#' @param verbose Logical. Print messages
#' @return Named list mapping function names to package names
build_function_mappings <- function(package_path, verbose = TRUE) {
  
  desc_file <- file.path(package_path, "DESCRIPTION")
  if (!file.exists(desc_file)) {
    if (verbose) message("No DESCRIPTION file found")
    return(list())
  }
  
  # Parse DESCRIPTION dependencies
  deps <- parse_description_dependencies(package_path)
  target_packages <- c(deps$imports, deps$depends)
  
  if (length(target_packages) == 0) {
    if (verbose) message("No dependencies found in DESCRIPTION")
    return(list())
  }
  
  # Remove R itself
  target_packages <- target_packages[target_packages != "R"]
  
  if (verbose) message("Building function mappings for ", length(target_packages), " packages...")
  
  mappings <- list()
  
  for (pkg in target_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      tryCatch({
        exports <- getNamespaceExports(pkg)
        # Filter out internal functions and very short names
        user_functions <- exports[nchar(exports) >= 2 & !grepl("^\\.", exports)]
        
        for (func in user_functions) {
          # Only map if function name isn't already mapped to another package
          if (!func %in% names(mappings)) {
            mappings[[func]] <- pkg
          }
        }
        
        if (verbose) message("  ", pkg, ": ", length(user_functions), " functions")
        
      }, error = function(e) {
        if (verbose) message("  Error processing ", pkg, ": ", e$message)
      })
    } else {
      if (verbose) message("  Package not available: ", pkg)
    }
  }
  
  if (verbose) message("Total function mappings: ", length(mappings))
  
  return(mappings)
}

#' Find Namespace Conversion Opportunities in File Content
#'
#' @param content Character vector. File lines
#' @param mappings Named list. Function to package mappings
#' @return List of opportunities
find_namespace_opportunities <- function(content, mappings) {
  
  opportunities <- list()
  
  for (i in seq_along(content)) {
    line <- content[i]
    
    # Skip comment lines
    if (grepl("^\\s*#", line)) next
    
    # Check each mapped function
    for (func_name in names(mappings)) {
      package_name <- mappings[[func_name]]
      
      # Create pattern to match function calls without namespace
      # Look for function_name( but not package::function_name(
      escaped_func <- gsub("([\\[\\]\\(\\)\\{\\}\\^\\$\\*\\+\\?\\|\\\\\\.])", "\\\\\\1", func_name)
      pattern <- paste0("(?<!::|\\w)", escaped_func, "\\s*\\(")
      
      if (grepl(pattern, line, perl = TRUE)) {
        # Check that it's not already namespaced
        namespace_pattern <- paste0(package_name, "::", escaped_func)
        if (!grepl(namespace_pattern, line, fixed = TRUE)) {
          
          opportunities[[paste0(func_name, "_line_", i)]] <- list(
            function_name = func_name,
            package = package_name,
            line_number = i,
            original_line = line,
            suggested_replacement = gsub(
              pattern, 
              paste0(package_name, "::", func_name, "("), 
              line, 
              perl = TRUE
            )
          )
        }
      }
    }
  }
  
  return(opportunities)
}

#' Apply Namespace Conversion to Files
#'
#' @param package_path Character. Package root path
#' @param files Character vector. Specific files to convert (optional)
#' @param exclude_functions Character vector. Functions to exclude from conversion
#' @param preview_only Logical. Show changes without applying them
#' @param backup Logical. Create backup files
#' @param verbose Logical. Print progress messages
#'
#' @return List with conversion results
#'
#' @examples
#' \dontrun{
#' # Preview conversions for all files
#' result <- apply_namespace_conversion(preview_only = TRUE)
#' 
#' # Apply conversions with backup
#' result <- apply_namespace_conversion(backup = TRUE)
#' 
#' # Convert specific file only
#' result <- apply_namespace_conversion(files = "R/my_file.R")
#' 
#' # Exclude certain functions from conversion
#' result <- apply_namespace_conversion(exclude_functions = c("filter", "select"))
#' }
#'
#' @export
apply_namespace_conversion <- function(package_path = ".", 
                                     files = NULL,
                                     exclude_functions = c(),
                                     preview_only = FALSE,
                                     backup = TRUE,
                                     verbose = TRUE) {
  
  package_path <- detect_package_root(package_path, verbose = FALSE)
  
  # Get namespace opportunities
  analysis <- analyze_namespace_usage(package_path, verbose = FALSE)
  
  if (analysis$stats$total_opportunities == 0) {
    if (verbose) message("No namespace conversion opportunities found")
    return(list(files_processed = character(0), total_changes = 0))
  }
  
  # Determine which files to process
  if (is.null(files)) {
    # Get all files with opportunities
    functions_data <- scan_r_functions(package_path, verbose = FALSE)
    opportunity_files <- names(analysis$opportunities)
    all_files <- unique(functions_data$file_path)
    files_to_process <- all_files[basename(all_files) %in% opportunity_files]
  } else {
    files_to_process <- files
  }
  
  if (length(files_to_process) == 0) {
    if (verbose) message("No files to process")
    return(list(files_processed = character(0), total_changes = 0))
  }
  
  if (verbose) {
    message("Processing ", length(files_to_process), " files...")
    if (preview_only) message("PREVIEW MODE - No changes will be applied")
  }
  
  results <- list()
  total_changes <- 0
  
  for (file_path in files_to_process) {
    result <- convert_file_namespace(
      file_path = file_path,
      mappings = analysis$mappings,
      exclude_functions = exclude_functions,
      preview_only = preview_only,
      backup = backup,
      verbose = verbose
    )
    
    results[[file_path]] <- result
    total_changes <- total_changes + result$changes_made
  }
  
  if (verbose) {
    message("Namespace conversion complete!")
    message("  Files processed: ", length(files_to_process))
    message("  Total changes: ", total_changes)
    if (preview_only) message("  (Preview mode - no files modified)")
  }
  
  return(list(
    files_processed = files_to_process,
    total_changes = total_changes,
    results = results,
    preview_only = preview_only
  ))
}

#' Convert Single File to Use Namespace Calls
#'
#' @param file_path Character. Path to R file
#' @param mappings Named list. Function to package mappings  
#' @param exclude_functions Character vector. Functions to exclude
#' @param preview_only Logical. Preview mode
#' @param backup Logical. Create backup
#' @param verbose Logical. Print messages
#'
#' @return List with conversion results for the file
convert_file_namespace <- function(file_path, 
                                  mappings, 
                                  exclude_functions = c(),
                                  preview_only = FALSE,
                                  backup = TRUE,
                                  verbose = TRUE) {
  
  original_content <- readLines(file_path, warn = FALSE)
  modified_content <- original_content
  changes <- list()
  
  # Find opportunities in this file
  opportunities <- find_namespace_opportunities(original_content, mappings)
  
  if (length(opportunities) == 0) {
    return(list(
      file_path = file_path,
      changes_made = 0,
      success = TRUE,
      opportunities = list()
    ))
  }
  
  # Apply conversions
  for (opp_name in names(opportunities)) {
    opp <- opportunities[[opp_name]]
    func_name <- opp$function_name
    
    # Skip if function is excluded
    if (func_name %in% exclude_functions) {
      next
    }
    
    line_num <- opp$line_number
    original_line <- modified_content[line_num]
    package_name <- opp$package
    
    # Create the replacement pattern
    escaped_func <- gsub("([\\[\\]\\(\\)\\{\\}\\^\\$\\*\\+\\?\\|\\\\\\.])", "\\\\\\1", func_name)
    pattern <- paste0("(?<!::|\\w)", escaped_func, "\\s*\\(")
    replacement <- paste0(package_name, "::", func_name, "(")
    
    # Apply the replacement
    new_line <- gsub(pattern, replacement, original_line, perl = TRUE)
    
    if (new_line != original_line) {
      modified_content[line_num] <- new_line
      
      changes[[length(changes) + 1]] <- list(
        line_number = line_num,
        function_name = func_name,
        package = package_name,
        original = original_line,
        modified = new_line
      )
    }
  }
  
  if (verbose && length(changes) > 0) {
    message("  ", basename(file_path), ": ", length(changes), " changes")
    if (preview_only) {
      for (change in changes[1:min(3, length(changes))]) {
        message("    Line ", change$line_number, ": ", change$function_name, " -> ", change$package, "::", change$function_name)
      }
      if (length(changes) > 3) {
        message("    ... and ", length(changes) - 3, " more")
      }
    }
  }
  
  # Apply changes if not preview mode
  if (!preview_only && length(changes) > 0) {
    # Create backup
    if (backup) {
      backup_path <- paste0(file_path, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
      file.copy(file_path, backup_path)
    }
    
    # Write modified content
    writeLines(modified_content, file_path)
  }
  
  return(list(
    file_path = file_path,
    changes_made = length(changes),
    success = TRUE,
    changes = changes,
    backup_created = backup && length(changes) > 0 && !preview_only
  ))
}

#' Generate Namespace Conversion Report
#'
#' @param analysis_result List. Result from analyze_namespace_usage()
#' @return Character. Formatted report
#' @export
generate_namespace_report <- function(analysis_result) {
  
  report <- "NAMESPACE CONVERSION ANALYSIS\n"
  report <- paste0(report, "=============================\n\n")
  
  stats <- analysis_result$stats
  
  # Summary
  report <- paste0(report, "SUMMARY:\n")
  report <- paste0(report, "  Total opportunities: ", stats$total_opportunities, "\n")
  report <- paste0(report, "  Files affected: ", stats$files_affected, "\n")
  report <- paste0(report, "  Packages involved: ", length(stats$packages_involved), "\n\n")
  
  if (length(stats$packages_involved) > 0) {
    report <- paste0(report, "PACKAGES TO BE NAMESPACED:\n")
    for (pkg in sort(stats$packages_involved)) {
      # Count opportunities for this package
      pkg_count <- sum(unlist(lapply(analysis_result$opportunities, function(file_opps) {
        sum(sapply(file_opps, function(opp) opp$package == pkg))
      })))
      report <- paste0(report, "  • ", pkg, " (", pkg_count, " opportunities)\n")
    }
    report <- paste0(report, "\n")
  }
  
  # File details
  if (length(analysis_result$opportunities) > 0) {
    report <- paste0(report, "OPPORTUNITIES BY FILE:\n")
    for (file_name in names(analysis_result$opportunities)) {
      file_opps <- analysis_result$opportunities[[file_name]]
      report <- paste0(report, "  ", file_name, " (", length(file_opps), " opportunities):\n")
      
      # Group by package
      by_package <- split(file_opps, sapply(file_opps, function(x) x$package))
      
      for (pkg in names(by_package)) {
        funcs <- sapply(by_package[[pkg]], function(x) x$function_name)
        funcs_str <- paste(unique(funcs), collapse = ", ")
        report <- paste0(report, "    ", pkg, ":: ", funcs_str, "\n")
      }
      report <- paste0(report, "\n")
    }
  }
  
  return(report)
}
  