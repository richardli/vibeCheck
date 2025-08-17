#' Analyze Package Dependencies (FIXED)
#'
#' Comprehensive dependency analysis including DESCRIPTION file parsing,
#' code scanning for package usage, and missing package detection.
#' Enhanced with function-level dependency tracking and smart path detection.
#'
#' @param package_path Character. Path to package root (default: auto-detect)
#' @param functions_data Data.frame. Function data from scan_r_functions
#' @param verbose Logical. Print progress messages
#' @param include_suggests Logical. Whether to include Suggests packages in unused check (default: FALSE)
#'
#' @return List with dependency analysis results including function-level tracking
#'
#' @examples
#' \dontrun{
#' # Auto-detect and analyze dependencies for current package
#' deps <- analyze_package_dependencies()
#' 
#' # With existing function data
#' functions <- scan_r_functions()
#' deps <- analyze_package_dependencies(functions_data = functions)
#' 
#' # Include suggests packages in unused check
#' deps <- analyze_package_dependencies(include_suggests = TRUE)
#' }
#'
#' @export
analyze_package_dependencies <- function(package_path = NULL, functions_data = NULL, verbose = TRUE, include_suggests = FALSE) {
  
  # Smart path detection
  if (is.null(package_path)) {
    package_path <- smart_detect_package_path()
  }
  
  # If no functions data provided, scan the package
  if (is.null(functions_data)) {
    functions_data <- scan_r_functions(package_path, verbose = FALSE)
  }
  
  # Analyze DESCRIPTION file
  description_deps <- parse_description_dependencies(package_path)
  
  # Scan code for package usage
  # Use the function that actually exists in your original code
  code_deps <- scan_code_dependencies(functions_data, package_path)
  
  # Find missing packages
  all_detected <- unique(c(
    code_deps$library_calls,
    code_deps$require_calls,
    code_deps$namespace_calls,
    code_deps$roxygen_imports,
    code_deps$suspected_packages
  ))
  
  missing_packages <- check_missing_packages(all_detected)
  
  # Compare declared vs detected
  # ALL declared packages include Imports, Depends, AND Suggests
  all_declared <- unique(c(description_deps$imports, description_deps$depends, description_deps$suggests))
  
  # For "unused" check, exclude Suggests unless specified
  declared_for_unused_check <- c(description_deps$imports, description_deps$depends)
  if (include_suggests) {
    declared_for_unused_check <- c(declared_for_unused_check, description_deps$suggests)
  }
  declared_for_unused_check <- unique(declared_for_unused_check)
  
  # UNDECLARED: Packages you use in code but didn't declare ANYWHERE in DESCRIPTION
  # (not in Imports, Depends, OR Suggests)
  undeclared <- setdiff(all_detected, all_declared)
  
  # UNUSED: Packages you declared in DESCRIPTION but don't actually use in code
  # Only check Imports/Depends unless include_suggests=TRUE
  unused <- setdiff(declared_for_unused_check, all_detected)
  
  # SUGGESTED BUT USED: Packages in Suggests that are actually used in code
  # (This is often okay, but worth noting)
  suggested_but_used <- intersect(all_detected, description_deps$suggests)
  
  result <- list(
    description = description_deps,
    detected = code_deps,
    missing = missing_packages,
    undeclared = undeclared,  # These need to be added to DESCRIPTION (not in any field)
    unused = unused,          # These can be removed from DESCRIPTION  
    suggested_but_used = suggested_but_used,  # Packages in Suggests that you actually use
    function_usage = code_deps$function_usage,  # New: which functions use which packages
    usage_summary = code_deps$usage_summary,    # New: summary by package
    summary = list(
      total_detected = length(all_detected),
      total_declared = length(all_declared),
      missing_count = length(missing_packages),
      undeclared_count = length(undeclared),
      unused_count = length(unused),
      suggested_but_used_count = length(suggested_but_used),
      suggests_included = include_suggests
    )
  )
  
  if (verbose) {
    message("Dependency analysis:")
    message("  Detected packages: ", length(all_detected))
    message("  Declared packages: ", length(all_declared))
    message("  Missing packages: ", length(missing_packages))
    message("  Undeclared packages: ", length(undeclared))
    message("  Unused packages: ", length(unused))
    if (length(suggested_but_used) > 0) {
      message("  Suggested but used: ", length(suggested_but_used))
    }
    if (!include_suggests && length(description_deps$suggests) > 0) {
      message("  (Suggests packages not included in unused check)")
    }
  }
  
  return(result)
}
#' Parse DESCRIPTION File Dependencies
#'
#' @param package_path Character. Package root path
#' @return List with declared dependencies
#' @export
parse_description_dependencies <- function(package_path) {
  
  desc_file <- file.path(package_path, "DESCRIPTION")
  
  if (!file.exists(desc_file)) {
    return(list(
      imports = character(0),
      depends = character(0),
      suggests = character(0),
      status = "No DESCRIPTION file found"
    ))
  }
  
  desc_content <- readLines(desc_file, warn = FALSE)
  desc_text <- paste(desc_content, collapse = "\n")
  
  # Extract different dependency types
  imports <- extract_description_field(desc_text, "Imports")
  depends <- extract_description_field(desc_text, "Depends")
  suggests <- extract_description_field(desc_text, "Suggests")
  
  return(list(
    imports = imports,
    depends = depends,
    suggests = suggests,
    status = "OK"
  ))
}

#' Extract Package Names from DESCRIPTION Field
#'
#' @param desc_text Character. Full DESCRIPTION content
#' @param field_name Character. Field name (e.g., "Imports")
#' @return Character vector of package names
extract_description_field <- function(desc_text, field_name) {
  
  pattern <- paste0(field_name, ":\\s*([^\\n]*(?:\\n\\s+[^\\n]*)*)")
  match <- stringr::str_match(desc_text, pattern)
  
  if (is.na(match[1, 2])) {
    return(character(0))
  }
  
  field_text <- match[1, 2]
  
  # Remove version specifications and whitespace
  clean_text <- stringr::str_replace_all(field_text, "\\s*\\([^)]*\\)", "")
  clean_text <- stringr::str_replace_all(clean_text, "\\s+", " ")
  
  # Split by comma and clean up
  packages <- stringr::str_split(clean_text, ",\\s*")[[1]]
  packages <- trimws(packages)
  packages <- packages[packages != "" & !is.na(packages)]
  
  # Remove R itself
  packages <- packages[packages != "R"]
  
  return(packages)
}

#' Scan Code for Package Dependencies (Enhanced)
#'
#' @param functions_data Data.frame. Function information
#' @param package_path Character. Package root path
#' @return List with detected dependencies and function-level usage
#' @export
scan_code_dependencies <- function(functions_data, package_path) {
  # Call the enhanced version for backward compatibility
  return(scan_code_dependencies_enhanced(functions_data, package_path))
}

#' Enhanced Code Dependency Scanning with Function-Level Tracking
#'
#' @param functions_data Data.frame. Function information
#' @param package_path Character. Package root path
#' @return List with detected dependencies and function-level usage
scan_code_dependencies_enhanced <- function(functions_data, package_path) {
  
  if (nrow(functions_data) == 0) {
    return(list(
      library_calls = character(0),
      require_calls = character(0),
      namespace_calls = character(0),
      roxygen_imports = character(0),
      suspected_packages = character(0),
      function_usage = data.frame(),
      usage_summary = list()
    ))
  }
  
  # Get all R files
  r_files <- unique(functions_data$file_path)
  
  all_deps <- list(
    library_calls = character(0),
    require_calls = character(0),
    namespace_calls = character(0),
    roxygen_imports = character(0),
    suspected_packages = character(0)
  )
  
  # Track which functions use which packages
  function_usage <- data.frame(
    file_path = character(0),
    function_name = character(0),
    package = character(0),
    usage_type = character(0),  # "library", "require", "namespace", "roxygen", "suspected"
    stringsAsFactors = FALSE
  )
  
  # Scan each file
  for (file_path in r_files) {
    file_deps <- scan_file_dependencies_enhanced(file_path, functions_data)
    
    # Combine global results
    for (dep_type in names(all_deps)) {
      if (dep_type %in% names(file_deps)) {
        all_deps[[dep_type]] <- c(all_deps[[dep_type]], file_deps[[dep_type]])
      }
    }
    
    # Add function-level usage tracking
    if ("function_usage" %in% names(file_deps) && nrow(file_deps$function_usage) > 0) {
      function_usage <- rbind(function_usage, file_deps$function_usage)
    }
  }
  
  # Remove duplicates from global lists
  all_deps <- lapply(all_deps, unique)
  
  # Create usage summary by package
  usage_summary <- create_usage_summary(function_usage)
  
  # Add additional checks for common hidden dependencies
  additional_deps <- check_additional_dependencies(package_path)
  all_deps <- merge_additional_deps(all_deps, additional_deps)
  
  return(c(all_deps, list(
    function_usage = function_usage,
    usage_summary = usage_summary
  )))
}

#' Scan Single File for Dependencies (Enhanced)
#'
#' @param file_path Character. Path to R file
#' @param functions_data Data.frame. Function information for context
#' @return List with detected dependencies and function usage
scan_file_dependencies <- function(file_path) {
  # For backward compatibility, call enhanced version without function context
  return(scan_file_dependencies_enhanced(file_path, data.frame()))
}

#' Enhanced File Dependency Scanning with Function Context
#'
#' @param file_path Character. Path to R file
#' @param functions_data Data.frame. Function information for context
#' @return List with detected dependencies and function usage
scan_file_dependencies_enhanced <- function(file_path, functions_data) {
  
  content <- readLines(file_path, warn = FALSE)
  file_text <- paste(content, collapse = "\n")
  
  deps <- list()
  function_usage <- data.frame(
    file_path = character(0),
    function_name = character(0),
    package = character(0),
    usage_type = character(0),
    stringsAsFactors = FALSE
  )
  
  # Get functions in this file
  file_functions <- functions_data[functions_data$file_path == file_path, ]
  
  # Library calls: library(package)
  library_pattern <- "library\\s*\\(\\s*(['\"]?)([^'\"\\)]+)\\1\\s*\\)"
  library_matches <- stringr::str_match_all(file_text, library_pattern)[[1]]
  if (nrow(library_matches) > 0) {
    deps$library_calls <- library_matches[, 3]
    
    # Add to function usage (these are typically global)
    for (pkg in deps$library_calls) {
      for (i in seq_len(nrow(file_functions))) {
        function_usage <- rbind(function_usage, data.frame(
          file_path = file_path,
          function_name = file_functions$function_name[i],
          package = pkg,
          usage_type = "library",
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Require calls: require(package)
  require_pattern <- "require\\s*\\(\\s*(['\"]?)([^'\"\\)]+)\\1\\s*\\)"
  require_matches <- stringr::str_match_all(file_text, require_pattern)[[1]]
  if (nrow(require_matches) > 0) {
    deps$require_calls <- require_matches[, 3]
    
    # Add to function usage
    for (pkg in deps$require_calls) {
      for (i in seq_len(nrow(file_functions))) {
        function_usage <- rbind(function_usage, data.frame(
          file_path = file_path,
          function_name = file_functions$function_name[i],
          package = pkg,
          usage_type = "require",
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Namespace calls: package::function
  namespace_pattern <- "([a-zA-Z][a-zA-Z0-9\\.]*)::"
  namespace_matches <- stringr::str_match_all(file_text, namespace_pattern)[[1]]
  if (nrow(namespace_matches) > 0) {
    deps$namespace_calls <- unique(namespace_matches[, 2])
    
    # For namespace calls, try to determine which function uses which package
    for (pkg in deps$namespace_calls) {
      pkg_pattern <- paste0("\\b", stringr::str_escape(pkg), "::")
      pkg_matches <- stringr::str_locate_all(file_text, pkg_pattern)[[1]]
      
      if (nrow(pkg_matches) > 0) {
        for (j in seq_len(nrow(pkg_matches))) {
          match_pos <- pkg_matches[j, 1]
          
          # Find which function this usage belongs to
          func_name <- find_function_at_position(file_text, match_pos, file_functions)
          
          if (!is.na(func_name)) {
            function_usage <- rbind(function_usage, data.frame(
              file_path = file_path,
              function_name = func_name,
              package = pkg,
              usage_type = "namespace",
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  # Roxygen imports: @import package, @importFrom package
  import_pattern <- "#'\\s*@import(?:From)?\\s+([a-zA-Z][a-zA-Z0-9\\.]*)"
  import_matches <- stringr::str_match_all(file_text, import_pattern)[[1]]
  if (nrow(import_matches) > 0) {
    deps$roxygen_imports <- import_matches[, 2]
    
    # Associate roxygen imports with the function they document
    for (pkg in deps$roxygen_imports) {
      import_pattern_specific <- paste0("#'\\s*@import(?:From)?\\s+", stringr::str_escape(pkg))
      import_positions <- stringr::str_locate_all(file_text, import_pattern_specific)[[1]]
      
      if (nrow(import_positions) > 0) {
        for (j in seq_len(nrow(import_positions))) {
          match_pos <- import_positions[j, 1]
          func_name <- find_function_after_position(file_text, match_pos, file_functions)
          
          if (!is.na(func_name)) {
            function_usage <- rbind(function_usage, data.frame(
              file_path = file_path,
              function_name = func_name,
              package = pkg,
              usage_type = "roxygen",
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  # Suspected package usage based on function patterns
  suspected_packages <- detect_suspected_packages(file_text)
  if (length(suspected_packages) > 0) {
    deps$suspected_packages <- suspected_packages
    
    # For suspected packages, associate with functions that likely use them
    for (pkg in suspected_packages) {
      functions_using_pkg <- find_functions_using_package_patterns(file_text, pkg, file_functions)
      
      for (func_name in functions_using_pkg) {
        function_usage <- rbind(function_usage, data.frame(
          file_path = file_path,
          function_name = func_name,
          package = pkg,
          usage_type = "suspected",
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  deps$function_usage <- function_usage
  return(deps)
}

#' Get Package Name from DESCRIPTION File
#'
#' @param package_path Character. Path to package root
#' @return Character. Package name or NULL if not found
get_package_name_from_description <- function(package_path) {
  
  desc_file <- file.path(package_path, "DESCRIPTION")
  
  if (!file.exists(desc_file)) {
    return(NULL)
  }
  
  tryCatch({
    desc_content <- readLines(desc_file, warn = FALSE)
    
    # Look for Package: line
    package_line <- grep("^Package:", desc_content, value = TRUE)
    
    if (length(package_line) > 0) {
      package_name <- trimws(gsub("^Package:\\s*", "", package_line[1]))
      return(package_name)
    }
    
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

#' Detect Suspected Package Usage from Code Patterns (UPDATED - Removed False Positives)
#'
#' @param code Character. Code content
#' @return Character vector of suspected packages
#' @export
detect_suspected_packages <- function(code) {
  
  # Common function patterns that suggest package usage
  # REMOVED purrr and testthat due to false positives
  patterns <- list(
    "ggplot2" = c("ggplot\\s*\\(", "geom_", "aes\\s*\\(", "theme_", "scale_", "labs\\s*\\("),
    "dplyr" = c("mutate\\s*\\(", "filter\\s*\\(", "select\\s*\\(", "arrange\\s*\\(", 
                "summarise\\s*\\(", "group_by\\s*\\(", "%>%"),
    "tidyr" = c("pivot_longer\\s*\\(", "pivot_wider\\s*\\(", "separate\\s*\\(", 
                "unite\\s*\\(", "nest\\s*\\("),
    "shiny" = c("fluidPage\\s*\\(", "renderPlot\\s*\\(", "observeEvent\\s*\\(", 
                "reactive\\s*\\(", "actionButton\\s*\\("),
    "DT" = c("datatable\\s*\\(", "renderDataTable\\s*\\("),
    "stringr" = c("str_detect\\s*\\(", "str_replace\\s*\\(", "str_extract\\s*\\(", 
                  "str_split\\s*\\(", "str_match\\s*\\("),
    "readr" = c("read_csv\\s*\\(", "write_csv\\s*\\(", "read_delim\\s*\\("),
    "lubridate" = c("ymd\\s*\\(", "mdy\\s*\\(", "as_date\\s*\\(", "interval\\s*\\(")
    # Removed "purrr" and "testthat" patterns due to false positives
  )
  
  suspected <- character(0)
  
  for (pkg in names(patterns)) {
    for (pattern in patterns[[pkg]]) {
      if (stringr::str_detect(code, pattern)) {
        suspected <- c(suspected, pkg)
        break  # Found one pattern for this package, move to next
      }
    }
  }
  
  return(unique(suspected))
}

#' Get Specific Function Patterns for Detailed Extraction (UPDATED - Removed False Positives)
#'
#' @param package_name Character. Package name
#' @return List. Pattern information with function names
get_specific_function_patterns <- function(package_name) {
  
  # REMOVED purrr and testthat patterns to avoid false positives
  pattern_map <- list(
    "ggplot2" = list(
      list(pattern = "ggplot\\s*\\(", function_name = "ggplot"),
      list(pattern = "(geom_\\w+)\\s*\\(", extract_group = 1),
      list(pattern = "aes\\s*\\(", function_name = "aes"),
      list(pattern = "(theme_\\w+)\\s*\\(", extract_group = 1),
      list(pattern = "(scale_\\w+)\\s*\\(", extract_group = 1),
      list(pattern = "labs\\s*\\(", function_name = "labs")
    ),
    "dplyr" = list(
      list(pattern = "mutate\\s*\\(", function_name = "mutate"),
      list(pattern = "filter\\s*\\(", function_name = "filter"),
      list(pattern = "select\\s*\\(", function_name = "select"),
      list(pattern = "arrange\\s*\\(", function_name = "arrange"),
      list(pattern = "summarise\\s*\\(", function_name = "summarise"),
      list(pattern = "group_by\\s*\\(", function_name = "group_by"),
      list(pattern = "%>%", function_name = "%>%")
    ),
    "tidyr" = list(
      list(pattern = "pivot_longer\\s*\\(", function_name = "pivot_longer"),
      list(pattern = "pivot_wider\\s*\\(", function_name = "pivot_wider"),
      list(pattern = "separate\\s*\\(", function_name = "separate"),
      list(pattern = "unite\\s*\\(", function_name = "unite"),
      list(pattern = "nest\\s*\\(", function_name = "nest")
    ),
    "stringr" = list(
      list(pattern = "(str_\\w+)\\s*\\(", extract_group = 1)
    ),
    "lubridate" = list(
      list(pattern = "ymd\\s*\\(", function_name = "ymd"),
      list(pattern = "mdy\\s*\\(", function_name = "mdy"),
      list(pattern = "as_date\\s*\\(", function_name = "as_date")
    )
    # Removed purrr and testthat patterns
  )
  
  if (package_name %in% names(pattern_map)) {
    return(pattern_map[[package_name]])
  }
  
  return(list())
}

#' Check Which Packages Are Missing/Not Installed
#'
#' @param packages Character vector. Package names to check
#' @return Character vector of missing packages
#' @export
check_missing_packages <- function(packages) {
  
  if (length(packages) == 0) {
    return(character(0))
  }
  
  # Remove base packages that are always available
  base_packages <- c("base", "utils", "stats", "graphics", "grDevices", "methods", "datasets")
  packages <- setdiff(packages, base_packages)
  
  missing <- character(0)
  
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing <- c(missing, pkg)
    }
  }
  
  return(missing)
}

#' Install Missing Packages
#'
#' @param packages Character vector. Package names to install
#' @param dependencies Logical. Install dependencies (default: TRUE)
#' @param verbose Logical. Print progress messages (default: TRUE)
#' @return List with installation results
#' @export
install_missing_packages <- function(packages, dependencies = TRUE, verbose = TRUE) {
  
  if (length(packages) == 0) {
    if (verbose) message("No packages to install")
    return(list(success = character(0), failed = character(0)))
  }
  
  if (verbose) message("Installing ", length(packages), " packages...")
  
  success <- character(0)
  failed <- character(0)
  
  for (pkg in packages) {
    if (verbose) message("Installing ", pkg, "...")
    
    tryCatch({
      utils::install.packages(pkg, dependencies = dependencies, quiet = !verbose)
      
      # Verify installation
      if (requireNamespace(pkg, quietly = TRUE)) {
        success <- c(success, pkg)
        if (verbose) message("  ✓ ", pkg, " installed successfully")
      } else {
        failed <- c(failed, pkg)
        if (verbose) message("  ✗ ", pkg, " installation verification failed")
      }
      
    }, error = function(e) {
      failed <- c(failed, pkg)
      if (verbose) message("  ✗ ", pkg, " installation failed: ", e$message)
    })
  }
  
  if (verbose) {
    message("Installation complete:")
    message("  Success: ", length(success))
    message("  Failed: ", length(failed))
  }
  
  return(list(success = success, failed = failed))
}

#' Generate Dependency Report (UPDATED)
#'
#' @param dependency_data List. Result from analyze_package_dependencies
#' @return Character. Formatted report
#' @export
generate_dependency_report <- function(dependency_data) {
  
  report <- "PACKAGE DEPENDENCY ANALYSIS\n"
  report <- paste0(report, "==========================\n\n")
  
  # Summary
  summary <- dependency_data$summary
  report <- paste0(report, "SUMMARY:\n")
  report <- paste0(report, "  Detected packages: ", summary$total_detected, "\n")
  report <- paste0(report, "  Declared packages: ", summary$total_declared, "\n")
  report <- paste0(report, "  Missing packages: ", summary$missing_count, "\n")
  report <- paste0(report, "  Undeclared packages: ", summary$undeclared_count, "\n")
  report <- paste0(report, "  Unused declarations: ", summary$unused_count, "\n")
  
  if ("suggested_but_used_count" %in% names(summary) && summary$suggested_but_used_count > 0) {
    report <- paste0(report, "  Suggested but used: ", summary$suggested_but_used_count, "\n")
  }
  
  if ("suggests_included" %in% names(summary)) {
    report <- paste0(report, "  Suggests included in unused check: ", summary$suggests_included, "\n")
  }
  report <- paste0(report, "\n")
  
  # Missing packages
  if (length(dependency_data$missing) > 0) {
    report <- paste0(report, "❌ MISSING PACKAGES:\n")
    for (pkg in dependency_data$missing) {
      report <- paste0(report, "  • ", pkg, "\n")
    }
    report <- paste0(report, "\n")
  }
  
  # Undeclared packages (truly undeclared - not in any DESCRIPTION field)
  if (length(dependency_data$undeclared) > 0) {
    report <- paste0(report, "⚠️  UNDECLARED PACKAGES (used but not in DESCRIPTION at all):\n")
    for (pkg in dependency_data$undeclared) {
      if ("usage_summary" %in% names(dependency_data) && pkg %in% names(dependency_data$usage_summary)) {
        usage <- dependency_data$usage_summary[[pkg]]
        report <- paste0(report, "  • ", pkg, " (used in: ", paste(usage$functions, collapse = ", "), ")\n")
      } else {
        report <- paste0(report, "  • ", pkg, "\n")
      }
    }
    report <- paste0(report, "\n")
  }
  
  # Suggested but used packages (informational - not necessarily a problem)
  if ("suggested_but_used" %in% names(dependency_data) && length(dependency_data$suggested_but_used) > 0) {
    report <- paste0(report, "💡 SUGGESTED BUT USED (in Suggests but actually used in code):\n")
    for (pkg in dependency_data$suggested_but_used) {
      if ("usage_summary" %in% names(dependency_data) && pkg %in% names(dependency_data$usage_summary)) {
        usage <- dependency_data$usage_summary[[pkg]]
        report <- paste0(report, "  • ", pkg, " (used in: ", paste(usage$functions, collapse = ", "), ")\n")
      } else {
        report <- paste0(report, "  • ", pkg, "\n")
      }
    }
    report <- paste0(report, "  Note: This is often okay - Suggests packages can be used conditionally\n")
    report <- paste0(report, "\n")
  }
  
  # Unused packages
  if (length(dependency_data$unused) > 0) {
    report <- paste0(report, "📋 POSSIBLY UNUSED PACKAGES (declared but not detected):\n")
    for (pkg in dependency_data$unused) {
      report <- paste0(report, "  • ", pkg, "\n")
    }
    if ("suggests_included" %in% names(summary) && !summary$suggests_included) {
      report <- paste0(report, "  Note: Suggests packages were not included in this check\n")
    }
    report <- paste0(report, "\n")
  }
  
  # Usage by package (if available)
  if ("usage_summary" %in% names(dependency_data) && length(dependency_data$usage_summary) > 0) {
    report <- paste0(report, "📦 PACKAGE USAGE DETAILS:\n")
    for (pkg in sort(names(dependency_data$usage_summary))) {
      usage <- dependency_data$usage_summary[[pkg]]
      
      # Determine package status
      status <- ""
      if (pkg %in% dependency_data$description$imports) {
        status <- "(Imports)"
      } else if (pkg %in% dependency_data$description$depends) {
        status <- "(Depends)"
      } else if (pkg %in% dependency_data$description$suggests) {
        status <- "(Suggests)"
      } else {
        status <- "(UNDECLARED)"
      }
      
      report <- paste0(report, "  ", pkg, " ", status, ":\n")
      report <- paste0(report, "    Functions: ", paste(usage$functions, collapse = ", "), "\n")
      report <- paste0(report, "    Usage types: ", paste(usage$usage_types, collapse = ", "), "\n")
      report <- paste0(report, "    Files: ", length(usage$files), "\n")
    }
  } else {
    # All detected packages (fallback for older format)
    all_detected <- unique(c(
      dependency_data$detected$library_calls,
      dependency_data$detected$require_calls,
      dependency_data$detected$namespace_calls,
      dependency_data$detected$suspected_packages
    ))
    
    if (length(all_detected) > 0) {
      report <- paste0(report, "✅ ALL DETECTED PACKAGES:\n")
      for (pkg in sort(all_detected)) {
        # Determine package status
        status <- ""
        if (pkg %in% dependency_data$description$imports) {
          status <- "(Imports)"
        } else if (pkg %in% dependency_data$description$depends) {
          status <- "(Depends)"
        } else if (pkg %in% dependency_data$description$suggests) {
          status <- "(Suggests)"
        } else {
          status <- "(UNDECLARED)"
        }
        report <- paste0(report, "  • ", pkg, " ", status, "\n")
      }
    }
  }
  
  return(report)
}

# ===== NEW ENHANCED FUNCTIONS FOR PACKAGE FUNCTION EXTRACTION =====

#' Extract Functions Used from a Specific Package
#'
#' Analyzes your code to find all functions used from a specific package,
#' including namespace calls (pkg::func), suspected usage patterns, and more.
#'
#' @param package_name Character. Name of the package to analyze (e.g., "dplyr", "ggplot2")
#' @param package_path Character. Path to your package directory (default: ".")
#' @param dependency_data List. Pre-computed dependency analysis (optional)
#' @param detailed Logical. Return detailed breakdown by file and usage type (default: TRUE)
#'
#' @return List with functions used from the specified package
#'
#' @examples
#' \dontrun{
#' # Find all dplyr functions used
#' dplyr_usage <- extract_package_functions("dplyr")
#' 
#' # Find tidyverse functions (note: tidyverse loads multiple packages)
#' tidyverse_usage <- extract_package_functions("tidyverse")
#' 
#' # Get just the function names
#' ggplot_funcs <- extract_package_functions("ggplot2", detailed = FALSE)
#' 
#' # Use with pre-computed analysis
#' deps <- analyze_package_dependencies()
#' stringr_usage <- extract_package_functions("stringr", dependency_data = deps)
#' }
#'
#' @export
extract_package_functions <- function(package_name, package_path = ".", dependency_data = NULL, detailed = TRUE) {
  
  # If no dependency data provided, compute it
  if (is.null(dependency_data)) {
    message("Analyzing package dependencies...")
    dependency_data <- analyze_package_dependencies(package_path, verbose = FALSE)
  }
  
  # Check if we have function usage data
  if (!"function_usage" %in% names(dependency_data)) {
    message("Function usage data not available. Using basic detection...")
    # Fallback to basic detection
    return(extract_package_functions_basic(package_name, package_path, detailed))
  }
  
  function_usage <- dependency_data$function_usage
  
  # Filter for the specific package
  pkg_usage <- function_usage[function_usage$package == package_name, ]
  
  if (nrow(pkg_usage) == 0) {
    message("No usage found for package: ", package_name)
    if (detailed) {
      return(list(
        package = package_name,
        functions_found = character(0),
        usage_summary = list(),
        detailed_usage = data.frame()
      ))
    } else {
      return(character(0))
    }
  }
  
  # Extract actual function names used
  extracted_funcs <- extract_specific_functions_from_code(pkg_usage, package_path, package_name)
  
  if (!detailed) {
    return(sort(unique(extracted_funcs$all_functions)))
  }
  
  # Create detailed summary
  result <- list(
    package = package_name,
    functions_found = sort(unique(extracted_funcs$all_functions)),
    total_functions = length(unique(extracted_funcs$all_functions)),
    usage_by_type = extracted_funcs$by_type,
    usage_by_file = extracted_funcs$by_file,
    usage_by_your_function = extracted_funcs$by_your_function,
    detailed_usage = pkg_usage
  )
  
  return(result)
}

#' Generate Package Usage Report
#'
#' Creates a detailed report of how a specific package is used in your code.
#'
#' @param package_name Character. Package name
#' @param package_path Character. Your package path (default: ".")
#' @param dependency_data List. Pre-computed dependency analysis (optional)
#'
#' @return Character. Formatted report
#'
#' @examples
#' \dontrun{
#' # Generate report for dplyr usage
#' report <- generate_package_usage_report("dplyr")
#' cat(report)
#' 
#' # Generate report for ggplot2
#' ggplot_report <- generate_package_usage_report("ggplot2")
#' cat(ggplot_report)
#' }
#'
#' @export
generate_package_usage_report <- function(package_name, package_path = ".", dependency_data = NULL) {
  
  usage_data <- extract_package_functions(package_name, package_path, dependency_data, detailed = TRUE)
  
  if (length(usage_data$functions_found) == 0) {
    return(paste0("No usage found for package: ", package_name, "\n"))
  }
  
  report <- paste0("PACKAGE USAGE REPORT: ", package_name, "\n")
  report <- paste0(report, paste(rep("=", nchar(package_name) + 23), collapse = ""), "\n\n")
  
  # Summary
  report <- paste0(report, "SUMMARY:\n")
  report <- paste0(report, "  Total functions used: ", usage_data$total_functions, "\n")
  report <- paste0(report, "  Files involved: ", length(usage_data$usage_by_file), "\n")
  report <- paste0(report, "  Your functions using this package: ", length(usage_data$usage_by_your_function), "\n\n")
  
  # Functions found
  report <- paste0(report, "FUNCTIONS USED:\n")
  for (func in usage_data$functions_found) {
    report <- paste0(report, "  • ", func, "\n")
  }
  report <- paste0(report, "\n")
  
  # Usage by type
  if (length(usage_data$usage_by_type) > 0) {
    report <- paste0(report, "USAGE BY TYPE:\n")
    for (usage_type in names(usage_data$usage_by_type)) {
      funcs <- usage_data$usage_by_type[[usage_type]]
      if (length(funcs) > 0) {
        report <- paste0(report, "  ", usage_type, ": ", paste(funcs, collapse = ", "), "\n")
      }
    }
    report <- paste0(report, "\n")
  }
  
  # Usage by file
  if (length(usage_data$usage_by_file) > 0) {
    report <- paste0(report, "USAGE BY FILE:\n")
    for (file_name in names(usage_data$usage_by_file)) {
      funcs <- usage_data$usage_by_file[[file_name]]
      if (length(funcs) > 0) {
        report <- paste0(report, "  ", file_name, ": ", paste(funcs, collapse = ", "), "\n")
      }
    }
    report <- paste0(report, "\n")
  }
  
  # Usage by your functions
  if (length(usage_data$usage_by_your_function) > 0) {
    report <- paste0(report, "YOUR FUNCTIONS USING ", toupper(package_name), ":\n")
    for (your_func in names(usage_data$usage_by_your_function)) {
      func_info <- usage_data$usage_by_your_function[[your_func]]
      report <- paste0(report, "  ", your_func, " (in ", basename(func_info$files[1]), ")\n")
    }
  }
  
  return(report)
}

# ===== HELPER FUNCTIONS FOR ENHANCED FUNCTIONALITY =====

#' Create Usage Summary by Package
#'
#' @param function_usage Data.frame. Function usage data
#' @return List. Summary by package
create_usage_summary <- function(function_usage) {
  
  if (nrow(function_usage) == 0) {
    return(list())
  }
  
  # Group by package
  packages <- unique(function_usage$package)
  summary <- list()
  
  for (pkg in packages) {
    pkg_usage <- function_usage[function_usage$package == pkg, ]
    
    summary[[pkg]] <- list(
      functions = unique(pkg_usage$function_name),
      files = unique(pkg_usage$file_path),
      usage_types = unique(pkg_usage$usage_type),
      total_usages = nrow(pkg_usage)
    )
  }
  
  return(summary)
}

#' Find Function at Specific Position in File
#'
#' @param file_text Character. Full file content
#' @param position Integer. Character position
#' @param file_functions Data.frame. Functions in this file
#' @return Character. Function name or NA
find_function_at_position <- function(file_text, position, file_functions) {
  
  if (nrow(file_functions) == 0) return(NA_character_)
  
  # Convert position to line number
  lines_before <- substr(file_text, 1, position)
  line_number <- length(strsplit(lines_before, "\n")[[1]])
  
  # Find function boundaries (this is approximate)
  for (i in seq_len(nrow(file_functions))) {
    func_pattern <- paste0("\\b", stringr::str_escape(file_functions$function_name[i]), "\\s*<-\\s*function")
    func_match <- stringr::str_locate(file_text, func_pattern)
    
    if (!is.na(func_match[1, 1])) {
      func_lines_before <- substr(file_text, 1, func_match[1, 1])
      func_start_line <- length(strsplit(func_lines_before, "\n")[[1]])
      
      # Simple heuristic: if the usage is after the function definition and before the next function
      if (line_number >= func_start_line) {
        # Check if this is the closest function
        if (i == nrow(file_functions) || line_number < func_start_line + 100) {  # Assume functions are < 100 lines
          return(file_functions$function_name[i])
        }
      }
    }
  }
  
  return(NA_character_)
}

#' Find Function After Specific Position (for roxygen comments)
#'
#' @param file_text Character. Full file content
#' @param position Integer. Character position
#' @param file_functions Data.frame. Functions in this file
#' @return Character. Function name or NA
find_function_after_position <- function(file_text, position, file_functions) {
  
  if (nrow(file_functions) == 0) return(NA_character_)
  
  # Look for the next function definition after this position
  remaining_text <- substr(file_text, position, nchar(file_text))
  
  for (i in seq_len(nrow(file_functions))) {
    func_pattern <- paste0("\\b", stringr::str_escape(file_functions$function_name[i]), "\\s*<-\\s*function")
    func_match <- stringr::str_locate(remaining_text, func_pattern)
    
    if (!is.na(func_match[1, 1])) {
      return(file_functions$function_name[i])
    }
  }
  
  return(NA_character_)
}

#' Find Functions Using Package Patterns
#'
#' @param file_text Character. File content
#' @param package Character. Package name
#' @param file_functions Data.frame. Functions in file
#' @return Character vector. Function names
find_functions_using_package_patterns <- function(file_text, package, file_functions) {
  
  # Get patterns for this package
  patterns <- get_package_patterns(package)
  if (length(patterns) == 0) return(character(0))
  
  functions_using <- character(0)
  
  for (pattern in patterns) {
    pattern_matches <- stringr::str_locate_all(file_text, pattern)[[1]]
    
    if (nrow(pattern_matches) > 0) {
      for (j in seq_len(nrow(pattern_matches))) {
        match_pos <- pattern_matches[j, 1]
        func_name <- find_function_at_position(file_text, match_pos, file_functions)
        
        if (!is.na(func_name)) {
          functions_using <- c(functions_using, func_name)
        }
      }
    }
  }
  
  return(unique(functions_using))
}

#' Get Package-Specific Function Patterns
#'
#' @param package Character. Package name
#' @return Character vector. Regex patterns
get_package_patterns <- function(package) {
  
  patterns_map <- list(
    "ggplot2" = c("ggplot\\s*\\(", "geom_", "aes\\s*\\(", "theme_", "scale_", "labs\\s*\\("),
    "dplyr" = c("mutate\\s*\\(", "filter\\s*\\(", "select\\s*\\(", "arrange\\s*\\(", 
                "summarise\\s*\\(", "group_by\\s*\\(", "%>%"),
    "tidyr" = c("pivot_longer\\s*\\(", "pivot_wider\\s*\\(", "separate\\s*\\(", 
                "unite\\s*\\(", "nest\\s*\\("),
    "shiny" = c("fluidPage\\s*\\(", "renderPlot\\s*\\(", "observeEvent\\s*\\(", 
                "reactive\\s*\\(", "actionButton\\s*\\("),
    "DT" = c("datatable\\s*\\(", "renderDataTable\\s*\\("),
    "stringr" = c("str_detect\\s*\\(", "str_replace\\s*\\(", "str_extract\\s*\\(", 
                  "str_split\\s*\\(", "str_match\\s*\\("),
    "purrr" = c("map\\s*\\(", "map_dfr\\s*\\(", "map_chr\\s*\\(", "walk\\s*\\("),
    "readr" = c("read_csv\\s*\\(", "write_csv\\s*\\(", "read_delim\\s*\\("),
    "lubridate" = c("ymd\\s*\\(", "mdy\\s*\\(", "as_date\\s*\\(", "interval\\s*\\("),
    "testthat" = c("test_that\\s*\\(", "expect_", "describe\\s*\\(", "it\\s*\\(")
  )
  
  if (package %in% names(patterns_map)) {
    return(patterns_map[[package]])
  }
  
  return(character(0))
}

#' Check for Additional Hidden Dependencies
#'
#' @param package_path Character. Package root
#' @return List. Additional dependencies found
check_additional_dependencies <- function(package_path) {
  
  additional <- list()
  
  # Check for testthat in tests directory
  tests_dir <- file.path(package_path, "tests")
  if (dir.exists(tests_dir)) {
    test_files <- list.files(tests_dir, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
    
    if (length(test_files) > 0) {
      for (test_file in test_files) {
        tryCatch({
          test_content <- readLines(test_file, warn = FALSE)
          test_text <- paste(test_content, collapse = "\n")
          
          # Check for testthat usage
          if (stringr::str_detect(test_text, "test_that|expect_|describe\\s*\\(")) {
            additional$suspected_packages <- c(additional$suspected_packages, "testthat")
          }
          
          # Check for other test-related packages
          if (stringr::str_detect(test_text, "library\\s*\\(\\s*testthat\\s*\\)|require\\s*\\(\\s*testthat\\s*\\)")) {
            additional$library_calls <- c(additional$library_calls, "testthat")
          }
          
        }, error = function(e) {
          # Skip files that can't be read
        })
      }
    }
  }
  
  # Check for knitr/rmarkdown in vignettes
  vignettes_dir <- file.path(package_path, "vignettes")
  if (dir.exists(vignettes_dir)) {
    vignette_files <- list.files(vignettes_dir, pattern = "\\.(Rmd|rmd)$", full.names = TRUE)
    
    if (length(vignette_files) > 0) {
      additional$suspected_packages <- c(additional$suspected_packages, "knitr", "rmarkdown")
    }
  }
  
  # Remove duplicates
  additional <- lapply(additional, unique)
  
  return(additional)
}

#' Merge Additional Dependencies
#'
#' @param all_deps List. Main dependencies
#' @param additional_deps List. Additional dependencies
#' @return List. Merged dependencies
merge_additional_deps <- function(all_deps, additional_deps) {
  
  for (dep_type in names(additional_deps)) {
    if (dep_type %in% names(all_deps)) {
      all_deps[[dep_type]] <- unique(c(all_deps[[dep_type]], additional_deps[[dep_type]]))
    } else {
      all_deps[[dep_type]] <- additional_deps[[dep_type]]
    }
  }
  
  return(all_deps)
}

#' Extract Specific Functions from Code for a Package
#'
#' @param pkg_usage Data.frame. Usage data for specific package
#' @param package_path Character. Package path
#' @param package_name Character. Package name
#' @return List. Detailed function extraction
extract_specific_functions_from_code <- function(pkg_usage, package_path, package_name) {
  
  all_functions <- character(0)
  by_type <- list()
  by_file <- list()
  by_your_function <- list()
  
  # Group by usage type
  usage_types <- unique(pkg_usage$usage_type)
  
  for (usage_type in usage_types) {
    type_usage <- pkg_usage[pkg_usage$usage_type == usage_type, ]
    type_functions <- character(0)
    
    if (usage_type == "namespace") {
      # For namespace calls, extract the actual function names
      type_functions <- extract_namespace_functions(type_usage, package_name)
      
    } else if (usage_type == "suspected") {
      # For suspected usage, extract based on patterns
      type_functions <- extract_suspected_functions(type_usage, package_name)
      
    } else if (usage_type %in% c("library", "require")) {
      # For library/require, we know the package is loaded but need to find specific functions
      type_functions <- extract_functions_after_library(type_usage, package_name)
      
    } else if (usage_type == "roxygen") {
      # For roxygen imports, extract from @importFrom statements
      type_functions <- extract_roxygen_functions(type_usage, package_name)
    }
    
    by_type[[usage_type]] <- unique(type_functions)
    all_functions <- c(all_functions, type_functions)
  }
  
  # Group by file
  files <- unique(pkg_usage$file_path)
  for (file_path in files) {
    file_usage <- pkg_usage[pkg_usage$file_path == file_path, ]
    file_functions <- character(0)
    
    # Extract functions for this specific file
    for (i in seq_len(nrow(file_usage))) {
      usage_row <- file_usage[i, ]
      
      if (usage_row$usage_type == "namespace") {
        file_functions <- c(file_functions, extract_namespace_functions_from_file(file_path, package_name))
      } else if (usage_row$usage_type == "suspected") {
        file_functions <- c(file_functions, extract_suspected_functions_from_file(file_path, package_name))
      }
    }
    
    by_file[[basename(file_path)]] <- unique(file_functions)
  }
  
  # Group by your functions
  your_functions <- unique(pkg_usage$function_name)
  for (your_func in your_functions) {
    func_usage <- pkg_usage[pkg_usage$function_name == your_func, ]
    by_your_function[[your_func]] <- list(
      files = unique(func_usage$file_path),
      usage_types = unique(func_usage$usage_type),
      external_functions_used = character(0)  # Will be filled below
    )
  }
  
  return(list(
    all_functions = unique(all_functions),
    by_type = by_type,
    by_file = by_file,
    by_your_function = by_your_function
  ))
}

#' Extract Function Names from Namespace Calls
#'
#' @param type_usage Data.frame. Usage data for namespace type
#' @param package_name Character. Package name
#' @return Character vector. Function names
extract_namespace_functions <- function(type_usage, package_name) {
  
  functions_found <- character(0)
  
  for (i in seq_len(nrow(type_usage))) {
    file_path <- type_usage$file_path[i]
    functions_found <- c(functions_found, extract_namespace_functions_from_file(file_path, package_name))
  }
  
  return(unique(functions_found))
}

#' Extract Namespace Functions from Single File
#'
#' @param file_path Character. Path to file
#' @param package_name Character. Package name
#' @return Character vector. Function names
extract_namespace_functions_from_file <- function(file_path, package_name) {
  
  tryCatch({
    content <- readLines(file_path, warn = FALSE)
    file_text <- paste(content, collapse = "\n")
    
    # Pattern to match package::function_name
    namespace_pattern <- paste0("\\b", stringr::str_escape(package_name), "::([a-zA-Z_][a-zA-Z0-9_\\.]*)")
    matches <- stringr::str_match_all(file_text, namespace_pattern)[[1]]
    
    if (nrow(matches) > 0) {
      return(unique(matches[, 2]))
    }
    
  }, error = function(e) {
    warning("Could not read file: ", file_path)
  })
  
  return(character(0))
}

#' Extract Functions from Suspected Usage Patterns
#'
#' @param type_usage Data.frame. Usage data for suspected type
#' @param package_name Character. Package name
#' @return Character vector. Function names
extract_suspected_functions <- function(type_usage, package_name) {
  
  functions_found <- character(0)
  
  for (i in seq_len(nrow(type_usage))) {
    file_path <- type_usage$file_path[i]
    functions_found <- c(functions_found, extract_suspected_functions_from_file(file_path, package_name))
  }
  
  return(unique(functions_found))
}

#' Extract Suspected Functions from File
#'
#' @param file_path Character. File path
#' @param package_name Character. Package name
#' @return Character vector. Function names
extract_suspected_functions_from_file <- function(file_path, package_name) {
  
  tryCatch({
    content <- readLines(file_path, warn = FALSE)
    file_text <- paste(content, collapse = "\n")
    
    # Get specific patterns for this package
    patterns <- get_specific_function_patterns(package_name)
    functions_found <- character(0)
    
    for (pattern_info in patterns) {
      matches <- stringr::str_match_all(file_text, pattern_info$pattern)[[1]]
      if (nrow(matches) > 0) {
        if (!is.null(pattern_info$extract_group) && pattern_info$extract_group > 0) {
          # Extract specific group from regex
          functions_found <- c(functions_found, matches[, pattern_info$extract_group + 1])
        } else {
          # Use the function name directly
          functions_found <- c(functions_found, pattern_info$function_name)
        }
      }
    }
    
    return(unique(functions_found))
    
  }, error = function(e) {
    warning("Could not read file: ", file_path)
  })
  
  return(character(0))
}


#' Extract Functions After Library/Require Calls
#'
#' @param type_usage Data.frame. Usage data
#' @param package_name Character. Package name
#' @return Character vector. Function names
extract_functions_after_library <- function(type_usage, package_name) {
  
  # This is more complex - would need to parse code after library() calls
  # For now, return suspected functions as a fallback
  functions_found <- character(0)
  
  for (i in seq_len(nrow(type_usage))) {
    file_path <- type_usage$file_path[i]
    functions_found <- c(functions_found, extract_suspected_functions_from_file(file_path, package_name))
  }
  
  return(unique(functions_found))
}

#' Extract Functions from Roxygen @importFrom
#'
#' @param type_usage Data.frame. Usage data
#' @param package_name Character. Package name
#' @return Character vector. Function names
extract_roxygen_functions <- function(type_usage, package_name) {
  
  functions_found <- character(0)
  
  for (i in seq_len(nrow(type_usage))) {
    file_path <- type_usage$file_path[i]
    
    tryCatch({
      content <- readLines(file_path, warn = FALSE)
      file_text <- paste(content, collapse = "\n")
      
      # Pattern to match @importFrom package function1 function2
      import_pattern <- paste0("#'\\s*@importFrom\\s+", stringr::str_escape(package_name), "\\s+([^\n]+)")
      matches <- stringr::str_match_all(file_text, import_pattern)[[1]]
      
      if (nrow(matches) > 0) {
        for (j in seq_len(nrow(matches))) {
          func_list <- trimws(matches[j, 2])
          # Split by whitespace to get individual function names
          funcs <- stringr::str_split(func_list, "\\s+")[[1]]
          functions_found <- c(functions_found, funcs)
        }
      }
      
    }, error = function(e) {
      warning("Could not read file: ", file_path)
    })
  }
  
  return(unique(functions_found))
}

#' Basic Package Function Extraction (Fallback)
#'
#' @param package_name Character. Package name
#' @param package_path Character. Package path
#' @param detailed Logical. Return detailed info
#' @return List or character vector. Function extraction results
extract_package_functions_basic <- function(package_name, package_path, detailed) {
  
  functions_data <- scan_r_functions(package_path, verbose = FALSE)
  r_files <- unique(functions_data$file_path)
  
  all_functions <- character(0)
  
  for (file_path in r_files) {
    # Extract namespace functions
    namespace_funcs <- extract_namespace_functions_from_file(file_path, package_name)
    all_functions <- c(all_functions, namespace_funcs)
    
    # Extract suspected functions
    suspected_funcs <- extract_suspected_functions_from_file(file_path, package_name)
    all_functions <- c(all_functions, suspected_funcs)
  }
  
  all_functions <- unique(all_functions)
  
  if (!detailed) {
    return(sort(all_functions))
  }
  
  # Basic detailed response
  return(list(
    package = package_name,
    functions_found = sort(all_functions),
    total_functions = length(all_functions),
    usage_by_type = list(basic_detection = all_functions),
    usage_by_file = list(),
    usage_by_your_function = list(),
    detailed_usage = data.frame()
  ))
}