#' Analyze Package Dependencies
#'
#' Comprehensive dependency analysis including DESCRIPTION file parsing,
#' code scanning for package usage, and missing package detection.
#'
#' @param package_path Character. Path to package root
#' @param functions_data Data.frame. Function data from scan_r_functions
#' @param verbose Logical. Print progress messages
#'
#' @return List with dependency analysis results
#'
#' @examples
#' \dontrun{
#' # Analyze dependencies for current package
#' deps <- analyze_package_dependencies(".")
#' 
#' # With existing function data
#' functions <- scan_r_functions(".")
#' deps <- analyze_package_dependencies(".", functions)
#' }
#'
#' @export
analyze_package_dependencies <- function(package_path, functions_data = NULL, verbose = TRUE) {
  
  # If no functions data provided, scan the package
  if (is.null(functions_data)) {
    functions_data <- scan_r_functions(package_path, verbose = FALSE)
  }
  
  # Analyze DESCRIPTION file
  description_deps <- parse_description_dependencies(package_path)
  
  # Scan code for package usage
  code_deps <- scan_code_dependencies(functions_data, package_path)
  
  # Find missing packages
  all_detected <- unique(c(
    code_deps$library_calls,
    code_deps$require_calls,
    code_deps$namespace_calls,
    code_deps$suspected_packages
  ))
  
  missing_packages <- check_missing_packages(all_detected)
  
  # Compare declared vs detected
  all_declared <- unique(c(
    description_deps$imports,
    description_deps$depends,
    description_deps$suggests
  ))
  
  undeclared <- setdiff(all_detected, all_declared)
  unused <- setdiff(all_declared, all_detected)
  
  result <- list(
    description = description_deps,
    detected = code_deps,
    missing = missing_packages,
    undeclared = undeclared,
    unused = unused,
    summary = list(
      total_detected = length(all_detected),
      total_declared = length(all_declared),
      missing_count = length(missing_packages),
      undeclared_count = length(undeclared),
      unused_count = length(unused)
    )
  )
  
  if (verbose) {
    message("Dependency analysis:")
    message("  Detected packages: ", length(all_detected))
    message("  Declared packages: ", length(all_declared))
    message("  Missing packages: ", length(missing_packages))
    message("  Undeclared packages: ", length(undeclared))
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

#' Scan Code for Package Dependencies
#'
#' @param functions_data Data.frame. Function information
#' @param package_path Character. Package root path
#' @return List with detected dependencies
#' @export
scan_code_dependencies <- function(functions_data, package_path) {
  
  if (nrow(functions_data) == 0) {
    return(list(
      library_calls = character(0),
      require_calls = character(0),
      namespace_calls = character(0),
      roxygen_imports = character(0),
      suspected_packages = character(0)
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
  
  # Scan each file
  for (file_path in r_files) {
    file_deps <- scan_file_dependencies(file_path)
    
    # Combine results
    for (dep_type in names(all_deps)) {
      if (dep_type %in% names(file_deps)) {
        all_deps[[dep_type]] <- c(all_deps[[dep_type]], file_deps[[dep_type]])
      }
    }
  }
  
  # Remove duplicates
  all_deps <- lapply(all_deps, unique)
  
  return(all_deps)
}

#' Scan Single File for Dependencies
#'
#' @param file_path Character. Path to R file
#' @return List with detected dependencies
scan_file_dependencies <- function(file_path) {
  
  content <- readLines(file_path, warn = FALSE)
  file_text <- paste(content, collapse = "\n")
  
  deps <- list()
  
  # Library calls: library(package)
  library_pattern <- "library\\s*\\(\\s*(['\"]?)([^'\"\\)]+)\\1\\s*\\)"
  library_matches <- stringr::str_match_all(file_text, library_pattern)[[1]]
  if (nrow(library_matches) > 0) {
    deps$library_calls <- library_matches[, 3]
  }
  
  # Require calls: require(package)
  require_pattern <- "require\\s*\\(\\s*(['\"]?)([^'\"\\)]+)\\1\\s*\\)"
  require_matches <- stringr::str_match_all(file_text, require_pattern)[[1]]
  if (nrow(require_matches) > 0) {
    deps$require_calls <- require_matches[, 3]
  }
  
  # Namespace calls: package::function
  namespace_pattern <- "([a-zA-Z][a-zA-Z0-9\\.]*)::"
  namespace_matches <- stringr::str_match_all(file_text, namespace_pattern)[[1]]
  if (nrow(namespace_matches) > 0) {
    deps$namespace_calls <- unique(namespace_matches[, 2])
  }
  
  # Roxygen imports: @import package, @importFrom package
  import_pattern <- "#'\\s*@import(?:From)?\\s+([a-zA-Z][a-zA-Z0-9\\.]*)"
  import_matches <- stringr::str_match_all(file_text, import_pattern)[[1]]
  if (nrow(import_matches) > 0) {
    deps$roxygen_imports <- import_matches[, 2]
  }
  
  # Suspected package usage based on function patterns
  deps$suspected_packages <- detect_suspected_packages(file_text)
  
  return(deps)
}

#' Detect Suspected Package Usage from Code Patterns
#'
#' @param code Character. Code content
#' @return Character vector of suspected packages
#' @export
detect_suspected_packages <- function(code) {
  
  # Common function patterns that suggest package usage
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
    "purrr" = c("map\\s*\\(", "map_dfr\\s*\\(", "map_chr\\s*\\(", "walk\\s*\\("),
    "readr" = c("read_csv\\s*\\(", "write_csv\\s*\\(", "read_delim\\s*\\("),
    "lubridate" = c("ymd\\s*\\(", "mdy\\s*\\(", "as_date\\s*\\(", "interval\\s*\\("),
    "testthat" = c("test_that\\s*\\(", "expect_", "describe\\s*\\(", "it\\s*\\(")
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

#' Generate Dependency Report
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
  report <- paste0(report, "  Unused declarations: ", summary$unused_count, "\n\n")
  
  # Missing packages
  if (length(dependency_data$missing) > 0) {
    report <- paste0(report, "❌ MISSING PACKAGES:\n")
    for (pkg in dependency_data$missing) {
      report <- paste0(report, "  • ", pkg, "\n")
    }
    report <- paste0(report, "\n")
  }
  
  # Undeclared packages
  if (length(dependency_data$undeclared) > 0) {
    report <- paste0(report, "⚠️  UNDECLARED PACKAGES (used but not in DESCRIPTION):\n")
    for (pkg in dependency_data$undeclared) {
      report <- paste0(report, "  • ", pkg, "\n")
    }
    report <- paste0(report, "\n")
  }
  
  # Unused packages
  if (length(dependency_data$unused) > 0) {
    report <- paste0(report, "📋 POSSIBLY UNUSED PACKAGES (declared but not detected):\n")
    for (pkg in dependency_data$unused) {
      report <- paste0(report, "  • ", pkg, "\n")
    }
    report <- paste0(report, "\n")
  }
  
  # All detected packages
  all_detected <- unique(c(
    dependency_data$detected$library_calls,
    dependency_data$detected$require_calls,
    dependency_data$detected$namespace_calls,
    dependency_data$detected$suspected_packages
  ))
  
  if (length(all_detected) > 0) {
    report <- paste0(report, "✅ ALL DETECTED PACKAGES:\n")
    for (pkg in sort(all_detected)) {
      report <- paste0(report, "  • ", pkg, "\n")
    }
  }
  
  return(report)
}