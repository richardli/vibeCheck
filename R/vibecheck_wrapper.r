#' VibeCheck: Complete Package Analysis and Fixes (CORRECTED)
#'
#' Main wrapper function that provides a comprehensive analysis of an R package
#' and offers to fix common R CMD check issues. Updated with smart path detection.
#'
#' @param path Character. Path to package directory (default: auto-detect from current directory)
#' @param fix_issues Logical. Whether to interactively fix found issues (default: TRUE)
#' @param include_dependencies Logical. Whether to analyze dependencies (default: TRUE)
#' @param verbose Logical. Print detailed progress messages (default: TRUE)
#'
#' @return List with complete package analysis results
#'
#' @examples
#' \dontrun{
#' # Auto-detect package from current directory
#' result <- vibecheck()
#' 
#' 
#' # Full analysis with dependencies
#' result <- vibecheck(include_dependencies = TRUE)
#' 
#' # Analyze specific package
#' result <- vibecheck("/path/to/package")
#' }
#'
#' @export
vibecheck <- function(path = NULL, fix_issues = FALSE, include_dependencies = FALSE, verbose = TRUE) {
  
  # Smart path detection
  if (is.null(path)) {
    path <- smart_detect_package_path()
  }
  
  if (verbose) {
    cat("🔍 VibeCheck: R Package Analysis & Fixes\n")
    cat("=========================================\n\n")
  }
  
  # Step 1: Package analysis (using regular analyze_package - it's fast enough)
  if (verbose) cat("📊 Analyzing package structure...\n")
  pkg_info <- analyze_package(path, include_dependencies, verbose = FALSE)
  
  # Step 2: Print summary
  print_package_summary(pkg_info, verbose)
  
  # Step 3: Check for common issues and offer fixes
  if (fix_issues) {
    offer_interactive_fixes(pkg_info, verbose)
  }
  
  return(invisible(pkg_info))
}

#' Smart Package Path Detection
#'
#' Automatically detects package root from current working directory.
#' If CWD is package root, uses it. If CWD is R/ folder, uses parent.
#'
#' @return Character. Detected package path
#' @export
smart_detect_package_path <- function() {
  
  cwd <- getwd()
  
  # Check if current directory is package root
  if (is_package_root(cwd)) {
    return(cwd)
  }
  
  # Check if current directory is R/ folder inside a package
  if (basename(cwd) == "R") {
    parent_dir <- dirname(cwd)
    if (is_package_root(parent_dir)) {
      return(parent_dir)
    }
  }
  
  # Check parent directory in case we're in a subdirectory
  parent_dir <- dirname(cwd)
  if (is_package_root(parent_dir)) {
    return(parent_dir)
  }
  
  # If no package detected, return current directory with warning
  warning("Could not detect R package structure. Using current directory: ", cwd)
  return(cwd)
}

#' Quick Package Check (CORRECTED)
#'
#' Fast version of package checking that only reports critical issues.
#' Uses regular dependency analysis for accuracy.
#'
#' @param path Character. Package path (default: auto-detect)
#' @return List with issues summary
#' @export
quick_package_check <- function(path = NULL) {
  
  # Smart path detection
  if (is.null(path)) {
    path <- smart_detect_package_path()
  }
  
  cat("⚡ Quick Package Check\n")
  cat("=====================\n")
  cat("Package path:", path, "\n\n")
  
  # Basic analysis with proper dependency checking
  pkg_info <- analyze_package(path, include_dependencies = TRUE, verbose = FALSE)
  
  issues <- list()
  
  # Check for missing packages (using proper dependency analysis)
  if (!is.null(pkg_info$dependencies)) {
    if (length(pkg_info$dependencies$missing) > 0) {
      issues$missing_packages <- pkg_info$dependencies$missing
    }
    
    # Check for undeclared dependencies
    if (length(pkg_info$dependencies$undeclared) > 0) {
      issues$undeclared_packages <- pkg_info$dependencies$undeclared
    }
  }
  
  # Check for undocumented functions
  undocumented <- pkg_info$stats$total_functions - pkg_info$stats$documented_functions
  if (undocumented > 0) {
    issues$undocumented_functions <- undocumented
  }
  
  # Print summary
  if (length(issues) == 0) {
    cat("✅ No critical issues found! Package looks good.\n\n")
  } else {
    cat("⚠️  Issues found:\n")
    
    if ("missing_packages" %in% names(issues)) {
      cat("  • Missing packages:", length(issues$missing_packages), "\n")
      cat("    ", paste(issues$missing_packages, collapse = ", "), "\n")
    }
    
    if ("undocumented_functions" %in% names(issues)) {
      cat("  • Undocumented functions:", issues$undocumented_functions, "\n")
    }
    
    if ("undeclared_packages" %in% names(issues)) {
      cat("  • Undeclared packages:", length(issues$undeclared_packages), "\n")
      cat("    ", paste(issues$undeclared_packages, collapse = ", "), "\n")
    }
    
    cat("\nRun vibecheck() for detailed analysis and fixes.\n\n")
  }
  
  return(invisible(issues))
}

#' Print Package Analysis Summary
#'
#' @param pkg_info List. Result from analyze_package()
#' @param verbose Logical. Print detailed information
print_package_summary <- function(pkg_info, verbose = TRUE) {
  
  if (!verbose) return(invisible())
  
  stats <- pkg_info$stats
  
  cat("📋 PACKAGE SUMMARY\n")
  cat("==================\n")
  cat("Package path:", pkg_info$package_path, "\n")
  cat("Total functions:", stats$total_functions, "\n")
  cat("Documented functions:", stats$documented_functions, "\n")
  cat("Undocumented functions:", stats$total_functions - stats$documented_functions, "\n")
  cat("R files:", stats$total_files, "\n")
  
  if ("dependencies" %in% names(pkg_info) && !is.null(pkg_info$dependencies)) {
    deps <- pkg_info$dependencies
    missing_count <- if ("missing" %in% names(deps)) length(deps$missing) else 0
    undeclared_count <- if ("undeclared" %in% names(deps)) length(deps$undeclared) else 0
    
    cat("Missing packages:", missing_count, "\n")
    cat("Undeclared packages:", undeclared_count, "\n")
    
    if (missing_count > 0) {
      cat("\n❌ MISSING PACKAGES:\n")
      for (pkg in deps$missing) {
        cat("  •", pkg, "\n")
      }
    }
    
    if (undeclared_count > 0) {
      cat("\n⚠️  UNDECLARED PACKAGES:\n")
      for (pkg in deps$undeclared) {
        cat("  •", pkg, "\n")
      }
    }
  }
  
  cat("\n")
}

#' Offer Interactive Fixes for Common Issues
#'
#' @param pkg_info List. Package analysis results
#' @param verbose Logical. Print messages
offer_interactive_fixes <- function(pkg_info, verbose = TRUE) {
  
  if (!verbose) return(invisible())
  
  cat("🔧 AVAILABLE FIXES\n")
  cat("==================\n")
  
  fixes_available <- FALSE
  
  # 1. Missing packages
  if ("dependencies" %in% names(pkg_info) && !is.null(pkg_info$dependencies) && length(pkg_info$dependencies$missing) > 0) {
    fixes_available <- TRUE
    cat("1. Install missing packages (", length(pkg_info$dependencies$missing), " packages)\n")
  }
  
  # 2. Undocumented functions
  undocumented_count <- pkg_info$stats$total_functions - pkg_info$stats$documented_functions
  if (undocumented_count > 0) {
    fixes_available <- TRUE
    cat("2. Generate documentation templates (", undocumented_count, " functions)\n")
  }
  
  # 3. Namespace opportunities
  namespace_analysis <- analyze_namespace_usage(pkg_info$package_path, verbose = FALSE)
  if (namespace_analysis$stats$total_opportunities > 0) {
    fixes_available <- TRUE
    cat("3. Convert to namespace calls (", namespace_analysis$stats$total_opportunities, " opportunities)\n")
  }
  
  # 4. R CMD check fixes
  fixes_available <- TRUE
  cat("4. Fix R CMD check issues (global variables, non-ASCII characters, empty examples)\n")
  
  if (!fixes_available) {
    cat("✅ No common issues found! Your package looks good.\n\n")
    return(invisible())
  }
  
  cat("\nWould you like to apply any fixes? (y/n): ")
  response <- readline()
  
  if (tolower(substr(response, 1, 1)) == "y") {
    interactive_fix_menu(pkg_info, namespace_analysis)
  } else {
    cat("No fixes applied. You can run individual fix functions later.\n\n")
  }
}

#' Interactive Fix Menu
#'
#' @param pkg_info List. Package analysis results
#' @param namespace_analysis List. Namespace analysis results
interactive_fix_menu <- function(pkg_info, namespace_analysis = NULL) {
  
  repeat {
    cat("\n🛠️  INTERACTIVE FIX MENU\n")
    cat("=======================\n")
    cat("1. Install missing packages\n")
    cat("2. Generate documentation templates\n") 
    cat("3. Convert to namespace calls\n")
    cat("4. Fix global variables (from R CMD check output)\n")
    cat("5. Fix non-ASCII characters (from R CMD check output)\n")
    cat("6. Fix empty examples (from R CMD check output)\n")
    cat("7. Generate dependency report\n")
    cat("8. Generate namespace report\n")
    cat("0. Exit\n")
    cat("\nSelect option (0-8): ")
    
    choice <- readline()
    
    switch(choice,
      "1" = fix_missing_packages_interactive(pkg_info),
      "2" = fix_documentation_interactive(pkg_info),
      "3" = {
        if (is.null(namespace_analysis)) {
          cat("Analyzing namespace opportunities...\n")
          namespace_analysis <- analyze_namespace_usage(pkg_info$package_path, verbose = FALSE)
        }
        fix_namespace_interactive(namespace_analysis)
      },
      "4" = fix_global_variables_interactive(),
      "5" = fix_non_ascii_characters_interactive(),
      "6" = fix_empty_examples_interactive(),
      "7" = show_dependency_report(pkg_info),
      "8" = {
        if (is.null(namespace_analysis)) {
          cat("Analyzing namespace opportunities...\n")
          namespace_analysis <- analyze_namespace_usage(pkg_info$package_path, verbose = FALSE)
        }
        show_namespace_report(namespace_analysis)
      },
      "0" = {
        cat("👋 Exiting fix menu. Happy coding!\n\n")
        break
      },
      cat("Invalid option. Please select 0-8.\n")
    )
  }
}

#' Interactive Missing Packages Fix
#'
#' @param pkg_info List. Package analysis results
fix_missing_packages_interactive <- function(pkg_info) {
  
  if (!"dependencies" %in% names(pkg_info)) {
    cat("❌ No dependency information available.\n")
    return(invisible())
  }
  
  missing <- pkg_info$dependencies$missing
  
  if (length(missing) == 0) {
    cat("✅ No missing packages found!\n")
    return(invisible())
  }
  
  cat("\n📦 MISSING PACKAGES\n")
  cat("===================\n")
  for (pkg in missing) {
    cat("  •", pkg, "\n")
  }
  
  cat("\nInstall all missing packages? (y/n): ")
  response <- readline()
  
  if (tolower(substr(response, 1, 1)) == "y") {
    result <- install_missing_packages(missing, verbose = TRUE)
    
    if (length(result$failed) > 0) {
      cat("\n❌ Failed to install:\n")
      for (pkg in result$failed) {
        cat("  •", pkg, "\n")
      }
    }
    
    if (length(result$success) > 0) {
      cat("\n✅ Successfully installed:\n")
      for (pkg in result$success) {
        cat("  •", pkg, "\n")
      }
    }
  }
}

#' Interactive Documentation Fix
#'
#' @param pkg_info List. Package analysis results  
fix_documentation_interactive <- function(pkg_info) {
  
  undocumented_count <- pkg_info$stats$total_functions - pkg_info$stats$documented_functions
  
  if (undocumented_count == 0) {
    cat("✅ All functions are already documented!\n")
    return(invisible())
  }
  
  cat("\n📝 DOCUMENTATION GENERATION\n")
  cat("===========================\n")
  cat("Found", undocumented_count, "undocumented functions.\n")
  cat("\nGenerate documentation templates? (y/n): ")
  
  response <- readline()
  
  if (tolower(substr(response, 1, 1)) == "y") {
    cat("Save templates directly to files? (y/n): ")
    save_response <- readline()
    save_to_files <- tolower(substr(save_response, 1, 1)) == "y"
    
    result <- generate_bulk_documentation(
      package_data = pkg_info,
      save_to_files = save_to_files,
      backup = TRUE
    )
    
    if (save_to_files) {
      cat("✅ Documentation templates generated and saved!\n")
    } else {
      cat("✅ Documentation templates generated (not saved to files).\n")
      cat("Use save_function_docs() to save individual templates.\n")
    }
  }
}

#' Interactive Namespace Fix
#'
#' @param namespace_analysis List. Namespace analysis results
fix_namespace_interactive <- function(namespace_analysis) {
  
  if (namespace_analysis$stats$total_opportunities == 0) {
    cat("✅ No namespace conversion opportunities found!\n")
    return(invisible())
  }
  
  cat("\n🔗 NAMESPACE CONVERSION\n")
  cat("=======================\n")
  cat("Found", namespace_analysis$stats$total_opportunities, "namespace opportunities.\n")
  cat("Files affected:", namespace_analysis$stats$files_affected, "\n")
  cat("Packages:", paste(namespace_analysis$stats$packages_involved, collapse = ", "), "\n")
  
  cat("\nView detailed report first? (y/n): ")
  response <- readline()
  
  if (tolower(substr(response, 1, 1)) == "y") {
    report <- generate_namespace_report(namespace_analysis)
    cat("\n", report, "\n")
  }
  
  cat("Apply namespace conversions? (y/n): ")
  response <- readline()
  
  if (tolower(substr(response, 1, 1)) == "y") {
    cat("Preview changes first? (y/n): ")
    preview_response <- readline()
    
    if (tolower(substr(preview_response, 1, 1)) == "y") {
      # Preview first
      apply_namespace_conversion(preview_only = TRUE, verbose = TRUE)
      
      cat("\nApply these changes? (y/n): ")
      final_response <- readline()
      
      if (tolower(substr(final_response, 1, 1)) == "y") {
        apply_namespace_conversion(backup = TRUE, verbose = TRUE)
        cat("✅ Namespace conversions applied!\n")
      }
    } else {
      # Apply directly
      apply_namespace_conversion(backup = TRUE, verbose = TRUE)
      cat("✅ Namespace conversions applied!\n")
    }
  }
}

#' Show Dependency Report
#'
#' @param pkg_info List. Package analysis results
show_dependency_report <- function(pkg_info) {
  
  if (!"dependencies" %in% names(pkg_info)) {
    cat("❌ No dependency information available.\n")
    return(invisible())
  }
  
  report <- generate_dependency_report(pkg_info$dependencies)
  cat("\n", report, "\n")
}

#' Show Namespace Report
#'
#' @param namespace_analysis List. Namespace analysis results
show_namespace_report <- function(namespace_analysis) {
  
  report <- generate_namespace_report(namespace_analysis)
  cat("\n", report, "\n")
}

# Add these helper functions for R CMD check fixes
fix_global_variables_interactive <- function() {
  cat("\nPaste R CMD check output containing global variable errors:\n")
  cat("(Press Enter twice when done)\n")
  
  lines <- c()
  repeat {
    line <- readline()
    if (nchar(line) == 0) break
    lines <- c(lines, line)
  }
  
  if (length(lines) == 0) {
    cat("No input provided.\n")
    return(invisible())
  }
  
  check_output <- paste(lines, collapse = "\n")
  
  tryCatch({
    result <- extract_and_update_globals(check_output)
    cat("✅ Global variables fixed!\n")
    cat("Variables added:", paste(result$new_variables, collapse = ", "), "\n")
  }, error = function(e) {
    cat("❌ Error:", e$message, "\n")
  })
}

fix_non_ascii_characters_interactive <- function() {
  cat("\n❌ Non-ASCII character fixing not implemented in this version.\n")
  cat("Please use R CMD check output and fix manually or implement the function.\n")
}

fix_empty_examples_interactive <- function() {
  cat("\n📝 Empty examples fixing available through add_custom_examples() function.\n")
  cat("Use the documentation generation features instead.\n")
}