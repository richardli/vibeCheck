#' R Function Documentation Helper Class with Dependency Analysis
#'
#' An R6 class that provides comprehensive analysis of R package functions,
#' their documentation, parameters, and dependencies. Supports automatic
#' documentation template generation and dependency scanning.
#'
#' @field package_path Character. Path to the R package directory
#' @field functions_data Data.frame. Information about all functions found
#' @field param_history List. Historical parameter documentation patterns
#' @field dependencies List. Package dependency information
#' @field missing_deps Character vector. Missing/uninstalled packages
#'
#' @examples
#' \dontrun{
#' # Create a documentation helper for current directory
#' helper <- DocumentationHelper$new(".")
#' 
#' # Scan a specific package
#' helper <- DocumentationHelper$new("/path/to/package")
#' 
#' # Generate documentation template
#' template <- helper$generate_template("my_function", list(x = NULL, y = "default"))
#' }
#'
#' @export
DocumentationHelper <- R6::R6Class("DocumentationHelper",
  public = list(
    package_path = NULL,
    functions_data = NULL,
    param_history = list(),
    dependencies = list(),
    missing_deps = list(),
    
    # Initialize DocumentationHelper
    initialize = function(package_path = ".") {
      # Smart path detection - handle common directory issues
      self$package_path <- self$detect_package_root(package_path)
      self$scan_package()
      self$build_param_history()
      self$scan_dependencies()
    },
    
    # Detect the actual package root directory
    detect_package_root = function(initial_path) {
      # Normalize the path
      path <- normalizePath(initial_path, mustWork = FALSE)
      
      # Check if we're in a subdirectory of a package
      current_path <- path
      max_levels <- 3  # Don't go up more than 3 levels
      
      for (i in 1:max_levels) {
        # Check if current path looks like a package root
        if (self$is_package_root(current_path)) {
          if (current_path != path) {
            message("Detected package root at: ", current_path, " (moved up from ", path, ")")
          }
          return(current_path)
        }
        
        # Move up one directory
        parent_path <- dirname(current_path)
        if (parent_path == current_path) {
          break  # We've reached the filesystem root
        }
        current_path <- parent_path
      }
      
      # If we couldn't find a package root, warn but use original path
      if (!self$is_package_root(path)) {
        warning("Directory '", path, "' doesn't appear to be an R package root. ",
                "Looking for DESCRIPTION file and R/ directory.")
      }
      
      return(path)
    },
    
    # Check if a directory looks like a package root
    is_package_root = function(path) {
      # A package root should have:
      # 1. A DESCRIPTION file
      # 2. An R/ directory (or at least some R files)
      
      has_description <- file.exists(file.path(path, "DESCRIPTION"))
      has_r_dir <- dir.exists(file.path(path, "R"))
      
      # Also check for R files in the directory itself (for non-standard packages)
      has_r_files <- length(list.files(path, pattern = "\\.R$")) > 0
      
      # Alternative check: look for common package files
      has_namespace <- file.exists(file.path(path, "NAMESPACE"))
      has_man_dir <- dir.exists(file.path(path, "man"))
      
      # Package root criteria (flexible)
      is_standard_package <- has_description && has_r_dir
      is_alternative_package <- has_description && (has_r_files || has_namespace || has_man_dir)
      
      return(is_standard_package || is_alternative_package)
    },
    
    # Scan package for R functions
    scan_package = function() {
      # Enhanced directory detection - try multiple strategies
      possible_dirs <- c()
      
      # Strategy 1: Standard package structure
      r_dir <- file.path(self$package_path, "R")
      if (dir.exists(r_dir)) {
        possible_dirs <- c(possible_dirs, r_dir)
      }
      
      # Strategy 2: If we're already in R/ directory, use current directory
      if (basename(self$package_path) == "R" && length(list.files(self$package_path, pattern = "\\.R$")) > 0) {
        possible_dirs <- c(possible_dirs, self$package_path)
        # Also check parent directory in case it's the package root
        parent_dir <- dirname(self$package_path)
        if (self$is_package_root(parent_dir)) {
          message("Detected that you're in the R/ subdirectory. Using parent as package root.")
          self$package_path <- parent_dir
        }
      }
      
      # Strategy 3: Current directory has R files
      if (length(list.files(self$package_path, pattern = "\\.R$")) > 0) {
        possible_dirs <- c(possible_dirs, self$package_path)
      }
      
      # Strategy 4: Other common locations
      other_dirs <- c(
        file.path(self$package_path, "src"),
        file.path(self$package_path, "scripts")
      )
      
      for (dir in other_dirs) {
        if (dir.exists(dir)) {
          possible_dirs <- c(possible_dirs, dir)
        }
      }
      
      # Find R files
      r_files <- c()
      for (dir in possible_dirs) {
        if (dir.exists(dir)) {
          files <- list.files(dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
          r_files <- c(r_files, files)
        }
      }
      
      # Remove duplicates
      r_files <- unique(r_files)
      
      if (length(r_files) == 0) {
        stop("No R files found in any of these locations:\n", 
             paste(possible_dirs, collapse = "\n"),
             "\n\nCurrent working directory: ", getwd(),
             "\nPackage path: ", self$package_path,
             "\nTip: Make sure you're in the package root directory (where DESCRIPTION is located)")
      }
      
      message("Found ", length(r_files), " R files")
      if (length(possible_dirs) > 1) {
        message("Searched in: ", paste(basename(possible_dirs), collapse = ", "))
      }
      
      self$functions_data <- purrr::map_dfr(r_files, ~self$parse_r_file(.x))
      
      invisible(self)
    },
    
    # Parse R file for functions and existing documentation
    parse_r_file = function(file_path) {
      lines <- readLines(file_path, warn = FALSE)
      content <- paste(lines, collapse = "\n")
      
      func_pattern <- "((?:#'[^\n]*\n)*)(\\s*)([a-zA-Z_][a-zA-Z0-9_\\.]*)\\s*<-\\s*function\\s*\\(([^\\)]*)\\)\\s*\\{"
      matches <- stringr::str_locate_all(content, func_pattern)[[1]]
      
      if (nrow(matches) == 0) return(data.frame())
      
      functions_info <- data.frame(
        file_path = file_path,
        file_name = basename(file_path),
        stringsAsFactors = FALSE
      )
      
      for (i in seq_len(nrow(matches))) {
        start_pos <- matches[i, "start"]
        end_pos <- self$find_function_end(content, matches[i, "end"])
        
        func_text <- substr(content, start_pos, end_pos)
        func_match <- stringr::str_match(func_text, func_pattern)
        existing_docs <- trimws(func_match[1, 2])
        function_name <- func_match[1, 4]
        args_string <- func_match[1, 5]
        
        if (i == 1) {
          functions_info$existing_docs <- existing_docs
          functions_info$function_name <- function_name
          functions_info$args_string <- args_string
          functions_info$full_function <- func_text
          functions_info$start_line <- stringr::str_count(substr(content, 1, start_pos), "\n") + 1
          functions_info$end_line <- stringr::str_count(substr(content, 1, end_pos), "\n") + 1
          functions_info$has_docs <- nchar(existing_docs) > 0
        } else {
          functions_info <- rbind(functions_info, data.frame(
            file_path = file_path,
            file_name = basename(file_path),
            existing_docs = existing_docs,
            function_name = function_name,
            args_string = args_string,
            full_function = func_text,
            start_line = stringr::str_count(substr(content, 1, start_pos), "\n") + 1,
            end_line = stringr::str_count(substr(content, 1, end_pos), "\n") + 1,
            has_docs = nchar(existing_docs) > 0,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      functions_info$parsed_args <- purrr::map(functions_info$args_string, ~self$parse_arguments(.x))
      functions_info$dependencies <- purrr::map(functions_info$full_function, ~self$extract_dependencies(.x))
      functions_info$missing_deps <- purrr::map(functions_info$dependencies, ~self$check_missing_packages(.x))
      
      return(functions_info)
    },
    
    # Find the end of a function
    find_function_end = function(content, start_pos) {
      chars <- unlist(strsplit(substr(content, start_pos, nchar(content)), ""))
      brace_count <- 0
      
      for (i in seq_along(chars)) {
        if (chars[i] == "{") brace_count <- brace_count + 1
        if (chars[i] == "}") brace_count <- brace_count - 1
        
        if (brace_count == 0 && chars[i] == "}") {
          return(start_pos + i - 1)
        }
      }
      
      return(nchar(content))
    },
    
    # Parse function arguments
    parse_arguments = function(args_string) {
      if (nchar(trimws(args_string)) == 0) return(list())
      
      args <- stringr::str_split(args_string, ",")[[1]]
      args <- trimws(args)
      
      arg_list <- list()
      for (arg in args) {
        if (stringr::str_detect(arg, "=")) {
          parts <- stringr::str_split(arg, "=", n = 2)[[1]]
          name <- trimws(parts[1])
          default <- trimws(parts[2])
          arg_list[[name]] <- default
        } else {
          name <- trimws(arg)
          if (name != "") arg_list[[name]] <- NULL
        }
      }
      
      return(arg_list)
    },
    
    # Extract package dependencies from function code
    extract_dependencies = function(code) {
      deps <- list()
      
      library_pattern <- "library\\s*\\(\\s*(['\"]?)([^'\"\\)]+)\\1\\s*\\)"
      library_matches <- stringr::str_match_all(code, library_pattern)[[1]]
      if (nrow(library_matches) > 0) {
        deps$library_calls <- library_matches[, 3]
      }
      
      require_pattern <- "require\\s*\\(\\s*(['\"]?)([^'\"\\)]+)\\1\\s*\\)"
      require_matches <- stringr::str_match_all(code, require_pattern)[[1]]
      if (nrow(require_matches) > 0) {
        deps$require_calls <- require_matches[, 3]
      }
      
      namespace_pattern <- "([a-zA-Z][a-zA-Z0-9\\.]*)::"
      namespace_matches <- stringr::str_match_all(code, namespace_pattern)[[1]]
      if (nrow(namespace_matches) > 0) {
        deps$namespace_calls <- unique(namespace_matches[, 2])
      }
      
      import_pattern <- "#'\\s*@import\\s+([a-zA-Z][a-zA-Z0-9\\.]*)"
      import_matches <- stringr::str_match_all(code, import_pattern)[[1]]
      if (nrow(import_matches) > 0) {
        deps$roxygen_imports <- import_matches[, 2]
      }
      
      importfrom_pattern <- "#'\\s*@importFrom\\s+([a-zA-Z][a-zA-Z0-9\\.]*)\\s+"
      importfrom_matches <- stringr::str_match_all(code, importfrom_pattern)[[1]]
      if (nrow(importfrom_matches) > 0) {
        deps$roxygen_importfrom <- importfrom_matches[, 2]
      }
      
      deps$suspected_packages <- self$detect_suspected_packages(code)
      
      return(deps)
    },
    
    # Detect suspected package usage
    detect_suspected_packages = function(code) {
      suspected <- c()
      
      patterns <- list(
        "ggplot2" = c("ggplot\\s*\\(", "geom_", "aes\\s*\\(", "theme_", "scale_", "labs\\s*\\("),
        "dplyr" = c("mutate\\s*\\(", "filter\\s*\\(", "select\\s*\\(", "arrange\\s*\\(", "summarise\\s*\\(", "group_by\\s*\\(", "%>%"),
        "tidyr" = c("pivot_longer\\s*\\(", "pivot_wider\\s*\\(", "separate\\s*\\(", "unite\\s*\\(", "nest\\s*\\("),
        "shiny" = c("fluidPage\\s*\\(", "renderPlot\\s*\\(", "observeEvent\\s*\\(", "reactive\\s*\\(", "actionButton\\s*\\("),
        "DT" = c("datatable\\s*\\(", "renderDataTable\\s*\\("),
        "stringr" = c("str_detect\\s*\\(", "str_replace\\s*\\(", "str_extract\\s*\\(", "str_split\\s*\\("),
        "purrr" = c("map\\s*\\(", "map_dfr\\s*\\(", "map_chr\\s*\\(", "walk\\s*\\(")
      )
      
      for (pkg in names(patterns)) {
        for (pattern in patterns[[pkg]]) {
          if (stringr::str_detect(code, pattern)) {
            suspected <- c(suspected, pkg)
            break
          }
        }
      }
      
      return(unique(suspected))
    },
    
    # Check which packages are missing
    check_missing_packages = function(deps) {
      all_packages <- unique(c(
        deps$library_calls,
        deps$require_calls, 
        deps$namespace_calls,
        deps$roxygen_imports,
        deps$roxygen_importfrom,
        deps$suspected_packages
      ))
      
      if (length(all_packages) == 0) return(c())
      
      base_packages <- c("base", "utils", "stats", "graphics", "grDevices", "methods", "datasets")
      all_packages <- setdiff(all_packages, base_packages)
      
      missing <- c()
      for (pkg in all_packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
          missing <- c(missing, pkg)
        }
      }
      
      return(missing)
    },
    
    # Build parameter documentation history
    build_param_history = function() {
      if (is.null(self$functions_data) || nrow(self$functions_data) == 0) {
        return(invisible(self))
      }
      
      for (i in seq_len(nrow(self$functions_data))) {
        if (self$functions_data$has_docs[i]) {
          docs <- self$functions_data$existing_docs[i]
          param_lines <- stringr::str_extract_all(docs, "@param\\s+\\w+[^\n]*")[[1]]
          
          for (param_line in param_lines) {
            param_match <- stringr::str_match(param_line, "@param\\s+(\\w+)\\s+(.*)")
            if (!is.na(param_match[1, 1])) {
              param_name <- param_match[1, 2]
              param_desc <- param_match[1, 3]
              
              if (is.null(self$param_history[[param_name]])) {
                self$param_history[[param_name]] <- list()
              }
              
              if (!param_desc %in% self$param_history[[param_name]]) {
                self$param_history[[param_name]] <- append(self$param_history[[param_name]], param_desc)
              }
            }
          }
        }
      }
      
      invisible(self)
    },
    
    # Scan dependencies
    scan_dependencies = function() {
      if (is.null(self$functions_data) || nrow(self$functions_data) == 0) {
        return(invisible(self))
      }
      
      all_deps <- list(
        library_calls = c(),
        require_calls = c(),
        namespace_calls = c(),
        roxygen_imports = c(),
        roxygen_importfrom = c(),
        suspected_packages = c()
      )
      
      all_missing <- c()
      
      for (i in seq_len(nrow(self$functions_data))) {
        func_deps <- self$functions_data$dependencies[[i]]
        func_missing <- self$functions_data$missing_deps[[i]]
        
        for (type in names(all_deps)) {
          if (type %in% names(func_deps)) {
            all_deps[[type]] <- c(all_deps[[type]], func_deps[[type]])
          }
        }
        
        all_missing <- c(all_missing, func_missing)
      }
      
      self$dependencies <- purrr::map(all_deps, unique)
      self$missing_deps <- unique(all_missing)
      
      self$check_description_dependencies()
      
      return(invisible(self))
    },
    
    # Check DESCRIPTION file dependencies
    check_description_dependencies = function() {
      desc_file <- file.path(self$package_path, "DESCRIPTION")
      if (!file.exists(desc_file)) {
        self$dependencies$description_status <- "No DESCRIPTION file found"
        return(invisible(self))
      }
      
      desc_content <- readLines(desc_file, warn = FALSE)
      desc_text <- paste(desc_content, collapse = "\n")
      
      imports_match <- stringr::str_match(desc_text, "Imports:\\s*([^\\n]*(?:\\n\\s+[^\\n]*)*)")
      depends_match <- stringr::str_match(desc_text, "Depends:\\s*([^\\n]*(?:\\n\\s+[^\\n]*)*)")
      suggests_match <- stringr::str_match(desc_text, "Suggests:\\s*([^\\n]*(?:\\n\\s+[^\\n]*)*)")
      
      desc_deps <- list()
      
      if (!is.na(imports_match[1, 2])) {
        imports_clean <- stringr::str_replace_all(imports_match[1, 2], "\\s*\\([^)]*\\)", "")
        imports_clean <- stringr::str_replace_all(imports_clean, "\\s+", " ")
        desc_deps$imports <- stringr::str_split(imports_clean, ",\\s*")[[1]]
        desc_deps$imports <- trimws(desc_deps$imports)
        desc_deps$imports <- desc_deps$imports[desc_deps$imports != ""]
      }
      
      if (!is.na(depends_match[1, 2])) {
        depends_clean <- stringr::str_replace_all(depends_match[1, 2], "\\s*\\([^)]*\\)", "")
        depends_clean <- stringr::str_replace_all(depends_clean, "\\s+", " ")
        desc_deps$depends <- stringr::str_split(depends_clean, ",\\s*")[[1]]
        desc_deps$depends <- trimws(desc_deps$depends)
        desc_deps$depends <- desc_deps$depends[desc_deps$depends != ""]
      }
      
      if (!is.na(suggests_match[1, 2])) {
        suggests_clean <- stringr::str_replace_all(suggests_match[1, 2], "\\s*\\([^)]*\\)", "")
        suggests_clean <- stringr::str_replace_all(suggests_clean, "\\s+", " ")
        desc_deps$suggests <- stringr::str_split(suggests_clean, ",\\s*")[[1]]
        desc_deps$suggests <- trimws(desc_deps$suggests)
        desc_deps$suggests <- desc_deps$suggests[desc_deps$suggests != ""]
      }
      
      self$dependencies$description_deps <- desc_deps
      
      all_detected <- unique(c(
        self$dependencies$library_calls,
        self$dependencies$require_calls,
        self$dependencies$namespace_calls,
        self$dependencies$suspected_packages
      ))
      
      all_declared <- unique(c(
        desc_deps$imports,
        desc_deps$depends,
        desc_deps$suggests
      ))
      
      self$dependencies$undeclared_deps <- setdiff(all_detected, all_declared)
      self$dependencies$unused_deps <- setdiff(all_declared, all_detected)
      
      return(invisible(self))
    },
    
    # Generate documentation template
    generate_template = function(function_name, args, existing_docs = "") {
      if (nchar(existing_docs) > 0) {
        return(existing_docs)
      }
      
      template <- paste0("#' ", function_name, "\n#'\n#' [Description]\n#'\n")
      
      for (arg_name in names(args)) {
        if (arg_name %in% names(self$param_history)) {
          suggested_desc <- self$param_history[[arg_name]][[1]]
          template <- paste0(template, "#' @param ", arg_name, " ", suggested_desc, "\n")
        } else {
          template <- paste0(template, "#' @param ", arg_name, " [Description]\n")
        }
      }
      
      template <- paste0(template, "#' @return [Description]\n")
      template <- paste0(template, "#' @examples\n")
      template <- paste0(template, "#' # [Example]\n")
      template <- paste0(template, "#' @export\n")
      
      return(template)
    },
    
    # Save documentation to file
    save_documentation = function(file_path, function_name, new_docs, old_function_text) {
      content <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
      
      func_without_docs <- stringr::str_replace(old_function_text, "^(#'[^\n]*\n)*", "")
      new_function_text <- paste0(new_docs, "\n", func_without_docs)
      
      content <- stringr::str_replace(content, stringr::fixed(old_function_text), new_function_text)
      
      writeLines(strsplit(content, "\n")[[1]], file_path)
      
      return(TRUE)
    },
    
    # Analyze namespace conversion opportunities
    analyze_namespace_opportunities = function() {
      
      # Get function mappings from DESCRIPTION
      tryCatch({
        if (exists("build_mappings_from_description", mode = "function")) {
          mappings <- build_mappings_from_description(self$package_path, verbose = FALSE)
        } else {
          # Fallback: simple DESCRIPTION parsing
          mappings <- self$simple_build_mappings_from_description()
        }
      }, error = function(e) {
        return(list(
          summary = paste("Error reading DESCRIPTION file:", e$message),
          mappings = list(),
          opportunities = list()
        ))
      })
      
      if (length(mappings) == 0) {
        return(list(
          summary = "No dependencies found in DESCRIPTION file",
          mappings = list(),
          opportunities = list()
        ))
      }
      
      if (is.null(self$functions_data) || nrow(self$functions_data) == 0) {
        self$scan_package()
      }
      
      r_files <- unique(self$functions_data$file_path)
      opportunities <- list()
      
      for (file in r_files) {
        if (!file.exists(file)) next
        
        content <- readLines(file, warn = FALSE)
        file_opportunities <- self$find_conversion_opportunities_in_file(content, mappings)
        
        if (length(file_opportunities) > 0) {
          opportunities[[basename(file)]] <- file_opportunities
        }
      }
      
      total_opportunities <- sum(sapply(opportunities, length))
      packages_involved <- unique(unlist(lapply(opportunities, function(x) sapply(x, function(y) y$package))))
      
      summary_text <- paste0(
        "NAMESPACE CONVERSION OPPORTUNITIES\n",
        "=====================================\n",
        "Dependencies in DESCRIPTION: ", length(unique(unlist(mappings))), " packages\n",
        "Available function mappings: ", length(mappings), " functions\n",
        "Files with opportunities: ", length(opportunities), "\n",
        "Total conversion opportunities: ", total_opportunities, "\n",
        "Packages involved: ", paste(packages_involved, collapse = ", ")
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
    },
    
    # Simple fallback function to build mappings from DESCRIPTION
    simple_build_mappings_from_description = function() {
      desc_file <- file.path(self$package_path, "DESCRIPTION")
      if (!file.exists(desc_file)) {
        return(list())
      }
      
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
        return(list())
      }
      
      target_packages <- target_packages[target_packages != "R"]
      
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
          }, error = function(e) {
            # Skip packages that cause errors
          })
        }
      }
      
      return(mappings)
    },
    
    # Find conversion opportunities in a single file
    find_conversion_opportunities_in_file = function(content, mappings) {
      opportunities <- list()
      
      for (i in seq_along(content)) {
        line <- content[i]
        
        if (stringr::str_detect(trimws(line), "^#")) next
        
        for (func_name in names(mappings)) {
          escaped_func <- gsub("([\\[\\]\\(\\)\\{\\}\\^\\$\\*\\+\\?\\|\\\\\\.])", "\\\\\\1", func_name)
          pattern <- paste0("(?<!::)\\b", escaped_func, "\\s*\\(")
          
          if (grepl(pattern, line, perl = TRUE)) {
            opportunities[[paste0(func_name, "_line_", i)]] <- list(
              function_name = func_name,
              package = mappings[[func_name]],
              line_number = i,
              line_content = line
            )
          }
        }
      }
      
      return(opportunities)
    },
    
    # Auto-convert function calls to namespace format
    auto_namespace_conversion = function(target_files = NULL, 
                                       backup = TRUE, 
                                       preview_only = FALSE,
                                       custom_mappings = NULL,
                                       exclude_functions = c(),
                                       include_suggests = FALSE) {
      
      # Build function mappings from DESCRIPTION file
      tryCatch({
        if (exists("build_mappings_from_description", mode = "function")) {
          func_to_package <- build_mappings_from_description(
            package_path = self$package_path,
            include_suggests = include_suggests,
            custom_mappings = custom_mappings,
            verbose = preview_only
          )
        } else {
          # Fallback to simple method
          func_to_package <- self$simple_build_mappings_from_description()
          if (!is.null(custom_mappings)) {
            func_to_package <- c(func_to_package, custom_mappings)
          }
        }
      }, error = function(e) {
        stop("Could not build function mappings from DESCRIPTION file: ", e$message)
      })
      
      if (length(func_to_package) == 0) {
        message("No function mappings found. Check your DESCRIPTION file dependencies.")
        return(list())
      }
      
      # If no specific files provided, use all files from functions_data
      if (is.null(target_files)) {
        if (is.null(self$functions_data) || nrow(self$functions_data) == 0) {
          stop("No functions data available. Run scan_package() first.")
        }
        target_files <- unique(self$functions_data$file_path)
      }
      
      # Process each file
      conversion_results <- list()
      
      for (file_path in target_files) {
        if (!file.exists(file_path)) {
          warning("File not found: ", file_path)
          next
        }
        
        message("Converting: ", file_path)
        result <- self$convert_file_namespaces(
          file_path = file_path,
          func_to_package = func_to_package,
          backup = backup,
          preview_only = preview_only,
          exclude_functions = exclude_functions
        )
        
        conversion_results[[file_path]] <- result
      }
      
      # If changes were made and not preview only, rescan the package
      if (!preview_only && any(sapply(conversion_results, function(x) x$lines_modified > 0))) {
        message("Rescanning package after namespace conversion...")
        self$scan_package()
        self$scan_dependencies()
      }
      
      return(conversion_results)
    },
    
    # Convert a single file's function calls to namespace format
    convert_file_namespaces = function(file_path, func_to_package, backup, preview_only, exclude_functions) {
      
      # Read the file
      original_content <- readLines(file_path, warn = FALSE)
      modified_content <- original_content
      
      # Track changes
      changes <- list()
      
      # Process each line
      for (i in seq_along(modified_content)) {
        line <- modified_content[i]
        
        # Skip comments and strings (basic detection)
        if (self$is_comment_or_string_line(line)) {
          next
        }
        
        # Find function calls that need namespacing
        line_changes <- self$process_line_for_namespacing(line, func_to_package, exclude_functions)
        
        if (length(line_changes$modified_line) > 0 && line_changes$modified_line != line) {
          modified_content[i] <- line_changes$modified_line
          
          # Record the change
          changes[[length(changes) + 1]] <- list(
            line_number = i,
            original = line,
            modified = line_changes$modified_line,
            functions_changed = line_changes$functions_changed
          )
        }
      }
      
      # Results summary
      results <- list(
        file_path = file_path,
        total_lines = length(original_content),
        lines_modified = length(changes),
        changes = changes,
        functions_converted = unique(unlist(lapply(changes, function(x) x$functions_changed))),
        preview_only = preview_only
      )
      
      # Apply changes if not preview only
      if (!preview_only && length(changes) > 0) {
        # Create backup if requested
        if (backup) {
          backup_path <- paste0(file_path, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
          file.copy(file_path, backup_path)
          results$backup_path <- backup_path
          message("Backup created: ", backup_path)
        }
        
        # Write modified content
        writeLines(modified_content, file_path)
        message("File modified: ", file_path)
        message("Lines changed: ", length(changes))
      }
      
      return(results)
    },
    
    # Check if a line is primarily a comment or string
    is_comment_or_string_line = function(line) {
      trimmed <- trimws(line)
      
      # Skip empty lines
      if (nchar(trimmed) == 0) return(TRUE)
      
      # Skip comment lines (basic detection)
      if (startsWith(trimmed, "#")) return(TRUE)
      
      # Skip roxygen lines
      if (startsWith(trimmed, "#'")) return(TRUE)
      
      return(FALSE)
    },
    
    # Process a single line for namespace conversion
    process_line_for_namespacing = function(line, func_to_package, exclude_functions) {
      
      modified_line <- line
      functions_changed <- c()
      
      # Find function calls using regex
      for (func_name in names(func_to_package)) {
        
        # Skip if function is in exclude list
        if (func_name %in% exclude_functions) next
        
        package_name <- func_to_package[[func_name]]
        
        # Escape special regex characters in function name
        escaped_func <- gsub("([\\[\\]\\(\\)\\{\\}\\^\\$\\*\\+\\?\\|\\\\\\.])", "\\\\\\1", func_name)
        
        # Pattern to match function calls that don't already have namespace
        pattern <- paste0("(?<!::)\\b", escaped_func, "\\s*\\(")
        
        # Check if pattern exists in line using perl=TRUE for lookbehind
        if (grepl(pattern, line, perl = TRUE)) {
          
          # Replace with namespaced version
          replacement <- paste0(package_name, "::", func_name, "(")
          # Use a simpler pattern for replacement
          simple_pattern <- paste0("\\b", escaped_func, "\\s*\\(")
          
          # Only replace if not already namespaced
          if (!grepl(paste0(package_name, "::", escaped_func), line)) {
            modified_line <- gsub(simple_pattern, replacement, modified_line)
            functions_changed <- c(functions_changed, func_name)
          }
        }
      }
      
      return(list(
        modified_line = modified_line,
        functions_changed = unique(functions_changed)
      ))
    }
  )
)