#' Analyze R Package Structure and Functions
#'
#' Main function to analyze an R package, extracting functions, documentation,
#' dependencies, and other metadata. Returns a simple list structure.
#' Updated to handle both .R and .r file extensions.
#'
#' @param path Character. Path to package directory (default: ".")
#' @param include_dependencies Logical. Whether to analyze dependencies (default: FALSE)
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @return List with package analysis results
#'
#' @examples
#' \dontrun{
#' # Analyze current package
#' pkg_info <- analyze_package()
#' 
#' # Analyze specific package
#' pkg_info <- analyze_package("/path/to/package")
#' 
#' # Quick analysis without dependencies
#' pkg_info <- analyze_package(include_dependencies = FALSE)
#' }
#'
#' @export
analyze_package <- function(path = ".", include_dependencies = FALSE, verbose = TRUE) {
  
  if (verbose) message("Analyzing package at: ", path)
  
  # Detect package root
  package_path <- detect_package_root(path, verbose = verbose)
  
  # Scan R functions (now handles both .R and .r)
  if (verbose) message("Scanning R functions...")
  functions_data <- scan_r_functions(package_path, verbose = verbose)
  
  # Parse documentation
  if (verbose) message("Parsing existing documentation...")
  docs_data <- parse_function_docs(functions_data)
  
  # Build parameter history from existing docs
  if (verbose) message("Building parameter suggestions...")
  param_history <- build_parameter_history(docs_data)
  
  result <- list(
    package_path = package_path,
    functions = functions_data,
    documentation = docs_data,
    parameter_history = param_history,
    stats = list(
      total_functions = nrow(functions_data),
      documented_functions = sum(docs_data$has_docs, na.rm = TRUE),
      total_files = length(unique(functions_data$file_path))
    )
  )
  
  # Add dependency analysis if requested
  if (include_dependencies) {
    if (verbose) message("Analyzing dependencies...")
    deps <- analyze_package_dependencies(package_path, functions_data, verbose = verbose)
    result$dependencies <- deps
  }
  
  if (verbose) {
    message("Package analysis complete!")
    message("  Functions found: ", result$stats$total_functions)
    message("  Documented: ", result$stats$documented_functions)
    message("  Files: ", result$stats$total_files)
    if (include_dependencies) {
      message("  Missing packages: ", length(result$dependencies$missing))
    }
  }
  
  return(result)
}

#' Detect Package Root Directory
#'
#' @param path Character. Initial path
#' @param verbose Logical. Print messages
#' @return Character. Package root path
#' @export
detect_package_root <- function(path, verbose = TRUE) {
  path <- normalizePath(path, mustWork = FALSE)
  current_path <- path
  
  for (i in 1:3) {
    if (is_package_root(current_path)) {
      if (current_path != path && verbose) {
        message("Detected package root at: ", current_path)
      }
      return(current_path)
    }
    
    parent_path <- dirname(current_path)
    if (parent_path == current_path) break
    current_path <- parent_path
  }
  
  if (!is_package_root(path) && verbose) {
    warning("Directory doesn't appear to be an R package root")
  }
  
  return(path)
}

#' Check if Directory is Package Root
#'
#' @param path Character. Path to check
#' @return Logical. TRUE if package root
is_package_root <- function(path) {
  has_description <- file.exists(file.path(path, "DESCRIPTION"))
  has_r_dir <- dir.exists(file.path(path, "R"))
  has_r_files <- length(list.files(path, pattern = "\\.[Rr]$")) > 0  # Fixed: case insensitive
  has_namespace <- file.exists(file.path(path, "NAMESPACE"))
  
  return(has_description && (has_r_dir || has_r_files || has_namespace))
}

#' Scan R Functions in Package
#'
#' Updated to scan both .R and .r files (case insensitive).
#'
#' @param package_path Character. Package root path
#' @param verbose Logical. Print messages
#' @return Data.frame with function information
#' @export
scan_r_functions <- function(package_path, verbose = TRUE) {
  
  # Find R directories
  possible_dirs <- c(
    file.path(package_path, "R"),
    package_path,
    file.path(package_path, "src")
  )
  
  r_files <- c()
  for (dir in possible_dirs) {
    if (dir.exists(dir)) {
      # Updated pattern to match both .R and .r files
      files <- list.files(dir, pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)
      r_files <- c(r_files, files)
    }
  }
  
  r_files <- unique(r_files)
  
  if (length(r_files) == 0) {
    if (verbose) {
      message("No R files found. Checked directories:")
      for (dir in possible_dirs) {
        if (dir.exists(dir)) {
          message("  ", dir, " (exists)")
        } else {
          message("  ", dir, " (does not exist)")
        }
      }
    }
    stop("No R files found in package")
  }
  
  if (verbose) {
    message("Found ", length(r_files), " R files:")
    for (file in r_files) {
      message("  ", basename(file))
    }
  }
  
  # Parse each file
  all_functions <- list()
  for (file_path in r_files) {
    functions <- extract_functions_from_file(file_path)
    if (nrow(functions) > 0) {
      all_functions[[file_path]] <- functions
    }
  }
  
  if (length(all_functions) == 0) {
    if (verbose) message("No functions found in any R files")
    return(data.frame())
  }
  
  # Combine all functions
  result <- do.call(rbind, all_functions)
  rownames(result) <- NULL
  
  if (verbose) {
    message("Found ", nrow(result), " functions total")
  }
  
  return(result)
}

#' Extract Functions from Single R File
#'
#' @param file_path Character. Path to R file
#' @return Data.frame with function information
extract_functions_from_file <- function(file_path) {
  
  tryCatch({
    content <- readLines(file_path, warn = FALSE)
  }, error = function(e) {
    warning("Could not read file: ", file_path)
    return(data.frame())
  })
  
  if (length(content) == 0) return(data.frame())
  
  file_text <- paste(content, collapse = "\n")
  
  # Improved regex to find function definitions with better capture groups
  # This regex looks for: (optional roxygen comments)(whitespace)(function_name)<-function(args)
  func_pattern <- "(?:^|\\n)((?:\\s*#'[^\\n]*\\n)*)(\\s*)([a-zA-Z_][a-zA-Z0-9_\\.]*)\\s*<-\\s*function\\s*\\(([^\\)]*)\\)"
  
  matches <- gregexpr(func_pattern, file_text, perl = TRUE)[[1]]
  
  if (matches[1] == -1) {
    return(data.frame())
  }
  
  # Extract function information
  functions_info <- data.frame(
    file_path = character(0),
    file_name = character(0),
    function_name = character(0),
    args_string = character(0),
    start_pos = integer(0),
    has_docs = logical(0),
    stringsAsFactors = FALSE
  )
  
  match_data <- attr(matches, "capture.start")
  match_lengths <- attr(matches, "capture.length")
  
  for (i in seq_along(matches)) {
    match_start <- matches[i]
    
    # Extract captured groups
    docs_start <- match_data[i, 1]
    docs_length <- match_lengths[i, 1]
    
    whitespace_start <- match_data[i, 2]
    whitespace_length <- match_lengths[i, 2]
    
    func_name_start <- match_data[i, 3]
    func_name_length <- match_lengths[i, 3]
    
    args_start <- match_data[i, 4]
    args_length <- match_lengths[i, 4]
    
    # Extract strings
    existing_docs <- if (docs_length > 0) {
      substr(file_text, docs_start, docs_start + docs_length - 1)
    } else {
      ""
    }
    
    function_name <- substr(file_text, func_name_start, func_name_start + func_name_length - 1)
    args_string <- substr(file_text, args_start, args_start + args_length - 1)
    
    # The start position should be the beginning of the documentation or function
    # If there are docs, start from docs; if not, start from function
    start_pos <- if (docs_length > 0) docs_start else whitespace_start
    
    # Check if we actually have documentation
    has_docs_flag <- docs_length > 0 && nchar(trimws(existing_docs)) > 0
    
    functions_info <- rbind(functions_info, data.frame(
      file_path = file_path,
      file_name = basename(file_path),
      function_name = function_name,
      args_string = trimws(args_string),
      start_pos = start_pos,
      has_docs = has_docs_flag,
      stringsAsFactors = FALSE
    ))
  }
  
  return(functions_info)
}

#' Extract Function Documentation from File (Fixed)
#'
#' @param file_path Character. Path to file
#' @param start_pos Integer. Starting position of function or its documentation
#' @return Character. Documentation string
extract_function_docs <- function(file_path, start_pos) {
  
  tryCatch({
    content <- readLines(file_path, warn = FALSE)
  }, error = function(e) {
    warning("Could not read file: ", file_path)
    return("")
  })
  
  if (length(content) == 0) {
    return("")
  }
  
  file_text <- paste(content, collapse = "\n")
  
  # Convert character position to line number
  text_before <- substr(file_text, 1, start_pos - 1)
  lines_before <- length(strsplit(text_before, "\n")[[1]])
  
  # Start from the line before start_pos and work backwards
  lines <- strsplit(file_text, "\n")[[1]]
  
  # Find the actual function line by looking forward from start_pos
  function_line_idx <- lines_before
  
  # Work backwards from the function line to collect all roxygen comments
  roxygen_lines <- character(0)
  
  # Start from the line just before the function and work backwards
  for (i in (function_line_idx - 1):max(1, function_line_idx - 50)) {
    if (i < 1) break
    
    line <- lines[i]
    trimmed_line <- trimws(line)
    
    # If we hit an empty line, continue (allow gaps in documentation)
    if (nchar(trimmed_line) == 0) {
      # Only continue if we haven't started collecting roxygen lines yet
      # or if we're in the middle of documentation block
      if (length(roxygen_lines) == 0) {
        next
      } else {
        # If we already have roxygen lines, an empty line might be part of the docs
        roxygen_lines <- c(line, roxygen_lines)
        next
      }
    }
    
    # Check for roxygen comment
    if (stringr::str_detect(trimmed_line, "^#'")) {
      roxygen_lines <- c(line, roxygen_lines)
    } else {
      # Stop at first non-roxygen, non-empty line
      break
    }
  }
  
  # If we found roxygen lines, combine them
  if (length(roxygen_lines) > 0) {
    # Remove the reversal - roxygen_lines is already in correct order
    return(paste(roxygen_lines, collapse = "\n"))
  }
  
  return("")
}

#' Parse Function Documentation
#'
#' @param functions_data Data.frame from scan_r_functions
#' @return Data.frame with documentation information
#' @export
parse_function_docs <- function(functions_data) {
  
  if (nrow(functions_data) == 0) {
    return(data.frame())
  }
  
  docs_data <- functions_data
  docs_data$parsed_args <- vector("list", nrow(functions_data))
  docs_data$existing_docs <- character(nrow(functions_data))
  
  for (i in seq_len(nrow(functions_data))) {
    # Parse function arguments
    args_string <- functions_data$args_string[i]
    docs_data$parsed_args[[i]] <- parse_function_arguments(args_string)
    
    # Extract existing documentation if present
    if (functions_data$has_docs[i]) {
      file_path <- functions_data$file_path[i]
      func_name <- functions_data$function_name[i]
      start_pos <- functions_data$start_pos[i]
      
      docs_data$existing_docs[i] <- extract_function_docs(file_path, start_pos)
    } else {
      docs_data$existing_docs[i] <- ""
    }
  }
  
  return(docs_data)
}

#' Parse Function Arguments String
#'
#' @param args_string Character. Function arguments as string
#' @return Named list of arguments and their defaults
parse_function_arguments <- function(args_string) {
  
  if (is.na(args_string) || nchar(trimws(args_string)) == 0) {
    return(list())
  }
  
  # Split by comma, but be careful with nested structures
  args <- strsplit(args_string, ",")[[1]]
  args <- trimws(args)
  
  arg_list <- list()
  for (arg in args) {
    if (nchar(arg) == 0) next
    
    if (grepl("=", arg)) {
      parts <- strsplit(arg, "=", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        name <- trimws(parts[1])
        default <- trimws(paste(parts[-1], collapse = "="))
        arg_list[[name]] <- default
      }
    } else {
      name <- trimws(arg)
      if (name != "") {
        arg_list[[name]] <- NULL
      }
    }
  }
  
  return(arg_list)
}

#' Extract Function Documentation from File
#'
#' @param file_path Character. Path to file
#' @param start_pos Integer. Starting position of function
#' @return Character. Documentation string
extract_function_docs <- function(file_path, start_pos) {
  
  tryCatch({
    content <- readLines(file_path, warn = FALSE)
  }, error = function(e) {
    warning("Could not read file: ", file_path)
    return("")
  })
  
  if (length(content) == 0) {
    return("")
  }
  
  file_text <- paste(content, collapse = "\n")
  
  # Extract text before the function definition
  before_func <- substr(file_text, 1, start_pos - 1)
  
  # Find roxygen comments immediately before function
  lines_before <- strsplit(before_func, "\n")[[1]]
  
  if (length(lines_before) == 0) {
    return("")
  }
  
  # Work backwards to find roxygen comments
  roxygen_lines <- character(0)
  for (i in length(lines_before):1) {
    line <- lines_before[i]
    
    # Handle empty or NULL lines
    if (is.na(line) || is.null(line) || length(line) == 0) {
      next
    }
    
    line <- trimws(line)
    
    # Check for roxygen comment
    if (nchar(line) > 0 && grepl("^#'", line)) {
      roxygen_lines <- c(line, roxygen_lines)
    } else if (nchar(line) == 0) {
      # Skip empty lines
      next
    } else {
      # Stop at first non-roxygen, non-empty line
      break
    }
  }
  
  return(paste(roxygen_lines, collapse = "\n"))
}

#' Build Parameter History from Existing Documentation
#'
#' @param docs_data Data.frame with documentation information
#' @return Named list of parameter suggestions
#' @export
build_parameter_history <- function(docs_data) {
  
  param_history <- list()
  
  if (nrow(docs_data) == 0) {
    return(param_history)
  }
  
  for (i in seq_len(nrow(docs_data))) {
    if (!docs_data$has_docs[i]) next
    
    docs <- docs_data$existing_docs[i]
    if (nchar(docs) == 0) next
    
    # Extract @param lines
    param_lines <- stringr::str_extract_all(docs, "#'\\s*@param\\s+\\w+[^\n]*")[[1]]
    
    for (param_line in param_lines) {
      # Parse @param name description
      param_match <- stringr::str_match(param_line, "#'\\s*@param\\s+(\\w+)\\s+(.*)")
      
      if (!is.na(param_match[1, 1])) {
        param_name <- param_match[1, 2]
        param_desc <- trimws(param_match[1, 3])
        
        if (nchar(param_desc) > 0) {
          if (is.null(param_history[[param_name]])) {
            param_history[[param_name]] <- character(0)
          }
          
          # Add unique descriptions only
          if (!param_desc %in% param_history[[param_name]]) {
            param_history[[param_name]] <- c(param_history[[param_name]], param_desc)
          }
        }
      }
    }
  }
  
  return(param_history)
}