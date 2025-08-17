#' Analyze R Package Structure and Functions (UPDATED)
#'
#' Main function to analyze an R package, extracting functions, documentation,
#' dependencies, and other metadata. Now with smart path detection.
#' Updated to handle both .R and .r file extensions.
#'
#' @param path Character. Path to package directory (default: auto-detect)
#' @param include_dependencies Logical. Whether to analyze dependencies (default: FALSE)
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @return List with package analysis results
#'
#' @examples
#' \dontrun{
#' # Auto-detect and analyze current package
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
analyze_package <- function(path = NULL, include_dependencies = FALSE, verbose = TRUE) {
  
  # Smart path detection
  if (is.null(path)) {
    path <- smart_detect_package_path()
  }
  
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

#' Detect Package Root Directory (UPDATED)
#'
#' @param path Character. Initial path (default: auto-detect)
#' @param verbose Logical. Print messages
#' @return Character. Package root path
#' @export
detect_package_root <- function(path = NULL, verbose = TRUE) {
  
  # Smart path detection if not provided
  if (is.null(path)) {
    path <- smart_detect_package_path()
  }
  
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

#' Scan R Functions in Package (UPDATED)
#'
#' Updated to scan both .R and .r files (case insensitive) with smart path detection.
#'
#' @param package_path Character. Package root path (default: auto-detect)
#' @param verbose Logical. Print messages
#' @return Data.frame with function information
#' @export
scan_r_functions <- function(package_path = NULL, verbose = TRUE) {
  
  # Smart path detection
  if (is.null(package_path)) {
    package_path <- smart_detect_package_path()
  }
  
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

 

#' Extract Functions from Single R File (WORKING VERSION)
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
  
  # Join all lines into single text for pattern matching
  file_text <- paste(content, collapse = "\n")
  
  # Use the pattern that worked in debug
  func_pattern <- "([a-zA-Z_][a-zA-Z0-9_\\.]*)\\s*<-\\s*function"
  
  # Find all matches
  matches <- gregexpr(func_pattern, file_text, perl = TRUE)[[1]]
  
  if (matches[1] == -1) {
    return(data.frame(
      file_path = character(0),
      file_name = character(0),
      function_name = character(0),
      args_string = character(0),
      start_pos = integer(0),
      has_docs = logical(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Initialize result data frame
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
    func_start_pos <- matches[i]
    
    # Extract function name from capture group
    func_name_start <- match_data[i, 1]
    func_name_length <- match_lengths[i, 1]
    function_name <- substr(file_text, func_name_start, func_name_start + func_name_length - 1)
    
    # Extract arguments - find the parentheses after function
    args_string <- extract_function_args_simple(file_text, func_start_pos)
    
    # Find documentation start by looking backwards from function
    docs_info <- find_docs_start_simple(file_text, func_start_pos)
    
    functions_info <- rbind(functions_info, data.frame(
      file_path = file_path,
      file_name = basename(file_path),
      function_name = function_name,
      args_string = args_string,
      start_pos = docs_info$start_pos,
      has_docs = docs_info$has_docs,
      stringsAsFactors = FALSE
    ))
  }
  
  return(functions_info)
}

#' Extract Function Arguments (Simple Version)
#'
#' @param file_text Character. Full file content
#' @param func_start_pos Integer. Position where function definition starts
#' @return Character. Arguments string
extract_function_args_simple <- function(file_text, func_start_pos) {
  
  # Find "function(" starting from func_start_pos
  remaining_text <- substr(file_text, func_start_pos, nchar(file_text))
  
  # Look for "function" followed by "("
  func_match <- stringr::str_locate(remaining_text, "function\\s*\\(")
  
  if (is.na(func_match[1, 1])) {
    return("")
  }
  
  # Position of opening parenthesis in full text
  paren_start <- func_start_pos + func_match[1, 2]
  
  # Find matching closing parenthesis
  paren_count <- 1
  pos <- paren_start
  
  while (pos <= nchar(file_text) && paren_count > 0) {
    char <- substr(file_text, pos, pos)
    if (char == "(") {
      paren_count <- paren_count + 1
    } else if (char == ")") {
      paren_count <- paren_count - 1
    }
    pos <- pos + 1
  }
  
  if (paren_count == 0) {
    # Found matching parenthesis
    args_end <- pos - 2
    args_text <- substr(file_text, paren_start, args_end)
    
    # Clean up whitespace and newlines
    args_text <- gsub("\\s+", " ", args_text)
    args_text <- trimws(args_text)
    
    return(args_text)
  }
  
  return("")
}

#' Find Documentation Start (Simple Version)
#'
#' @param file_text Character. Full file content
#' @param func_start_pos Integer. Position where function starts
#' @return List with start_pos and has_docs
find_docs_start_simple <- function(file_text, func_start_pos) {
  
  # Convert to lines
  lines <- strsplit(file_text, "\n")[[1]]
  
  # Find which line the function is on
  text_before_func <- substr(file_text, 1, func_start_pos - 1)
  func_line_num <- length(strsplit(text_before_func, "\n")[[1]])
  
  # Look backwards for roxygen documentation
  doc_start_line <- func_line_num
  found_docs <- FALSE
  
  # Search backwards up to 100 lines
  for (i in (func_line_num - 1):max(1, func_line_num - 100)) {
    if (i < 1) break
    
    line <- lines[i]
    trimmed_line <- trimws(line)
    
    if (stringr::str_detect(trimmed_line, "^#'")) {
      doc_start_line <- i
      found_docs <- TRUE
    } else if (nchar(trimmed_line) == 0) {
      # Empty line - if we're collecting docs, include it
      if (found_docs) {
        doc_start_line <- i
      }
    } else {
      # Non-roxygen, non-empty line
      if (found_docs) {
        # Found end of docs block, docs start after this line
        doc_start_line <- i + 1
        break
      }
    }
  }
  
  # Calculate character position
  start_pos <- if (found_docs && doc_start_line < func_line_num) {
    if (doc_start_line == 1) {
      1
    } else {
      sum(nchar(lines[1:(doc_start_line - 1)])) + (doc_start_line - 1) + 1
    }
  } else {
    func_start_pos
  }
  
  return(list(
    start_pos = start_pos,
    has_docs = found_docs
  ))
}

#' Extract Function Documentation (WITH WARNINGS)
#'
#' @param file_path Character. Path to file
#' @param start_pos Integer. Starting position (for backward compatibility)
#' @param function_name Character. Function name for better extraction
#' @return Character. Complete documentation string
extract_function_docs <- function(file_path, start_pos, function_name = NULL) {
  
  tryCatch({
    content <- readLines(file_path, warn = FALSE)
  }, error = function(e) {
    warning("Could not read file: ", file_path)
    return("")
  })
  
  if (length(content) == 0) {
    return("")
  }
  
  # Use function name for most reliable extraction
  if (!is.null(function_name)) {
    return(extract_complete_roxygen_block(content, function_name, file_path))
  }
  
  # Fallback to position-based
  file_text <- paste(content, collapse = "\n")
  text_before <- substr(file_text, 1, start_pos - 1)
  line_num <- length(strsplit(text_before, "\n")[[1]])
  
  return(extract_complete_roxygen_block_by_line(content, line_num, function_name, file_path))
}

#' Extract Complete Roxygen Block by Function Name (WITH WARNINGS)
#'
#' @param content Character vector. File lines
#' @param function_name Character. Function name
#' @param file_path Character. File path for warnings
#' @return Character. Complete roxygen documentation
extract_complete_roxygen_block <- function(content, function_name, file_path = NULL) {
  
  # Find function definition line
  func_patterns <- c(
    paste0("^\\s*", stringr::str_escape(function_name), "\\s*<-\\s*function"),
    paste0("\\b", stringr::str_escape(function_name), "\\s*<-\\s*function")
  )
  
  func_line <- NULL
  for (pattern in func_patterns) {
    matches <- which(stringr::str_detect(content, pattern))
    if (length(matches) > 0) {
      func_line <- matches[1]
      break
    }
  }
  
  if (is.null(func_line)) {
    return("")
  }
  
  return(extract_complete_roxygen_block_by_line(content, func_line, function_name, file_path))
}

#' Extract Complete Roxygen Block by Line Number (WITH WARNINGS)
#'
#' @param content Character vector. File lines
#' @param func_line Integer. Line where function is defined
#' @param function_name Character. Function name for warning messages
#' @param file_path Character. File path for warning messages
#' @return Character. Complete roxygen documentation
extract_complete_roxygen_block_by_line <- function(content, func_line, function_name = NULL, file_path = NULL) {
  
  # Strategy: Find ALL roxygen lines above the function, warn about breaks
  
  roxygen_lines_with_positions <- list()
  broken_blocks <- list()  # Track where blocks are broken
  
  # Track state for detecting breaks
  in_roxygen_block <- FALSE
  last_roxygen_line <- NULL
  
  for (i in (func_line - 1):1) {
    line <- content[i]
    trimmed <- trimws(line)
    
    if (stringr::str_detect(trimmed, "^#'")) {
      # This is a roxygen line
      roxygen_lines_with_positions[[length(roxygen_lines_with_positions) + 1]] <- list(
        line_num = i,
        content = line
      )
      
      # Check if there was a break in the block
      if (in_roxygen_block && !is.null(last_roxygen_line) && (last_roxygen_line - i) > 1) {
        # There was a gap - check what caused it
        gap_lines <- (i + 1):(last_roxygen_line - 1)
        breaking_lines <- c()
        
        for (gap_line in gap_lines) {
          gap_content <- trimws(content[gap_line])
          if (nchar(gap_content) > 0 && !stringr::str_detect(gap_content, "^#'")) {
            # This line broke the roxygen block
            breaking_lines <- c(breaking_lines, gap_line)
          }
        }
        
        if (length(breaking_lines) > 0) {
          broken_blocks[[length(broken_blocks) + 1]] <- list(
            start_line = i,
            end_line = last_roxygen_line,
            breaking_lines = breaking_lines
          )
        }
      }
      
      in_roxygen_block <- TRUE
      last_roxygen_line <- i
      
    } else if (nchar(trimmed) == 0) {
      # Empty line - might be part of roxygen block, check context
      has_roxygen_before <- FALSE
      has_roxygen_after <- FALSE
      
      # Check a few lines before
      for (j in (i-1):max(1, i-5)) {
        if (j >= 1 && stringr::str_detect(trimws(content[j]), "^#'")) {
          has_roxygen_before <- TRUE
          break
        }
      }
      
      # Check a few lines after
      for (j in (i+1):min(func_line-1, i+5)) {
        if (j <= func_line-1 && stringr::str_detect(trimws(content[j]), "^#'")) {
          has_roxygen_after <- TRUE
          break
        }
      }
      
      # Include empty line if it's between roxygen lines
      if (has_roxygen_before && has_roxygen_after) {
        roxygen_lines_with_positions[[length(roxygen_lines_with_positions) + 1]] <- list(
          line_num = i,
          content = line
        )
      }
    }
    # Note: We ignore regular comments (#) and code lines but track them as potential breaks
  }
  
  # Reverse the order and extract content
  if (length(roxygen_lines_with_positions) == 0) {
    return("")
  }
  
  roxygen_lines_with_positions <- rev(roxygen_lines_with_positions)
  final_lines <- sapply(roxygen_lines_with_positions, function(x) x$content)
  
  return(paste(final_lines, collapse = "\n"))
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
      
      # Pass function name for better extraction
      docs_data$existing_docs[i] <- extract_function_docs(file_path, start_pos, func_name)
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