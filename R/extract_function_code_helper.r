#' Extract Function Code from File
#'
#' Extracts the complete function definition from an R file.
#'
#' @param file_path Character. Path to R file
#' @param function_name Character. Name of function to extract
#' @return Character. Complete function code including roxygen docs
extract_function_code <- function(file_path, function_name) {
  
  if (!file.exists(file_path)) {
    return(paste("Error: File not found:", file_path))
  }
  
  tryCatch({
    content <- readLines(file_path, warn = FALSE)
    
    if (length(content) == 0) {
      return("Error: File is empty")
    }
    
    # Find the function definition line
    func_patterns <- c(
      paste0("^\\s*", stringr::str_escape(function_name), "\\s*<-\\s*function"),
      paste0("^\\s*", stringr::str_escape(function_name), "\\s*=\\s*function"),
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
      return(paste("Error: Function", function_name, "not found in file"))
    }
    
    # Find the start of documentation (look backwards for roxygen comments)
    doc_start <- func_line
    for (i in (func_line - 1):max(1, func_line - 50)) {
      if (i < 1) break
      
      line <- trimws(content[i])
      if (stringr::str_detect(line, "^#'")) {
        doc_start <- i
      } else if (nchar(line) == 0) {
        # Empty line - continue looking
        next
      } else if (!stringr::str_detect(line, "^#'")) {
        # Non-roxygen, non-empty line - stop looking
        if (doc_start < func_line) {
          # We found some docs, start from the line after this non-roxygen line
          doc_start <- i + 1
        }
        break
      }
    }
    
    # Find the end of the function
    func_end <- find_function_end(content, func_line)
    
    if (is.null(func_end)) {
      func_end <- min(length(content), func_line + 50)  # Fallback: take next 50 lines
    }
    
    # Extract the complete function
    function_code <- content[doc_start:func_end]
    
    return(paste(function_code, collapse = "\n"))
    
  }, error = function(e) {
    return(paste("Error extracting function code:", e$message))
  })
}

#' Find Function End Line
#'
#' Attempts to find where a function definition ends by tracking braces.
#'
#' @param content Character vector. File lines
#' @param start_line Integer. Line where function starts
#' @return Integer. Line where function ends, or NULL if not found
find_function_end <- function(content, start_line) {
  
  if (start_line > length(content)) return(NULL)
  
  # Look for the opening brace
  brace_count <- 0
  paren_count <- 0
  in_function_def <- FALSE
  
  for (i in start_line:length(content)) {
    line <- content[i]
    
    # Count parentheses (for function arguments)
    paren_matches <- stringr::str_count(line, "\\(") - stringr::str_count(line, "\\)")
    paren_count <- paren_count + paren_matches
    
    # Once we've closed all parentheses, we should see the opening brace
    if (paren_count <= 0 && !in_function_def) {
      in_function_def <- TRUE
    }
    
    if (in_function_def) {
      # Count braces
      brace_matches <- stringr::str_count(line, "\\{") - stringr::str_count(line, "\\}")
      brace_count <- brace_count + brace_matches
      
      # If we've closed all braces, we're done
      if (brace_count <= 0 && stringr::str_detect(line, "\\}")) {
        return(i)
      }
    }
    
    # Safety check - don't go more than 200 lines
    if (i - start_line > 200) {
      return(i)
    }
  }
  
  # If we couldn't find the end, return a reasonable guess
  return(min(length(content), start_line + 100))
}