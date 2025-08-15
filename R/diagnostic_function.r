#' Diagnostic Function to Test Examples Detection
#'
#' Run this function to diagnose exactly what's happening with your examples detection
#'
#' @export
diagnose_examples_issue <- function() {
  
  cat("=== DIAGNOSING EXAMPLES DETECTION ISSUE ===\n\n")
  
  # Test case: analyze_package function
  test_docs <- "#' @examples
#' \\dontrun{
#' # Analyze current package
#' pkg_info <- analyze_package()
#' 
#' # Analyze specific package
#' pkg_info <- analyze_package(\"/path/to/package\")
#' 
#' # Quick analysis without dependencies
#' pkg_info <- analyze_package(include_dependencies = FALSE)
#' }
#'
#' @export"
  
  cat("1. Testing with analyze_package example:\n")
  cat("   Documentation:\n")
  cat(test_docs)
  cat("\n\n")
  
  # Step 1: Check if @examples is detected
  has_examples <- stringr::str_detect(test_docs, "#'\\s*@examples")
  cat("2. Has @examples section:", has_examples, "\n")
  
  if (!has_examples) {
    cat("   ERROR: @examples not detected!\n")
    return()
  }
  
  # Step 2: Extract examples section
  cat("3. Extracting examples content...\n")
  tryCatch({
    examples_content <- extract_examples_section(test_docs)
    cat("   Extraction successful. Length:", nchar(examples_content), "\n")
    cat("   Content preview (first 100 chars):\n")
    cat("   '", substr(examples_content, 1, 100), "...'\n\n")
    
    # Step 3: Check if empty
    cat("4. Testing if examples are considered empty...\n")
    is_empty <- is_examples_section_empty(examples_content)
    cat("   Result: is_empty =", is_empty, "\n\n")
    
    if (is_empty) {
      cat("   ❌ PROBLEM: Function incorrectly thinks examples are empty!\n")
      
      # Further diagnosis
      cat("5. Detailed diagnosis of why it's considered empty:\n")
      
      # Check for \dontrun
      has_dontrun <- stringr::str_detect(examples_content, "\\\\dontrun")
      cat("   Has \\dontrun blocks:", has_dontrun, "\n")
      
      if (has_dontrun) {
        cat("   Extracting \\dontrun content...\n")
        dontrun_content <- extract_dontrun_content(examples_content)
        cat("   Dontrun content length:", nchar(dontrun_content), "\n")
        cat("   Dontrun content:\n")
        cat("   '", dontrun_content, "'\n")
        
        # Test the content
        is_dontrun_empty <- is_content_substantially_empty(dontrun_content)
        cat("   Dontrun content considered empty:", is_dontrun_empty, "\n")
        
        if (is_dontrun_empty) {
          cat("   ERROR: \\dontrun content incorrectly flagged as empty!\n")
        }
      } else {
        cat("   Testing raw content...\n")
        is_raw_empty <- is_content_substantially_empty(examples_content)
        cat("   Raw content considered empty:", is_raw_empty, "\n")
      }
      
    } else {
      cat("   ✅ SUCCESS: Examples correctly identified as non-empty!\n")
    }
    
  }, error = function(e) {
    cat("   ERROR in function calls:", e$message, "\n")
    cat("   This suggests you might not have the fixed functions loaded.\n")
  })
  
  cat("\n6. Checking which functions are currently loaded:\n")
  
  # Check if the functions exist and what they look like
  functions_to_check <- c("extract_examples_section", "is_examples_section_empty", 
                         "extract_dontrun_content", "is_content_substantially_empty")
  
  for (func_name in functions_to_check) {
    if (exists(func_name)) {
      cat("   ✅", func_name, "exists\n")
      
      # Try to get some info about the function
      func_obj <- get(func_name)
      if (is.function(func_obj)) {
        func_body <- deparse(body(func_obj))
        if (length(func_body) > 5) {
          cat("      Function has", length(func_body), "lines\n")
        } else {
          cat("      Function body: ", paste(func_body[1:min(2, length(func_body))], collapse = " "), "\n")
        }
      }
    } else {
      cat("   ❌", func_name, "does NOT exist\n")
    }
  }
  
  cat("\n7. RECOMMENDATIONS:\n")
  cat("   - If any functions are missing, you need to load the fixed versions\n")
  cat("   - If functions exist but examples are still flagged as empty, you have old buggy versions\n")
  cat("   - Replace the functions in R/documentation_functions.r with the fixed versions\n")
  cat("   - Run devtools::load_all() to reload the package\n")
  
  invisible(NULL)
}

#' Test the Fixed Functions Work Correctly
#'
#' @export
test_fixed_functions <- function() {
  
  cat("=== TESTING FIXED FUNCTIONS ===\n\n")
  
  # Test case 1: Real examples (should NOT be empty)
  test1 <- "data(ZambiaAdm1)\nclass(ZambiaAdm1)\ninfo <- adminInfo(poly.adm=ZambiaAdm1, admin = 1, by.adm=\"NAME_1\")"
  
  cat("Test 1: Real examples\n")
  cat("Content:", test1, "\n")
  result1 <- is_content_substantially_empty(test1)
  cat("Is empty:", result1, "(should be FALSE)\n")
  cat("Result:", if (!result1) "✅ PASS" else "❌ FAIL", "\n\n")
  
  # Test case 2: dontrun with real code (should NOT be empty)
  test2 <- "\\dontrun{\n# Analyze current package\npkg_info <- analyze_package()\n}"
  
  cat("Test 2: Dontrun with real code\n")
  cat("Content:", test2, "\n")
  
  # Extract dontrun content
  dontrun_extracted <- extract_dontrun_content(test2)
  cat("Extracted:", dontrun_extracted, "\n")
  result2 <- is_content_substantially_empty(dontrun_extracted)
  cat("Is empty:", result2, "(should be FALSE)\n")
  cat("Result:", if (!result2) "✅ PASS" else "❌ FAIL", "\n\n")
  
  # Test case 3: Empty placeholder (should BE empty)
  test3 <- "\\dontrun{\n# TODO: Add examples\n}"
  
  cat("Test 3: Empty placeholder\n")
  cat("Content:", test3, "\n")
  dontrun_extracted3 <- extract_dontrun_content(test3)
  cat("Extracted:", dontrun_extracted3, "\n")
  result3 <- is_content_substantially_empty(dontrun_extracted3)
  cat("Is empty:", result3, "(should be TRUE)\n")
  cat("Result:", if (result3) "✅ PASS" else "❌ FAIL", "\n\n")
  
  cat("=== SUMMARY ===\n")
  all_pass <- !result1 && !result2 && result3
  cat("All tests passed:", all_pass, "\n")
  
  if (!all_pass) {
    cat("❌ Some tests failed - you need to update your functions\n")
  } else {
    cat("✅ All tests passed - functions are working correctly\n")
  }
  
  invisible(all_pass)
}