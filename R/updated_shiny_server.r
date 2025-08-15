#' Create the Shiny Server for Documentation Assistant (Updated)
#'
#' Creates the server logic for the R Documentation Assistant Shiny application
#' with proper integration of enhanced dependency analysis.
#'
#' @return Shiny server function
#' @export
create_doc_server <- function() {
  function(input, output, session) {
    
    # Reactive values to store package analysis data
    values <- shiny::reactiveValues(
      package_data = NULL,
      dependency_data = NULL,
      namespace_analysis = NULL,
      current_function = NULL,
      package_path = "."
    )
    
    # Load package on startup with better error handling
    shiny::observe({
      tryCatch({
        message("Loading package analysis for: ", values$package_path)
        
        # Use enhanced analysis
        values$package_data <- analyze_package(values$package_path, verbose = FALSE)
        
        # Ensure we have enhanced dependency data
        if (!is.null(values$package_data$dependencies)) {
          values$dependency_data <- values$package_data$dependencies
        } else {
          # Fallback: run enhanced dependency analysis separately
          message("Running enhanced dependency analysis...")
          values$dependency_data <- analyze_package_dependencies(
            values$package_path, 
            values$package_data$functions, 
            verbose = FALSE
          )
          # Add it back to package_data
          values$package_data$dependencies <- values$dependency_data
        }
        
        # Namespace analysis
        values$namespace_analysis <- analyze_namespace_usage(values$package_path, verbose = FALSE)
        
        message("Package analysis completed successfully")
        
      }, error = function(e) {
        message("Error loading package: ", e$message)
        shiny::showNotification(paste("Error loading package:", e$message), type = "error")
        
        # Try basic fallback
        tryCatch({
          message("Attempting basic fallback analysis...")
          functions_data <- scan_r_functions(values$package_path, verbose = FALSE)
          docs_data <- parse_function_docs(functions_data)
          param_history <- build_parameter_history(docs_data)
          
          values$package_data <- list(
            package_path = values$package_path,
            functions = functions_data,
            documentation = docs_data,
            parameter_history = param_history,
            stats = list(
              total_functions = nrow(functions_data),
              documented_functions = sum(docs_data$has_docs, na.rm = TRUE),
              total_files = length(unique(functions_data$file_path))
            )
          )
          
          message("Basic analysis completed")
        }, error = function(e2) {
          message("Fallback analysis also failed: ", e2$message)
        })
      })
    })
    
    # Reload package when button is clicked
    shiny::observeEvent(input$reload_package, {
      old_path <- values$package_path
      values$package_path <- input$package_path
      
      message("Reloading package. Old path: ", old_path, ", New path: ", values$package_path)
      
      tryCatch({
        shiny::withProgress(message = "Analyzing package...", {
          shiny::incProgress(0.1, detail = "Checking package structure")
          
          # Validate package path
          if (!dir.exists(values$package_path)) {
            stop("Directory does not exist: ", values$package_path)
          }
          
          shiny::incProgress(0.2, detail = "Scanning functions")
          values$package_data <- analyze_package(values$package_path, verbose = FALSE)
          
          shiny::incProgress(0.4, detail = "Analyzing dependencies") 
          if (!is.null(values$package_data$dependencies)) {
            values$dependency_data <- values$package_data$dependencies
          } else {
            values$dependency_data <- analyze_package_dependencies(
              values$package_path, 
              values$package_data$functions, 
              verbose = FALSE
            )
            values$package_data$dependencies <- values$dependency_data
          }
          
          shiny::incProgress(0.2, detail = "Checking namespace opportunities")
          values$namespace_analysis <- analyze_namespace_usage(values$package_path, verbose = FALSE)
          
          shiny::incProgress(0.2, detail = "Finalizing")
        })
        
        shiny::showNotification("Package reloaded successfully!", type = "success")
        
        # Debug output
        if (!is.null(values$package_data)) {
          message("Functions found: ", nrow(values$package_data$functions))
          message("Files found: ", length(unique(values$package_data$functions$file_path)))
        }
        
      }, error = function(e) {
        message("Error during reload: ", e$message)
        shiny::showNotification(paste("Error loading package:", e$message), type = "error")
        
        # Show diagnostic information
        check_r_files(values$package_path)
      })
    })
    
    # Package summary output with better error handling
    output$package_summary <- shiny::renderText({
      if (is.null(values$package_data)) {
        return("No package loaded. Check the package path and click 'Reload Package'.")
      }
      
      tryCatch({
        stats <- values$package_data$stats
        missing_count <- if (!is.null(values$dependency_data)) length(values$dependency_data$missing) else 0
        
        paste(
          "Functions found:", stats$total_functions, "\n",
          "Documented:", stats$documented_functions, "\n", 
          "Undocumented:", stats$total_functions - stats$documented_functions, "\n",
          "Files:", stats$total_files, "\n",
          "Missing packages:", missing_count
        )
      }, error = function(e) {
        paste("Error generating summary:", e$message)
      })
    })
    
    # Functions table with enhanced error handling
    output$functions_table <- DT::renderDataTable({
      if (is.null(values$package_data)) {
        return(data.frame(Message = "No package data available"))
      }
      
      tryCatch({
        func_data <- values$package_data$functions
        docs_data <- values$package_data$documentation
        
        if (nrow(func_data) == 0) {
          return(data.frame(Message = "No functions found in package"))
        }
        
        # Debug output
        message("Creating functions table with ", nrow(func_data), " functions")
        
        display_data <- data.frame(
          File = func_data$file_name,
          Function = func_data$function_name,
          Arguments = sapply(docs_data$parsed_args, function(x) {
            if (length(x) == 0) return("()")
            args_preview <- names(x)
            if (length(args_preview) > 3) {
              args_preview <- c(args_preview[1:3], "...")
            }
            paste(args_preview, collapse = ", ")
          }),
          Documented = ifelse(docs_data$has_docs, "\u2713", "\u2717"),
          stringsAsFactors = FALSE
        )
        
        message("Display data created with ", nrow(display_data), " rows")
        
        DT::datatable(display_data, 
                      selection = "single",
                      options = list(
                        pageLength = 15, 
                        scrollX = TRUE,
                        search = list(regex = FALSE, caseInsensitive = TRUE)
                      ))
      }, error = function(e) {
        message("Error creating functions table: ", e$message)
        data.frame(Error = paste("Error creating table:", e$message))
      })
    })
    
    # Handle function selection with better error handling
    shiny::observeEvent(input$functions_table_rows_selected, {
      if (is.null(values$package_data) || length(input$functions_table_rows_selected) == 0) {
        return()
      }
      
      tryCatch({
        selected_row <- input$functions_table_rows_selected
        func_data <- values$package_data$functions
        docs_data <- values$package_data$documentation
        
        if (selected_row > nrow(func_data)) {
          message("Selected row ", selected_row, " is out of bounds (max: ", nrow(func_data), ")")
          return()
        }
        
        values$current_function <- list(
          name = func_data$function_name[selected_row],
          file_path = func_data$file_path[selected_row], 
          args = docs_data$parsed_args[[selected_row]],
          existing_docs = docs_data$existing_docs[selected_row],
          has_docs = docs_data$has_docs[selected_row]
        )
        
        message("Selected function: ", values$current_function$name)
        
        # Update function code display
        func_signature <- paste0(
          values$current_function$name, "(",
          if (length(values$current_function$args) > 0) {
            paste(names(values$current_function$args), collapse = ", ")
          } else {
            ""
          },
          ")"
        )
        
        shinyAce::updateAceEditor(session, "function_code", value = func_signature)
        
        # Update documentation editor
        if (values$current_function$has_docs && nchar(values$current_function$existing_docs) > 0) {
          shinyAce::updateAceEditor(session, "docs_editor", value = values$current_function$existing_docs)
        } else {
          # Generate template
          template <- generate_roxygen_template(
            function_name = values$current_function$name,
            args = values$current_function$args,
            param_suggestions = values$package_data$parameter_history
          )
          shinyAce::updateAceEditor(session, "docs_editor", value = template)
        }
        
      }, error = function(e) {
        message("Error handling function selection: ", e$message)
        shiny::showNotification(paste("Error selecting function:", e$message), type = "error")
      })
    })
    
    # Auto-generate template
    shiny::observeEvent(input$auto_template, {
      if (is.null(values$current_function)) {
        shiny::showNotification("No function selected", type = "warning")
        return()
      }
      
      tryCatch({
        template <- generate_roxygen_template(
          function_name = values$current_function$name,
          args = values$current_function$args,
          param_suggestions = values$package_data$parameter_history,
          template_type = "exported"
        )
        
        shinyAce::updateAceEditor(session, "docs_editor", value = template)
        shiny::showNotification("Template generated successfully!", type = "success")
        
      }, error = function(e) {
        shiny::showNotification(paste("Error generating template:", e$message), type = "error")
      })
    })
    
    # Save documentation
    shiny::observeEvent(input$save_docs, {
      if (is.null(values$current_function)) {
        shiny::showNotification("No function selected", type = "warning")
        return()
      }
      
      tryCatch({
        result <- save_function_docs(
          file_path = values$current_function$file_path,
          function_name = values$current_function$name,
          documentation = input$docs_editor,
          backup = TRUE
        )
        
        if (result$success) {
          shiny::showNotification("Documentation saved successfully!", type = "success")
          
          # Reload package data to reflect changes
          values$package_data <- analyze_package(values$package_path, verbose = FALSE)
        }
        
      }, error = function(e) {
        shiny::showNotification(paste("Error saving:", e$message), type = "error")
      })
    })
    
    # Generate bulk documentation
    shiny::observeEvent(input$generate_bulk_docs, {
      if (is.null(values$package_data)) {
        shiny::showNotification("No package data available", type = "warning")
        return()
      }
      
      tryCatch({
        shiny::withProgress(message = "Generating documentation...", {
          result <- generate_bulk_documentation(
            package_data = values$package_data,
            save_to_files = input$save_bulk_docs,
            backup = TRUE
          )
          
          generated_count <- sum(sapply(result, function(x) x$generated))
          saved_count <- sum(sapply(result, function(x) isTRUE(x$saved)))
          
          if (input$save_bulk_docs) {
            msg <- paste("Generated and saved", saved_count, "documentation templates")
          } else {
            msg <- paste("Generated", generated_count, "documentation templates (not saved)")
          }
          
          shiny::showNotification(msg, type = "success")
          
          # Reload if saved
          if (input$save_bulk_docs && saved_count > 0) {
            values$package_data <- analyze_package(values$package_path, verbose = FALSE)
          }
        })
      }, error = function(e) {
        shiny::showNotification(paste("Error generating docs:", e$message), type = "error")
      })
    })
    
    # Parameter suggestions
    output$param_suggestions <- shiny::renderText({
      if (is.null(values$current_function)) {
        return("Select a function to see parameter suggestions")
      }
      
      tryCatch({
        if (length(values$current_function$args) == 0) {
          return("Function has no parameters")
        }
        
        suggestions <- ""
        for (arg_name in names(values$current_function$args)) {
          param_suggestions <- suggest_parameters(values$package_data, arg_name)
          
          if (length(param_suggestions) > 0) {
            suggestions <- paste0(suggestions, 
                                 arg_name, " suggestions:\n",
                                 paste("  -", param_suggestions, collapse = "\n"),
                                 "\n\n")
          }
        }
        
        if (suggestions == "") {
          return("No parameter suggestions available for this function")
        }
        
        return(suggestions)
        
      }, error = function(e) {
        return(paste("Error getting suggestions:", e$message))
      })
    })
    
    # Dependencies summary with enhanced formatting
    output$dependencies_summary <- shiny::renderText({
      if (is.null(values$dependency_data)) {
        return("No dependency data available. Try reloading the package.")
      }
      
      tryCatch({
        report <- generate_dependency_report(values$dependency_data)
        return(report)
      }, error = function(e) {
        return(paste("Error generating dependency report:", e$message))
      })
    })
    
    # Missing packages
    output$missing_packages <- shiny::renderText({
      if (is.null(values$dependency_data)) {
        return("No dependency data available")
      }
      
      tryCatch({
        missing <- values$dependency_data$missing
        
        if (length(missing) == 0) {
          return("\u2705 All detected packages are installed!")
        }
        
        result <- paste0(
          "The following packages are used in your code but not installed:\n\n",
          paste(paste0("\u2022 ", missing), collapse = "\n"),
          "\n\nClick 'Install Missing Packages' to install them automatically."
        )
        
        return(result)
        
      }, error = function(e) {
        return(paste("Error checking missing packages:", e$message))
      })
    })
    
    # Install missing packages
    shiny::observeEvent(input$install_missing, {
      if (is.null(values$dependency_data)) {
        shiny::showNotification("No dependency data available", type = "warning")
        return()
      }
      
      missing <- values$dependency_data$missing
      
      if (length(missing) == 0) {
        shiny::showNotification("No missing packages to install!", type = "message")
        return()
      }
      
      shiny::showModal(shiny::modalDialog(
        title = "Install Missing Packages",
        paste("This will install the following packages:", paste(missing, collapse = ", ")),
        shiny::br(), shiny::br(),
        "Do you want to continue?",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_install", "Install", class = "btn-primary")
        )
      ))
    })
    
    shiny::observeEvent(input$confirm_install, {
      shiny::removeModal()
      
      missing <- values$dependency_data$missing
      
      shiny::withProgress(message = "Installing packages...", value = 0, {
        result <- install_missing_packages(missing, verbose = FALSE)
        
        if (length(result$success) > 0) {
          shiny::showNotification(
            paste("Successfully installed:", paste(result$success, collapse = ", ")), 
            type = "success"
          )
        }
        
        if (length(result$failed) > 0) {
          shiny::showNotification(
            paste("Failed to install:", paste(result$failed, collapse = ", ")), 
            type = "error"
          )
        }
        
        # Reload dependency analysis
        values$package_data <- analyze_package(values$package_path, verbose = FALSE)
        if (!is.null(values$package_data$dependencies)) {
          values$dependency_data <- values$package_data$dependencies
        }
      })
    })
    
    # Namespace analysis outputs
    output$namespace_summary <- shiny::renderText({
      if (is.null(values$namespace_analysis)) {
        return("No namespace analysis available")
      }
      
      return(values$namespace_analysis$summary)
    })
    
    output$namespace_report <- shiny::renderText({
      if (is.null(values$namespace_analysis)) {
        return("No namespace analysis available")
      }
      
      if (values$namespace_analysis$stats$total_opportunities == 0) {
        return("✅ No namespace conversion opportunities found!")
      }
      
      report <- generate_namespace_report(values$namespace_analysis)
      return(report)
    })
    
    # Apply namespace conversion
    shiny::observeEvent(input$apply_namespace, {
      if (is.null(values$namespace_analysis)) {
        shiny::showNotification("No namespace analysis available", type = "warning")
        return()
      }
      
      if (values$namespace_analysis$stats$total_opportunities == 0) {
        shiny::showNotification("No namespace opportunities found", type = "message")
        return()
      }
      
      shiny::showModal(shiny::modalDialog(
        title = "Apply Namespace Conversion",
        paste("This will convert", values$namespace_analysis$stats$total_opportunities, 
              "function calls to use explicit namespace notation."),
        shiny::br(), shiny::br(),
        "Backup files will be created automatically.",
        shiny::br(), shiny::br(),
        "Do you want to continue?",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("preview_namespace", "Preview First", class = "btn-info"),
          shiny::actionButton("confirm_namespace", "Apply Now", class = "btn-primary")
        )
      ))
    })
    
    shiny::observeEvent(input$preview_namespace, {
      shiny::removeModal()
      
      tryCatch({
        shiny::withProgress(message = "Previewing changes...", {
          result <- apply_namespace_conversion(
            package_path = values$package_path,
            preview_only = TRUE,
            verbose = FALSE
          )
          
          # Show preview results in a modal
          shiny::showModal(shiny::modalDialog(
            title = "Namespace Conversion Preview",
            size = "l",
            shiny::verbatimTextOutput("namespace_preview"),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton("confirm_namespace_after_preview", "Apply Changes", class = "btn-primary")
            )
          ))
          
          # Set preview content
          output$namespace_preview <- shiny::renderText({
            paste("Files to be processed:", result$total_changes, "changes in", 
                  length(result$files_processed), "files")
          })
        })
      }, error = function(e) {
        shiny::showNotification(paste("Preview error:", e$message), type = "error")
      })
    })
    
    shiny::observeEvent(input$confirm_namespace, {
      shiny::removeModal()
      apply_namespace_conversion_action()
    })
    
    shiny::observeEvent(input$confirm_namespace_after_preview, {
      shiny::removeModal()
      apply_namespace_conversion_action()
    })
    
    # Helper function for namespace conversion
    apply_namespace_conversion_action <- function() {
      tryCatch({
        shiny::withProgress(message = "Applying namespace conversion...", {
          result <- apply_namespace_conversion(
            package_path = values$package_path,
            backup = TRUE,
            verbose = FALSE
          )
          
          shiny::showNotification(
            paste("Namespace conversion complete!", result$total_changes, "changes applied"), 
            type = "success"
          )
          
          # Reload namespace analysis
          values$namespace_analysis <- analyze_namespace_usage(values$package_path, verbose = FALSE)
        })
      }, error = function(e) {
        shiny::showNotification(paste("Conversion error:", e$message), type = "error")
      })
    }
    
#' Check R Files Diagnostic (Updated)
#'
#' Diagnostic function to check what R files are available in a directory
#' and provide information about package structure. Now checks both .R and .r files.
#'
#' @param path Character. Path to check (default: ".")
#'
#' @examples
#' \dontrun{
#' # Check current directory
#' check_r_files()
#' 
#' # Check specific package
#' check_r_files("/path/to/package")
#' }
#'
#' @export
check_r_files <- function(path = ".") {
  cat("=== R FILES DIAGNOSTIC ===\n")
  cat("Checking path:", normalizePath(path, mustWork = FALSE), "\n\n")
  
  # Check different possible locations
  locations <- c(
    "R" = file.path(path, "R"),
    "Current dir" = path,
    "src" = file.path(path, "src"),
    "scripts" = file.path(path, "scripts")
  )
  
  for (name in names(locations)) {
    dir_path <- locations[[name]]
    cat("Checking", name, "directory:", dir_path, "\n")
    
    if (dir.exists(dir_path)) {
      # Updated to check for both .R and .r files
      r_files <- list.files(dir_path, pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)
      cat("  ✓ Directory exists\n")
      cat("  ✓ Found", length(r_files), "R files (.R and .r)\n")
      
      if (length(r_files) > 0) {
        cat("  Files:\n")
        for (f in utils::head(r_files, 10)) {  # Show first 10 files
          cat("    -", basename(f), "\n")
        }
        if (length(r_files) > 10) cat("    ... and", length(r_files) - 10, "more\n")
      }
    } else {
      cat("  ✗ Directory does not exist\n")
    }
    cat("\n")
  }
  
  # Check if this looks like an R package
  has_description <- file.exists(file.path(path, "DESCRIPTION"))
  has_namespace <- file.exists(file.path(path, "NAMESPACE"))
  
  cat("Package indicators:\n")
  cat("  DESCRIPTION file:", if(has_description) "✓" else "✗", "\n")
  cat("  NAMESPACE file:", if(has_namespace) "✓" else "✗", "\n")
  
  if (has_description && has_namespace) {
    cat("  ✓ This looks like an R package\n")
  } else {
    cat("  ⚠ This doesn't look like a standard R package\n")
  }
  
  invisible(NULL)
}
    
    # Global variables fix (unchanged)
    output$global_vars_help <- shiny::renderText({
      "Paste R CMD check output containing 'no visible binding for global variable' errors here:"
    })
    
    shiny::observeEvent(input$fix_global_vars, {
      if (nchar(trimws(input$global_vars_input)) == 0) {
        shiny::showNotification("Please enter R CMD check output", type = "warning")
        return()
      }
      
      tryCatch({
        result <- fix_global_variables(
          check_output = input$global_vars_input,
          preview_only = input$preview_global_vars
        )
        
        if (input$preview_global_vars) {
          shiny::showNotification("Preview generated - check console output", type = "message")
        } else {
          msg <- paste("Fixed", length(result$variables_added), "global variables")
          shiny::showNotification(msg, type = "success")
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Non-ASCII fix (unchanged)
    output$non_ascii_help <- shiny::renderText({
      "Paste R CMD check output containing 'non-ASCII characters' errors here:"
    })
    
    shiny::observeEvent(input$fix_non_ascii, {
      if (nchar(trimws(input$non_ascii_input)) == 0) {
        shiny::showNotification("Please enter R CMD check output", type = "warning")
        return()
      }
      
      tryCatch({
        result <- fix_non_ascii_characters(
          check_output = input$non_ascii_input,
          package_path = values$package_path,
          preview_only = input$preview_non_ascii
        )
        
        if (input$preview_non_ascii) {
          shiny::showNotification("Preview generated - check console output", type = "message")
        } else {
          msg <- paste("Fixed non-ASCII characters in", length(result$files_processed), "files")
          shiny::showNotification(msg, type = "success")
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    })
  }
}