#' Create the Shiny Server for Documentation Assistant (Functional Version)
#'
#' Creates the server logic for the R Documentation Assistant Shiny application
#' using the new functional architecture instead of R6 classes.
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
    
    # Load package on startup
    shiny::observe({
      tryCatch({
        values$package_data <- analyze_package(values$package_path, verbose = FALSE)
        if (!is.null(values$package_data$dependencies)) {
          values$dependency_data <- values$package_data$dependencies
        }
        values$namespace_analysis <- analyze_namespace_usage(values$package_path, verbose = FALSE)
      }, error = function(e) {
        shiny::showNotification(paste("Error loading package:", e$message), type = "error")
      })
    })
    
    # Reload package when button is clicked
    shiny::observeEvent(input$reload_package, {
      values$package_path <- input$package_path
      
      tryCatch({
        shiny::withProgress(message = "Analyzing package...", {
          shiny::incProgress(0.2, detail = "Scanning functions")
          values$package_data <- analyze_package(values$package_path, verbose = FALSE)
          
          shiny::incProgress(0.4, detail = "Analyzing dependencies")
          if (!is.null(values$package_data$dependencies)) {
            values$dependency_data <- values$package_data$dependencies
          }
          
          shiny::incProgress(0.2, detail = "Checking namespace opportunities")
          values$namespace_analysis <- analyze_namespace_usage(values$package_path, verbose = FALSE)
        })
        
        shiny::showNotification("Package reloaded successfully!", type = "success")
      }, error = function(e) {
        shiny::showNotification(paste("Error loading package:", e$message), type = "error")
      })
    })
    
    # Package summary output
    output$package_summary <- shiny::renderText({
      if (is.null(values$package_data)) return("No package loaded")
      
      stats <- values$package_data$stats
      missing_count <- if (!is.null(values$dependency_data)) length(values$dependency_data$missing) else 0
      
      paste(
        "Functions found:", stats$total_functions, "\n",
        "Documented:", stats$documented_functions, "\n", 
        "Undocumented:", stats$total_functions - stats$documented_functions, "\n",
        "Files:", stats$total_files, "\n",
        "Missing packages:", missing_count
      )
    })
    
    # Functions table
    output$functions_table <- DT::renderDataTable({
      if (is.null(values$package_data)) return(data.frame())
      
      func_data <- values$package_data$functions
      docs_data <- values$package_data$documentation
      
      if (nrow(func_data) == 0) return(data.frame())
      
      display_data <- data.frame(
        File = func_data$file_name,
        Function = func_data$function_name,
        Arguments = sapply(docs_data$parsed_args, function(x) {
          if (length(x) == 0) return("()")
          paste(names(x), collapse = ", ")
        }),
        Documented = ifelse(docs_data$has_docs, "\u2713", "\u2717"),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(display_data, 
                    selection = "single",
                    options = list(pageLength = 15, scrollX = TRUE))
    })
    
    # Handle function selection
    shiny::observeEvent(input$functions_table_rows_selected, {
      if (is.null(values$package_data) || length(input$functions_table_rows_selected) == 0) return()
      
      selected_row <- input$functions_table_rows_selected
      func_data <- values$package_data$functions
      docs_data <- values$package_data$documentation
      
      values$current_function <- list(
        name = func_data$function_name[selected_row],
        file_path = func_data$file_path[selected_row], 
        args = docs_data$parsed_args[[selected_row]],
        existing_docs = docs_data$existing_docs[selected_row],
        has_docs = docs_data$has_docs[selected_row]
      )
      
      # Update function code display (simplified - just show function signature)
      func_signature <- paste0(values$current_function$name, "(", 
                              paste(names(values$current_function$args), collapse = ", "), ")")
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
    })
    
    # Auto-generate template
    shiny::observeEvent(input$auto_template, {
      if (is.null(values$current_function)) return()
      
      template <- generate_roxygen_template(
        function_name = values$current_function$name,
        args = values$current_function$args,
        param_suggestions = values$package_data$parameter_history,
        template_type = "exported"
      )
      
      shinyAce::updateAceEditor(session, "docs_editor", value = template)
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
    })
    
    # Dependencies summary
    output$dependencies_summary <- shiny::renderText({
      if (is.null(values$dependency_data)) return("No dependency data available")
      
      report <- generate_dependency_report(values$dependency_data)
      return(report)
    })
    
    # Missing packages
    output$missing_packages <- shiny::renderText({
      if (is.null(values$dependency_data)) return("No dependency data available")
      
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
    })
    
    # Install missing packages
    shiny::observeEvent(input$install_missing, {
      if (is.null(values$dependency_data)) return()
      
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
      if (is.null(values$namespace_analysis)) return("No namespace analysis available")
      
      return(values$namespace_analysis$summary)
    })
    
    output$namespace_report <- shiny::renderText({
      if (is.null(values$namespace_analysis)) return("No namespace analysis available")
      
      if (values$namespace_analysis$stats$total_opportunities == 0) {
        return("✅ No namespace conversion opportunities found!")
      }
      
      report <- generate_namespace_report(values$namespace_analysis)
      return(report)
    })
    
    # Apply namespace conversion
    shiny::observeEvent(input$apply_namespace, {
      if (is.null(values$namespace_analysis)) return()
      
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
    
    # R CMD Check fixes (these remain the same as they're already functional)
    
    # Global variables fix
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
    
    # Non-ASCII fix
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