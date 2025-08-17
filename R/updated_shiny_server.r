#' Create Clean Shiny Server for Documentation Assistant
#'
#' Creates the server logic for the R Documentation Assistant Shiny application
#' with fixed syntax and proper integration with current functions.
#'
#' @return Shiny server function
#' @export
create_doc_server <- function() {
  function(input, output, session) {
    
    # Reactive values to store data
    values <- shiny::reactiveValues(
      package_data = NULL,
      dependency_data = NULL,
      namespace_analysis = NULL,
      current_function = NULL,
      package_path = ".",
      last_analysis_time = NULL
    )
    
    # Initialize with smart path detection
    shiny::observe({
      if (is.null(input$package_path) || input$package_path == "") {
        values$package_path <- smart_detect_package_path()
        shiny::updateTextInput(session, "package_path", value = values$package_path)
      } else {
        values$package_path <- input$package_path
      }
    })
    
    # Load package on startup
    shiny::observe({
      if (!is.null(values$package_path) && values$package_path != "") {
        tryCatch({
          # Try to load FULL package data immediately for template generation
          values$package_data <- analyze_package(values$package_path, include_dependencies = FALSE, verbose = FALSE)
        }, error = function(e) {
          # Fallback: basic function scanning only
          tryCatch({
            functions_data <- scan_r_functions(values$package_path, verbose = FALSE)
            if (nrow(functions_data) > 0) {
              values$package_data <- list(
                package_path = values$package_path,
                functions = functions_data,
                stats = list(
                  total_functions = nrow(functions_data),
                  documented_functions = sum(functions_data$has_docs, na.rm = TRUE),
                  total_files = length(unique(functions_data$file_path))
                )
              )
            }
          }, error = function(e2) {
            # Complete failure - silently fail
          })
        })
      }
    })
    
    # Quick Check Action
    shiny::observeEvent(input$quick_check, {
      values$package_path <- input$package_path
      
      shiny::withProgress(message = "Running quick check...", value = 0, {
        tryCatch({
          shiny::incProgress(0.3, detail = "Checking package structure")
          
          # Use analyze_package without dependencies for speed if skip_dependencies is checked
          include_deps <- !input$skip_dependencies
          
          if (include_deps) {
            shiny::incProgress(0.2, detail = "Quick dependency check")
          } else {
            shiny::incProgress(0.4, detail = "Skipping dependencies for speed")
          }
          
          # Capture output
          check_output <- capture.output({
            # Do a quick analysis
            pkg_info <- analyze_package(values$package_path, include_dependencies = include_deps, verbose = FALSE)
            issues <- list()
            
            # Basic issue detection
            undocumented <- pkg_info$stats$total_functions - pkg_info$stats$documented_functions
            if (undocumented > 0) {
              issues$undocumented_functions <- undocumented
            }
            
            if (include_deps && !is.null(pkg_info$dependencies)) {
              if (length(pkg_info$dependencies$missing) > 0) {
                issues$missing_packages <- pkg_info$dependencies$missing
              }
              if (length(pkg_info$dependencies$undeclared) > 0) {
                issues$undeclared_packages <- pkg_info$dependencies$undeclared
              }
            }
            
            # Print summary
            if (length(issues) == 0) {
              cat("✅ No critical issues found! Package looks good.\n")
            } else {
              cat("⚠️  Issues found:\n")
              if ("missing_packages" %in% names(issues)) {
                cat("  • Missing packages:", length(issues$missing_packages), "\n")
              }
              if ("undocumented_functions" %in% names(issues)) {
                cat("  • Undocumented functions:", issues$undocumented_functions, "\n")
              }
              if ("undeclared_packages" %in% names(issues)) {
                cat("  • Undeclared packages:", length(issues$undeclared_packages), "\n")
              }
            }
            
            if (!include_deps) {
              cat("\nNote: Dependency check skipped for faster analysis\n")
            }
          })
          
          shiny::incProgress(0.5, detail = "Finalizing")
          
          # Update package summary with check results
          output$package_summary <- shiny::renderText({
            paste(check_output, collapse = "\n")
          })
          
          duration_msg <- if (input$skip_dependencies) "Ultra-quick check completed!" else "Quick check completed!"
          shiny::showNotification(duration_msg, type = "success", duration = 3)
          
        }, error = function(e) {
          shiny::showNotification(paste("Quick check failed:", e$message), type = "error")
          output$package_summary <- shiny::renderText({
            paste("Error during quick check:", e$message)
          })
        })
      })
    })
    
    # Full Analysis Action
    shiny::observeEvent(input$reload_package, {
      if (is.null(input$package_path) || input$package_path == "") {
        values$package_path <- smart_detect_package_path()
        shiny::updateTextInput(session, "package_path", value = values$package_path)
      } else {
        values$package_path <- input$package_path
      }
      
      shiny::withProgress(message = "Analyzing package...", value = 0, {
        tryCatch({
          shiny::incProgress(0.1, detail = "Checking package structure")
          
          if (!dir.exists(values$package_path)) {
            stop("Directory does not exist: ", values$package_path)
          }
          
          shiny::incProgress(0.2, detail = "Scanning functions")
          
          # Use dependency checking based on settings
          include_deps <- !input$fast_mode && !input$skip_dependencies
          
          if (include_deps) {
            shiny::incProgress(0.1, detail = "Will include dependency analysis")
          } else {
            shiny::incProgress(0.3, detail = "Skipping dependencies for speed")
          }
          
          values$package_data <- analyze_package(values$package_path, include_dependencies = include_deps, verbose = FALSE)
          
          shiny::incProgress(0.4, detail = "Processing dependencies")
          
          if (!is.null(values$package_data$dependencies)) {
            values$dependency_data <- values$package_data$dependencies
          }
          
          shiny::incProgress(0.3, detail = "Finalizing")
          values$last_analysis_time <- Sys.time()
          
          shiny::showNotification("Package analysis completed!", type = "success")
          
        }, error = function(e) {
          shiny::showNotification(paste("Analysis failed:", e$message), type = "error")
          values$package_data <- NULL
          values$dependency_data <- NULL
        })
      })
    })
    
    # Package summary output
    output$package_summary <- shiny::renderText({
      if (is.null(values$package_data)) {
        return("No package loaded. Set package path and click 'Quick Check' or 'Full Analysis'.")
      }
      
      tryCatch({
        stats <- values$package_data$stats
        missing_count <- if (!is.null(values$dependency_data)) length(values$dependency_data$missing) else 0
        
        analysis_type <- if (!is.null(values$last_analysis_time)) {
          paste("Last analysis:", format(values$last_analysis_time, "%H:%M:%S"))
        } else {
          "Analysis type: Unknown"
        }
        
        paste(
          analysis_type, "\n",
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
    
    # Functions table
    output$functions_table <- DT::renderDataTable({
      if (is.null(values$package_data) || is.null(values$package_data$functions)) {
        return(data.frame(Message = "No package data available"))
      }
      
      tryCatch({
        func_data <- values$package_data$functions
        
        if (nrow(func_data) == 0) {
          return(data.frame(Message = "No functions found in package"))
        }
        
        # Create display data
        display_data <- data.frame(
          File = basename(func_data$file_path),
          Function = func_data$function_name,
          Arguments = sapply(seq_len(nrow(func_data)), function(i) {
            if ("documentation" %in% names(values$package_data) && "parsed_args" %in% names(values$package_data$documentation)) {
              args <- values$package_data$documentation$parsed_args[[i]]
              if (length(args) == 0) return("()")
              args_preview <- names(args)
              if (length(args_preview) > 3) {
                args_preview <- c(args_preview[1:3], "...")
              }
              paste(args_preview, collapse = ", ")
            } else {
              # Fallback to args_string
              if (nchar(func_data$args_string[i]) > 30) {
                paste0(substr(func_data$args_string[i], 1, 30), "...")
              } else {
                func_data$args_string[i]
              }
            }
          }),
          Documented = ifelse(func_data$has_docs, "✓", "✗"),
          stringsAsFactors = FALSE
        )
        
        DT::datatable(display_data, 
                      selection = "single",
                      options = list(
                        pageLength = 15, 
                        scrollX = TRUE,
                        search = list(regex = FALSE, caseInsensitive = TRUE)
                      ))
      }, error = function(e) {
        data.frame(Error = paste("Error creating table:", e$message))
      })
    })
    
    # Handle function selection
    shiny::observeEvent(input$functions_table_rows_selected, {
      if (is.null(values$package_data) || length(input$functions_table_rows_selected) == 0) {
        return()
      }
      
      tryCatch({
        selected_row <- input$functions_table_rows_selected
        func_data <- values$package_data$functions
        
        if (selected_row > nrow(func_data)) {
          return()
        }
        
        values$current_function <- list(
          name = func_data$function_name[selected_row],
          file_path = func_data$file_path[selected_row],
          args_string = func_data$args_string[selected_row],
          has_docs = func_data$has_docs[selected_row],
          index = selected_row
        )
        
        # Get parsed args if available
        if ("documentation" %in% names(values$package_data) && 
            "parsed_args" %in% names(values$package_data$documentation)) {
          values$current_function$args <- values$package_data$documentation$parsed_args[[selected_row]]
        } else {
          values$current_function$args <- parse_function_arguments(values$current_function$args_string)
        }
        
        # Extract and display function code
        function_code <- extract_function_code(values$current_function$file_path, values$current_function$name)
        shinyAce::updateAceEditor(session, "code_viewer", value = function_code)
        
        # Get existing docs if available
        existing_docs <- ""
        if (values$current_function$has_docs && 
            "documentation" %in% names(values$package_data) &&
            "existing_docs" %in% names(values$package_data$documentation)) {
          existing_docs <- values$package_data$documentation$existing_docs[selected_row]
          if (!is.na(existing_docs) && nchar(existing_docs) > 0) {
            shinyAce::updateAceEditor(session, "docs_editor", value = existing_docs)
            # Also show in the read-only viewer
            shinyAce::updateAceEditor(session, "existing_docs_viewer", value = existing_docs)
          } else {
            shinyAce::updateAceEditor(session, "existing_docs_viewer", value = "No existing documentation found")
          }
        } else {
          shinyAce::updateAceEditor(session, "existing_docs_viewer", value = "No existing documentation found")
        }
        
      }, error = function(e) {
        shiny::showNotification(paste("Error selecting function:", e$message), type = "error")
      })
    })
    
    # Selected function info
    output$selected_function_info <- shiny::renderText({
      if (is.null(values$current_function)) {
        return("No function selected")
      }
      
      paste(
        "Function:", values$current_function$name, "\n",
        "File:", basename(values$current_function$file_path), "\n",
        "Arguments:", values$current_function$args_string, "\n",
        "Documented:", if (values$current_function$has_docs) "Yes" else "No"
      )
    })
    
    # Generate template for selected function
    shiny::observeEvent(input$generate_template, {
      if (is.null(values$current_function)) {
        shiny::showNotification("No function selected", type = "warning")
        return()
      }
      
      tryCatch({
        # Ensure we have full package data with documentation and parameter history
        if (!"documentation" %in% names(values$package_data) || !"parameter_history" %in% names(values$package_data)) {
          shiny::withProgress(message = "Loading full package data for template generation...", {
            values$package_data <- analyze_package(values$package_path, include_dependencies = FALSE, verbose = FALSE)
          })
        }
        
        # Get parameter history for smart suggestions
        param_history <- if ("parameter_history" %in% names(values$package_data)) {
          values$package_data$parameter_history
        } else {
          list()  # Empty list if not available
        }
        
        # Get function arguments
        func_args <- values$current_function$args
        if (is.null(func_args) || length(func_args) == 0) {
          # Try to parse from args_string if args are empty
          func_args <- parse_function_arguments(values$current_function$args_string)
        }
        
        # Generate template with proper parameters
        template <- generate_roxygen_template(
          function_name = values$current_function$name,
          args = func_args,
          param_suggestions = param_history,
          template_type = "exported",
          add_examples = TRUE
        )
        
        # Update the documentation editor
        shinyAce::updateAceEditor(session, "docs_editor", value = template)
        
        # Show success message with details
        param_count <- length(func_args)
        suggestion_count <- sum(names(func_args) %in% names(param_history))
        
        msg <- paste0("Template generated! ", param_count, " parameters found")
        if (suggestion_count > 0) {
          msg <- paste0(msg, " (", suggestion_count, " with smart suggestions)")
        }
        
        shiny::showNotification(msg, type = "success")
        
      }, error = function(e) {
        # More detailed error message
        error_msg <- paste("Error generating template:", e$message)
        shiny::showNotification(error_msg, type = "error")
        
        # Try to generate a basic template as fallback
        tryCatch({
          basic_template <- paste0(
            "#' ", values$current_function$name, "\n",
            "#'\n",
            "#' [Brief description of what the function does]\n",
            "#'\n"
          )
          
          # Add basic parameters if available
          if (!is.null(values$current_function$args) && length(values$current_function$args) > 0) {
            for (arg_name in names(values$current_function$args)) {
              basic_template <- paste0(basic_template, "#' @param ", arg_name, " [Description]\n")
            }
            basic_template <- paste0(basic_template, "#'\n")
          }
          
          basic_template <- paste0(basic_template, "#' @return [Description of return value]\n")
          basic_template <- paste0(basic_template, "#'\n")
          basic_template <- paste0(basic_template, "#' @export\n")
          
          shinyAce::updateAceEditor(session, "docs_editor", value = basic_template)
          shiny::showNotification("Basic template generated as fallback", type = "message")
          
        }, error = function(e2) {
          # Complete failure
          shiny::showNotification("Template generation failed completely", type = "error")
        })
      })
    })
    
    # Save documentation for selected function (unified save action)
    shiny::observeEvent(input$save_selected_docs, {
      if (is.null(values$current_function)) {
        shiny::showNotification("No function selected", type = "warning")
        return()
      }
      
      if (is.null(input$docs_editor) || nchar(trimws(input$docs_editor)) == 0) {
        shiny::showNotification("No documentation to save", type = "warning")
        return()
      }
      
      tryCatch({
        result <- save_function_docs(
          file_path = values$current_function$file_path,
          function_name = values$current_function$name,
          documentation = input$docs_editor,
          backup = input$create_backup
        )
        
        if (result$success) {
          shiny::showNotification("Documentation saved successfully!", type = "success")
          # Mark as documented
          if (!is.null(values$package_data$functions)) {
            values$package_data$functions$has_docs[values$current_function$index] <- TRUE
          }
          
          # Refresh the existing docs viewer
          shinyAce::updateAceEditor(session, "existing_docs_viewer", value = input$docs_editor)
        }
        
      }, error = function(e) {
        shiny::showNotification(paste("Error saving documentation:", e$message), type = "error")
      })
    })
    
    # Refresh functions table
    shiny::observeEvent(input$refresh_functions, {
      if (!is.null(values$package_data)) {
        # Re-scan functions
        tryCatch({
          functions_data <- scan_r_functions(values$package_path, verbose = FALSE)
          values$package_data$functions <- functions_data
          values$package_data$stats$total_functions <- nrow(functions_data)
          values$package_data$stats$documented_functions <- sum(functions_data$has_docs, na.rm = TRUE)
          values$package_data$stats$total_files <- length(unique(functions_data$file_path))
          shiny::showNotification("Functions table refreshed!", type = "success")
        }, error = function(e) {
          shiny::showNotification(paste("Error refreshing:", e$message), type = "error")
        })
      }
    })
    
    # Placeholder outputs for other tabs (removed unused save_docs observer)
    output$dependencies_summary <- shiny::renderText("Click 'Check Dependencies' to analyze dependencies")
    output$missing_packages <- shiny::renderText("No dependency data available")
    output$package_usage_report <- shiny::renderText("Select a package to analyze usage")
    output$namespace_report <- shiny::renderText("Click 'Check Opportunities' to analyze namespace conversion")
    output$bulk_docs_result <- shiny::renderText("Configure options and click 'Generate All Missing Docs'")
    output$examples_result <- shiny::renderText("Enter template and click 'Add Examples'")
    output$doc_analysis_result <- shiny::renderText("Click analysis buttons for results")
    output$global_vars_result <- shiny::renderText("Enter R CMD check output and click 'Fix Global Variables'")
    output$examples_check_result <- shiny::renderText("Click 'Check Missing Examples' for analysis")
    output$analysis_results <- shiny::renderText("Click 'Run Analysis' to see results")
    output$detailed_stats <- shiny::renderText({
      if (is.null(values$package_data)) {
        return("No package data available")
      }
      
      stats <- values$package_data$stats
      
      paste(
        "Package Analysis Stats\n\n",
        "Functions:", stats$total_functions, "\n",
        "Documented:", stats$documented_functions, "\n",
        "Undocumented:", stats$total_functions - stats$documented_functions, "\n",
        "Files:", stats$total_files, "\n",
        "Package path:", values$package_path
      )
    })
    
    output$current_package_info <- shiny::renderText({
      paste(
        "Path:", values$package_path, "\n",
        "Fast mode:", input$fast_mode, "\n",
        "Data loaded:", !is.null(values$package_data)
      )
    })
    
  }
}