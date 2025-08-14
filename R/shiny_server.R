#' Create the Shiny Server for Documentation Assistant
#'
#' Creates the server logic for the R Documentation Assistant Shiny application.
#'
#' @return Shiny server function
#' @export
create_doc_server <- function() {
  function(input, output, session) {
    # Initialize documentation helper
    values <- shiny::reactiveValues(
      doc_helper = NULL,
      selected_function = NULL,
      current_function_data = NULL
    )
    
    # Load package on startup
    shiny::observe({
      values$doc_helper <- DocumentationHelper$new(".")
    })
    
    # Reload package
    shiny::observeEvent(input$reload_package, {
      tryCatch({
        values$doc_helper <- DocumentationHelper$new(input$package_path)
        shiny::showNotification("Package reloaded successfully!", type = "success")
      }, error = function(e) {
        shiny::showNotification(paste("Error loading package:", e$message), type = "error")
      })
    })
    
    # Package summary
    output$package_summary <- shiny::renderText({
      if (is.null(values$doc_helper)) return("No package loaded")
      
      data <- values$doc_helper$functions_data
      if (is.null(data) || nrow(data) == 0) return("No functions found")
      
      missing_count <- length(values$doc_helper$missing_deps)
      
      paste(
        "Functions found:", nrow(data), "\n",
        "Documented:", sum(data$has_docs), "\n",
        "Undocumented:", sum(!data$has_docs), "\n",
        "Files:", length(unique(data$file_name)), "\n",
        "Missing packages:", missing_count
      )
    })
    
    # Functions table
    output$functions_table <- DT::renderDataTable({
      if (is.null(values$doc_helper)) return(data.frame())
      
      data <- values$doc_helper$functions_data
      if (is.null(data) || nrow(data) == 0) return(data.frame())
      
      display_data <- data.frame(
        File = data$file_name,
        Function = data$function_name,
        Arguments = sapply(data$parsed_args, function(x) paste(names(x), collapse = ", ")),
        Documented = ifelse(data$has_docs, "\u2713", "\u2717"),
        Dependencies = sapply(data$dependencies, function(x) {
          all_deps <- unique(c(x$library_calls, x$require_calls, x$namespace_calls, x$suspected_packages))
          if (length(all_deps) == 0) return("None")
          return(paste(head(all_deps, 3), collapse = ", "))
        }),
        Missing = sapply(data$missing_deps, function(x) {
          if (length(x) == 0) return("None")
          return(paste(head(x, 3), collapse = ", "))
        }),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(display_data, 
                    selection = "single",
                    options = list(pageLength = 15, scrollX = TRUE))
    })
    
    # Handle function selection
    shiny::observeEvent(input$functions_table_rows_selected, {
      if (is.null(values$doc_helper) || length(input$functions_table_rows_selected) == 0) return()
      
      selected_row <- input$functions_table_rows_selected
      values$current_function_data <- values$doc_helper$functions_data[selected_row, ]
      
      # Update function code display
      shinyAce::updateAceEditor(session, "function_code", 
                      value = values$current_function_data$full_function)
      
      # Update documentation editor
      template <- values$doc_helper$generate_template(
        values$current_function_data$function_name,
        values$current_function_data$parsed_args[[1]],
        values$current_function_data$existing_docs
      )
      
      shinyAce::updateAceEditor(session, "docs_editor", value = template)
    })
    
    # Auto-generate template
    shiny::observeEvent(input$auto_template, {
      if (is.null(values$current_function_data)) return()
      
      template <- values$doc_helper$generate_template(
        values$current_function_data$function_name,
        values$current_function_data$parsed_args[[1]],
        existing_docs = ""  # Force new template
      )
      
      shinyAce::updateAceEditor(session, "docs_editor", value = template)
    })
    
    # Save documentation
    shiny::observeEvent(input$save_docs, {
      if (is.null(values$current_function_data)) {
        shiny::showNotification("No function selected", type = "warning")
        return()
      }
      
      tryCatch({
        success <- values$doc_helper$save_documentation(
          values$current_function_data$file_path,
          values$current_function_data$function_name,
          input$docs_editor,
          values$current_function_data$full_function
        )
        
        if (success) {
          shiny::showNotification("Documentation saved successfully!", type = "success")
          
          # Reload to reflect changes
          values$doc_helper$scan_package()
          values$doc_helper$build_param_history()
        }
        
      }, error = function(e) {
        shiny::showNotification(paste("Error saving:", e$message), type = "error")
      })
    })
    
    # Parameter suggestions
    output$param_suggestions <- shiny::renderText({
      if (is.null(values$current_function_data)) return("Select a function to see parameter suggestions")
      
      args <- names(values$current_function_data$parsed_args[[1]])
      suggestions <- ""
      
      for (arg in args) {
        if (arg %in% names(values$doc_helper$param_history)) {
          suggestions <- paste0(suggestions, 
                               arg, " suggestions:\n",
                               paste("  -", values$doc_helper$param_history[[arg]], collapse = "\n"),
                               "\n\n")
        }
      }
      
      if (suggestions == "") {
        return("No parameter suggestions available for this function")
      }
      
      return(suggestions)
    })
    
    # Function dependencies display
    output$function_dependencies <- shiny::renderText({
      if (is.null(values$current_function_data)) return("Select a function to see dependencies")
      
      deps <- values$current_function_data$dependencies[[1]]
      missing <- values$current_function_data$missing_deps[[1]]
      
      if (length(deps) == 0) return("No dependencies detected")
      
      result <- "Detected dependencies:\n"
      
      if (length(deps$library_calls) > 0) {
        result <- paste0(result, "Library calls: ", paste(deps$library_calls, collapse = ", "), "\n")
      }
      
      if (length(deps$namespace_calls) > 0) {
        result <- paste0(result, "Namespace calls: ", paste(deps$namespace_calls, collapse = ", "), "\n")
      }
      
      if (length(deps$suspected_packages) > 0) {
        result <- paste0(result, "Suspected usage: ", paste(deps$suspected_packages, collapse = ", "), "\n")
      }
      
      if (length(missing) > 0) {
        result <- paste0(result, "\n\u26a0\ufe0f MISSING PACKAGES: ", paste(missing, collapse = ", "), "\n")
      }
      
      return(result)
    })
    
    # Dependencies tab outputs
    output$dependencies_summary <- shiny::renderText({
      if (is.null(values$doc_helper)) return("No package loaded")
      
      deps <- values$doc_helper$dependencies
      missing <- values$doc_helper$missing_deps
      
      all_detected <- unique(c(
        deps$library_calls,
        deps$require_calls,
        deps$namespace_calls,
        deps$suspected_packages
      ))
      
      result <- paste(
        "PACKAGE DEPENDENCY ANALYSIS\n",
        "==========================\n",
        "Total unique packages detected:", length(all_detected), "\n",
        "Missing/uninstalled packages:", length(missing), "\n\n"
      )
      
      if (length(missing) > 0) {
        result <- paste0(result, "\u274c MISSING PACKAGES:\n", paste(missing, collapse = ", "), "\n\n")
      }
      
      if ("undeclared_deps" %in% names(deps) && length(deps$undeclared_deps) > 0) {
        result <- paste0(result, "\u26a0\ufe0f UNDECLARED IN DESCRIPTION:\n", paste(deps$undeclared_deps, collapse = ", "), "\n\n")
      }
      
      if ("unused_deps" %in% names(deps) && length(deps$unused_deps) > 0) {
        result <- paste0(result, "\ud83d\udcc4 POSSIBLY UNUSED IN DESCRIPTION:\n", paste(deps$unused_deps, collapse = ", "), "\n\n")
      }
      
      result <- paste0(result, "\u2705 ALL DETECTED PACKAGES:\n", paste(all_detected, collapse = ", "))
      
      return(result)
    })
    
    output$missing_packages <- shiny::renderText({
      if (is.null(values$doc_helper)) return("No package loaded")
      
      missing <- values$doc_helper$missing_deps
      
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
    
    output$description_analysis <- shiny::renderText({
      if (is.null(values$doc_helper)) return("No package loaded")
      
      deps <- values$doc_helper$dependencies
      
      if (!"description_deps" %in% names(deps)) {
        return("No DESCRIPTION file found or analyzed")
      }
      
      desc_deps <- deps$description_deps
      
      if (is.character(desc_deps)) {
        return(desc_deps)  # Error message
      }
      
      result <- "DESCRIPTION File Analysis:\n\n"
      
      if ("imports" %in% names(desc_deps) && length(desc_deps$imports) > 0) {
        result <- paste0(result, "Imports: ", paste(desc_deps$imports, collapse = ", "), "\n\n")
      }
      
      if ("depends" %in% names(desc_deps) && length(desc_deps$depends) > 0) {
        result <- paste0(result, "Depends: ", paste(desc_deps$depends, collapse = ", "), "\n\n")
      }
      
      if ("suggests" %in% names(desc_deps) && length(desc_deps$suggests) > 0) {
        result <- paste0(result, "Suggests: ", paste(desc_deps$suggests, collapse = ", "), "\n\n")
      }
      
      if ("undeclared_deps" %in% names(deps) && length(deps$undeclared_deps) > 0) {
        result <- paste0(result, "\u26a0\ufe0f Used but not declared:\n", paste(deps$undeclared_deps, collapse = ", "), "\n\n")
      }
      
      if ("unused_deps" %in% names(deps) && length(deps$unused_deps) > 0) {
        result <- paste0(result, "\ud83d\udcc4 Declared but not detected in code:\n", paste(deps$unused_deps, collapse = ", "))
      }
      
      return(result)
    })
    
    output$library_calls <- shiny::renderText({
      if (is.null(values$doc_helper)) return("No package loaded")
      
      deps <- values$doc_helper$dependencies
      calls <- unique(c(deps$library_calls, deps$require_calls))
      
      if (length(calls) == 0) return("No library() or require() calls found")
      
      return(paste("Packages loaded via library()/require():\n", paste(calls, collapse = ", ")))
    })
    
    output$namespace_calls <- shiny::renderText({
      if (is.null(values$doc_helper)) return("No package loaded")
      
      calls <- values$doc_helper$dependencies$namespace_calls
      
      if (length(calls) == 0) return("No package::function calls found")
      
      return(paste("Packages used via namespace calls (::):\n", paste(calls, collapse = ", ")))
    })
    
    output$suspected_usage <- shiny::renderText({
      if (is.null(values$doc_helper)) return("No package loaded")
      
      suspected <- values$doc_helper$dependencies$suspected_packages
      
      if (length(suspected) == 0) return("No suspected package usage patterns detected")
      
      return(paste("Packages suspected based on function patterns:\n", paste(suspected, collapse = ", ")))
    })
    
    output$roxygen_imports <- shiny::renderText({
      if (is.null(values$doc_helper)) return("No package loaded")
      
      deps <- values$doc_helper$dependencies
      imports <- unique(c(deps$roxygen_imports, deps$roxygen_importfrom))
      
      if (length(imports) == 0) return("No @import or @importFrom roxygen tags found")
      
      return(paste("Packages in roxygen @import/@importFrom tags:\n", paste(imports, collapse = ", ")))
    })
    
    output$function_deps_table <- DT::renderDataTable({
      if (is.null(values$doc_helper)) return(data.frame())
      
      data <- values$doc_helper$functions_data
      if (is.null(data) || nrow(data) == 0) return(data.frame())
      
      deps_table <- data.frame(
        File = data$file_name,
        Function = data$function_name,
        "Library Calls" = sapply(data$dependencies, function(x) paste(x$library_calls, collapse = ", ")),
        "Namespace Calls" = sapply(data$dependencies, function(x) paste(x$namespace_calls, collapse = ", ")),
        "Suspected" = sapply(data$dependencies, function(x) paste(x$suspected_packages, collapse = ", ")),
        "Missing" = sapply(data$missing_deps, function(x) paste(x, collapse = ", ")),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      # Replace empty strings with "None"
      deps_table[deps_table == ""] <- "None"
      
      DT::datatable(deps_table,
                    options = list(pageLength = 15, scrollX = TRUE),
                    filter = "top")
    })
    
    # Install missing packages
    shiny::observeEvent(input$install_missing, {
      if (is.null(values$doc_helper)) return()
      
      missing <- values$doc_helper$missing_deps
      
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
      
      missing <- values$doc_helper$missing_deps
      
      shiny::withProgress(message = "Installing packages...", value = 0, {
        for (i in seq_along(missing)) {
          pkg <- missing[i]
          shiny::incProgress(1/length(missing), detail = paste("Installing", pkg))
          
          tryCatch({
            utils::install.packages(pkg, dependencies = TRUE, quiet = TRUE)
          }, error = function(e) {
            shiny::showNotification(paste("Failed to install", pkg, ":", e$message), type = "error")
          })
        }
      })
      
      shiny::showNotification("Package installation completed! Reloading package analysis...", type = "success")
      
      # Reload the package analysis
      values$doc_helper$scan_dependencies()
    })
    
    # Detailed statistics
    output$detailed_stats <- shiny::renderText({
      if (is.null(values$doc_helper)) return("No package loaded")
      
      data <- values$doc_helper$functions_data
      if (is.null(data) || nrow(data) == 0) return("No functions found")
      
      # Parameter frequency analysis
      all_params <- unlist(lapply(data$parsed_args, names))
      param_freq <- sort(table(all_params), decreasing = TRUE)
      
      paste(
        "Total functions:", nrow(data), "\n",
        "Documented functions:", sum(data$has_docs), "\n",
        "Undocumented functions:", sum(!data$has_docs), "\n",
        "Unique parameters:", length(unique(all_params)), "\n\n",
        "Most common parameters:\n",
        paste(paste(names(param_freq)[1:min(10, length(param_freq))], 
                    param_freq[1:min(10, length(param_freq))], sep = ": "), 
              collapse = "\n")
      )
    })
    
    # Parameter history display
    output$param_history_display <- shiny::renderText({
      if (is.null(values$doc_helper)) return("No parameter history available")
      
      history <- values$doc_helper$param_history
      if (length(history) == 0) return("No documented parameters found in existing code")
      
      result <- ""
      for (param_name in names(history)) {
        result <- paste0(result, param_name, ":\n")
        for (desc in history[[param_name]]) {
          result <- paste0(result, "  - ", desc, "\n")
        }
        result <- paste0(result, "\n")
      }
      
      return(result)
    })
  }
}