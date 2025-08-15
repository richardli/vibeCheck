# vibeCheck 

> **Interactive R Package Development Assistant**

_Disclaimer:_ **This package is completely created with vibe coding using Claude**, thus the name of the package. I have not checked all function details, except for small things that come up in my experiment. It likely contains bugs and unhandled edge cases, but it does what it is designed to do for my specific use case. I may vibe-update this package when I need to use it to update my R packages in the future. If you want to read about how I vibe coded this package, see [these running thoughts](vibe-coding-thoughts.md).

**vibeCheck** is a comprehensive toolkit for R package development that combines interactive analysis, automated documentation generation, dependency management, and common R CMD check fixes. Built with a clean functional architecture, it provides both a beautiful Shiny interface and powerful command-line tools.

## ✨ Features

✅ **Smart Package Analysis** - Comprehensive scanning of R functions, documentation, and dependencies  
✅ **Interactive Documentation Generation** - Roxygen2 templates with intelligent parameter suggestions  
✅ **Dependency Management** - Detect missing packages, analyze DESCRIPTION files, install dependencies  
✅ **Namespace Conversion** - Automatically convert `func()` to `pkg::func()` calls  
✅ **R CMD Check Fixes** - Automated solutions for global variables, non-ASCII characters, and empty examples  
✅ **Beautiful Shiny Interface** - Interactive dashboard for all features  
✅ **Command-Line Tools** - Functional API for scripting and automation  
✅ **Smart Suggestions** - Learn from existing documentation patterns  

## 🚀 Installation

You can install the development version of vibeCheck from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("richardli/vibeCheck")
```

## 🎯 Quick Start

### Interactive Analysis (Recommended)
```r
library(vibeCheck)

# Complete interactive package analysis with fixes
vibecheck()

# Quick check without interactive fixes
quick_package_check()

# Launch Shiny dashboard
launch_doc_app()
```

### Command-Line Usage
```r
# Analyze package structure
pkg_info <- analyze_package()
print(pkg_info$stats)

# Generate documentation for all undocumented functions
docs <- generate_bulk_documentation(pkg_info, save_to_files = TRUE)

# Analyze your package dependencies  
deps <- analyze_package_dependencies("path/to/package/root")

# Example of dependency analysis: extract all dplyr functions you're using
dplyr_funcs <- extract_package_functions("dplyr", dependency_data = deps)
print(dplyr_funcs$functions_found)

# Generate a detailed report
report <- generate_package_usage_report("dplyr")
cat(report)
```

## 📊 Core Functions

### Package Analysis
```r
# Complete package analysis
pkg_info <- analyze_package("path/to/package")
#> Analyzing package at: path/to/package
#> Scanning R functions...
#> Found 15 R files
#> Parsing existing documentation...
#> Building parameter suggestions...
#> Analyzing dependencies...
#> Package analysis complete!
#>   Functions found: 42
#>   Documented: 28
#>   Files: 15
#>   Missing packages: 3

# Quick diagnostic
functions_data <- scan_r_functions(".")
docs_data <- parse_function_docs(functions_data)
param_history <- build_parameter_history(docs_data)
```

### Documentation Generation
```r
# Generate smart documentation template
template <- generate_roxygen_template(
  function_name = "process_data",
  args = list(data = NULL, method = '"auto"', verbose = "TRUE"),
  param_suggestions = pkg_info$parameter_history
)

# Save documentation to file
result <- save_function_docs(
  file_path = "R/my_functions.R",
  function_name = "process_data", 
  documentation = template,
  backup = TRUE
)

# Bulk documentation generation
bulk_result <- generate_bulk_documentation(
  package_data = pkg_info,
  save_to_files = TRUE,
  template_type = "exported"
)
```

### Dependency Management
```r
# Comprehensive dependency analysis
deps <- analyze_package_dependencies(".")
#> Dependency analysis:
#>   Detected packages: 12
#>   Declared packages: 10
#>   Missing packages: 2
#>   Undeclared packages: 4

# Generate detailed report
report <- generate_dependency_report(deps)
cat(report)

# Install missing packages
install_result <- install_missing_packages(deps$missing)
#> Installing 2 packages...
#> Installing dplyr...
#>   ✓ dplyr installed successfully
#> Installing ggplot2...
#>   ✓ ggplot2 installed successfully

# Parse DESCRIPTION file
desc_deps <- parse_description_dependencies(".")
```

## 🔗 Namespace Management

### Analyze Namespace Opportunities
```r
# Find functions that could use explicit namespacing
analysis <- analyze_namespace_usage(".")
cat(analysis$summary)
#> NAMESPACE CONVERSION OPPORTUNITIES
#> =====================================
#> Dependencies in DESCRIPTION: 8 packages
#> Available function mappings: 156 functions
#> Files with opportunities: 5
#> Total conversion opportunities: 23
#> Packages involved: dplyr, ggplot2, stringr

# Generate detailed report
report <- generate_namespace_report(analysis)
cat(report)
```

### Apply Namespace Conversion
```r
# Preview changes first
result <- apply_namespace_conversion(
  preview_only = TRUE,
  verbose = TRUE
)

# Apply conversions with backup
result <- apply_namespace_conversion(
  backup = TRUE,
  exclude_functions = c("filter", "select")  # Keep these unnamespaced
)
#> Processing 5 files...
#>   analysis.R: 8 changes
#>   plotting.R: 12 changes
#>   utils.R: 3 changes
#> Namespace conversion complete!
#>   Files processed: 5
#>   Total changes: 23
```

## 🔧 R CMD Check Fixes

### Global Variable Fixes
```r
# Copy R CMD check output with global variable errors
check_output <- "
checking R code for possible problems ... NOTE
  process_data: no visible binding for global variable 'species'
  analyze_results: no visible binding for global variable 'measurement'
  plot_summary: no visible binding for global variable 'category'
"

# Preview what would be fixed
fix_global_variables(check_output, preview_only = TRUE)
#> Found 3 global variable binding issues:
#>   - species
#>   - measurement  
#>   - category
#> 
#> === PREVIEW MODE ===
#> Would write to: R/check_global.R
#> 
#> New file content:
#> # Global Variables Declaration
#> # This file declares global variables to avoid R CMD check NOTEs
#> # about 'no visible binding for global variable'
#> # Generated automatically by fix_global_variables()
#> 
#> utils::globalVariables(c(
#>   "category",
#>   "measurement", 
#>   "species"
#> ))

# Apply the fix
fix_global_variables(check_output)

# Interactive mode
fix_global_variables_interactive()
```

### Non-ASCII Character Fixes
```r
# Copy R CMD check output with non-ASCII character errors
check_output <- "
Found the following files with non-ASCII characters:
  R/analysis_functions.R
  R/plotting_helpers.R
"

# Preview what characters would be replaced
fix_non_ascii_characters(check_output, preview_only = TRUE)
#> Found 2 files with non-ASCII characters:
#>   - R/analysis_functions.R
#>   - R/plotting_helpers.R
#> 
#>   analysis_functions.R: Found 3 non-ASCII character issues
#>     PREVIEW - Changes that would be made:
#>       Line 15: '"' -> '"'
#>       Line 23: '"' -> '"'
#>       Line 67: '–' -> '-'

# Apply the fixes (creates backups by default)
fix_non_ascii_characters(check_output)

# Scan proactively for non-ASCII characters
scan_results <- scan_non_ascii_characters()

# Interactive mode
fix_non_ascii_characters_interactive()

# Custom character replacements
fix_non_ascii_characters(check_output, 
  custom_replacements = list("©" = "(c)", "®" = "(R)"))
```

### Empty Examples Fixes
```r
# Copy R CMD check output with empty examples warnings
check_output <- "
prepare_Rd: process_data.Rd:18-21: Dropping empty section \\examples
prepare_Rd: analyze_results.Rd:25-28: Dropping empty section \\examples  
"

# Preview what examples would be added
fix_empty_examples(check_output, preview_only = TRUE)

# Apply fixes with custom example template
fix_empty_examples(check_output, 
  example_template = "result <- FUNCNAME(my_data)")

# Interactive mode
fix_empty_examples_interactive()
```

## 🎨 Shiny Dashboard

Launch the interactive dashboard for a visual interface to all features:

```r
# Launch dashboard for current package
launch_doc_app()

# Launch for specific package
launch_doc_app("/path/to/your/package")

# Launch on specific port without opening browser
launch_doc_app(port = 3838, launch_browser = FALSE)
```

### Dashboard Features:
- **Functions Tab**: Browse functions, generate documentation, bulk operations
- **Dependencies Tab**: Analyze dependencies, install missing packages
- **Namespace Tab**: Interactive namespace conversion with preview
- **R CMD Check Tab**: GUI for common R CMD check fixes
- **Settings Tab**: Package statistics and function reference

## 🔬 Advanced Usage

### Custom Documentation Templates
```r
# Generate internal function template
template <- generate_roxygen_template(
  function_name = "helper_function",
  args = list(x = NULL, internal_param = "TRUE"),
  template_type = "internal",
  add_examples = FALSE
)

# Update existing documentation
update_function_docs(
  file_path = "R/my_file.R",
  function_name = "existing_function", 
  new_docs = updated_template
)
```

### Dependency Analysis
```r
# Detailed dependency scanning
code_deps <- scan_code_dependencies(functions_data, ".")
suspected <- detect_suspected_packages(code_content)
missing <- check_missing_packages(all_packages)

# Parse DESCRIPTION fields
imports <- extract_description_field(desc_text, "Imports")
suggests <- extract_description_field(desc_text, "Suggests")
```

### Smart Parameter Suggestions
```r
# Get parameter suggestions based on package history
suggestions <- suggest_parameters(
  package_data = pkg_info,
  param_name = "data",
  function_name = "process_data"
)

# Build parameter history from documentation
param_history <- build_parameter_history(docs_data)
```

## 📋 Workflow Examples

### Complete Package Setup Workflow
```r
# 1. Analyze package
pkg_info <- analyze_package()

# 2. Install missing dependencies  
if (length(pkg_info$dependencies$missing) > 0) {
  install_missing_packages(pkg_info$dependencies$missing)
}

# 3. Generate documentation for undocumented functions
generate_bulk_documentation(pkg_info, save_to_files = TRUE)

# 4. Convert to namespace calls
apply_namespace_conversion(backup = TRUE)

# 5. Interactive R CMD check fixes
vibecheck()
```

### Pre-CRAN Submission Checklist
```r
# Run comprehensive check
issues <- quick_package_check()

# Fix any R CMD check issues
# (paste actual R CMD check output)
fix_global_variables(check_output)
fix_non_ascii_characters(check_output)  
fix_empty_examples(check_output)

# Verify all dependencies are declared
deps <- analyze_package_dependencies()
if (length(deps$undeclared) > 0) {
  message("Add these to DESCRIPTION: ", paste(deps$undeclared, collapse = ", "))
}

# Final check
vibecheck()
```

## 🤝 Contributing

This package was created through "vibe coding" with Claude AI. While it works well for the intended use cases, there may be edge cases and bugs. Contributions, bug reports, and feature requests are welcome!

## 🙏 Acknowledgments

- Built with the power of AI-assisted development
- Inspired by the R package development community
- Special thanks to the maintainers of `roxygen2`, `devtools`, and `shiny`

---

**Happy package development! 📦✨**