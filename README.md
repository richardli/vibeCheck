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

# Complete interactive package analysis with fixes (TODO: too slow for now)
vibecheck()

# Quick check without interactive fixes (TODO: too slow for now)
quick_package_check()

# Launch Shiny dashboard (TODO: need update)
launch_doc_app()
```

## 📊 Core Functions

### Package Analysis and function/doc/parameter summary

```r
pkg_info <- analyze_package("path/to/package")
functions_data <- scan_r_functions(".")
docs_data <- parse_function_docs(functions_data)
param_history <- build_parameter_history(docs_data) 
```

### Dependency analysis
```r
# Analyze package structure
pkg_info <- analyze_package()
print(pkg_info$stats)

# Generate documentation for all undocumented functions
docs <- generate_bulk_documentation(pkg_info, save_to_files = FALSE)

# Analyze your package dependencies  (TODO: too slow for now)
deps <- analyze_package_dependencies("path/to/package/root")

# Example of dependency analysis: extract all dplyr functions you're using
dplyr_funcs <- extract_package_functions("dplyr", dependency_data = deps)
print(dplyr_funcs$functions_found)
report <- generate_package_usage_report("dplyr")
cat(report)
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



### 🔧 R CMD Check Fixes

#### Global Variable Fixes
```r
# Copy R CMD check output with global variable errors
check_output <- "
myfunction: no visible binding for global variable 'weighted_value'
plotfunction: no visible binding for global variable 'x'
Undefined global functions or variables:
  weighted_value x y_var
"

# Automatically fix the warnings
extract_and_update_globals(check_output)
```


#### Bulk example template
Suppose there are many functions with the same input. For example

```r
##' model_A
#' @param data  data 
#'
#' @return output from model A
#'
#' @export
model_A <- function(data){
}
##' model_B
#' @param data  data 
#'
#' @return output from model B
#'
#' @export
model_B <- function(data){
}
```

Suppose we want to add the same structures of examples `fit <- model_A(data)` and `fit <- model_B(data)`, ..., etc. The examples can have more than one lines and more than one arguments, but the only difference is that the function name changes.

```r 
# Custom example template: same call style with only function name changed
custom_template <- "
        data <- ...
        fit <- [FUNCNAME](data)
"
result <- add_custom_examples(
  package_data = pkg_info,
  example_template = custom_template,
  functions = c("model_A", "model_B"),  # Test on specific function
  backup = FALSE
)
```
If `functions` argument is not specified, all functions without examples will be updated.

The updated examples will be wrapped around \dontrun to be safe.

### "Smart" Parameter Suggestions

```r
# Get "smart" parameter suggestions based on package history
# That is, if you have already used similar parameters we will find it.
suggestions <- suggest_parameters(
  package_data = pkg_info,
  param_name = "data",
  function_name = "process_data"
)
```


## 🤝 Contributing

This package was created through "vibe coding" with Claude AI. While it worked for me for a specific use cases, there will be edge cases and bugs. Contributions, bug reports, and feature requests are welcome!

## 🙏 Acknowledgments

- Built with the power of AI-assisted development
- I do not recommend careless vibe coding after this project! See my full [thoughts](vibe-coding-thoughts.md) if you are interested.

---

**Happy package development! 📦✨**