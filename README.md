# vibeCheck

> Interactive R Package Documentation Assistant

_Disclaimer:_ **The package is completely created with vibe coding using Claude**, thus the name of the package. I have not checked all function details, except for small things that come up in my experiment. It likely contains bugs and unhandled edge cases, but it does what it is designed to do for my specific use case. I may vibe-update this package when I need to use it to update my R packages in the future.  

**vibeCheck**: An interactive Shiny application for analyzing R package documentation, dependencies, and generating roxygen2 documentation templates. `rdochelper` provides comprehensive dependency analysis, missing package detection, and smart parameter suggestions based on existing documentation patterns.

## Features

✅ **Interactive function documentation** with roxygen2  
✅ **Smart parameter suggestions** from existing docs  
✅ **Comprehensive dependency analysis**  
✅ **Missing package detection and installation**  
✅ **DESCRIPTION file analysis**  
✅ **Direct saving to R source files**  
✅ **Automatic namespace conversion** using package dependencies  
✅ **Global variable binding fixes** from R CMD check output  

## Installation

You can install the development version of vibeCheck from [GitHub](https://github.com/) with:

```
# install.packages("devtools")
devtools::install_github("richardli/vibeCheck")
```

## Quick Start
You can launch a package function dashboard at

```
library(vibeCheck)

# Launch the app for current directory
launch_doc_app()

# Launch the app for a specific package
launch_doc_app("/path/to/your/package")
```

Or from the command line only, without launching app

```
check_r_files("/path/to/package")
```
 

 
## Some fun stuff

### DocumentationHelper Class

The core functionality is provided by the `DocumentationHelper` R6 class:

```
# Create a documentation helper
helper <- DocumentationHelper$new("/path/to/package")
```


### Global Variable Fixes

Automatically deal "no visible binding for global variable" errors from R CMD check. Note that this is not a "fix", it is a hack to avoid R check to complain about it only. So only use this if there is a valid reason to do so!

```
# Copy R CMD check output that shows global variable errors
check_output <- "
checking R code for possible problems ... NOTE
  my_function: no visible binding for global variable 'variable_name'
  other_function: no visible binding for global variable 'another_var'
"

# Preview what variables would be added to globalVariables()
fix_global_variables(check_output, preview_only = TRUE)

# Apply the fix (creates or updates R/check_global.R)
fix_global_variables(check_output)

# Using with DocumentationHelper
helper <- DocumentationHelper$new(".")
result <- helper$fix_global_variables(check_output)

# Interactive mode - prompts you to paste check output
fix_global_variables_interactive()
```


### Namespace Conversion

Automatically convert function calls to use explicit namespace notation:

```
# Analyze what functions could be namespaced (from your DESCRIPTION imports)
analysis <- helper$analyze_namespace_opportunities()
cat(analysis$summary)

# Preview what would be converted
results <- helper$auto_namespace_conversion(preview_only = TRUE)

# Apply namespace conversion with backup
results <- helper$auto_namespace_conversion(backup = TRUE)

# Exclude certain functions from conversion
results <- helper$auto_namespace_conversion(
  exclude_functions = c("filter", "select"), 
  backup = TRUE
)
```

 
