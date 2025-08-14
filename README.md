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

## Installation

You can install the development version of rdochelper from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("richardli/vibeCheck")
```

## Quick Start

```r
library(vibeCheck)

# Launch the app for current directory
launch_doc_app()

# Launch the app for a specific package
launch_doc_app("/path/to/your/package")

# Launch on a specific port
launch_doc_app(port = 3838)

# Check R files diagnostic (without launching app)
check_r_files("/path/to/package")
```

## Main Components

### DocumentationHelper Class

The core functionality is provided by the `DocumentationHelper` R6 class:

```r
# Create a documentation helper
helper <- DocumentationHelper$new("/path/to/package")

# Generate documentation template
template <- helper$generate_template("my_function", list(x = NULL, y = "default"))

# Access function data
functions_data <- helper$functions_data
dependencies <- helper$dependencies
missing_packages <- helper$missing_deps
```

### Shiny Interface

The package provides a comprehensive Shiny interface with three main tabs:

1. **Functions Tab**: Browse functions, edit documentation, view suggestions
2. **Dependencies Tab**: Analyze package dependencies and missing packages  
3. **Settings Tab**: View package statistics and parameter history

## Dependency Analysis Features

`rdochelper` provides sophisticated dependency analysis including:

- **Library/Require calls**: `library(package)` and `require(package)`
- **Namespace calls**: `package::function()`
- **Roxygen imports**: `@import` and `@importFrom` tags
- **Pattern-based detection**: Recognizes common function patterns from popular packages
- **DESCRIPTION file comparison**: Finds undeclared and unused dependencies
- **Missing package detection**: Identifies packages that need to be installed

## Documentation Features

- **Template generation**: Creates roxygen2 documentation templates
- **Parameter suggestions**: Uses existing parameter documentation to suggest descriptions
- **Live editing**: Edit documentation directly in the interface
- **File saving**: Save changes directly back to R source files
- **Pattern recognition**: Learns from existing documentation patterns

## Examples

### Basic Usage

```r
library(rdochelper)

# Quick diagnostic of current directory
check_r_files()

# Launch full interface
launch_doc_app()
```

### Programmatic Access

```r
# Create helper for a package
helper <- DocumentationHelper$new(".")

# View all functions found
print(helper$functions_data)

# Check dependencies
print(helper$dependencies)

# Generate template for a specific function
args <- list(data = NULL, method = "default", verbose = TRUE)
template <- helper$generate_template("analyze_data", args)
cat(template)
```

### Dependency Analysis

```r
helper <- DocumentationHelper$new("/path/to/package")

# View missing packages
print(helper$missing_deps)

# View all detected dependencies
all_deps <- unique(c(
  helper$dependencies$library_calls,
  helper$dependencies$namespace_calls,
  helper$dependencies$suspected_packages
))
print(all_deps)
```

## Package Structure Analysis

The package can analyze various R project structures:

- **Standard R packages**: With `R/`, `DESCRIPTION`, `NAMESPACE`
- **Script collections**: R files in any directory
- **Mixed projects**: R files in `src/`, `scripts/`, or root directory

## Supported Patterns

The dependency detection recognizes patterns from popular packages including:

- **tidyverse**: ggplot2, dplyr, tidyr, stringr, purrr, readr
- **Shiny ecosystem**: shiny, shinydashboard, DT, plotly
- **Data manipulation**: data.table, lubridate
- **Development tools**: devtools, testthat, knitr, rmarkdown
- **And many more...**

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

