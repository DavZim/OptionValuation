# Option Valuation
A shiny application to explore the basics of option valuations

To run the project either fork/download the files and run the `app.R`-file, or in R run
```r
shiny::runGitHub('OptionValuation', 'DavZim')
```

In order to run the project properly, you need to have the following packages installed: `data.table`, `DT`, `ggplot2`, `knitr`, `magrittr`, `rmarkdown`, `fOptions`, and `shiny`.

To install all packages you can also use 

```r
pkgs <- c("data.table", "DT", "ggplot2", "knitr", "magrittr", "rmarkdown", "fOptions", "shiny")
install.packages(pkgs)
```

If you find any bugs, or if you have ideas to advance this project, please reach out to me.
