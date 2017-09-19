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


# Screenshots

The following screenshots were taken from the app itself:


## Payoff 

Add financial options to a basket and see the overall payoff.

![Payoff](https://github.com/DavZim/OptionValuation/raw/master/files/payoff.png)

## Option Valuation

Evaluate a financial option using a binomial tree approach.

![binomial_tree](https://github.com/DavZim/OptionValuation/raw/master/files/binomial_tree.png)

Or calculate the value of the option using the black-scholes approach.

![black_scholes](https://github.com/DavZim/OptionValuation/raw/master/files/black_scholes.png)

## Greeks

Have a look at the sensitivity factors have on the greeks.

![greeks](https://github.com/DavZim/OptionValuation/raw/master/files/greeks.png)
