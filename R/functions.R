calcPF <- function(dt.positions, xmin = 80, xmax = 120) {
  
  res <- lapply(1:nrow(dt.positions), function(i) {
    x <- xmin:xmax
    op <- dt.positions[i]
    s <- op[, strike]
    
    vals <- switch(toupper(op[, type]),
                   "CALL" = {
                     ifelse(x < s, 0, x - s)
                   },
                   "PUT" = {
                     ifelse(x < s, s - x, 0)
                   },
                   "UNDERLYING" = x - s)
    
    op.vals <- data.table(x = x,
                          name = op[, name],
                          type = op[, type],
                          dir = op[, dir],
                          strike = s,
                          premium = op[, premium],
                          payoff = op[, toupper(dir) == "LONG"]*vals - 
                            op[, toupper(dir) == "SHORT"]*vals)
    
    op.vals[, profit := payoff + ifelse(op[, toupper(dir) == "LONG"], 
                                        -premium, 
                                        premium)]
    
    return(op.vals)
  }) %>% rbindlist
  # Calculate total
  
  tot <- res[, .(name = "Net Position", type = "NET", dir = NA, strike = NA, 
                 premium = sum(premium), payoff = sum(payoff),
                 profit = sum(profit)),
             by = c("x")]
  
  res <- rbindlist(list(tot, res))
  return(res)  
}
plotPF <- function(output) {
  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  fUpper <- function(x){
    u <- toupper(substr(x, 1, 1))
    l <- tolower(substr(x, 2, nchar(x)))
    paste0(u,l)
  }
  
  basket <- dt.positions
  xvals <- basket$strike
  
  dt.vals <- calcPF(basket, xmin = 0.75*min(xvals), xmax = 1.25*max(xvals))
  
  dt.vals[, ':=' (name = ifelse(toupper(type) == "UNDERLYING",
                                paste(type, dir),
                                ifelse(toupper(type) == "NET",
                                       "Net Position", 
                                       paste(dir, type, strike, 
                                             paste0("(",premium,")")
                                             )
                                       )
                                )
                  )
          ]
  
  adj_names <- sort(dt.vals[!name %in% c("Net Position"), unique(name)])
  
  values <- gg_color_hue(length(adj_names))
  names(values) <- adj_names
  values <- c(c('Net Position' = "#000000"), values)
  
  linetypes <- rep("solid", length(adj_names))
  names(linetypes) <- adj_names
  linetypes <- c(c('Net Position' = "dashed"), linetypes)
  
  plot1 <- ggplot(dt.vals, aes(x = x, y = profit, 
                               color = name,
                               linetype = name)) +
    geom_hline(yintercept = 0, col = "grey") +
    geom_line(alpha = 0.5) + theme_bw() + 
    ggtitle("Profit/losses") + 
    xlab("Value of the Underlying Asset") + ylab("Profit/Loss") +
    scale_color_manual(name = "", values = values, 
                       breaks = c("Net Position", adj_names)) +
    scale_linetype_manual(name = "", values = linetypes,
                          breaks = c("Net Position", adj_names))
  
  render_plot1(plot1, output)
  render_positions_table(output)
  T
}
render_positions_table <- function(output) {
  output$t_option_basket <- DT::renderDataTable(dt.positions[, .(Type = type, 
                                                             Position = dir,
                                                             Strike = strike, 
                                                             Premium = premium)])
  
}
render_plot1 <- function(plot1 = NA, output) {
  
  if (!is.ggplot(plot1)){
    plot1 <- ggplot() + 
      annotate("text", x = 1, y = 1, label = "Empty Basket") + 
      theme_bw() + ggtitle("Profit/losses") + 
      xlab("Value of the Underlying Asset") + ylab("Profit/Loss")
  }
  
  output$p_payoffs <- renderPlot(plot1)
  
}
empty_dt_positions <- function(){
  dt.positions <<- data.table(name = numeric(0),
                              type = numeric(0),
                              dir = numeric(0),
                              strike = numeric(0),
                              premium = numeric(0))
}
fUpper <- function(x) {
  one <- toupper(substr(x, 1, 1))
  two <- substr(x, 2, nchar(x))
  return(paste0(one, two))
}
sensitivityInputs <- function(dat, var, val_min, val_max, n_vals = 100) {
  xvals <- seq(from = val_min, to = val_max, length.out = n_vals)
  
  res <- lapply(xvals, function(value) {
    r_type <- dat$type
    r_underlying <- dat$underlying
    r_strike <- dat$strike
    r_dvd_yield <- dat$dvd_yield
    r_rf <- dat$rf
    r_maturity <- dat$maturity
    r_vola <- dat$vola
    
    assign(paste0("r_", var), value)
    if (dat$eu_am == "European") {
      tmp <- EuropeanOption(type = r_type,
                            underlying = r_underlying,
                            strike = r_strike,
                            dividendYield = r_dvd_yield,
                            riskFreeRate = r_rf,
                            maturity = r_maturity,
                            volatility = r_vola) %>% as.numeric
    } else {
      tmp <- AmericanOption(type = r_type,
                            underlying = r_underlying,
                            strike = r_strike,
                            dividendYield = r_dvd_yield,
                            riskFreeRate = r_rf,
                            maturity = r_maturity,
                            volatility = r_vola,
                            engine = "CrankNicolson") %>% as.numeric
    }
    names(tmp) <- c("value", "delta", "gamma", "vega", "theta", "rho", "divRho")
    data.table(t(tmp))
  }) %>% rbindlist
  
  res[, var := xvals, with = F]
  
  return(res)
}
sensitivityWrapper <- function(input_params, dat) {
  
  res <- apply(input_params, 1, function(row_el) {
    sensitivityInputs(dat = dat, 
                      var = row_el["var"], 
                      val_min = as.numeric(row_el["val_min"]),
                      val_max = as.numeric(row_el["val_max"]), 
                      n_vals = 100)
  })
  
  tmp <- lapply(res, function(el) {
    var <- names(el)[ncol(el)]
    
    tm <- melt(el, id.vars = var, variable.name = "yvar", value.name = "yval")
    tm[, xvar := var]
    setnames(tm, var, "xval")
    setcolorder(tm, c("xvar", "xval", "yvar", "yval"))
    return(tm)
  }) %>% rbindlist
  
  return(tmp)
  
}

