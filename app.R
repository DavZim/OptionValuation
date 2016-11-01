########################################################################
# PACKAGES and FUNCTIONS #
########################################################################

# if necessary install the following packages with:
# pkgs <- c("data.table", "DT", "ggplot2", "knitr", "magrittr", "rmarkdown", 
#           "RQuantLib", "shiny")
# install.packages(pkgs)

library(data.table)
library(DT)
library(ggplot2)
library(knitr)
library(magrittr)
library(rmarkdown)
library(RQuantLib)
library(shiny)

source("R/functions.R")

# uncomment this if the text-files should be recompiled
# rmdfiles <- c("files/about.rmd", "files/text_intro_options.rmd", "files/text_intro_valuation.rmd")
# sapply(rmdfiles, knit, quiet = T)
# sapply(rmdfiles, render, quiet = T)

########################################################################
# UI #
########################################################################

# payoff_ui for the calculation-tab
payoff_ui <- function() {
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      h3("Option Basket Input"),
      radioButtons("I_basket_dir", "Position", 
                   list("Long" = 1, "Short" = 2), 
                   selected = 1, inline = T),
      radioButtons("I_basket_type", "Option Type",
                   list("Call" = 1, "Put" = 2, "Underlying" = 3),
                   selected = 1, inline = T),
      conditionalPanel("input.I_basket_type < 3", 
                       sliderInput("I_basket_strike", "Strike Price", value = 100,
                                   min = 0, max = 250)
      ),
      actionButton("BT_basket_new_option", "Insert", icon = icon("check")),
      actionButton("BT_basket_clear", "Clear", icon = icon("remove")),
      textOutput("text_new_option"),
      br(),
      helpText("The premium is automatically calculated for a given volatility\n",
      "and for an underlying currently worth $100.")
    ),
    mainPanel = mainPanel(
               tabsetPanel("asd",
                           tabPanel("Theory",
                                    htmlOutput("payoff_theory")),
                           tabPanel("Figure",
                                    plotOutput("p_payoffs")
                           ),
                           tabPanel("Data",
                                    DT::dataTableOutput("t_option_basket")
                           )
               )
    )
  )
  
}

# greek_ui for the display of the greeks
greek_ui <- function() {
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      h3("Financial Greek Input"),
      radioButtons("I_greek_dir", "Position", 
                   list("Long" = 1, "Short" = 2), 
                   selected = 1, inline = T),
      radioButtons("I_greek_type", "Option",
                   list("Call" = 1, "Put" = 2),
                   selected = 1, inline = T),
      radioButtons("I_greek_eu_am", "Option Type",
                   list("European" = 1, "American" = 2), inline = T),
      sliderInput("I_greek_strike", "Strike Price (in $)", value = 100,
                  min = 1, max = 250),
      sliderInput("I_greek_value_underlying", "Current Value Underlying (in $)", 
                  value = 100,
                  min = 1, max = 250),
      sliderInput("I_greek_maturity", "Maturity (in years)", value = 1,
                  min = 0, max = 5, step = 0.1),
      # dateInput("I_greek_maturity", "Maturity", value = Sys.Date() + 31,
      #           min = Sys.Date() + 1, max = Sys.Date() + 365 * 2),
      sliderInput("I_greek_dvd_yield", "Dividend Yield (in %)", value = 5,
                  min = 0, max = 20),
      sliderInput("I_greek_rf", "Risk-Free Rate (in %)", value = 1,
                  min = 0, max = 20),
      sliderInput("I_greek_vola", "Volatility (in %)", value = 1,
                  min = 0, max = 20)
    ),
    mainPanel = mainPanel(
      tabsetPanel("",
                  tabPanel("Theory",
                           DT::dataTableOutput("greek_theory")
                  ),
                  tabPanel("Text Result",
                           verbatimTextOutput("text_single_result")
                  ),
                  tabPanel("The Greeks ",
                           radioButtons("I_greek_var", "Variable",
                                        list("Underlying" = "underlying",
                                             "Strike" = "strike",
                                             "Dividend Yield" = "dvd_yield",
                                             "Risk-Free Rate" = "rf",
                                             "Maturity" = "maturity",
                                             "Volatility" = "vola"),
                                        inline = T),
                           plotOutput("plot_greeks"),
                           "Note: the greeks refer mostly to the cases where the variable is the underlying."
                  )
      )
    )
  )
}

# price_ui for the display of the greeks
price_ui <- function() {
  fluidPage(tabsetPanel("",
                        tabPanel("Theory",
                                 htmlOutput("price_theory")
                        ),
                        tabPanel("Binomial Model",
                                 sidebarLayout(
                                     sidebarPanel = sidebarPanel(
                                       h3("Binomial Input"),
                                       numericInput("I_price_n_steps", "Number of Steps",
                                                    min = 1, max = 10, value = 3),
                                       radioButtons("I_price_type", "Option",
                                                    list("Call" = 1, "Put" = 2),
                                                    selected = 1, inline = T),
                                       sliderInput("I_price_strike", "Strike Price (in $)", value = 100,
                                                   min = 1, max = 250),
                                       sliderInput("I_price_value_underlying", "Current Value Underlying (in $)",
                                                   value = 100,
                                                   min = 1, max = 250),
                                       sliderInput("I_price_tick", "Tick (in $)", value = 10,
                                                   min = 0, max = 50),
                                       sliderInput("I_price_rf", "Risk-Free Rate (in %)", value = 1,
                                                   min = 0, max = 20)
                                     ),
                                     mainPanel = mainPanel(
                                       plotOutput("price_binomial")
                                     )
                                   )
                        ),
                        tabPanel("Black-Scholes Model",
                                 sidebarLayout(
                                   sidebarPanel = sidebarPanel(
                                     h3("Black-Scholes Input"),
                                     radioButtons("I_bs_type", "Option",
                                                  list("Call" = 1, "Put" = 2),
                                                  selected = 1, inline = T),
                                     sliderInput("I_bs_strike", "Strike Price (in $)", value = 100,
                                                 min = 1, max = 250),
                                     sliderInput("I_bs_value_underlying", "Current Value Underlying (in $)", 
                                                 value = 100,
                                                 min = 1, max = 250),
                                     sliderInput("I_bs_maturity", "Maturity (in years)", value = 1,
                                                 min = 0, max = 5, step = 0.1),
                                     sliderInput("I_bs_rf", "Risk-Free Rate (in %)", value = 1,
                                                 min = 0, max = 20, step = 0.1),
                                     sliderInput("I_bs_vola", "Volatility (in %)", value = 1,
                                                 min = 0, max = 20, step = 0.1)
                                   ),
                                   mainPanel = mainPanel(
                                     uiOutput("price_bs")
                                   )
                                 )
                        )
            )
  )
}

# main_file for the main-tabs 
main_ui <- shinyUI(
  navbarPage("Options 101",
             #tabPanel("Theory", #"asd"),
            #          htmlOutput("theory")),
             #includeHTML("files/text_intro_options.html")),
             tabPanel("Payoff",
                      payoff_ui()
             ),   
             tabPanel("Valuation",
                      price_ui()
             ),
             tabPanel("Price and the Greeks",
                      greek_ui()
             ),
             tabPanel("About",
                      helpText("v 0.1 @ 2016", br(), 
                               "David Zimmermann", br(),
                               "All Rights Reserved", br(),
                               "david.zimmermann[at]zu.de", br(),
                               "All feedback welcome!"))#includeHTML("about.html"))
  )
)

########################################################################
# SERVER #
########################################################################

server_fun <- function(input, output, session) {
  # init ----
  output$payoff_theory <- renderUI({includeHTML("files/text_intro_options.html")})
  output$price_theory <- renderUI({includeHTML("files/text_intro_valuation.html")})
  empty_dt_positions()
  render_plot1(output = output)
  render_positions_table(output)
  output$text_new_option <- renderText("")
  
  # Observe Events Basket ----
  observeEvent(input$BT_basket_new_option, {
    cat("New Option!\n")
    opt.dir <- input$I_basket_dir
    opt.type <- input$I_basket_type
    opt.strike <- input$I_basket_strike 
    exp.date <- Sys.Date() + 365
    
    opt.dir <- ifelse(opt.dir == 1, "Long", "Short")
    
    if (opt.type == 1) {
      opt.type <- "Call"
    } else if (opt.type == 2) {
      opt.type <- "Put"
    } else if (opt.type == 3) {
      opt.type <- "Underlying"
      opt.strike <- 100
    }
    
    if (opt.type != "Underlying") {
      opt.premium <- EuropeanOption(type = tolower(opt.type),
                                    underlying = 100,
                                    strike = opt.strike,
                                    dividendYield = 0,
                                    riskFreeRate = 0,
                                    maturity = (exp.date - Sys.Date())/364,
                                    volatility = 0.2)$value %>% round(4)
      
    } else {
      opt.premium <- 0
    }
    
    tmp <- data.table(name = paste0(opt.dir, " ", 
                                    opt.type, " ", 
                                    opt.strike, exp.date),
                      type = opt.type,
                      dir = opt.dir,
                      strike = opt.strike,
                      premium = opt.premium)
    
    dt.positions <<- rbindlist(list(dt.positions,
                                    tmp))
    cat("New Option registered!\n")
    plotPF(output)
    text_option <- paste("New", opt.type, "inserted, with a premium of", 
                         opt.premium)
    output$text_new_option <- renderText(text_option)
  })
  
  observeEvent(input$BT_basket_clear, {
    cat("clear!\n")
    # reset inputs!
    output$text_new_option <- renderText("files/text_intro_options.html")
    updateSliderInput(session, "I_basket_strike", value = 100)
    updateRadioButtons(session, "I_basket_dir", selected = 1)
    updateRadioButtons(session, "I_basket_type", selected = "Call")
    
    # reset data!
    empty_dt_positions()
    render_plot1(NA, output = output)
    render_positions_table(output)
  })
  
  # Single Options ----
  option <- reactive(data.table(type = ifelse(input$I_greek_type == 1, "call", 
                                              "put"),
                                dir = ifelse(input$I_greek_dir == 1, "long", 
                                             "short"),
                                strike = input$I_greek_strike,
                                underlying = input$I_greek_value_underlying,
                                maturity = input$I_greek_maturity,
                                # as.numeric(input$I_greek_maturity -
                                #                      Sys.Date()) / 365,
                                dvd_yield = input$I_greek_dvd_yield / 100,
                                rf = input$I_greek_rf / 100,
                                vola = input$I_greek_vola / 100,
                                eu_am = ifelse(input$I_greek_eu_am == 1,
                                               "European", "American")))
  
  option_result <- reactive({
    dat <- option()
    
    if (dat$eu_am == "European") {
      EuropeanOption(type = dat$type,
                     underlying = dat$underlying,
                     strike = dat$strike,
                     dividendYield = dat$dvd_yield,
                     riskFreeRate = dat$rf,
                     maturity = dat$maturity,
                     volatility = dat$vola)
    } else {
      AmericanOption(type = dat$type,
                     underlying = dat$underlying,
                     strike = dat$strike,
                     dividendYield = dat$dvd_yield,
                     riskFreeRate = dat$rf,
                     maturity = dat$maturity,
                     volatility = dat$vola,
                     engine = "CrankNicolson")
    }
  })
  
  greek_plot_data <- reactive({
    dat <- option()
    
    var <- input$I_greek_var
    
    val <- dat[, var, with = F] %>% as.numeric()
    
    input_parameters <- data.table(var = var,
                                   val_min = round(val * 0.75, 4),
                                   val_max = round(val * 1.25, 4))
    print(input_parameters)
    pdat <- sensitivityWrapper(input_parameters, dat)
    return(pdat)
  })
  
  output$plot_greeks <- renderPlot({
    
    pdat <- greek_plot_data()
    
    ggplot(pdat[is.finite(yval)], aes(x = xval, y = yval)) + geom_line() + 
      theme_bw() + xlab(fUpper(pdat[1, xvar])) + 
      facet_wrap(~yvar, scales = "free") + ggtitle("The Greeks") +
      ylab("Value of the Greek") 
  })
  
  interpretation <-  c("Value of the option",
                       "Sensitivity of the option value to a change in the underlying",
                       "Sensitivity of the option delta to a change in the underlying",
                       "Sensitivity of the option value to a change in the underlying's volatility",
                       "Sensitivity of the option value to a change in t, the remaining time to maturity",
                       "Sensitivity of the option value to a change in the risk-free interest rate",
                       "Sensitivity of the option value to a change in the dividend yield")
  
  df <- data.frame(greek = c("value", "delta", "gamma", "vega", "theta", "rho", "dividendRho"),
                   interpretation = interpretation)
  
  df_dt <- datatable(df, rownames = NA, colnames = c("Greek", "Interpretation"))
  
  output$greek_theory <- DT::renderDataTable(df_dt)
  
  output$text_single_result <- renderPrint(option_result())
  
  
  # Prices & Valuation
  
  output$price_binomial <- renderPlot({
    # cat("n_steps: ", input$I_price_n_steps,
    #     "\ntype: ", input$I_price_type,
    #     "\ns_0: ", input$I_price_value_underlying,
    #     "\ntick: ", input$I_price_tick,
    #     "\nk: ", input$I_price_strike,
    #     "\nrf: ", input$I_price_rf,"\n")
    create_tree(n_steps = input$I_price_n_steps,
                type = ifelse(input$I_price_type == 1, "call", "put"),
                s_0 = input$I_price_value_underlying, 
                tick = input$I_price_tick, 
                k = input$I_price_strike, 
                rf = input$I_price_rf/100)
  }, height = 700)
  
  bs_data <- reactive({
    list(
      type = input$I_bs_type,
      s0 = input$I_bs_value_underlying,
      k = input$I_bs_strike,
      r = input$I_bs_rf/100,
      ti = input$I_bs_maturity,
      s = input$I_bs_vola/100
    )
  })
  
  output$price_bs <- renderUI({
    text1 <- "The Black-Scholes formula predicts an option price for a European call option to be equal to
$$ C = S_0 N(d_1) - Ke^{-rt} N(d_2) $$
with
$$ d_1 = \\frac{ln(S_o/Ke^{-rT})}{\\sigma \\sqrt{T}} $$
and 
$$ d_2 = d_1 - \\sigma \\sqrt{T}. $$

The put option is priced using the put-call parity and is expected to be worth

$$ P = Ke^{-rT} N(-d_2) - S_0N(-d_1). $$


The result of the inputs from the left side are:
    "
    
    text2_call <- "$$ C = %.0f*N(%.03f) - %.0f e^{-%.03f * %.01f} * N(%.03f) = %.03f $$
with 
$$ d_1 = \\frac{ln(%.0f/%.0fe^{-%.03f*%.01f})}{%.03f\\sqrt{%.01f}} = %.03f, $$

$$ d_2 = %.03f - %.03f \\sqrt{%.01f} = %0.3f, $$

$$ N(d_1) = N(%.03f) = %.03f, $$
and
$$ N(d_2) = N(%.03f) = %.03f, $$
    "
    
    text2_put <- "$$ P = %.0fe^{%.03f*%.01f} * N(-%.03f) - %.0f * N(-%.03f) = %.03f $$
with 
$$ d_1 = \\frac{ln(%.0f/%.0fe^{%.03f*%.01f})}{%.03f\\sqrt{%.01f}} = %.03f $$

$$ d_2 = %.03f - %.03f \\sqrt{%.01f} = %0.3f, $$

$$ N(-d1) = N(%.03f) = %.03f, $$
and
$$ N(-d2) = N(%.03f) = %.03f, $$
    "
    
    s0 <- bs_data()$s0
    k <- bs_data()$k
    r <- bs_data()$r
    ti <- bs_data()$ti
    s <- bs_data()$s
    
    d1 <- (log(s0/(k*exp(-r*ti)))) / (s*sqrt(ti))
    d2 <-  d1 - s*sqrt(ti)
    n_d1 <-  pnorm(d1)
    n_d2 <- pnorm(d2)
    n_d1_minus <- pnorm(-d1)
    n_d2_minus <- pnorm(-d2)
    C <- s0*n_d1 - k * exp(-r*ti)*n_d2
    P <- k*exp(-r*ti)*n_d2_minus - s0*n_d1_minus
    
    if (bs_data()$type == 1) {
      withMathJax(
        sprintf(paste0(text1, text2_call), s0, d1, k, r, ti, d2, C, s0, k, r, ti, 
                s, ti, d1, d1, s, ti, d2, d1, n_d1, d2, n_d2))
    } else {
      withMathJax(
        sprintf(paste0(text1, text2_put), k, -r, ti, d2, s0, d1, P, s0, k, -r, ti,
                s, ti, d1, d1, s, ti, d2, -d1, n_d1_minus, -d2, n_d2_minus))
    }
  })
  
}

########################################################################
# RUN APP #
########################################################################

shinyApp(main_ui, server_fun)