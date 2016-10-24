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
# rmdfiles <- c("files/about.rmd", "files/text_intro_options.rmd")
# sapply(rmdfiles, knit, quiet = T)
# sapply(rmdfiles, render, quiet = T)

########################################################################
# UI #
########################################################################

# calc_ui for the calculation-tab
calc_ui <- function() {
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      h3("Add Option to Basket"),
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
                           tabPanel("Payoff-Diagramm",
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
      h3("Change Option"),
      radioButtons("I_single_dir", "Position", 
                   list("Long" = 1, "Short" = 2), 
                   selected = 1, inline = T),
      radioButtons("I_single_type", "Option",
                   list("Call" = 1, "Put" = 2),
                   selected = 1, inline = T),
      radioButtons("I_single_eu_am", "Option Type",
                   list("European" = 1, "American" = 2), inline = T),
      sliderInput("I_single_strike", "Strike Price (Option)", value = 100,
                  min = 1, max = 250),
      sliderInput("I_single_value_underlying", "Current Value Underlying", 
                  value = 100,
                  min = 1, max = 250),
      dateInput("I_single_maturity", "Maturity", value = Sys.Date() + 31,
                min = Sys.Date() + 1, max = Sys.Date() + 365 * 2),
      sliderInput("I_single_dvd_yield", "Dividend Yield (in %)", value = 5,
                  min = 0, max = 20),
      sliderInput("I_single_rf", "Risk-Free Rate (in %)", value = 1,
                  min = 0, max = 20),
      sliderInput("I_single_vola", "Volatility (in %)", value = 1,
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
                  tabPanel("The Greeks",
                           radioButtons("I_single_greek", "Variable",
                                        list("Underlying" = "underlying",
                                             "Strike" = "strike",
                                             "Dividend Yield" = "dvd_yield",
                                             "Risk-Free Rate" = "rf",
                                             "Maturity" = "maturity",
                                             "Volatility" = "vola"),
                                        inline = T),
                           plotOutput("plot_greeks")
                           )
                  )
    )
  )
}

# main_file for the main-tabs 
main_ui <- shinyUI(
  navbarPage("Options 101",
             tabPanel("Theory", #"asd"),
                      htmlOutput("theory")),
             #includeHTML("files/text_intro_options.html")),
             tabPanel("Option Calculations",
                      calc_ui()
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
  output$theory <- renderUI({includeHTML("files/text_intro_options.html")})
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
  option <- reactive(data.table(type = ifelse(input$I_single_type == 1, "call", 
                                              "put"),
                                dir = ifelse(input$I_single_dir == 1, "long", 
                                             "short"),
                                strike = input$I_single_strike,
                                underlying = input$I_single_value_underlying,
                                maturity = as.numeric(input$I_single_maturity -
                                                        Sys.Date()) / 365,
                                dvd_yield = input$I_single_dvd_yield / 100,
                                rf = input$I_single_rf / 100,
                                vola = input$I_single_vola / 100,
                                eu_am = ifelse(input$I_single_eu_am == 1,
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
    
    var <- input$I_single_greek
    
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
                       "Sensitivity of the option value for a change in the underlying",
                       "Sensitivity of the option delta for a change in the underlying",
                       "Sensitivity of the option value for a change in the underlying's volatility",
                       "Sensitivity of the option value for a change in t, the remaining time to maturity",
                       "Sensitivity of the option value for a change in the risk-free interest rate",
                       "Sensitivity of the option value for a change in the dividend yield")
  
  df <- data.frame(greek = c("value", "delta", "gamma", "vega", "theta", "rho", "dividendRho"),
                   interpretation = interpretation)
  
  df_dt <- datatable(df, rownames = NA, colnames = c("Greek", "Interpretation"))
  
  output$greek_theory <- DT::renderDataTable(df_dt)
  
  output$text_single_result <- renderPrint(option_result())
}

########################################################################
# RUN APP #
########################################################################

shinyApp(main_ui, server_fun)