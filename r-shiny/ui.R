library(shiny)
shinyUI(fluidPage(
  titlePanel("The bloody markers"),
  sidebarLayout(
    sidebarPanel(
      h3('Input markers'),
      numericInput("ldh", "LDH:", 200, min = 1, max = 1500, step=5),
      numericInput("hs_crp", "hs-CRP:", 10, min = 1, max = 300, step=5),
      numericInput("lymph", "lymphocytes count:", 10, min = 0, max = 25, step=1),
      h5("Survival probability"),
      textOutput("surv_prob")
    ),
    mainPanel(
      h3('Plots'),
      plotOutput("ldh_plt"),
      plotOutput("hs_crp_plt"),
      plotOutput("lymph_plt")
    )
  )
))