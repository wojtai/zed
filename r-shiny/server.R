library(shiny)
library(dplyr)
library(tidyr)
library(readxl)
library(httr)
library(ggplot2)
library(caret)
library(randomForest)
library(e1071)

shinyServer(
  function(input, output) {
    data <- reactive({
      url <- "http://www.cs.put.poznan.pl/dbrzezinski/teaching/zed/wuhan_blood_sample_data_Jan_Feb_2020.xlsx"
      GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
      df <- read_excel(tf) %>% fill(PATIENT_ID)
      df <- df %>% rename_with(~ toupper(gsub(" ", "_", .x, fixed = TRUE)), 1:7)
      df <- df %>% 
        mutate(across("GENDER", ~factor(., levels=c(1,2), labels=c("MALE", "FEMALE")))) %>%
        mutate(across("OUTCOME", ~factor(., levels=c(0,1), labels=c("CURED","DECEASED"))))
      df <- df %>%
        filter(RE_DATE < DISCHARGE_TIME) %>%
        rename("LDH" = "Lactate dehydrogenase") %>%
        rename("hs_CRP" = "High sensitivity C-reactive protein") %>%
        rename("lymphocyte_count" = "lymphocyte count") %>% 
        select(PATIENT_ID, LDH, hs_CRP, lymphocyte_count, OUTCOME)
      patients <- df %>% 
        group_by(PATIENT_ID) %>% 
        fill(everything()) %>% 
        summarise_all(last) %>% 
        ungroup() %>%
        select(-PATIENT_ID)
      patients[complete.cases(patients), ]
    })
    
    model <- reactive({
      set.seed(23)
      tc <- trainControl(method="repeatedcv", number=10, repeats=3)
      rf.fit <- train(OUTCOME ~ .,
            data=data(),
            method="rf",
            preProc = c("center", "scale"),
            trControl = tc,
            ntree = 30
            )
      rf.fit
    })
    
    output$ldh_plt <- renderPlot({
      ggplot(data(), aes(x=LDH)) + 
        geom_area(stat="bin", binwidth=25) +
        geom_vline(xintercept=input$ldh, color="red")
    })
    output$hs_crp_plt <- renderPlot({
      ggplot(data(), aes(x=hs_CRP)) + 
        geom_area(stat="bin", binwidth=25) +
        geom_vline(xintercept=input$hs_crp, color="red")
    })
    output$lymph_plt <- renderPlot({
      ggplot(data(), aes(x=lymphocyte_count)) + 
        geom_area(stat="bin", binwidth=25) +
        geom_vline(xintercept=input$lymph, color="red")
    })
    
    output$surv_prob <- renderText({
      set.seed(23)
      LDH <- c(input$ldh)
      hs_CRP <- c(input$hs_crp)
      lymphocyte_count <- c(input$lymph)
      nd = data.frame(LDH, hs_CRP, lymphocyte_count)
      print(nd)
      mdl = model()
      rs <- predict(mdl, newdata=nd, type="prob")
      print(rs)
      rs$CURED
    })
  }
)