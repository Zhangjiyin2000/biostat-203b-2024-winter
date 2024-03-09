
library(shiny)
library(readr)
library(bigrquery)
library(dbplyr)
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)
library(forcats)
library(ggplot2)

# path to the service account token
satoken <- "biostat-203b-2024-winter-313290ce47a6.json"
# BigQuery authentication using service account
bq_auth(path = satoken)

# connect to the BigQuery database `biostat-203b-2024-winter.mimic4_v2_2`
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)
con_bq

# read the dataset
dataset <- readRDS("mimic_icu_cohort.rds")
longData <- dataset %>%
  pivot_longer(
    cols = c(
      "bicarbonate", "chloride", "creatinine",
      "glucose", "potassium", "sodium",
      "hematocrit", "wbc"
    ),
    names_to = "vital_lab",
    values_to = "Value_lab"
  ) 
longData_chart <- dataset %>%
  pivot_longer(
    cols = c(
      "heart_rate", "non_invasive_blood_pressure_systolic", 
      "non_invasive_blood_pressure_diastolic", "temperature_fahrenheit", 
      "respiratory_rate"
    ),
    names_to = "vital_chart",
    values_to = "Value_chart"
  )

ui <- fluidPage(
  
  # App title ----
  titlePanel("MIMIC-IV Cohort Explorer"),
  fluidRow(
    tabsetPanel(
      tabPanel("Categorical Data Summary",
               tableOutput("summaryTable")
      ),
      tabPanel(
        "Continuous Data Summary",
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
          
          # Sidebar panel for inputs ----
          sidebarPanel(
            
            # Input: Selector for choosing dataset ----
            selectInput(
              inputId = "variable",
              label = "Variable of interest",
              choices = c(
                "Chart Events Vitals" = "vital_chart",
                "Lab Events Vitals" = "vital_lab",
                "Age" = "age_intime"
              )
            )
          
          ),
          mainPanel(
            tableOutput("summary_continuous")
          )
        )
      ),
      tabPanel(
        "Patient characteristics",
        verbatimTextOutput("summary"),
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
          
          # Sidebar panel for inputs ----
          sidebarPanel(
            
            # Input: Selector for choosing dataset ----
            selectInput(
              inputId = "var",
              label = "Variable of interest",
              choices = c(
                "Last Care Unit" = "last_careunit",
                "Lab Events" = "lab_events",
                "Gender" = "gender",
                "Race" = "race",
                "Marital Status" = "marital_status",
                "Insurance" = "insurance",
                "Discharge Location" = "discharge_location",
                "Admission Type" = "admission_type",
                "Admission Location" = "admission_location",
                "First Care Unit" = "first_careunit",
                "Age" = "age_intime"
              )
            ),
            checkboxInput(
              "outliers",
              "Remove outliers in IQR method for measurements?",
              FALSE
            )
          ),
          mainPanel(
            plotOutput("plot1")
          )
        )
      ),
      tabPanel(
        "Patient's ADT and ICU stay infomation",
        tableOutput("view"),
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
          
          # Sidebar panel for inputs ----
          sidebarPanel(
            
            # Input:  ----
            # make a number input box into the UI.
            tags$p("Select a patient."),
            selectInput("patient_id", 
                           label = "Patient ID", 
                           choices = sample(unique(dataset$subject_id), 5)
            )
          ),
          mainPanel(
            plotOutput("plot2")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # read the dataset
  dataset <- readRDS("mimic_icu_cohort.rds")
  get_first_value <- function(data, column_name,
                              default_value = "Not Available") {
    if (!is.null(data) && nrow(data) > 0 && column_name %in% names(data)) {
      return(data[[column_name]][1])
    } else {
      return(default_value)
    }
  }
  # Plot for the first tab ----------------------Patient characteristics
  output$plot1 <- renderPlot({
    if (input$var == "last_careunit") {
      ggplot(dataset, aes_string(input$var)) +
        geom_bar(position = position_stack(reverse = TRUE)) +
        coord_flip() +
        theme_minimal() +
        labs(
          title = "Patient count by stay or patient variable group",
          y = "Count",
          x = "Last Care Unit"
        )
    } else if (input$var == "lab_events") {
      # Melt (pivot) the dataset for only the lab events columns
      longData <- dataset %>%
        pivot_longer(
          cols = c(
            "bicarbonate", "chloride", "creatinine",
            "glucose", "potassium", "sodium",
            "hematocrit", "wbc"
          ),
          names_to = "LabEvent",
          values_to = "Value"
        )
      
      # Remove outliers if the checkbox is checked
      if (input$outliers) {
        filterlongData <- longData %>%
          group_by(LabEvent) %>%
          # Calculate the IQR and filter within the same pipeline
          mutate(
            Q1 = quantile(Value, 0.25, na.rm = TRUE),
            Q3 = quantile(Value, 0.75, na.rm = TRUE),
            IQR = Q3 - Q1,
            LowerBound = Q1 - 1.5 * IQR,
            UpperBound = Q3 + 1.5 * IQR
          ) %>%
          filter(Value >= LowerBound & Value <= UpperBound) %>%
          select(-c(Q1, Q3, IQR, LowerBound, UpperBound)) # Remove the helper columns
        
        # Plot boxplots for all lab events
        ggplot(filterlongData, aes(x = LabEvent, y = Value)) +
          geom_boxplot() +
          theme_minimal() +
          labs(title = "Distribution of Lab Events across Patients", 
               x = "Lab Event", 
               y = "Value") +
          # Rotate x-axis labels for readability
          theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
          coord_flip()
      } else {
        # Plot boxplots for all lab events
        ggplot(longData, aes(x = LabEvent, y = Value)) +
          geom_boxplot() +
          theme_minimal() +
          labs(title = "Distribution of Lab Events across Patients", 
               x = "Lab Event", 
               y = "Value") +
          # Rotate x-axis labels for readability
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          coord_flip()
      }
    } else if (input$var == "gender") {
      ggplot(dataset, aes_string(input$var)) +
        geom_bar(position = position_stack(reverse = TRUE)) +
        coord_flip() +
        theme_minimal() +
        labs(
          title = "Patient count by stay or patient variable group",
          y = "Count",
          x = "Gender")
    } else if (input$var == "race") {
      ggplot(dataset, aes_string(input$var)) +
        geom_bar(position = position_stack(reverse = TRUE)) +
        coord_flip() +
        theme_minimal() +
        labs(
          title = "Patient count by stay or patient variable group",
          y = "Count",
          x = "Race")
    } else if(input$var == "marital_status") {
      ggplot(dataset, aes_string(input$var)) +
        geom_bar(position = position_stack(reverse = TRUE)) +
        coord_flip() +
        theme_minimal() +
        labs(
          title = "Patient count by stay or patient variable group",
          y = "Count",
          x = "Marital Status")
    } else if(input$var == "insurance") {
      ggplot(dataset, aes_string(input$var)) +
        geom_bar(position = position_stack(reverse = TRUE)) +
        coord_flip() +
        theme_minimal() +
        labs(
          title = "Patient count by stay or patient variable group",
          y = "Count",
          x = "Insurance")
    } else if(input$var == "discharge_location") {
      ggplot(dataset, aes_string(input$var)) +
        geom_bar(position = position_stack(reverse = TRUE)) +
        coord_flip() +
        theme_minimal() +
        labs(
          title = "Patient count by stay or patient variable group",
          y = "Count",
          x = "Discharge Location")
    } else if(input$var == "admission_type") {
      ggplot(dataset, aes_string(input$var)) +
        geom_bar(position = position_stack(reverse = TRUE)) +
        coord_flip() +
        theme_minimal() +
        labs(
          title = "Patient count by stay or patient variable group",
          y = "Count",
          x = "Admission Type")
    } else if(input$var == "admission_location") {
      ggplot(dataset, aes_string(input$var)) +
        geom_bar(position = position_stack(reverse = TRUE)) +
        coord_flip() +
        theme_minimal() +
        labs(
          title = "Patient count by stay or patient variable group",
          y = "Count",
          x = "Admission Location")
    } else if(input$var == "first_careunit") {
      ggplot(dataset, aes_string(input$var)) +
        geom_bar(position = position_stack(reverse = TRUE)) +
        coord_flip() +
        theme_minimal() +
        labs(
          title = "Patient count by stay or patient variable group",
          y = "Count",
          x = "First Care Unit")
    } else if(input$var == "age_intime") {
      ggplot(dataset, aes_string(input$var)) +
        geom_histogram(binwidth = 5) +
        theme_minimal() +
        labs(
          title = "Patient count by stay or patient variable group",
          y = "Count",
          x = "Age")
    }
    
  })
  
  
  # Plot for the second tab --- Patient's ADT and ICU stay infomation
  output$plot2 <- renderPlot({
    sid <- as.numeric(input$"patient_id")
    
    sid_info <- tbl(con_bq, "patients") |>
      filter(subject_id == sid)
    
    sid_adt <- tbl(con_bq, "transfers") |>
      filter(subject_id == sid) |>
      collect()
    
    sid_adm <- tbl(con_bq, "admissions") |>
      filter(subject_id == sid) |>
      collect()
    
    sid_lab <- tbl(con_bq, "labevents") |>
      filter(subject_id == sid) |>
      collect()
    
    sid_proc <- tbl(con_bq, "procedures_icd") |>
      filter(subject_id == sid) |>
      left_join(tbl(con_bq, "d_icd_procedures"),
                by = c("icd_code", "icd_version")
      )
    
    # Extract the top 3 diagnoses
    top_dia <- tbl(con_bq, "diagnoses_icd") |>
      filter(subject_id == sid, seq_num <= 3) |>
      select(seq_num, icd_code) |>
      arrange(seq_num) |>
      distinct() |>
      head(n = 3) |>
      pull(icd_code)
    
    # Extract the corresponding long titles for the top 3 diagnosis codes
    top_3_title <- tbl(con_bq, "d_icd_diagnoses") |>
      filter(icd_code %in% top_dia) |>
      pull(long_title)
    
    ggplot() +
      geom_segment(
        data = sid_adt %>% filter(eventtype != "discharge"),
        mapping = aes(
          x = intime,
          xend = outtime,
          y = "ADT",
          yend = "ADT",
          color = careunit,
          size = as.numeric(str_detect(careunit, "(ICU|CCU)"))
        )
      ) +
      geom_point(
        data = sid_lab %>% distinct(charttime, .keep_all = TRUE),
        mapping = aes(x = charttime, y = "Lab"),
        shape = "+",
        size = 5
      ) +
      geom_jitter(
        data = sid_proc,
        mapping = aes(
          x = chartdate + hours(12),
          y = "Procedure",
          shape = str_sub(long_title, 1, 25)
        ),
        size = 3,
        height = 0
      ) +
      labs(
        title = str_c(
          "Patient ", sid, " - ",
          get_first_value(filter(dataset, subject_id == sid), "gender"), " - ",
          get_first_value(filter(dataset, subject_id == sid), "age_at_intime"),
          " years old, ",
          str_to_lower(get_first_value(filter(sid_adm, subject_id == sid), "race"))
        ),
        subtitle = str_c(top_3_title, collapse = "\n"),
        x = "Calendar Time",
        y = "",
        color = "Care Unit",
        shape = "Procedure"
      ) +
      guides(size = "none") +
      scale_y_discrete(limits = rev) +
      theme_light() +
      theme(legend.position = "bottom", legend.box = "vertical")
  })
  
 
  
  # Categorical Data --- Summary table -----------------------------------------
  output$summaryTable <- renderTable({
    cat_data <- dataset %>%
      select(
        first_careunit, last_careunit, los, admission_type, admission_location,
        discharge_location, insurance, language, marital_status, race,
        hospital_expire_flag, gender, dod
      ) |>
      tbl_summary()
    
   
  })
  
  # Continuous Data --- Summary table (using dropdown) -----------------------
  output$summary_continuous<- renderTable({
    if(input$variable == "vital_chart") {
      longData_chart  %>%
        
        group_by(vital_chart) %>%
        summarise(
          Mean = mean(Value_chart, na.rm = TRUE),
          Median = median(Value_chart, na.rm = TRUE),
          min = min(Value_chart, na.rm = TRUE),
          max = max(Value_chart, na.rm = TRUE),
          `Standard Deviation` = sd(Value_chart, na.rm = TRUE),
          Q1 = quantile(Value_chart, 0.25, na.rm = TRUE),
          Q3 = quantile(Value_chart, 0.75, na.rm = TRUE)
        ) %>%
        ungroup()
    } else if(input$variable == "vital_lab") {
      
        longData %>%
          group_by(vital_lab) %>%
          summarise(
            Mean = mean(Value_lab, na.rm = TRUE),
            Median = median(Value_lab, na.rm = TRUE),
            min = min(Value_lab, na.rm = TRUE),
            max = max(Value_lab, na.rm = TRUE),
            `Standard Deviation` = sd(Value_lab, na.rm = TRUE),
            Q1 = quantile(Value_lab, 0.25, na.rm = TRUE),
            Q3 = quantile(Value_lab, 0.75, na.rm = TRUE)
          ) %>%
          ungroup()
    } else if(input$variable == "age_intime") {
      cat_data <- dataset %>%
        select(age_intime) |>
        summarise(
          Mean = mean(age_intime, na.rm = TRUE),
          Median = median(age_intime, na.rm = TRUE),
          min = min(age_intime, na.rm = TRUE),
          max = max(age_intime, na.rm = TRUE),
          `Standard Deviation` = sd(age_intime, na.rm = TRUE),
          Q1 = quantile(age_intime, 0.25, na.rm = TRUE),
          Q3 = quantile(age_intime, 0.75, na.rm = TRUE)
        ) 
    }
    
    
  })
}

shinyApp(ui, server)
