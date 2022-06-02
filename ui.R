library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library("tidyverse")
library(bslib)

student_drinking_df <- read.csv("https://query.data.world/s/r47f3cqixzyczsmnwqbrqvu6fcgwh3", stringsAsFactors=FALSE)
student_drinking_df <- student_drinking_df %>% 
  mutate(Talc = Walc + Dalc)

#Visualization 1 data table
fam_relation_df <- student_drinking_df %>%
  select(sex, age, Pstatus, famrel, Talc)

fam_relation_df$Pstatus <- gsub("T", "Together", as.character(fam_relation_df$Pstatus))
fam_relation_df$Pstatus <- gsub("A", "Apart", as.character(fam_relation_df$Pstatus))

parents_status <- fam_relation_df %>%
  group_by(Pstatus, famrel) %>%
  summarize(alc_avg = mean(Talc))


intro_tab <- tabPanel(
  'Introduction',
  fluidPage(
    h1('Introduction')
  )
)

scale_widget <- selectInput(
  inputId = "scale_select",
  label = "Family Relationship Scale",
  choices = parents_status$famrel,
  multiple = F,
)

main_panel_plot <- mainPanel(
  plotlyOutput(outputId = "fam_relation_plot")
)

viz_1_tab <- tabPanel(
  'Visualization 1',
  sidebarLayout(
    sidebarPanel(scale_widget),
    main_panel_plot),
  fluidPage(
    p('Description')
  )
)

viz_2_tab <- tabPanel(
  'Visualization 2',
  fluidPage(
    p('Description')
  )
)

viz_3_tab <- tabPanel(
  'Visualization 3',
  fluidPage(
    p('Description')
  )
)

conclusion_tab <- tabPanel(
  'Conclusion',
  fluidPage(
    p('More Description!')
  )
)

ui <- navbarPage(
  'Student Drinking - Final Project',
  intro_tab,
  viz_1_tab,
  viz_2_tab,
  viz_3_tab,
  conclusion_tab
)