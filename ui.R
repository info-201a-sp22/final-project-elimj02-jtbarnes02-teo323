library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library("tidyverse")
library(bslib)

student_drinking_df <- read.csv("https://query.data.world/s/r47f3cqixzyczsmnwqbrqvu6fcgwh3", stringsAsFactors=FALSE)
student_drinking_df <- student_drinking_df %>% 
  mutate(Talc = Walc + Dalc)


intro_tab <- tabPanel(
  'Introduction',
  fluidPage(
    h1('Introduction')
  )
)

viz_1_tab <- tabPanel(
  'Visualization 1',
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