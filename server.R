library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library("tidyverse")

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

server <- function(input, output) {
  
  output$fam_relation_plot <- renderPlotly({
    
    fam_relation_df <- parents_status %>%
      filter(famrel %in% input$scale_select)
    
    fam_relation_plot <- ggplot(data = fam_relation_df) + 
      geom_bar(mapping = aes(x = Pstatus, y = alc_avg, fill = Pstatus), stat='identity') +
      ylim(0, 10) +
      labs(title = "How Family Relations Impact Drinking",
           x = "Parent Marital Status",
           y = "Weekly Alcohol Consumption Scale (1-10)")
    
    return(fam_relation_plot)
  })
  
}