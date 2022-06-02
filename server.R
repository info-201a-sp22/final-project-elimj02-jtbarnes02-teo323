library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library("tidyverse")

student_drinking_df <- read.csv("https://query.data.world/s/r47f3cqixzyczsmnwqbrqvu6fcgwh3", stringsAsFactors=FALSE)
student_drinking_df <- student_drinking_df %>% 
  mutate(Talc = Walc + Dalc)

# Visualization 1 Data Table
fam_relation_df <- student_drinking_df %>%
  select(sex, age, Pstatus, famrel, Talc)

fam_relation_df$Pstatus <- gsub("T", "Together", as.character(fam_relation_df$Pstatus))
fam_relation_df$Pstatus <- gsub("A", "Apart", as.character(fam_relation_df$Pstatus))

parents_status <- fam_relation_df %>%
  group_by(Pstatus, famrel) %>%
  summarize(alc_avg = mean(Talc))

# Visualization 2 Data Table
age_range_2 <- function(age) {
  age_15_17 <- "15 - 17"
  age_18_20 <- "18 - 20"
  age_21_22 <- "21 -22"
  ifelse(age >= 15 & age <= 17, age_15_17, ifelse(age >= 18 & age <= 20, age_18_20, ifelse(age >= 21, age_21_22, 0)))
}

student_drinking_2 <- student_drinking_df %>% 
  mutate(age_ranges = age_range_2(age))

#Visualization 3 data table 1
famrel_df <- student_drinking_df %>%
  select(Mjob, famrel, freetime, G3, Talc) %>% 
  group_by(Mjob) %>%
  summarise(avg_family_relationship = mean(famrel))

freetime_df <- student_drinking_df %>%
  select(Mjob, famrel, freetime, G3, Talc) %>% 
  group_by(Mjob) %>%
  summarise(avg_freetime = mean(freetime))

G3_df <- student_drinking_df %>%
  select(Mjob, famrel, freetime, G3, Talc) %>% 
  group_by(Mjob) %>%  
  summarise(avg_final_grades = mean(G3))

Talc_df <- student_drinking_df %>%
  select(Mjob, famrel, freetime, G3, Talc) %>% 
  group_by(Mjob) %>% 
  summarise(avg_drinking = mean(Talc)) 

mom_job_df <- left_join(famrel_df, freetime_df, by = 'Mjob')
mom_job_df <- left_join(mom_job_df, G3_df, by = 'Mjob')
mom_job_df <- left_join(mom_job_df, Talc_df, by = 'Mjob')

mom_job_df <- mom_job_df %>% 
  pivot_longer(!c(Mjob),
               names_to = "Student_Ratings",
               values_to = "Values")

mom_job_df$Mjob <- gsub("at_home", "At Home", as.character(mom_job_df$Mjob))
mom_job_df$Mjob <- gsub("health", "Health", as.character(mom_job_df$Mjob))
mom_job_df$Mjob <- gsub("other", "Other", as.character(mom_job_df$Mjob))
mom_job_df$Mjob <- gsub("services", "Services", as.character(mom_job_df$Mjob))
mom_job_df$Mjob <- gsub("teacher", "Teacher", as.character(mom_job_df$Mjob))
mom_job_df$Student_Ratings <- gsub("avg_family_relationship", "Avg Family Relationship", as.character(mom_job_df$Student_Ratings))
mom_job_df$Student_Ratings <- gsub("avg_freetime", "Avg Freetime", as.character(mom_job_df$Student_Ratings))
mom_job_df$Student_Ratings <- gsub("avg_final_grades", "Avg Final Grades", as.character(mom_job_df$Student_Ratings))
mom_job_df$Student_Ratings <- gsub("avg_drinking", "Avg Alcohol Consumption", as.character(mom_job_df$Student_Ratings))

#Visualization 3 data table 2
famrel_df <- student_drinking_df %>%
  select(Fjob, famrel, freetime, G3, Talc) %>% 
  group_by(Fjob) %>%
  summarise(avg_family_relationship = mean(famrel))

freetime_df <- student_drinking_df %>%
  select(Fjob, famrel, freetime, G3, Talc) %>% 
  group_by(Fjob) %>%
  summarise(avg_freetime = mean(freetime))

G3_df <- student_drinking_df %>%
  select(Fjob, famrel, freetime, G3, Talc) %>% 
  group_by(Fjob) %>%  
  summarise(avg_final_grades = mean(G3))

Talc_df <- student_drinking_df %>%
  select(Fjob, famrel, freetime, G3, Talc) %>% 
  group_by(Fjob) %>% 
  summarise(avg_drinking = mean(Talc)) 

dad_job_df <- left_join(famrel_df, freetime_df, by = 'Fjob')
dad_job_df <- left_join(dad_job_df, G3_df, by = 'Fjob')
dad_job_df <- left_join(dad_job_df, Talc_df, by = 'Fjob')

dad_job_df <- dad_job_df %>% 
  pivot_longer(!c(Fjob),
               names_to = "Student_Ratings",
               values_to = "Values")

dad_job_df$Fjob <- gsub("at_home", "At Home", as.character(dad_job_df$Fjob))
dad_job_df$Fjob <- gsub("health", "Health", as.character(dad_job_df$Fjob))
dad_job_df$Fjob <- gsub("other", "Other", as.character(dad_job_df$Fjob))
dad_job_df$Fjob <- gsub("services", "Services", as.character(dad_job_df$Fjob))
dad_job_df$Fjob <- gsub("teacher", "Teacher", as.character(dad_job_df$Fjob))
dad_job_df$Student_Ratings <- gsub("avg_family_relationship", "Avg Family Relationship", as.character(dad_job_df$Student_Ratings))
dad_job_df$Student_Ratings <- gsub("avg_freetime", "Avg Freetime", as.character(dad_job_df$Student_Ratings))
dad_job_df$Student_Ratings <- gsub("avg_final_grades", "Avg Final Grades", as.character(dad_job_df$Student_Ratings))
dad_job_df$Student_Ratings <- gsub("avg_drinking", "Avg Alcohol Consumption", as.character(dad_job_df$Student_Ratings))



server <- function(input, output) {
  
#Visualization 1
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
  
#Visualization 2
  output$age_plot <- renderPlotly({
      
      age_filtered_df <- student_drinking_2 %>%
        filter(age_ranges %in% input$age_selection)
      
      age_range_plot <- ggplot(data = age_filtered_df) + 
        geom_point(mapping = aes(x = Talc, y = G3, color = age_ranges)) +
        ylim(0, 20) +
        xlim(0, 10) +
        labs(title = "Alcohol Consumption vs. Grades by Age",
             x = "Weekly Alcohol Consumption Scale (1-10)",
             y = "Grades",
             color = "Age")
      age_range_plot
  })
  
#Visualization 3
  output$mother_job_plot <- renderPlotly({
    
    mom_plot_selected_df <- mom_job_df %>%
      filter(Mjob %in% input$m_user_selection)
    
    
    mother_job_plot <- ggplot(data = mom_plot_selected_df) + 
      geom_bar(mapping = aes(x = Student_Ratings, 
                             y = Values, 
                             fill = Student_Ratings), stat = 'identity'
      ) +
      ylim(0,12.5) +
      labs(title = "Student Survey Results Based On Mother's Occupation",
           x = "Survey Questions",
           y = "Average Student Response",
           fill = "Categories")
    theme_classic()
    return(mother_job_plot)
  })
  
  output$father_job_plot <- renderPlotly({
    
    dad_plot_selected_df <- dad_job_df %>%
      filter(Fjob %in% input$f_user_selection)
    
    
    father_job_plot <- ggplot(data = dad_plot_selected_df) + 
      geom_bar(mapping = aes(x = Student_Ratings, 
                             y = Values, 
                             fill = Student_Ratings), stat = 'identity'
      ) +
      ylim(0,12.5) +
      labs(title = "Student Survey Results Based On Father's Occupation",
           x = "Survey Questions",
           y = "Average Student Response",
           fill = "Categories")
    theme_classic()
    return(father_job_plot)
  })
  
  
  
}