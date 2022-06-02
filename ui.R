library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library("tidyverse")
library(bslib)

student_drinking_df <- read.csv("https://query.data.world/s/r47f3cqixzyczsmnwqbrqvu6fcgwh3", stringsAsFactors=FALSE)
student_drinking_df <- student_drinking_df %>% 
  mutate(Talc = Walc + Dalc)

#Visualization 1 Data Table
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


intro_tab <- tabPanel(
  'Introduction',
  fluidPage(
    h1('Introduction'),
    p("Underage drinking has always been a major problem in America and most countries. While many other countries have different drinking age limit restrictions, drinking at a young age can lead to unwanted effects, whether it be legal or not. Therefore, our project focuses on the consumption of alcohol of students in secondary school in Portugal and how it affects their academic performance. By analyzing this data and the impact it has on students’ grades, we hope to come to a significant conclusion that can be generalized to the world population. "),
    p("Our main research question focuses on the correlation between the academic performance and alcohol consumption. This focuses on students’ finals grades on a 1-20 scale and the amount that they drink each week on a scale of 1-5.  Another secondary research question we are studying is how a student's home and family situation affects their drinking habits. This includes factors such as whether their parents are together or apart, if their parents are their guardian or somebody else, the education level and occupation of their parents, and if they have internet access at home. The reason for looking at this is to see if living in a less stable home situation will lead a student to drinking more."),
    p("We found the data through the Google Data Sets. Through researching the data we found the link to data world with the corresponding article titled" , a(href = "https://data.world/data-society/student-alcohol-consumption", "Student Alcohol Consumption"), "This data was collected by P. Cortez and A. Silva. To collect this data 382 students in secondary school in Portugal were surveyed in their math class with questions concerning social, gender and study information about students. This data was originally collected to discover a correlation between drinking from the age of 15 to 22 and the reasons behind it and how it affects their grades in math in Portugal. There are 395 observations and 33 features in the data."),
    p("A main ethical question and problem with the data we considered was how outside factors contribute to the students’ grades or alcohol consumption. We did not want to make any biased assumptions concerning a specific students’ grades and their reason for drinking. While there is most likely a correlation between drinking and grades, their decrease in grades could be attributed to the factors which are demonstrated in the dataset. When addressing the limitations, we found multiple issues that could affect our conclusion. Our project aims to identify possible contributing factors to grade differences correlated with alcohol use, yet we won’t be able to identify factors beyond what we have data for. For example, we might be able to find a connection to parent marital status differences because we have data about the students’ parents’ marital status, but these connections are mostly limited to what we have specific data for. Another limitation is that we only have data for students’ math class grades, so we have to make the assumption that student’s trends in math scores reflects their performance in school as a whole. While this will often likely be the case, there will undoubtedly be examples of students whose math grade is a poor indicator of their overall performance in school. Another challenge for our project is that we’re assuming the precedence that the grades were collected after the alcohol consumption. There could be scenarios where students turn to drinking because of their grade results. However, the data on alcohol consumption was based on habits in the last 30 days, with grades at different points throughout the grading period, which will help us avoid our precedence assumption. The final limitation of this data is the possibility of lying. While Portugal has less strict rules on consuming alcohol, it is still illegal for kids to consume alcohol under 18 which is most of the students in this data set. Many of these students could be lying about their consumption. In the survey, the students are asked about their drinking habits on a scale from 1 to 5. This is asked for both weekdays and weekends. While the survey is anonymous, many students may be wary of admitting their true drinking habits and do not wish to disclose it so they may have input a smaller number on the scale. This could skew the data and ruin the correlation between the level of alcohol consumption and how it affects their grades."),
    img(src = "https://static.dw.com/image/18396896_401.jpg", height = 432, width = 768),
    img(src = "https://lv7ms1pq6dm2sea8j1mrajzw-wpengine.netdna-ssl.com/wp-content/uploads/2019/08/bad-grade-scaled-e1597057096395.jpg", height = 432, width = 768),
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

age_filter <- sidebarPanel(
  selectInput(
    inputId = "age_selection",
    label = "Select age groups:",
    choices = student_drinking_2$age_ranges,
    multiple = F,
    selected = ""
  )
)

age_plot <- mainPanel(
  plotlyOutput(outputId = "age_plot")
)

viz_1_tab <- tabPanel(
  'Visualization 1',
  sidebarLayout(
    sidebarPanel(scale_widget),
    main_panel_plot),
  fluidPage(
    p('For visualization 1 we wanted to address one of our questions regarding what leads these Portuguese teenagers to drink. We understand the effect a student’s family life can have on their disposition to drink so this plot was created to determine how their family relationship and parents’ marital status affected their drinking habits. When investigating this plot, you can use the widget to change the family relationship scale from 1-5, 1 being low and 5 being high, to change the graph to demonstrate the direct relationship between parents’ relationship status and the child’s drinking habits throughout the week on a 1-10 scale respectively. After looking at the plot, one can tell that the more a student’s family relationship increases, their drinking scale goes down. Additionally, when a student has a low family relationship and separated parents, they have the highest disposition to drink. Adversely, when the student has a family relationship scale from 2 and 3 then if their parents are together, they are more disposed to drink. Lastly at a family relationship scale of 4 and 5 then both drinking scales are similar and the lowest.')
  )
)

viz_2_tab <- tabPanel(
  'Visualization 2',
  age_filter,
    age_plot,
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