library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library("tidyverse")
library(bslib)

theme <- bs_theme(bg = "#528AAE",
                  fg = "white",
                  primary = "#1E3F66",
                  base_font = font_google("Bree Serif"),
                  code_font = font_google("Bree Serif")
                  )

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

# Visualization 3 Data Table

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

#Visualization 1 widget
scale_widget <- selectInput(
  inputId = "scale_select",
  label = "Family Relationship Scale",
  choices = parents_status$famrel,
  multiple = F,
  selected = ""
)

main_panel_plot <- mainPanel(
  plotlyOutput(outputId = "fam_relation_plot")
)

#Visualization 2 widget
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

#Visualization 3 widget

viz_1_tab <- tabPanel(
  "Family's Effect on Drinking",
  sidebarLayout(
    sidebarPanel(scale_widget),
    main_panel_plot),
  fluidPage(
    p('For visualization 1 we wanted to address one of our questions regarding what leads these Portuguese teenagers to drink. We understand the effect a student’s family life can have on their disposition to drink so this plot was created to determine how their family relationship and parents’ marital status affected their drinking habits. When investigating this plot, you can use the widget to change the family relationship scale from 1-5, 1 being low and 5 being high, to change the graph to demonstrate the direct relationship between parents’ relationship status and the child’s drinking habits throughout the week on a 1-10 scale respectively. After looking at the plot, one can tell that the more a student’s family relationship increases, their drinking scale goes down. Additionally, when a student has a low family relationship and separated parents, they have the highest disposition to drink. Adversely, when the student has a family relationship scale from 2 and 3 then if their parents are together, they are more disposed to drink. Lastly at a family relationship scale of 4 and 5 then both drinking scales are similar and the lowest.')
  )
)

viz_2_tab <- tabPanel(
  'Drinking Impact on Grades',
  age_filter,
    age_plot,
   fluidPage(
    p('Our second visualization aims to examine the correlation between alcohol consumption by Portuguese students and their grades, while comparing different age groups. Grades at the schools this data was collected from was scaled to a 0 through 20 scale. The weekly alcohol consumption levels are on a 1 to 10 scale with 1 being the least frequent and 10 being the most frequent. By navigating through the age group options, users can display groups including ages 15 to 17, 18 to 20, and 21 to 22. Unfortunately, the data for the 21 to 22 age group has extremely minimal data, so no "real" conclusions can be drawn, but certain assumptions can hopefully be made based on the existing data.')
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
    h1("Conclusion"),
    p("One question we set out to answer in our project is how outside factors, especially family life, can affect a student or teenager’s disposition to drink in Portugal. To answer this question, we created our first visualization focusing specifically on the effect that parental status and the family relationship have on a student’s drinking habit. By comparing if a student’s parents were together or apart and their family relationship on a scale of 1-5, we were able to directly see how that impacted each student’s average drinking habits on a scale of 1-10. By looking at this plot we were able to answer our original research question and discover that family relationship plays a major part in a student’s disposition to drink and their drinking habits. In the visualization, it is objectively clear that as the scale increases from 1, being the worst relationship, to 5, the best relationship, the average drinking scale goes down, especially for parents that are separated. For example, when a student has a 1-family relationship and separated parents, their drinking is 6.5/10 on the scale, but if they have a 5-family relationship it drops to a 3.5/10. In addition, the high number of students with parents that are together came in at 4.5/10 on a family relationship scale of 2, but as the family relationship scale increased to 5, it was lowered to a 3.5/10. In addition, we found that if parents were separated then students would drink 4.2/10 on the scale vs. together at 3.9/10 on the scale. While this is not a major difference, we still found it to fit within our hypothesis and give us a good enough idea to come to a significant conclusion about Portuguese students that can be applicable to students around the world."),
    p("")
  )
)

ui <- navbarPage(
  theme = theme,
  'Student Drinking - Final Project',
  intro_tab,
  viz_1_tab,
  viz_2_tab,
  viz_3_tab,
  conclusion_tab
)