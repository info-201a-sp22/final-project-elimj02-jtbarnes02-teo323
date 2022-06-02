# The Affects of Student Drinking On Grades in Portugal
## INFO 201 "Foundational Skills for Data Science" — Spring 2022

Authors: Teo Koulias, Eli Jones, Jared Barnes

Link: (https://elimj02.shinyapps.io/final-project-elimj02/))

# Introduction
Underage drinking has always been a major problem in America and most countries. While many other countries have different drinking age limit restrictions, drinking at a young age can lead to unwanted effects, whether it be legal or not. Therefore, our project focuses on the consumption of alcohol of students in secondary school in Portugal and how it affects their academic performance. By analyzing this data and the impact it has on students’ grades, we hope to come to a significant conclusion that can be generalized to the world population.

Our main research question focuses on the correlation between the academic performance and alcohol consumption. This focuses on students’ finals grades on a 1-20 scale and the amount that they drink each week on a scale of 1-5. Another secondary research question we are studying is how a student's home and family situation affects their drinking habits. This includes factors such as whether their parents are together or apart, if their parents are their guardian or somebody else, the education level and occupation of their parents, and if they have internet access at home. The reason for looking at this is to see if living in a less stable home situation will lead a student to drinking more.

We found the data through the Google Data Sets. Through researching the data we found the link to data world with the corresponding article titled ["Student Alcohol Consumption."](https://data.world/data-society/student-alcohol-consumption) This data was collected by P. Cortez and A. Silva. To collect this data 382 students in secondary school in Portugal were surveyed in their math class with questions concerning social, gender and study information about students. This data was originally collected to discover a correlation between drinking from the age of 15 to 22 and the reasons behind it and how it affects their grades in math in Portugal. There are 395 observations and 33 features in the data.

A main ethical question and problem with the data we considered was how outside factors contribute to the students’ grades or alcohol consumption. We did not want to make any biased assumptions concerning a specific students’ grades and their reason for drinking. While there is most likely a correlation between drinking and grades, their decrease in grades could be attributed to the factors which are demonstrated in the dataset. When addressing the limitations, we found multiple issues that could affect our conclusion. Our project aims to identify possible contributing factors to grade differences correlated with alcohol use, yet we won’t be able to identify factors beyond what we have data for. For example, we might be able to find a connection to parent marital status differences because we have data about the students’ parents’ marital status, but these connections are mostly limited to what we have specific data for. Another limitation is that we only have data for students’ math class grades, so we have to make the assumption that student’s trends in math scores reflects their performance in school as a whole. While this will often likely be the case, there will undoubtedly be examples of students whose math grade is a poor indicator of their overall performance in school. Another challenge for our project is that we’re assuming the precedence that the grades were collected after the alcohol consumption. There could be scenarios where students turn to drinking because of their grade results. However, the data on alcohol consumption was based on habits in the last 30 days, with grades at different points throughout the grading period, which will help us avoid our precedence assumption. The final limitation of this data is the possibility of lying. While Portugal has less strict rules on consuming alcohol, it is still illegal for kids to consume alcohol under 18 which is most of the students in this data set. Many of these students could be lying about their consumption. In the survey, the students are asked about their drinking habits on a scale from 1 to 5. This is asked for both weekdays and weekends. While the survey is anonymous, many students may be wary of admitting their true drinking habits and do not wish to disclose it so they may have input a smaller number on the scale. This could skew the data and ruin the correlation between the level of alcohol consumption and how it affects their grades.


# Conclusion / Summary Takeaways
One question we set out to answer in our project is how outside factors, especially family life, can affect a student or teenager’s disposition to drink in Portugal. To answer this question, we created our first visualization focusing specifically on the effect that parental status and the family relationship have on a student’s drinking habit. By comparing if a student’s parents were together or apart and their family relationship on a scale of 1-5, we were able to directly see how that impacted each student’s average drinking habits on a scale of 1-10. By looking at this plot we were able to answer our original research question and discover that family relationship plays a major part in a student’s disposition to drink and their drinking habits. In the visualization, it is objectively clear that as the scale increases from 1, being the worst relationship, to 5, the best relationship, the average drinking scale goes down, especially for parents that are separated. For example, when a student has a 1-family relationship and separated parents, their drinking is 6.5/10 on the scale, but if they have a 5-family relationship it drops to a 3.5/10. In addition, the high number of students with parents that are together came in at 4.5/10 on a family relationship scale of 2, but as the family relationship scale increased to 5, it was lowered to a 3.5/10. In addition, we found that if parents were separated then students would drink 4.2/10 on the scale vs. together at 3.9/10 on the scale. While this is not a major difference, we still found it to fit within our hypothesis and give us a good enough idea to come to a significant conclusion about Portuguese students that can be applicable to students around the world.

Another key takeaway is that increased alcohol consumption correlates to lower grades across age groups with Portuguese students. This is represented in the second visualization that compares alcohol consumption to grades with the ability to change the age group. In both the 15 to 17 age group and the 18 to 20 age group, there is a clear negative correlation between alcohol consumption levels and grades, meaning as drinking increases, grades fall. In both age groups, there are several outliers that received overall grades of “0,” despite what their alcohol consumption levels were. This could be explained by a number of unknown factors such as dropping out of school, but these shouldn’t seriously impact the correlation between drinking and grades. The 20 to 22 age group had very insufficient data, but based on this minimal data, one could make an argument that the impact of drinking on grades becomes less significant for older ages because of the lack of any negative correlation shown for this age group. However, this cannot truly be assumed, and we don’t recommend using this age group for any implications.

When looking at average alcohol consumption, there is little correlation for mother's occupation, as the range between the lowest and highest average was only 0.23. However for fathers the correlation is much larger, as students with fathers that work in services or other jobs are much more likely to drink on average than other father occupations. These types of students are also much more likely to have lower final grades than students who's fathers work as teachers or in health. Looking at the mothers chart, students with a stay at home mom or other job have much lower final grades than other mother occupations. As for family relationship and freetime, there doesn't seem to be any correlation between a parents occupation and those average values. There also doesn't appear to be any correlation between the average value for family relationship or freetime when compared to alcohol consumption or final grades in either parent graph

From our collective analysis, the most important insight that was discovered was that having a weaker family relationship can increase the likelihood of increased alcohol consumption habits in Portuguese students, which in turn, can result in lower grades for these students. This shows how while drinking may have a negative impact on Portuguese students’ grades, these drinking habits could be more deeply rooted in students’ weak relationships with their families and their parents reltionships as well.

After coming to our final insight on this project we discovered the importance of family life and how it can go on to affect Portuguese students’ drinking and grades. While the study only focused on Portuguese students, we felt that this data was significant enough to be able to generalize to the global population of teenagers and create broader implications. Now that we understand the importance of family life on drinking, realize the underlying factors that contribute to students drinking habits. The broader implications of this could help to create better support systems in school for students that do have a weaker family life to help them succeed in class. In addition, more emphasis could be placed on a student’s family life to help combat drinking and more systems could be put in place to help try and rectify that relationship if possible. By successfully eliminating the source of students drinking habits, we can lower the scale of drinking in teenagers and students thus improving grades in the long term.
