#Reading in the data
#replacing any posible blank entries as 'NA'.
storyteller<- read.csv("C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST707\\Homework\\data_storyteller.csv")

#checking data types to see what may need changing
str(storyteller)

#changing "school" & "section" columns from chracter type to factor
storyteller$School<- factor(storyteller$School)
storyteller$Section<- factor(storyteller$Section)

#Data Cleaning
#changing remaining columns to numeric values
storyteller$Very.Ahead..5<- as.integer(storyteller$Very.Ahead..5)
storyteller$Middling..0<- as.integer(storyteller$Middling..0)
storyteller$Behind..1.5<- as.integer(storyteller$Behind..1.5)
storyteller$More.Behind..6.10<- as.integer(storyteller$More.Behind..6.10)
storyteller$Very.Behind..11<- as.integer(storyteller$Very.Behind..11)
storyteller$Completed<- as.integer(storyteller$Completed)

#changing column names
colnames(storyteller)<- c('Section', 'School', 'Completed', 'Very Ahead 5+', 'Middling +0', 'Behind -1-5', 'More Behind -6-10', 'Very Behind -11')

#Organizing the Data Structure
#Reorganizing columns to get a cleaner picture.
storytellerTemp<- storyteller[,c(2,1,8,3,4,5,6,7)]
storyteller<-storytellerTemp
head(storyteller)

#Missing Data
#Checking for any NA values
sum(is.na(storyteller))
#There are no NA values in dataset. 
#The dataset is cleaned. 
head(storyteller)

#Exploratoty Data Analysis and DAta Viz
#Observation: To be considered 'ahead' in any way, you must be 'Very Ahead' and 
# 'Complete'. 
#There are 3 categories that describe 'behind'. Those are 'behind', 
# 'more behind', and 'very behind'. 

#Creating a bar chart to show the number of sections from each school. 
SchoolValues<- c(length(which(storyteller$School == 'A')), length(which(storyteller$School == 'B')), length(which(storyteller$School == 'C')), length(which(storyteller$School == 'D')), length(which(storyteller$School == 'E'))) 
barplot(SchoolValues)
barplot(SchoolValues, names.arg = c('A','B','C','D','E'), main='Number of Sections per School')

#plotting Section and Completed and summarizing the data
barplot(storyteller$Completed, main='#completed students / section', names.arg = c(1:30))
summary(storyteller$Completed)

#Further Data Cleaning based on EDA and Viz
#plotting Aection and Very Ahead and Summarizing
barplot(storyteller$`Very Ahead 5+`, main='Number of Very Ahead Students per Section', names.arg = c(1:30))
summary(storyteller$`Very Ahead 5+`)

#plotting Section and Middling and summarizing
barplot(storyteller$`Middling +0`, main='Number of Middling Students per Section', names.arg = c(1:30))
summary(storyteller$`Middling +0`)

#plotting Section and Behind and summarizing
barplot(storyteller$`Behind -1-5`, main='Number of Behind Students per Section', names.arg = c(1:30))
summary(storyteller$`Behind -1-5`)

#plotting Section and More Behind and summarizing
barplot(storyteller$`More Behind -6-10`, main='Number of More Behind Students per Section', names.arg = c(1:30))
summary(storyteller$`More Behind -6-10`)

#plotting Section and Very Behind and summarizing
barplot(storyteller$`Very Behind -11`, main='Number of Very Behind Students per Section', names.arg = c(1:30))
summary(storyteller$`Very Behind -11`)

#determing the number of students in each Category
StudentSums<- colSums(storyteller[,3:8])
sum(StudentSums)

#determing the amount of students in each Category
data.frame(StudentSums)

#determing the amount of students in each Section
StudentSection<- rowSums(storyteller[,3:8])
sum(StudentSection)
data.frame(StudentSection)

#creating a barplot to show distribution
StudentSums<- colSums(storyteller[,3:8])
barplot(StudentSums, main="Student Totals Across all Categories")

#Note: There is a gap between 'Middling' and 'Completed'. This means that those 
# students whom are deemed ahead have completed the course. 
# There is not a category 'Ahead', where many students may fall. This could 
# by skewing it, but it could make the data more accurate. 
# Also, most students are recorded in the "Behind' category.

#EDA continued
StudentSums/sum(StudentSums)
#Note: 19% of students have completed the course. 14% are middling. 47% are 
# behind. 6% are more behind. 13% are very behind. Thus 66% of students are
# behind. 

plot(storyteller$Completed, storyteller$`Middling +0`)
# ALthough 66% are behind on their course, there is still no quantitative 
# clarity on the amount of students are not doing well. We know that 20% of 
# students are ahead/completed, but how many is that?

#determining the number of students are in the categories per school
storytellerA<- storyteller[which(storyteller$School == 'A'),]
storytellerB<- storyteller[which(storyteller$School == 'B'),]
storytellerC<- storyteller[which(storyteller$School == 'C'),]
storytellerD<- storyteller[which(storyteller$School == 'D'),]
storytellerE<- storyteller[which(storyteller$School == 'E'),]

#plotting and summarizing School A
StudentSumsA<- colSums(storytellerA[3:8])
data.frame(StudentSumsA)
barplot(StudentSumsA, main = "School A")

#plotting and summarizing School B
StudentSumsB<- colSums(storytellerB[3:8])
data.frame(StudentSumsB)
barplot(StudentSumsB, main = "School B")

#plotting and summarizing School C
StudentSumsC<- colSums(storytellerC[3:8])
data.frame(StudentSums)
barplot(StudentSumsC, main = "School C")

#plotting and summarizing School D
StudentSumsD<- colSums(storytellerD[3:8])
data.frame(StudentSumsD)
barplot(StudentSumsD, main = "School D")

#plotting and summarizing School E
StudentSumsE<- colSums(storytellerE[3:8])
data.frame(StudentSumsB)
barplot(StudentSumsE, main = "School E")

#Initial Observations and Remarks
#Observe:
# Barplots of Schools B and D do not visually resemble the orginal barplot 
# with data from all school. 
# Barplots of Schools A, C, and E resemble the original barplot. 
# Most of the students qualify for the 'Behind' category, and the fewest are in 
# the "More Behind' and 'Very Behind' categories. 
# SChool D demonstrates that most students are in the 'Behind' category, and 
# many are in the 'Very Behind' category. The amount of students in the
# 'Completed' and 'Middling' are about the same (+/-1), and the fewest students 
# are in the 'More Behind' category. 

#Comparing the Schools B and D as a representation of the program takers.
sum(StudentSumsB)/sum(StudentSums)
sum(StudentSumsD)/sum(StudentSums)

#School D makes up 1.4% of the students in the program, meaning that this school
# is not a good representation of of how difficult the prgram may be. However, 
# school B makes 27.0% (over a quarter!) of the students in the program. Thus,
# this school is a better representation. Aaditionally, School B has better 
# demonstrated its ability to get students to complete the program.

#determining the quality of the sections at School B
barplot(storytellerB$Completed, names.arg = c(1:12), ylim=c(0,30), main = "Students Completed by Section")
#Section 2 has the lowest quality rating, meaning that students will have the
# hardest time completing this section. However, students have the highest 
# chance of successfuly completing sections 6 and 12. 

#plotting students that are behind at School B by section
barplot(storytellerB$`Behind -1-5`, storyteller$`Very Behind -11`, names.arg = c(1:12), ylim=c(0,30), main = "School B Students Behind by Section")
