# Solution

# Activate the various package used for data importing, analysis and visualization
library(readxl)
library(dplyr)
library(ggplot2)
library(forcats)

# Import the data from the source. The dataset used for this project is an excel file, hence we use the 
# read_xlsx function from the readxl package
Employees_Details <- readxl::read_xlsx("Congos.xlsx")
View(Employees_Details)

# Question 1
dim(Employees_Details)
# There are 606 rows and 12 columns

# Question 2
Job_Roles <- select(Employees_Details,`Job Title`) %>% distinct(`Job Title`) %>% arrange(`Job Title`)
View(Job_Roles)
# There are 50 distinct job roles some of them include Data scientist, Analytical engineer, AI Scientist e.t.c

# Question 3
Range <- select(Employees_Details, Salary) %>% range(Employees_Details$Salary)
print(Range)
# The range of salaries are from 4,000 to 30,000,000

# Question 4
# For average salary in each job roles
Average <- Employees_Details %>% group_by(`Job Title`) %>% summarise(Average_Salary = mean(Salary))
View(Average)

# For median salary in each job roles
Median_Salary <- Employees_Details %>% group_by(`Job Title`) %>% summarise(Median_Pay = median(Salary))
View(Median_Salary)

# The get the standard deviation in salary for each job roled
Standard_Deviation <- Employees_Details %>% group_by(`Job Title`) %>%
  summarise(St_Deviation = sd(Salary))
View(Standard_Deviation)

# Question 5
# For lowest salaries for each jobs, we say
Lowest_Pay <- Employees_Details %>% group_by(`Job Title`) %>% summarise(Lowest_Payment = min(Salary))
View(Lowest_Pay)

# For highest salaries for each jobs, we say
Highest_Pay <- Employees_Details %>% group_by(`Job Title`) %>% summarise(Highest_Payment = max(Salary))
View(Highest_Pay)

# Question 6
# Total summary
Data_Summary <- Employees_Details %>%  group_by(`Job Title`) %>% summarise(count = n(),
                                                                           Total_Average = mean(Salary),
                                                                           Total_Median = median(Salary),
                                                                           Total_Standard = sd(Salary))
View(Data_Summary)

# Question 7
# For year 2020
Trends_2020 <- Trends_Salaries %>% filter(Trends_Salaries$`Work Year` == 2020) %>%
  group_by(`Job Title`) %>% summarise(Total = mean(Salary))

# For proper arrangement using the fct_order function in the forcats package
Trends_2020 <- Trends_2020 %>% mutate(`Job Title` = fct_reorder(`Job Title`, Total))

# Visualization using ggplot2
ggplot(Trends_2020, aes(x = Total, y = reorder(`Job Title`, Total))) +
  geom_bar(stat = "identity", fill = "black") +
  labs(title = "Total Salaries by Job Title (2020)", x = "Total Salary", y = "Job Title") +
  theme_minimal()

# For year 2021
Trends_2021 <- Trends_Salaries %>% filter(Trends_Salaries$`Work Year` == 2021) %>%
  group_by(`Job Title`) %>% summarise(Total = mean(Salary))

# For proper arrangement using the fct_order function in the forcats package
Trends_2021 <- Trends_2021 %>% mutate(`Job Title` = fct_reorder(`Job Title`, Total))

# Visualization using ggplot2
ggplot(data = Trends_2021, mapping = aes(x = Total, y = `Job Title`)) + geom_bar(stat = "identity",
                                                                                 fill = "brown",
                                                                                 color = "brown",
                                                                                 width = 0.8) +
  labs(title = "Job trends 2021", x = "Average salary", y = "Job title") + theme_minimal()

# For year 2022
Trends_2022 <- Trends_Salaries %>% filter(Trends_Salaries$`Work Year` == 2022) %>%
  group_by(`Job Title`) %>% summarise(Total = mean(Salary))

# For proper arrangement
Trends_2022 <- Trends_2022 %>% mutate(`Job Title` = fct_reorder(`Job Title`, Total))

# Visualization using ggplot2
ggplot(data = Trends_2022, mapping = aes(x = Total, y = `Job Title`)) + geom_bar(stat = "identity",
                                                                                 fill = "purple",
                                                                                 color = "purple",
                                                                                 width = 0.8) +
  labs(title = "Job trends 2022", x = "Average salary", y = "Job title") + theme_minimal()

# Question 8
# Firstly lets select the required variables for this analysis
Barchart_Visuals <- Employees_Details %>% group_by(`Job Title`) %>% summarise(Average = round(mean(Salary),1))
View(Barchart_Visuals) 

# Using fct_order from the FORCATS package for proper arrangement when visualizing
Barchart_Visuals <- Barchart_Visuals %>% mutate(`Job Title`= fct_reorder(`Job Title`,Average))

# Visualization using the ggplot package
ggplot(Barchart_Visuals, aes(x = Average, y = `Job Title`)) + geom_bar(stat = "identity",
                                                                       fill = "yellow",
                                                                       color = "black",
                                                                       width = 1) +
  labs(title = "Average salaries for each job roles", x = "Average", y = "`Job Title`") + 
  theme_minimal()

# Question 9
Employee_Plots <- Employees_Details %>% count(`Job Title`)
ggplot(data = Employee_Plots, mapping = aes(fill =`Job Title`, area = n, 
                                            label = `Job Title`)) + geom_treemap() +
  geom_treemap_text(colour = "black", place = "centre" ) + labs(title = "Each jobs count") +
  theme(legend.position = "none")
