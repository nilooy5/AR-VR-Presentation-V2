library(readr)
library(plotrix)
library(ggplot2)
library(dplyr)
library(rstatix)
library(plotly)

Budget_review <- read_csv("Budget_review_2021-2022_Infrastructure_Investment_Program.csv")

#Checking missing value
sum(is.na(Budget_review))
# check type of data
str(Budget_review)
data <- Budget_review                   # Duplicate data frame
names(Budget_review)                    # Check column names

totalBudgetedFin <- data$`Total Budgeted Financing ($'000)`
domPriority <- data$`Wellbeing Domain/ Government Priority`
pclas <- data$`Project Classification`
ptyp <- data$`Project Type`

# USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
# USPersonalExpenditure
# data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

fig <- plot_ly(data, labels = ~domPriority, values = ~totalBudgetedFin, type = 'pie')
fig <- fig %>% layout(title = 'Government Priority by Total Budgeted Financing Till 2026',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# goals for their budget
fig
# make plotply bar chart
plot_ly(data, x = ~ptyp, y = ~totalBudgetedFin, type = 'bar')


# things they are working or will be working on
classificationOfProjects <- plot_ly(data, x = ~pclas, y = ~domPriority, type = 'scatter', mode = 'markers')
classificationOfProjects <- classificationOfProjects %>% layout(title = 'Fields of Expected outcomes',
                                xaxis = list(title = 'Project Classification'),
                                yaxis = list(title = 'Wellbeing Domain/ Government Priority'))
classificationOfProjects

plot_ly(data, labels = ~pclas, values = ~totalBudgetedFin, type = 'pie')
plot_ly(data, labels = ~ptyp, values = ~totalBudgetedFin, type = 'pie')
# order data by total budgeted financing in descending order
totalBudgetOrdered <- data[order(-data$`Total Budgeted Financing ($'000)`),]
top10 <- head(totalBudgetOrdered,10)
totalBudgetOrdered$`Total Budgeted Financing ($'000)`

# summerize data by project classification
sumByClass <- aggregate(totalBudgetOrdered$`Total Budgeted Financing ($'000)`, by=list(totalBudgetOrdered$`Project Classification`), FUN=sum)
sumByType <- aggregate(totalBudgetOrdered$`Total Budgeted Financing ($'000)`, by=list(totalBudgetOrdered$`Project Type`), FUN=sum)
sumByType
# aggregate multiple columns
budgetProgressPerType <- totalBudgetOrdered %>%
  group_by(`Project Type`) %>%
  summarise(
    "budget 2021-2022" = sum(`2021-2022 Budgeted Financing ($'000)`),
    "budget 2022-2023" = sum(`2022-2023 Budgeted Financing ($'000)`),
    "budget 2023-2024" = sum(`2023-2024 Budgeted Financing ($'000)`),
    "budget 2024-2025" = sum(`2024-2025 Budgeted Financing ($'000)`),
    "budget 2025-2026" = sum(`2025-2026 Budgeted Financing ($'000)`))

budgetProgressPerTypeWTotal <- totalBudgetOrdered %>%
  group_by(`Project Type`) %>%
  summarise(
    "budget 2021-2022" = sum(`2021-2022 Budgeted Financing ($'000)`),
    "budget 2022-2023" = sum(`2022-2023 Budgeted Financing ($'000)`),
    "budget 2023-2024" = sum(`2023-2024 Budgeted Financing ($'000)`),
    "budget 2024-2025" = sum(`2024-2025 Budgeted Financing ($'000)`),
    "budget 2025-2026" = sum(`2025-2026 Budgeted Financing ($'000)`),
    "total" = sum(`Total Budgeted Financing ($'000)`))

budgetProgressPerType
budgetProgressPerTypeWTotal
# budgetProgressPerTypeWTotal order by total
budgetProgressPerTypeWTotalOrdered <- budgetProgressPerTypeWTotal[order(-budgetProgressPerTypeWTotal$total),]
classesWithMostTotalBudget <- head(budgetProgressPerTypeWTotalOrdered, 5)
classesWithMostTotalBudget
budgetProgressPerType
# remove rows from budgetProgressPerType that are not in classesWithMostTotalBudget$`Project Type`
budgetProgressPerTypeTop5 <- budgetProgressPerType[(budgetProgressPerType$`Project Type` %in% classesWithMostTotalBudget$`Project Type`),]
budgetProgressPerTypeTop5

budgetProgressPerType %>%
  gather(key = "bud_year", value = "budget", -`Project Type`) %>%
  ggplot(aes(x = bud_year, y = budget, color = `Project Type`, group = `Project Type`)) +
  geom_line() +
  geom_point(size=2) +
  labs(title = "Budget Progress per Type", x = "Project Type", y = "Budget ($'000)")

# taking top 5 project types
budgetProgressPerTypeTop5 %>%
  gather(key = "bud_year", value = "budget", -`Project Type`) %>%
  ggplot(aes(x = bud_year, y = budget, color = `Project Type`, group = `Project Type`)) +
  geom_line(size=2) +
  geom_point(size=3) +
  # geom_smooth(method = "lm", se = FALSE, size = .5) +
  labs(title = "Budget till 2026", x = "Project Type", y = "Budget ($'000)")

# add trend line
# fig2 <- fig2 + geom_smooth(method = "lm", se = FALSE, color = "grey", size = 1)
fig2
budgetProgressPerType
# plot budget progress per type
budgetProgressPerType %>%
  ggplot(aes(x = `Project Type`, y = `budget 2021-2022`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Budget Progress per Type", x = "Project Type", y = "Budget ($'000)")
# plot budget progress per type
budgetProgressPerType %>%
  ggplot(aes(x = `Project Type`, y = `budget 2022-2023`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Budget Progress per Type", x = "Project Type", y = "Budget ($'000)")

# make barchart of top 10 projects
top10 %>%
  ggplot(aes(x = `Wellbeing Domain/ Government Priority`, y = `Total Budgeted Financing ($'000)`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Projects", x = "Project", y = "Budget ($'000)")


# make a plotly barchart of top 10 projects
top10 %>%
  plot_ly(x = ~`Wellbeing Domain/ Government Priority`, y = ~`Total Budgeted Financing ($'000)`, type = 'bar', orientation = 'h') %>%
  layout(title = 'Top 10 Projects', xaxis = list(title = 'Budget ($\'000)'), yaxis = list(title = 'Project'))
