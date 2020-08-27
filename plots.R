##############################
# Author: Mari Roberts
# Date: 8/03/2020
# Plots
##############################

# libraries
require(scales)
require(ggplot2)

# date
inmates$common_date <- as.Date(paste0(inmates$year,"-",format(inmates$date_admitted, "%j")), "%Y-%j")

# remove Native American and Unknown races
df <- inmates %>% filter(race != "Native American" & race != "Unknown")

# count admissions by date and race
df <- df %>%
  group_by(common_date, race) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))

# subset data by 8-26-2019 to 8-26-2020
df <- df %>% filter(common_date >= as.Date("2019-08-26"))

p <- ggplot(data = df,
            mapping = aes(x = common_date, y = count, shape = race, colour = race)) +
            geom_line() +
            facet_grid(facets = race ~ .) +
            theme(legend.position = "right",family = "Helvetica") + 
  theme_minimal() + labs(x = "\nDate Admitted", y = "Count\n") 

# time series plot over one year
p <- p + theme(plot.title = element_text(hjust = 0.5, family = "Helvetica", size=20)) + 
  ggtitle("Daily Inmates In Custody\n")+ 
  scale_x_date(labels = date_format("%b-%Y"), date_breaks = "1 month") +
  theme(
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18)
  ) 

# custody level
inter <- inmates %>%
  group_by(custody_level) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))
p <- ggplot(inter, aes(x = reorder(custody_level, count), y = count, fill="#3182bd")) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme_minimal() + 
  labs(y = "\nCount", x = "Custody Level\n", title = "Number of People in Custody by Custody Level") + 
  coord_flip() + theme(plot.title = element_text(hjust = 0.5, family = "Helvetica", size=18),
                       axis.title.x = element_text(size=18),
                       axis.title.y = element_text(size=18)) 
p <- p + geom_text(aes(label=count), position=position_dodge(width=0.9), hjust = -0.4, vjust = 0, family="Helvetica", colour = "#636363")
p + theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.position = "none")

# race
inter <- inmates %>%
  group_by(race) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))
p <- ggplot(inter, aes(x = reorder(race, count), y = count, fill="#3182bd")) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme_minimal() + 
  labs(y = "\nCount", x = "Race\n", title = "Number of People in Custody by Race") + 
  coord_flip() + theme(plot.title = element_text(hjust = 0.5, family = "Helvetica", size=20),
                       axis.title.x = element_text(size=18),
                       axis.title.y = element_text(size=18)) 
p <- p + geom_text(aes(label=count), position=position_dodge(width=0.9), hjust = -0.4, vjust = 0, family="Helvetica", colour = "#636363")
p + theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          legend.position = "none") 

# gender  
inter <- inmates %>%
  group_by(gender) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))
p <- ggplot(inter, aes(x = reorder(gender, count), y = count, fill="#3182bd")) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme_minimal() + 
  labs(y = "\nCount", x = "Gender\n", title = "Number of People in Custody by Gender") + 
  coord_flip() + theme(plot.title = element_text(hjust = 0.5, family = "Helvetica", size=18),
                       axis.title.x = element_text(size=18),
                       axis.title.y = element_text(size=18)) 
p <- p + geom_text(aes(label=count), position=position_dodge(width=0.9), hjust = -0.4, vjust = 0, family="Helvetica", colour = "#636363")
p + theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          legend.position = "none")

# age group  
inter <- inmates %>%
  group_by(agegroup) %>%
  summarise(count = n())
p <- ggplot(inter, aes(x = agegroup, y = count, fill="#3182bd")) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme_minimal() + 
  labs(y = "\nCount", x = "Age Group\n", title = "Number of People in Custody by Age Group") + 
  coord_flip() + theme(plot.title = element_text(hjust = 0.5, family = "Helvetica", size=18),
                       axis.title.x = element_text(size=18),
                       axis.title.y = element_text(size=18)) 
p <- p + geom_text(aes(label=count), position=position_dodge(width=0.9), hjust = -0.4, vjust = 0, family="Helvetica", colour = "#636363")
p + theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          legend.position = "none")
                                                                                                                                        