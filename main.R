# import necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

#read data from csv file
student_habits_performance <- read.csv(directory)

#create categories from exam score
student_habits_performance <- student_habits_performance %>%
  mutate(exam_score_group = case_when(
    exam_score <= 50 ~ "0-50",
    exam_score <= 60 ~ "51-60",
    exam_score <= 70 ~ "61-70",
    exam_score <= 80 ~ "71-80",
    exam_score <= 90 ~ "81-90",
    exam_score > 90 ~ "91-100"
  ))

#calculate avg hours per category
average_hours_per_group <- student_habits_performance %>%
  group_by(exam_score_group) %>%
  summarise(
    avg_study_hours = mean(study_hours_per_day),
    avg_sleep_hours = mean(sleep_hours),
    avg_netflix_hours = mean(netflix_hours),
    avg_social_media_hours = mean(social_media_hours)
  ) %>%
  mutate(
    other_hours = 24 - (avg_study_hours + avg_sleep_hours + avg_netflix_hours + avg_social_media_hours)
  )

#create long format for plotting
average_hours_long <- average_hours_per_group %>%
  gather(key = "Category", value = "Average_Hours", 
         avg_study_hours, avg_sleep_hours, avg_netflix_hours, avg_social_media_hours, other_hours)

#order categories
average_hours_long$Category <- factor(average_hours_long$Category,
                                      levels = c("other_hours", "avg_study_hours","avg_social_media_hours", "avg_netflix_hours", "avg_sleep_hours"))

# plot
ggplot() +
  
  #plot bars
  geom_bar(data = average_hours_long, aes(x = exam_score_group, y = Average_Hours, fill = Category),
           stat = "identity", position = "stack", show.legend = TRUE) +
  
  #plot trendline
  geom_line(data = average_hours_per_group, aes(x = exam_score_group, y = 24 - other_hours, group = 1),
            color = "darkgreen", size = 0.95, linetype = "dashed") +
  
  #adjust the titles
  labs(
    title = "Average Day of Student by Exam Score",
    subtitle = "Study, Sleep, Netflix, Social Media and Other Activities",
    x = "Exam Score (%)",
    y = "Average Hours per Day",
    fill = "Activity Category"
  ) +
  
  #adjust colors and labels
  scale_fill_manual(
    values = c(
      "avg_study_hours" = alpha("green", 1),
      "avg_sleep_hours" = alpha("blue", 0.4),
      "avg_social_media_hours" = alpha("darkorange", 0.4),
      "avg_netflix_hours" = alpha("red", 0.4),
      "other_hours" = alpha("gray70", 0.6)
    ),
    labels = c(
      "avg_study_hours" = "Study Time",
      "avg_sleep_hours" = "Sleep Time",
      "avg_social_media_hours" = "Social Media Time",
      "avg_netflix_hours" = "Netflix Usage",
      "other_hours" = "Other Activities"
    )
  ) +
  
  #adjust theme
  theme_minimal(base_size = 14 ) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  ) +
  
  #costumize ticks
  scale_y_continuous(breaks = seq(0, 24, by = 4), labels = comma)
