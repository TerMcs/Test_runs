library(ggplot2)
library(reshape2)

task1 <- c('Main ethics', '2020-10-01', '2021-03-01')
task2 <- c('Pilot data collection', '2020-10-01', '2020-12-30')
task3 <- c('Main data collection', '2021-04-01', '2021-07-01')
task4 <- c('Analyse data', '2021-06-01', '2021-08-01')
task5 <- c('Write report', '2021-10-01', '2022-04-01')

df <- as.data.frame(rbind(task1, task2, task3, task4, task5))

names(df) <- c('task', 'start', 'end')

df$task <- factor(df$task, levels = df$task)
df$start <- as.Date(df$start)
df$end <- as.Date(df$end)
df_melted <- melt(df, measure.vars = c('start', 'end'))

start_date <- as.Date('2020-08-01')

p <- ggplot(df_melted, aes(value, task, color = task, show.legend = FALSE)) + 
  geom_line(size = 30) +
  labs(x = '', y = '', title = 'Gantt chart - QIBs in CLBP') +
  theme_light(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(colour="grey80", linetype = "dashed"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 0)) +
  scale_x_date(date_labels = "%Y %b", limits = c(start_date, NA), date_breaks = "3 months")

p + theme(legend.position = "none")

