# title: Shot Charts Script
# description: Creating charts from our data for each player
# input(s): the five data CSV files (one for each player)
# output(s): Scatterplots

library(ggplot2)

data <- read.csv("../data/shots-data.csv", stringsAsFactors = FALSE)
klay <- data[data[,'name'] == 'Klay Thompson', ]
andre <- data[data$name == 'Andre Iguodala', ]
draymond <- data[data$name == 'Draymond Green', ]
kevin <- data[data$name == 'Kevin Durant', ]
stephen <- data[data$name == 'Stephen Curry', ]

klay_scatterplot <- ggplot(data = klay) + geom_point(aes(x = x, y = y, color = shot_made_flag))

court_file <- "../images/nba-court.jpg"

court_image <- rasterGrob( readJPEG(court_file), width = unit(1, "npc"), height = unit(1, "npc"))

klay_shot_chart <- ggplot(data = klay) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') + theme_minimal()

pdf(file = '../images/klay-thompson-shot-chart.pdf', width = 6.5, height = 5)
klay_shot_chart
dev.off()


andre_scatterplot <- ggplot(data = andre) + geom_point(aes(x = x, y = y, color = shot_made_flag))
andre_shot_chart <- ggplot(data = andre) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') + theme_minimal()

pdf(file = '../images/andre-iguodala-shot-chart.pdf', width = 6.5, height = 5)
andre_shot_chart
dev.off()

draymond_scatterplot <- ggplot(data = draymond) + geom_point(aes(x = x, y = y, color = shot_made_flag))
draymond_shot_chart <- ggplot(data = draymond) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') + theme_minimal()

pdf(file = '../images/draymond-green-shot-chart.pdf', width = 6.5, height = 5)
draymond_shot_chart
dev.off()

kevin_scatterplot <- ggplot(data = kevin) + geom_point(aes(x = x, y = y, color = shot_made_flag))
kevin_shot_chart <- ggplot(data = kevin) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') + theme_minimal()

pdf(file = '../images/kevin-durant-shot-chart.pdf', width = 6.5, height = 5)
kevin_shot_chart
dev.off()

stephen_scatterplot <- ggplot(data = stephen) + geom_point(aes(x = x, y = y, color = shot_made_flag))
stephen_shot_chart <- ggplot(data = stephen) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') + theme_minimal()

pdf(file = '../images/stephen-curry-shot-chart.pdf', width = 6.5, height = 5)
stephen_shot_chart
dev.off()

gsw_shot_chart <- ggplot(data = data) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: GSW (2016 season)') + theme_minimal() + facet_wrap(~name) + theme(legend.position = "top", legend.title = element_blank())

pdf(file = '../images/gsw-shot-charts.pdf', width = 8, height = 7)
gsw_shot_chart
dev.off()

png(filename = "../images/gsw-shot-charts.png", width = 8, height = 7, units = "in", res = 100)
gsw_shot_chart
dev.off()
