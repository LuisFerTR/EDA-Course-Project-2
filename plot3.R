library(dplyr)
library(ggplot2)

# Get data
data.dir <- "./data"
if(!file.exists(data.dir)) {
  dir.create(data.dir)
}

zip.url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
zip.file <- paste(data.dir, "dataset.zip", sep="/")

if(!file.exists(zip.file))
{
  download.file(zip.url, destfile = zip.file)  
}

unzip(zip.file, exdir = data.dir)

NEI <- readRDS(paste(data.dir, "summarySCC_PM25.rds", sep="/"))
SCC <- readRDS(paste(data.dir, "Source_Classification_Code.rds", sep="/"))

# Group data by year and plot it
total.emissions.year <- NEI %>% 
  group_by(year,type) %>% 
  summarise(total = sum(Emissions))

png("plot3.png")
g <- ggplot(total.emissions.year, aes(year, total))
g2 <- g + geom_point(alpha=1/3, aes(color = type)) + 
  geom_smooth(method="lm", se = FALSE, aes(color = type)) + 
  theme_light() + 
  labs(title = "Total PM2.5 Emissions by type (1999-2008)")

print(g2)
# Close graphic device connection
dev.off()
