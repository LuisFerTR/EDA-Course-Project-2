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

# Get coal combustion-related sources code
coal.names <- grep('coal',SCC$Short.Name,value=TRUE,ignore.case=TRUE)
coal.scc <- SCC %>% filter(Short.Name %in% coal.names) %>% select(SCC)

NEI.coal <- filter(NEI, SCC %in% coal.scc$SCC)

# Group data by year and plot it
total.emissions.year <- NEI.coal %>% 
  group_by(year) %>% 
  summarise(total = sum(Emissions))

png("plot4.png")
g <- ggplot(total.emissions.year, aes(year, total))
g2 <- g + geom_point(colour = "dodgerblue4") + 
  geom_smooth(method="lm", se = FALSE, colour = "firebrick2") + 
  theme(legend.position = "none") +  
  labs(title = "Total PM2.5 Emissions from coal combustion-related sources (1999-2008)")

print(g2)
# Close graphic device connection
dev.off()
