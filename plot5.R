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

# Get motor vehicle sources code
mvehicle.names <- grep('vehicle',SCC$EI.Sector,value=TRUE,ignore.case=TRUE)
mvehicle.scc <- SCC %>% filter(EI.Sector %in% mvehicle.names) %>% select(SCC)

NEI.mvehicle <- filter(NEI, SCC %in% mvehicle.scc$SCC & fips == "24510")

# Group data by year and plot it
total.emissions.year <- NEI.mvehicle %>% 
  group_by(year) %>% 
  summarise(total = sum(Emissions))

png("plot5.png")
g <- ggplot(total.emissions.year, aes(year, total))
g2 <- g + geom_point(colour = "darkorchid") + 
  geom_smooth(method="lm", se = FALSE, colour = "darkorange2") + 
  theme(legend.position = "none") +  
  labs(title = "Total PM2.5 Emissions from motor vehicle sources in Baltimore City (1999-2008)")

print(g2)
# Close graphic device connection
dev.off()
