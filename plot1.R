library(dplyr)

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

# Group data by year
total.emissions.year <- NEI %>% 
  group_by(year) %>% 
  summarise(total = sum(Emissions))

png("plot1.png")
plot(total.emissions.year, 
     main="Total PM2.5 Emissions (1999-2008)", 
     col = "dodgerblue4",
     pch = 16)

x <- total.emissions.year$year
y <- total.emissions.year$total

fit <- lm(y ~ x)

x0 <- seq(min(x), max(x), length = 10)  
y0 <- predict.lm(fit, newdata = list(x = x0))  

lines(x0, y0, col = 2)

# Close graphic device connection
dev.off()