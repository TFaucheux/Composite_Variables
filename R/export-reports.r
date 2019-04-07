# Generate pdf reports for each wrap zone 
# To be run on machine hosting Tableau server
# valvcsqli001vm

setwd("C:/Users/qlikuser/Documents/Tableau/WrapZoneReports/")

# Get zone mapping data
sdmToRegion <- read.csv("data/sdm-region.csv")
colnames(sdmToRegion) <- c("SDM_NAME", "CURRENT_REGION")
wrapToSdm <- read.csv("data/sdm-wrap-xref.csv")

merged <- merge(wrapToSdm, sdmToRegion)

regions <- unique(as.character(merged$CURRENT_REGION))
zones <- as.character(merged$WRAP_NAME)
charZones <- gsub("/", "", gsub(" ", "", zones))
zoneIds <- wrapToSdm$WRAP_MKT_ID
zoneRegions <- merged$CURRENT_REGION

df <- data.frame(charZones, merged$WRAP_MKT_ID, merged$CURRENT_REGION, merged$WRAP_NAME)
df$merged.WRAP_NAME <- gsub("/", " ", df$merged.WRAP_NAME)

# Setup directory structure
dir.create("reports")
for(reg in regions){
  pth <- paste("reports/", reg, sep="")
  dir.create(pth)
}


# Log in to Tableau server
# NOTE: fill in password to login successfully
logincmd <- "tabcmd login -s http://valvcsqli001vm -u XXXXX -p XXXXX"
system(logincmd)


# Loop through zones
# Exporting pdf for each filter value
# 20150713 - Added "AllSharpShooters" to uri after publishing
# new dashboard with updated data provided by Deb
cmd <- 'tabcmd export "WrapZoneReportAllSharpShooters/WrapZones?CharOnlyName= --username rileyd --server http://valvcsqli001vm --pagelayout landscape --pdf --filename '


for(i in 1:nrow(df)){
  cmd <- paste('tabcmd export "WrapZoneReport/WrapZones?CharOnlyName=', 
               df$charZones[i], '"',
               ' --username rileyd --server http://valvcsqli001vm --pagelayout landscape --pdf --filename ',
               '"reports/', df$merged.CURRENT_REGION[i], '/',
               df$merged.WRAP_NAME[i], '"', sep="")
  system(cmd)
}



