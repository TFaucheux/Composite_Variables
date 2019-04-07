# Reshape data to enable consolidation
newFielddescr <- sapply(as.character(df$FIELDDESCR), mapFielddescr)
names(newFielddescr) <- NULL

fieldCategory <- unlist(sapply(as.character(df$FIELDDESCR), highLevelCategory))
names(fieldCategory) <- NULL

df$ShortDescr <- factor(newFielddescr)
df$Category <- factor(fieldCategory)


# Adding fields to create a flat data source
dmg <- df[,-c(2,3,7)]
dmg <- ddply(dmg, .(GEOCODE, ID, NAME, Category, ShortDescr), 
      summarize, value = sum(VALUE), natl_avg = sum(NATL_AVG))
wrap14 <- sqlQuery(conn, "select geocode, smc_w, solo_w, persfduw, percapts  from claritas.cl_wrap_tab14 where geocode in (select wrap from claritas.cl_wrap_name where smc_w > 0)")
us14 <- sqlQuery(conn, "select smc_w, solo_w, persfduw, percapts  from claritas.cl_us_tab14")
colnames(us14) <- c("US_SMC_W", "US_SOLO_W", "US_PERSFDUW", "US_PERCAPTS")



dmg <- merge(dmg, wrap14) # on GEOCODE
dmg <- merge(dmg, us14) # repeat for every row


ss <- read.csv("data/SharpShooters2.csv")
aam <- read.csv("data/AAM.csv")
aam <- aam[, c("ID", "NAME", "S_PAPER", 
               "S_PEN", "D_PAPER", "D_PEN")]

ss_us_row <- sqlQuery(conn, "select * from claritas.cl_us_tab33")
ss_us <- data.frame(t(ss_us_row), names(ss_us_row)
colnames(ss_us) <- c("SS_NATL_AVG", "SS_CD")

temp <- merge(dmg, aam)
temp2 <- merge(temp, ss)
all <- merge(temp2, ss_us)
all$S_PEN[is.na(all$S_PEN)] <- 0
all$D_PEN[is.na(all$D_PEN)] <- 0

final <- merge(all, df2)

# Helper functions/objects
mapFielddescr <- function(x){
  if(x == "% '15 HHs w/HH Inc < $30,000"){
    return("< $30k")
  }else if(x == "% '15 HHs w/HH Inc $30-34.9K"){
    return("$30-50K")
  }else if(x == "% '15 HHs w/HH Inc $35-39.9K"){
    return("$30-50K")
  }else if(x == "% '15 HHs w/HH Inc $40-44.9K"){
    return("$30-50K")
  }else if(x == "% '15 HHs w/HH Inc $45-49.9K"){
    return("$30-50K")
  }else if(x == "% '15 HHs w/HH Inc $50-59.9K"){
    return("$50-75k")
  } else if(x == "% '15 HHs w/HH Inc $60-74.9K"){
    return("$50-75k")
  }else if(x == "% '15 HHs w/HH Inc $75-99.9K"){
    return("$75-125k")
  }else if(x == "% '15 HHs w/HH Inc $100-124.9K"){
    return("$75-125k")
  }else if(x == "% '15 HHs w/HH Inc $125-149.9K"){
    return("$125-200k")
  }else if(x == "% '15 HHs w/HH Inc $150-199.9K"){
    return("$125-200k")
  }else if(x == "% '15 HHs w/HH Inc $200-499.9K"){
    return("$200-500k")
  }else if(x == "% '15 HHs w/HH Inc $500,000 +"){
    return("> $500k")
  }else if(x == "% '15 Pop under Age 5"){
    return("< 5")
  }else if(x == "% '15 Pop Age 5-9"){
    return("5-17")
  }else if(x == "% '15 Pop Age 10-14"){
    return("5-17")
  }else if(x == "% '15 Pop Age 15-17"){
    return("5-17")
  }else if(x == "% '15 Pop Age 18-20"){
    return("18-24")
  }else if(x == "% '15 Pop Age 21-24"){
    return("18-24")
  }else if(x == "% '15 Pop Age 25-29"){
    return("25-34")
  }else if(x == "% '15 Pop Age 30-34"){
    return("25-34")
  }else if(x == "% '15 Pop Age 35-39"){
    return("35-54")
  }else if(x == "% '15 Pop Age 40-49"){
    return("35-54")
  }else if(x == "% '15 Pop Age 50-54"){
    return("35-54")
  }else if(x == "% '15 Pop Age 55-59"){
    return("55-74")
  }else if(x == "% '15 Pop Age 60-69"){
    return("55-74")
  }else if(x == "% '15 Pop Age 70-74"){
    return("55-74")
  }else if(x == "% '15 Pop Age 75-79"){
    return("75+")
  }else if(x == "% '15 Pop Age 80-84"){
    return("75+")
  }else if(x == "% '15 Pop Age 85 +"){
    return("75+")
  }else if(x == "% '15 Pop Age 25+, 9th Grd Not Complete"){
    return("No HS Diploma")
  }else if(x == "% '15 Pop Age 25+, 9-12th Grd No Diploma"){
    return("No HS Diploma")
  }else if(x == "% '15 Pop Age 25+, High School Grad or ="){
    return("HS Diploma")
  }else if(x == "% '15 Pop Age 25+, Associate Degree"){
    return("1-3 Clg")
  }else if(x == "% '15 Pop Age 25+, Bachelor's Degree"){
    return("B/M")
  }else if(x == "% '15 Pop Age 25+, Master's Degree"){
    return("B/M")
  } else if(x == "% '15 Pop Age 25+, Doctorate Degree"){
    return("PhD")
  }else if(x == "% '15 Pop White Alone Non-Hisp"){
    return("White")
  }else if(x == "% '15 Pop Black Alone Non-Hisp"){
    return("Black")
  }else if(x == "% '15 Pop Hispanic or Latino"){
    return("Hispanic")
  }else if(x == "% '15 Pop Asian Alone Non-Hisp"){
    return("Asian")
  } else if(x == "% '15 Pop Other Race Alone Non-Hisp"){
    return("Other")
  } else {
    return(x)
  }
}



highLevelCategory <- function(x){
  if(grepl("Pop Age", x) == 1 | grepl("Pop under Age", x) == 1){
    if(!grepl("Pop Age 25+", x)){
      return("Age")
    } else if(!grepl("25-29", x)) {
      return("Education")
    }
  } else if(grepl("Hisp", x) == 1){
    return("Ethnicity")
  } else if(grepl("w/HH Inc", x) == 1){
    return("Income")
  }
  return("Other")
}

