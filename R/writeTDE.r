# Write R data.frame to a Tableau data extract file (.tde) by building and executing 
# a python script which utilizes the Tableau data extract API (a hack, yes).
#
# This, naturally, has a hard dependency on the TDE API, so is only available for 
# Windows and Linux systems (unfortunately)
#
# Devin Riley
# October, 2014


writeTDE <- function(df, file, clean=TRUE){
  
  # Get info about data.frame
  cols <- dim(df)[2]
  rows <- dim(df)[1]
  colcl <- sapply(df, class)
  colnm <- colnames(df)
  nm <- deparse(substitute(df))
  
  # Define a hash between R class and API data type
  # API data type integer is a property of the extract API
  hsh <- list("integer" = 7, 
              "numeric" = 10, 
              "factor" = 15,
              "character" = 15, 
              "logical" = 11,
              "Date" = 12)
  # Define a hash between R class and API set functions and cast functions
  fhsh <- list("integer" = c("setInteger", "int"),
               "numeric" = c("setDouble", "float"),
               "factor" = c("setCharString", "str"),
               "character" = c("setCharString", "str"),
               "logical" = c("setBool", "bool"),
               "Date" = c("setDate", "datetime.datetime.strptime"))
  
  
  # Define universal start to python script
  python <- "import dataextract as tde\nimport os, time, datetime\nimport csv\n\nstart = time.time()\n\n"
  
  
  # Create extract template with provided filename
  more <- paste("try:\n\tos.system('del ", file, ".tde')\n",
                "\tos.system('del DataExtract.log')\n", 
                "\ttdefile = tde.Extract('", file, ".tde')\n",
                "except:\n\ttdefile = tde.Extract('", file, ".tde')\n",
                "\ntableDef = tde.TableDefinition()\n\n", sep="")
  
  # Append the tabledef code to the python script
  python <- paste(python, more, sep="")
  
  
  # Add column definitions to table definition
  for(i in 1:cols){
    more <- paste("tableDef.addColumn('", colnm[i],"', ", hsh[[colcl[i]]], ")\n", sep="")
    python <- paste(python, more, sep="")
  }
  
  # Add table to extract with column definitions
  more <- "\ntable = tdefile.addTable('Extract', tableDef)\n\n"
  python <- paste(python, more, sep="")
  
  
  # Write data.frame out to temporary csv file
  # The Python script will iterate over this file, creating the TDE
  write.csv(df, "temp.csv", row.names=FALSE)
  
  # Define a csv reader
  more <- paste("csvReader = csv.reader(open('temp.csv', 'rb'), delimiter=',', ", 
                paste("quotechar=", "'", '"', "'", sep=""), ")\n\n", sep="")
  python <- paste(python, more, sep="")
  
  # Create a row object
  more <- paste("newrow = tde.Row(tableDef)\n",
                "csvReader.next() #ignore header\n",
                "for line in csvReader:\n",
                sep="")
  # One line of code per column for each row (i.e. nxn lines executed)
  for(i in 1:cols){
    if(colcl[i] == "Date"){
      # Need to transform date variable
      newline <- paste("\tdate = ", fhsh[[colcl[i]]][2], "(line[", i-1, "], '%Y-%m-%d')\n",
                       "\tnewrow.", fhsh[[colcl[i]]][1], 
                       "(", i-1, ", date.year, date.month, date.day)\n", sep="")
      more <- paste(more, newline, sep="")
      
    }
    else{
      # char, int, double just need different function names
      newline <- paste("\tnewrow.", fhsh[[colcl[i]]][1], 
                       "(", i - 1, ", ", fhsh[[colcl[i]]][2], 
                       "(line[", i - 1, "]))\n", sep="")
      more <- paste(more, newline, sep="")
    }
  }
  python <- paste(python, more, "\ttable.insert(newrow)\n", sep="")
  
                
  more <- "\ntdefile.close()"
  python <- paste(python, more, sep="")
  
  # Write python code to file and run
  write(python, file="toTDE.py")
  system("python toTDE.py", wait=TRUE)
  
  # Clean up directory (if desired)
  # If true, leaves only the TDE
  if(clean == TRUE){
    sapply(c("temp.csv", "toTDE.py"), file.remove)
  }
  
}

