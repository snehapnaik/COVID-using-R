

# Prepare the header and display contents of the workspace 
ls()

# Comment the command for clearing the workspace 
rm(list=ls()) 



# Load the workspace from previous as
load("<your_path>/Prev_Workspace.RData")


# Display the contents of the workspace
ls()


# Permanently change County_name column from factor to character
May28$COUNTY_NAME <- (as.character(May28$COUNTY_NAME))

# Display the structure of the new data frame
str(May28)

# Assign the value 500 to a scalar

ylimit <- 500

# Conditional statement to change the value of upper limit scalar (ylimit)
# change to max of positive cases if ylimit is negative

if(ylimit < 0) {
  ylimit <- max(CovidTexas$PEOPLE_POSITIVE_CASES_COUNT , na.rm = TRUE)
} else {
  ylimit <- ylimit
}
ylimit

# Create a subset with only positive cases row for Brazos county.
# Select only positive cases and date columns using column number
county_name <- "Brazos"

COVID_BRAZOS <- CovidTexas[CovidTexas$PEOPLE_POSITIVE_CASES_COUNT > 0
                           & CovidTexas$COUNTY_NAME == "Brazos", c(3 , 8) ]

# Order according to ascending order of dates

COVID_BRAZOS <- COVID_BRAZOS[order(COVID_BRAZOS$Date) , ]

# Create a bar plot for cases by day.
barplot(COVID_BRAZOS$PEOPLE_POSITIVE_CASES_COUNT,
        xlab = "Dates" ,
        ylab = "Total number of Positive cases",
        main = bquote(paste(.(county_name)," County ", 
		 group(lceil, "Scale", rceil) ,"=" , .(ylimit) )),
        ylim = c(0 , ylimit) ,
        names.arg = COVID_BRAZOS$Date 
)

#Create a function replacing the hard-coded values
# Assign the value 500 to a scalar

ylimit <- 500

# Create a conditional ylimit 

map_covid <- function(county_name, ylimit = -1 , 
		      datasource = CovidTexas , 
		      col3 = 3 , col8 = 8)
{
  if(ylimit < 0) {
    ylimit <- max(datasource$PEOPLE_POSITIVE_CASES_COUNT , na.rm = TRUE)
  } else {
    ylimit <- ylimit ## change here to 500
  }

  
# Create a subset with only positive cases row for input county.
# Select only positive cases and date columns using column number as parameters

  
  
  COVID_county<- datasource[datasource[ , col3] > 0
                            & datasource[ , 1] == county_name, c(col3 , col8) ]


# Order according to ascending order of dates
  
  
  COVID_county <- COVID_county[order(COVID_county$Date) , ]
  
# Create a bar plot for cases by day.


barplot(COVID_county$PEOPLE_POSITIVE_CASES_COUNT,
          xlab = "Dates" ,
          ylab = "Total number of Positive cases",
          main = bquote(paste(.(county_name)," County ", 
		 group(lceil, "Scale", rceil) ,"=" , .(ylimit) )) ,
          ylim = c(0 , ylimit) ,
          names.arg = COVID_county$Date 
  )
}
########### function ends ##############

# Open a PDF file to save all the graphics
# Current directory is my assignments folder
# Width of the page 1 inches and height 8.5 inches

pdf("graphics.pdf" , width = 11 , height = 8.5 ) # set up page




# Call the function with only Dallas as an argument


map_covid("Dallas")




# Set up graphics page as per requirement


par(
  mfrow = c(1,2) , 
  omi = c(1.5 , 1 , 0.5 , 1),
  mar = c(0 , 2 , 4 , 2)
  )  



# Call the function for Brazos and McLennan counties with ylimit as 500



map_covid("Brazos" , 500)
map_covid("McLennan" , 500)



# Reset graphics are back to 1x1



par(mfrow = c(1,1))



# Create a sample vector of size 10 for county names
# The positive cases should not be NA / missing
# Seed of value 2020607 to get the correct sample of 10 counties

county_names_vector <- May28[!is.na(May28$PEOPLE_POSITIVE_CASES_COUNT) ,]$COUNTY_NAME
set.seed(2020607)
(county_sample <- sample(county_names_vector,10))



# Loop this sample to call the function for each county along with ylimit 2000

for  ( i in 1 : length(county_sample))
{
  map_covid(county_sample[i] , 2000)
  i <- i+1
  mtext(text=Sys.time(),side=4,line=0,outer=TRUE , adj = 1)
}


dev.off()  ### close the device when finished


