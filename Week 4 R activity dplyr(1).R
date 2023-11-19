#Week 4: dplyr package

#Task: Write the function to get a dataset from Base R: Titanic
#and give the dataframe a new name of your choice
#(hint: you will want your data to be a dataframe. Use the function: as.data.frame(Titanic))

getTitanicData <- function(newtitanic)
  
newtitanic <- as.data.frame(Titanic)

#See the top rows of the data
#TASK: Write the function to see the top rows of the data

head(newtitanic, n=5)


#Install and call the package dplyr
#TASK: Write the functions to install and call dplyr

install.packages("dplyr")
library(dplyr)

#Let's just see the Survived and Sex columns
#Task: Write the function to 'select' the Survived and Sex columns 
#(hint: use the 'select' function)

selectColumns <- function(data) {
  selected_data <- select(data, Survived, Sex)
  return(selected_data)
}

ssonlycolumns <- selectColumns(newtitanic)
head(ssonlycolumns)

#Let's name the dataset with just the two columns, Survived and Sex
#TASK: Write the function to save the two columns as one new dataset
#and give it a name 

saveSurvivedSexColumns <- function(data) {
  updatedtitanic <- data %>% 
    select(Survived, Sex)
  assign("updatedtitanic", updatedtitanic, envir = .GlobalEnv)
}
saveSurvivedSexColumns(newtitanic)

#Let's get rid of the Sex column in the new dataset created above
#TASK: Write the function that deselects the sex column
#(hint: use the 'select' function to not select a column)

removeSexColumn <- function(data) {
  withoutsex <- data %>% 
    select(-Sex)
  return(withoutsex)
}
withoutsex <- removeSexColumn(updatedtitanic)
head(withoutsex)

#Let's rename a column name
#TASK: Write the function that renames 'Sex' to 'Gender'

renameColumn <- function(data) {
  renamed_data <- data %>%
    rename(Gender = Sex)
  return(renamed_data)
}
updatedtitanicgender <- renameColumn(updatedtitanic)
head(updatedtitanicgender)

#Let's make a new dataframe  with the new column name gender
#TASK: Write the function that names a new dataset that includes the 'gender' column

createnewwithgender <- function(data) {
  newdataframegender <- data %>% 
    rename(Gender = Sex)
  assign("newdataframegender", newdataframegender, envir = .GlobalEnv)
}
createnewwithgender(updatedtitanic)

#Let's 'filter' just the males from our dataset
#TASK: Write the function that includes only rows that are 'male'

filtermale <- function(data) {
  male_data <- data %>%
    filter(Gender == "Male")
  return(male_data)
}
malerows <- filtermale(newdataframegender)
head(malerows)


#Let's 'arrange' our data by gender (not the data you just filtered)
#TASK: Write the function to group the data by gender (hint: arrange())

arrangedgender <- function(data) {
  arranged_data <- data %>%
    arrange(Gender)
  return(arranged_data)
}
arrangedgender<- arrangedgender(updatedtitanicgender)
head(arrangedgender)

#Let's see how many people were examined in the dataset (total the frequency in the original dataframe)
#TASK: Sum the Freq column
#TASK: After you run it, write the total here:2201 

total_examined <- sum(newtitanic$Freq)
print(total_examined)

#Since we have a males dataset, let's make a females dataset
#TASK: Write the function that includes only rows that are 'female'

filterfemale <- function(data) {
  female_data <- data %>%
    filter(Gender == "Female")
  return(female_data)
}
femalerows <- filterfemale(newdataframegender)
head(femalerows)

#And now let's join the males and females
#TASK: Write the function that joins the male and female rows 
#(hint: try using 'union' or 'bind_rows')

joingender <- function(male_data, female_data) {
  combined_data <- bind_rows(male_data, female_data)
  return(combined_data)
}
comined <- joingender(malerows,femalerows)

#Optional Task: add any of the other functions 
#you learned about from the dplyr package

