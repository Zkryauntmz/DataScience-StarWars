install.packages("tidyverse")
install.packages("starwars")
install.packages("ggplot2")
library(tidyverse)
library(dplyr)
library(starwars)
library(ggplot2)


starwars <- dplyr::starwars

#QUESTION 1 : How many characters do have at least one starship? List the names of the characters having at least one starship.

  rows_without_NAs <- starwars %>%              
  drop_na(starships) %>% 
  select(name , starships)
  list(rows_without_NAs)



# QUESTION 2 : Get the frequencies of the eye color of the characters. Rank them from most to least.

 starwars %>% count(eye_color , sort = TRUE) 
 
 
     # QUESTÝON 3  : According to the data available, what are the mean (average) age values across each species?. Note that in the data, there is no age column, instead there is a birth_year column. This column represents how many years before the Battle of Yavin the character was born. So take this column as the ages of characters at the Battle of Yavin and calculate the mean.

      starwars %>%                      
            filter(!is.na(birth_year)) %>%
            filter(!is.na(species)) %>%
            group_by(species) %>%
            arrange(birth_year) %>%
        summarise(max(birth_year))


      #QUESTÝON 4 : Create a new data set by adding a new observation to this data. This observation should be based on your own character (your name or nickname, your weight and height, your homeworld, your starships etc). Note that you can pick one or more than one Star Wars films in which you played as a movie star.
starwars_mydata<-(starwars%>%       
              add_row(name="zekeriya",height=185,mass=90,hair_color="black",skin_color="dark",
                      eye_color="black",birth_year=1997,sex="male",gender="feminine",homeworld="Corellia",
                      species="Human",films=list(c("Solo: A Star Wars Story","Star Wars: Episode IV - A New Hope")),vehicles=list(c("Snowspeeder")),starships=list(c("Millennium Falcon"))))



           # QUESTÝON 5 :  Calculate the body mass index (BMI) values (dividing the mass value in kg to the square of the height value in meter) for all observations and categorize the observations as underweight (BMI below 18.5), healthy (BMI between 18.5-24.99), overweight (BMI between 25.0-29.99) and obese (BMI above 30.0). Add these two variables to your new data created at the 4th question.

 
starwars_mydata <- starwars_mydata  %>%             
  mutate( Age = 2021 - (1977 - birth_year))

starwars_mydata <- starwars_mydata %>% 
  mutate( BMI = mass / ((height / 100)  ^ 2))

  
  starwars_mydata <- starwars_mydata %>%
    mutate(BMI_Observation = cut(BMI,breaks=c(-Inf , 18.5 , 25.0 ,  30.0 , Inf),labels=c("underweight","healthy","overweight","obese")))

 #QUESTÝON 6 : Plot the distribution of ages less than 100 by BMI groups. (i.e. use filter function to select the ages less then 100)

starwars_Less <- starwars_mydata %>%       
      filter(Age < 100 )  
  
  ggplot(data = starwars_Less , mapping = aes(x =Age , fill =BMI_Observation )) + 
    geom_histogram(alpha = 1.5) + 
    labs(x = "Less 100 Age in ", 
         title = "Plot the distribution of ages less than 100 by BMI groups", 
         subtitle = "by BMI OBSERVATÝON")


      #QUESTÝON 7 : By plotting a graph, show the relationship between age and BMI values (use point and line at the same time). Re-plot the same graph after filtering the data as both age and BMI less than 100.
  
  ggplot(data=starwars_Less) +                                        
    geom_point(mapping = aes(x = Age, y = BMI ,alpha=Age, color=BMI ) ) +
    labs(title="the relationship between age and BMI values ", x="AGE", y="BMI VALUEs")
  
  
  
  
