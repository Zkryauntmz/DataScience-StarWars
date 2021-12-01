install.packages("tidyverse")
install.packages("starwars")
install.packages("ggplot2")
library(tidyverse)
library(dplyr)
library(starwars)
library(ggplot2)


starwars <- dplyr::starwars


  rows_without_NAs <- starwars %>%              #QUESTÝON 1
  drop_na(starships) %>% 
  select(name , starships)
  list(rows_without_NAs)





 starwars %>% count(eye_color , sort = TRUE) # QUESTÝON 2
 
 
 
      starwars %>%                          # QUESTÝON 3 
            filter(!is.na(birth_year)) %>%
            filter(!is.na(species)) %>%
            group_by(species) %>%
            arrange(birth_year) %>%
        summarise(max(birth_year))


      
starwars_mydata<-(starwars%>%       #QUESTÝON 4
              add_row(name="zekeriya",height=185,mass=90,hair_color="black",skin_color="dark",
                      eye_color="black",birth_year=1997,sex="male",gender="feminine",homeworld="Corellia",
                      species="Human",films=list(c("Solo: A Star Wars Story","Star Wars: Episode IV - A New Hope")),vehicles=list(c("Snowspeeder")),starships=list(c("Millennium Falcon"))))





 
starwars_mydata <- starwars_mydata  %>%                        # QUESTÝON 5
  mutate( Age = 2021 - (1977 - birth_year))

starwars_mydata <- starwars_mydata %>% 
  mutate( BMI = mass / ((height / 100)  ^ 2))

  
  starwars_mydata <- starwars_mydata %>%
    mutate(BMI_Observation = cut(BMI,breaks=c(-Inf , 18.5 , 25.0 ,  30.0 , Inf),labels=c("underweight","healthy","overweight","obese")))



starwars_Less <- starwars_mydata %>%        #QUESTÝON 6
      filter(Age < 100 )  
  
  ggplot(data = starwars_Less , mapping = aes(x =Age , fill =BMI_Observation )) + 
    geom_histogram(alpha = 1.5) + 
    labs(x = "Less 100 Age in ", 
         title = "Plot the distribution of ages less than 100 by BMI groups", 
         subtitle = "by BMI OBSERVATÝON")


  
  
  ggplot(data=starwars_Less) +                                            #QUESTÝON 7
    geom_point(mapping = aes(x = Age, y = BMI ,alpha=Age, color=BMI ) ) +
    labs(title="the relationship between age and BMI values ", x="AGE", y="BMI VALUEs")
  
  
  
  
