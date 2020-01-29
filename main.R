library(foreign)
library(tidyverse)
library(haven)
library(magrittr)
library(rjson)

# file location:
folder <- "C:/Users/k1926273/Documents/R Projects/PISR/Data/"
folder_mappings <- "C:/Users/k1926273/Documents/R Projects/PISR/Mappings/"


dat_school <- load_PISR_data(folder, "CY07_MSU_SCH_QQQ", mapping=TRUE, folder_mappings)
dat_teacher <- load_PISR_data(folder, "CY07_MSU_TCH_QQQ", mapping=FALSE, folder_mappings)
dat_student <- load_PISR_data(folder, "CY07_MSU_STU_QQQ", mapping=TRUE, folder_mappings)

print_fields(dat_teacher)

###### Notes
# unique ID of students: CNTSTUID
## the school unique ID: CNTSCHID
## grade (and therefore kind of age) for the student: ST001D01T
## student gender: ST004D01T


###### Questions
# Q does the immigrant status of student explain over performance
# Q how digital are British students
# Q does digital aptitude change amongst British students?
# Q 
# Q does online computer usage predict unhappiness?

# loads the large pisa spss files, and, if asked, filters them on specified fields  and renames fields
load_PISR_data <- function(folder, file = "CY07_MSU_SCH_QQQ", mapping=TRUE, folder_mappings){
  df <- read_sav(paste0(folder, file, ".sav"))
  
  if(mapping){
    maps <- load_field_mappings(json_folder = folder_mappings, 
                        json_file = "well_being_mapping.txt",
                        PISR_file = file)
    fields <- names(maps)
    df <- df %>% select(fields)
    
    names(df) <- unlist(maps)
  }
  return(df)
}

# get new names for fields as specified in the json file
load_field_mappings <- function(json_folder,
                                json_file = "well_being_mapping.txt", 
                                PISR_file = "CY07_MSU_SCH_QQQ"){
  json_data <- fromJSON(file=paste0(json_folder, json_file))
 
  # get the JSON sub area based on the file you are loading
  return(json_data[[PISR_file]])
}


print_fields <- function(df){
  names(df) 
}

# message output that describes the attributes of the focus column
describe_dataset <- function(df, focus=quo(ST013Q01TA)){
  name <- df %>% pull(!!focus) %>% attr("label")
  lbls <- df %>% pull(!!focus) %>% attr("labels") %>% names()
  cnt_lbls <- df %>% pull(CNT) %>% attr("labels") %>% names()
  levs <- df %>% select(!!focus) %>% filter(!is.na(!!focus)) %>% distinct(!!focus) %$% nrow(.)
  
  message("focus on ", quo_name(focus), " \n__", 
          name, "__ \n ",
          levs, " levels:\n",
          print(lbls,sep=", "))
  return(levs)
  
}

########
# gives the percentage results of a single variable where the variable only has two values
# TODO id=quo(focus) does't appear to have a function
binaryQuestion <- function(df, focus=quo(SC156Q05HA), id=quo(focus)){
  
  # df <- dat_student
  # focus <- quo(SC156Q05HA)
  levs <- describe_dataset(df, focus)
  
  # catch error if wrong focus is selected
  
  if(levs != 2){
    warning("More than two levels, exiting: ", levs)
    warning(df %>% select(!!focus) %>% filter(!is.na(!!focus)) %>% distinct(!!focus))
    return(FALSE)
  }
  
  
  #NOTE: as_factor is a haven command
  tmp <- df %>% mutate(!!focus := as_factor(!!focus), CNT = as_factor(CNT)) %>%
    filter(!is.na(!!focus)) %>% 
    group_by(CNT) %>% 
    mutate(total_schools = n()) %>% select(!!focus, total_schools) %>% ungroup() %>%
    group_by(CNT, !!focus) %>%  mutate(binary = n(), # mutate(!!id := n()) %>%
                                           percentage = round(100 * (binary / total_schools), 1)
                                           ) %>%
    distinct(CNT, !!focus, percentage, total_schools) %>% spread(!!focus, percentage)
  
  return(tmp)
}

# returns tally for any given dataset with multiple outcomes
lickertQuestion <- function(df, focus=quo(ST013Q01TA), id=quo(focus)){
  
  levs <- describe_dataset(df, focus)
  
  # !!focus := as_factor(!!focus), 
  tmp <- df %>% mutate(CNT = as_factor(CNT)) %>%
    filter(!is.na(!!focus)) %>% 
    group_by(CNT) %>% 
    mutate(total = n()) %>% select(!!focus, total) %>% ungroup() %>%
    group_by(CNT, !!focus) %>%  mutate(binary = n(), # mutate(!!id := n()) %>%
                                       percentage = round(100 * (binary / total), 1)
    ) %>%
    distinct(CNT, !!focus, percentage, total)
  return(tmp)
}

# takes the result of a lickertQuestion function
lickertQuestionAvg <- function(df, focus=quo(ST013Q01TA)){
  df<- df %>% group_by(CNT) %>% select(!!focus) %>% 
    summarise(M = mean(!!focus, na.rm=TRUE), SD = sd(!!focus, na.rm=TRUE), n=n())
  
  # TODO: does n() count na
  # TODO: what about those lickert results which have error entries, e.g. 98, 96 etc
  
  # df <- df %>% group_by(CNT) %>% 
  #   mutate(weight = !!focus * (percentage / 100)) %>% 
  #   group_by(CNT, total) %>% 
  #   summarise(weight = sum(weight)) %>%
  #   arrange(desc(weight))
  return(df)
}



### question on happiness linked to computer usage at home
dat_school %>% select(ST158Q01HA)
dat_teacher %>% select(ST158Q01HA)
dat_student %>% select(IC001Q01TA)




### questions on digital infrastructure in schools
binaryQuestion(dat_school,  quo(SC156Q05HA)) %>% arrange(desc(Yes)) # online safety lessons
binaryQuestion(dat_school,  quo(SC155Q09HA)) # learning platform NOT WORKING
binaryQuestion(dat_school,  quo(SC156Q06HA)) %>% arrange(desc(Yes))  # social networks



dat_student$ST158Q01HA


#### organise by country school, student id, year, and gender
tmp <- dat_student %>% select(CNT, CNTSCHID, CNTSTUID, ST001D01T, ST004D01T, 
                       ST158Q01HA,
                       ST158Q02HA,
                       ST158Q04HA,
                       ST158Q07HA) %>%
  group_by(CNT, ST001D01T, ST004D01T) %>%
  summarise(n= n())

# SC013Q01TA public or private schools
## summary for school type by region and public private
tmp <- dat_school %>% select(CNT, NatCen, Region, SC013Q01TA) %>% group_by(CNT, NatCen, Region, SC013Q01TA) %>% summarise(n = n())
# dat_school %>% filter(SC013Q01TA == 1) %>% 
  tmp <- binaryQuestion(dat_school %>% mutate(CNT = paste(CNT, NatCen, as_factor(Region), as_factor(SC013Q01TA), sep="__")),  quo(SC156Q05HA)) %>% arrange(desc(Yes))
  
tmp <- dat_school %>% distinct(CNT)

# TODO: how does a link to the internet at home link with use of the internet?
tmp <- binaryQuestion(dat_student,  quo(SC156Q05HA)) %>% arrange(desc(Yes))



# TODO: relationships between books, internet and performance in maths and english

# TODO: taught esafety courses in schools and what these courses involve. Linked to national curriculums?
# TODO: get a taste of school computing provision by looking at how students perform on tests versus whether the school offers courses.
# TODO: ST158Q01HA being taught how to use search engines is very poor in UK, 
tmp <- binaryQuestion(dat_student,  quo(ST158Q01HA)) %>% arrange(desc(Yes)) # rank 30 on how to search
tmp <- binaryQuestion(dat_student,  quo(ST158Q02HA)) %>% arrange(desc(Yes)) # # rank 15 on trusting websites
tmp <- binaryQuestion(dat_student,  quo(ST158Q04HA)) %>% arrange(desc(Yes)) # 2nd on making data available
tmp <- binaryQuestion(dat_student,  quo(ST158Q07HA)) %>% arrange(desc(Yes)) # spam emails #23

# non binary questions
q_books_cats <- lickertQuestion(dat_student,  quo(ST013Q01TA)) %>% arrange(desc(ST013Q01TA)) # number of books
q_books_msd <- lickertQuestionAvg(dat_student,  quo(ST013Q01TA))  %>% arrange(desc(M))


q_ebooks_cats <- lickertQuestion(dat_student,  quo(ST012Q08NA)) # number of books
q_ebooks_msd <- lickertQuestionAvg(dat_student,  quo(ST012Q08NA))  %>% arrange(desc(M))


# does increasing ebooks lead to more books?
tmp <- left_join(q_books_msd, q_ebooks_msd, by="CNT")
ggplot(data=tmp, aes(M.x, M.y)) + geom_point() + geom_smooth() + geom_label()

# TODO: check that large immigrant population increases the performance of PISA countries, 
# we can test this through looking at the language taught at home





