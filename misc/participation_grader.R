### Load Libraries
library(tidyverse)

### Load data
zoom_p1 <- read_csv("~/Desktop/grading/j397dj/zoom_week2.csv") %>% mutate(Week="Week 1")
zoom_p2 <- read_csv("~/Desktop/grading/j397dj/zoom_week3.csv") %>% mutate(Week="Week 2")
zoom_p3 <- read_csv("~/Desktop/grading/j397dj/zoom_week4.csv") %>% mutate(Week="Week 3")
activities <- read_csv("~/Desktop/grading/j397dj/period_1_activities.csv")

### Combine Zoom Data
zoom <- bind_rows(zoom_p1 %>% filter(complete.cases(.)), zoom_p2 %>% filter(complete.cases(.)), zoom_p3 %>% filter(complete.cases(.)))
rm(zoom_p1, zoom_p2, zoom_p3)

### Calculate Zoom Participation
p_zoom <- zoom %>%
  mutate(`User Email` = str_remove(`User Email`, coll("donahue.")), `Total Duration (Minutes)`=as.numeric(`Total Duration (Minutes)`)) %>%
  filter(`Total Duration (Minutes)` >= 60) %>%
  group_by(`User Email`) %>%
  count() %>%
  arrange(n)

### Double-Check Zoom Participation
zoom %>%
  filter(`User Email` == "esuarezherna@umass.edu")

### Check Activities
p_act <- activities %>%
  mutate(Email = str_remove(Email, coll("donahue."))) %>%
  filter(Accept=="Yes") %>%
  group_by(Email) %>%
  count() %>%
  arrange(n)

### Produce Grading Dataframe
full_join(p_zoom, p_act, by=c("User Email" = "Email")) %>%
  rename(email=`User Email`, zoom_attended=n.x, activities_completed=n.y) %>%
  filter(email != "rzamith@umass.edu") %>%
  arrange(email) %>%
  mutate(flag=ifelse(zoom_attended < 2 | activities_completed < 2, "FLAG", NA)) %>%
  write_csv("/tmp/grading.csv")
