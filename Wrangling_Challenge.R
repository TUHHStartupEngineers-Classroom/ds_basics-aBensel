library(vroom)
library(tidyverse)
library(data.table)


#uspc
col_types_uspc <- list(
  patent_id = col_character(),
  mainclass_id = col_number(),
  sequence = col_number()
)

uspc_tbl <- vroom(
  file       = "Patent_data_reduced/uspc.tsv",
  delim      = "\t",
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)
setDT(uspc_tbl)

#patent
col_types_patent <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file       = "Patent_data_reduced/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)
setDT(patent_tbl)

#patent_assignee
col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)


patent_assignee_tbl <- vroom(
  file       = "Patent_data_reduced/patent_assignee.tsv",
  delim      = "\t",
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)
setDT(patent_assignee_tbl)

#assignee
col_types_assignee <- list(
  id = col_character(),
  type = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "Patent_data_reduced/assignee.tsv",
  delim      = "\t",
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)
setDT(assignee_tbl)


#type 2: US Company or Corporation
us_top10_companies <- merge(x = patent_assignee_tbl, y = assignee_tbl, by.x="assignee_id", by.y="id") %>%
  filter(type == 2)%>%
  filter(!is.na(patent_id) || !is.na(organization)) %>%
  select(organization, patent_id) %>%
  group_by(organization) %>%
  count() %>%
  arrange(desc(n)) %>%  
  head(n = 10)

##Part 2

us_top10_aug_14 <- merge(x = patent_tbl, y = patent_assignee_tbl, by.x="id", by.y="patent_id") %>%
  merge(y = assignee_tbl, by.x="assignee_id", by.y="id")%>%
  filter(type == 2)%>%
  filter(!is.na(id) || !is.na(organization)) %>%
  separate(col  = date,
           into = c("year", "month", "day"),
           sep  = "-", remove = TRUE) %>%
  mutate(
    month = as.numeric(month)
  )%>%
  filter(month == 08)%>%
  select(-year, -month, -day, -assignee_id, -num_claims, -type) %>%
  group_by(organization) %>%
  count() %>%
  arrange(desc(n)) %>%  
  head(n = 10)

##Part 3

worldwide_top10 <- merge(x = patent_assignee_tbl, y = assignee_tbl, by.x="assignee_id", by.y="id") %>%
  filter(!is.na(patent_id) || !is.na(organization)) %>%
  group_by(organization) %>%
  count(patent_id) %>%
  summarise(total_patents = sum(n))%>%
  arrange(desc(total_patents)) %>%
  filter(!is.na(organization)) %>%
  ungroup() %>%
  head(n = 10)

top5_USPTO <- merge(x = uspc_tbl, y = patent_assignee_tbl, by = 'patent_id') %>%
  merge(y = assignee_tbl, by.x="assignee_id", by.y="id") %>%
  merge(y = worldwide_top10, by = 'organization') %>%
  group_by(mainclass_id) %>%
  count()%>%
  summarise(total = sum(n))%>%
  arrange(desc(total))%>%
  ungroup() %>%
  head(n = 5)
  