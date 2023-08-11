#    _       ____            _____   ___    ___  ___  __ 
#   | |     / __ \    /\    / ____| |__ \  / _ \|__ \/_ |
#   | |    | |  | |  /  \  | |         ) || | | |  ) || |
#   | |    | |  | | / /\ \ | |        / / | | | | / / | |
#   | |____| |__| |/ ____ \| |____   / /_ | |_| |/ /_ | |
#   |______|\____//_/    \_\\_____| |____| \___/|____||_|
#                                                        
###############################################################################     
# This code was used to create the London Output Area Classification
# Created by:
# Alex Singleton, University of Liverpool
# Paul Longley, University College London
###############################################################################     

########################################################
# Setup and Data Import
########################################################

#---------
# Load packages
#---------

library(tidyverse)
library(sf)
library(magrittr)
library(janitor)
library(scales)
library(arrow)
library(h2o)
library(ggpubr)
library(patchwork)
library(chatgpt)
library(tcltk2)
libary(ggspatial)
library(osmdata)
library(ggrepel)
library(showtext)
font_add_google("Open Sans", "Light 300")

#---------
# Import spatial data
#---------

# Output Areas (Dec 2021) Boundaries Generalised Clipped EW (BGC)
OA_2021_Boundary <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Output_Areas_Dec_2021_Boundaries_Generalised_Clipped_EW_BGC_2022/FeatureServer/0/query?where=1%3D1&outFields=OA21CD&outSR=4326&f=json")

# Output Area to Region (December 2021) Lookup in England and Wales
OA_Region <- read_csv("https://www.arcgis.com/sharing/rest/content/items/efda0d0e14da4badbd8bdf8ae31d2f00/data")

# Output Area to Upper-Tier Local Authorities (December 2021) Lookup in England and Wales
OA_UTLAD <- read_csv("https://www.arcgis.com/sharing/rest/content/items/4393e36fa6184b0fb1b2562d98db1da6/data")

# Local Authority Districts (December 2021) GB BGC
LAD_Dec21 <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2021_GB_BGC_2022/FeatureServer/0/query?where=1%3D1&outFields=LAD21CD,LAD21NM&outSR=4326&f=json")

# Fix Projections
OA_2021_Boundary %<>%
st_transform(27700)

LAD_Dec21 %<>%
st_transform(27700)


# Create Greater London OA lookup
OA_london <- OA_Region %>%
              filter( rgn22nm == "London") %>%
              select(oa21cd) %>%
              left_join(OA_UTLAD) %>%
              rename(OA = oa21cd) %>%
              select(OA, utla22cd,utla22nm)
  
#---------
#  Pull in census 2021 data, calculate PCT
#---------

# Get Census Table Lists
census_tables <- read_csv("https://github.com/alexsingleton/Census_2021_Output_Areas/raw/main/Table_Metadata.csv",show_col_types = FALSE)

# Read Census Table (Remove ts041 - number of households,ts006 - density)
C_Table_Name_List <- census_tables %>% select(Table_ID) %>% unique() %>% pull()
C_Table_Name_List %<>% setdiff(c("ts041","ts006"))

for (ct_tmpID in C_Table_Name_List) {
  
  # Download Census Table
  CT_tmp <- read_csv(paste0("https://github.com/alexsingleton/Census_2021_Output_Areas/blob/main/output_data/csv/",ct_tmpID,".csv?raw=true"),show_col_types = FALSE)
  
  # Calculate Percentages
  CT_tmp %<>%
    
    mutate_at(vars(-1:-2), list(PCT = ~(. / !!sym(paste0(ct_tmpID,"0001"))*100)))
  
  assign(ct_tmpID,CT_tmp)
  rm(ct_tmpID,CT_tmp)
  
}

# Extra Census Data
ts041 <- read_csv(paste0("https://github.com/alexsingleton/Census_2021_Output_Areas/blob/main/output_data/csv/ts041.csv?raw=true"),show_col_types = FALSE)
ts006 <- read_csv(paste0("https://github.com/alexsingleton/Census_2021_Output_Areas/blob/main/output_data/csv/ts006.csv?raw=true"),show_col_types = FALSE)

# Industry Table
ts060 <- read_csv("./data/census/custom-filtered-2023-05-28T16_32_12Z.csv")

ts060_w <- ts060 %>%
  select(`Output Areas`,`Industry (current) (9 categories) Code`,Observation) %>%
  pivot_wider(
    names_from = `Industry (current) (9 categories) Code`,     # Column containing variable names
    values_from = Observation        # Column containing variable values
  ) %>%
  rename(
  OA = `Output Areas`,
  ts0600002 =  `1`,
  ts0600003 =  `2`,
  ts0600004 =  `3`,
  ts0600005 =  `4`,
  ts0600006 =  `5`,
  ts0600007 =  `6`,
  ts0600008 =  `7`,
  ts0600009 =  `8`,
  ts0600010 =  `-8`) %>%
  rowwise() %>%
  mutate(ts0600001 = sum(c_across(ts0600010:ts0600009)))

ts060_w %<>%
  select(OA,ts0600001,ts0600002,ts0600003,ts0600004,ts0600005,ts0600006,ts0600007,ts0600008,ts0600009,ts0600010)

# Calculate Percentages
ts060_w %<>%
  mutate_at(vars(-1:-2), list(PCT = ~(. / !!sym("ts0600001")*100)))

# Create Meta Data for Industry
tmp_meta <- ts060 %>% 
      distinct(`Industry (current) (9 categories) Code`, `Industry (current) (9 categories)`) %>%
      rename(
        old_names = `Industry (current) (9 categories)`,
        ID = `Industry (current) (9 categories) Code`) %>%
      mutate(
        Table_Name = "Industry",
        Type = "Value",
        Table_ID = "ts060",
        new_names = c("ts0600010","ts0600002","ts0600003","ts0600004","ts0600005","ts0600006","ts0600007","ts0600008","ts0600009"),
        Variable_Name = old_names) %>%
  select(Table_Name,Variable_Name,Type,new_names,Table_ID,old_names)

# Clean up...
census_tables %<>%
  bind_rows(tmp_meta)
ts060 <- ts060_w
rm(ts060_w,tmp_meta,C_Table_Name_List)

#save.image("~/source_data.RData")

############################################################
# Create Input Measures
############################################################


# Import variables lookup
variable_lookup <- read_csv("./data/lookup/Variables_LOAC.csv")

#---------
# Non-Percentages or multi-census table variables
#---------

# v01 - Usual residents per square kilometer
Non_PCT <- ts006 %>%
  rename(v01 = ts0060001)

# Import regional disability and population data
disability_region <- read_csv("./data/census/custom-filtered-2023-06-01T17_21_50Z.csv") 
pop_region <- read_csv("./data/census/custom-filtered-2023-06-02T16_15_50Z.csv")

disability_region %<>%
  filter(Regions == "London") %>%
  rename(disability = Observation) %>%
  clean_names() %>%
  select(age_6_categories,disability_3_categories,disability)

pop_region %<>%
  filter(Regions == "London") %>%
  rename(population = Observation) %>%
  clean_names() %>%
  select(age_6_categories,population)

disability_region %<>% 
  left_join(pop_region, by = "age_6_categories") %>%
    filter(disability_3_categories == "Disabled under the Equality Act") %>%
        select(age_6_categories,disability,population) %>%
        mutate( rate = disability / population)

# Calculate expected rates for London; append actual rates; calculate SDR
OA21_Pop_London <- read_csv("./data/census/custom-filtered-2023-06-03T13_25_11Z.csv")

OA21_SDR_London <- OA21_Pop_London %<>%
  clean_names() %>%
  left_join(disability_region) %>%
  mutate(expected = observation * rate) %>%
  select(output_areas_code,expected) %>%
  group_by(output_areas_code) %>%
  summarise(expected = sum(expected))

OA21_SDR_London %<>%
  left_join(ts038, by = c("output_areas_code" = "OA")) %>%
  select(output_areas_code, expected,ts0380002) %>%
  mutate(v42 = ts0380002 / expected * 100) %>%
  rename(OA = output_areas_code) %>%
  select(OA, v42)


#---------
# Percentage Variables
#---------

# Get a list of input variables that are not combinations or non percentages
v_s <- variable_lookup %>%
        filter((!str_detect(Variables, "&"))& is.na(Non_PCT)) %>%
        select(Variables) %>%
        pull()

v_t <- unique(sub("....$", "", v_s)) # table list
v_t <- map(v_t, get) # convert to a list of objects

v_s <- paste0(v_s,"_PCT") # Append _PCT to select percentages

# Create the temporary table to hold the percentages
tmp_table <- v_t %>% 
  reduce(full_join, by = "OA") %>%
  select(OA, all_of(v_s)) %>%
  rename_all(~str_replace_all(., "_PCT", "")) # remove the _PCT from column names

# Get the LOAC variable names from the LOAC lookup variable lookup table
V_name <- variable_lookup %>%
            filter(Variables %in% colnames(tmp_table)) %>%
            select(No.) %>%
            pull()
          
# Change the census variable names to the LOAC variable names
tmp_table %<>% 
  rename_with(~ c("OA",V_name), everything())


#---------
# Combined Variables
#---------

# v03 - Aged 5 to 14 years
v03 <- ts007a %>% select(OA, ts007a0003_PCT,ts007a0004_PCT) %>% 
  mutate(v03 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v03)

# v04 - Aged 25 to 44 years
v04 <- ts007a %>% select(OA, ts007a0007_PCT,ts007a0008_PCT,ts007a0009_PCT,ts007a0010_PCT) %>% 
  mutate(v04 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v04)

# v05 - Aged 45 to 64 years
v05 <- ts007a %>% select(OA, ts007a0011_PCT,ts007a0012_PCT,ts007a0013_PCT,ts007a0014_PCT) %>% 
  mutate(v05 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v05)

# v06 - Aged 65 to 84 years
v06 <- ts007a %>% select(OA, ts007a0015_PCT,ts007a0016_PCT,ts007a0017_PCT,ts007a0018_PCT) %>% 
  mutate(v06 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v06)

# v26   Separated or divorced
v26 <- ts002 %>% select(OA, ts0020010_PCT,ts0020013_PCT) %>% 
  mutate(v26 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v26)

# v28   Families with no children
v28 <- ts003 %>% select(OA,  ts0030008_PCT, ts0030012_PCT) %>% 
  mutate(v28 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v28)

# v29   Families with dependent children
v29 <- ts003 %>% select(OA, ts0030009_PCT,ts0030013_PCT,ts0030016_PCT) %>% 
  mutate(v29 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v29)

# v36   Flat, maisonette or apartment
v36 <- ts044 %>% select(OA, ts0440005_PCT,ts0440006_PCT,ts0440007_PCT) %>% 
  mutate(v36 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v36)
 
# v37   Ownership or shared ownership
v37 <- ts054 %>% select(OA,ts0540002_PCT,ts0540005_PCT) %>% 
  mutate(v37 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v37)

# v60 Unemployed 
v60 <- ts065 %>% select(OA,ts0650001) %>% 
  left_join(ts066) %>%
  select(OA,ts0650001,ts0660001) %>%
  mutate(v60 = 100 / ts0660001 * ts0650001) %>%
  select(OA, v60)


############################################################
# Assemble and Prepare Cluster Inputs
############################################################

#---------
# Combine All the Variables for London
#---------

# List of data
list_of_dfs <- list(OA_london, Non_PCT, OA21_SDR_London,tmp_table,v03,v04,v05,v06,v26,v28,v29,v36,v37,v60)

# Combine
OA_london <- list_of_dfs %>%
  reduce(left_join, by = "OA")

#---------
# Calculate the Inverse hyperbolic sine and range standardize
#---------

OA_london %<>%
  mutate_at(vars(-(1:3)), ~asinh(.)) %>%
  mutate_at(vars(-(1:3)), ~rescale(.))

# Save the raw input data
write_parquet(OA_london, "./data/OA_Input_london.parquet")

############################################################
# Create Supergroup Clusters
############################################################

h2o.init(max_mem_size="30G")
OA_london_h20 <- as.h2o(OA_london)
v_used <-  colnames(OA_london)[-1:-3]

# Clustering Function
kmeans_func <- function(x) {
  results_run <- h2o.kmeans(training_frame = OA_london_h20, k = 7, x = v_used, init = "Random",max_iterations=1000,standardize = FALSE)
  list(results = results_run, wss = h2o.tot_withinss(results_run))
}

ptm <- proc.time()
# Apply the function for each element in the vector
results_list <- map(1:1000, ~kmeans_func(.x))
proc.time() - ptm


# Find the result with the minimum wss
best_result <- reduce(results_list, function(a, b) {
  if(b$wss < a$wss) b else a
})

# Extract the best results
results <- best_result$results
wss <- best_result$wss

# Cluster Assignments
cluster_assignments <- OA_london %>%
  select(OA)
  
results_lookup <- h2o.predict(results, OA_london_h20) %>%
  as_tibble()

cluster_assignments %<>%
  bind_cols(results_lookup)

cluster_assignments_SF <- OA_2021_Boundary %>%
 left_join(cluster_assignments, by = c("OA21CD" = "OA")) %>%
   filter(!is.na(predict))

st_write(cluster_assignments_SF, "./map/LOAC_SuperGroup.gpkg", driver = "GPKG")
write_parquet(cluster_assignments, "./data/LOAC_SuperGroup.parquet")

h2o.shutdown()

############################################################
# Create Group Clusters
############################################################

#---------
# Prepare the Group clustering input data
#---------

# Read the Super Group Lookup
cluster_assignments <- read_parquet("./data/LOAC_SuperGroup.parquet")

# Append data to SG lookup
Group_Input_Data <- cluster_assignments %>%
  left_join(OA_london)

# Create a list of Group inputs split by Super Group
SG <- Group_Input_Data %>% pull(SG)
Group_Input_Data_Split <- split(Group_Input_Data, f = SG)

#---------
# Create Group Clusters
#---------

# Setup
h2o.init(max_mem_size="30G")
v_used <-  colnames(OA_london)[-1:-3]

# List of input tibbles TableA to TableG along with their corresponding k values
tables_list <- list(
  list(tbl = Group_Input_Data_Split[["A"]], k = 3),  
  list(tbl = Group_Input_Data_Split[["B"]], k = 2),  
  list(tbl = Group_Input_Data_Split[["C"]], k = 2),  
  list(tbl = Group_Input_Data_Split[["D"]], k = 3),  
  list(tbl = Group_Input_Data_Split[["E"]], k = 2),  
  list(tbl = Group_Input_Data_Split[["F"]], k = 2),  
  list(tbl = Group_Input_Data_Split[["G"]], k = 2)   
)

# Group Clustering Function
kmeans_func_groups <- function(x) {
  results_run <- h2o.kmeans(training_frame = tbl_h20, k = clusters, x = v_used, init = "Random",max_iterations=100,standardize = FALSE)
  list(results = results_run, wss = h2o.tot_withinss(results_run))
}

ptm <- proc.time()
# Applying the function for each Super Group and storing cluster assignments
cluster_assignments_Groups <- vector("list", length(tables_list))

for (i in seq_along(tables_list)) {
  
  # Select the Super Group data subset
  tbl_h20 <- as.h2o(tables_list[[i]]$tbl)
  
  # Select the number of clusters for the Group
  clusters <- tables_list[[i]]$k
  
  # Apply the function for each element in the vector
  results_list <- map(1:1000, ~kmeans_func_groups(.x))
  
  # Find the result with the minimum wss
  best_result <- reduce(results_list, function(a, b) {
    if(b$wss < a$wss) b else a
  })
  
  # Extract the best results
  results <- best_result$results
  
  # Find the cluster assignments for OA
  results_lookup <- h2o.predict(results, tbl_h20) %>%
    as_tibble()
  
  # Create OA lookup
  tmp_cluster_assignments_Groups <- tbl_h20 %>%
    as_tibble() %>% 
    select(OA) %>%
    bind_cols(results_lookup)
  
  # Append OA lookup to the results list
  cluster_assignments_Groups[[i]] <- tmp_cluster_assignments_Groups
  
  # Remove tmp items
  rm(tbl_h20,clusters,results_list,best_result,results,results_lookup,tmp_cluster_assignments_Groups)
  
}

proc.time() - ptm

h2o.shutdown()

#---------
# Create Group Lookups
#---------

# Create Lookup for Groups
cluster_assignments_Groups <- bind_rows(cluster_assignments_Groups)


# Create Groups lookup
cluster_assignments_Groups %<>%
  left_join(cluster_assignments) %>%
  mutate(predict = predict + 1 ) %>%
  unite(G, SG, predict, sep = "",remove = FALSE) %>%
  select(-predict)

# Create outputs

cluster_assignments_SF <- OA_2021_Boundary %>%
  left_join(cluster_assignments_Groups, by = c("OA21CD" = "OA")) %>%
  filter(!is.na(SG))

st_write(cluster_assignments_SF, "./map/LOAC_Group.gpkg", driver = "GPKG")
write_parquet(cluster_assignments_Groups, "./data/LOAC_Group.parquet")

st_write(LAD_Dec21, "./map/LAD_Dec21.gpkg", driver = "GPKG")


############################################################
# Create Descriptive Material
############################################################

cluster_assignments_Groups <- read_parquet("./data/LOAC_Group.parquet")

#---------
# Create Index Scores for Super Groups and Groups
#---------

# Get data
OA_london <- OA_Region %>%
  filter( rgn22nm == "London") %>%
  select(oa21cd) %>%
  left_join(OA_UTLAD) %>%
  rename(OA = oa21cd) %>%
  select(OA, utla22cd,utla22nm)

# List of data
list_of_dfs <- list(OA_london, Non_PCT, OA21_SDR_London,tmp_table,v03,v04,v05,v06,v26,v28,v29,v36,v37,v60)

# Combine
OA_london <- list_of_dfs %>%
  reduce(left_join, by = "OA")

# Append Clusters
OA_london %<>%
  left_join(cluster_assignments_Groups)

# Calculate Index Scores and transpose Groups
IS_Groups <- OA_london %>%
                group_by(G) %>%
                summarise(across(where(is.numeric), mean, na.rm = TRUE))%>%
                ungroup() %>%
                mutate(across(where(is.numeric), ~ .x / mean(.x, na.rm = TRUE) * 100)) #index score

IS_Groups %<>%
  pivot_longer(-G, names_to = "no", values_to = "Value") %>%
  pivot_wider(names_from = G, values_from = Value)

# Append variable names
Index_Scores_Groups <- variable_lookup %>%
  clean_names() %>%
  select(no, variable_name,domain) %>%
  left_join(IS_Groups)

# Calculate Index Scores and transpose Supergroups
IS_SGroups <- OA_london %>%
  group_by(SG) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))%>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~ .x / mean(.x, na.rm = TRUE) * 100)) #index score

IS_SGroups %<>%
  pivot_longer(-SG, names_to = "no", values_to = "Value") %>%
  pivot_wider(names_from = SG, values_from = Value)

# Append variable names
Index_Scores_SGroups <- variable_lookup %>%
  clean_names() %>%
  select(no, variable_name,domain) %>%
  left_join(IS_SGroups)

#---------
# Create Group Index Score Plots
#---------

# Group Index Score Table
Index_Scores_Groups_L <- Index_Scores_Groups %>% 
  pivot_longer(cols = c(A1, A2, A3, B1, B2, C1, C2, D1, D2, D3, E1, E2, F1, F2, G1, G2), names_to = "new_variable", values_to = "value") %>%
  mutate(
    category = factor(case_when(
      value < 80 ~ "<80",
      value >= 80 & value <= 120 ~ "80-120",
      value > 120 & value <= 200 ~ "120-200",
      value > 200 ~ "200+",
      TRUE ~ "Other"
    ), levels = rev(c("<80", "80-120", "120-200", "200+"))),
    
    variable_name = factor(variable_name, levels = Index_Scores_Groups$variable_name)
  ) 


my_colors <- c("<80" = "#2a9d8f", "80-120" = "#e9c46a", "120-200" = "#f4a261", "200+" = "#e76f51")

demographic <- Index_Scores_Groups_L %>%
  filter(domain == "Demographic") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = category)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  geom_vline(xintercept = 3.5,lwd=1.3) +
  geom_vline(xintercept = 5.5,lwd=1.3) +
  geom_vline(xintercept = 7.5,lwd=1.3) +
  geom_vline(xintercept = 10.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 14.5,lwd=1.3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

employment <- Index_Scores_Groups_L %>%
  filter(domain == "Employment") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = category)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  labs(x = "Super Group", y = NULL) +
  geom_vline(xintercept = 3.5,lwd=1.3) +
  geom_vline(xintercept = 5.5,lwd=1.3) +
  geom_vline(xintercept = 7.5,lwd=1.3) +
  geom_vline(xintercept = 10.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 14.5,lwd=1.3) +
  coord_fixed() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

Ethnicity_Origins <- Index_Scores_Groups_L %>%
  filter(domain == "Ethnicity and Origins") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = category)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  geom_vline(xintercept = 3.5,lwd=1.3) +
  geom_vline(xintercept = 5.5,lwd=1.3) +
  geom_vline(xintercept = 7.5,lwd=1.3) +
  geom_vline(xintercept = 10.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 14.5,lwd=1.3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

Living_Arrangements <- Index_Scores_Groups_L %>%
  filter(domain == "Living Arrangements") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = category)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  geom_vline(xintercept = 3.5,lwd=1.3) +
  geom_vline(xintercept = 5.5,lwd=1.3) +
  geom_vline(xintercept = 7.5,lwd=1.3) +
  geom_vline(xintercept = 10.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 14.5,lwd=1.3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

Other <- Index_Scores_Groups_L %>%
  filter(domain == "Other") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = category)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  geom_vline(xintercept = 3.5,lwd=1.3) +
  geom_vline(xintercept = 5.5,lwd=1.3) +
  geom_vline(xintercept = 7.5,lwd=1.3) +
  geom_vline(xintercept = 10.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 14.5,lwd=1.3) +
  theme(axis.title.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

Usual_Residence <- Index_Scores_Groups_L %>%
  filter(domain == "Usual Residence") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = category)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors,name = "Index") +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  geom_vline(xintercept = 3.5,lwd=1.3) +
  geom_vline(xintercept = 5.5,lwd=1.3) +
  geom_vline(xintercept = 7.5,lwd=1.3) +
  geom_vline(xintercept = 10.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 14.5,lwd=1.3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.background = element_rect(fill = "#f8f9fa"),
        legend.background = element_rect(fill = "transparent")) 

Usual_Residence / Ethnicity_Origins / demographic / employment / 
  Living_Arrangements / Other + plot_layout(guides = 'collect')

ggsave("index_plot_groups.png", height = 30, width = 20, units = "cm")

#---------
# Create Supergroup Index Score Plots
#---------

# Supergroup Index Score Table
Index_Scores_L <- Index_Scores_SGroups  %>% 
  pivot_longer(cols = c(A, B, C, D, E, F, G), names_to = "new_variable", values_to = "value") %>%
  mutate(
    category = factor(case_when(
      value < 80 ~ "<80",
      value >= 80 & value <= 120 ~ "80-120",
      value > 120 & value <= 200 ~ "120-200",
      value > 200 ~ "200+",
      TRUE ~ "Other"
    ), levels = rev(c("<80", "80-120", "120-200", "200+"))),
    
    variable_name = factor(variable_name, levels = Index_Scores_SGroups$variable_name)
  ) 


my_colors <- c("<80" = "#2a9d8f", "80-120" = "#e9c46a", "120-200" = "#f4a261", "200+" = "#e76f51")


demographic <- Index_Scores_L %>%
  filter(domain == "Demographic") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = category)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

employment <- Index_Scores_L %>%
  filter(domain == "Employment") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = category)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  labs(x = "Super Group", y = NULL) +
  coord_fixed() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

Ethnicity_Origins <- Index_Scores_L %>%
  filter(domain == "Ethnicity and Origins") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = category)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

Living_Arrangements <- Index_Scores_L %>%
  filter(domain == "Living Arrangements") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = category)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

Other <- Index_Scores_L %>%
  filter(domain == "Other") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = category)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  theme(axis.title.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

Usual_Residence <- Index_Scores_L %>%
  filter(domain == "Usual Residence") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = category)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors,name = "Index") +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.background = element_rect(fill = "#f8f9fa"),
        legend.background = element_rect(fill = "transparent")) 

Usual_Residence / Ethnicity_Origins / demographic / employment / 
  Living_Arrangements / Other + plot_layout(guides = 'collect')

ggsave("index_plot.png", height = 30, width = 20, units = "cm")

#---------
# Create Merged Index Plot for Supergroups and Groups
#---------

# Create two new tables as copies
Index_Scores_Compare <- Index_Scores_SGroups %>% select(A:G)
Index_Scores_Groups_Compare <- Index_Scores_Groups %>% select(A1:G2)

# Recode each column into the plot categories
recode_numeric <- function(x) {
  case_when(
    x < 80 ~ "<80",
    x >= 80 & x <= 120 ~ "80-120",
    x > 120 & x <= 200 ~ "120-200",
    x > 200 ~ "200+",
    TRUE ~ NA_character_
  )
}

Index_Scores_Compare <- Index_Scores_Compare %>% mutate_all(recode_numeric)
Index_Scores_Groups_Compare <- Index_Scores_Groups_Compare %>% mutate_all(recode_numeric)

# Create an empty table
Index_Scores_Groups_SuperGroups_Compare <- Index_Scores_Groups_Compare

# Loop over the column names in Index_Scores_Groups_Compare
for (col_name in names(Index_Scores_Groups_Compare)) {
  # Extract the first letter of the column name
  first_letter <- substr(col_name, 1, 1)
  # If a column matches the first letter, compare the two columns
  if (first_letter %in% names(Index_Scores_Compare)) {
    # Create a new column in Index_Scores_Groups_SuperGroups_Compare
    Index_Scores_Groups_SuperGroups_Compare[[col_name]] <- ifelse(Index_Scores_Groups_Compare[[col_name]] == Index_Scores_Compare[[first_letter]], "Same", Index_Scores_Groups_Compare[[col_name]])
  }
}

# Append the variable names
Index_Scores_Groups_SuperGroups_Compare <- Index_Scores_Groups %>%
  select(no, variable_name,domain) %>%
  bind_cols(Index_Scores_Groups_SuperGroups_Compare)

# Group / SuperGroup Comparison Index Score Table
Index_Scores_Comparison_L <- Index_Scores_Groups_SuperGroups_Compare %>% 
  pivot_longer(cols = c(A1, A2, A3, B1, B2, C1, C2, D1, D2, D3, E1, E2, F1, F2, G1, G2), names_to = "new_variable", values_to = "value") %>%
  mutate(variable_name = factor(variable_name, levels = Index_Scores_Groups_SuperGroups_Compare$variable_name))

Index_Scores_Comparison_L$value <- factor(Index_Scores_Comparison_L$value, levels = rev(c("Same","<80", "80-120", "120-200", "200+")))

my_colors <- c("<80" = "#2a9d8f", "80-120" = "#e9c46a", "120-200" = "#f4a261", "200+" = "#e76f51","Same"= "#edeff2")

demographic <- Index_Scores_Comparison_L %>%
  filter(domain == "Demographic") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  geom_vline(xintercept = 3.5,lwd=1.3) +
  geom_vline(xintercept = 5.5,lwd=1.3) +
  geom_vline(xintercept = 7.5,lwd=1.3) +
  geom_vline(xintercept = 10.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 14.5,lwd=1.3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

employment <- Index_Scores_Comparison_L %>%
  filter(domain == "Employment") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  labs(x = "Super Group", y = NULL) +
  geom_vline(xintercept = 3.5,lwd=1.3) +
  geom_vline(xintercept = 5.5,lwd=1.3) +
  geom_vline(xintercept = 7.5,lwd=1.3) +
  geom_vline(xintercept = 10.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 14.5,lwd=1.3) +
  coord_fixed() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

Ethnicity_Origins <- Index_Scores_Comparison_L %>%
  filter(domain == "Ethnicity and Origins") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  geom_vline(xintercept = 3.5,lwd=1.3) +
  geom_vline(xintercept = 5.5,lwd=1.3) +
  geom_vline(xintercept = 7.5,lwd=1.3) +
  geom_vline(xintercept = 10.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 14.5,lwd=1.3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

Living_Arrangements <- Index_Scores_Comparison_L %>%
  filter(domain == "Living Arrangements") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  geom_vline(xintercept = 3.5,lwd=1.3) +
  geom_vline(xintercept = 5.5,lwd=1.3) +
  geom_vline(xintercept = 7.5,lwd=1.3) +
  geom_vline(xintercept = 10.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 14.5,lwd=1.3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

Other <- Index_Scores_Comparison_L %>%
  filter(domain == "Other") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors) +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  geom_vline(xintercept = 3.5,lwd=1.3) +
  geom_vline(xintercept = 5.5,lwd=1.3) +
  geom_vline(xintercept = 7.5,lwd=1.3) +
  geom_vline(xintercept = 10.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 14.5,lwd=1.3) +
  theme(axis.title.x=element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#f8f9fa")) 

Usual_Residence <- Index_Scores_Comparison_L %>%
  filter(domain == "Usual Residence") %>%
  ggplot(aes(x = new_variable, y = variable_name, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = my_colors,name = "Index") +
  coord_fixed() +
  labs(x = "Super Group", y = NULL) +
  geom_vline(xintercept = 3.5,lwd=1.3) +
  geom_vline(xintercept = 5.5,lwd=1.3) +
  geom_vline(xintercept = 7.5,lwd=1.3) +
  geom_vline(xintercept = 10.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 12.5,lwd=1.3) +
  geom_vline(xintercept = 14.5,lwd=1.3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.background = element_rect(fill = "#f8f9fa"),
        legend.background = element_rect(fill = "transparent")) 

Usual_Residence / Ethnicity_Origins / demographic / employment / 
  Living_Arrangements / Other + plot_layout(guides = 'collect')

ggsave("index_plot_comparisons.png", height = 30, width = 20, units = "cm")

#---------
# Check top and bottom 10 features for Supergroups and Groups
#---------

# Ranking Functions
rank_rows_top <- function(df, column_name) {
  column_name <- sym(column_name)
  df %>%
    arrange(desc( !!sym(column_name) )) %>%
    filter(!!sym(column_name) > 120) %>%
    select(variable_name,  !!sym(column_name) )
}

rank_rows_bottom <- function(df, column_name) {
  column_name <- sym(column_name)
  df %>%
    arrange( !!sym(column_name) ) %>%
    filter(!!sym(column_name) <80) %>%
    select(variable_name,  !!sym(column_name) )
}

# List of column names A-G
column_names <- c("A", "B", "C", "D", "E", "F", "G")

# Create a list of data frames, each containing the variable_name and one of the columns A-G
ranked_index_list_top_SG <- map(column_names, ~ rank_rows_top(Index_Scores_SGroups, .))
ranked_index_list_bottom_SG <- map(column_names, ~ rank_rows_bottom(Index_Scores_SGroups, .))

column_names <- c("A1", "A2", "A3", "B1", "B2", "C1", "C2", "D1", "D2", "D3", "E1", "E2", "F1", "F2", "G1", "G2")

# Create a list of data frames, each containing the variable_name and one of the columns A-G
ranked_index_list_top_G <- map(column_names, ~ rank_rows_top(Index_Scores_Groups, .))
ranked_index_list_bottom_G <- map(column_names, ~ rank_rows_bottom(Index_Scores_Groups, .))

############################################################
# Create Maps for Supergroups and Groups
############################################################

#---------
# Icon Maps
#---------

cluster_assignments_SF <- st_read("./map/LOAC_Group.gpkg")
  
# Map Cluster Function  
  plot_and_save <- function(cluster) {
    plot <- ggplot() +
      geom_sf(data = cluster_assignments_SF, fill = "#1b263b", color = "#1b263b") +
      geom_sf(data = dplyr::filter(cluster_assignments_SF, SG == cluster), fill = "#415a77", color = "#415a77") +
      theme_void()
    
    ggsave(filename = paste0(cluster, ".png"), plot = plot)
  }
  
  # Supergroups
  clusters <- LETTERS[1:7]
  purrr::map(clusters, plot_and_save)
  
  # Groups
  clusters <- c("A1", "A2", "A3", "B1", "B2", "C1", "C2", "D1", "D2", "D3", "E1", "E2", "F1", "F2", "G1", "G2")
  purrr::map(clusters, plot_and_save)

  #---------
  # GLA Supergroup and Group Map
  #---------

SGroup_palette <- c("A" = "#66c2a5", "B" = "#fc8d62", "C" = "#8da0cb", "D" = "#e78ac3", "E" = "#a6d854", "F" = "#ffd92f", "G" = "#D9dddf","black" ="Not Built Up" )

LonLAD <- paste0("E090000", sprintf("%02d", 1:33))

ggplot() +
  geom_sf(data = cluster_assignments_SF, aes(fill = SG),color = NA) +
  scale_fill_manual(values = SGroup_palette,drop = FALSE,name = "SG") +
  geom_sf(data = dplyr::filter(LAD_Dec21, LAD21CD %in% LonLAD), fill = NA, color = "black")+
  theme_void() 

Group_palette <- c("A1" = "#539B84", "A2" = "#97C3B5", "A3" = "#C5FFEC",
                    "B1" = "#CA714E", "B2" = "#FC9872",
                    "C1" = "#63708E", "C2" = "#C6D0E5",
                    "D1" = "#B96E9C", "D2" = "#EEADD5", "D3" = "#FFB9DA",
                    "E1" = "#74973B", "E2" = "#CAE898",
                    "F1" = "#CCAE26", "F2" = "#FFE882",
                    "G1" = "#7D7D7D", "G2" = "#D1D1D1",
                   "Not Built Up" = "#484848")

ggplot() +
  geom_sf(data = cluster_assignments_SF, aes(fill = G),color = NA) +
  scale_fill_manual(values = Group_palette,drop = FALSE,name = "SG") +
  geom_sf(data = dplyr::filter(LAD_Dec21, LAD21CD %in% LonLAD), fill = NA, color = "black")+
  theme_void()
#





#---------
# Borough Atlas
#---------

#Append Borough codes
OA_LAD <- cluster_assignments_SF %>%
  st_centroid() %>%
  st_join(LAD_Dec21) %>%
  st_drop_geometry() %>%
  select(OA21CD, LAD21CD) %>%
  left_join(cluster_assignments_SF) %>%
  st_as_sf(crs =27700) 


# Import geopackage for built environment (https://osdatahub.os.uk/downloads/open/BuiltUpAreas)
built <- st_read("./map/OS_Open_Built_Up_Areas.gpkg", layer = "OS_Open_Built_Up_Areas", quiet = TRUE)

# Import geopackage for greenspace (https://osdatahub.os.uk/downloads/open/OpenGreenspace)
greenspace <- st_read("./map/opgrsp_gb.gpkg", layer = "greenspace_site", quiet = TRUE)

# Calculate Area
greenspace %<>%
  mutate(area = st_area(.)) %>%
  filter(area > units::set_units(40000, m^2))

# Smooth jagged edges
built %<>%
  st_buffer(dist=21) %>%
  st_buffer(dist=-20) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 15)

# Perform the clip operation to trim LSOA to built area
OA_LAD_Built <- st_intersection(OA_LAD, built)

# Clean up attributes
OA_LAD_Built %<>%
  select(OA21CD, LAD21CD,G, SG)

# Cut out greenspace
OA_LAD_Built %<>%
  st_difference(st_union(greenspace))

# Write lsoa_21_built to geopackage
st_write(OA_LAD_Built, "map/LOAC_Group_built.gpkg", layer = "OA_LAD_Built", driver = "GPKG")




# Get train station data
tx <- LAD_Dec21 %>%
      dplyr::filter(LAD21CD == "E09000023") %>%
      st_transform(4326)


Transport_Tmp <- opq( st_bbox(tx)) %>% 
  add_osm_feature(key = "railway", value = "station") %>% 
  add_osm_feature(key = "network", value = c("Underground","Docklands","Rail","Tram","DLR"),value_exact=FALSE) %>%
  osmdata_sf()
  

c("Underground","Docklands","Rail","Tram","DLR")

Transport_Tmp <- Transport_Tmp$osm_points %>%
                  select(name) %>%
                  st_intersection(tx) %>%
                  st_transform(27700) %>%
                  mutate(x = sf::st_coordinates(.)[,1],
                         y = sf::st_coordinates(.)[,2])
# Plot

#select the LAD and amend levels
LAD <- dplyr::filter(OA_LAD_Built, LAD21CD == "E09000031") %>% 
  mutate(G = factor(G, levels = c("A1", "A2", "A3", "B1", "B2", "C1", "C2", "D1", "D2", "D3", "E1", "E2", "F1", "F2", "G1", "G2","Not Built Up")))

ggplot() +
  geom_sf(data = dplyr::filter(LAD_Dec21, LAD21CD == "E09000031"), fill = "#484848", color = NA)+
  geom_sf(data = LAD, aes(fill = G),lwd = 0.1,color = "black") +
  scale_fill_manual(values = Group_palette,drop = FALSE, name = "Group") +
  geom_sf(data = dplyr::filter(LAD_Dec21, LAD21CD == "E09000031"), fill = NA, lwd = 0.8,color = "black")+
  geom_sf(data = Transport_Tmp, color = "red") +
  geom_text_repel(data = Transport_Tmp, bg.color = "white",aes(x = x, y = y, label = name),size=2) +
  ggspatial::annotation_scale() +
  theme_void()

ggsave(paste0(dplyr::filter(LAD_Dec21, LAD21CD == "E09000031") %>% select(LAD21NM) %>% st_drop_geometry() %>% pull(),".pdf"),
       width = 20,
       height = 20,
       units = 'cm')

    






















create_map <- function(LAD21CD_val, LAD_Dec21, OA_LAD_Built, Group_palette) {
  
  # Filter and transform
  tx <- LAD_Dec21 %>%
    dplyr::filter(LAD21CD == LAD21CD_val) %>%
    st_transform(4326)
  
  # OSM data
  Transport_Tmp <- opq(st_bbox(tx)) %>%
    add_osm_feature(key = "railway", value = "station") %>% 
    add_osm_feature(key = "network", value = c("Underground","Docklands","Rail","Tram","DLR"),value_exact=FALSE) %>%
    osmdata_sf()
  
  # Check if Transport_Tmp$osm_points is empty
  if (nrow(Transport_Tmp$osm_points) == 0) {
    Transport_Tmp <- NULL
    warning(paste("No data retrieved for LAD21CD:", LAD21CD_val))
  } else {
    Transport_Tmp <- Transport_Tmp$osm_points %>%
      select(name) %>%
      st_intersection(tx) %>%
      st_transform(27700) %>%
      mutate(x = sf::st_coordinates(.)[, 1], y = sf::st_coordinates(.)[, 2])
  }
  
  
  #select the LAD and amend levels
  LAD <- dplyr::filter(OA_LAD_Built, LAD21CD == LAD21CD_val) %>% 
    mutate(G = factor(G, levels = c("A1", "A2", "A3", "B1", "B2", "C1", "C2", "D1", "D2", "D3", "E1", "E2", "F1", "F2", "G1", "G2","Not Built Up")))
  
  
  # Plot
  plot <- ggplot() +
    geom_sf(data = dplyr::filter(LAD_Dec21, LAD21CD == LAD21CD_val), fill = "#484848", color = NA) +
    geom_sf(data = LAD, aes(fill = G), lwd = 0.1, color = "black") +
    scale_fill_manual(values = Group_palette, drop = FALSE, name = "Group") +
    geom_sf(data = dplyr::filter(LAD_Dec21, LAD21CD == LAD21CD_val), fill = NA, lwd = 0.8, color = "black") +
    ggspatial::annotation_scale() +
    theme_void()
  
  # Add the transport layers conditionally, if data exists
  if (!is.null(Transport_Tmp)) {
    plot <- plot + 
      geom_sf(data = Transport_Tmp, color = "red") +
      geom_text_repel(data = Transport_Tmp, bg.color = "white", aes(x = x, y = y, label = name), size = 2)
  }
  
  # Save
  ggsave(paste0(dplyr::filter(LAD_Dec21, LAD21CD == LAD21CD_val) %>% select(LAD21NM) %>% st_drop_geometry() %>% pull(), ".pdf"),
         plot = plot,
         width = 20,
         height = 20,
         units = 'cm')
}

# Applying to each value in LonLAD list
lapply(LonLAD, function(LAD21CD_val) {
  create_map(LAD21CD_val, LAD_Dec21, OA_LAD_Built, Group_palette)
})




















