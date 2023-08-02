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

# Output Areas (Dec 2021) Boundaries Generalised Clipped EW (BGC)
OA_2021_Boundary <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Output_Areas_Dec_2021_Boundaries_Generalised_Clipped_EW_BGC_2022/FeatureServer/0/query?where=1%3D1&outFields=OA21CD&outSR=4326&f=json")

# Output Area to Region (December 2021) Lookup in England and Wales
OA_Region <- read_csv("https://www.arcgis.com/sharing/rest/content/items/efda0d0e14da4badbd8bdf8ae31d2f00/data")

# Output Area to Upper-Tier Local Authorities (December 2021) Lookup in England and Wales
OA_UTLAD <- read_csv("https://www.arcgis.com/sharing/rest/content/items/4393e36fa6184b0fb1b2562d98db1da6/data")

# Local Authority Districts (December 2021) GB BGC
 <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2021_GB_BGC_2022/FeatureServer/0/query?where=1%3D1&outFields=LAD21CD,LAD21NM&outSR=4326&f=json")



# Create LOAC OA lookup:

OA_london <- OA_Region %>%
              filter( rgn22nm == "London") %>%
              select(oa21cd) %>%
              left_join(OA_UTLAD) %>%
              rename(OA = oa21cd) %>%
              select(OA, utla22cd,utla22nm)
  
  


# Pull Bulk Census Data

########################################################
# Pull in census 2021 data, calculate PCT
#########################################################

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

###############################################################################


#save.image("~/source_data.RData")


############################################################
# Collate the input data
############################################################


#Import the lookup

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
    # mutate(age_recode = case_when(
    # age_6_categories %in% c("Aged 16 to 24 years",
    #                               "Aged 25 to 34 years",
    #                               "Aged 35 to 49 years",
    #                               "Aged 50 to 64 years") ~ "Aged 16-64",
    #   TRUE ~ "Aged 0-15 or 65+"
    # )) %>%
        select(age_6_categories,disability,population) %>%
    #    group_by(age_recode) %>%
    #    summarise(disability  = sum(disability),
    #              population = sum(population)) %>%
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

#Get the LOAC variable names from the LOAC lookup variable lookup table
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

#v03 - Aged 5 to 14 years
v03 <- ts007a %>% select(OA, ts007a0003_PCT,ts007a0004_PCT) %>% 
  mutate(v03 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v03)

#v04 - Aged 25 to 44 years
v04 <- ts007a %>% select(OA, ts007a0007_PCT,ts007a0008_PCT,ts007a0009_PCT,ts007a0010_PCT) %>% 
  mutate(v04 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v04)

#v05 - Aged 45 to 64 years
v05 <- ts007a %>% select(OA, ts007a0011_PCT,ts007a0012_PCT,ts007a0013_PCT,ts007a0014_PCT) %>% 
  mutate(v05 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v05)

#v06 - Aged 65 to 84 years
v06 <- ts007a %>% select(OA, ts007a0015_PCT,ts007a0016_PCT,ts007a0017_PCT,ts007a0018_PCT) %>% 
  mutate(v06 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v06)

#v26   Separated or divorced
v26 <- ts002 %>% select(OA, ts0020010_PCT,ts0020013_PCT) %>% 
  mutate(v26 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v26)

#v28   Families with no children
v28 <- ts003 %>% select(OA,  ts0030008_PCT, ts0030012_PCT) %>% 
  mutate(v28 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v28)

#v29   Families with dependent children
v29 <- ts003 %>% select(OA, ts0030009_PCT,ts0030013_PCT,ts0030016_PCT) %>% 
  mutate(v29 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v29)

#v36   Flat, maisonette or apartment
v36 <- ts044 %>% select(OA, ts0440005_PCT,ts0440006_PCT,ts0440007_PCT) %>% 
  mutate(v36 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v36)

#v37   Ownership or shared ownership
v37 <- ts054 %>% select(OA,ts0540002_PCT,ts0540005_PCT) %>% 
  mutate(v37 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v37)

#v60 Unemployed 
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
# Create Super Group Clusters
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
# Create Index Scores for Super Groups
############################################################

#Create Super Group Labels

mapping <- setNames(LETTERS, 0:6)

cluster_assignments %<>%
  rename(SG = predict) %>%
  mutate(SG = map_chr(SG, ~mapping[as.character(.x)]))


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


#Append Clusters
OA_london %<>%
  left_join(cluster_assignments)
  

#Calculate Index Scores and transpose
OA_london %<>%
  group_by(SG) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))%>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~ .x / mean(.x, na.rm = TRUE) * 100)) #index score
  #mutate(across(where(is.numeric), ~ ((.x / mean(.x, na.rm = TRUE) * 100) - 100))) # % average


OA_london %<>%
  pivot_longer(-SG, names_to = "no", values_to = "Value") %>%
  pivot_wider(names_from = SG, values_from = Value)


#Append names

Index_Scores <- variable_lookup %>%
  clean_names() %>%
  select(no, variable_name,domain) %>%
  left_join(OA_london)
  

# % Difference Graph



# ggbarplot(Index_Scores, x = "variable_name", y = "C",
#           fill = "domain",           # change fill color by mpg_level
#           color = "white",            # Set bar border colors to white
#           palette = "jco",            # jco journal color palett. see ?ggpar
#           sort.val = "asc",           # Sort the value in ascending order
#           sort.by.groups = FALSE,     # Don't sort inside each group
#           #x.text.angle = 90,          # Rotate vertically x axis texts
#           ylab = "% Difference from the London Average",
#           xlab = "",
#           legend.title = "Domain",
#           rotate = TRUE,
#           ggtheme = theme_minimal()
#           
# ) 


#Index Score Table

Index_Scores_L <- Index_Scores %>% 
  pivot_longer(cols = c(A, B, C, D, E, F, G), names_to = "new_variable", values_to = "value") %>%
  mutate(
    category = factor(case_when(
      value < 80 ~ "<80",
      value >= 80 & value <= 120 ~ "80-120",
      value > 120 & value <= 200 ~ "120-200",
      value > 200 ~ "200+",
      TRUE ~ "Other"
    ), levels = rev(c("<80", "80-120", "120-200", "200+"))),
    
    variable_name = factor(variable_name, levels = Index_Scores$variable_name)
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


############################################################
# Descriptions for Super Groups
############################################################

# Set API

Sys.setenv(OPENAI_API_KEY = "XX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")



# Setup Prompts

# Create a table of prompt scores
Prompt_Scores <- variable_lookup %>%
  clean_names() %>%
  select(gpt, no) %>%
  left_join(OA_london) %>%
  #select(-no) %>%
  mutate_if(is.numeric, round) %>%
  filter(! no %in% c("v01","v18","v23","v44","v68"))

Prompt_Scores %<>%
  mutate_at(vars(A:G), 
            list(Prompt = ~paste(Prompt_Scores$gpt, ":", ., sep="")))

P_1 <- "A geodemographics company is trying to explain the characteristics of a neighborhood to a new customer. They present a table of data comparing the characteristics of this cluster to the national average. A score of 100 means the neighborhood is equivalent to the national average, a score of 150 means the neighborhood one and a half times the national average, a score of 200 means the neighborhood is twice the national average, a score of 50 means the neighborhood is half of the national average, a score of 300 means the neighborhood is three times the national average. The neighborhood has the following characteristics, described in #DATA# below. Data are presented for each characteristic followed by a colon, and then a score. The description of the neighborhood should focus on characteristics that have scores which are greater than 120 or less than 80.\n\n #DATA#"

P_3 <- "\n\n In the third person and style of a commercial geodemographic company, write a description of the neighborhood in no more than 300 words. Don't mention the specific scores from the #DATA#, but use descriptive words to illustrate rates that are above or below the national average."



generate_prompt <- function(df, colname){
  df %>%
    pull({{colname}}) %>%
    paste(., collapse = "\n")
}

prompts <- lapply(c('A_Prompt', 'B_Prompt', 'C_Prompt', 'D_Prompt', 'E_Prompt', 'F_Prompt', 'G_Prompt'), generate_prompt, df = Prompt_Scores)

prompts <- tibble(P2 = unlist(prompts)) %>%
  mutate(P1 = P_1) %>%
  mutate(P3 = P_3) %>%
  unite("GPT_Prompt", P1,P2,P3, sep = "\n")


# convert the list to a single string with "\n\n" as separator
#prompt_text <- paste(P_1,prompts,P_3, collapse = "\n")

# write to the file
sink("GPT_Prompts.txt")
cat(unlist(prompts))
sink()


# Function to call API and get response
get_api_response <- function(row) {
  # Extract necessary information from row
  query <- paste(row['GPT_Prompt'])
  
  # Call API and get response
  response <- ask_chatgpt(query)
  
  # Return response
  return(response)
}


# Initialize an empty list to store the responses
prompts_returned  <- list()

# For loop to iterate over each row of the dataframe
for(i in 1:nrow(prompts)) {
  # Get the i-th row of the dataframe
  row <- prompts[i, ]
  
  # Call the function and store the response
  prompts_returned[[i]] <- get_api_response(row)
}









#In the style of a commercial geodemographic company; label this cluster description (give 7 different versions) and provide a one sentence summary of the desciption. The cluster is in London, so augment where possible with characteristics of london.  The label should be no more than 3 words. The description of the cluster is as follows:





############################################################
# Maps for Super Groups
############################################################


mapping <- setNames(LETTERS, 0:6)

cluster_assignments_SF %<>%
  rename(SG = predict) %>%
  mutate(SG = map_chr(SG, ~mapping[as.character(.x)]))

st_write(cluster_assignments_SF, "./map/LOAC_Group.gpkg", driver = "GPKG")

  
  
#Map Cluster Function  
  plot_and_save <- function(cluster) {
    plot <- ggplot() +
      geom_sf(data = cluster_assignments_SF, fill = "#1b263b", color = "#1b263b") +
      geom_sf(data = dplyr::filter(cluster_assignments_SF, SG == cluster), fill = "#415a77", color = "#415a77") +
      theme_void()
    
    ggsave(filename = paste0(cluster, ".png"), plot = plot)
  }
  
  # You can now call this function for multiple subsets using purrr::map
  clusters <- LETTERS[1:7]
  purrr::map(clusters, plot_and_save)
  
  
  
  
############################################################
# Create Groups
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



cluster_assignments_Groups %>%
    count(SG) %>%
      mutate(percentage = n / sum(n) * 100)





############################################################
# Create Index Scores for Groups
############################################################



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


# Calculate Index Scores and transpose
OA_london %<>%
  group_by(G) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))%>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~ .x / mean(.x, na.rm = TRUE) * 100)) #index score


OA_london %<>%
  pivot_longer(-G, names_to = "no", values_to = "Value") %>%
  pivot_wider(names_from = G, values_from = Value)


# Append variable names

Index_Scores_Groups <- variable_lookup %>%
  clean_names() %>%
  select(no, variable_name,domain) %>%
  left_join(OA_london)


#---------
# Create Group Plots
#---------


#Group Index Score Table

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



############################################################
# Create Descriptions for the Groups
############################################################



#---------
# Create Merged Index Plot for Super Group and Groups
#---------

# Create two new tables as copies
Index_Scores_Compare <- Index_Scores %>% select(A:G)
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

#Group / SuperGroup Comparison Index Score Table

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
# Create GPT Prompts
#---------



# Set API

Sys.setenv(OPENAI_API_KEY = "XX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")



# Setup Prompts

# Create a table of prompt scores

Prompt_Scores <- variable_lookup %>%
  clean_names() %>%
  select(gpt, no) %>%
  left_join(OA_london) %>%
  #select(-no) %>%
  mutate_if(is.numeric, round) %>%
  filter(! no %in% c("v01","v18","v23","v44","v68"))

Prompt_Scores %<>%
  mutate_at(vars(A1:G2), 
            list(Prompt = ~paste(Prompt_Scores$gpt, ":", ., sep="")))

#Create a table with the Super Group Descriptions

SG_Descriptions <- list(
  
  A = "These London neighbourhood residents are predominantly White, educated and secular. Many are employed in professional occupations and live in privately owned or rented terraced houses. These neighbourhoods house people of all ages, predominantly of White British and European extraction. Resident turnover is low. Religious affiliation is less common than the London average and more likely to be Christian if expressed. Homeownership rates, typically of terraced houses, are high and social renting is uncommon. Employment is typically in professional, managerial and associate professional or technical occupations and there are few full-time students. Level 4 qualifications are common. More households lack dependent children than have them which, considered alongside low levels of crowding and the over-all age structure indicates that many households maybe post child-rearing with an ageing demographic structure. Levels of illness are low, as is residence in communal establishments.",
  B = "Communities scattered throughout London with some tendency towards single, well-educated professionals living in flats.Embodying London living in most all respects except the relative absence of residents identifying with (non-Chinese) Asian groups, these neighbourhoods pervade all but some parts of west and north-east London. There is some tendency towards lower-than-average prevalence of families with dependent children, while occurrences of never-married individuals and single-person households are relatively common. The age distribution is skewed towards younger, single residents and couples without children.Social or private renting are slightly more common than the London average and flats are prevalent.Individuals typically work in professional and associated roles in public administration, education, and health rather than in elementary occupations in agriculture, energy, water, construction or manufacturing.Incidence of educational qualifications is slightly below the London average, and full-time students are also slightly less common than the London average. Individuals declaring no religion are more prevalent than the London average and incidence of difficulty in use of English is below the London average.",
  C = "London neighbourhoods featuring Indian, Pakistani, and Bangladeshi influences, with family-focused living in houses rather than flats. Outside of management and the professions, the labour force is very active across a full range of occupations, but some community members remain challenged by language barriers and overcrowding. Many residents of these neighbourhoods are of (non-Chinese) Asian descent, particularly India, Pakistan, and Bangladesh. Neighbourhoods are located across large areas of suburban west, north-east and south London.Detached, semi-detached and terraced houses are more prevalent than flats and socially rented housing is uncommon. Few residents live in communal establishments. Many families have dependent children, sometimes in overcrowded accommodation, and few households are ethnically mixed. Marriage rates are above the London average. The even age distribution, relative absence of individuals living alone and frequent incidence of households with children suggests that multi-generation households may be relatively common.Employment is often in skilled trades, sales and customer service occupations, and roles as process, plant, and machine operatives. Manufacturing and construction are well represented, along with employment in distribution, hotels, and restaurants. Many adults have only level 1, 2, or apprenticeship qualifications. English proficiency remains a challenge for some residents. Religious affiliation is relatively high.",
  D = "These Central London neighbourhood are home for a blend of young, educated professionals from diverse backgrounds. Residents are of prime working age and typically live in privately rented flats, some of them crowded. Adult residents of these neighbourhoods are typically aged 25 to 44, working full-time in professional, managerial or associate professional occupations. There are few families with dependent children. The predominantly Inner London neighbourhoods have an international character, including many residents born elsewhere in Europe alongside individuals identifying as of Chinese, Indian, Other Asian or mixed ethnicity heritage. Many individuals are never married, childless and/or living alone. Above average numbers of individuals, likely to be full-time students, live in communal establishments. Elsewhere, privately rented flats are the dominant housing type. Residents of these areas are well-qualified, with a significant number holding Level 4 or above qualifications. There is a correspondingly high level of individuals employed full-time in professional, managerial and associate professional or technical occupations. Employing industries are financial, real estate, professional, administration, and, to a lesser degree, transport and communications. Unemployment is uncommon.",
  E = "Predominantly located in Inner London, these community members are drawn from a wide range of ethnicities, with high incidence of African and Bangladeshi residents. Younger adults, many living with children, predominate, living in socially rented flats.Residents of these neighbourhoods together comprise a wide ethnic mix, with sizable numbers drawn from outside Europe, particularly in Africa and from Bangladesh. The proportion of residents identifying as White is well below the London average.Neighbourhood age profiles are skewed towards younger adults, and many families have children. English proficiency levels are below the London average. Marriage rates are low, and levels of separation or divorce are above the London average.Housing is predominantly in flats, and social renting the norm – few residents are owner occupiers. These units include much of the most overcrowded in London. Illness rates are above the London average, although levels of unpaid care provision are around the London average.Employment is in caring, leisure, other service occupations, sales and customer service, or process, plant, and machine operation. Part time working and full time student study are common. Levels of unemployment are slightly above the London average. Most residents have not progressed beyond Level 1 or 2 educational qualifications or apprenticeships.",
  F = "Family-oriented living, concentrated multi-ethnic neighbourhoods throughout London. Many residents identify as Black African and many families have children. Employment is across the labour market, apart from professional or managerial occupations. Mainly located beyond Inner London, many families in these neighbourhoods have young children. Socially rented or owner occupied housing is the norm, in terraced, semi-detached or detached units. While over-all residential densities are low, overcrowding is also prevalent locally. Residents are drawn from a range of ethnic minorities, with many residents identifying as Black and above average numbers born in Africa. Numbers of Chinese, Indian and White ethnic groups are below the London average.Levels of proficiency in English are below the London average. Levels of separation or divorce and incidence of long-term illness are both above average. Education is typically limited to Level 1, 2, or apprenticeship qualifications. Few residents work in professional or managerial occupations but the employment structure is otherwise diverse: it includes skilled trades, caring, leisure and other service occupations, sales and customer service occupations, and work as process, plant, and machine operatives. Employment in the construction industry is particularly common.",
  G = "Outer London neighbourhoods mainly comprising White, UK-born owner-occupiers living in owned detached or semi-detached houses.The age distribution of these neighbourhoods is skewed towards older age groups, although few residents live alone or in communal establishments and numbers of dependent children are around the London average. Owner occupation is the norm, as is residence in detached or semi-detached houses. Residential densities are low and many households have spare rooms. Most residents were born in the UK and, aside from some members of Chinese and Indian ethnicities, identify as White British. Mixed ethnicity households are rare.Incidence of married couples are higher than the London average and few individuals have never been married. A large proportion of those individuals still in employment work in administrative and secretarial occupations, as well as in the construction industry. Few residents are students and many households own more than one car."
)

P3 <- tibble(
  G = c("A1", "A2", "A3", "B1", "B2", "C1", "C2", "D1", "D2", "D3", "E1", "E2", "F1", "F2", "G1", "G2"),
  SG = substr(G, 1, 1),
  P3 = unlist(SG_Descriptions[SG])) %>% select(P3) 



# Create contextual and direction paragraphs

P_0 <- "\n\n A geodemographic company is trying to explain the characteristics of a geodemographic cluster to a new customer. They present a table of data comparing the characteristics of this cluster to the national average. A score of 100 means the neighborhood is equivalent to the national average, a score of 150 means the neighborhood one and a half times the national average, a score of 200 means the neighborhood is twice the national average, a score of 50 means the neighborhood is half of the national average, a score of 300 means the neighborhood is three times the national average. The neighborhood has the following characteristics, described in #DATA# below. Data are presented for each characteristic followed by a colon, and then a score. The description of the neighborhood should focus on characteristics that have scores which are greater than 120 or less than 80.\n\n #DATA#"
P_2 <- "\n\n This geodemographic classification is hierarchical, organised into Super Groups (which are more aggregate) and Groups. The cluster they are trying to describe is a Group, which belongs to a Super Group which is described as:"
P_4 <- "\n\n In the third person and in the style of the Super Group description, write a description of the neighborhood in no more than 300 words. Don't mention the specific scores from the #DATA#, but use descriptive words to illustrate rates that are above or below the national average."


# Create prompt data tables

generate_prompt <- function(df, colname){
  df %>%
    pull({{colname}}) %>%
    paste(., collapse = "\n")
}

prompts <- lapply(c("A1_Prompt","A2_Prompt","A3_Prompt","B1_Prompt","B2_Prompt","C1_Prompt","C2_Prompt","D1_Prompt","D2_Prompt","D3_Prompt","E1_Prompt","E2_Prompt","F1_Prompt","F2_Prompt","G1_Prompt","G2_Prompt"), generate_prompt, df = Prompt_Scores)

prompts <- paste(c("A1_Prompt","A2_Prompt","A3_Prompt","B1_Prompt","B2_Prompt","C1_Prompt","C2_Prompt","D1_Prompt","D2_Prompt","D3_Prompt","E1_Prompt","E2_Prompt","F1_Prompt","F2_Prompt","G1_Prompt","G2_Prompt"),"\n\n", prompts)

# Combine into a single table

prompts <- tibble(P1 = unlist(prompts)) %>%
  bind_cols(P3) %>%
  mutate(P0 = P_0) %>%
  mutate(P2 = P_2) %>%
  mutate(P4 = P_4) %>%
  unite("GPT_Prompt", P0,P1,P2,P3,P4, sep = "\n")


# convert the list to a single string with "\n\n" as separator
#prompt_text <- paste(P_1,prompts,P_3, collapse = "\n")

# write to the file
sink("GPT_Prompts_Groups.txt")
cat(unlist(prompts))
sink()


# Function to call API and get response
get_api_response <- function(row) {
  # Extract necessary information from row
  query <- paste(row['GPT_Prompt'])
  
  # Call API and get response
  response <- ask_chatgpt(query)
  
  # Return response
  return(response)
}


# Initialize an empty list to store the responses
prompts_returned  <- list()

# For loop to iterate over each row of the dataframe
for(i in 1:nrow(prompts)) {
  # Get the i-th row of the dataframe
  row <- prompts[i, ]
  
  # Call the function and store the response
  prompts_returned[[i]] <- get_api_response(row)
}



#In the style of a commercial geodemographic company; label this cluster description (give 7 different versions) and provide a one sentence summary of the desciption. The cluster is in London, so augment where possible with characteristics of london.  The label should be no more than 3 words. The description of the cluster is as follows:




############################################################
# Check top and bottom 10 features for Super Groups and Groups
############################################################

Index_Scores

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
ranked_index_list_top_SG <- map(column_names, ~ rank_rows_top(Index_Scores, .))
ranked_index_list_bottom_SG <- map(column_names, ~ rank_rows_bottom(Index_Scores, .))


column_names <- c("A1", "A2", "A3", "B1", "B2", "C1", "C2", "D1", "D2", "D3", "E1", "E2", "F1", "F2", "G1", "G2")

# Create a list of data frames, each containing the variable_name and one of the columns A-G
ranked_index_list_top_G <- map(column_names, ~ rank_rows_top(Index_Scores_Groups, .))
ranked_index_list_bottom_G <- map(column_names, ~ rank_rows_bottom(Index_Scores_Groups, .))






############################################################
# Maps for Groups and Super Groups
############################################################


SGroup_palette <- c("A" = "#66c2a5", "B" = "#fc8d62", "C" = "#8da0cb", "D" = "#e78ac3", "E" = "#a6d854", "F" = "#ffd92f", "G" = "#D9dddf")

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
                    "G1" = "#7D7D7D", "G2" = "#D1D1D1")

ggplot() +
  geom_sf(data = cluster_assignments_SF, aes(fill = G),color = NA) +
  scale_fill_manual(values = Group_palette,drop = FALSE,name = "SG") +
  geom_sf(data = dplyr::filter(LAD_Dec21, LAD21CD %in% LonLAD), fill = NA, color = "black")+
  theme_void() 




#Map Groups

plot_and_save <- function(cluster) {
  plot <- ggplot() +
    geom_sf(data = cluster_assignments_SF, fill = "#1b263b", color = "#1b263b") +
    geom_sf(data = dplyr::filter(cluster_assignments_SF, G == cluster), fill = "#415a77", color = "#415a77") +
    theme_void()
  
  ggsave(filename = paste0(cluster, ".png"), plot = plot)
}

# You can now call this function for multiple subsets using purrr::map
clusters <- c("A1", "A2", "A3", "B1", "B2", "C1", "C2", "D1", "D2", "D3", "E1", "E2", "F1", "F2", "G1", "G2")
purrr::map(clusters, plot_and_save)

















  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
############################################################
# Compare LOAC Super Groups With and Without Ethnicity
############################################################
  
SG_With_Ethnic <- read_parquet("./data/LOAC_SuperGroup.parquet")
SG_No_Ethnic <- read_parquet("./data/LOAC_SuperGroup_no_ethnic.parquet")

SG_No_Ethnic %<>%
  mutate(
    SG_No_Ethnic = case_when(
      predict == 3 ~ "A",
      predict == 2 ~ "B",
      predict == 0 ~ "C",
      predict == 5 ~ "D",
      predict == 1 ~ "E",
      predict == 4 ~ "F",
      predict == 6 ~ "G"
    )
  )
  
  
SG_Ethnic_Compare <-SG_With_Ethnic %>%
                          left_join(SG_No_Ethnic) %>%
                          left_join(ts001) %>%
                          select(SG,SG_No_Ethnic,ts0010001 )
                    
SG_Ethnic_Compare <- aggregate(ts0010001 ~ SG + SG_No_Ethnic, SG_Ethnic_Compare, FUN = sum)


                        
install.packages("ggalluvial")
  library(ggalluvial)
library(RColorBrewer)

colors <- brewer.pal(7, "Set2")


ggplot(data = SG_Ethnic_Compare,
       aes(axis1 = SG, axis2 = SG_No_Ethnic,
           y = ts0010001)) +
  geom_alluvium(aes(fill = SG)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_fill_manual(values = colors) +
  theme_void() +
  guides(fill = guide_legend(title = "Super Group"))
  
  
ggsave("SG_Ethnicity_Comparison.pdf", height = 20, width = 30, units = "cm")

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  






############################################################
# Compare 2011 LOAC with 2021 LOAC
############################################################

  install.packages("ggalluvial")
  
  
  PCD_POP_2011 <- read_csv("./data/census/Postcode_Estimates_Table_1.csv")%>%
    mutate(Postcode = str_remove_all(Postcode, "\\s+"))
  
  
  
  # Get list of file names
  files <- list.files(path = "path_to_your_directory", pattern = "*.csv", full.names = TRUE)
  
  # Read and bind all CSV files
  df <- files %>% 
    map_df(~read_csv(.))
  
  
  
  # install.packages("ggalluvial")
  # library(ggalluvial)
  # 
  # ggplot(data = vaccinations,
  #        aes(axis1 = survey, axis2 = response, y = freq)) +
  #   geom_alluvium(aes(fill = response)) +
  #   geom_stratum() +
  #   geom_text(stat = "stratum",
  #             aes(label = after_stat(stratum))) +
  #   scale_x_discrete(limits = c("Survey", "Response"),
  #                    expand = c(0.15, 0.05)) +
  #   theme_void() + 
  #   theme(legend.position = "none")
  
  
  
  
OA_11_21_lookup <- read_csv("./data/lookup/OA_(2011)_to_OA_(2021)_to_Local_Authority_District_(2022)_for_England_and_Wales_Lookup_(Version_2).csv")

OA_11_21_lookup %<>%
  select(OA11CD, OA21CD,CHNGIND)


OAC_2011 <- read_csv("./data/lookup/LOAC_2011.csv")


OA_11_21_lookup %<>%
  left_join(cluster_assignments, by = c("OA21CD" = "OA")) %>% #LOAC21
  left_join(OAC_2011, by = c("OA11CD" = "oa_code"))


OA_11_21_lookup %<>%
  filter(!(is.na(predict) & is.na(supgrp_cd) & is.na(grp_cd)))


table(OA_11_21_lookup$supgrp_cd,OA_11_21_lookup$predict)





##
























































########## Ancillary Code #################


# Check the variable matches

# variable_lookup %>%
#   separate_rows(Variables, sep = "&") %>%
#   mutate(Variables = str_replace_all(Variables, " ", "")) %>%
#   left_join(census_tables,by = c("Variables" = "new_names")) %>%
#   select(No.,`Variable Name`,Variables,old_names)


# Input maps

#OA_london <- read_parquet("./data/OA_Input_london.parquet")

#OA_london_SF <- OA_2021_Boundary %>%
# left_join(OA_london, by = c("OA21CD" = "OA")) %>%
#   filter(!is.na(utla22cd))

#st_write(OA_london_SF, "./map/LOAC_Input.gpkg", driver = "GPKG")




#correlation analysis

# numeric_OA_london <- OA_london %>% 
#   purrr::keep(is.numeric)
# 
# # Calculate correlation matrix using `cor` function
# correlation_matrix <- numeric_OA_london %>% 
#   cor() %>% 
#   as_tibble(rownames = "column") %>% 
#   pivot_longer(-column, names_to = "column2", values_to = "correlation")

