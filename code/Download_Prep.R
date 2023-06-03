library(tidyverse)
library(sf)
library(magrittr)
library(janitor)
library(scales)
library(arrow)

# Output Areas (Dec 2021) Boundaries Generalised Clipped EW (BGC)
OA_2021_Boundary <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Output_Areas_Dec_2021_Boundaries_Generalised_Clipped_EW_BGC_2022/FeatureServer/0/query?where=1%3D1&outFields=OA21CD&outSR=4326&f=json")

# Output Area to Region (December 2021) Lookup in England and Wales
OA_Region <- read_csv("https://www.arcgis.com/sharing/rest/content/items/efda0d0e14da4badbd8bdf8ae31d2f00/data")

# Output Area to Upper-Tier Local Authorities (December 2021) Lookup in England and Wales
OA_UTLAD <- read_csv("https://www.arcgis.com/sharing/rest/content/items/4393e36fa6184b0fb1b2562d98db1da6/data")

# Local Authority Districts (December 2021) GB BGC
LAD_Dec21 <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2021_GB_BGC_2022/FeatureServer/0/query?where=1%3D1&outFields=LAD21CD,LAD21NM&outSR=4326&f=json")



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

# v01 - Usual residents per square kilometre
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

#v17 - Ethnic group: Black
v17 <- ts021 %>% select(OA, ts0210008_PCT,ts0210009_PCT,ts0210010_PCT,ts0210011_PCT) %>% 
  mutate(v17 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v17)

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
# Combine All the Variables for London
############################################################


# List of data
list_of_dfs <- list(OA_london, Non_PCT, OA21_SDR_London,tmp_table,v03,v04,v05,v06,v17,v26,v28,v29,v36,v37,v60)

# Combine
OA_london <- list_of_dfs %>%
  reduce(left_join, by = "OA")


############################################################
# Calculate the Inverse hyperbolic sine and range standardise
############################################################

OA_london %<>%
  mutate_at(vars(-(1:3)), ~asinh(.)) %>%
  mutate_at(vars(-(1:3)), ~rescale(.))




write_parquet(OA_london, "./data/OA_Input_london.parquet")








