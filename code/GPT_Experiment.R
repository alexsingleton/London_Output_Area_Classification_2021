
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



