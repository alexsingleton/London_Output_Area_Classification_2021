

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





