library(summarytools)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)


# import internal and contextual anabling conditions for MPAs
wiompa_cond<- read_csv("csv/wiompa_internal_contextual_conditions.csv")
View(wiompa_cond)

# calculate summary statistics

dfSummary(wiompa_cond) %>% stview()


# plot of distribution of levels of protection

wiompa_stage<-ggplot(wiompa_cond, aes(x = Country, fill = protection_level)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribution of MPA levels of protection in the WIO",
    x = "Country",
    y = "Frequency",
    fill = "Level of protection"
  ) +
  scale_fill_manual(values=c(
    "#000000", 
    "#E69F00", 
    "#56B4E9" ,
    "#009E73"
  ))+
  theme_minimal()

# plot of distribution of stage of establishment

wiompa_level<-ggplot(wiompa_cond, aes(x = Country, fill = establishment_stage)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribution of MPA stages of establishment in the WIO",
    x = "Country",
    y = "Frequency",
    fill = "Stage of establishment"
  ) +
  scale_fill_manual(values=c(
    "#000000", 
    "#E69F00", 
    "#56B4E9" ,
    "#009E73"
  ))+
  theme_minimal()

# merge stage and level figures
wiompa_stage + wiompa_level

# subset MPAs with unclassified stages and levels

wiompa_stages_levels<-wiompa_cond[,c(2:5)]

wiompa_unclassified <- wiompa_stages_levels %>% 
  filter(
    establishment_stage == "Unclassified" | 
      protection_level == "Unclassified"
    )
write_csv(wiompa_unclassified,"wiompa_unclassified.csv")
