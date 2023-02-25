# Title: Classify the affiliation of Findata applicants
# Date: 08/2022
# Author: Oscar Brück


# Load libraries
library(tidyverse)
library(readxl)
library(reshape2)



# Read data
setwd("/Users/oscarbruck/OneDrive - University of Helsinki/Tutkimus/Projekteja/Findata/")
## Data has been manually calculated from https://findata.fi/luvat/myonnetyt-luvat/
df <- readxl::read_xlsx("./data/findata_tietoluvat.xlsx")


# Modify data
## Wide to long
df1 <- melt(df, id.vars = c("Vuosi", "Kuukausia", "Tietoluvat"))
df1 <- df1 %>% 
  dplyr::mutate(
    ## Proportion per month
                value_mo = ifelse(value == 0, 0, value/Kuukausia),
                ## Proportion per Tietoluvat
                value = ifelse(value == 0, 0, value*100/Tietoluvat),
                ## Source
                variable = factor(variable, levels=c("Julkinen/Laitos", "Kaupallinen", "Yksityishenkilö"))) %>%
  dplyr::rename(Hakija = variable)


#########################################################################################################################################################
######################################### HAKIJA/TIETOLUVAT #############################################################################################
#########################################################################################################################################################


# Plot
png("./results/findata_tietoluvat1.png", width = 6, height = 5, res = 300, units = "in")
ggplot() +
  geom_bar(data = df1, aes(x = Vuosi, y = value, fill = Hakija), stat = "identity", color="black", position=position_dodge()) +
  # geom_bar(data = df1, aes(x = Vuosi, y = value, fill = Hakija), stat = "identity", color="black", position="fill") +
  labs(y="Osuus tietoluvista (%)") +
  scale_y_continuous(limits = c(0,80), expand = c(0, 0)) +
  scale_fill_brewer(palette="Set1") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, colour = "black"),
        axis.text.y = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "bottom")
dev.off()



#########################################################################################################################################################
############################################ HAKIJA %/VUOSI LINEAARINEN REGRESSIO  ######################################################################
#########################################################################################################################################################


# Fit a linear regression
df1_oy <- df1 %>%
  dplyr::filter(Hakija == "Kaupallinen")
df1_oy$pred1 <- predict(lm(value ~ Vuosi, data=df1_oy))
df1_js <- df1 %>%
  dplyr::filter(Hakija == "Julkinen/Laitos")
df1_js$pred1 <- predict(lm(value ~ Vuosi, data=df1_js))


# Plot
## Kaupallinen
p_oy <- ggplot(df1_oy, aes(x = Vuosi, y=value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(2020,2030), breaks = c(2020:2030)) +
  scale_y_continuous(limits = c(0,100))
print(p_oy)
## Julkinen/Laitos
p_js <- ggplot(df1_js, aes(x = Vuosi, y=value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(2020,2030), breaks = c(2020:2030)) +
  scale_y_continuous(limits = c(0,100))
print(p_js)



# Extrapolate
## Kaupallinen
pred_oy <- data.frame(Vuosi=2020:2030, Hakija="Kaupallinen")
pred_oy$value <- predict(lm(value ~ Vuosi, data=df1_oy), newdata=pred_oy)
p_oy + geom_line(color="red", data=pred_oy)

## Julkinen/Laitos
pred_js <- data.frame(Vuosi=2020:2030, Hakija="Julkinen/Laitos")
pred_js$value <- predict(lm(value ~ Vuosi, data=df1_js), newdata=pred_js)




# Combine
pred <- rbind(pred_js, pred_oy)
# Replace predicted values (years 2020-2022) by the true values
pred1 <- pred %>%
  dplyr::filter(Vuosi > 2022) %>%
  dplyr::mutate(Data="Prediktoitu") %>%
  dplyr::full_join(df1 %>% dplyr::select(Vuosi, Hakija, value) %>% dplyr::filter(Hakija %in% c("Julkinen/Laitos", "Kaupallinen")) %>% dplyr::mutate(Data="Findata"))


# Plot
png("./results/findata_tietoluvat2.png", width = 6, height = 5, res = 300, units = "in")
ggplot(pred1, aes(x = Vuosi, y=value)) +
  geom_smooth(method = "lm", size = 1.5, aes(color=Hakija)) +
  geom_point(size = 5, aes(shape = Data)) +
  labs(y="Osuus tietoluvista (%)") +
  scale_x_continuous(limits = c(2020, 2030), breaks = c(2020:2030)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_brewer(palette="Set1") +
  scale_shape_manual(values = c(15, 19)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, colour = "black"),
        axis.text.y = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.box = "vertical",
        legend.position = "bottom",
        legend.margin = margin(),
        legend.spacing.y = unit(0.1, "cm")) +
  guides(color=guide_legend(override.aes=list(fill=NA)))
dev.off()



#########################################################################################################################################################
############################################ HAKIJA %/VUOSI BARPLOT #####################################################################################
#########################################################################################################################################################



# Plot
png("./results/findata_tietoluvat3.png", width = 6, height = 5, res = 300, units = "in")
ggplot() +
  geom_bar(data = df1, aes(x = Vuosi, y = value_mo, fill = Hakija), stat = "identity", color="black", position=position_dodge()) +
  # geom_bar(data = df1, aes(x = Vuosi, y = value_mo, fill = Hakija), stat="identity", color = "black") +
  scale_fill_brewer(palette="Set1") +
  labs(y="Tietolupien määrä/kk") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, colour = "black"),
        axis.text.y = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "bottom")
dev.off()
