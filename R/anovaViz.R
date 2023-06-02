my.datanuts<-read.csv("NS_HABsNutData2022.csv", header=TRUE, sep= ",", na.strings = c("ND"))
alldata_nuts_grid<-read.csv("alldata_nuts_grid.csv", header=TRUE, sep= ",", na.strings = c("ND"))

source ("ANOVAPlots.R")
library(ggpubr)
library(tidyverse)
theme_set(theme_pubr())
##library(424ewdsqa)
library (AICcmodavg)
library(multcompView)
library(knitr)
library(kableExtra)
library(plyr)


######################
##Nutrient Data workup

my.datanuts_long<-my.datanuts %>%
  filter(Type!="Blank")%>%
  pivot_longer(-c(Date,Site, Round, Type, Year),names_to="Parameter")%>%
  drop_na()%>%
  filter(value!="ND")%>%
  mutate(Site=factor(Site),Date=as.Date(Date, format="%m/%d/%Y"), Round=factor(Round))%>%
  mutate(Parameter=recode(Parameter,
                          "Chl.A..ug.L."="Chlorphyll A (µg/L)", "Nitrate.nitrite..mg.L."="Nitrate + nitrite (mg/L)",
                          "Ortho.P..mg.L."="Ortho-P (mg/L)","TN..mg.L."="TN (mg/L)", "TP..mg.L."="TP (mg/L)", "TSS..mg.L."="TSS (mg/L)" )) %>%
  group_by(Date, Round, Site, Parameter, Year)
  summarize(value=mean(value, na.rm = T))
 
######################
##NutsThroughTime
my.datanuts_long%>% 
    mutate(day1=ifelse(as.numeric(Site)<8,"West","East"),
           day1 = factor(day1, levels = c("West", "East")))%>%
  ggplot(aes(x=Date, y=value, color=Site))+
  geom_point()+
  geom_smooth(se=FALSE, span=1) +
  facet_grid(rows=vars(Parameter), cols=vars(day1),scales = "free_y")+
  ggsave("C:/Users/coonee/Documents/Monitoring Projects/NearshoreHABs/2022/NutsTime.jpeg")

##NutsThroughSpace.jpeg
ggplot(my.datanuts_long, aes(x=as.numeric(Site), y=value, color=Round))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~Parameter,scales = "free_y")+
ggsave("C:/Users/coonee/Documents/Monitoring Projects/NearshoreHABs/2022/NutsSpace.jpeg")

##NutsDist.jpeg
ggplot(my.datanuts_long, aes(x=Site, y=value,))+
  geom_violin()+
  facet_wrap(~Parameter, scales="free_y")+
ggsave("C:/Users/coonee/Documents/Monitoring Projects/NearshoreHABs/2022/NutsDistbySite.jpeg")

###NutsGrid.
ggplot(my.datanuts_long, aes(x=Date, y=value))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE,)+
  facet_grid(row= vars(Parameter), cols=vars(Site), scales = "free_y",labeller = label_wrap_gen(width=10))+
  labs(title = "Nutrient Data", x='2022 Dates', y = 'Parameter Value', subtitle = 'Site')+
  theme(
    plot.title = element_text(hjust=0.5, size = 20, margin = margin(0.5,0,0.5,0, 'cm')),
    plot.subtitle = element_text(hjust=0.5, size=20),
    axis.text.x = element_text(size = 16, angle=45, hjust=1),
    axis.text.y = element_text(size = 16),
    axis.line = element_line(size=1),
    axis.title = element_text(size=20),
    strip.text.x = element_text(size=16),
    strip.text.y = element_text(size=10, margin = margin(1,1,1,1, 'cm')),
    panel.grid.major = element_line(size=1),
    panel.border = element_rect(size=1, fill=NA),
  )
ggsave("C:/Users/coonee/Documents/Monitoring Projects/NearshoreHABs/2022/NutsGrid.jpeg")

####PastData

my.data2019nuts<-read.csv("2019NutsData.csv", header=TRUE, sep= ",", na.strings = c("ND", "", "NA"))

my.data2019nuts_long<-my.data2019nuts %>%
  select(-c("NH3")) %>%
  pivot_longer(-c(Date,Year,Site, RouNA, Type),names_to="Parameter")%>%
  drop_na()%>%
  mutate(Site=factor(Site),Date=as.Date(Date, format="%m/%d/%Y"), Round=factor(RouNA) )%>%
  mutate(Parameter=recode(Parameter,
                          "ChlA"="Chlorphyll A (µg/L)", "NH3"="Ammonia (mg/L)","Nitrate.nitrite"="Nitrate + nitrite (mg/L)",
                          "Ortho.P"="Ortho-P (mg/L)","TN"="TN (mg/L)", "TP"="TP (mg/L)", "TSS"="TSS (mg/L)" ))%>%
  select(-c(RouNA)) 


my.data2021<-read.csv("2021NutsData.csv", header=TRUE, sep= ",",  na.strings = c("ND", "", "NA", "NS"))
my.data2021_long<-my.data2021 %>%
  select(-c(X, X.1,NH3..mg.L.)) %>%
  pivot_longer(-c(Date,Site, Round, Type, Year),names_to="Parameter")%>%
  drop_na()%>%
  filter(value!="ND")%>%
  mutate(Site=factor(Site),Date=as.Date(Date, format="%m/%d/%Y"), Round=factor(Round),value=as.numeric(value))%>%
  mutate(Parameter=recode(Parameter,"DOC..ppm."="DOC (mg/L)",
                          "Chl.A..ug.L."="Chlorphyll A (µg/L)", "NH3..mg.L."="Ammonia (mg/L)","Nitrate.nitrite..mg.L."="Nitrate + nitrite (mg/L)",
                          "Ortho.P..mg.L."="Ortho-P (mg/L)","TN..mg.L."="TN (mg/L)", "TP..mg.L."="TP (mg/L)", "TSS..mg.L."="TSS (mg/L)" ))

my.datanuts_long<-my.datanuts_long%>%
  mutate (Round=factor(Round), Site=factor(Site)) %>%
  mutate(Parameter=recode(Parameter,
                          "Chl.A"="Chlorphyll A (µg/L)","Nitrate.nitrite"="Nitrate + nitrite (mg/L)",
                          "Ortho.P"="Ortho-P (mg/L)","TN"="TN (mg/L)", "TP"="TP (mg/L)", "TSS"="TSS (mg/L)" ))%>%
  ungroup()

alldata<-do.call("rbind", list(my.data2019nuts_long,my.data2021_long, my.datanuts_long))

##ANOVA Plots

alldata_wide <- alldata %>%
  pivot_wider(names_from = Parameter, values_from = value, values_fn = mean)%>%
  mutate(Year=factor(Year))

alldata_wide$TP <- alldata_wide$`TP (mg/L)`
ANOVA_Plot(df = alldata_wide,
           Nutrient = "TP",
           fac="Site",
           filepath = "C:/Users/coonee/Documents/Monitoring Projects/NearshoreHABs/2022/Boxplot") 

##ANOVA Summary Table 

ANOVA_sum_tables<-alldata %>%
  group_by(Year,Parameter) %>%
  dplyr::summarise(Count=n(), Mean=mean(value), Min=min(value), Max=max(value), Stdev=signif(sd(value), 1)) %>%
  mutate(Mean = as.character(round_any(Mean, Stdev)), Min = as.character(round_any(Min, Stdev)), Max = as.character(round_any(Max, Stdev)), Stdev = as.character(Stdev)) %>%
  kable(caption = "Summary Statistics for Sampling Years.", 
        longtable=T, booktabs = TRUE) %>% 
  remove_column(1)%>%
  pack_rows(index = c("2019" = 6, "2021" = 7, "2022" = 6))%>%
  kable_classic()

print(ANOVA_sum_tables)


ANOVA_sum_site_tables<-alldata %>%
  group_by(Site,Parameter) %>%
  dplyr::summarise(Count=n(), Mean=mean(value), Min=min(value), Max=max(value), Stdev=signif(sd(value), 1)) %>%
  mutate(Mean = as.character(round_any(Mean, Stdev)), Min = as.character(round_any(Min, Stdev)), Max = as.character(round_any(Max, Stdev)), Stdev = as.character(Stdev)) %>%
  kable(caption = "Site Summary Statistics for All Data.", 
        longtable=T, booktabs = TRUE) %>% 
  remove_column(1)%>%
  pack_rows(index = c("Site 1" = 7, "Site 2" = 7, "Site 3" = 7,"Site 4" = 7,"Site 5" = 7,"Site 6" = 7,"Site 7" = 7,
                      "Site 8" = 7, "Site 8.5" = 5, "Site 9" = 7,"Site 10" = 6,"Site 11" = 7,
                      "Site 12" = 7,"Site 13" = 6,"Site 14" = 6,"Site 15" = 6))%>%
  kable_classic()

print(ANOVA_sum_site_tables)

####All data Grid analysis

ggplot(alldata_nuts_grid, aes(x=Julian, y=value, col=as.factor(Year)))+
  geom_point()+
  facet_grid(row= vars(Parameter), cols=vars(Site), scales = "free_y",labeller = label_wrap_gen(width=10))+
  labs(title = "All Year Nutrient Data", x=' Julian Date', y = 'Parameter Value', subtitle = 'Site',)+
  theme(
    plot.title = element_text(hjust=0.5, size = 20, margin = margin(0.5,0,0.5,0, 'cm')),
    plot.subtitle = element_text(hjust=0.5, size=16),
    axis.text.x = element_text(size = 16, angle=45, hjust=1),
    axis.text.y = element_text(size = 16),
    axis.line = element_line(size=1),
    axis.title = element_text(size=20),
    strip.text.x = element_text(size=16),
    strip.text.y = element_text(size=10, margin = margin(1,1,1,1, 'cm')),
    panel.grid.major = element_line(size=1),
    panel.border = element_rect(size=1, fill=NA),
  )


#####Partion of Variance
# fractions [a+b+c]:
rda.all <- rda (chla ~ dose + cover, data = fertil.env)
# fractions [a+b]:
rda.dose <- rda (fertil.spe ~ dose, data = fertil.env)
# fractions [b+c]:
rda.cover <- rda (fertil.spe ~ cover, data = fertil.env)


library(tidyr)
alldata_nuts_grid %>%
  group_by(Parameter) %>%
  nest() %>%
  mutate(lin = map(data , ~lm(value ~ Julian, data=  .)),
         tidied = map(lin,  tidy))



###################trophic state index#####################3333##
###EGAD # 3200-2018-08, WisCALM 2018 Lake Trophic State Index (TSI) Assessment Parameter Documentation

my.datanutsTSI<-my.datanuts %>%
  mutate(Site=factor(Site),Date=as.Date(Date, format="%m/%d/%Y"), Round=factor(Round))%>%
  mutate(TSIchla=((9.81*log(Chl.A)))+30.6)%>%
  mutate(TSITP=(14.42*log(TP*1000))+4.15)%>%
  mutate(TSI=(TSITP+TSIchla)/2)

my.dataTSIlong<-my.datanutsTSI%>%
  select(-c(Type,Date, DOC, TP, TN, Ortho.P, Chl.A, TSS, Nitrate.nitrite, NH3, X, X.1))%>%
  pivot_longer(-c(Site, Round),names_to="TSI")%>%
  mutate(Site=factor(Site), Round=factor(Round))


ggplot(my.dataTSIlong, aes(x=as.numeric(Site), y=value, color=Round))+
  geom_point()+
  geom_smooth(method= "lm", se = FALSE)+
  facet_wrap(~TSI,scales = "free_y")+
  labs(title = "Trophic State Index", x='Sites', y = 'Index Value')


mutate(Site=factor(Site),Date=as.Date(Date, format="%m/%d/%Y"), Round=factor(Round))%>%
  mutate(Parameter=recode(Parameter,"DOC"="DOC (mg/L)",
                          "Chl.A"="Chlorphyll A (µg/L)", "NH3"="Ammonia (mg/L)","Nitrate.nitrite"="Nitrate + nitrite (mg/L)",
                          "Ortho.P"="Ortho-P (mg/L)","TN"="TN (mg/L", "TP"="TP (mg/L)", "TSS"="TSS (mg/L)" ))


write.csv(alldata_wide,"C:/Users/coonee/Documents/Monitoring Projects/NearshoreHABs/2022/alldata_wide.csv", row.names=FALSE)

