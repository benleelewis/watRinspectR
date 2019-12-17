library(purrr)
library(dplyr)
library(magrittr)
library(lubridate)
library(r2d3)
library(ggplot2)
library(plotly)
library(ggthemes)
library(forcats)
setwd("C:/Users/ggayl/Documents/benR/water/")

files <- map(list.files("Data", recursive = TRUE, pattern = "*.csv",full.names=TRUE), read.csv)


is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


str(files[[1]])

files[[8]] %>%
  group_by(ANALYTE_NAME) %>%
  summarize(nObs = n()) %>%
  arrange(desc(nObs))

files[[8]] %>%
  group_by(year(VIOL_END_DATE)) %>%
  summarize(count = n())


files[[9]] %>%
  group_by(year(VIOL_END_DATE)) %>%
  summarize(count = n())

files[[8]] %>%
  filter(MCL == "20 PCI/L") %>%
  group_by(ANALYTE_NAME) %>%
  summarize(avg_violation = mean(as.integer(RESULT) - as.integer(MCL), na.rm=TRUE))


mean(as.integer(files[[8]]$RESULT)- as.integer(files[[8]]$MCL), na.rm = TRUE)


df <- files[[8]] %>%
  mutate(ENF_ACTION_ISSUE_DATE = ymd(ENF_ACTION_ISSUE_DATE)) %>%
  group_by(ENF_ACTION_ISSUE_DATE) %>%
  summarize(total = n()) 

r2d3(
  data = read.csv("dji.csv"),
  d3_version = 4,
  container = "div",
  options = list(start = 2009, end = 2019),
  script = "calendar.js"
)


#arsenic <- files[[8]] %>%
  
still_active <- setdiff(files[[9]],files[[8]])

write.csv(df, "obj.csv")   

threshold <- files[[8]] %>%
  mutate(RESULT = as.numeric(gsub("[a-zA-Z/]*","", RESULT)),
         MCL = as.numeric(gsub("[a-zA-Z/]*","", MCL))) %>%
  mutate(RESULT = ifelse(RESULT > 1, RESULT/1000, RESULT),
         MCL = ifelse(MCL > 1,MCL/1000,MCL)) %>%
  mutate(violation_amt = RESULT - MCL) 


#Violation amt by top 3 analytes
is_outlier(files[[8]]$violation_amt)


label_threshold <- threshold%>%
  filter(ANALYTE_NAME %in% c("ARSENIC", "NITRATE", "TTHM")) %>%
  na.omit() %>%
  group_by(ANALYTE_NAME) %>%
  mutate(outlier = ifelse(is_outlier(violation_amt), as.character(COUNTY), NA)) 

label_threshold$outlier[which(is.na(label_threshold$is_outlier))] <- as.numeric(NA)

  ggplot(data = label_threshold, aes(factor(year(
    ymd(VIOL_BEGIN_DATE)
  )),
  violation_amt)) +
  geom_boxplot() +
  facet_grid(ANALYTE_NAME ~ ., scales = "free_y") +
  xlab("Violation Year") +
  ylab("Amount over MCL (mg/L)") +
    ggtitle("Violations by Analyte Type") +
  #geom_text(aes(label = outlier), na.rm = TRUE, hjust =0.3) +
    theme_economist() + scale_colour_economist()
  




ggplot(data = threshold %>%
         filter(ANALYTE_NAME %in% c("ARSENIC","NITRATE", "TTHM")),
       aes(factor(year(ymd(VIOL_BEGIN_DATE))), 
                                                violation_amt)) +
         geom_boxplot() + 
         facet_grid(ANALYTE_NAME~., scales="free_y") + 
        xlab("Violation Year") +
        ylab("Amount over MCL (mg/L)")

#Violations by county by year (boxplot)
ggplot(data = threshold %>%
         filter(ANALYTE_NAME %in% c("ARSENIC"),
                COUNTY %in% c("KERN","MADERA","MONTEREY")),
       aes(factor(year(ymd(VIOL_BEGIN_DATE))), 
           violation_amt)) +
  geom_boxplot()  +
  facet_grid(COUNTY~.,scales = "free_y") +
  ylab("Amount over MCL(mg/L") +
  xlab("Violation Year") + 
  ggtitle("Arsenic Violation Amounts by County and Year") +
  theme_economist() + scale_colour_economist()



#Violations by county by year (tells the story better for distribution of reports)
ggplot(data = threshold %>%
         filter(ANALYTE_NAME %in% c("ARSENIC"),
                COUNTY %in% c("KERN","MADERA","MONTEREY")),
       aes(factor(year(ymd(VIOL_BEGIN_DATE))), 
           violation_amt)) +
  geom_violin()  +
  facet_grid(COUNTY~.,scales = "free_y") +
  ylab("Amount over MCL(mg/L") +
  xlab("Violation Year") + 
  ggtitle("Arsenic Violation Amounts by County and Year") +
  theme_economist() + scale_colour_economist()


# 
# mtcars %>%
#   group_by(cyl) %>%
#   mutate(outlier = ifelse(is_outlier(drat), drat, as.numeric(NA))) %>%
#   ggplot(., aes(x = factor(cyl), y = drat)) +
#   geom_boxplot() +
#   geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)


#Violations by type


files[[8]] %>%
  group_by(ANALYTE_NAME) %>%
  summarize(nObs = n()) %>%
  arrange(desc(nObs))
  
# 
# ggplot(data = ., aes(ANALYTE_NAME, nObs)) +
#   geom_bar(stat = "identity") +
#   coord_flip()

ggplot(data = violations %>%
         mutate(radioactive = as.factor(ifelse(grepl("mg", RESULT) | grepl("g\\L",MCL),
                                     0,
                                     1))), aes(fct_rev(fct_infreq(ANALYTE_NAME)), 
       fill = radioactive)) + 
  geom_bar() +
  coord_flip() +
  theme_economist() + 
  xlab("Analyte Type") +
  ylab("Frequency") 
  
  grepl("[g/L]", violations$RESULT)
  
  
  ggplot(data = violations, aes(fct_rev(fct_infreq(ANALYTE_NAME)))) + 
    geom_bar() +
    coord_flip() +
 #   theme_economist() + scale_colour_economist()+
  xlab("Analyte Type") +
    ylab("Frequency") 
  

table(threshold$COUNTY[which(threshold$ANALYTE_NAME=="ARSENIC")])

violations <- files[[8]]
violations %>%
  filter(WATER_SYSTEM_NAME == "KETTLEMAN CITY ELEMENTARY")
return_to_compliance <- files[[9]]
table(return_to_compliance$ENF_ACTION_TYPE_ISSUED)
