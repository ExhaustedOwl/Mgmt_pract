#Data --------------------------------------------------------------------------
library(shiny)
library(plotly)
library(readxl)
library(reshape2)
library(DT)
library(shinythemes)
library(tidyverse)

### Aggregate changes
dat<- read_excel("Data/T1_overall_wide.xlsx")

convert.size <-
        function(x) {
                return(factor(
                        x,
                        levels = c("0", "1", "2", "3", "4"),
                        labels = c("All", "6-19 RME", "20-49 RME", "50-99 RME",
                                   "100+ RME")
                ))
        }

convert.industry <- 
        function(x) {
                return(factor(x, levels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", 
                                          "10", "11", "12", "13", "14", "15", "16", "17", "18"),
                              labels = c("All", "Agriculture, forestry and fishing", "Mining",
                                         "Manufacturing", 
                                         "Electricity, gas, water and waste services",
                                         "Construction", "Wholesale trade", "Retail trade",
                                         "Accomodation", "Transport, postal and warehousing",
                                         "Informationn, media and telecommunications", 
                                         "Financial and information services",
                                         "Rental, hiring and real estate services",
                                         "Professional, scientific and technical services",
                                         "Administration and support services", 
                                         "Education and training", "Health care and social assistance",
                                         "Arts and recreation services", "Other services" )))
        }

convert.cluster <- function(x) {
        return(factor(x, levels=c("n/a", "strat_what", "strat_how", "ss_chains",
                                  "info_assess", "info_scan", "employees", "quality"),
                      labels = c("Other", "Strategy - focus", "Strategy - practices", 
                                 "Supply chain linkages", "Information - Assessment",
                                 "Information - scanning", "Employee practices",
                                 "Quality and process")))
        
}

convert.weight <- function(x) {
        return(factor(x, levels=c("pweight", "eweight"),
                      labels = c("Weighted by firms", "Weighted by employment")))
        
}

dat <- dat %>%
        rename(`2005` = rr_2005, 
               `2009` = rr_2009,
               `2013` = rr_2013,
               `2017` = rr_2017,
               variable_code = variable, 
               variable_name = short_name, 
               variable_fullname = long_name,  
               aggregate_change = change05_17, 
               pct_change = pct_change05_17,
               p_value = prtest05_17) %>%
        mutate(size = convert.size(size),
               industry = convert.industry(industry),
               cluster = convert.cluster(cluster),
               weight = convert.weight(weight),
               `2005` = ifelse(`2005`>1, 1, `2005`),
               `2009` = ifelse(`2009`>1, 1, `2009`),
               `2013` = ifelse(`2013`>1, 1, `2013`),
               `2017` = ifelse(`2017`>1, 1, `2017`)) %>% 
        mutate_at(vars(1:6), as.factor) %>% 
        mutate_at(vars(7:22), as.numeric) %>%
        mutate_at(vars(23:26), as.factor)  

dat[dat$aggregate_change < -0.15 & dat$n ==1,]$cluster

below <- vector("character")
above <- vector("character")
get_outliers <- function(df){
        for(i in seq_along(df)){ 
                if(df$aggregate_change[i] < -0.15){
                        below <- c(below, as.character(df$cluster[[i]]))
                }
                else if(df$aggregate_change[i] > 0.15){
                        above <- c(above, as.character(df$cluster[[i]]))      
                }
        }
        below_value <- df[df$cluster %in% unique(below)]
}

### Decomposition
dat2 <- read_excel("Data/2c_decomp_CONF.xlsx", range = cell_cols("D:J"), sheet = 5) %>% select(variable, cluster, weight, rr_decomp_within, rr_decomp_across, rr_D_total) %>%
        rbind(read_excel("Data/2c_decomp_CONF.xlsx", range = cell_cols("D:J"), sheet = 6) %>% select(variable, cluster, weight, rr_decomp_within, rr_decomp_across, rr_D_total)) %>%
        mutate(cluster = convert.cluster(cluster),
               weight = convert.weight(weight)) %>%
        rename(variable_code = variable) %>%
        mutate_at(vars(1:3), as.factor) %>%
        mutate_at(vars(4:6), as.numeric)

dat2 <- left_join(dat2,
                  subset(dat, dat$size== "All" & dat$industry == "All", select = c("variable_code", "variable_name", 
                                                                                   "weight", "size", "industry",
                                                                                   "p_value", "variable_fullname")),
                  by = c("variable_code" = "variable_code", "weight" = "weight")) %>%
        rename(decomp_within = rr_decomp_within, decomp_across = rr_decomp_across, 
               decomp_overall = rr_D_total) %>% drop_na() %>%
        mutate(variable_code = as.factor(variable_code))


### Individual Decomposition
dat3 <- read_excel("Data/2a_decomp.xlsx", range = cell_cols("A:L")) %>% 
        select(variable, short_name, long_name, d, cluster, weight, 
               value, prtest05_17) %>% 
        mutate(cluster = convert.cluster(cluster),
               weight = convert.weight(weight)) %>% 
        mutate_at(vars(1:6), as.factor) %>% 
        mutate_at(vars(7:8), as.numeric) %>% 
        rename(variable_code = variable, variable_name = short_name, 
               variable_fullname = long_name, group = d, decomp = value, 
               p_value = prtest05_17) %>% 
        mutate(group = fct_rev(group), 
               size = "All",
               industry = "All",
               limit_min = ifelse(variable_name %in% c("Goals - customer needs", 
                                                       "Risks - Competitors"), -0.067,
                                  ifelse(variable_name == "Risks - Skills", -0.145, 
                                         -0.045)),
               limit_max = ifelse(variable_name == "Training - Any", 0.0845, 
                                 0.075)) %>% 
                       drop_na()

min(dat3_decomp[dat3_decomp$variable_name == "Goals - customer needs",]$decomp)
min(dat3_decomp[dat3_decomp$variable_name == "Risks - Competitors",]$decomp)
min(dat3_decomp[dat3_decomp$variable_name == "Risks - Skills",]$decomp)
max(dat3_decomp[dat3_decomp$variable_name == "Training - Any",]$decomp)
##################################################################
dat3_decomp <- dat3_final <- dat3  %>%
                        dplyr::select(
                                variable_name, group, cluster, 
                                size, industry, weight, decomp,
                                p_value, variable_code,
                                variable_fullname) %>%
                        filter(variable_name %in% input$variable_name_decomp)
                


                dat3_decomp() %>%
                mutate(significance  = ifelse(p_value < 0.1, TRUE, FALSE)) %>%
                ggplot(aes(
                        x = group,
                        y = decomp,
                        fill = significance 
                )) +
                geom_bar(stat = "identity",
                         position = "identity",
                         colour = "#006272") +
                scale_y_continuous(
                        breaks = c(-0.04, -0.03, -0.02, -0.01, 0, 0.01, 
                                   0.02, 0.03, 0.04, 0.05, 0.06, 0.07),
                        limits =  c(-0.045,0.075),
                        expand = expand_scale(mult = c(0.035, 0.035))
                ) +
                scale_fill_manual(values = c(NA, '#006272')) + 
                facet_grid(variable_name ~ weight, scales = "free_y") +
                coord_flip() +
                labs(title = "Individual decomposition",x = "", y = "") +
                theme_bw() +
                theme(panel.grid.major.y = element_blank(),
                      panel.grid.minor.y = element_blank(),
                      panel.grid.major.x = element_line(colour = "#e5faff"),
                      panel.grid.minor.x = element_blank(),
                      panel.background = element_blank(),
                      legend.position = "none",
                      plot.title = element_text(size=14, hjust = 0.5, vjust = 2, face="bold"),
                      axis.text.x=element_text(size=10),
                      axis.text.y=element_text(size=10),
                      axis.title = element_text(size=12,face="bold"),
                      strip.text = element_text(size = 12),
                      plot.margin = unit(c(0.5,0.5,0.25,0.25), "cm"))
        

        
