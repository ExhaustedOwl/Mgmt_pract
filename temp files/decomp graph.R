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
        rename(variable_name = short_name, 
               aggregate_change = change05_17, 
               pct_change = pct_change05_17,
               p_value = prtest05_17) %>%
        mutate(size = convert.size(size),
               industry = convert.industry(industry),
               cluster = convert.cluster(cluster),
               weight = convert.weight(weight)) %>% 
        mutate_at(vars(1:6), as.factor) %>% 
        mutate_at(vars(7:22), as.numeric) %>%
        mutate_at(vars(23:26), as.factor) 


### Decomposition
dat2 <- read_excel("Data/2c_decomp_CONF.xlsx", range = cell_cols("D:J"), sheet = 5) %>% select(variable, cluster, weight, rr_decomp_within, rr_decomp_across, rr_D_total) %>% 
        rbind(read_excel("Data/2c_decomp_CONF.xlsx", range = cell_cols("D:J"), sheet = 6) %>% select(variable, cluster, weight, rr_decomp_within, rr_decomp_across, rr_D_total)) %>% 
        mutate(cluster = convert.cluster(cluster),
               weight = convert.weight(weight)) %>% 
        mutate_at(vars(1:3), as.factor) %>% 
        mutate_at(vars(4:6), as.numeric)
dat2 <- left_join(dat2,
                  subset(dat, dat$size== "All" & dat$industry == "All", select = c("variable", "weight", "long_name", "p_value")),
                  by = c("variable" = "variable", "weight" = "weight")) %>% 
        rename(question = long_name, decomp_within = rr_decomp_within, 
               decomp_across = rr_decomp_across, decomp_overall = rr_D_total) %>% drop_na() %>% 
        mutate(variable = as.factor(variable))

dat2_decomp <- dat2  %>% #select(-variable) %>%
                        gather(value = "Decomposition", 
                               key = "Type", 
                               -question, -cluster, -weight, -p_value, -variable) #%>%
        #mutate(Type = factor(Type, levels = c("decomp_within", "decomp_across")))
        

dat2_decomp %>% filter(Type != "decomp_overall", cluster == "Strategy - focus") %>% 
        mutate(significance  = ifelse(p_value < 0.1, TRUE, FALSE)) %>%
        ggplot(aes(
                x = question,
                y = Decomposition,
                fill = Type,
                colour = Type,
                alpha = significance
        )) +
        geom_bar(stat = "identity") +
        scale_y_continuous(
                breaks = c(-0.15,-0.1,-0.05, 0, 0.05, 0.1, 0.15),
                limits = c(-0.15, 0.15),
                expand = expand_scale(mult = c(0.035, 0.035))
        ) +
        scale_fill_manual(values = c('#97D700', '#006272')) +
        scale_colour_manual(values = c('#97D700', '#006272')) +
        scale_alpha_manual(values = c(0, 1), guide = FALSE) +
        facet_grid(cluster ~ weight, scales = "free_y") +
        coord_flip() +
        labs(title = "Aggregate index change by cluster (2005 - 2017)",x = "", y = "") +
        theme_bw() +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_line(colour = "#e5faff"),
              panel.grid.minor.x = element_blank(),
              panel.background = element_blank(),
              legend.position = "bottom",
              plot.title = element_text(size=14, hjust = 5, vjust = -5, face="bold"),
              axis.text.x=element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title = element_text(size=12,face="bold"),
              strip.text = element_text(size = 12))
