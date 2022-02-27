#Data --------------------------------------------------------------------------
library(shiny)
library(plotly)
library(readxl)
library(reshape2)
library(DT)
library(shinythemes)
library(tidyverse)

### Aggregate changes
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

### Individual Decomposition
dat3 <- read_excel("Data/2a_decomp.xlsx", range = cell_cols("A:L")) %>% 
        select(variable, short_name, long_name, d, cluster, weight, 
               value, prtest05_17) %>% 
        mutate(cluster = convert.cluster(cluster),
               weight = convert.weight(weight)) %>% 
        mutate_at(vars(1:6), as.factor) %>% 
        mutate_at(vars(7:8), as.numeric) %>% 
        rename(variable_code = variable, variable_name = short_name, 
               question = long_name, group = d, decomp = value, 
               p_value = prtest05_17) %>% 
        mutate(group = fct_rev(group)) %>% drop_na()

lmt <- ifelse(dat3$question == "Goals - incorporate customer requirements" |
                      dat3$question == "Identify risks or opportunities from competitors" |
                      dat3$question == "Identify risks or opportunities from skill availability" | 
                      dat3$question == "Employees participate in training - any", c(min(dat3$decomp), max(dat3$decomp)), c(-0.045,0.075))
dat3 %>% filter(question == "Formal performance reviews") %>% 
        mutate(significance  = ifelse(p_value < 0.1, TRUE, FALSE))%>%
        ggplot(aes(
                x = factor(group, levels = rev(levels(group))),
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
                expand = expand_scale(mult = c(0, 0))
        ) +
        scale_fill_manual(values = c(NA,'#006272')) + 
        facet_grid(question ~ weight) +
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
              strip.text = element_text(size = 12))


