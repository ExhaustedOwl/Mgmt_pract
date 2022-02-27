### Individual practices ###
library(tidyverse)
library(plotly)
library(readxl)
library(haven)

dat <- read_stata("Data/T1_overall_wide.dta")

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
                      labels = c("Firm", "Employment")))
        
}


dat <- dat %>%
        rename(`2005` = rr_2005, 
               `2009` = rr_2009,
               `2013` = rr_2013,
               `2017` = rr_2017,
               variable_code = variable, 
               variable_name = long_name, 
               practice_shortname = short_name,  
               aggregate_change = change05_17, 
               pct_change = pct_change05_17,
               p_value = prtest05_17) %>%
        filter(variable_code != "smp_index") %>% 
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


        
               dat_final <- 
                        dat %>% 
                        filter(n == "1") %>% 
                        dplyr::select(variable_name, cluster:weight,
                                      aggregate_change, p_value, 
                                      practice_shortname
                                      ) 
 
g <-                dat_final %>% 
                        mutate(significance = ifelse(p_value < 0.1, TRUE,
                                                     FALSE)) %>% 
                        ggplot(aes(x = variable_name,
                                   y = aggregate_change,
                                   fill = significance)
                               ) +
                        geom_bar(stat = "identity",
                                 position = "identity",
                                 colour = "#006272"
                                 ) +
                        geom_hline(yintercept = 0, size = 0.45) +
                        scale_y_continuous(
                                breaks = c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15),
                                limits = c(-0.13, 0.13),
                                expand = expansion(mult = c(0, 0.035))
                                ) +
                        scale_fill_manual(values = c("white", '#006272')) +
                        facet_grid(cluster ~ weight, scales = "free_y") +
                        coord_flip() +
                        labs(title = "", x = "", y = "") +
                        theme_bw() +
                        theme(panel.grid.major.y = element_line(colour = "#e5faff"),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.background = element_blank(),
                              panel.spacing = unit(.75, units = "cm"),
                              plot.title = element_text(size=14, face="bold"),
                              legend.title = element_blank(),
                              legend.position = "none",
                              axis.text.x=element_text(size=10, hjust = -0.5),
                              axis.text.y=element_text(size=10),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text = element_text(size = 14, face="bold"),
                              plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm")
                        )               

ggplotly(g) %>%
        layout(uniformtext=list(minsize=8, mode='show'),
               title = list(text="Aggregate index change by cluster (2005 - 2017)", 
                            y = 1, x = 0.5, xanchor = 'center', yanchor =  'top'
                            ),
               yaxis = list(ticklen = 20, tickcolor = "transparent")
               ) %>%  
        config(displaylogo = FALSE,
               displayModeBar = TRUE,
               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                          'lasso2d','zoomIn2d','zoomOut2d',
                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                          'hoverCompareCartesian')
        )
