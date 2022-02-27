### Individual practices ###
library(tidyverse)
library(plotly)
library(readxl)
library(haven)


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


# dat<- read_excel("Data/T1_overall_wide.xlsx")
dat <- read_stata("/Data/T1_overall_wide.dta")

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

dat_s <-  
        dat %>% 
        dplyr::select(variable_name, cluster:weight, 
                      `2005`:se_2017, practice_shortname
        ) %>%
        dplyr::filter(variable_name == "Assess performance based on cost measures",
                      size =="All",
                      industry == "All"
        )

dat_i <-  
        dat %>% 
        dplyr::select(variable_name, cluster:weight, 
                      `2005`:se_2017, practice_shortname
        ) %>%
        dplyr::filter(variable_name == "Assess performance based on cost measures",
                      size =="All",
                      industry == "All"
        )

dat_final <- rbind(dat_s, dat_i)



dat_plot_rr <- 
        dat_final %>% 
        select(-starts_with("se")) %>%
        pivot_longer(
                c(`2005`, `2009`, `2013`, `2017`), 
                names_to = "year",
                values_to = "value"
                ) %>% 
        mutate(year = as.numeric(year))

dat_plot_se <- 
        dat_final %>% 
        select(-`2005`, -`2009`, -`2013`, -`2017`) %>%
        pivot_longer(
                c(starts_with("se")),
                names_to = "year",
                values_to = "se"
                ) %>% 
        mutate(year = as.numeric(gsub("se_", "", year)))

dat_plot <- 
        dat_plot_rr %>% 
        left_join(dat_plot_se) %>% 
        distinct() %>% 
        relocate(practice_shortname, .after = last_col())
 

g <- dat_plot %>% 
        filter(industry == "All") %>% 
        ggplot(aes(x = year,
                   y = value,
                   col = size
                   )
               ) +
        geom_path()  + 
        geom_point()  +
        geom_errorbar(aes(ymin = value - se,
                          ymax = value + se),
                      width = 0.25
                      ) +
        facet_wrap(~weight) + 
        scale_y_continuous(breaks = seq(0, 1, 0.1),
                           limits = c(0, 1.0),
                           expand = expansion(mult = c(0, 0.035))
                           ) +
        scale_x_continuous(breaks = c(2005, 2009, 2013, 2017),
                           labels = c("2005", "2009","2013", "2017")
                           ) +
        scale_colour_manual(values = c('#006272', '#97D700', '#00B5E2', 
                                       '#753BBD', '#DF1995')
                            ) +
        labs(y = "", x = "") +
        theme_bw() +
        theme(panel.grid.major.y = element_line(colour = "#dff7ff"),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.background = element_blank(),
              panel.spacing = unit(.75, units = "cm"),
              legend.direction = "horizontal",
              plot.title = element_text(size=14, hjust = 5, vjust = -5, face="bold"),
              axis.text.x=element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title = element_text(size=12,face="bold"),
              strip.background = element_rect(fill = "#dfe3e6"),
              strip.text = element_text(size = 14, face="bold"),
              legend.position = "bottom",
              plot.margin = unit(c(0.5,0.5,0.25,0.25), "cm")
              )

ggplotly(g) %>% 
        layout(yaxis = list(autorange = TRUE)) %>% 
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                          'lasso2d','zoomIn2d','zoomOut2d',
                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                          'hoverCompareCartesian', 'autoScale2d') #'resetScale2d'
               )
        

