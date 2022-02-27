library(shiny)
library(plotly)
library(readxl)
library(reshape2)
library(DT)
library(shinythemes)
library(tidyverse)
library(haven)

convert.size <-
        function(x) {
                return(factor(x,
                              levels = c("0", "1", "2", "3", "4"),
                              labels = c("All", "6-19 RME", "20-49 RME", "50-99 RME",
                                         "100+ RME")
                )
                )
        }

convert.industry <- 
        function(x) {
                return(factor(x, 
                              levels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
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
                                         "Arts and recreation services", "Other services")
                )
                )
        }

convert.cluster <- 
        function(x) {
                return(factor(x, 
                              levels=c("n/a", "strat_what", "strat_how", "ss_chains", 
                                       "info_assess", "info_scan", "employees", "quality"),
                              labels = c("Other", "Strategy - focus", "Strategy - practices", 
                                         "Supply chain linkages", "Information - Assessment",
                                         "Information - scanning", "Employee practices",
                                         "Quality and process")
                )
                )
        }

convert.weight <- 
        function(x) {
                return(factor(x, 
                              levels=c("pweight", "eweight"),
                              labels = c("Firm", "Employment")
                )
                )
        }

dat <- read_stata("Data/T1_overall_wide.dta")
dat <- dat %>%
        rename(`2005` = rr_2005, 
               `2009` = rr_2009,
               `2013` = rr_2013,
               `2017` = rr_2017,
               variable_code = variable, 
               practice_name = long_name, 
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

dat3 <- read_excel("Data/2a_decomp_CONF.xlsx", 
                   range = cell_cols("D:N"),
                   sheet = "Transpose"
                   ) %>%
        select(-`...8`) %>%
        `colnames<-` (c("Continuers - within", "Continuers - across", "Entrants",
                        "Exiters", "Joiners", "Leavers", "Total", 
                        "variable_code", "cluster", "weight")
                      ) %>% 
        mutate(cluster = convert.cluster(cluster),
               weight = convert.weight(weight)
               ) %>%
        filter(cluster != "Other") %>% 
        mutate_at(vars(1:7), as.numeric) %>%
        mutate_at(vars(8:9), as.factor) %>% 
        left_join(
                subset(dat, dat$size== "All" & dat$industry == "All", 
                       select = c("variable_code", "practice_name", 
                                  "weight", "size", "industry", 
                                  "p_value", "practice_shortname")
                ), by = c("variable_code" = "variable_code", 
                          "weight" = "weight")
        ) %>%  
        drop_na()
 
dat3_decomp <-
        reactive({
                dat3_final <-
                        dat3  %>%
                        dplyr::select(
                                practice_name, cluster, size, 
                                industry, weight, `Continuers - within`, 
                                `Continuers - across`, Entrants,
                                Exiters, Joiners, Leavers, Total,
                                p_value, practice_shortname) %>%
                        filter(practice_name %in% input$practice_name_decomp)
                
                return(dat3_final)
        })       
        
        

        
        dat3_plot_decomp <- 
                reactive({
                        dat3_plot <- dat3_decomp() 
                        
                        dat3_plot %<>% 
                                select(c(1,5:11,13)) %>%
                                pivot_longer(
                                        c(`Continuers - within`, 
                                          `Continuers - across`, Entrants,
                                          Exiters, Joiners, Leavers),
                                        names_to = "Group",
                                        values_to = "Decomposition"
                                )  %>% 
                                mutate(significance = ifelse(p_value < 0.1, TRUE,
                                                             FALSE),
                                       limit_min = ifelse(practice_name == "Identify risks or opportunities from skill availability", -0.07,
                                                                 -0.03),
                                       limit_max = ifelse(practice_name == "Measures to reduce environmental impacty", 0.05,
                                                          0.04),
                                       Group = fct_rev(Group))
                        
                        return(dat3_plot)
                        
                })
        
        output$ui_plot_decomp2 <- renderUI({
                
                n_facets <- length(input$practice_name) * 400
                plotlyOutput("plot_decomp2", height = glue("{n_facets}px"))
                
        })
        
        output$plot_decomp2 <- renderPlotly({
                shiny::validate(
                        need(input$practice_name_decomp, "Input a value!")
                )
                
                g <- 
                        dat3_plot_decomp() %>% 
                        ggplot(aes(
                                x = Group,
                                y = Decomposition,
                                alpha = significance)
                        ) +
                        geom_bar(stat = "identity",
                                 position = "identity",
                                 fill = '#006272',
                                 colour = '#006272') +
                        geom_hline(yintercept = 0, size = 0.45) +
                        scale_y_continuous(
                                breaks = c(-0.075, -0.05, -0.025, 0, 0.025,
                                           0.05, 0.075),
                                limits =  c(min(dat3_plot_decomp()$limit_min),
                                            max(dat3_plot_decomp()$limit_max)),
                                expand = expansion(mult = c(0, 0.035))
                                ) +
                        scale_alpha_manual(values = c(0, 1), guide = FALSE) +
                        facet_grid(practice_name ~ weight, scales = "free_y", space = "free") +
                        coord_flip() +
                        labs(title = "",x = "", y = "") +
                        theme_bw() +
                        theme(panel.grid.major.y = element_line(colour = "#e5faff"),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.background = element_blank(),
                              panel.spacing.x = unit(.25, units = "cm"),
                              panel.spacing.y = unit(.15, units = "cm"),
                              plot.title = element_text(size=14, face="bold"),
                              legend.title = element_blank(),
                              legend.position = "none",
                              axis.text.x=element_text(size=10, hjust = -0.5),
                              axis.text.y=element_text(size=10),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text.x = element_text(size = 14, face="bold"),
                              strip.text.y = element_text(size = 10, face="bold"),
                              plot.margin = unit(c(0.5,0.5,0.25,0.25), "cm")
                        )
                
                
                ggplotly(g, tooltip = c("Decomposition", "significance")) %>%
                        layout(#uniformtext=list(minsize=8, mode='show'),
                                yaxis = list(ticklen = 20, tickcolor = "transparent")
                        ) %>%  
                        config(displaylogo = FALSE,
                               displayModeBar = TRUE,
                               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                          'lasso2d','zoomIn2d','zoomOut2d',
                                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                                          'hoverCompareCartesian')
                        )
                
        })