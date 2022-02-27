library(shiny)
library(plotly)
library(readxl)
library(reshape2)
library(DT)
library(shinythemes)
library(tidyverse)
library(haven)
library(xlsx)
library(magrittr)

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


excel_sheets <- excel_sheets("Data/2b_within_decomp_CONF.xlsx")
dat4 <- data.frame()
t <- vector('list', 152)
names(t) <- excel_sheets

for(i in seq_along(excel_sheets)){
        
        t[[i]] <- read_excel("Data/2b_within_decomp_CONF.xlsx", 
                        range = "A2:W20", 
                        col_names = T,
                        sheet =  excel_sheets[i]) %>% 
                drop_na() #%>% 
                #mutate(sheet_name = paste(excel_sheets[i])) 
        
        dat4 <- rbind(dat4, t[[i]])
           
}

dat4 <- data.frame()
for(i in seq_along(t)){
        
        temp <- t[[i]] %>% 
                filter(VARIABLES != c("y1", "y2")) %>% 
                pivot_longer(-VARIABLES) %>% 
                pivot_wider(names_from=VARIABLES, values_from=value) %>% 
                select(-1)
        
        dat4 <- rbind(dat4, temp)
        rm("temp")
}

openxlsx::write.xlsx(dat4, "Data/2b_within_decomp_transpose.xlsx")

dat4 <- read_excel("Data/2b_within_decomp_transpose.xlsx") %>% 
                `colnames<-` (c("Continuers - within", "Continuers - across", "Continuers - enter",
                                "Continuers - exit", "Entrants", "Exiters", "Joiners", 
                                "Leavers", "Total", "variable_code", "cluster", "weight",
                                "by_var", "by_group")) %>% 
                filter(cluster != "n/a") %>% 
                pivot_wider(names_from = by_var, values_from = by_group) %>% 
                mutate(
                        size = ifelse(is.na(size), 0, size),
                        industry = ifelse(is.na(industry), 0, industry)
                       ) %>% 
                filter(!industry %in% c(2,4,18)) %>% 
                mutate(size = convert.size(size),
                       industry = convert.industry(industry),
                       cluster = convert.cluster(cluster),
                       weight = convert.weight(weight)
                       ) %>% 
                left_join(
                        subset(dat, 
                               select = c("variable_code", "practice_name", 
                                          "weight", "size", "industry", 
                                          "p_value", "practice_shortname")
                        ), by = c("variable_code" = "variable_code", 
                                  "weight" = "weight",
                                  "size" = "size",
                                  "industry" = "industry")
                        ) %>%  
                drop_na()


dat4_decomp <-
        reactive({
                dat4_s <-
                        dat4  %>%
                        dplyr::select(
                                practice_name, cluster, size, 
                                industry, weight, `Continuers - within`, 
                                `Continuers - across`, `Continuers - enter`, 
                                `Continuers - exit`, Entrants, Exiters, 
                                Joiners, Leavers, Total, p_value, practice_shortname
                                ) %>%
                        dplyr::filter(
                                practice_name %in% input$practice_name_decomp2,
                                size %in% input$size_decomp2,
                                industry == "All")
                
                dat4_i <-
                        dat4  %>%
                        dplyr::select(
                                practice_name, cluster, size, 
                                industry, weight, `Continuers - within`, 
                                `Continuers - across`, `Continuers - enter`, 
                                `Continuers - exit`, Entrants, Exiters, 
                                Joiners, Leavers, Total, p_value, practice_shortname
                        ) %>%
                        dplyr::filter(
                                practice_name %in% input$practice_name_decomp2,
                                industry %in% input$industry_decomp2,
                                size == "All")
                
                dat4_final <- rbind(dat4_s, dat4_i) %>% 
                        distinct()
                
                return(dat4_final)
        })




dat4_plot_decomp <- 
        reactive({
                dat4_plot <- dat4_decomp() 
                
                dat4_plot %<>%
                        select(-Total) %>%
                        pivot_longer(
                                c(`Continuers - within`, `Continuers - across`, 
                                  `Continuers - enter`, `Continuers - exit`, 
                                  Entrants, Exiters, Joiners, Leavers),
                                names_to = "Group",
                                values_to = "Decomposition"
                                )  %>% 
                        mutate(significance = ifelse(p_value < 0.1, TRUE,
                                                     FALSE),
                               Decomposition = as.numeric(ifelse(Decomposition == "c", NA, Decomposition)),
                               Group = fct_rev(Group))
                
                return(dat4_plot)
                
        })


output$plot_size_decomp <- renderPlotly({
        shiny::validate(
                need(input$size, "Input a value!")
        )
        
        alpha_scale <- 
                if(length(
                        unique(
                        dplyr::filter(
                                dat4_plot_decomp(), industry == "All")$significance)) == 1 
                   & sum(dplyr::filter(
                           dat4_plot_decomp(), industry == "All")$significance) > 0) { 
                        c(1,0) 
                        } else{
                                c(0,1) }
        
        g <- 
                dat4_plot_decomp() %>% 
                dplyr::filter(industry == "All") %>% 
                ggplot(aes(
                        x = Group,
                        y = Decomposition,
                        alpha = significance)
                ) +
                geom_bar(stat = "identity",
                         position = "identity",
                         colour = '#006272',
                         fill = '#006272') +
                geom_hline(yintercept = 0, size = 0.45) +
                scale_y_continuous(
                        breaks = seq(-0.5, 0.5, by = 0.05),
                        limits =  c(min(dat4_plot_decomp()$limit_min),
                                    max(dat4_plot_decomp()$limit_max)),
                        expand = expansion(mult = c(0, 0.035))
                ) +
                scale_alpha_manual(values = alpha_scale, guide = FALSE) +
                facet_grid(practice_shortname ~ weight, scales = "free_y", space = "free") +
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
                      plot.title = element_blank(),
                      legend.title = element_blank(),
                      legend.position = "none",
                      axis.text.x = element_text(size=11, hjust = -0.5),
                      axis.text.y = element_text(size=11),
                      axis.title = element_text(size=12,face="bold"),
                      strip.background = element_rect(fill = "#dfe3e6"),
                      strip.text.x = element_text(size = 14, face="bold"),
                      strip.text.y = element_text(size = 14, face="bold"),
                      plot.margin = unit(c(0.5,0.5,0.25,0.25), "cm")
                )
        
        
        ggplotly(g, tooltip = c("Decomposition", "significance")) %>%
                config(displaylogo = FALSE,
                       displayModeBar = TRUE,
                       modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                  'lasso2d','zoomIn2d','zoomOut2d',
                                                  'toggleSpikelines', 'hoverClosestCartesian', 
                                                  'hoverCompareCartesian')
                )
        
})


dat4_temp <- dat4 %>% 
        select(-Total) %>%
        pivot_longer(
                c(`Continuers - within`, `Continuers - across`, 
                  `Continuers - enter`, `Continuers - exit`, 
                  Entrants, Exiters, Joiners, Leavers),
                names_to = "Group",
                values_to = "Decomposition"
        ) %>% 
        mutate(Decomposition = as.numeric(ifelse(Decomposition == "c", NA, Decomposition)),
               limit_max = ifelse(Decomposition >= 0.0725 & Decomposition <= 0.1, 0.105, 
                                  ifelse(Decomposition >= 0.1 & Decomposition <= 0.15, 0.155,
                                         ifelse(Decomposition >= 0.15 & Decomposition <= 0.2, 0.205, 
                                                ifelse(Decomposition >= 0.2 & Decomposition <= 0.3, 0.305,
                                                       ifelse(Decomposition >= 0.3 & Decomposition <= 0.4, 0.405,
                                                              ifelse(Decomposition >= 0.4, 0.45, 
                                                                     0.075)))))),
               limit_min = ifelse(Decomposition <= -0.0725 & Decomposition >= -0.1, -0.105, 
                                  ifelse(Decomposition <= -0.1 & Decomposition >= -0.15, -0.155,
                                         ifelse(Decomposition <= -0.15 , -0.185, 
                                                -0.075)))) %>% 
        mutate(limit_max = ifelse(is.na(limit_max), 0.075, limit_max),
               limit_min = ifelse(is.na(limit_min), -0.075, limit_min))

limit_max <- ifelse(dat4_temp$Decomposition >= 0.075 & dat4_temp$Decomposition <= 1,
                    1.05, 
)
# test <- read_excel("Data/2b_within_decomp_CONF.xlsx", 
#                    range = "A2:W20", 
#                    sheet =  1) %>% 
#         drop_na() %>% 
#         filter(VARIABLES != c("y1", "y2")) %>% 
#         pivot_longer(-VARIABLES) %>% 
#         pivot_wider(names_from=VARIABLES, values_from=value) %>% 
#         select(-1)

        
