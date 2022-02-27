library(shiny)
library(plotly)
library(readxl)
library(DT)
library(shinythemes)
library(tidyverse)
library(magrittr)
library(haven)
library(glue)


shinyServer(function(input, output) {
        dat <- read_stata("Data/T1_overall_wide.dta")
        
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
        
        dat2 <- read_excel("Data/2c_decomp_CONF.xlsx", 
                           range = cell_cols("D:J"), 
                           sheet =  "pw_anz06_transpose"
                           ) %>% 
                select(variable, cluster, weight, rr_decomp_within, 
                       rr_decomp_across, rr_D_total
                       ) %>%
                rbind(
                        read_excel("Data/2c_decomp_CONF.xlsx", 
                                   range = cell_cols("D:J"), 
                                   sheet = "ew_anz06_transpose"
                                   ) %>% 
                                select(variable, cluster, weight, 
                                       rr_decomp_within, rr_decomp_across, rr_D_total)
                        ) %>%
                mutate(cluster = convert.cluster(cluster),
                       weight = convert.weight(weight)
                       ) %>%
                filter(cluster != "Other") %>% 
                rename(variable_code = variable,
                       decomp_within = rr_decomp_within, 
                       decomp_across = rr_decomp_across,
                       decomp_overall = rr_D_total
                       ) %>%
                mutate_at(vars(1:3), as.factor) %>%
                mutate_at(vars(4:6), as.numeric) %>% 
                left_join(
                        subset(dat, dat$size== "All" & dat$industry == "All", 
                               select = c("variable_code", "practice_name", 
                                          "weight", "size", "industry", 
                                          "p_value", "practice_shortname")
                               ), by = c("variable_code" = "variable_code", 
                                         "weight" = "weight")
                        ) %>%
                drop_na() %>% 
                mutate(variable_code = as.factor(variable_code))
        
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
                                          "p_value", "practice_shortname")), 
                        by = c("variable_code" = "variable_code", 
                               "weight" = "weight",
                               "size" = "size",
                               "industry" = "industry")
                        ) %>%  
                drop_na()
        
        ### Tab 2; Prevalence of practices ###
        dat_pract <- 
                reactive({
                        dat_final <- dat %>% 
                                        dplyr::select(practice_name, cluster:weight, 
                                                      `2005`:se_2017, practice_shortname
                                                      ) %>% 
                                        dplyr::filter(practice_name %in% input$practice_prevalence,
                                                      size == "All",
                                                      industry == "All"
                                                      ) %>% 
                                        relocate(practice_shortname, .after = last_col())
                        
                        return(dat_final)
                        
                })
        
        
        dat_plot_pract <- 
                reactive({
                        dat_plot <- dat_pract() 
                        
                        dat_plot_rr <- dat_plot %>% 
                                        select(-starts_with("se")) %>%
                                        pivot_longer(c(`2005`, `2009`, `2013`, `2017`), 
                                                     names_to = "year",
                                                     values_to = "value"
                                                     ) %>% 
                                        mutate(year = as.numeric(year))
                        
                        dat_plot_se <- dat_plot %>% 
                                        select(-`2005`, -`2009`, -`2013`, -`2017`) %>%
                                        pivot_longer(c(starts_with("se")),
                                                     names_to = "year",
                                                     values_to = "se"
                                                     ) %>%
                                        mutate(year = as.numeric(gsub("se_", "", year)))
                        
                        dat_plot <- 
                                dat_plot_rr %>% 
                                left_join(dat_plot_se) 
                        
                        return(dat_plot)
                        
                })
        
        output$plot_pract <- renderPlotly({
                shiny::validate(
                                need(input$practice_prevalence, "Input a value!")
                                )
                
                g <-     
                        dat_plot_pract() %>% 
                        ggplot(aes(x = year,
                                   y = value,
                                   col = practice_shortname)
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
                                           expand = expansion(mult = c(0, 0))
                                           ) +
                        scale_x_continuous(breaks = c(2005, 2009, 2013, 2017),
                                           labels = c("2005", "2009","2013", "2017")
                                           ) +
                        scale_colour_manual(values = c('#006272', '#97D700', '#00B5E2', 
                                                       '#753BBD', '#DF1995', '#FF6900', 
                                                       '#FBE122', '#000000',
                                                       '#91bbc2', '#caeb7e', '#81dbf1',
                                                       '#bfa5e1', '#f091cc', '#ffb581',
                                                       '#fdf29c', '#252443', '#a8a7b4',
                                                       '#c6dce0', '#e9f7ca', '#bababa')
                                            ) +
                        labs(y = "", x = "") +
                        theme_bw() +
                        theme(panel.grid.major.y = element_line(colour = "#dff7ff"),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_blank(),
                              panel.background = element_blank(),
                              panel.spacing = unit(.25, units = "cm"),
                              plot.title = element_text(size=14, hjust = 5, vjust = -5, face="bold"),
                              legend.title = element_blank(),
                              axis.text.x=element_text(size=10),
                              axis.text.y=element_text(size=10),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text = element_text(size = 14, face="bold"),
                              plot.margin = unit(c(0.5,0.25,0.5,0.25), "cm")
                              )
                
                ggplotly(g) %>% 
                        layout(legend = list(title=list(text='<b> Practice: </b>'),
                                             x = 0.0225, y = -0.125, 
                                             orientation = 'h')
                               ) %>%  #yaxis = list(autorange = TRUE)
                        config(displaylogo = FALSE,
                               displayModeBar = TRUE,
                               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                          'lasso2d','zoomIn2d','zoomOut2d',
                                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                                          'hoverCompareCartesian') #'resetScale2d' 'autoScale2d'
                        )
                
                
                
        })
        
        output$table_pract <- 
                renderDT({
                        dat_pract() %>% 
                                datatable(extensions = 'Buttons', 
                                          rownames = FALSE,
                                          options = list(dom = 'Brtip',
                                                         lengthMenu = list(c(5, 15, -1), 
                                                                           c('5', '15', 'All')),
                                                         pageLength = 10,
                                                         buttons = list(
                                                                 list(extend = 'copy',
                                                                      buttons = c('copy'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'csv',
                                                                      buttons = c('csv'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'print',
                                                                      buttons = c('print'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show All',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(-1);
                                                                                        dt.ajax.reload();
                                                                                        }")
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show Less',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(10);
                                                                                        dt.ajax.reload();}")
                                                                 )
                                                         )
                                          )
                                )
                }) 
        
        
        ### Tab 2; Individual Practices ###
        dat_ind <- 
                reactive({
                        dat_s <-  
                                dat %>% 
                                dplyr::select(practice_name, cluster:weight, 
                                              `2005`:se_2017, practice_shortname
                                              ) %>%
                                dplyr::filter(practice_name %in% input$practice_name,
                                              size %in% input$size,
                                              industry == "All"
                                              ) 
                        dat_i <-  
                                dat %>% 
                                dplyr::select(practice_name, cluster:weight, 
                                              `2005`:se_2017, practice_shortname
                                              ) %>%
                                dplyr::filter(practice_name %in% input$practice_name,
                                              industry %in% input$industry,
                                              size == "All"
                                              ) 
                        
                        dat_final <- rbind(dat_s, dat_i) %>% 
                                        distinct() %>% 
                                        relocate(practice_shortname, .after = last_col())
                        
                        return(dat_final)
                        
                })
        
        
        dat_plot_ind<- 
                reactive({
                        dat_plot <- dat_ind() 
                        dat_plot_rr <- 
                                dat_plot %>% 
                                select(-starts_with("se")) %>%
                                pivot_longer(
                                        c(`2005`, `2009`, `2013`, `2017`), 
                                        names_to = "year",
                                        values_to = "value"
                                        ) %>% 
                                mutate(year = as.numeric(year))
                        
                        dat_plot_se <- 
                                dat_plot %>% 
                                select(-`2005`, -`2009`, -`2013`, -`2017`) %>%
                                pivot_longer(
                                        c(starts_with("se")),
                                        names_to = "year",
                                        values_to = "se"
                                        ) %>% 
                                mutate(year = as.numeric(gsub("se_", "", year)))
                        
                        dat_plot <- 
                                dat_plot_rr %>% 
                                left_join(dat_plot_se) 
                        
                        return(dat_plot)
                        
                })
        
        output$plot_size_ind <- renderPlotly({
                shiny::validate(
                        need(input$size, "Input a value!")
                )
                
                dodge <- position_dodge(width=0.9)
                
                g <-     
                        dat_plot_ind() %>% 
                        filter(industry == "All") %>% 
                        ggplot(aes(x = year,
                                   y = value,
                                   col = size)
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
                                           expand = expansion(mult = c(0, 0))
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
                              plot.title = element_text(size=14, hjust = 5, vjust = -5, face="bold"),
                              legend.title = element_blank(),
                              axis.text.x=element_text(size=10),
                              axis.text.y=element_text(size=10),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text = element_text(size = 14, face="bold"),
                              plot.margin = unit(c(0.25,0.25,0.5,0.25), "cm")
                              )
                
                ggplotly(g) %>% 
                        layout(legend = list(title=list(text='<b> Size: </b>'),
                                             x = 0.0225, y = -0.125, 
                                             orientation = 'h')
                               ) %>%  #yaxis = list(autorange = TRUE)
                        config(displaylogo = FALSE,
                               displayModeBar = TRUE,
                               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                          'lasso2d','zoomIn2d','zoomOut2d',
                                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                                          'hoverCompareCartesian') #'resetScale2d' 'autoScale2d'
                        )
                
                
                
        })
        
        output$plot_industry_ind <- renderPlotly({
                shiny::validate(
                        need(input$industry, "Input a value!")
                )
                dodge <- position_dodge(width=0.9)
                
                
                g <-     
                        dat_plot_ind() %>% 
                        filter(size == "All") %>% 
                        ggplot(aes(x = year,
                                   y = value,
                                   col = industry)
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
                                           expand = expansion(mult = c(0, 0))
                                           ) +
                        scale_x_continuous(breaks = c(2005, 2009, 2013, 2017),
                                           labels = c("2005", "2009","2013", "2017")
                                           ) +
                        scale_colour_manual(values = c('#006272', '#97D700', '#00B5E2', 
                                                       '#753BBD', '#DF1995', '#FF6900', 
                                                       '#FBE122', '#000000',
                                                       '#91bbc2', '#caeb7e', '#81dbf1',
                                                       '#bfa5e1', '#f091cc', '#ffb581',
                                                       '#fdf29c', '#252443', '#a8a7b4',
                                                       '#c6dce0', '#e9f7ca')
                                            ) +
                        labs(y = "", x = "") +
                        theme_bw() +
                        theme(panel.grid.major.y = element_line(colour = "#dff7ff"),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_blank(),
                              panel.background = element_blank(),
                              panel.spacing = unit(.75, units = "cm"),
                              plot.title = element_text(size=14, hjust = 5, vjust = -5, face="bold"),
                              legend.title = element_blank(),
                              axis.text.x=element_text(size=10),
                              axis.text.y=element_text(size=10),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text = element_text(size = 14, face="bold"),
                              plot.margin = unit(c(0.25,0.25,0.5,0.25), "cm")
                        )
                
                ggplotly(g) %>% 
                        layout(legend = list(title=list(text='<b> Industry: </b>'),
                                             x = 0.0225, y = -0.125, 
                                             orientation = 'h', yaxis = list(autorange = TRUE)) 
                        ) %>%  
                        config(displaylogo = FALSE,
                               displayModeBar = TRUE,
                               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                          'lasso2d','zoomIn2d','zoomOut2d',
                                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                                          'hoverCompareCartesian') #'resetScale2d' 'autoScale2d'
                        )
                
                
        })
        
        
        output$table_ind <- 
                renderDT({
                        dat_ind() %>% 
                                datatable(extensions = 'Buttons', 
                                          rownames = FALSE,
                                          options = list(dom = 'Brtip',
                                                         lengthMenu = list(c(5, 15, -1), 
                                                                           c('5', '15', 'All')),
                                                         pageLength = 10,
                                                         buttons = list(
                                                                        list(extend = 'copy',
                                                                             buttons = c('copy'),
                                                                             exportOptions = list(modifiers = list(page = "current"))
                                                                             ),
                                                                        list(extend = 'csv',
                                                                             buttons = c('csv'),
                                                                             exportOptions = list(modifiers = list(page = "current"))
                                                                             ),
                                                                        list(extend = 'print',
                                                                             buttons = c('print'),
                                                                             exportOptions = list(modifiers = list(page = "current"))
                                                                             ),
                                                                        list(extend = "collection",
                                                                             text = 'Show All',
                                                                             action = DT::JS(
                                                                                        "function ( e, dt, node, config ) {
                                                                                        dt.page.len(-1);
                                                                                        dt.ajax.reload();
                                                                                        }")
                                                                             ),
                                                                        list(extend = "collection",
                                                                             text = 'Show Less',
                                                                             action = DT::JS(
                                                                                        "function ( e, dt, node, config ) {
                                                                                        dt.page.len(10);
                                                                                        dt.ajax.reload();}")
                                                                             )
                                                                        )
                                                         )
                                          )
        }) 
        


        ### Tab 2; Aggregate change/decomposition ###
        dat_agg <-
                reactive({
                        dat_final <- 
                                dat %>% 
                                filter(n == "1") %>% 
                                dplyr::select(practice_name, cluster:weight,
                                              aggregate_change, p_value, 
                                              practice_shortname
                                              ) %>%
                                filter(cluster %in% input$cluster_agg)
                        
                        return(dat_final)
                        })
        
        output$ui_plot <- renderUI({
                
                n_facets <- length(input$cluster_agg) * 400
                plotlyOutput("plot_agg", height = glue("{n_facets}px"))
                
        })
        
         output$plot_agg <- renderPlotly({
                 shiny::validate(
                         need(input$cluster_agg, "Input a value!")
                         )

                 g <-
                   dat_agg() %>% 
                         mutate(significance = ifelse(p_value < 0.1, TRUE,
                                                      FALSE)) %>% 
                         ggplot(aes(x = practice_name,
                                    y = aggregate_change,
                                    alpha = significance)
                                ) +
                         geom_bar(stat = "identity",
                                  position = "identity",
                                  colour = "#006272",
                                  fill = "#006272"
                                  ) +
                         geom_hline(yintercept = 0, size = 0.45) +
                         scale_y_continuous(
                                 breaks = c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15),
                                 limits = c(-0.13, 0.13),
                                 expand = expansion(mult = c(0, 0.035))
                                 ) +
                         scale_alpha_manual(values = c(0,1)) +
                         facet_grid(cluster ~ weight, scales = "free_y", space = "free") +
                         coord_flip() +
                         labs(title = "", x = "", y = "") +
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
                               axis.text.x=element_text(size=11, hjust = -0.5),
                               axis.text.y=element_text(size=11),
                               axis.title = element_text(size=12,face="bold"),
                               strip.background = element_rect(fill = "#dfe3e6"),
                               strip.text = element_text(size = 14, face="bold"),
                               plot.margin = unit(c(0.5,0.5,0.25,0.25), "cm")
                               )               
                 
                 ggplotly(g, tooltip = c("aggregate_change", "significance")) %>%
                         # layout(uniformtext=list(minsize=8, mode='show'),
                         #        yaxis = list(ticklen = 20, tickcolor = "transparent")
                         #        ) %>%  
                         config(displaylogo = FALSE,
                                displayModeBar = TRUE,
                                modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                           'lasso2d','zoomIn2d','zoomOut2d',
                                                           'toggleSpikelines', 'hoverClosestCartesian', 
                                                           'hoverCompareCartesian')
                                )

         })

         output$table_agg <- 
                 renderDT({
                         dat_agg() %>% 
                                 datatable(extensions = 'Buttons', 
                                           rownames = FALSE,
                                           options = list(dom = 'Brtip',
                                                          lengthMenu = list(c(5, 15, -1), 
                                                                            c('5', '15', 'All')),
                                                          pageLength = 10,
                                                          buttons = list(
                                                                  list(extend = 'copy',
                                                                       buttons = c('copy'),
                                                                       exportOptions = list(modifiers = list(page = "current"))
                                                                       ),
                                                                  list(extend = 'csv',
                                                                       buttons = c('csv'),
                                                                       exportOptions = list(modifiers = list(page = "current"))
                                                                       ),
                                                                  list(extend = 'print',
                                                                       buttons = c('print'),
                                                                       exportOptions = list(modifiers = list(page = "current"))
                                                                       ),
                                                                  list(extend = "collection",
                                                                       text = 'Show All',
                                                                       action = DT::JS(
                                                                               "function ( e, dt, node, config ) {
                                                                                        dt.page.len(-1);
                                                                                        dt.ajax.reload();
                                                                                        }")
                                                                       ),
                                                                  list(extend = "collection",
                                                                       text = 'Show Less',
                                                                       action = DT::JS(
                                                                               "function ( e, dt, node, config ) {
                                                                                        dt.page.len(10);
                                                                                        dt.ajax.reload();}")
                                                                       )
                                                                  )
                                                          )
                                           )
        }) 
         

        ### Tab 4; industry decomposition ###
        dat2_decomp <-
                reactive({
                        dat2_final <-
                                dat2  %>%
                                dplyr::select(
                                        practice_name, cluster, size,
                                        industry, weight, decomp_within,
                                        decomp_across, decomp_overall,
                                        p_value, practice_shortname
                                        ) %>% 
                                filter(cluster %in% input$cluster_decomp)

                        return(dat2_final)
                })
        
        dat2_plot_decomp <- 
                reactive({
                        dat2_plot <- dat2_decomp() 
                        
                        dat2_plot %<>% 
                                select(practice_name, cluster, weight, 
                                       decomp_within, decomp_across, p_value
                                       ) %>%
                                pivot_longer(
                                        c(decomp_within, decomp_across),
                                        names_to = "Type",
                                        values_to = "Decomposition"
                                        )  %>% 
                                mutate(significance = ifelse(p_value < 0.1, TRUE,
                                                             FALSE))
                        
                        return(dat2_plot)
                        
                })
        
        output$ui_plot_decomp <- renderUI({
                
                n_facets <- length(input$cluster_decomp) * 400
                plotlyOutput("plot_decomp", height = glue("{n_facets}px"))
                
        })
        
        output$plot_decomp <- renderPlotly({
                shiny::validate(
                        need(input$cluster_decomp, "Input a value!")
                        )
                
                g <- 
                    dat2_plot_decomp() %>% 
                        ggplot(aes(
                                x = practice_name,
                                y = Decomposition,
                                fill = Type,
                                colour = Type,
                                alpha = significance)
                               ) +
                        geom_bar(stat = "identity") +
                        geom_hline(yintercept = 0, size = 0.45) +
                        scale_y_continuous(
                                breaks = c(-0.15,-0.1,-0.05, 0, 0.05, 0.1, 0.15),
                                limits = c(-0.1375, 0.1375),
                                expand = expansion(mult = c(0, 0.035))
                                ) +
                        scale_fill_manual(values = c('#97D700', '#006272'), guide = "none") +
                        scale_colour_manual(values = c('#97D700', '#006272'), guide = "none") +
                        scale_alpha_manual(values = c(0, 1), guide = "none") +
                        facet_grid(cluster ~ weight, scales = "free_y", space = "free") +
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
                              axis.text.x=element_text(size=11, hjust = -0.5),
                              axis.text.y=element_text(size=11),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text = element_text(size = 14, face="bold"),
                              plot.margin = unit(c(0.5,0.5,0.25,0.25), "cm")
                              )


                ggplotly(g, tooltip = c("Decomposition", "fill", "significance")) %>%
                        config(displaylogo = FALSE,
                               displayModeBar = TRUE,
                               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                          'lasso2d','zoomIn2d','zoomOut2d',
                                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                                          'hoverCompareCartesian')
                        )

        })

        output$table_decomp <-
                renderDT({
                        dat2_decomp() %>% 
                                datatable(extensions = 'Buttons', 
                                          rownames = FALSE,
                                          options = list(dom = 'Brtip',
                                                         lengthMenu = list(c(5, 15, -1), 
                                                                           c('5', '15', 'All')),
                                                         pageLength = 10,
                                                         buttons = list(
                                                                 list(extend = 'copy',
                                                                      buttons = c('copy'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'csv',
                                                                      buttons = c('csv'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'print',
                                                                      buttons = c('print'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show All',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(-1);
                                                                                        dt.ajax.reload();
                                                                                        }")
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show Less',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(10);
                                                                                        dt.ajax.reload();}")
                                                                 )
                                                         )
                                          )
                                )
                })

        ### Tab 5; Decomp by dynamics ###
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
                                select(c(1,5:11,13,14)) %>%
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
                                                          -0.045),
                                       limit_max = ifelse(practice_name == "Measures to reduce environmental impact", 0.05,
                                                          0.045),
                                       Group = fct_rev(Group))
                        
                        return(dat3_plot)
                        
                })
        
        output$ui_plot_decomp2 <- renderUI({
                
                n_facets <- length(input$practice_name_decomp) * 400
                plotlyOutput("plot_decomp2", height = glue("{n_facets}px"))
                
        })

        output$plot_decomp2 <- renderPlotly({
                shiny::validate(
                        need(input$practice_name_decomp, "Input a value!")
                )
                
                alpha_scale <- 
                        if(length(unique(dat3_plot_decomp()$significance)) == 1 & 
                           sum(dat3_plot_decomp()$significance) > 0) {
                                c(1,0) 
                                } else{ 
                                        c(0,1)
                                        }
                
                g <- 
                        dat3_plot_decomp() %>% 
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
                                breaks = c(-0.08, -0.06, -0.04, -0.02, 0, 0.02,
                                           0.04, 0.06, 0.08),
                                limits =  c(min(dat3_plot_decomp()$limit_min),
                                            max(dat3_plot_decomp()$limit_max)),
                                expand = expansion(mult = c(0, 0.035))
                        ) +
                        scale_alpha_manual(values = alpha_scale, guide = "none") +
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

        output$table_decomp2 <-
                renderDT({
                        dat3_decomp() %>% 
                                datatable(extensions = 'Buttons', 
                                          rownames = FALSE,
                                          options = list(dom = 'Brtip',
                                                         lengthMenu = list(c(5, 15, -1), 
                                                                           c('5', '15', 'All')),
                                                         pageLength = 10,
                                                         buttons = list(
                                                                 list(extend = 'copy',
                                                                      buttons = c('copy'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'csv',
                                                                      buttons = c('csv'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'print',
                                                                      buttons = c('print'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show All',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(-1);
                                                                                        dt.ajax.reload();
                                                                                        }")
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show Less',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(10);
                                                                                        dt.ajax.reload();}")
                                                                 )
                                                         )
                                          )
                                )
                })
        
        ### Tab 6; Decomp by dynamics; size and industry ###
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
                                       limit_max = ifelse(Decomposition >= 0.025 & Decomposition <= 0.05, 0.055, 
                                                          ifelse(Decomposition >= 0.05 & Decomposition <= 0.075, 0.0755, 
                                                                  ifelse(Decomposition >= 0.075 & Decomposition <= 0.1, 0.105, 
                                                                          ifelse(Decomposition >= 0.1 & Decomposition <= 0.15, 0.155,
                                                                                 ifelse(Decomposition >= 0.15 & Decomposition <= 0.2, 0.205, 
                                                                                        ifelse(Decomposition >= 0.2 & Decomposition <= 0.3, 0.305,
                                                                                               ifelse(Decomposition >= 0.3 & Decomposition <= 0.4, 0.405,
                                                                                                      ifelse(Decomposition >= 0.4, 0.45, 
                                                                                                             0.0275)))))))),
                                       limit_min = ifelse(Decomposition <= -0.025 & Decomposition >= -0.05, -0.055,
                                                          ifelse(Decomposition <= -0.0525 & Decomposition >= -0.075, -0.0755,
                                                                  ifelse(Decomposition <= -0.0725 & Decomposition >= -0.1, -0.105, 
                                                                          ifelse(Decomposition <= -0.1 & Decomposition >= -0.15, -0.155,
                                                                                 ifelse(Decomposition <= -0.15 , -0.185, 
                                                                                        -0.0275))))),
                                       Group = fct_rev(Group)) %>% 
                                mutate(limit_max = ifelse(is.na(limit_max), 0.0275, limit_max),
                                       limit_min = ifelse(is.na(limit_min), -0.0275, limit_min))
                        
                        return(dat4_plot)
                        
                })
        
        output$ui_plot_decomp_size <- renderUI({
                
                n_size <- length(input$size_decomp2) * 200 + 200
                plotlyOutput("plot_size_decomp", height = glue("{n_size}px"))
                
        })
        
        
        output$plot_size_decomp <- renderPlotly({
                shiny::validate(
                        need(input$size_decomp2, "Input a value!")
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
                                fill = size,
                                colour = size,
                                alpha = significance)
                        ) +
                        geom_bar(stat = "identity",
                                 position = position_dodge2(preserve = "single",
                                                            padding =  0.075),
                                 width = 0.8) +
                        geom_hline(yintercept = 0, size = 0.45) +
                        scale_y_continuous(
                                breaks = seq(-0.4, 0.4, by = 0.02),
                                limits = c(min(dat4_plot_decomp()$limit_min),
                                           max(dat4_plot_decomp()$limit_max)),
                                expand = expansion(mult = c(0, 0))
                                ) +
                        scale_fill_manual(values = c('#006272', '#97D700', '#00B5E2', 
                                                       '#753BBD'), guide = "none") +
                        scale_colour_manual(values = c('#006272', '#97D700', '#00B5E2', 
                                                     '#753BBD'), guide = "none") +
                        scale_alpha_manual(values = alpha_scale) +
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
                              axis.text.x = element_text(size=11, hjust = -0.5),
                              axis.text.y = element_text(size=11),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text.x = element_text(size = 14, face="bold"),
                              strip.text.y = element_text(size = 14, face="bold"),
                              plot.margin = unit(c(0.5,0.5,0.25,0.25), "cm")
                        )
                
                
                ggplotly(g, tooltip = c("Decomposition", "significance", "fill")) %>%
                        layout(legend = list(title=list(text='<b> Size: </b>'),
                                             x = 0.0225, y = -0.125, 
                                             orientation = 'h')) %>% 
                        config(displaylogo = FALSE,
                               displayModeBar = TRUE,
                               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                          'lasso2d','zoomIn2d','zoomOut2d',
                                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                                          'hoverCompareCartesian')
                        ) 
                
        })
        
        output$ui_plot_decomp_industry <- renderUI({
                
                n_industry <- length(input$industry_decomp2) * 200 + 200
                plotlyOutput("plot_industry_decomp", height = glue("{n_industry}px"))
                
        })
        
        
        output$plot_industry_decomp <- renderPlotly({
                shiny::validate(
                        need(input$industry_decomp2, "Input a value!")
                )
                
                alpha_scale <- 
                        if(length(
                                unique(
                                        dplyr::filter(
                                                dat4_plot_decomp(), size == "All")$significance)) == 1 
                           & sum(dplyr::filter(
                                   dat4_plot_decomp(), size == "All")$significance) > 0) { 
                                c(1,0) 
                        } else{
                                c(0,1) }
                
                g <- 
                        dat4_plot_decomp() %>% 
                        dplyr::filter(size == "All") %>% 
                        ggplot(aes(
                                x = Group,
                                y = Decomposition,
                                fill = industry,
                                colour = industry,
                                alpha = significance)
                        ) +
                        geom_bar(stat = "identity",
                                 position = position_dodge2(preserve = "single",
                                                            padding =  0.075),
                                 width = 0.8) +
                        geom_hline(yintercept = 0, size = 0.45) +
                        scale_y_continuous(
                                breaks = seq(-0.4, 0.4, by = 0.02),
                                limits = c(min(dat4_plot_decomp()$limit_min),
                                           max(dat4_plot_decomp()$limit_max)),
                                expand = expansion(mult = c(0, 0))
                        ) +
                        
                        scale_fill_manual(values = c('#006272', '#97D700', '#00B5E2', 
                                                       '#753BBD', '#DF1995', '#FF6900', 
                                                       '#FBE122', '#000000', '#91bbc2', 
                                                       '#caeb7e', '#81dbf1', '#bfa5e1',
                                                       '#f091cc', '#ffb581', '#fdf29c')
                        ) +
                        scale_colour_manual(values = c('#006272', '#97D700', '#00B5E2', 
                                                     '#753BBD', '#DF1995', '#FF6900', 
                                                     '#FBE122', '#000000', '#91bbc2', 
                                                     '#caeb7e', '#81dbf1', '#bfa5e1',
                                                     '#f091cc', '#ffb581', '#fdf29c'),
                                            guide = "none"
                                            ) +
                        scale_alpha_manual(values = alpha_scale, guide = "none") +
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
                              axis.text.x = element_text(size=11, hjust = -0.5),
                              axis.text.y = element_text(size=11),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text.x = element_text(size = 14, face="bold"),
                              strip.text.y = element_text(size = 14, face="bold"),
                              plot.margin = unit(c(0.5,0.5,0.25,0.25), "cm")
                        )
                
                
                ggplotly(g, tooltip = c("Decomposition", "significance", "fill")) %>%
                        layout(legend = list(title=list(text='<b> Industry: </b>'),
                                             x = 0.0225, y = -0.125, 
                                             orientation = 'h')) %>% 
                        config(displaylogo = FALSE,
                               displayModeBar = TRUE,
                               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                          'lasso2d','zoomIn2d','zoomOut2d',
                                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                                          'hoverCompareCartesian')
                        )
                
        })
        
        output$table_decomp3 <-
                renderDT({
                        dat4_decomp() %>% 
                                datatable(extensions = 'Buttons', 
                                          rownames = FALSE,
                                          options = list(dom = 'Brtip',
                                                         lengthMenu = list(c(5, 15, -1), 
                                                                           c('5', '15', 'All')),
                                                         pageLength = 10,
                                                         buttons = list(
                                                                 list(extend = 'copy',
                                                                      buttons = c('copy'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'csv',
                                                                      buttons = c('csv'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'print',
                                                                      buttons = c('print'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show All',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(-1);
                                                                                        dt.ajax.reload();
                                                                                        }")
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show Less',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(10);
                                                                                        dt.ajax.reload();}")
                                                                 )
                                                         )
                                          )
                                )
                })


})

