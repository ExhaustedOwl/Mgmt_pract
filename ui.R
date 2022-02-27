#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Data --------------------------------------------------------------------------
library(shiny)
library(plotly)
library(readxl)
library(DT)
library(shinythemes)
library(tidyverse)
library(htmltools)
library(glue)
library(shinydashboard)
library(haven)

### Aggregate changes

inpts <- list(practice_name = read_stata("Data/T1_overall_wide.dta") %>% 
                select(long_name) %>% 
                filter(long_name != "SMP index") %>% 
                unique() %>% 
                #arrange(long_name) %>% 
                pull(), ## pull = $ which is used to get rid of dataframe attr
              
                size = c("All", "6-19 RME", "20-49 RME", "50-99 RME",
                       "100+ RME"),
              
                industry = c("All", "Agriculture, forestry and fishing", "Mining",
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
                             "Arts and recreation services", "Other services"),
              
              cluster = c("Strategy - focus", "Strategy - practices", 
                          "Supply chain linkages", "Information - Assessment",
                          "Information - scanning", "Employee practices",
                          "Quality and process"),
              
              weight = c("Firm", "Employment")
          )                  

yr <- format(Sys.time(), "%Y")


# Shiny UI ------------------------------------------------------------------
shinyUI(tagList(
  
  tags$head(tags$script(type="text/javascript", src = "code.js")),
  tags$style(HTML('.navbar {
                          height: 77.6px;
                          min-height:77.6px !important;
                          font-size: 17px !important;
                          text-align: center !important;
                        }
                        
                  .navbar-nav > li > a, .navbar-brand {
                          padding-top:1px !important; 
                          padding-bottom:1px !important;
                          height: 77.6px;
                          float: left;
                          height: 77.6px;
                          display: flex;
                          align-items: center;
                        }'
                  )
             ),

  navbarPage(title = "",
             theme = shinytheme("cerulean"),

                          
    tabPanel("Home page",
             fluidPage(
               titlePanel("Interactive data appendix"),
               sidebarLayout(
                             mainPanel(HTML(
                                      '<font size="3">
                                       <br>
                                       This online interactive tool is intended to be an accompanying data appendix for the research paper titled
                                       <b>The Evolution of Management Practices in New Zealand </b> published by the <em>Ministry of 
                                       Business, Innovation and Employment</em>. 
                                       <br>
                                       <br>
                                       The full research paper can be accessed via:<a href="https://www.mbie.govt.nz/data-and-analysis/" target="_blank"> https://www.mbie.govt.nz/data-and-analysis/ </a>
                                       <br>
                                       <br>
                                       For any enquiries regarding the analysis and results, please contact: <a href = "mailto:Lynda.Sanderson@mbie.govt.nz">Lynda.Sanderson@mbie.govt.nz</a>
                                       <br>
                                       <br>
                                       </font>')
                                      ),
                            sidebarPanel(h4("Disclaimer"),
                                         HTML(
                                              '<font size="1.5">
                                              These results are not official statistics. They have been created for research purposes from the Integrated Data Infrastructure (IDI) and 
                                              Longitudinal Business Database (LBD) which are carefully managed by Stats NZ. For more information about the IDI and LBD please visit 
                                              <a href="https://www.stats.govt.nz/integrated-data" target="_blank"> www.stats.govt.nz/integrated-data/</a>.
                                              <br>
                                              <br>
                                              The results are based in part on tax data supplied by Inland Revenue to Stats NZ under the Tax Administration Act 1994 for statistical purposes. 
                                              Any discussion of data limitations or weaknesses is in the context of using the IDI for statistical purposes, and is not related 
                                              to the data’s ability to support Inland Revenue’s core operational requirements.
                                              </font>')
                                         )
                            ),
               fluidRow(column(9, ""), 
                        column(3, img(src = 'Rhombus graphic.png', 
                                      height = '307.2px', 
                                      width = '410.6px')
                               )
                        ),
               hr(),
               tags$footer(HTML(
                               glue(
                                    "<!-- Footer -->
                                    <footer class='page-footer font-large indigo'>
                                    <!-- Copyright -->
                                    <div class='footer-copyright text-center py-3'>© {yr}: Lucas Chen
                                    <br>
                                    <a href='https://'> Source code </a>
                                    </div>
                                    <!-- Copyright -->
                                    </footer>
                                    <!-- Footer -->")
                               )
                           )
               )
             ),
    
    tabPanel("Prevalence of practices",
             sidebarLayout(
               sidebarPanel(HTML(
                 '<font size="2">
                                 This tab provides a plot of the management practice indices for individual practices across four waves of the Business Operations Survey, from 2005 to 2017. 
                                 <br>
                                 <br>
                                 Indices are available by overall practice type (aggregated across industry and firm size). Data points for selected graphs can be viewed and downloaded from the Table tab.
                                 <br>
                                 <br>
                                 <b>To view the practice indices:</b>
                                 
                                 <ol type = "1"> 
                                    <li>Select one or more (max 20) practice of interest in the top box </li>
                                    <li>Click "<em>autoscale</em>" on plot for a zoomed in view</li>
                                 </ol>
                                 </font>' 
                                ),
               hr(),
               h4('Inputs:'),
               selectizeInput('practice_prevalence', "Practice name",
                           inpts$practice_name, 
                           multiple = TRUE,
                           options = list(maxItems = 20),
                           selected = "Assess performance based on cost measures"
                           ) 
               ),
               
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Plot",
                                      plotlyOutput("plot_pract", height = "800px")
                                      ),
                             tabPanel("Table", DTOutput("table_pract"), style = "font-size: 95%; width: 95%")
                             )
                        )
               )
             ),

    tabPanel("Individual practices",
             sidebarLayout(
               sidebarPanel(HTML(
                                '<font size="2">
                                 This tab provides a plot of the management practice indices for each individual practice across four waves of the Business Operations Survey, from 2005 to 2017. 
                                 <br>
                                 <br>
                                 Indices are available by firm size or industry, shown on separate graphs. Data points for selected graphs can be viewed and downloaded from the Table tab.
                                 <br>
                                 <br>
                                 <b>To view the practice indices:</b>
                                 
                                 <ol type = "1"> 
                                    <li>Select one practice of interest in the top box </li>
                                    <li>Select one or more firm size group and/or one or more industry</li>
                                    <li>Click "<em>autoscale</em>" on plot for focused in view</li>
                                 </ol>
                                 </font>'
                                ),
                            hr(),
                            h4('Inputs:'),
                            selectInput('practice_name', "Practice name",
                                        inpts$practice_name, 
                                        selected = "Assess performance based on cost measures"
                                        ), ###
                            selectInput('size', strong('Size'),
                                        inpts$size, 
                                        multiple = TRUE,
                                        selected ="All"
                                               ),
                            selectInput('industry', strong('Industry'), 
                                        inpts$industry,
                                        multiple = TRUE,
                                        selected ="All"
                                               )
                            ),
               
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Plot",
                                      plotlyOutput("plot_size_ind", height = "400px"),
                                      plotlyOutput("plot_industry_ind", height = "400px")
                                      ),
                             tabPanel("Table", DTOutput("table_ind"), style = "font-size: 95%; width: 95%")
                             )
                        )
                      )
             ),
    
    
    tabPanel("Aggregate change",
             sidebarLayout(
               sidebarPanel(HTML(
                                 '<font size="2">
                                 This tab provides a comparison of the change in the aggregate practice indices between 2005 and 2017, grouped according to different clusters of practices.
                                 Solid bars represent practices for which the index values are statistically significantly different in 2017 from 2005 (p-values of <0.10).
                                 Empty bars indicate that the observed change is not statistically significant.
                                 <br>
                                 </font>'
                                 ),
                            hr(),
                            h4('Inputs:'),
                            selectInput('cluster_agg', 'Cluster', 
                                        inpts$cluster, 
                                        multiple = TRUE, 
                                        selected = c("Strategy - focus", "Strategy - practices")
                                        )              
                            ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Plot", 
                                      uiOutput("ui_plot")
                                      ),
                             tabPanel("Table", DTOutput("table_agg"))
                             )
                        )
                      )
            ),
    

    tabPanel("Industry decomposition",
             sidebarLayout(
               sidebarPanel(HTML(
                                 '<font size="2">
                                 This tab decomposes the aggregate change in practice indices between 2005 and 2017 into the change coming from changing industry composition,
                                 defined at the level of two-digit ANZSIC industries (green bars) and that coming from changing prevalence of practices within an industry (blue bars).
                                 Solid bars indicate that the aggregate change over the period is statistically significant (p-value < 0.10).
                                 <br>
                                 </font>'
                                 ),
                            hr(),
                            h4('Inputs:'),
                            selectInput('cluster_decomp', 'Cluster',
                                        inpts$cluster, 
                                        multiple = TRUE,
                                        selected = c("Strategy - focus", "Strategy - practices")
                                        )

                            ),
             
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Plot", 
                                      uiOutput("ui_plot_decomp")
                                      ),
                             tabPanel("Table", DTOutput("table_decomp")) 
                             )
                        )
               )
             ),
    

    tabPanel("Decomposition by dynamics, aggregate",
             sidebarLayout(
               sidebarPanel(HTML(
                                 '<font size="2">
                                 This tab decomposes the aggregate change in practice indices between 2005 and 2017 according to the proximate sources defined in
                                 <b>“The Evolution of Management Practices in New Zealand”</b>.
                                 <br style="line-height: 50%;" />
                                 <br style="line-height: 50%;" />
                                 These are:
                                 <ul type = "1">
                                    <li><b>Continuers – across:</b> change due to changes in the weights applied to continuing firms over time. In the firm-level decomposition this reflects changing survey weights, while in the employment-weighted decomposition it captures changing employment in the survey sample and in the population over time.</li>
                                    <li><b>Continuers – within:</b> change due to within-firm changes in practices among continuing firms.</li>
                                    <li><b>Entrants:</b> change due to new firms born between 2005 and 2017.</li>
                                    <li><b>Exiters:</b> change due to the exit of firms between 2005 and 2017.</li>
                                    <li><b>Joiners:</b> change due to firms which were included in the 2017 survey but not in the 2005 survey (but were active in both years).</li>
                                    <li><b>Leavers:</b> change due to firms which were included in the 2005 survey but not in the 2017 survey (but were active in both years).</li>
                                 </ul>
                                 Solid bars indicate that the change over the period for the relevant group (size group or industry) is statistically significant (p-value < 0.10).
                                 </font>'
                                 ),
                            hr(),
                            h4('Inputs:'),
                            selectInput('practice_name_decomp', 'Practice name',
                                        inpts$practice_name,
                                        multiple = TRUE,
                                        selected = c("Assess performance based on cost measures", "Assess performance based on financial measures")
                                        )
                            ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Plot", 
                                      uiOutput("ui_plot_decomp2")
                                      ),
                             tabPanel("Table", DTOutput("table_decomp2"), 
                                      style = "font-size: 87%; width: 87%")
                             )
                 )
               )
             ),
  

    tabPanel("Decomposition by dynamics, size and industry",
             sidebarLayout(
               sidebarPanel(HTML(
                                 '<font size="2">
                                 This tab decomposes the change in practice indices between 2005 and 2017 according to the proximate sources defined in
                                 <b>“The Evolution of Management Practices in New Zealand”</b>. Results are presented separately by industry and firm size groups.
                                 <br style="line-height: 50%;" />
                                 <br style="line-height: 50%;" />
                                 Decomposition categories include:
                                 <ul type = "1">
                                    <li><b>Continuers – across:</b> change due to changes in the weights applied to continuing firms over time. In the firm-level decomposition this reflects changing survey weights, while in the employment-weighted decomposition it captures changing employment in the survey sample and in the population over time.</li>
                                    <li><b>Continuers – within:</b> change due to within-firm changes in practices among continuing firms.</li>
                                    <li><b>Entrants:</b> change due to new firms born between 2005 and 2017.</li>
                                    <li><b>Exiters:</b> change due to the exit of firms between 2005 and 2017.</li>
                                    <li><b>Joiners:</b> change due to firms which were included in the 2017 survey but not in the 2005 survey (but were active in both years).</li>
                                    <li><b>Leavers:</b> change due to firms which were included in the 2005 survey but not in the 2017 survey (but were active in both years).</li>
                                    <li><b>Continuers – enter group:</b> change in the firm-size specific index due to firms which enter the relevant firm size group due to changes in firm-level employment over the period.</li>
                                    <li><b>Continuers – exit group:</b> change in the firm-size specific index due to firms which exit the relevant firm size group due to changes in firm-level employment over the period.</li>
                                 </ul>
                                 Solid bars indicate that the change over the period for the relevant group (size group or industry) is statistically significant (p-value < 0.10).
                                 </font>'
                                 ),
                            hr(),
                            h4('Inputs:'),
                            selectInput('practice_name_decomp2', 'Practice name',
                                        inpts$practice_name,
                                        multiple = FALSE,
                                        selected = c("Assess performance based on cost measures")
                                        ),
                            selectInput('size_decomp2', strong('Size'),
                                        inpts$size[-1],
                                        multiple = TRUE,
                                        selected ="6-19 RME"
                                        ),
                            selectInput('industry_decomp2', strong('Industry'),
                                        inpts$industry[-c(1,3,5,19)],
                                        multiple = TRUE,
                                        selected ="Agriculture, forestry and fishing"
                                        )
                            ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Plot",
                                      uiOutput("ui_plot_decomp_size"), 
                                      uiOutput("ui_plot_decomp_industry")
                                      ),
                             tabPanel("Table", DTOutput("table_decomp3"), 
                                      style = "font-size: 67.5%; width: 67.5%")
                             )
                        )
                      )
             )
          )
    )
  )



