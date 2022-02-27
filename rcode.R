
# Graphics ---------------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)
df <- read_excel("Data/T1_overall_wide.xlsx")

convert.size <-
        function(x) {
                return(factor(
                        x,
                        levels = c("0", "1", "2", "3", "4"),
                        labels = c("All", "6-19 RME", "20-49 RME", "50-99 RME", "100+ RME")
                ))
        }

# df$size <- convert.size(df$size)
# table(df$size)
# df$size <- factor(df$size,
#                   levels = c("0", "1", "2", "3", "4" ),
#                   labels = c("All", "6-19 RME", "20-49 RME", "50-99 RME", "100+ RME"))
# table(df$size)


convert.industry <- 
        function(x) {
        return(factor(x, levels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", 
                                "10", "11", "12", "13", "14", "15", "16", "17", "18"),
                    labels = c("All", "Agriculture, forestry and fishing", "Mining", "Manufacturing", 
                            "Electricity, gas, water and waste services", "Construction", 
                            "Wholesale trade", "Retail trade", "Accomodation", "Transport, postal and warehousing",
                            "Informationn, media and telecommunications", "Financial and information services",
                            "Rental, hiring and real estate services", "Professional, scientific and technical services",
                            "Administration and support services", "Education and training", "Health care and social assistance",
                            "Arts and recreation services", "Other services" )))
        }

# df$industry <- sapply(df$industry, convert.industry)

convert.cluster <- function(x) {
        return(factor(x, levels=c("n/a", "strat_what", "strat_how", "ss_chains", "info_assess",
                                "info_scan", "employees", "quality"),
               labels = c("Other", "Strategy - focus", "Strategy - practices", "Supply chain linkages", 
                    "Information - Assessment", "Information - scanning", "Employee practices",
                    "Quality and process")))

}

# df$cluster <- sapply(df$cluster, convert.cluster)


df <- df %>% 
        mutate(size = convert.size(size),
               industry = convert.industry(industry),
               cluster = convert.cluster(cluster)) %>% 
        mutate_at(vars(1:6), as.factor) %>% 
        mutate_at(vars(7:22), as.numeric) %>%
        mutate_at(vars(23:26), as.factor)


table(df$size)
table(df$cluster)
table(df$industry)

###Planning procceses
df.aggregate <- 
        subset(df, n == "1", c("variable", "cluster", "industry", 
                               "weight", "change05_17", "prtest05_17", "short_name"))

df.aggregate <- df.aggregate %>% 
        mutate(sig = ifelse(prtest05_17 < 0.1, TRUE, FALSE)) 

df.aggregate %>%
        filter(cluster == "Strategy - focus") %>%
        ggplot(aes(x=short_name, y=change05_17, fill=sig)) +
        geom_bar(stat ="identity", position="identity", colour="#006272") +
        scale_y_continuous(breaks = c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15), limits = c(-0.15,0.15), 
                           expand = expand_scale(mult=c(0.035,0.035))) +
        scale_fill_manual(values=c(NA,'#006272')) +
        facet_grid(~weight) +
        coord_flip() +
        ggtitle("cluster") +
        labs(caption = "Source: BOS, LBD") +
        theme(axis.line = element_line(colour = "black", size = 1),
              panel.grid.major = element_blank(),
              #panel.grid.major.y = element_line('#bdbdbd'),
              panel.grid.minor = element_blank(),
              #panel.grid.minor.y = element_line('#ededed'),
              panel.border = element_blank(),
              panel.background = element_blank(),
              legend.position="bottom",
              legend.direction = "horizontal",
              plot.title = element_text(size=30, vjust = 5, hjust = 0.4,face="bold"),
              axis.text.x=element_text(size=20, vjust = -0.85, hjust = 0.5),
              axis.text.y=element_text(size=20, vjust = 0.35),
              axis.title.x=element_blank(),
              axis.title.y=element_text(size=25,face="bold"),
              axis.ticks.length=unit(.25, "cm"),
              legend.title=element_blank(),
              legend.text=element_text(size=17.5),
              legend.key.size = unit(1.5,"line"),
              legend.key=element_blank(),
              plot.caption = element_text(size = 14, face = "bold.italic", hjust = 1),
              plot.margin = unit(c(2,2.25,1.25,1.25), "cm")) 

df.aggregate %>%
        ggplot(aes(x=short_name, y=change05_17, fill=sig)) +
        geom_bar(stat ="identity", position="identity", colour="#006272") +
        scale_y_continuous(breaks = c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15), limits = c(-0.15,0.15), 
                           expand = expand_scale(mult=c(0.035,0.035))) +
        scale_fill_manual(values=c(NA,'#006272')) +
        facet_grid(weight~cluster) +
        coord_flip() +
        ggtitle("cluster") +
        labs(caption = "Source: BOS, LBD") +
        theme(axis.line = element_line(colour = "black", size = 1),
              panel.grid.major = element_blank(),
              #panel.grid.major.y = element_line('#bdbdbd'),
              panel.grid.minor = element_blank(),
              #panel.grid.minor.y = element_line('#ededed'),
              panel.border = element_blank(),
              panel.background = element_blank(),
              legend.position="bottom",
              legend.direction = "horizontal",
              plot.title = element_text(size=30, vjust = 5, hjust = 0.4,face="bold"),
              axis.text.x=element_text(size=20, vjust = -0.85, hjust = 0.5),
              axis.text.y=element_text(size=20, vjust = 0.35),
              axis.title.x=element_blank(),
              axis.title.y=element_text(size=25,face="bold"),
              axis.ticks.length=unit(.25, "cm"),
              legend.title=element_blank(),
              legend.text=element_text(size=17.5),
              legend.key.size = unit(1.5,"line"),
              legend.key=element_blank(),
              plot.caption = element_text(size = 14, face = "bold.italic", hjust = 1),
              plot.margin = unit(c(2,2.25,1.25,1.25), "cm")) 


df.aggregate2 <- subset(df, n != 1, c("short_name", "cluster", "size", "industry", 
                                      "weight", "rr_2005", "rr_2009", "rr_2013", "rr_2017"))


ggplot(df, aes(x=Quarter, y=Percent, colour = df$variable)) +
        geom_line(aes(group = df$variable), size = 1.25) + 
        geom_point(size = 0.75) + 
        geom_hline(yintercept = 0, col = '#7e7e7e', size = 1, linetype=1) +
        scale_color_manual(values=c('#006272','#97D700','#00B5E2', '#753BBD')) + 
        scale_x_discrete(breaks = df$Quarter[seq(1, length(df$Quarter), by = 16)],
                         labels=c("2006Q1" = "2006", "2007Q1" = "2007",
                                  "2008Q1" = "2008", "2009Q1" = "2009",
                                  "2010Q1" = "2010", "2011Q1" = "2011",
                                  "2012Q1" = "2012", "2013Q1" = "2013",
                                  "2014Q1" = "2014", "2015Q1" = "2015",
                                  "2016Q1" = "2016", "2017Q1" = "2017",
                                  "2018Q1" = "2018", "2019Q1" = "2019"),
                         expand = expand_scale(mult=c(0,0.015))) +
        scale_y_continuous(breaks = seq(-2, 6, by = 2), expand = expand_scale(mult=c(0.1,0.035))) +
        ggtitle("Consumer Price Index vs Core Inflation") +
        labs(y = "Annual percent change", caption = "Source: RBNZ, StatsNZ") +
        theme(axis.line = element_line(colour = "black", size = 1),
              panel.grid.major = element_blank(),
              #panel.grid.major.y = element_line('#bdbdbd'),
              panel.grid.minor = element_blank(),
              #panel.grid.minor.y = element_line('#ededed'),
              panel.border = element_blank(),
              panel.background = element_blank(),
              legend.position=c(0.48, 0.04),
              legend.direction = "horizontal",
              plot.title = element_text(size=24, hjust = 0.4,face="bold"),
              axis.text.x=element_text(size=18, angle = -45, vjust = 0.45, hjust = 0.35),
              axis.text.y=element_text(size=18),
              axis.title.x=element_text(size=20,face="bold", vjust = -2),
              axis.title.y=element_text(size=20,face="bold"),
              legend.title=element_blank(), 
              legend.text=element_text(size=16),
              legend.key.size = unit(1.2,"line"),
              plot.caption = element_text(size = 14, face = "bold.italic", hjust = 1))   



# Something ---------------------------------------------------------------


