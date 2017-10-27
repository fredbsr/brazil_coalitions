## TIME SERIES - GOVERNING COSTS INDEX
## Kitchen
library(Hmisc)
library(utils)
library(tseries)
library(zoo)
library(lubridate)
library(xts)
library(tsfa)
library(bsts)
library(tidyverse)
library(readxl)
library(stringi)
library(stringr)
library(ggthemes)
library(htmlwidgets)
library(gganimate)
library(extrafont)


# Esquema de cores dos mandatos
escala_cor_mandato=c("#234784",
                     "#3071dd",
                     "#600000",
                     "#a30000",
                     "#7f007b",
                     "#ef02e8",
                     "#464951")

theme_coalitions <-   theme(text = element_text(family = "Calibri"),
                            plot.title=element_text(face="bold", size=16),
                            axis.title.x = element_text(size = 12,colour="black"),
    axis.text = element_text(size = 10,colour="black"),
    axis.text.x = element_text(size = 10,colour="black"),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.line.y = element_blank(),
    panel.grid.major = element_line(color = "grey85", size = 0.2,linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.position="bottom")


setwd("C:/Users/36137/Dropbox/IPG_Papers_CoalMgm/_data")
# setwd("~/Dropbox/IPG_Papers_CoalMgm/_data")

CMV8_longs <- readRDS("parties_brazil_long.RDS") 


coalition_anim_full <- CMV8_longs %>%
     mutate(year=year(date_complete),
            month=month(date_complete),
            date_comp=substr(date_complete,1,7)) %>%
         dplyr::filter(is.na(coal)==F
                       ,month %in% c(3,9)
                       ,ideo>0
                                ) %>%
        group_by(date_yearmon) %>%
        mutate(maxcoal=as.numeric(ifelse(coal=="Coalition",max(ideo*(coal=="Coalition"),na.rm=T),NaN)),
            mincoal=ifelse(coal=="Coalition",min(ideo*(maxcoal>0),na.rm=T),NaN),
            max_ideo=pmax(av_coal_ideo,av_cong_ideo),
            min_ideo=pmin(av_coal_ideo,av_cong_ideo),
            eqdist_coal_con=(max_ideo+min_ideo)/2) %>%
        ungroup () %>%
      ggplot(aes(y=seats, x=ideo,
          xmin = min_ideo,xmax = max_ideo,
          frame=paste0(date_comp," - ",term))) +
        geom_point(aes(shape=coal,color=coal,label=party), size=4) +
        scale_shape_manual(values=c(1,19),"") +
        scale_color_manual(values = c("black","darkseagreen4"),"") +
        scale_y_continuous("Seats",expand=c(0,3))+
        scale_x_continuous("Ideology",limits=c(1,10),breaks=c(1:10)) +
        geom_text(aes(label=party), size=3, vjust=0.4, hjust=-0.4) +
      #average congress ideology
        geom_vline(aes(xintercept = av_cong_ideo,frame=paste0(date_comp," - ",term)),
              size=2,
              colour="steelblue4",alpha=0.7) +
        geom_text(aes(x=av_cong_ideo,frame=paste0(date_comp," - ",term),
                  y=-7,
                  label=paste("Floor Ideology =",
                  round(av_cong_ideo,
                  digits = 2))),
                  size=3.5, angle=0, vjust=-0.4, hjust=0,color="steelblue4") +
#average coalition ideology
        geom_vline(aes(xintercept = av_coal_ideo,frame=paste0(date_comp," - ",term)),
                  size=2,
                  colour="darkseagreen4",alpha=0.7) +
        geom_text(aes(x=av_coal_ideo,frame=paste0(date_comp," - ",term),
                      y=-12,
            label=paste("Coalition ideology =",
                  round(av_coal_ideo,
                  digits = 2))),
                  size=3.5, angle=0, vjust=-0.4, hjust=0,color="darkseagreen4") +
        ggtitle(paste0("Brazilian Chamber of Deputies\n")) +
#error bar for ideological range
        geom_errorbarh(aes(y=(max(seats,na.rm=T)+2),
                           frame=paste0(date_comp," - ",term)),
              size=1.5,height=3) +
        geom_text(aes(x=max_ideo,frame=paste0(date_comp," - ",term),
              y=(max(seats,na.rm=T)+2),
              label=paste("Distance =",
                    round(abs(av_coal_ideo-av_cong_ideo),
                          digits = 1))),
                          size=4, vjust=0.5, hjust=-0.1) +
#theme
        theme_coalitions


setwd("D:/brazil_coalitions")
animation::ani.options()
animation::ani.options(ani.height=600,
                        ani.width=800,
                        description="",
                        title="",
                        verbose=F)

gganimate::gganimate(coalition_anim_full,interval=0.75,"index.html")
