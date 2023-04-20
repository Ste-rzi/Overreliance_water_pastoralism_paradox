
# Packages activation ####
library("ggplot2")
library("dplyr")
library("lubridate")

# reshaping the SPEI dataframe to make it matching the other datasets
admin_spei_namibe2<-admin_spei_namibe %>%
  mutate(Year=year(Date),
         Month=month(Date),
         Day=day(Date)) %>% 
  mutate(Variable="SPEI",
         Legend="Legend") %>% 
  mutate(Municipality=ifelse(Municipality=="Tômbwa (Porto Alexandre)", "Tombwa", Municipality)) %>% 
  filter(Year>1990 & Year<2021)


admin_spei_namibe3<-admin_spei_namibe2 %>% 
  mutate(Years=year(Date)) %>% 
  arrange(Municipality, Date)

dt_test3<-dt_test2 %>% 
  filter(Municipality!="Virei")

dt2<-dt1 %>% 
  filter(Municipality!="Virei")

all1<-list()

for (i in 1:length(unique(dt_test3$Municipality))){
  
  a<-c(4000, 4000, 1000, 2000)
  
  dt3<-dt2 %>% 
    filter(Municipality==unique(Municipality)[i]) %>% 
    filter(Years>1991) %>% 
    mutate(Years=as.numeric(Years)) %>%
    mutate(Value= ifelse(Municipality==unique(dt2$Municipality)[i] & Variable=="Boreholes", 
                         Value*a[i], Value)) %>% 
    mutate(Value_max=max(Value, na.rm = T),
           Value_min=min(Value, na.rm = T),
           Value_nrm=((Value-Value_min)/(Value_max-Value_min))) %>%
    dplyr::select(Municipality, Years, Variable, Legend, Value_nrm)
  
  admin_spei_namibe_pp<-admin_spei_namibe3 %>% 
    filter(Municipality==unique(admin_spei_namibe3$Municipality)[i]) %>%
    mutate(Years=year(Date),
           Month=month(Date)) %>% 
    filter(Years<2020) %>%
    group_by(Years) %>% 
    summarize(Value_mo=mean(Value, na.rm=T)) %>%
    mutate(Variable="Drought Intensity",
           Municipality=unique(admin_spei_namibe3$Municipality)[i],
           Value=abs(Value_mo),
           Value_max=max(Value, na.rm = T),
           Value_min=min(Value, na.rm = T),
           Value_nrm=((Value-Value_min)/(Value_max-Value_min)),
           Legend="Legend") %>%
    dplyr::select(Municipality, Years, Variable, Legend, Value_nrm)
  
  dt_test4<-dt_test3 %>% 
    filter(Municipality==unique(Municipality)[i]) %>% 
    filter(Years>1991) %>% 
    mutate(Years=as.numeric(Years)) %>%
    group_by(Variable) %>% 
    mutate(Value_max=max(Value, na.rm = T),
           Value_min=min(Value, na.rm = T),
           Value_nrm=((Value-Value_min)/(Value_max-Value_min)))%>%
    dplyr::select(Municipality, Years, Variable, Legend, Value_nrm)
  
  tt<-rbind(dt3, dt_test4, admin_spei_namibe_pp) %>% 
    mutate(Variable=ifelse(Variable=="cropland", "Cropland", 
                           ifelse(Variable=="grassland", "Pastureland", 
                                  ifelse(Variable=="Boreholes", "SWI", 
                                         ifelse(Variable=="Drought Intensity", "Drought", Variable)))))
  all1[[i]]<-tt 
  
  cond1<-element_blank()
  cond2<-element_text(face = "bold", size = 30, margin=margin(15,0,0,0))
  
  # using an ifelse to specify the case with or without axis.text.x for a better visualization in the final figure
  ifelse(i!=4,
  p1_crp_grs<-ggplot()+
    geom_area(data = tt[tt$Variable=="Drought",], aes(x=Years, y=Value_nrm, group=Variable, fill=Variable), size=3, alpha=0.8)+
    geom_line(data = tt[tt$Variable=="Drought",], aes(x=Years, y=Value_nrm, group=Variable, color=Variable), size=1.5)+
    geom_area(data = tt[tt$Variable=="SWI",], aes(x=Years, y=Value_nrm, group=Variable, fill=Variable), size=3, alpha=0.6)+
    geom_line(data = tt[tt$Variable=="SWI",], aes(x=Years, y=Value_nrm, group=Variable, color=Variable), size=1.5)+
    geom_line(data = tt[tt$Variable=="Cropland",], aes(x=Years, y=Value_nrm, group=Variable, color=Variable), size=4, alpha=0.8)+
    geom_line(data = tt[tt$Variable=="Pastureland",], aes(x=Years, y=Value_nrm, group=Variable, color=Variable), size=4, alpha=0.8)+
    labs(x="", y="")+
    scale_fill_manual(values=c("#7F7F7F", "#517EB9"), guide = "none")+
    scale_color_manual("Variables   ",
                       values = c("black", "grey40","#b2182b", "#2166ac"))+
    scale_x_continuous(limits=c(1992, 2019),
                       breaks=seq(1992, 2019, 6), expand = c(0.01,0.01))+
    scale_y_continuous(limits=c(0, 1),
                       breaks=c(0, 0.5, 1), expand = c(0.01,0.01))+
    theme_classic()+
    theme(axis.text.x = cond1,
          axis.text.y = element_text(face = "bold", size = 30, margin=margin(0,10,0,0)),
          axis.title = element_blank(),
          strip.text = element_blank(),
          axis.ticks = element_line(colour = "black", size = 2),
          legend.title = element_text(size=16),
          legend.text = element_text(size=14),
          legend.position="none",
          legend.box.margin=margin(5,5,5,5),
          legend.box.background = element_rect(colour = "black", size=0.5),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=10),
          strip.background = element_blank())+
    guides(colour = guide_legend(order = 1)),
  
  p1_crp_grs<-ggplot()+
    geom_area(data = tt[tt$Variable=="Drought",], aes(x=Years, y=Value_nrm, group=Variable, fill=Variable), size=3, alpha=0.8)+
    geom_line(data = tt[tt$Variable=="Drought",], aes(x=Years, y=Value_nrm, group=Variable, color=Variable), size=1.5)+
    geom_area(data = tt[tt$Variable=="SWI",], aes(x=Years, y=Value_nrm, group=Variable, fill=Variable), size=3, alpha=0.6)+
    geom_line(data = tt[tt$Variable=="SWI",], aes(x=Years, y=Value_nrm, group=Variable, color=Variable), size=1.5)+
    geom_line(data = tt[tt$Variable=="Cropland",], aes(x=Years, y=Value_nrm, group=Variable, color=Variable), size=4, alpha=0.8)+
    geom_line(data = tt[tt$Variable=="Pastureland",], aes(x=Years, y=Value_nrm, group=Variable, color=Variable), size=4, alpha=0.8)+
    labs(x="", y="")+
    scale_fill_manual(values=c("#7F7F7F", "#517EB9"), guide = "none")+
    scale_color_manual("Variables   ",
                       values = c("black", "grey40","#b2182b", "#2166ac"))+
    scale_x_continuous(limits=c(1992, 2019),
                       breaks=seq(1992, 2019, 6), expand = c(0.01,0.01))+
    scale_y_continuous(limits=c(0, 1),
                       breaks=c(0, 0.5, 1), expand = c(0.01,0.01))+
    theme_classic()+
    theme(axis.text.x = cond2,
          axis.text.y = element_text(face = "bold", size = 30, margin=margin(0,10,0,0)),
          axis.title = element_blank(),
          strip.text = element_blank(),
          axis.ticks = element_line(colour = "black", size = 2),
          legend.title = element_text(size=16),
          legend.text = element_text(size=14),
          legend.position="none",
          legend.box.margin=margin(5,5,5,5),
          legend.box.background = element_rect(colour = "black", size=0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=10),
          strip.background = element_blank())+
    guides(colour = guide_legend(order = 1)))
  
  # saving the plot in a dedicated folder
  # ggsave(plot= p1_crp_grs,
  #        filename = paste0("G:/.shortcut-targets-by-id/1S-7uIu2kZB2g5x7aF4cFAlZdmI-AOiTr/Water_pastoralists_PARADOX/2_Figures/Fig.3/", unique(tt$Municipality),"_test3_", i, ".png"),
  #        device = 'png', bg = 'transparent',
  #        width = 12, height = 8)
  
}


