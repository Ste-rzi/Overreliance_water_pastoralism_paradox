
# Script for plotting barplots of Aridity Index classes per Municipalities


library("raster")
library("sf")
library("purrr")
library("dplyr")
library("ggplot2")


# aridity map
aridity_namibe<-raster("G:/.shortcut-targets-by-id/1S-7uIu2kZB2g5x7aF4cFAlZdmI-AOiTr/Water_pastoralists_PARADOX/3_Data/Aridity/Aridy_class_Namibe.tif")

# municipalities
muni<-st_read("G:/.shortcut-targets-by-id/1S-7uIu2kZB2g5x7aF4cFAlZdmI-AOiTr/Water_pastoralists_PARADOX/3_Data/Admin_borders/MUNICIPIOS_NAMIBE.shp")

# drop z dimension
muniz<-st_zm(muni)

# Municipalities names in a separate vector
nomi<-muniz$Nome_Munic

# extract aridity values per each municipality
aridity_muni<-raster::extract(aridity_namibe, muniz, sp=T)

# create a dataframe with names only for value and municipality
titles<-data.frame(Var1=rep("Value", 5), Var2=rep("Municipality", 5))

# long dataframe in list with aridity values and municipality
test<- aridity_muni %>% 
  purrr::map(as.data.frame) %>% 
  purrr::map2(nomi, cbind) %>%
  purrr::map(as_tibble) %>%
  purrr::map2(asplit(titles, 1), set_names)

# all element of the list into one single dataframe
test2<-do.call(rbind, test) %>% 
  filter(Municipality!="Virei") %>% 
  arrange(Municipality)

# iteratively counting the number of aridity classes per each municipality and the percentage value
for(i in 1:length(unique(test2$Municipality))){

  test3<-test2 %>% 
    filter(Municipality==unique(test2$Municipality)[i]) %>% 
    mutate(Municipality=ifelse(Municipality=="Mocamedes", "Namibe", Municipality)) %>% 
    mutate(Variable="Aridity intensity") %>% 
    mutate(Value=as.factor(Value)) %>% 
    group_by(Value) %>% 
    mutate(Count=n()) %>%
    ungroup() %>% 
    mutate(Count_tot=nrow(.)) 
  
  test4<-test3 %>% 
    group_by(Value) %>% 
    summarize(Value_perc=(Count/Count_tot)*100) %>% 
    distinct(Value, Value_perc) %>% 
    mutate(Colors=case_when(
      Value == 6 ~ "#97923b",
      Value == 5 ~ "#c6c04e",
      Value == 4 ~ "#ffffb2",
      Value == 3 ~ "#fec46c",
      Value == 2 ~ "#e57217",
      Value == 1 ~ "#c4510b",
      Value == 0 ~ "NA")) %>% 
    arrange(desc(Value)) 

  t<-nrow(test4)
  
  # creating a dataset with id values as the number of rows 
  hj<-data.frame(test=c(1, 0, 1, 0, 1, 0, 1, 0, 1)) %>% 
    mutate(id=row.names(.)) %>% 
    filter(id<=nrow(test4))

  # binding the id vector to the existing dataframe
  test5<-test4 %>% 
    cbind(hj)

  # plotting the stack  
  stack_aridity<-ggplot(data=test5, aes(x="Aridity intensity", y=Value_perc, fill=id))+
    geom_col(position = "stack")+
    scale_fill_manual(values = c(test5$Colors))+
    scale_y_continuous(expand = c(0.04,0.04), labels = NULL, name = NULL,
                       sec.axis = sec_axis(~ (.)))+
    theme_classic()+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 18),
          axis.title = element_blank(),
          strip.text = element_blank(),
          axis.ticks = element_line(colour = "grey80", size = 0.1),
          legend.position="none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=10),
          strip.background = element_blank(),
          axis.line.y.left = element_blank()) 
  
  stack_aridity
  
  # ggsave(plot= stack_aridity,
  #        filename = paste0("G:/.shortcut-targets-by-id/1S-7uIu2kZB2g5x7aF4cFAlZdmI-AOiTr/Water_pastoralists_PARADOX/2_Figures/Fig.3/Stacked_", unique(test3$Municipality), ".png"),
  #        device = 'png',
  #        width = 3, height = 8)
  
}
