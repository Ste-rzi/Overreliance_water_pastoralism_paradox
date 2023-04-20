
library("corrplot")

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
}

# Correlation analysis ####

# Modifications to the corrplot function to have significance stars above the number
# as explained in https://stackoverflow.com/questions/63227830/r-corrplot-plot-correlation-coefficients-along-with-significance-stars

# Change the place_points function within the corrplot function. To do so, run:
  
# trace(corrplot, edit=TRUE)
# Then replace on line 443

# place_points = function(sig.locs, point) {
#   text(pos.pNew[, 1][sig.locs], pos.pNew[, 2][sig.locs], 
#        labels = point, col = pch.col, cex = pch.cex, 
#        lwd = 2)
# with:
    
# adjust text(X,Y ...) according to your needs, here +0.25 is added to the Y-position    
# place_points = function(sig.locs, point) {
#   text(pos.pNew[, 1][sig.locs], (pos.pNew[, 2][sig.locs])+0.25, 
#        labels = point, col = pch.col, cex = pch.cex, 
#        lwd = 2)
#      and then hit the "Save" button.
      
      
corrp1<-list()

for (i in 1:length(all1)){
  
  tt.1<-all1[[i]]
  
  nana<-unique(tt.1$Municipality)
  
  tt1.1<-tt.1 %>% 
    tidyr::spread(Variable, Value_nrm) %>% 
    dplyr::select("Drought", "SWI", "Pastureland", "Cropland") %>% 
    rename("SPEI"="Drought",
           "PAST"="Pastureland",
           "CROP"="Cropland") %>% 
    as.matrix()
  
  cor1<- tt1.1 %>% 
    cor(method="kendall")
  
  tt1.2<-tt.1 %>% 
    tidyr::spread(Variable, Value_nrm) %>% 
    dplyr::select("Drought", "SWI", "Pastureland", "Cropland") %>% 
    rename("SPEI"="Drought",
           "PAST"="Pastureland",
           "CROP"="Cropland")
  
  # Testing normality ####
  shapiro.test(tt1.2$SPEI) # W = 0.78485, p-value = 5.839e-05
  shapiro.test(tt1.2$SWI) # W = 0.81247, p-value = 0.0001785
  shapiro.test(tt1.2$PAST) # W = 0.83245, p-value = 0.0004219
  shapiro.test(tt1.2$CROP) # W = 0.42257, p-value = 1.985e-09

  test1<-cor.test(tt1.2$SPEI, tt1.2$SWI, method="kendall")
  p1<-test1$p.value
  
  test2<-cor.test(tt1.2$SPEI, tt1.2$PAST, method="kendall")
  p2<-test2$p.value

  test3<-cor.test(tt1.2$SPEI, tt1.2$CROP, method="kendall")
  p3<-test3$p.value
  
  test4<-cor.test(tt1.2$PAST, tt1.2$CROP, method="kendall")
  p4<-test4$p.value
  
  test5<-cor.test(tt1.2$SWI, tt1.2$CROP, method="kendall")
  p5<-test5$p.value
  
  test6<-cor.test(tt1.2$SWI, tt1.2$PAST, method="kendall")
  p6<-test6$p.value
  
  
  p_mat2<- c(1, p1, p2, p3, 
             p1, 1, p6, p5,
             p2, p6, 1, p4,
             p3, p5, p4, 1) %>% 
    matrix(., ncol=4)
  
  rownames(p_mat2) <- colnames(tt1.1)
  colnames(p_mat2) <- colnames(tt1.1)
  
  
  png(filename = paste0("G:/.shortcut-targets-by-id/1S-7uIu2kZB2g5x7aF4cFAlZdmI-AOiTr/Water_pastoralists_PARADOX/2_Figures/Correlations/Corrplot_legend", nana, ".png"),
      width = 1200, height = 800)
  
  coco=c("#9970ab","#c2a5cf","#e7d4e8", "#f7f7f7", "#d9f0d3", "#a6dba0", "#5aae61")
  
  corrplot(cor1,
           method="square",
           addCoef.col = "black",
           tl.pos="d",
           cl.pos = 'r',
           number.cex=3.5, 
           p.mat = p_mat2,
           insig = "label_sig",
           sig.level = c(.001, .01, .05),
           pch.cex = 3.5,
           pch.col = "black",
           tl.col="black",
           tl.cex=3.5,
           type="lower",
           col= coco,
           mar = c(0, 0, 0, 0),
           number.font=6)
  
  
  dev.off()

}
