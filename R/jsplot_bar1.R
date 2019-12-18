#' jsplot_bar1

jsplot.bar1<-function(data,
                      prob=FALSE,
                      col="dark blue",
                      lab_x='XNAME',
                      lab_y='YNAME',
                      lab_main='MAIN',
                      xlab_size=12,
                      ylab_size=12,
                      main_size=16,
                      main_loc='left',
                      xtext_size=10,
                      table=TRUE,
                      prob_table=FALSE,
                      sort=TRUE){
  library(ggplot2)
  
  col<-col
  lab_x<-lab_x
  lab_y<-lab_y
  lab_main<-lab_main
  
  tb<-as.data.frame(table(data$team))
  tb_pr<-as.data.frame(prop.table(table(data$team)))
  
  if(main_loc=='left'){main_loc<-0}else if(main_loc=='mid'){main_loc<-0.5}else{main_loc<-1}
  
  My_Theme<-theme(
    axis.title.x = element_text(size = xlab_size),
    axis.text.x = element_text(size = xtext_size),
    axis.title.y = element_text(size = ylab_size),
    plot.title = element_text(size=main_size,hjust=main_loc)       )
  # 0 왼쪽 0.5 중간  1 오른쪽
  
  sort_bar   <-  ggplot(tb, aes(x=reorder(Var1, -Freq), y = Freq))+ geom_bar(stat="identity", fill=col)+
    xlab(lab_x) +  ylab(lab_y) + ggtitle(lab_main)
  nosort_bar <-  ggplot(tb, aes(x=(Var1), y = Freq))+ geom_bar(stat="identity", fill=col)+
    xlab(lab_x) +  ylab(lab_y) + ggtitle(lab_main)
  
  prob_sort_bar   <-  ggplot(tb_pr, aes(x=reorder(Var1, -Freq), y = Freq))+ geom_bar(stat="identity", fill=col)+
    xlab(lab_x) +  ylab(lab_y) + ggtitle(lab_main)
  prob_nosort_bar <-  ggplot(tb_pr, aes(x=(Var1), y = Freq))+ geom_bar(stat="identity", fill=col)+
    xlab(lab_x) +  ylab(lab_y) + ggtitle(lab_main)
  
  if(table==TRUE){print(tb)}
  if(prob_table==TRUE){print(tb_pr)}
  
  if(sort==TRUE & prob==TRUE){prob_sort_bar+My_Theme}
  else if(sort==FALSE & prob==TRUE){prob_nosort_bar+My_Theme}
  else if(sort==TRUE & prob==FALSE){sort_bar+My_Theme}
  else{nosort_bar+My_Theme}
}
