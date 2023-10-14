library(tinyarray)
risk_plot = function(meta,genes){
  #meta = meta[!(is.na(meta$time)|is.na(meta$event)),]
  riskscore = meta$fp
  cut = median(riskscore)
  fp_dat=data.frame(patientid=1:length(riskscore),
                    riskscore=as.numeric(sort(riskscore)),
                    ri = meta$Risk[order(riskscore)])
  sur_dat=data.frame(patientid=1:length(riskscore),
                     time=meta[order(riskscore),'time'] ,
                     event=meta[order(riskscore),'event']) 
  sur_dat$event=ifelse(sur_dat$event==0,'alive','death')
  
  # exp
  
  exp_dat=scale(meta[order(riskscore),genes])
  
  ###第一个图----
  p1=ggplot(fp_dat,aes(x=patientid,y=riskscore,color = ri))+
    geom_point()+
    scale_color_manual(values = c("#2874C5","#f87669"))+
    geom_vline(xintercept = sum(riskscore<cut),lty = 2)+
    scale_x_continuous(expand=c(0,0))+
    theme_bw()
  #第二个图
  p2=ggplot(sur_dat,aes(x=patientid,y=time))+
    geom_point(aes(col=event))+
    geom_vline(xintercept = sum(riskscore<cut),lty = 2)+
    scale_color_manual(values = c("#2874C5","#f87669"))+
    scale_x_continuous(expand=c(0,0))+
    theme_bw()
  #第三个图
  p3 = ggheat(exp_dat,
              meta$Risk[order(riskscore)],
              show_rownames = F,
              color = c("#2874C5","white","#f87669"),
              legend_color = c("#2874C5","#f87669"))
  #拼图实现三图联动
  library(patchwork)
  p1/p2/p3 + plot_layout(design = "A
                         B
                         C
                         C
                         C")
}