library(ggplot2)

plotDensity = function(data, ... , mean, mode, quantile_0.05, quantile_0.95,xlabTitle,  ylabTitle, title, MAPhyperprior, CIUpperhyperprior, CILowerhyperprior) {


  ggplot(data, ...)+
    geom_bar(stat="identity", alpha=0.5)+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_blank())+
    geom_vline(xintercept = mean, colour="red")+
    geom_vline(xintercept = mode, colour="green")+
    geom_vline(xintercept = quantile_0.05, colour="darkgreen")+
    geom_vline(xintercept = quantile_0.95, colour="darkgreen")+
    geom_vline(xintercept = MAPhyperprior, colour="blue")+
    geom_vline(xintercept = CIUpperhyperprior, colour="darkblue")+
    geom_vline(xintercept = CILowerhyperprior, colour="darkblue")+
    scale_x_continuous(limits=c(-0.2, 1.2))+
    ggtitle(title)+ 
    ylab(ylabTitle)+
    xlab(xlabTitle)+
    labs(fill='95% confidence interval')
}

