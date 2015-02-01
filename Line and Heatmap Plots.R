energy.data <- RichlandWaterEData_Cleaned <- read.csv("~/Downloads/RichlandWaterEData_Cleaned.csv")

require(ggplot2)
require(lubridate)
require(dplyr)

clean.data <- energy.data %>%
  mutate(time = mdy_hm(TimeStamp)) %>%
  mutate(day= day(time),
         hour = hour(time),
         month = month(time),
         year = year(time))%>%
  group_by(year, month, day, hour)%>%
  summarise(kW = max(Total, na.rm = TRUE))%>%
  ungroup()

clean.data %>%
  monthplot(kW)

singlemonth <-clean.data %>%
  group_by(year, month, day)%>%
  summarise()%>%
  ungroup()%>%
  mutate(n=row_number())%>%
  filter(n<31)%>%
  inner_join(clean.data)%>%
  sumplot()



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Monthplot function
#
# Plots a hourly peak demand two ways (1) line chart of daily trends and (2) heatmap.  
# Facetted by month and year. 


monthplot <- function(month, kW, facet = year + month ~.){
  #max.value <- month %>%
  #  arrange( desc(Actual.Energy.Use))
  #max.value <- max.value[1,]
  #yvalue <- max.value$Actual.Energy.Use
  
  line<-ggplot(month, aes(x=hour, y= kW, 
                group = day))+
    facet_grid(facet)+
    ylim(0,NA)+
    geom_line(alpha = .2)+
    labs( title = "Hourly Peak Demand (KW)",
          y = "Demand (KW)",
          x = "Time of Day")+
    theme(panel.background = element_rect(fill = NA),
          plot.title = element_text(family = "Roboto", face="bold", colour = "#424242"),
          axis.title = element_text(family="Roboto-Condensed"),
          axis.line = element_line(colour = "#BDBDBD"),
          axis.line.y = element_blank(),
          axis.title.x = element_text(family = "Roboto", colour = "#757575", size = "12"),
          axis.title.y = element_text(family = "Roboto", colour = "#757575", size = "12"))+
    scale_x_continuous(breaks=c(0,6,12,18,23), labels=c("Midnight", "6:00 AM", "Noon", "6:00 PM", "11:00 PM"),
                       expand = c(0,0))+
    scale_y_continuous(limits = c(0,NA),
                       expand = c(0,0))
    #geom_hline(aes(yintercept = yvalue, 
    #               color = "#de2d26"))
  
  heat <- ggplot(month, aes(x=day, y= hour))+
    facet_grid(year + month ~.)+
    geom_tile(aes(fill = kW))+
    scale_fill_gradient2(
      low = "#ffffd9",
      mid = "#41b6c4",
      high = "#081d58")+
    #ylim(0,23)+
    xlim(1,31)+
    labs( title = "Hourly Peak Demand (KW)",
          x = "Day of Month",
          y = "Time")+
    scale_y_continuous(breaks=c(0,6,12,18,23), labels=c("Midnight", "6:00 AM", "Noon", "6:00 PM", "11:00 PM"))+
    theme(panel.background = element_rect(fill = NA),
          plot.title = element_text(family = "Roboto", face="bold", colour = "#424242"),
          axis.title = element_text(family="Roboto-Condensed"),
          axis.line = element_blank(),
          axis.title.x = element_text(family = "Roboto", colour = "#757575", size = "12"),
          axis.title.y = element_text(family = "Roboto", colour = "#757575", size = "12"))
  multiplot (line, heat, cols=2)
}

# sumplot function
#
# Plots a hourly peak demand two ways (1) line chart of daily trends and (2) heatmap.  
# NO Facetting. 

sumplot <- function(month, kW){
  max.value <- month %>%
  arrange( desc(kW))
  max.value <- max.value[1,]
  yvalue <- max.value$kW
  
  line<-ggplot(month, aes(x=hour, y= kW, 
                          group = n))+
    geom_line(alpha = .2)+
    labs( title = "Hourly Peak Demand (KW)",
          y = "Demand (KW)",
          x = "Time of Day")+
    theme(panel.background = element_rect(fill = NA),
          plot.title = element_text(family = "Roboto", face="bold", colour = "#424242"),
          axis.title = element_text(family="Roboto-Condensed"),
          axis.line = element_line(colour = "#BDBDBD"),
          axis.line.y = element_blank(),
          axis.title.x = element_text(family = "Roboto", colour = "#757575", size = "12"),
          axis.title.y = element_text(family = "Roboto", colour = "#757575", size = "12"))+
    scale_x_continuous(breaks=c(0,6,12,18,23), labels=c("Midnight", "6:00 AM", "Noon", "6:00 PM", "11:00 PM"),
                       expand = c(0,0))+
    scale_y_continuous(limits = c(0,NA),
                       expand = c(0,0))
    #geom_hline(aes(yintercept = yvalue, 
                 #color = "#de2d26"))
  
  heat <- ggplot(month, aes(x=n, y= hour))+
    geom_tile(aes(fill = kW))+
    scale_fill_gradient2(
      low = "#ffffd9",
      mid = "#41b6c4",
      high = "#081d58")+
    #ylim(0,23)+
    xlim(1,31)+
    labs( title = "Hourly Peak Demand (KW)",
          x = "Day of Month",
          y = "Time")+
    scale_y_continuous(breaks=c(0,6,12,18,23), 
                       labels=c("Midnight", "6:00 AM", "Noon", "6:00 PM", "11:00 PM"),
                       expand = c(0,0))+
    theme(panel.background = element_rect(fill = NA),
          plot.title = element_text(family = "Roboto", face="bold", colour = "#424242"),
          axis.title = element_text(family="Roboto-Condensed"),
          axis.line = element_blank(),
          axis.title.x = element_text(family = "Roboto", colour = "#757575", size = "12"),
          axis.title.y = element_text(family = "Roboto", colour = "#757575", size = "12"))
  
  multiplot (line, heat, cols=1)
}
