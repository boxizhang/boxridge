#' visualize airport delays
#' @name visulaze_airport_delays
#' @title visulaze_airport_delays
#' @description visulaze_airport_delays
#' @return a plot
#' @export
#'


library(nycflights13)
library(dplyr)
library(ggplot2)
library(ggmap)
library(rworldmap)
data("airports")
data("flights")

visualize_airport_delays <- function(){
  fli <- na.omit(flights)
  mean_delay <- aggregate(fli$arr_delay,list(fli$dest),mean)
  names(mean_delay)<-c("faa","mean_delay")
  airport <- airports[airports$faa%in%mean_delay$faa,]
  combine <- dplyr::inner_join(airport,mean_delay, by = "faa")
  combine$scaled<-"5"
  combine$scaled[combine$mean_delay<=12]<-"4"
  combine$scaled[combine$mean_delay<=8]<-"3"
  combine$scaled[combine$mean_delay<=4]<-"2"
  combine$scaled[combine$mean_delay<=0]<-"1"
  combine$sized <- "1"
  combine$sized[abs(combine$mean_delay)>4] <- "2"
  combine$sized[abs(combine$mean_delay)>8] <- "3"
  combine$sized[abs(combine$mean_delay)>12] <- "4"
  my_data <- dplyr::select(combine,lat,lon,mean_delay,scaled,sized)


  usa <- get_map(location = c(-161,20,-65,50),crop=FALSE,maptype = "toner-lite",zoom=3)

  map <- ggmap(usa) +
    geom_point(data=my_data, mapping = aes(x=lon, y=lat, color=scaled, size=sized)) +
    scale_colour_manual(values = c("1"="green","2"="red1","3"="red2","4"="red3","5"="red4"),
                        label = c("<0","<4","<8","<12",">12"), name="Mean delay") +
    scale_size_manual(values = c("1"=1,"2"=2,"3"=3,"4"=4),
                      labels = c("<4", "<8", "<12", ">12"),
                      name = "Absolute value of mean delay") +
    labs(x = "Longitude", y = "Latitude") +
    ggtitle("Mean delay of ﬂights from NYC to different airports") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
          plot.caption = element_text(size=12, hjust=0.5, margin=margin(t=15),colour="blue"))+labs(caption="LiU")
  map
}
