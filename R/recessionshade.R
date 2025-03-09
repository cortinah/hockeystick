library(fredr)
#fredr_set_key("insert_your_api_key")

fred_data<-fredr(series_id = "UNRATE",observation_start = as.Date("1920-01-01"))

head(fred_data)

#Generate a nice default layout for the plot

theme_am <- function (base_size = 12, base_family = "")
  {
    library(ggthemes)
    library(scales)
    library(extrafont)
    theme_hc(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.text.x = element_text(color = "grey20", size = 11,family="Calibri Light"),
        axis.text.y = element_text(color = "grey20", size = 11,family="Calibri Light"),
        axis.title.x = element_text(color = "grey20", size = 12,family="Calibri Light"),
        axis.title.y = element_text(color = "grey20", size = 12,family="Calibri Light"),
        plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri Light"),
        legend.text = element_text(color = "grey20", size = 12,family="Calibri Light")
      )
  }

#Define recession shading function (don’t forget to add your API key and activate the respective line)

  add_rec_shade <- function(st_date, ed_date, shade_color="darkgray")  {
    library(fredr)
    library(ggplot2)
    #fredr_set_key("insert_your_api_key")

    #st_date<-as.Date("2001-06-01")
    #ed_date<-as.Date(Sys.Date())

    recession <- fredr(series_id = "USRECD",observation_start = as.Date(st_date),observation_end = as.Date(ed_date))

    recession$diff <- recession$value-lag(recession$value)
    recession<-recession[!is.na(recession$diff),]
    recession.start<-recession[recession$diff==1,]$date
    recession.end<-recession[recession$diff==(-1),]$date

    if(length(recession.start)>length(recession.end))
    {recession.end<-c(recession.end,Sys.Date())}
    if(length(recession.end)>length(recession.start))
    {recession.start<-c(min(recession$date),recession.start)}

    recs<-as.data.frame(cbind(recession.start,recession.end))
    recs$recession.start<-as.Date(as.numeric(recs$recession.start),origin=as.Date("1970-01-01"))
    recs$recession.end<-as.Date(recs$recession.end,origin=as.Date("1970-01-01"))
    if(nrow(recs)>0)
    {
      rec_shade <- geom_rect(data=recs, inherit.aes=F,
                           aes(xmin=recession.start, xmax=recession.end, ymin=-Inf, ymax=+Inf),
                           fill=shade_color, alpha=0.5)
      return(rec_shade)
    }
  }


library(extrafont)

## Registering fonts with R

library(ggplot2)

my_plot <-
  ggplot(fred_data, aes(x=date)) +
  #Add recession shading here
  #******************************************************************
  add_rec_shade(min(fred_data$date),max(fred_data$date)) +
  #******************************************************************
  geom_line(aes(y=value/100),size = 0.8,color="#dd0400") +
  scale_y_continuous(name="Unemployment Rate in %",labels = scales::percent_format(accuracy = 1)) +
  theme_am() +
  scale_x_date(labels = date_format("%m-%Y"))+
  theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Arial"))+
  labs(title="US Unemployment Rate and NBER Recessions",x ="")

my_plot
