---

## My Curriculum Vitae

Update: 2022/01/19



- Calculate wind vectors for each time step
 # require package 'dplyr' and 'lubridate'
 # rm(list = ls(all = T))
 # Readin data and note: format of date is 'YYYY/MM/DD hh:mm:ss'
 # Note, header names should be 'date','wd','ws'
 UVconv <- function(mydata) {
    if ("wd" %in% names(mydata)) {
       if (is.numeric(mydata$wd) && "ws" %in% names(mydata)) {
          mydata <- transform(mydata, Uu = ws * cos(3 * pi/2 - wd * pi/180), Vv = ws * sin(3 * pi/2 - wd * pi/180)) # calculate U and V components of wind vector
       }
       if (is.numeric(mydata$wd) && !"ws" %in% names(mydata)) {
          mydata <- transform(mydata, Uu = cos(3 * pi/2 - wd * pi/180), Vv = sin(3 * pi/2 - wd * pi/180)) # calculate U and V components of wind vector
       }
       avmet <- mydata %>%
          mutate(date = ymd_hms(date), # ymd_hms is a function in package lubridate, and the order of alphabeta depends on time format. 
                 date  = ceiling_date(date, '10 min')) %>% # ceiling time at a 10-min time interval, could be 20 min, 30 min, 40 min ...
          group_by(date) %>%
          summarise_each(funs(mean(., na.rm = TRUE))) %>% # calculate the average value at a time interval
          na.omit() # ignore missing values
       avmet  = within(avmet , wd <- (270 - atan2(Vv, Uu) *  180/pi) %% 360) # recalculate wind direction
       avmet  = within(avmet , ws <- sqrt(Uu * Uu + Vv * Vv)) # recalculate wind speed
       avmet = subset(avmet, select = c(-Uu, -Vv, -date)) # remove unnecessary columns
       avmet # Show results
    }
 }
 uvmet <-UVconv(airDat)
 uvmet

 - ScatterPlot 4 concentration distribution within urban street canyons
 # require package 'openair'
 # rm(list = ls(all = T))
      tiff(paste("NAME",sep = ""), units="in", width=6, height=6, res=1500) # set tiff file name and resolution
      FILENAME <- scatterPlot(DATASET, x = "VAR1", y = "VAR2", z = "VAR3", method = "level", limits = c(NUM1, NUM2), # VAR1_canyonwidth, VAR2_canyonheight, VAR3_concentration
                           main = expression(AIRPOLLUTION),cex = 1.0, fontsize = 25, # set main title, font size, and font color
                           scales = list(at = c(.25,.5,.75, 1.0),y = list(tck=c(0.5,0)),x = list(at = c(-.5,-.25,0,.25,.5),tck=c(0.5,0))), # set x and y axis scale
                           cols = "default", auto.text = T, # set color, and auto.text
                           xlab = expression(italic(x)*'/'*italic(h)[0]), ylab = expression(italic(z)*'/'*italic(l)[0]), # set x and y axis label
                           panel = function(...){
                             panel.levelplot(...)
                             panel.abline(h = 0.25, col = "black", lwd = 2, lty = 2) # set horizontal line at 0.25
                             panel.abline(h = 0.5, col = "black", lwd = 2, lty = 2)
                             panel.abline(h = 0.75, col = "black", lwd = 2, lty = 2)
                             panel.abline(v = -0.25, col = "black", lwd = 2, lty = 2)
                             panel.abline(v = -0, col = "black", lwd = 2, lty = 2)
                             panel.abline(v = 0.25, col = "black", lwd = 2, lty = 2)
                           })
      dev.off()
 uvmet <-UVconv(airDat)
 uvmet


---
[◄◄ BACK](https://yuqingdai.xyz/#/README?id=publications)