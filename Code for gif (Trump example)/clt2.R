# trump  draw sample with normal curve

cols = rev(c("grey50", "#de0100", "#0015bc"))

n1 = 30
n2 = 40
n = n1*n2
for_indep = 23
for_trump = 561
for_biden = 616





# library("gifski")
# png_path <- file.path(tempdir(), "frame%03d.png")
# png(png_path, units="px", width=1600*1.4, height=1600, res=300)
par(ask = FALSE, mar = c(3,0.75,0.75,0.75))
nb = 150

res = rep(NA, nb)
set.seed(17)
for (iter in 1:nb){
  Sys.sleep(.3)
  plot(NA, xlim = c(0, 85), ylim = c(0, 40), ann = FALSE, axes = FALSE)
  counter = counter2 = counter3 = counter4 = 0
  text(15 + 5, 40, "Population", cex = 1.25)
  lines(c(27,42)-3, c(34,34))
  ymax_biden = for_biden/n*6 + 34
  ymax_trump = for_trump/n*6 + 34
  ymax_indep = for_indep/n*6 + 34

  rect(28-3, 34, 31-3, ymax_biden, col = cols[1])
  rect(33-3, 34, 36-3, ymax_trump, col = cols[2])
  rect(38-3, 34, 41-3, ymax_indep, col = cols[3])

  text(29.5 -3, 32.6, "B")
  text(34.5 -3, 32.6, "T")
  text(39.5 -3, 32.6, "I")
  rect(29.5 - 1 - 3, 32.6 - 1, 29 + 1.5 - 3, 32 + 1.5, border = cols[1], lwd = 2)
  rect(4-2, 36.5, 6-2, 38, col = cols[1])
  rect(4-2, 34, 6-2, 35.5, col = cols[2])
  rect(4-2, 31.5, 6-2, 33, col = cols[3])

  text(15 - 11, 35.5/2 + 37/2 + 1,         paste("Biden: ",round(100*for_biden/n,1), "%", sep = ""), pos = 4, cex = 0.9)
  text(15 - 11, 35.5/2 + 37/2 - 2.5 + 1,   paste("Trump: ",round(100*for_trump/n,1), "%", sep = ""), pos = 4, cex = 0.9)
  text(15 - 11, 35.5/2 + 37/2 - 2.5 - 1.5, paste("Indep: ",round(100*for_indep/n,1), "%", sep = ""), pos = 4, cex = 0.9)

  for (i in 1:30){
    for (j in 1:40){
      counter = counter + 1
      if (counter <= for_biden){
        points(j,i, col = cols[1], pch = 16, cex = 0.9)
      }else{
        if (counter <= (for_biden + for_trump)){
          points(j,i, col = cols[2], pch = 16, cex = 0.9)
        }else{
          points(j,i, col = cols[3], pch = 16, cex = 0.9)
        }
      }
    }
  }

  my_sample = sample(1:n) - 1
  for (h in 1:200){
    counter2 = counter2 + 1
    pos = my_sample[h]
    mod = pos %% n2
    ypos = floor(pos/n2) + 1
    if (ypos > n1){
      ypos = n1
    }
    points(mod + 1, ypos, col = "white", pch = 16, cex = 0.8)
  }

  res[iter] = mean(my_sample[1:200] <= for_biden)

  lines(c(43, 87), c(10,10) - 6.5)
  lines(c(45,45), c(9.7, 10.3)- 6.5)
  lines(c(50,50), c(9.7, 10.3)- 6.5)
  lines(c(55,55), c(9.7, 10.3)- 6.5)
  lines(c(60,60), c(9.7, 10.3)- 6.5)
  lines(c(65,65), c(9.7, 10.3)- 6.5)
  lines(c(70,70), c(9.7, 10.3)- 6.5)
  lines(c(75,75), c(9.7, 10.3)- 6.5)
  lines(c(80,80), c(9.7, 10.3)- 6.5)
  lines(c(85,85), c(9.7, 10.3)- 6.5)

  text(45, 9 - 6.5, "40")
  text(55, 9 - 6.5, "45")
  text(65, 9 - 6.5, "50")
  text(75, 9 - 6.5, "55")
  text(85, 9 - 6.5, "60")

  text(65, 0.5, "Estimated proportion of votes for Biden (%)")
  #for (myline in 1:18){
  #  lines(c(42.5,42.5) + 2.5*myline - 1.25, c(10, 30), col = "grey80")
  #}

  xx = seq(from = 40, 60, length.out = 1000)/100
  yy = dnorm(x = xx, mean = 0.513, sd = sqrt(0.513*(1-0.513)/200))/dnorm(x = 0.513, mean = 0.513, sd = sqrt(0.513*(1-0.513)/200))
  lines((xx/0.2 - 2)*40 + 45, 25*yy + 3.5, lwd = 2)
  tab = table(round(100*na.omit(res)/1.25)*1.25)

  for (i_res in 1:length(tab)){
    place = (as.numeric(names(tab[i_res])) - 40)/1.25
    nbb = as.numeric(tab[i_res])
    counter = 0
    for (inter_i in 1:nbb){
      if (place > 8){
        points(1.25 + 45 + 2.5*place - 2.5/2, 10 - 6.5 + inter_i - 0.5, cex = 1.4, pch = 16, col = cols[1])
      }else{
        if (place == 8){
          points(1.25 +45 + 2.5*place - 2.5/2, 10 - 6.5 + inter_i - 0.5, cex = 1.4, pch = 16, col = "purple2")
        }else{
          points(1.25 +45 + 2.5*place - 2.5/2, 10 - 6.5 + inter_i - 0.5, cex = 1.4, pch = 16, col = cols[2])
        }
      }
    }
  }
}


dev.off()


png_files <- sprintf(png_path, 1:nb)
gif_file <- tempfile(fileext = ".gif")
gifski(png_files, "GIF/sample_clt2.gif", delay = 0.1, loop = TRUE, progress = TRUE)
unlink(png_files)
utils::browseURL(gif_file)
