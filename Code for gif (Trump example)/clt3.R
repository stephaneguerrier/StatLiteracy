# trump draw sample and CI
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
  plot(NA, xlim = c(0, 90), ylim = c(0, 40), ann = FALSE, axes = FALSE)

  my_sample = sample(1:n) - 1

  res[iter] = mean(my_sample[1:200] <= for_biden)

  lines(c(43, 87) - 45, c(10,10) - 6.5)
  lines(c(45,45) - 45, c(9.7, 10.3)- 6.5)
  lines(c(50,50) - 45, c(9.7, 10.3)- 6.5)
  lines(c(55,55) - 45, c(9.7, 10.3)- 6.5)
  lines(c(60,60) - 45, c(9.7, 10.3)- 6.5)
  lines(c(65,65) - 45, c(9.7, 10.3)- 6.5)
  lines(c(70,70) - 45, c(9.7, 10.3)- 6.5)
  lines(c(75,75) - 45, c(9.7, 10.3)- 6.5)
  lines(c(80,80) - 45, c(9.7, 10.3)- 6.5)
  lines(c(85,85) - 45, c(9.7, 10.3)- 6.5)

  text(45 - 45, 9 - 6.5, "40")
  text(55 - 45, 9 - 6.5, "45")
  text(65 - 45, 9 - 6.5, "50")
  text(75 - 45, 9 - 6.5, "55")
  text(85 - 45, 9 - 6.5, "60")

  text(65 - 45, 0.5, "Estimated proportion of votes for Biden (%)")
  #for (myline in 1:18){
  #  lines(c(42.5,42.5) + 2.5*myline - 1.25, c(10, 30), col = "grey80")
  #}

  xx = seq(from = 40, 60, length.out = 1000)/100
  yy = dnorm(x = xx, mean = 0.513, sd = sqrt(0.513*(1-0.513)/200))/dnorm(x = 0.513, mean = 0.513, sd = sqrt(0.513*(1-0.513)/200))
  lines((xx/0.2 - 2)*40 + 45 - 45, 25*yy + 3.5, lwd = 2)
  tab = table(round(100*na.omit(res)/1.25)*1.25)

  for (i_res in 1:length(tab)){
    place = (as.numeric(names(tab[i_res])) - 40)/1.25
    nbb = as.numeric(tab[i_res])
    counter = 0
    for (inter_i in 1:nbb){
      if (place > 8){
        points(1.25 + 45 + 2.5*place - 2.5/2 - 45, 10 - 6.5 + inter_i - 0.5, cex = 1.4, pch = 16, col = cols[1])
      }else{
        if (place == 8){
          points(1.25 +45 + 2.5*place - 2.5/2 - 45, 10 - 6.5 + inter_i - 0.5, cex = 1.4, pch = 16, col = "purple2")
        }else{
          points(1.25 +45 + 2.5*place - 2.5/2 - 45, 10 - 6.5 + inter_i - 0.5, cex = 1.4, pch = 16, col = cols[2])
        }
      }
    }
  }


  lines(c(43, 87) + 5, c(10,10) - 6.5)
  lines(c(45,45) + 5, c(9.7, 10.3)- 6.5)
  lines(c(50,50) + 5, c(9.7, 10.3)- 6.5)
  lines(c(55,55) + 5, c(9.7, 10.3)- 6.5)
  lines(c(60,60) + 5, c(9.7, 10.3)- 6.5)
  lines(c(65,65) + 5, c(9.7, 10.3)- 6.5)
  lines(c(70,70) + 5, c(9.7, 10.3)- 6.5)
  lines(c(75,75) + 5, c(9.7, 10.3)- 6.5)
  lines(c(80,80) + 5, c(9.7, 10.3)- 6.5)
  lines(c(85,85) + 5, c(9.7, 10.3)- 6.5)

  text(45 + 5, 9 - 6.5, "30")
  text(55 + 5, 9 - 6.5, "40")
  text(65 + 5, 9 - 6.5, "50")
  text(75 + 5, 9 - 6.5, "60")
  text(85 + 5, 9 - 6.5, "70")

  text(65 + 5, 0.5, "Estimated proportion of votes for Biden (%)")
  lines(rep((0.513333 * 100 - 30) + 50, 2),  c(3.5, 35))
  for (kkk in 1:iter){
    inter = res[kkk]
    ci = inter + c(-1,1)*1.96*sqrt(inter*(1-inter)/200)
    if (ci[2] < 0.5133 || ci[1] > 0.5133){
      lines((ci * 100 - 30) + 50, c(kkk, kkk)/5 + 4, col = 2)
    }else{
      lines((ci * 100 - 30) + 50, c(kkk, kkk)/5 + 4, col = 1)
    }
  }
}


dev.off()


png_files <- sprintf(png_path, 1:nb)
gif_file <- tempfile(fileext = ".gif")
gifski(png_files, "GIF/sample_clt3.gif", delay = 0.1, loop = TRUE, progress = TRUE)
unlink(png_files)
utils::browseURL(gif_file)
