gg_color_hue <- function(n, alpha = 1) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}

cols = gg_color_hue(2, alpha = 1)
cols = gg_color_hue(2, alpha = 1)

n1 = 30
n2 = 40
n = n1*n2
for_a = 52/100

set.seed(16)
my_sample = sample(1:n)

library("gifski")
png_path <- file.path(tempdir(), "frame%03d.png")
png(png_path, units="px", width=1600*1.4, height=1600, res=300)
par(ask = FALSE, mar = c(1.5,0.5,0.5,0.5))
nb = 61

for (k in 1:nb){
  plot(NA, xlim = c(0, 85), ylim = c(0, 40), ann = FALSE, axes = FALSE)
  counter = counter2 = counter3 = 0
  text(15 + 5, 40, "Population", cex = 1.25)
  lines(c(27,37), c(34,34))
  ymax1 = for_a*6 + 34
  ymax2 = (1 - for_a)*6 + 34
  
  rect(28, 34, 31, ymax1, col = cols[1])
  rect(33, 34, 36, ymax2, col = cols[2])
  
  text(29.5, 32.6, "N")
  text(34.5, 32.6, "Y")
  rect(29.5 - 1, 32.6 - 1, 29 + 1.5, 32 + 1.5, border = cols[1], lwd = 2)
  rect(4, 35.5, 6, 37, col = cols[1])
  rect(4, 33, 6, 34.5, col = cols[2])
  
  text(15, 35.5/2 + 37/2,       paste("Against:  ",100*for_a, "%", sep = ""))
  text(15, 35.5/2 + 37/2 - 2.5, paste("For:       ",100*(1-for_a), "%", sep = ""))
  
  for (i in 1:30){
    for (j in 1:40){
      counter = counter + 1
      if (counter <= n*for_a){
        points(j,i, col = cols[1], pch = 16, cex = 0.9)
      }else{
        points(j,i, col = cols[2], pch = 16, cex = 0.9)
      }
    }
  }
  
  
  if (k > 1){
    for (l in 1:(k-1)){
      for (h in 1:10){
        counter2 = counter2 + 1
        pos = my_sample[10*(l-1) + h]
        mod = pos %% n2
        ypos = floor(pos/n2) + 1
        if (ypos > n1){
          ypos = n1
        }
        points(mod + 1, ypos, col = 1, pch = 16, cex = 0.9)
      }
    }
    
    for (g in 1:counter2){
      if (my_sample[g] <= n*for_a){
        counter3 = counter3 + 1
        points((g-1) %% (n2/2) + 55, floor((g-1)/(n2/2)) + 1, col = cols[1], pch = 16, cex = 0.9)
      }else{
        points((g-1) %% (n2/2) + 55, floor((g-1)/(n2/2)) + 1, col = cols[2], pch = 16, cex = 0.9)
      }
    }
    
    delta = 43
    text(15 + delta + 5, 40, paste("Sample (", round(counter2/n*100, 1), "% of pop.)", sep = ""), cex = 1.25)
    lines(c(27,37) + delta, c(34,34))
    ymax1 = (counter3/counter2)*6 + 34
    ymax2 = (1-counter3/counter2)*6 + 34
    
    rect(28 + delta, 34, 31 + delta, ymax1, col = cols[1])
    rect(33 + delta, 34, 36 + delta, ymax2, col = cols[2])
    
    text(29.5 + delta, 32.6, "N")
    text(34.5 + delta, 32.6, "Y")
    
    rect(4 + delta -1, 35.5, 6 + delta - 1, 37, col = cols[1])
    rect(4 + delta -1, 33, 6 + delta - 1, 34.5, col = cols[2])
    
    text(15 + delta -1, 35.5/2 + 37/2, paste("Against:  ",round(100*(counter3/counter2),1), "%", sep = ""))
    text(15 + delta -1 , 35.5/2 + 37/2 - 2.5, paste("For:       ",round(100*(1-counter3/counter2),1), "%", sep = ""))
    
    if (counter3/counter2 > 0.5){
      rect(29.5 + delta - 1, 32.6 - 1, 29 + delta + 1.5, 32 + 1.5, border = cols[1], lwd = 2)
    }else{
      if (counter3/counter2 < 0.5){
        rect(34.5 + delta - 1, 32.6 - 1, 34 + delta + 1.5, 32 + 1.5, border = cols[2], lwd = 2)
      }
    }
    
  }
}

dev.off()


png_files <- sprintf(png_path, 1:nb)
gif_file <- tempfile(fileext = ".gif")
gifski(png_files, "GIF/sample.gif", delay = 0.5, loop = TRUE, progress = TRUE)
unlink(png_files)
utils::browseURL(gif_file)

