# code for trump estimated p and ci bounds
library(png)
library(grid)
cols = rev(c("grey50", "#de0100", "#0015bc"))

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color

  ## Get RGB values for named color
  rgb.val <- col2rgb(color)

  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)

  ## Save the color
  invisible(t.col)
}

cols_trans = c(t_col("#0015bc", percent = 80), t_col("#de0100", percent = 80))
n1 = 30
n2 = 40
n = n1*n2
for_indep = 23
for_trump = 561
for_biden = 616
total_n = for_indep+for_trump+for_biden

set.seed(16456 + 690)
# 335  690 3389
my_sample = sample(1:n)

library("gifski")
png_path <- file.path(tempdir(), "frame%03d.png")
png(png_path, units="px", width=1600*1.4, height=1600*1.13, res=300)
par(ask = FALSE, mar = c(6,5,2,2))
nb = 61
alpha = 0.05
pval1 = pval2 = rep(NA, nb -1)
for (k in 1:nb){
  # k=8
  #Sys.sleep(.2)
  plot(NA, xlim = c(0, (nb-1)*10 + 80), ylim = c(0, 115), ann = TRUE, axes = FALSE, xlab = "Sample size",
       ylab = "Pvalue %")
  axis(1, at = c(0, 100, 200, 300, 400, 500, 600))
  axis(2, at = c(0, 20, 40, 60, 80, 100))
  counter = counter2 = counter3 = counter4 = 0

  # draw population
  for (i in 1:30){
    for (j in 1:40){
      counter = counter + 1
      if (counter <= for_biden){
        #points(j,i, col = cols[1], pch = 16, cex = 0.9)
      }else{
        if (counter <= (for_biden + for_trump)){
          #points(j,i, col = cols[2], pch = 16, cex = 0.9)
        }else{
          #points(j,i, col = cols[3], pch = 16, cex = 0.9)
        }
      }
    }
  }


  if (k > 1){

    # draw selected
    for (l in 1:(k-1)){
      for (h in 1:10){
        counter2 = counter2 + 1
        pos = my_sample[10*(l-1) + h]
        mod = pos %% n2
        ypos = floor(pos/n2) + 1
        if (ypos > n1){
          ypos = n1
        }
        #points(mod + 1, ypos, col = "white", pch = 16, cex = 0.8)
      }
    }


    # draw sample
    for (g in 1:counter2){
      if (my_sample[g] <= for_biden){
        counter3 = counter3 + 1
        #points((g-1) %% (n2/2) + 45, floor((g-1)/(n2/2)) + 1, col = cols[1], pch = 16, cex = 0.9)
      }else{
        if (my_sample[g] <= (for_biden + for_trump)){
          counter4 = counter4 + 1
          #points((g-1) %% (n2/2) + 45, floor((g-1)/(n2/2)) + 1, col = cols[2], pch = 16, cex = 0.9)
        }else{
          #points((g-1) %% (n2/2) + 45, floor((g-1)/(n2/2)) + 1, col = cols[3], pch = 16, cex = 0.9)
        }
      }
    }

  }

  if (k > 1){
    pval1[k-1] = binom.test(x = counter3, n = counter2, p = 0.5, alternative = c("greater"))$p.value
    pval2[k-1] = binom.test(x = counter3, n = counter2, p = 0.5, alternative = c("less"))$p.value

    points((1:(k-1))*10, 100*pval1[1:(k-1)], col = cols[1], type = "l")
    points((1:(k-1))*10, 100*pval2[1:(k-1)], col = cols[2], type = "l")

    for (hh in 1:(k-1)){
      if (pval1[hh] < 0.05){
        points(10*hh, 100*pval1[hh], col = cols[1], pch = 16)
      }

      if (pval2[hh] < 0.05){
        points(10*hh, 100*pval2[hh], col = cols[2], pch = 16)
      }
    }


    abline(h = 5, lty = 2)

    trump = readPNG('pics/trump.png')
    biden = readPNG('pics/biden.png')

    # x/ymin 0.18 0.22
    # x/ymax 0.93 0.8
    if (pval2[(k-1)] < 0.05){
      grid.raster(trump, x = 0.22 + (0.86-0.22)*(k-1)/(nb-1),
                  y = 0.26 + (0.83-0.26)*(pval2[k-1]), width=.08)

      text(350, 110, "Trump will win!!", cex = 2, col = cols[2])
      grid.raster(trump, x = 0.32,
                  y = 0.88, width=.08)

    }else{
      grid.raster(trump, x = 0.22 + (0.86-0.22)*(k-1)/(nb-1),
                  y = 0.25 + (0.83-0.25)*(pval2[k-1]), width=.07 - 0.03*(pval2[k-1]))
    }

    if (pval1[(k-1)] < 0.05){
      grid.raster(biden, x = 0.22 + (0.86-0.22)*(k-1)/(nb-1),
                  y = 0.26 + (0.83-0.26)*(pval1[k-1]), width=.08)

      text(350, 110, "Biden will win!!", cex = 2, col = cols[1])
      grid.raster(biden, x = 0.32,
                  y = 0.88, width=.08)

    }else{
      grid.raster(biden, x = 0.22 + (0.86-0.22)*(k-1)/(nb-1),
                  y = 0.25 + (0.83-0.25)*(pval1[k-1]), width=.07 - 0.03*(pval1[k-1]))
    }

  }


}

dev.off()


png_files <- sprintf(png_path, 1:nb)
gif_file <- tempfile(fileext = ".gif")
gifski(png_files, "GIF/trump_ts.gif", delay = 0.5, loop = TRUE, progress = TRUE)
unlink(png_files)
#utils::browseURL(gif_file)
