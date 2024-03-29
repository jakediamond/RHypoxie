

data = filter(df, site == "coise larajasse",
              year == 2020) %>%
  mutate(DOper = imputeTS::na_kalman(DO_per))


# Function to get spectrum
spec_fun <- function(data)
{
  ts <- ts(data$DOper) # Turn into R timeseries
  sp <- spectrum(ts, span = c(3, 3))
  spec <- sp$spec # power spectra
  wl <- (1 / sp$freq) / (24) # wavelength (days)
  results <- data.frame(spec, wl)
}

# Spectral data
spect <- wt %>% 
  group_by(site) %>% 
  do(spec_fun(.))

# Function to calculate slopes
piecewise_fun <- function(data, p_value) {
  library(segmented)
  library(strucchange)
  library(MASS)
  data$spec <- log(data$spec)
  data$wl <- log(data$wl)
  lin.mod <- lm(spec ~ wl, data = data) #linear model with IRLS for data
  d <- davies.test(lin.mod, seg.Z = ~ wl) # test if difference in slopes
  if(d$p.value < p_value){ #only does piecewise regression if null hypothesis of same slopes is rejected
    segmented.mod <- segmented(lin.mod,
                               seg.Z = ~ wl,
                               psi = NA,
                               control = seg.control(
                                 stop.if.error = FALSE,
                                 it.max = 5000,
                                 K = 1,
                                 n.boot = 0,
                                 h = 0.15)) #create piecewise regression
    if(is.null(summary(segmented.mod)$psi[1, 2])){
      slope1 <- coef(lin.mod)[[2]]
      slope2 <- slope1
      intercept1 <- coef(lin.mod)[[1]]
      intercept2 <- intercept1
      wlsplit <- NA
    } else {
      slope1 <- slope(segmented.mod)$wl[[1]]
      intercept1 <- intercept(segmented.mod)$wl[[1]]
      slope2 <- slope(segmented.mod)$wl[[2]]
      intercept2 <- intercept(segmented.mod)$wl[[2]]
      wlsplit <- exp(summary(segmented.mod)$psi[1, 2])
    }
  } else{
    slope1 <- coef(lin.mod)[[2]]
    slope2 <- slope1
    intercept1 <- coef(lin.mod)[[1]]
    intercept2 <- intercept1
    wlsplit <- NA
  }
  
  results <- data.frame(wlsplit, slope1, intercept1, slope2, intercept2)
}

# Calculate slopes
slopes <- spect %>%
  group_by(site) %>%
  do(piecewise_fun(., 0.05))

# Plot spectral data
# Create data frame for slope example
x = 10 ^ seq(1.4, 2.2, 0.2) 
y_fit = exp(-4) * x ^ 2
dat = data.frame(x, y_fit)

spec_plot <- ggplot(data = results, 
                    aes(x = wl, y = spec)) + 
  geom_line() +
  # geom_line(data = dat, aes(x = x, y = y_fit), colour = "red", size = 1) +
  scale_x_log10(
    labels = trans_format('log10', math_format(10 ^ .x)),
    breaks = c(10 ^ -1, 10 ^ -0, 10 ^ 1, 10 ^ 2, 10 ^ 3)) +
  scale_y_log10(
    labels = trans_format('log10', math_format(10 ^ .x)),
    breaks = c(10 ^ 0, 10 ^ 1, 10 ^ 2, 10 ^ 3, 10 ^ 4, 10 ^ 5, 10 ^
                 6)) +
  theme_bw() +
  theme(
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 18, colour = "black"),
    panel.background = element_blank(),
    legend.position = "none"
  ) +
  xlab("Wavelength (days)") +
  ylab("Spectral power") +
  annotate("text", x = 50, y = 10^1, label = "1/f ^ 2", parse = TRUE,
           colour = "red") #+
  # facet_wrap(~site)
spec_plot
