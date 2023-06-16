
# function 1 --------------------------------------------------------------



Duration_Dry_Spell_wave <- function(T, threshold) {
  I_on <- which(T > threshold)
  I_off <- which(T <= threshold)
  
  sigma <- rep(0, length(T))
  
  sigma[I_on] <- 1 # 1 if above threshold
  sigma[I_off] <- 0
  
  I <- diff(sigma)
  ti_tem <- which(abs(I - 1) < 0.1) + 1
  tf <- which(abs(I + 1) < 0.1) + 1
  
  if (T[1] >= threshold) {
    ti <- numeric(length(ti_tem) + 1)
    ti[1] <- 1
    ti[2:length(ti)] <- ti_tem
  } else {
    ti <- ti_tem
  }
  
  if (length(ti) > length(tf)) {
    ti <- ti[1:(length(ti) - 1)]
  }
  
  duration <- tf - ti
  mean_dur <- mean(duration)
  std_dur <- sd(duration)
  tL <- std_dur ^ 2 / mean_dur
  
  return(list(
    duration = duration,
    mean_dur = mean_dur,
    std_dur = std_dur,
    tL = tL
  ))
}



# function 2 --------------------------------------------------------------

Duration_dry <- function(prec, threshold) {
  I_wet <- which(prec > threshold)
  I_dry <- which(prec <= threshold)
  
  sigma <- rep(0, length(prec))
  sigma[I_wet] <- 1  # 1 if it rains that day, 0 if not
  sigma[I_dry] <- 0
  
  I <- diff(sigma)
  ti_tem <- which(abs(I + 1) < 0.1) + 1
  tf <- which(abs(I - 1) < 0.1) + 1
  
  if (length(ti_tem) < 10) {
    duration <- NA
    ti <- NA
    tf <- NA
    n <- NA
    mean_dur <- NA
    tL <- NA
    t99 <- NA
    t999 <- NA
  } else {
    if (prec[1] > threshold) {
      ti <- numeric(length(ti_tem))
      ti <- ti_tem
    } else {
      ti <- numeric(length(ti_tem) + 1)
      ti[1] <- 1
      ti[2:length(ti)] <- ti_tem
    }
    
    if (length(ti) > length(tf)) {
      ti <- ti[-length(ti)]
    }
    
    duration <- tf - ti
    n <- length(duration)
    
    ti <- as.integer(ti)
    tf <- as.integer(tf)
    
    mean_dur <- mean(duration)
    tL <- var(duration) / mean(duration)
    t99 <- quantile(duration, 0.99)
    t999 <- quantile(duration, 0.999)
  }
  
  return(list(duration = duration, ti = ti, tf = tf, n = n, mean_dur = mean_dur, tL = tL, t99 = t99, t999 = t999))
}
