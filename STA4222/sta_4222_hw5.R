# ==========================================
#               Problem 1
# ==========================================

m1 <- 65
m2 <- 25
m3 <- 48
m4 <- 65
m5 <- 2
m6 <- 62
m7 <- 65
m8 <- 62
m9 <- 61
m10 <- 41

y1 <- c(3, 0, 0, 4)
y2 <- c(2, 1, 2, 0)
y3 <- c(0, 0, 1, 0)
y4 <- c(2, 0, 1, 0)
y5 <- c(2, 0)
y6 <- c(0, 2, 2, 5)
y7 <- c(1, 0, 0, 3)
y8 <- c(4, 1, 0, 0)
y9 <- c(2, 2, 3, 1)
y10 <- c(2, 5, 12, 3)

phi1 <- 0.0805452
phi2 <- 0.0309789
phi3 <- 0.0594796
phi4 <- 0.0805452
phi5 <- 0.0024783
phi6 <- 0.0768278
phi7 <- 0.0805452
phi8 <- 0.0768278
phi9 <- 0.0755886
phi10 <- 0.0508055

length(y1)

y1_bar <- sum(y1) / length(y1)
y2_bar <- sum(y2) / length(y2)
y3_bar <- sum(y3) / length(y3)
y4_bar <- sum(y4) / length(y4)
y5_bar <- sum(y5) / length(y5)
y6_bar <- sum(y6) / length(y6)
y7_bar <- sum(y7) / length(y7)
y8_bar <- sum(y8) / length(y8)
y9_bar <- sum(y9) / length(y9)
y10_bar <- sum(y10) / length(y10)


t1_hat <- m1 * y1_bar
t2_hat <- m2 * y2_bar
t3_hat <- m3 * y3_bar
t4_hat <- m4 * y4_bar
t5_hat <- m5 * y5_bar
t6_hat <- m6 * y6_bar
t7_hat <- m7 * y7_bar
t8_hat <- m8 * y8_bar
t9_hat <- m9 * y9_bar
t10_hat <- m10 * y10_bar

t1_bar <- t1_hat / phi1
t2_bar <- t2_hat / phi2
t3_bar <- t3_hat / phi3
t4_bar <- t4_hat / phi4
t5_bar <- t5_hat / phi5
t6_bar <- t6_hat / phi6
t7_bar <- t7_hat / phi7
t8_bar <- t8_hat / phi8
t9_bar <- t9_hat / phi9
t10_bar <- t10_hat / phi10

total <- sum(
    t1_bar, t2_bar, t3_bar, t4_bar, t5_bar,
    t6_bar, t7_bar, t8_bar, t9_bar, t10_bar
) / 10

total

diff1 <- (t1_bar - total) ^ 2
diff2 <- (t2_bar - total) ^ 2
diff3 <- (t3_bar - total) ^ 2
diff4 <- (t4_bar - total) ^ 2
diff5 <- (t5_bar - total) ^ 2
diff6 <- (t6_bar - total) ^ 2
diff7 <- (t7_bar - total) ^ 2
diff8 <- (t8_bar - total) ^ 2
diff9 <- (t9_bar - total) ^ 2
diff10 <- (t10_bar - total) ^ 2

total_diff <- sum(
    diff1, diff2, diff3, diff4, diff5,
    diff6, diff7, diff8, diff9, diff10
)


error <- sqrt((1 / 10) * (total_diff / 9))
error