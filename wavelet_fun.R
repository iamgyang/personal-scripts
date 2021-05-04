# apply many iterations of haar wavelet transform on an image ---------------

# clear environment objects (this deletes everything in the environment but
# the packages)
rm(list = ls())

# this downloads all the packages you need to use. change the vector,
# "list.of.packages" as needed

list.of.packages <-
  c("data.table", "dplyr", "png", "colorspace", "RCurl", "jpeg")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
for (package in list.of.packages) {
  library(eval((package)), character.only = TRUE)
}

myurl <-
  "https://www.celebrityhow.com/wp-content/uploads/2018/12/Liam-Neeson.png"
x <-  readPNG(getURLContent(myurl))
dim(x)
y <- rgb(x[, , 1], x[, , 2], x[, , 3])
yg <- desaturate(y)
yn <- col2rgb(yg)[1,]

dim(yn) <- dim(yg) <- dim(x)[1:2]

# make image even dimensions:
create_even_dims <- function(mat_) {
  if (dim(mat_)[2] %% 2 == 1) {
    mat_ <- mat_[1:dim(mat_)[1],
                 1:(dim(mat_)[2] - 1)]
  }
  if (dim(mat_)[1] %% 2 == 1) {
    mat_ <- mat_[1:(dim(mat_)[1] - 1),
                 1:(dim(mat_)[2])]
  }
  mat_
}

yn <- create_even_dims(yn)

haar_lo <- c(sqrt(2) / 2, sqrt(2) / 2)
haar_hi <- c(-sqrt(2) / 2, sqrt(2) / 2)

# get the wavelet filter by shifting the first column
# by 2 multiple times.
all_two_translates <-
  function(bob, dim_) {
    for (i_ in seq(2L, (dim_) / 2L)) {
      bob[, paste0("V", i_) :=
            shift(V1, 2L * (i_ - 1L), fill = 0L)]
      bob <- as.data.table(bob)
    }
    t(bob) %>% as.data.table()
  }

# create a matrix based on low, high, and
# number of columns of the final wavelet
# transform matrix

create_filter_matrix <-
  function(low_pass,
           high_pass,
           dim_) {
    N_low <- length(low_pass)
    N_high <- length(low_pass)
    stopifnot(N_low == N_high)
    
    bob <-
      as.data.table(c(low_pass,
                      rep(0, dim_ - N_low)))
    
    sally <-
      as.data.table(c(high_pass,
                      rep(0, dim_ - N_low)))
    
    bob <- all_two_translates(bob, dim_) %>% as.matrix()
    sally <- all_two_translates(sally, dim_) %>% as.matrix()
    output <- rbind(bob, sally)
    output
  }


# function to compute an iteration of Haar filter:

haar_left <- create_filter_matrix(haar_lo, haar_hi, dim(yn)[1])
haar_right <- create_filter_matrix(haar_lo, haar_hi, dim(yn)[2])

iterate_haar <- function(mat_) {
  haar_left %*% mat_ %*% t(haar_right)
}

invert_haar <- function(mat_) {
  t(haar_left) %*% mat_ %*% haar_right
}

# plot a matrix in R:

plot.matrix <- function(mat_) {
  image(t(mat_[nrow(mat_):1, ]) / 256,
        axes = FALSE,
        col = grey(seq(0, 1, length =
                         256)))
}

png("neeson.first.png")
plot.matrix(yn)
dev.off()
png("neeson.haar1.png")
plot.matrix(iterate_haar(yn))
dev.off()
png("neeson.haar2.png")
plot.matrix(iterate_haar(iterate_haar(yn)))
dev.off()
png("neeson.haar2.invert1.png")
plot.matrix(invert_haar(iterate_haar(iterate_haar(yn))))
dev.off()
png("neeson.haar2.invert2.png")
plot.matrix(invert_haar(invert_haar(iterate_haar(iterate_haar(
  yn
)))))
dev.off()


# get set an alpha and threshold a vector -------------

bob <-
  fread(" a
          0
          0
          0
          0
          0
          0
          0
          0
          0
          0
          51
          77
          170
          204
          220
          268
          292
          316
          380
          410")

alpha <- 1
tau_1 <- mean(max(bob$a), min(bob$a))

while (if (exists("tau_2")) {
  abs(tau_2 - tau_1) >= alpha
} else {
  TRUE
}) {
  if (exists("tau_2")) {
    tau_1 <- tau_2
  }
  cat(paste("tau_1 =",  tau_1), sep = "\n")
  s_1 <- bob$a[bob$a >= tau_1]
  cat(paste("s_1 =",
            paste((s_1), collapse = "; ")), sep = "\n")
  s_2 <- bob$a[bob$a < tau_1]
  cat(paste("s_2 =",
            paste((s_2), collapse = "; ")), sep = "\n")
  tau_2 <- mean(c(mean(s_1), mean(s_2)))
  cat(paste("tau_2 =",  tau_2), sep = "\n")
}

