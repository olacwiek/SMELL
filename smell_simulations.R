library(lme4)
library(survival)
library(parallel)

# frequentist models stand in for the preregistered brms models to keep the runs fast in the proposal stage
nsim  <- 500
cores <- max(1, detectCores() - 1)

run <- function(grid, f, n = nsim, offset = 0) {
  jobs <- expand.grid(row = seq_len(nrow(grid)), sim = seq_len(n))
  res <- parLapply(cl, seq_len(nrow(jobs)), function(j, jobs, grid, f, offset) {
    set.seed(offset + j)
    do.call(f, as.list(grid[jobs$row[j], , drop = FALSE]))
  }, jobs, grid, f, offset)
  out <- cbind(grid[jobs$row, , drop = FALSE], do.call(rbind, res))
  aggregate(out[setdiff(names(out), names(grid))], out[names(grid)], mean)
}

assign_odors <- function(pairs, per_pair, odors = 20) {
  used <- integer(odors)
  do.call(rbind, lapply(seq_len(pairs), function(p) {
    o <- order(used + runif(odors))[seq_len(per_pair)]
    used[o] <<- used[o] + 1
    data.frame(pair = p, odor = o)
  }))
}


# study 1: convergence (H1a) -------------------------------------------------

vowels     <- c("a", "e", "i", "o", "u", "y")
consonants <- c("p", "b", "t", "d", "k", "g", "f", "w", "s", "z",
                "S", "Z", "x", "m", "n", "l", "r", "j", "c", "C")
templates  <- c("CVCV", "CVCVC", "CCVCV", "CVCCV", "VCV") # this is not exhaustive, but very common patterns
template_p <- c(.4, .2, .15, .15, .1)

make_words <- function(d, sd_odor, sd_pair = .5, odors = 20) {
  pairs <- max(d$pair)
  odor_v <- matrix(rnorm(odors * 6, 0, sd_odor), odors)
  odor_c <- matrix(rnorm(odors * 20, 0, sd_odor), odors)
  pair_v <- matrix(rnorm(pairs * 6, 0, sd_pair), pairs)
  pair_c <- matrix(rnorm(pairs * 20, 0, sd_pair), pairs)
  vapply(seq_len(nrow(d)), function(i) {
    slots <- strsplit(sample(templates, 1, prob = template_p), "")[[1]]
    pv <- exp(odor_v[d$odor[i], ] + pair_v[d$pair[i], ])
    pc <- exp(odor_c[d$odor[i], ] + pair_c[d$pair[i], ])
    paste(vapply(slots, function(s)
      if (s == "V") sample(vowels, 1, prob = pv) else sample(consonants, 1, prob = pc), ""),
      collapse = "")
  }, "")
}

dist_levenshtein <- function(w) {
  n <- nchar(w)
  adist(w) / outer(n, n, pmax)
}

dist_bigram <- function(w) {
  b <- lapply(paste0("#", w, "#"), function(x) unique(substring(x, 1:(nchar(x) - 1), 2:nchar(x))))
  all_b <- unique(unlist(b))
  B <- t(vapply(b, function(x) all_b %in% x, logical(length(all_b)))) * 1
  shared <- tcrossprod(B)
  size <- rowSums(B)
  1 - shared / (outer(size, size, "+") - shared)
}

test_h1a <- function(D, d, nperm = 100) {
  other_pair <- outer(d$pair, d$pair, "!=") * 1
  Dk <- D * other_pair
  odors <- max(d$odor)
  M <- outer(d$odor, seq_len(odors), "==") * 1
  n_same <- sum(M * (other_pair %*% M))
  stat <- function(M) sum(M * (Dk %*% M)) / n_same - sum(Dk) / sum(other_pair)
  obs <- stat(M)
  perm <- replicate(nperm, {
    o <- ave(d$odor, d$pair, FUN = function(x) x[sample.int(length(x))])
    stat(outer(o, seq_len(odors), "==") * 1)
  })
  c(p = pnorm((obs - mean(perm)) / sd(perm)), diff = obs)
}

sim_study1 <- function(sd_odor, pairs = 40, per_pair = 10) {
  d <- assign_odors(pairs, per_pair)
  w <- make_words(d, sd_odor)
  lev <- test_h1a(dist_levenshtein(w), d)
  big <- test_h1a(dist_bigram(w), d)
  c(levenshtein = lev[["p"]] < .05, bigram = big[["p"]] < .05, closer = -100 * lev[["diff"]])
}


# study 2: words entering the lexicon (H1b) ---------------------------------

sim_study2 <- function(trials, p_linked = .35) {
  linked <- rep(c(TRUE, FALSE), each = 50)
  p <- ifelse(linked, plogis(rnorm(100, qlogis(p_linked), .4)), 1 / 6)
  eta <- qlogis(rep(p, each = trials)) + rnorm(100 * trials, 0, .5)
  k <- rowSums(matrix(rbinom(100 * trials, 1, plogis(eta)), 100, byrow = TRUE))
  in_lexicon <- pbeta(1 / 6, 1 + k, 1 + trials - k, lower.tail = FALSE) > .95
  c(linked = mean(in_lexicon[linked]), chance = mean(in_lexicon[!linked]))
}


# study 3: affective code (H2a) ---------------------------------------------

sim_study3a <- function(odors, words_per_odor, b) {
  d <- assign_odors(odors * words_per_odor / 10, 10, odors)
  pleasant <- rnorm(odors)
  d$pleasant <- pleasant[d$odor]
  eta <- qlogis(1 / 6) + b * d$pleasant +
    rnorm(odors, 0, .3)[d$odor] + rnorm(max(d$pair), 0, .3)[d$pair]
  d$k <- rbinom(nrow(d), 2, plogis(eta))
  m <- glmer(cbind(k, 2 - k) ~ pleasant + (1 | odor) + (1 | pair), d,
             family = binomial, nAGQ = 0)
  c(power = coef(summary(m))["pleasant", "z value"] > qnorm(.95))
}


# study 3: odor quality beyond affect (H2b) ---------------------------------

loo_density <- function(y, X) {
  vapply(seq_along(y), function(k) {
    f <- lm.fit(X[-k, , drop = FALSE], y[-k])
    df <- length(y) - 1 - ncol(X)
    s <- sqrt(sum(f$residuals^2) / df)
    se <- s * sqrt(1 + drop(X[k, ] %*% chol2inv(qr.R(f$qr)) %*% X[k, ]))
    dt((y[k] - sum(X[k, ] * f$coefficients)) / se, df, log = TRUE) - log(se)
  }, 0)
}

sim_study3b <- function(odors, quality, extra = 0, words_per_odor = 20, icc = .15, nperm = 499) {
  affect <- rnorm(odors)
  map <- cbind(.4 * affect + sqrt(1 - .4^2) * rnorm(odors), rnorm(odors))
  map_beyond <- cbind((map[, 1] - .4 * affect) / sqrt(1 - .4^2), map[, 2])
  w_affect <- sample(c(-1, 1), 4, TRUE) * sqrt(.3)
  angle <- runif(4, 0, 2 * pi)
  w_quality <- rbind(cos(angle), sin(angle)) * sqrt(quality)
  Y <- outer(affect, w_affect) + map_beyond %*% w_quality +
    matrix(rnorm(odors * 4, 0, sqrt(1 - .3 - quality)), odors) +
    matrix(rnorm(odors * 4, 0, sqrt((1 - icc) / icc / words_per_odor)), odors)
  X_affect <- cbind(1, affect)
  X_full <- cbind(X_affect, map, matrix(rnorm(odors * extra), odors))
  gain <- rowSums(sapply(1:4, function(j) loo_density(Y[, j], X_full) - loo_density(Y[, j], X_affect)))
  z <- sum(gain) / (sqrt(odors) * sd(gain))
  lt <- lower.tri(diag(odors))
  qa <- qr(cbind(1, as.matrix(dist(affect))[lt]))
  r_map <- qr.resid(qa, as.matrix(dist(map))[lt])
  DY <- as.matrix(dist(Y))
  rsa <- function(o) cor(r_map, qr.resid(qa, DY[o, o][lt]))
  obs <- rsa(seq_len(odors))
  perm <- replicate(nperm, rsa(sample(odors)))
  c(loo = z > 2, rsa = mean(c(perm, obs) >= obs) < .05)
}


# study 4: generated words against foils (H3) -------------------------------

sim_study4 <- function(participants, quality, affect = .6, matched = 3, odors = 10) {
  d <- expand.grid(odor = seq_len(odors), id = seq_len(participants))
  d$word <- (d$odor - 1) * 5 + (d$id + d$odor) %% 5 + 1
  n <- nrow(d)
  scale <- exp(rnorm(participants, 0, .3))[d$id]
  foil_affect <- rep(c(1, 0), c(matched, 5 - matched))
  U <- scale * cbind(affect + quality + rnorm(odors * 5, 0, .3)[d$word],
                     matrix(affect * foil_affect, n, 5, byrow = TRUE) + rnorm(n * 5, 0, .3))
  choice <- max.col(U - log(-log(matrix(runif(n * 6), n))))
  d$correct <- as.integer(choice == 1)
  overall <- glmer(correct ~ 1 + (1 | id) + (1 | odor) + (1 | word), d,
                   family = binomial, nAGQ = 0)
  z_overall <- (fixef(overall)[[1]] - qlogis(1 / 6)) / sqrt(vcov(overall)[1, 1])
  alts <- data.frame(trial = rep(seq_len(n), 6),
                     id = rep(d$id, 6),
                     chosen = as.integer(rep(1:6, each = n) == rep(choice, 6)),
                     target = rep(c(1, 0), c(n, 5 * n)),
                     affect = rep(c(1, foil_affect), each = n))
  cl_fit <- clogit(chosen ~ target + affect + strata(trial) + cluster(id), alts,
                   method = "efron")
  c(matching = mean(d$correct),
    overall = z_overall > qnorm(.95),
    beyond_affect = coef(summary(cl_fit))["target", "z"] > qnorm(.95))
}


# study 5: transfer with words vs odors only (H4a) --------------------------

sim_study5 <- function(n, gain, base = .40, retest = .02, trials = 20) {
  b0 <- qlogis(base)
  b_time <- qlogis(base + retest) - b0
  b_words <- qlogis(base + retest + gain) - qlogis(base + retest)
  d <- expand.grid(id = seq_len(2 * n), time = 0:1)
  d$words <- as.integer(d$id > n)
  eta <- b0 + rnorm(2 * n, 0, .5)[d$id] + b_time * d$time +
    b_words * d$time * d$words + rnorm(nrow(d), 0, .3)
  d$k <- rbinom(nrow(d), trials, plogis(eta))
  m <- glmer(cbind(k, trials - k) ~ words * time + (1 | id) + (1 | id:time), d,
             family = binomial)
  c(power = coef(summary(m))["words:time", "z value"] > qnorm(.95))
}

n_ancova <- function(d, r) ceiling(2 * (qnorm(.95) + qnorm(.8))^2 * (1 - r^2) / d^2)


# runs -----------------------------------------------------------------------

cl <- makeCluster(cores)
invisible(clusterEvalQ(cl, { library(lme4); library(survival) }))
clusterExport(cl, setdiff(ls(), "cl"))

results <- list(
  study1  = run(expand.grid(sd_odor = c(0, .1, .15, .2, .25, .3)), sim_study1, offset = 1e5),
  study2  = run(expand.grid(trials = c(8, 12, 16, 20, 30, 40)), sim_study2, offset = 2e5),
  study3a = run(expand.grid(odors = c(15, 20, 25, 30), words_per_odor = c(10, 20, 40), b = c(0, .4)),
                sim_study3a, offset = 3e5),
  study3b = run(expand.grid(odors = c(15, 20, 25, 30), quality = c(0, .3)), sim_study3b, offset = 4e5),
  study3c = run(expand.grid(odors = 20, quality = .3, extra = c(0, 2, 4, 6, 8)), sim_study3b, offset = 5e5),
  study4  = run(expand.grid(participants = c(30, 45, 60, 90, 120), quality = c(0, .1, .2, .3)),
                sim_study4, offset = 6e5),
  study5  = run(expand.grid(n = c(40, 60, 80, 100, 120, 150), gain = c(0, .04, .06, .08, .10)),
                sim_study5, offset = 7e5),
  study5_secondary = outer(c(d0.2 = .2, d0.3 = .3, d0.4 = .4, d0.5 = .5),
                           c(r0.5 = .5, r0.6 = .6, r0.7 = .7), n_ancova)
)

stopCluster(cl)
saveRDS(results, "smell_simulations.rds")
print(results)
