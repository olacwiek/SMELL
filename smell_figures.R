library(ggplot2)
library(patchwork)

ink     <- "#191919"
muted   <- "#666666"
grid_c  <- "#e7e7e7"
axis_c  <- "#c2c2c2"
blue    <- "#257de0"
orange  <- "#f47230"
light   <- "#84b9f3"
ramp3   <- c("#84b9f3", "#257de0", "#0e3d7e")
ramp4   <- c("#84b9f3", "#338ced", "#1b64be", "#0e3d7e")
fs      <- 1.3
pt      <- function(x) fs * x / .pt

theme_set(
  theme_minimal(base_size = 8 * fs) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = grid_c, linewidth = .25),
          axis.line = element_line(colour = axis_c, linewidth = .3),
          axis.ticks = element_line(colour = axis_c, linewidth = .3),
          axis.text = element_text(colour = muted),
          axis.title = element_text(colour = ink),
          plot.title = element_text(colour = ink, face = "bold", size = rel(1)),
          plot.title.position = "plot",
          plot.tag = element_text(face = "bold", size = rel(1.25)),
          legend.position = "top",
          legend.justification = "left",
          legend.title = element_text(colour = muted, size = rel(.875)),
          legend.text = element_text(size = rel(.875)),
          legend.key.width = unit(5, "mm"),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.spacing = unit(1, "mm"))
)

save_fig <- function(p, name, height) {
  ggsave(paste0(name, ".pdf"), p, width = 17, height = height, units = "cm", device = cairo_pdf)
  ggsave(paste0(name, ".png"), p, width = 17, height = height, units = "cm", dpi = 600, bg = "white")
}

head_arrow <- arrow(length = unit(1.3, "mm"), type = "closed")


# figure 1 ---------------------------------------

fig1a <- ggplot() +
  annotate("text", x = 0, y = c(4.6, 3.4, 1.6), label = c("Sound", "Shape, size", "Smell"),
           hjust = 0, fontface = "bold", size = pt(8), colour = ink) +
  annotate("text", x = 4, y = c(4.6, 3.4, 1.6), label = c("a cat's meow", "a spiky shape", "garlic"),
           size = pt(8), colour = ink) +
  annotate("text", x = 4, y = 1, label = "cannot be produced\nwith the voice",
           size = pt(6.5), colour = muted, lineheight = .9) +
  annotate("text", x = 8.5, y = c(4.6, 3.4, 2.37, .9),
           label = c("the voice imitates\nthe sound", "a vocal property\nstands in for it",
                     "the reaction", "odor quality?"),
           size = pt(7.5), colour = ink, lineheight = .9) +
  annotate("text", x = 8.5, y = 1.95, label = 'paste("it causes: ", italic("blech!"))',
           parse = TRUE, size = pt(7.5), colour = ink) +
  annotate("segment", x = c(5.6, 10.3), xend = c(6.7, 11.4), y = 4.6, yend = 4.6,
           arrow = head_arrow, linewidth = .4, colour = ink) +
  annotate("segment", x = c(5.6, 10.3), xend = c(6.7, 11.4), y = 3.4, yend = 3.4,
           arrow = head_arrow, linewidth = .4, colour = ink) +
  annotate("segment", x = c(5.6, 10.3), xend = c(6.7, 11.4), y = c(1.7, 2.15), yend = c(2.15, 1.7),
           arrow = head_arrow, linewidth = .5, colour = blue) +
  annotate("segment", x = c(5.6, 10.3), xend = c(6.7, 11.4), y = c(1.5, .9), yend = c(.9, 1.5),
           arrow = head_arrow, linewidth = .5, colour = orange, linetype = "22") +
  annotate("text", x = 10.85, y = c(2.32, .62), label = c("H2a", "H2b"),
           fontface = "bold", size = pt(7), colour = ink) +
  annotate("text", x = 11.7, y = c(4.6, 3.4, 1.6), label = c("miau", "kiki", "bleszczur"),
           hjust = 0, fontface = "italic", size = pt(9), colour = ink) +
  coord_cartesian(xlim = c(0, 17), ylim = c(.4, 5.1), expand = FALSE) +
  theme_void()

pilot <- data.frame(
  odor       = c("soap", "whole nutmeg", "limescale remover", "garlic", "fish sauce"),
  word       = c("lamok", "flubuf", "lemklin", "bleszczur", "żarfek"),
  segments   = c("l a m o k", "f l u b u f", "l e m k l i n", "b l e sz cz u r", "ż a r f e k"),
  unpleasant = c(.10, .25, .55, .72, .92),
  quality    = c(.80, .35, .75, .32, .10),
  colour     = c("#257de0", "#84b9f3", "#a6a6a6", "#f4a3a4", "#ec4647")
)
segs <- strsplit(pilot$segments, " ")
pilot$affective <- sapply(segs, function(s) mean(s %in% c("ż", "sz", "cz", "r")))
pilot$sonorant  <- sapply(segs, function(s) mean(s %in% c("l", "m", "n")))

space <- function(x, y, label, italic = FALSE) {
  ggplot(pilot, aes({{ x }}, {{ y }})) +
    geom_point(aes(fill = colour), shape = 21, size = 2.6, stroke = .3, colour = ink) +
    geom_text(aes(label = {{ label }}), nudge_y = .07, size = pt(7.5), colour = ink,
              fontface = if (italic) "italic" else "plain") +
    scale_fill_identity()
}

fig1b_odors <- space(unpleasant, quality, odor) +
  scale_x_continuous("Unpleasantness", limits = c(-.05, 1.1), breaks = NULL) +
  scale_y_continuous("Odor quality", limits = c(0, 1), breaks = NULL) +
  labs(title = "Odor space")

fig1b_words <- space(affective, sonorant, word, italic = TRUE) +
  scale_x_continuous("Share of ż, sz, cz, r", limits = c(-.05, .5)) +
  scale_y_continuous("Share of l, m, n", limits = c(-.05, .67)) +
  labs(title = "Word space")

fig1 <- fig1a / (fig1b_odors | fig1b_words) +
  plot_layout(heights = c(4.8, 6)) +
  plot_annotation(tag_levels = list(c("A", "B", ""))) &
  theme(plot.tag = element_text(face = "bold", size = 10 * fs))

save_fig(fig1, "figure1", 12)


# figure 2 -------------------------

boxes <- data.frame(
  x      = c(1.6, 5, 8.4, 11.8, 15.2, 13.5),
  y      = c(5, 5, 5, 5, 5, 1.6),
  w      = c(3, 3, 3, 3, 3, 6),
  h      = c(1.35, 1.35, 1.35, 1.35, 1.35, 1.2),
  head   = c("Task 1", "Study 1", "Study 2", "Study 3", "Study 4", "Study 5"),
  rq     = c("months 1–8", "RQ1", "RQ1", "RQ2, RQ3", "RQ3", "RQ4"),
  title  = c("Odor set and\nodor space", "Creating words", "Matching words",
             "AI model and\ngeneration", "Matching\ngenerated words", "Learning words"),
  detail = c("40 raters,\n30 odors", "40 pairs,\n400 words", "150 participants,\n100 words",
             "50 generated\nwords", "60 participants",
             "240 participants, two groups,\nToruń and Wrocław"),
  fill   = c("white", "white", "#e6f0fc", "white", "white", "white"),
  line   = c(axis_c, axis_c, blue, axis_c, axis_c, axis_c)
)

fig2 <- ggplot(boxes) +
  geom_rect(aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h, ymax = y + h,
                fill = fill, colour = line), linewidth = .4) +
  geom_text(aes(x, y + h - .35, label = head), fontface = "bold", size = pt(7.5), colour = ink) +
  geom_text(aes(x, y + h - .72, label = rq), size = pt(6.5), colour = muted) +
  geom_text(aes(x, y - .11, label = title), size = pt(7.5), colour = ink, lineheight = .9) +
  geom_text(aes(x, y - h + .5, label = detail), size = pt(6.5), colour = muted, lineheight = .9) +
  annotate("segment", x = c(3.1, 6.5, 9.9, 13.3), xend = c(3.5, 6.9, 10.3, 13.7),
           y = 5, yend = 5, arrow = head_arrow, linewidth = .4, colour = ink) +
  annotate("segment", x = 8.4, xend = 8.4, y = 3.65, yend = 1.6, linewidth = .4, colour = blue) +
  annotate("segment", x = 8.4, xend = 10.5, y = 1.6, yend = 1.6,
           arrow = head_arrow, linewidth = .4, colour = blue) +
  annotate("text", x = 8.55, y = 3.2, label = "validated lexicon (month 18)",
           hjust = 0, fontface = "bold", size = pt(7), colour = ink) +
  annotate("curve", x = 1.6, xend = 11.8, y = 6.35, yend = 6.35, curvature = -.12,
           arrow = head_arrow, linewidth = .4, colour = muted) +
  annotate("text", x = 6.7, y = 7.35, label = "odor descriptions and the 10 held-back odors",
           size = pt(7), colour = muted) +
  scale_fill_identity() +
  scale_colour_identity() +
  coord_cartesian(xlim = c(0, 17), ylim = c(.2, 7.7), expand = FALSE) +
  theme_void()

save_fig(fig2, "figure2", 7.9)


# figure 3 --------------------------------------------------

tasks <- c("Task 1. Odor set and\nodor space",
           "Task 2. Study 1:\ncreating odor words",
           "Task 3. Study 2: matching\nthe created words",
           "Task 4. Study 3: AI model\nand generation",
           "Task 5. Study 4: matching\nthe generated words",
           "Task 6. Study 5:\nlearning odor words")

bars <- read.table(header = TRUE, text = "
task start end label prep
1     1    8   ''              0
2     7   12   ''              0
3    11   18   ''              0
4    13   16   prep.           1
4    17   24   ''              0
5    23   30   ''              0
6    16   19   pilot           1
6    20   32   'data collection' 0
6    33   36   analysis        0
")

milestones <- data.frame(name = paste0("M", 1:8),
                         month = c(6, 8, 12, 18, 24, 30, 32, 36),
                         task = c(1, 1, 2, 3, 4, 5, 6, 6))

years <- data.frame(year = 2027:2030, from = c(0, 3, 15, 27), to = c(3, 15, 27, 36))

fig3 <- ggplot() +
  geom_vline(xintercept = c(3, 15, 27), colour = axis_c, linewidth = .3) +
  annotate("text", x = (years$from + years$to) / 2, y = 7.1, label = years$year,
           size = pt(7), colour = muted) +
  geom_vline(xintercept = 18, linetype = "dashed", colour = ink, linewidth = .35) +
  geom_rect(aes(xmin = start - 1, xmax = end, ymin = 7 - task - .28, ymax = 7 - task + .28,
                fill = factor(prep)), data = bars, colour = "white", linewidth = .4) +
  geom_text(aes((start - 1 + end) / 2, 7 - task, label = label,
                colour = factor(prep)), data = bars, size = pt(6.5)) +
  geom_point(aes(month, 7 - task + .28), data = milestones, shape = 23, size = 2,
             fill = orange, colour = "white", stroke = .4) +
  geom_text(aes(month, 7 - task + .55, label = name), data = milestones,
            size = pt(6.5), colour = ink) +
  scale_fill_manual(values = c("0" = blue, "1" = light), guide = "none") +
  scale_colour_manual(values = c("0" = "white", "1" = ink), guide = "none") +
  scale_x_continuous("Project month", breaks = seq(3, 36, 3) - .5, labels = seq(3, 36, 3),
                     limits = c(0, 36), expand = c(0, 0)) +
  scale_y_continuous(NULL, breaks = 6:1, labels = tasks, limits = c(.5, 7.3), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5.5, 10, 5.5, 5.5),
        panel.grid.major = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = ink, hjust = 0, lineheight = .9))

save_fig(fig3, "figure3", 7.5)


# figure 4 -----------------------------

sims <- readRDS("smell_simulations.rds")

power_axis <- list(
  geom_hline(yintercept = .8, linetype = "dashed", colour = muted, linewidth = .3),
  scale_y_continuous("Power", limits = c(0, 1), breaks = seq(0, 1, .2), labels = scales::percent)
)
design <- function(x) geom_vline(xintercept = x, linetype = "dotted", colour = muted, linewidth = .3)
lines <- list(geom_line(linewidth = .5), geom_point(size = 1.3))

s1 <- sims$study1
s1 <- data.frame(closer = s1$closer, power = c(s1$levenshtein, s1$bigram),
                 measure = rep(c("Levenshtein", "bigrams"), each = nrow(s1)))
fig4a <- ggplot(s1, aes(closer, power, colour = measure)) +
  power_axis + lines +
  scale_colour_manual("Distance", values = c(Levenshtein = blue, bigrams = orange),
                      breaks = c("Levenshtein", "bigrams")) +
  labs(title = "Study 1: convergence (H1a)",
       x = "Same-odor words closer by\n(% of normalized Levenshtein)")

s2 <- sims$study2
s2 <- data.frame(trials = s2$trials, share = c(s2$linked, s2$chance),
                 word = rep(c("35% correct", "at chance"), each = nrow(s2)))
fig4b <- ggplot(s2, aes(trials, share, colour = word)) +
  design(30) + lines +
  scale_colour_manual("Words", values = c(`35% correct` = blue, `at chance` = axis_c)) +
  scale_y_continuous("In the lexicon", limits = c(0, 1), labels = scales::percent) +
  labs(title = "Study 2: validated lexicon (H1b)", x = "Trials per word")

s3a <- subset(sims$study3a, b > 0)
fig4c <- ggplot(s3a, aes(odors, power, colour = factor(words_per_odor))) +
  design(20) + power_axis + lines +
  scale_colour_manual("Words per odor", values = ramp3) +
  labs(title = "Study 3: affective code (H2a)", x = "Core odors")

s3b <- subset(sims$study3b, quality > 0)
s3b <- data.frame(odors = s3b$odors, power = c(s3b$rsa, s3b$loo),
                  test = rep(c("RSA", "leave-one-odor-out"), each = nrow(s3b)))
fig4d <- ggplot(s3b, aes(odors, power, colour = test)) +
  design(20) + power_axis + lines +
  scale_colour_manual("Test", values = c(RSA = blue, `leave-one-odor-out` = orange),
                      breaks = c("RSA", "leave-one-odor-out")) +
  labs(title = "Study 3: odor quality (H2b)", x = "Core odors")

s4 <- sims$study4
rate <- tapply(s4$matching, s4$quality, mean)
s4$level <- factor(s4$quality, names(rate), round(100 * rate))
fig4e <- ggplot(s4, aes(participants, beyond_affect, colour = level)) +
  design(60) + power_axis + lines +
  scale_colour_manual("Matching (%)", values = c(axis_c, ramp3)) +
  scale_x_continuous(breaks = c(30, 45, 60, 90, 120)) +
  labs(title = "Study 4: matching beyond affect (H3)", x = "Participants")

s5 <- subset(sims$study5, gain > 0)
fig4f <- ggplot(s5, aes(n, power, colour = factor(100 * gain))) +
  design(102) + power_axis + lines +
  scale_colour_manual("Gain (points)", values = ramp4) +
  scale_x_continuous(breaks = c(40, 60, 80, 100, 120, 150)) +
  labs(title = "Study 5: transfer (H4a)", x = "Completers per group")

fig4 <- (fig4a | fig4b) / (fig4c | fig4d) / (fig4e | fig4f) +
  plot_annotation(tag_levels = "A") &
  theme(plot.title = element_text(size = rel(1.25)))

save_fig(fig4, "figure4", 19.5)
