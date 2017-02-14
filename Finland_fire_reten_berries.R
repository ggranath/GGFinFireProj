################################################################################################
# R code to reproduce results in 
# Strengbom et al. 
# Submitted to Conservation Biological
# Contact: Gustaf.Granath@gmail.com
#################################################################################################

# load data and packages
library(MCMCglmm)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

zero.dat <- read.csv("FIN_fire_reten_exp.csv", stringsAsFactors = FALSE)

# Data were collected near trees/stumps on the northern and southern side. There is no indication
# that aspect matters and therefore data is aggregated into "tree"
# micro_hab.two has "tree" and "flat"
zero.dat$micro_hab.two <- factor(ifelse(zero.dat$micro_hab == "N"|zero.dat$micro_hab == "S", 
                                        "tree", "flat"))
zero.dat$site <- factor(zero.dat$site)

# no retention means clearcut (i.e., cut)
zero.dat[zero.dat$retention == "no", "retention"] <- "cut"
zero.dat$retention <- factor(zero.dat$retention)
zero.dat$fire <- factor(zero.dat$fire)
zero.dat$micro_hab.two <- factor(zero.dat$micro_hab.two)

#plots nested in site
zero.dat$nested_plot <- factor(with(zero.dat, paste(site, plot, sep= "_")) ) 

#### Fruits ####

# Plot raw data, but average within plots

# At plot level and per m2 
library(dplyr)
dd <- zero.dat %>% 
  group_by(site, fire, retention, type, micro_hab.two) %>% 
  summarise_at(vars(VV_cover, VV_fruit, 
                    VM_cover, VM_fruit),
               funs(mean)) %>%
  as.data.frame()

dd$micro_hab.two.ed <- interaction(dd$micro_hab.two, dd$type)
dd$micro_hab.two.ed2 <- ifelse(dd$retention == "elev", as.character(dd$micro_hab.two.ed), 
                               as.character(dd$micro_hab.two) )

# cowberry
cow.prod.raw <- ggplot(data=dd, aes(y=VV_fruit, x=micro_hab.two.ed2, fill = fire)) +
  geom_point(shape = 21, size=2, position=position_jitterdodge(jitter.width=0.95,dodge.width=0)) +
  scale_fill_manual(breaks = c("no", "yes"), values = c("no" = "white", "yes" = "black"),
                    labels = c("unburned", "burned")) +
  facet_grid(~ retention, scales = "free", space = "free", 
             labeller = as_labeller(c('cut' = "clearcut", 'elev' = "retention", 'full' = "unlogged"))) +
  xlab("") +
  ylab(bquote("Berries per 0.16 m" ^2)) +
  theme(axis.text.x  = element_text(size=14, color="black"),
        axis.text.y  = element_text(size=14, color="black"),
        axis.title = element_text(size=14),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size=16),
        legend.title = element_blank(),
        legend.position= c(0.9,0.9),
        legend.key = element_blank(),
        legend.background = element_rect(color = "black", 
                                         fill = "white", size = 0.7, linetype = "solid"),
        axis.line.x = element_line(color="black", size = 0.8),
        axis.line.y = element_line(color="black", size = 0.8),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_x_discrete(breaks = c("flat", "tree", "flat.gr", "flat.open", "tree.gr", "tree.open", "flat", "tree"),
                   labels=c("flat", "tree", "flat\npatch", "flat\ncut", "tree\npatch", "tree\ncut", "flat", "tree")) +
  annotation_custom(
    grob = textGrob(label = "a)", gp = gpar(fontsize = 20)),
    ymin = 113,      # Vertical position of the textGrob
    #  ymax = 5,
    xmin = -16)
cow.gt <- ggplot_gtable(ggplot_build(cow.prod.raw))
cow.gt$layout$clip[cow.gt$layout$name == "panel"] <- "off"
grid.draw(cow.gt)

# bilberry
bil.prod.raw <- ggplot(data=dd, aes(y=VM_fruit, x=micro_hab.two.ed2, fill = fire)) +
  geom_point(shape = 21, size=2, position=position_jitterdodge(jitter.width=0.95,dodge.width=0)) +
  scale_fill_manual(breaks = c("no", "yes"), values = c("no" = "white", "yes" = "black"),
                    labels = c("unburned", "burned"), guide=FALSE) +
  facet_grid(~ retention, scales = "free", space = "free", 
             labeller = as_labeller(c('cut' = "clearcut", 'elev' = "retention", 'full' = "unlogged"))) +
  xlab("") +
  ylab(bquote("Berries per 0.16 m" ^2)) +
  theme(axis.text.x  = element_text(size=14, color="black"),
        axis.text.y  = element_text(size=14, color="black"),
        axis.title = element_text(size=14),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size=16),
        legend.title = element_blank(),
        legend.position= "none",
        legend.key = element_blank(),
        axis.line.x = element_line(color="black", size = 0.8),
        axis.line.y = element_line(color="black", size = 0.8),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_x_discrete(breaks = c("flat", "tree", "flat.gr", "flat.open", "tree.gr", "tree.open", "flat", "tree"),
                   labels=c("flat", "tree", "flat\npatch", "flat\ncut", "tree\npatch", "tree\ncut", "flat", "tree")) +
  annotation_custom(
    grob = textGrob(label = "b)", gp = gpar(fontsize = 20)),
    ymin = 12,      # Vertical position of the textGrob
    #  ymax = 5,
    xmin = -15.5)
bil.gt <- ggplot_gtable(ggplot_build(bil.prod.raw))
bil.gt$layout$clip[bil.gt$layout$name == "panel"] <- "off"
grid.draw(bil.gt)

# Plot Figure 2
png("figure2_berries_raw.png", width=22, height=24, units="cm", res=300)
grid.arrange(cow.gt , bil.gt, ncol=1, nrow =2)
dev.off()

# Means per retention level
means <- zero.dat %>% 
  group_by(retention, fire) %>% 
  summarise_at(vars(VV_cover, VV_fruit, 
                    VM_cover, VM_fruit),
               funs(mean)) %>%
  as.data.frame()
means

# fruits per 1 m2 (sample plot size 0.16 m2)
means[5, c(4,6)] * (1/0.16)

# Cowberry fruits ####

# zip model cowberry
nitt = 300000 #low for testing
thin = 25 #low for testing
burnin = 15000 #low for testing
prior = list( R = list(V = diag(2), nu = 0.002, fix = 2), 
              G=list(G1=list(V=diag(2)*c(1,0.001), nu=0.002, fix=2),
              G2=list(V=diag(2)*c(1,0.001), nu=0.002, fix=2))) 

zip.cow <- MCMCglmm(VV_fruit ~ trait -1 + at.level(trait, 1):(fire*retention*micro_hab.two), 
                random = ~ idh(trait):site + idh(trait):nested_plot, rcov = ~idh(trait):units,
                data=zero.dat, family = "zipoisson",  nitt = nitt, 
                burnin = burnin, thin=thin, prior=prior, pr = TRUE, pl = TRUE)
summary(zip.cow)
# Gives a warning! And contrasts are not straight forward to interpret.

# Effect of vicinity of stumps on clearcuts
exp(summary(zip.cow)$solutions[6,1]) # ca 22 times
# Effect of vicinity of stumps on clearcuts after burning
exp(summary(zip.cow)$solutions[6,1] + summary(zip.cow)$solutions[9,1] - 
  summary(zip.cow)$solutions[3,1]*-1) #2.3 times

# compare unburned management treatments, unlogged vs clearcut
unVScl.ub <- (summary(zip.cow)$solutions[5,1]+ # unlogged flat
  0.5*(summary(zip.cow)$solutions[6,1] + summary(zip.cow)$solutions[11,1]) - #unlogged tree
    0.5*summary(zip.cow)$solutions[6,1]) # add tree habitat on clearcut
exp(unVScl.ub*-1) # minus 1 to get how much more berries on clearcuts
# ca 80 times

# compare burned management treatments, clearcuts vs retention vs unlogged
# clearcut vs retention
clVSret.ci <- zip.cow$Sol[,4] + zip.cow$Sol[,7] + # retention flat burned
  0.5*(zip.cow$Sol[,6] + zip.cow$Sol[,9] + zip.cow$Sol[,10] +
         zip.cow$Sol[,12]) - # near stumps retention. 0.5 to get the mean over both microhabitats
  0.5*(zip.cow$Sol[,6] + zip.cow$Sol[,9])  # mean for clearcuts
exp(mean(clVSret.ci)) #mean
exp(HPDinterval(clVSret.ci)) #CI
# 4.4 times more in burned retention than burned clearcuts

# clearcut vs unlogged
clVSun <- summary(zip.cow)$solutions[5,1] + summary(zip.cow)$solutions[8,1] + # unlogged flat burned
      0.5*(summary(zip.cow)$solutions[6,1] + summary(zip.cow)$solutions[9,1] + summary(zip.cow)$solutions[11,1] +
             summary(zip.cow)$solutions[13,1]) - # near stumps unlogged. 0.5 to get the mean over both microhabitats
      0.5*(summary(zip.cow)$solutions[6,1] + summary(zip.cow)$solutions[9,1])  # mean for clearcuts
exp(clVSun)
# 64% decrease

# unlogged vs retention
exp(clVSun*-1 + clVSret)
# 12 times

# Controll for plant cover with offset
prior = list( B = list(mu = matrix(c(0,0,0,0,0,0,1,0,0,0,0,0,0,0),14),V = diag(14)*(10)),
              R = list(V = diag(2), nu = 0.002, fix = 2), 
              G=list(G1=list(V=diag(2)*c(1,0.001), nu=0.002, fix=2),
                     G2=list(V=diag(2)*c(1,0.001), nu=0.002, fix=2)))
diag(prior$B$V)[7]<-1e-9
zip.cow.off <- MCMCglmm(VV_fruit ~ trait -1 + at.level(trait, 1):(fire*retention*micro_hab.two+log(VV_cover+0.01)), 
                    random = ~ idh(trait):site + idh(trait):nested_plot, rcov = ~idh(trait):units,
                    data=zero.dat, family = "zipoisson",  nitt = nitt, prior=prior,
                    burnin = burnin, thin=thin, pr = TRUE, pl = TRUE)
summary(zip.cow.off)
# Gives a warning!! And contrasts are not straight forward to interpret.


# Fruits sub-tests: cowberry####
# Question: Is the effect of logging different if trees are retained in groups?
# Compare logged areas in Retention treatment with the logged treatment
# CUT - RETENTION - NO CUT
logg.sub <- zero.dat[zero.dat$type == "open",]
logg.sub <- droplevels(logg.sub)

zip.logg.cow.com <- MCMCglmm(VV_fruit ~ trait -1 + at.level(trait, 1):(fire*retention), 
                random = ~ idh(trait):nested_plot + idh(trait):site, rcov = ~idh(trait):units,
                data=logg.sub, family = "zipoisson",  nitt = nitt, 
                burnin = burnin, thin=thin, prior=prior, pr = TRUE, pl = TRUE)
summary(zip.logg.cow.com)
summary(zip.logg.cow.com2)

# Plot sub-logg model
raw.means <- aggregate(VV_fruit ~ fire*retention*micro_hab.two, logg.sub, mean)
newDat <- raw.means[,1:4] 
X <- model.matrix(formula(~ fire*retention*micro_hab.two),
                  newDat)
coefs <- zip.logg.cow.com$Sol[,3:9]
coefs[,1] <- coefs[,1]*-1
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")
raw.means[1,6:7] <- as.numeric(HPDinterval(zip.logg.cow.com$Sol[,1]) - mean(zip.logg.cow.com$Sol[,1]))

raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
raw.means$order <- c(1, 3, 5, 7, 2, 4, 6, 8)

# rearrange
raw.means$vars <- factor(raw.means$vars, levels = raw.means$vars[order(raw.means$order)])
raw.means$vars.plot <- factor(paste(raw.means[,2], raw.means[,3], sep = "+"))

cow.fruit.logg.p <- ggplot(raw.means[,], aes(x=vars.plot, y=eff, fill=fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0,position=position_dodge(width=0.5)) +
  geom_point(size=4, pch=21,position=position_dodge(width=0.5)) +
  #ylim(c(-4.5, 8)) +
  xlab("") +
  ylab("log rate ratio") +
  theme(axis.text.x  = element_text(size=14, color="black"),
        axis.text.y  = element_text(size=14, color="black"),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.2,0.8),
        legend.key = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = seq(-5,9, by=2), limits=c(-4.5,9))+
  scale_x_discrete(labels=c("flat", "tree","flat", "tree"))+
  annotate("text", label = "a)", y = -4.5, x = 4.3, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("white", "black"),
                    labels = c("unburned", "burned"))+
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  annotation_custom(grob = textGrob(label=bquote(underline("     clearcut    ")), gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -6.7, ymax = -6.7) +
  annotation_custom(grob = textGrob(label=bquote(underline("retention (cut)")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -6.7, ymax = -6.7)

cow.fruit.logg.p <- ggplotGrob(cow.fruit.logg.p )
cow.fruit.logg.p $layout[grepl("panel", cow.fruit.logg.p $layout$name), ]$clip <- "off"
#grid.draw(cow.fruit.logg.p)

  #cow.fruit.logg.p <- ggplot(raw.means[,], aes(x=vars, y=eff, fill=fire)) + 
  #  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  #  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
  #               lwd=0.7, width=0) +
  # geom_point(size=4, pch=21) +
  # ylim(c(-4.5, 8)) +
  # xlab("") +
  # ylab("log rate ratio") +
  # theme(axis.text.x  = element_text(size=14),
  #       axis.text.y  = element_text(size=14),
  #       axis.title = element_text(size=14),
  #       legend.text = element_text(size=14),
  #       legend.title = element_blank(),
  #       legend.position= c(0.2,0.8)) +
  # annotate("text", label = "a)", y = -4.5, x = 8, size = 10) +
  # coord_flip() +
  # scale_fill_manual(values=c("black", "red"),
  #                   labels = c("unburned", "burned"))+
  # guides(fill=guide_legend(reverse=TRUE), 
  #        colour=guide_legend(reverse=TRUE))

# Question: Are retained tree groups similar to intact forest?
# Compare intact areas in the Retention treatment with intact forest
full.sub <- zero.dat[zero.dat$type == "gr",]
full.sub <- droplevels(full.sub)

zip.full.cow.com <- MCMCglmm(VV_fruit ~ trait -1 + at.level(trait, 1):(fire*retention*micro_hab.two), 
                         random = ~ idh(trait):nested_plot + idh(trait):site, rcov = ~idh(trait):units,
                         data=full.sub, family = "zipoisson",  nitt = nitt, 
                         burnin = burnin, thin=thin, prior=prior, pr = TRUE, pl = TRUE)
summary(zip.full.cow.com)

# Plot sub-full model
raw.means <- aggregate(VV_fruit ~ fire*retention*micro_hab.two, full.sub, mean)
newDat <- raw.means[,1:4] 
X <- model.matrix(formula(~ fire*retention*micro_hab.two),
                  newDat)
coefs <- zip.full.cow.com$Sol[,3:9]
coefs[,1] <- coefs[,1]*-1
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")
raw.means[1,6:7] <- as.numeric(HPDinterval(zip.full.cow.com$Sol[,1]) - mean(zip.full.cow.com$Sol[,1]))

raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
raw.means$order <- c(1, 3, 5, 7, 2, 4, 6, 8)

# rearrange
raw.means$vars <- factor(raw.means$vars, levels = raw.means$vars[order(raw.means$order)])
raw.means$vars.plot <- factor(paste(raw.means[,2], raw.means[,3], sep = "+"))

cow.fruit.full.p <- ggplot(raw.means[,], aes(x=vars.plot, y=eff, fill=fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0,position=position_dodge(width=0.5)) +
  geom_point(size=4, pch=21,position=position_dodge(width=0.5)) +
  xlab("") +
  ylab("log rate ratio") +
  theme(axis.text.x  = element_text(size=14, color="black"),
        axis.text.y  = element_text(size=14, color="black"),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.8,0.8),
        legend.key = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = seq(-7,5, by=2), limits=c(-7, 5))+
  scale_x_discrete(labels=c("flat", "tree","flat", "tree"))+
  annotate("text", label = "b)", y = -7, x = 4.3, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("white", "black"),
                    labels = c("unburned", "burned"))+
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE))  +
  annotation_custom(grob = textGrob(label=bquote(underline(retention(patch))), gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -8.9, ymax = -8.9) +
  annotation_custom(grob = textGrob(label=bquote(underline("     unlogged    ")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -8.9, ymax = -8.9)

cow.fruit.full.p <- ggplotGrob(cow.fruit.full.p)
cow.fruit.full.p$layout[grepl("panel", cow.fruit.full.p$layout$name), ]$clip <- "off"
#grid.draw(cow.fruit.full.p)

# cow.fruit.full.p <- ggplot(raw.means, aes(x=vars, y=eff, fill=fire)) + 
#   geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
#   geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
#                 lwd=0.7, width=0) +
#   geom_point(size=4, pch=21) +
#   ylim(c(-7.5, 5))  +
#   xlab("") +
#   ylab("Odds ratio") +
#   theme(axis.text.x  = element_text(size=14),
#         axis.text.y  = element_text(size=14),
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=14),
#         legend.title = element_blank(),
#         legend.position= c(0.8,0.8)) +
#   annotate("text", label = "a)", y = -7.5, x = 8, size = 10) +
#   coord_flip() +
#   scale_fill_manual(values=c("black", "red"),
#                     labels = c("unburned", "burned"))+
#   guides(fill=guide_legend(reverse=TRUE), 
#          colour=guide_legend(reverse=TRUE))

# Bilberry fruits ####

#zip model
nitt = 80000 #low for testing
thin = 20 #low for testing
burnin = 5000 #low for testing
prior = list( R = list(V = diag(2), nu = 0.002, fix = 2), 
              G=list(G1=list(V=diag(2)*c(1,0.001), nu=0.002, fix=2),
                     G2=list(V=diag(2)*c(1,0.001), nu=0.002, fix=2))) 

zip.bil <- MCMCglmm(VM_fruit ~ trait -1 + at.level(trait, 1):(fire*retention*micro_hab.two), 
                    random = ~ idh(trait):site + idh(trait):nested_plot, rcov = ~idh(trait):units,
                    data=zero.dat, family = "zipoisson",  nitt = nitt, 
                    burnin = burnin, thin=thin, 
                    prior=c(prior,list(B=list(mu=rep(0,13), V=diag(9,13)))), 
                    pr = TRUE, pl = TRUE)
summary(zip.bil)
# Gives a warning!! And contrasts are not straight forward to interpret.

# Controll for plant cover with offset
prior = list( B = list(mu = matrix(c(0,0,0,0,0,0,1,0,0,0,0,0,0,0),14),V = diag(14)*(10)),
              R = list(V = diag(2), nu = 0.002, fix = 2), 
              G=list(G1=list(V=diag(2)*c(1,0.001), nu=0.002, fix=2),
                     G2=list(V=diag(2)*c(1,0.001), nu=0.002, fix=2)))
diag(prior$B$V)[7]<-1e-9
zip.bil.off <- MCMCglmm(VM_fruit ~ trait -1 + at.level(trait, 1):(fire*retention*micro_hab.two+log(VM_cover+0.01)), 
                        random = ~ idh(trait):site + idh(trait):nested_plot, rcov = ~idh(trait):units,
                        data=zero.dat, family = "zipoisson",  nitt = nitt, prior=prior,
                        burnin = burnin, thin=thin, pr = TRUE, pl = TRUE)
summary(zip.bil.off)


# Poisson model bilberry
prior = list( R = list(V = diag(1), nu = 0.002), 
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2),
                     G2=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2)) )
pois.bil <- MCMCglmm(VM_fruit ~ fire*retention*micro_hab.two, 
                     random = ~ site + nested_plot,
                     data=zero.dat, family = "poisson",  nitt = nitt, 
                     burnin = burnin, thin=thin, 
                     prior= c(prior,list(B=list(mu=rep(0,12),
                                                V=diag(9,12))))
                     , pr = TRUE, pl = TRUE)
summary(pois.bil)

# Poisson model 
oz <- sum(zero.dat$VM_fruit == 0)
sim.pois <- simulate(pois.bil, type="response", posterior = "mean", nsim=1000)
dist.zeros.pois <- apply(sim.pois, 2, function (x) sum(x==0))
hist(dist.zeros.pois)
abline(v = oz, col = "red")
# not much Zero-inflated

p.zip <- predict(zip.bil,   type="response", posterior = "mean")
p.zap <- predict(zap,  type="response", posterior = "mean")
p.pois <- predict(pois.bil,   type="response", posterior = "mean")
cbind(aggregate(VM_fruit ~ fire*retention*micro_hab.two, zero.dat, mean),
      zip = aggregate(p.zip ~ fire*retention*micro_hab.two, zero.dat, mean)$V1,
      #zap = aggregate(p.zap ~ X1*X2*X3_nest, zero.dat, mean)$V1,
      pois = aggregate(p.pois ~ fire*retention*micro_hab.two, zero.dat, mean)$V1)
# poissoon model give extreme (unrealistic) values. zap best I think.

# Fruits sub-tests: bilberry####
# Question: Is the effect of logging different if trees are retained in groups?
# Compare logged areas in Retention treatment with the logged treatment
# CUT - RETENTION - NO CUT
logg.sub <- zero.dat[zero.dat$type == "open",]
logg.sub <- droplevels(logg.sub)

zip.logg.bil.com <- MCMCglmm(VM_fruit ~ trait -1 + at.level(trait, 1):(fire*retention*micro_hab.two), 
                         random = ~ idh(trait):site + idh(trait):nested_plot,  rcov = ~idh(trait):units,
                         data=logg.sub, family = "zipoisson",  nitt = nitt, 
                         burnin = burnin, thin=thin, 
                         prior=c(prior,list(B=list(mu=rep(0,9), V=diag(9,9)))), 
                         pr = TRUE, pl = TRUE)
summary(zip.logg.bil.com)
# effect of stumps on logged retention plots (retention-L)
exp(summary(zip.logg.bil.com)$solutions[c(8),1])
# ca 45 times

# Plot sub-logg model
raw.means <- aggregate(VV_fruit ~ fire*retention*micro_hab.two, logg.sub, mean)
newDat <- raw.means[,1:4] 
X <- model.matrix(formula(~ fire*retention*micro_hab.two),
                  newDat)
coefs <- zip.logg.bil.com$Sol[,3:9]
coefs[,1] <- coefs[,1]*-1
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")
raw.means[1,6:7] <- as.numeric(HPDinterval(zip.logg.bil.com$Sol[,1]) - mean(zip.logg.bil.com$Sol[,1]))

raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
raw.means$order <- c(1, 3, 5, 7, 2, 4, 6, 8)

# rearrange
raw.means$vars <- factor(raw.means$vars, levels = raw.means$vars[order(raw.means$order)])
raw.means$vars.plot <- factor(paste(raw.means[,2], raw.means[,3], sep = "+"))

bil.fruit.logg.p <- ggplot(raw.means[,], aes(x=vars.plot, y=eff, fill=fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0,position=position_dodge(width=0.5)) +
  geom_point(size=4, pch=21,position=position_dodge(width=0.5)) +
  #ylim(c(-4.5, 8)) +
  xlab("") +
  ylab("log rate ratio") +
  theme(axis.text.x  = element_text(size=14, color="black"),
        axis.text.y  = element_text(size=14, color="black"),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.2,0.8),
        legend.key = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = seq(-5,9, by=2), limits=c(-4.5,9))+
  scale_x_discrete(labels=c("flat", "tree","flat", "tree"))+
  annotate("text", label = "c)", y = -4.5, x = 4.3, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("white", "black"),
                    labels = c("unburned", "burned"))+
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  annotation_custom(grob = textGrob(label=bquote(underline("     clearcut    ")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -6.7, ymax = -6.7) +
  annotation_custom(grob = textGrob(label=bquote(underline("retention (cut)")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -6.7, ymax = -6.7)

bil.fruit.logg.p <- ggplotGrob(bil.fruit.logg.p)
bil.fruit.logg.p$layout[grepl("panel", bil.fruit.logg.p$layout$name), ]$clip <- "off"
#grid.draw(bil.fruit.logg.p)

# bil.fruit.logg.p <- ggplot(raw.means, aes(x=vars, y=eff, fill=fire)) + 
#   geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
#   geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
#                 lwd=0.7, width=0) +
#   geom_point(size=4, pch=21) +
#   ylim(c(-4.5, 8)) +
#   xlab("") +
#   ylab("log rate ratio") +
#   theme(axis.text.x  = element_text(size=14),
#         axis.text.y  = element_text(size=14),
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=14),
#         legend.title = element_blank(),
#         legend.position= c(0.8,0.2)) +
#   annotate("text", label = "b)", y = -4.5, x = 8, size = 10) +
#   coord_flip() +
#   scale_fill_manual(values=c("black", "red"),
#                     labels = c("unburned", "burned"))+
#   guides(fill=guide_legend(reverse=TRUE), 
#          colour=guide_legend(reverse=TRUE))

# Question: Are retained tree groups similar to intact forest?
# Compare intact areas in the Retention treatment with intact forest
full.sub <- zero.dat[zero.dat$type == "gr",]
full.sub <- droplevels(full.sub)

zip.full.bil.com <- MCMCglmm(VM_fruit ~ trait -1 + at.level(trait, 1):(fire*retention*micro_hab.two), 
                         random = ~ idh(trait):site + idh(trait):nested_plot, rcov = ~idh(trait):units,
                         data=full.sub, family = "zipoisson",  nitt = 150000, 
                         burnin = burnin, thin=thin, prior=prior, pr = TRUE, pl = TRUE)
summary(zip.full.bil.com)

# effect of prescribed burning differ between retention-P and unlogged
# retention-P: no interaction with microhabitat so we use the coef
# for falt ground
exp(summary(zip.full.bil.com)$solutions[c(3),1]*(-1)) #-94% 

# unlogged: no significant interactions so use the the effect for 
# flat ground again.
exp(summary(zip.full.bil.com)$solutions[c(3),1]*(-1) + 
      summary(zip.full.bil.com)$solutions[6,1]) #+226% 


# Plot sub-full model
raw.means <- aggregate(VM_fruit ~ fire*retention*micro_hab.two, full.sub, mean)
newDat <- raw.means[,1:4] 
X <- model.matrix(formula(~ fire*retention*micro_hab.two),
                  newDat)
coefs <- zip.full.bil.com$Sol[,3:9]
coefs[,1] <- coefs[,1]*-1
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")
raw.means[1,6:7] <- as.numeric(HPDinterval(zip.full.bil.com$Sol[,1]) - mean(zip.full.bil.com$Sol[,1]))

raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
raw.means$order <- c(1, 3, 5, 7, 2, 4, 6, 8)

# rearrange
raw.means$vars <- factor(raw.means$vars, levels = raw.means$vars[order(raw.means$order)])
raw.means$vars.plot <- factor(paste(raw.means[,2], raw.means[,3], sep = "+"))

bil.fruit.full.p <- ggplot(raw.means[,], aes(x=vars.plot, y=eff, fill=fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0,position=position_dodge(width=0.5)) +
  geom_point(size=4, pch=21,position=position_dodge(width=0.5)) +
  #xlab("  retention (patch)               unlogged      ") +
  xlab("")+
  ylab("log rate ratio") +
  theme(axis.text.x  = element_text(size=14, color="black"),
        axis.text.y  = element_text(size=14, color="black"),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.2,0.8),
        legend.key = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = seq(-7,5, by=2), limits=c(-7, 5))+
  scale_x_discrete(labels=c("flat", "tree","flat", "tree"))+
  annotate("text", label = "d)", y = -7, x = 4.3, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("white", "black"),
                    labels = c("unburned", "burned")) +
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  annotation_custom(grob = textGrob(label=bquote(underline(retention(patch))), 
                                    gp=gpar(fontsize=16),rot=90), 
                  xmin = 0.75, xmax = 2.25, ymin = -8.9, ymax = -8.9) +
  annotation_custom(grob = textGrob(label=bquote(underline("     unlogged    ")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -8.9, ymax = -8.9)

bil.fruit.full.p <- ggplotGrob(bil.fruit.full.p)
bil.fruit.full.p$layout[grepl("panel", bil.fruit.full.p$layout$name), ]$clip <- "off"

# bil.full.p <- ggplot(raw.means[,], aes(x=vars, y=eff, fill=fire)) + 
#   geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
#   geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
#                 lwd=0.7, width=0) +
#   geom_point(size=4, pch=21) +
#   ylim(c(-7.5, 5)) +
#   xlab("") +
#   ylab("Odds ratio") +
#   theme(axis.text.x  = element_text(size=14),
#         axis.text.y  = element_text(size=14),
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=14),
#         legend.title = element_blank(),
#         legend.position= c(0.2,0.8)) +
#   annotate("text", label = "b)", y = -7.5, x = 8, size = 10) +
#   coord_flip() +
#   scale_fill_manual(values=c("black", "red"),
#                     labels = c("unburned", "burned"))+
#   guides(fill=guide_legend(reverse=TRUE), 
#          colour=guide_legend(reverse=TRUE))

# Combine sub-test fruit plots
fig4.g1 <- arrangeGrob(cow.fruit.logg.p)
fig4.g2 <- arrangeGrob(cow.fruit.full.p)
fig4.g3 <- arrangeGrob(bil.fruit.logg.p)
fig4.g4 <- arrangeGrob(bil.fruit.full.p)
png("figure4_logg_full_comp_fruit.png", width=32, height=30, units="cm", res=300)
grid.arrange(fig4.g1, fig4.g2, fig4.g3, fig4.g4,
             ncol=2, nrow =2)
dev.off()


# Logg vs logg test
png("SM_figure1_logg_comp_fruit_plot.png", width=15, height=30, units="cm", res=300)
grid.arrange(cow.fruit.logg.p, bil.fruit.logg.p, ncol=1, nrow =2)
dev.off()

# Full vs full test
png("SM_figure2_full_comp_fruit_plot.png", width=15, height=30, units="cm", res=300)
grid.arrange(cow.fruit.full.p, bil.full.p, ncol=1, nrow =2)
dev.off()


# Fruit effect plots ####

# cowberry
raw.means <- aggregate(VV_fruit ~ fire*retention*micro_hab.two, zero.dat, mean)
newDat <- raw.means[,1:4] 
X <- model.matrix(formula(~ fire*retention*micro_hab.two),
                  newDat)
coefs <- zip.cow$Sol[,3:13]
coefs[,1] <- coefs[,1]*-1
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")

raw.means[1,6:7] <- as.numeric(HPDinterval(zip.cow$Sol[,1]) - mean(zip.cow$Sol[,1]))

#raw.means$vars <- c("No-full-Out", "Yes-full-Out","No-medium-Out", "Yes-medium-Out", "No-No-Out", "Yes-No-Out",
#                    "No-Full-Tree", "Yes-Full-Tree", "No-Medium-Tree", "Yes-Medium-Tree", "No-No-Tree", "Yes-No-Tree")

raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
raw.means$order <- c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)

# rearrange
raw.means$vars <- factor(raw.means$vars, levels = raw.means$vars[order(raw.means$order)])
raw.means$vars.plot <- factor(paste(raw.means[,2], raw.means[,3], sep = "+"))

cow.fruit.p <- ggplot(raw.means[,], aes(x=vars.plot, y=eff, fill=fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0,position=position_dodge(width=0.5)) +
  geom_point(size=4, pch=21,position=position_dodge(width=0.5)) +
  #ylim(c(-4.5, 8)) +
  xlab("") +
  ylab("log rate ratio") +
  theme(axis.text.x  = element_text(size=14, color="black"),
        axis.text.y  = element_text(size=14, color="black"),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.8,0.8),
        legend.key = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = seq(-6,9, by=2), limits=c(-5,9))+
  scale_x_discrete(labels=c("flat", "tree","flat", "tree","flat", "tree")) +
  annotate("text", label = "b)", y = -4.8, x = 6.3, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("white", "black"),
                    labels = c("unburned", "burned"))+
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  annotation_custom(grob = textGrob(label=bquote(underline("  clearcut  ")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -7.2, ymax = -7.2) +
  annotation_custom(grob = textGrob(label=bquote(underline("retention")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -7.2, ymax = -7.2) +
  annotation_custom(grob = textGrob(label=bquote(underline("unlogged")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 4.75, xmax = 6.25, ymin = -7.2, ymax = -7.2)

cow.fruit.p <- ggplotGrob(cow.fruit.p)
cow.fruit.p$layout[grepl("panel", cow.fruit.p$layout$name), ]$clip <- "off"
#grid.draw(cow.fruit.p)


# cow.fruit.p <- ggplot(raw.means[,], aes(x=vars, y=eff, fill=fire)) + 
#   geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
#   geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
#                 lwd=0.7, width=0) +
#   geom_point(size=4, pch=21) +
#   ylim(c(-6, 9)) +
#   xlab("") +
#   ylab("Odds ratio") +
#   theme(axis.text.x  = element_text(size=14),
#         axis.text.y  = element_text(size=14),
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=14),
#         legend.title = element_blank(),
#         legend.position= c(0.8,0.8)) +
#   annotate("text", label = "a)", y = -6, x = 12, size = 10) +
#   coord_flip() +
#   scale_fill_manual(values=c("black", "red"),
#                     labels = c("unburned", "burned"))+
#   guides(fill=guide_legend(reverse=TRUE), 
#          colour=guide_legend(reverse=TRUE))


# Bilberry
raw.means <- aggregate(VM_fruit ~ fire*retention*micro_hab.two, zero.dat, mean)
newDat <- raw.means[,1:4] 
X <- model.matrix(formula(~ fire*retention*micro_hab.two),
                  newDat)
coefs <- zip.bil$Sol[,c(3:13)]
coefs[,1] <- coefs[,1]*-1
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")

raw.means[1,6:7] <- as.numeric(HPDinterval(zip.bil$Sol[,1]) - mean(zip.bil$Sol[,1]))

#raw.means$vars <- c("No-full-Out", "Yes-full-Out","No-medium-Out", "Yes-medium-Out", "No-No-Out", "Yes-No-Out",
#                    "No-Full-Tree", "Yes-Full-Tree", "No-Medium-Tree", "Yes-Medium-Tree", "No-No-Tree", "Yes-No-Tree")

raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
raw.means$order <- c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)

# rearrange
raw.means$vars <- factor(raw.means$vars, levels = raw.means$vars[order(raw.means$order)])
raw.means$vars.plot <- factor(paste(raw.means[,2], raw.means[,3], sep = "+"))

bil.fruit.p <- ggplot(raw.means[,], aes(x=vars.plot, y=eff, fill=fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0,position=position_dodge(width=0.5)) +
  geom_point(size=4, pch=21,position=position_dodge(width=0.5)) +
  #ylim(c(-4.5, 8)) +
  xlab("") +
  ylab("log rate ratio") +
  theme(axis.text.x  = element_text(size=14, color="black"),
        axis.text.y  = element_text(size=14, color="black"),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.8,0.2),
        legend.key = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = seq(-6,9, by=2), limits=c(-5,9))+
  scale_x_discrete(labels=c("flat", "tree","flat", "tree","flat", "tree")) +
  annotate("text", label = "d)", y = -4.8, x = 6.3, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("white", "black"),
                    labels = c("unburned", "burned"))+
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  annotation_custom(grob = textGrob(label=bquote(underline("  clearcut  ")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -7.2, ymax = -7.2) +
  annotation_custom(grob = textGrob(label=bquote(underline("retention")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -7.2, ymax = -7.2) +
  annotation_custom(grob = textGrob(label=bquote(underline("unlogged")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 4.75, xmax = 6.25, ymin = -7.2, ymax = -7.2)

bil.fruit.p <- ggplotGrob(bil.fruit.p)
bil.fruit.p$layout[grepl("panel", bil.fruit.p$layout$name), ]$clip <- "off"
#grid.draw(bil.fruit.p)

# bil.fruit.p <- ggplot(raw.means, aes(x=vars, y=eff, fill=fire)) + 
#   geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
#   geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
#                 lwd=0.7, width=0) +
#   geom_point(size=4, pch=21) +
#   ylim(c(-6, 9)) +
#   xlab("") +
#   ylab("Odds ratio") +
#   theme(axis.text.x  = element_text(size=14),
#         axis.text.y  = element_text(size=14),
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=14),
#         legend.title = element_blank(),
#         legend.position= c(0.8,0.2)) +
#   annotate("text", label = "b)", y = -6, x = 12, size = 10) +
#   coord_flip() +
#   scale_fill_manual(values=c("black", "red"),
#                     labels = c("unburned", "burned")) +
#   guides(fill=guide_legend(reverse=TRUE), 
#          colour=guide_legend(reverse=TRUE))


#bilberry offset model
raw.means <- aggregate(VM_fruit ~ fire*retention*micro_hab.two, zero.dat, mean)
newDat <- raw.means[,1:4] 
X <- model.matrix(formula(~ fire*retention*micro_hab.two),
                  newDat)
coefs <- zip.bil.off$Sol[,c(3:6,8:14)]
coefs[,1] <- coefs[,1]*-1
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")

raw.means[1,6:7] <- as.numeric(HPDinterval(zip.bil$Sol[,1]) - mean(zip.bil$Sol[,1]))

#raw.means$vars <- c("No-full-Out", "Yes-full-Out","No-medium-Out", "Yes-medium-Out", "No-No-Out", "Yes-No-Out",
#                    "No-Full-Tree", "Yes-Full-Tree", "No-Medium-Tree", "Yes-Medium-Tree", "No-No-Tree", "Yes-No-Tree")

raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
raw.means$order <- c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)

# rearrange
raw.means$vars <- factor(raw.means$vars, levels = raw.means$vars[order(raw.means$order)])

bil.fruit.p <- ggplot(raw.means, aes(x=vars, y=eff, fill=fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0) +
  geom_point(size=4, pch=21) +
  ylim(c(-6, 9)) +
  xlab("") +
  ylab("Odds ratio") +
  theme(axis.text.x  = element_text(size=14),
        axis.text.y  = element_text(size=14),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.8,0.2)) +
  annotate("text", label = "b)", y = -6, x = 12, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("black", "red"),
                    labels = c("unburned", "burned")) +
  guides(fill=guide_legend(reverse=TRUE), 
         colour=guide_legend(reverse=TRUE))




zip.bil <- MCMCglmm(VM_fruit ~ trait -1 + at.level(trait, 1):(fire*retention*micro_hab.two), 
                random = ~ idh(trait):nested_plot + idh(trait):site, rcov = ~idh(trait):units,
                data=zero.dat, family = "zipoisson",  nitt = nitt, 
                burnin = burnin, thin=thin, prior=prior, pr = TRUE, pl = TRUE)
summary(zip)


#zap model
prior = list( R = list(V = diag(2), nu = 0.002, fix = 2), 
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 
zap <- MCMCglmm(y ~ trait*(X1*X2*X3_nest), 
                    random = ~ nested_plot, rcov = ~idh(trait):units,
                    data=zero.dat, family = "zapoisson",  nitt = nitt, 
                    burnin = burnin, thin=thin,  prior=prior, pr = TRUE, pl = TRUE)
summary(zap)
# No warning and everything looks good

# Poisson model
prior = list( R = list(V = diag(1), nu = 0.002), 
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 
pois <- MCMCglmm(y ~ X1*X2*X3_nest, 
                random = ~ nested_plot,
                data=zero.dat, family = "poisson",  nitt = nitt, 
                burnin = burnin, thin=thin,  prior=prior, pr = TRUE, pl = TRUE)
summary(pois)
# No warning

# Some predictions
# by default, all random factors are marginalised
p.zip <- predict(zip,   type="response", posterior = "mean")
p.zap <- predict(zap,  type="response", posterior = "mean")
p.pois <- predict(pois,   type="response", posterior = "mean")
cbind(aggregate(y ~ X1*X2*X3_nest, zero.dat, mean),
      zip = aggregate(p.zip ~ X1*X2*X3_nest, zero.dat, mean)$V1),
      zap = aggregate(p.zap ~ X1*X2*X3_nest, zero.dat, mean)$V1,
      pois = aggregate(p.pois ~ X1*X2*X3_nest, zero.dat, mean)$V1)
# poissoon model give extreme (unrealistic) values. zap best I think.


# Predictive testing: zeros

# zip model
oz <- sum(zero.dat == 0)
sim.zi <- simulate(zip, type="response", posterior = "all", nsim=1000)
dist.zeros.zi <- apply(sim.zi, 2, function (x) sum(x==0))
hist(dist.zeros.zi)
abline(v = oz, col = "red")

# zap model
oz <- sum(zero.dat == 0)
sim.za <- simulate(zap, type="response", posterior = "mean", nsim=1000)
dist.zeros.za <- apply(sim.za, 2, function (x) sum(x==0))
hist(dist.zeros.za)
abline(v = oz, col = "red")
# very good prediction of zeros!

# Poisson model 
oz <- sum(zero.dat == 0)
sim.pois <- simulate(pois, type="response", posterior = "mean", nsim=1000)
dist.zeros.pois <- apply(sim.pois, 2, function (x) sum(x==0))
hist(dist.zeros.pois, xlim=c(900,1250))
abline(v = oz, col = "red")
# Zero-inflated!

# Simulation from pois model, by hand.
mc.samp <- nrow(pois$VCV)
nz <- 1:mc.samp
oz <- sum(zero.dat == 0)
for (i in 1:mc.samp) {
  pred.l <- rnorm(nrow(zero.dat), (cbind(pois$X,pois$Z) %*% pois$Sol[1,])@x, sqrt(pois$VCV[i,]))
  nz[i] <- sum(rpois(nrow(zero.dat), exp(pred.l)) == 0)
}
hist(nz, xlim=c(900,1250))
abline(v = oz, col = "red")


# plot zero distributions
zeroDens <- data.frame(y = c(dist.zeros.zi, dist.zeros.za, dist.zeros.pois, sample(nz,1000)),
                       model = rep(c("zi", "za", "pois", "pois.byHand"), each=1000))
library(ggplot2)
ggplot(zeroDens,aes(x=y, fill=model)) + geom_density(alpha=0.25) + geom_vline(xintercept=oz)
# only ZAP that captures all the zeros. Zip not that bad though.

# test
#library(lme4)
#summary(lmer(y ~ X1*X2*X3_nest + (1|nested_plot), zero.dat))

raw.means <- aggregate(y ~ X1+X2+X3_nest, zero.dat, mean)
newDat <- raw.means[,1:4] 
#newDat$latitude.s <- 0
X <- model.matrix(formula(~ X1*X2*X3_nest),
                  newDat)
coefs <- zip$Sol[,3:13]
coefs[,1] <- coefs[,1]*-1
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")
#lat.eff <- summary(bil.cov.mc)$solutions[6,1:3] #latitude effect
#names(lat.eff) <- colnames(raw.means)[6:8]
#raw.means <- rbind(raw.means, c(raw.means[10,1:5], lat.eff) )
#raw.means <- raw.means[c(1,3,5,7,9,11),]
#raw.means$vars <- c("control", "Thin","N", "Thin+N", "Thin+N+P", "Latitude")

raw.means[1,6:7] <- as.numeric(HPDinterval(zip$Sol[,1]) - mean(zip$Sol[,1]))

raw.means$vars <- c("No-full-Out", "Yes-full-Out","No-medium-Out", "Yes-medium-Out", "No-No-Out", "Yes-No-Out",
                    "No-Full-Tree", "Yes-Full-Tree", "No-Medium-Tree", "Yes-Medium-Tree", "No-No-Tree", "Yes-No-Tree")
raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
library(ggplot2)
bil.fruit.p <- ggplot(raw.means[,], aes(x=vars, y=eff, fill = fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0) +
  geom_point(size=4, pch=21) +
  #ylim(c(-30, 30)) +
  xlab("") +
  ylab("Odds ratio") +
  theme(axis.text.x  = element_text(size=14),
        axis.text.y  = element_text(size=14),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.8,0.2)) +
  annotate("text", label = "b)", y = -3.5, x = 12, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("black", "red"),
                    labels = c("unburned", "burned"))+
  guides(fill=guide_legend(reverse=TRUE), 
         colour=guide_legend(reverse=TRUE))




# Subset model cowberry
zip.cow <- zip
raw.means <- aggregate(VV_fruit ~ fire*retention*micro_hab.two, zero.dat, mean)
newDat <- raw.means[,1:4] 
X <- model.matrix(formula(~ fire*retention*micro_hab.two),
                  newDat)
coefs <- zip.cow$Sol[,3:13]
coefs[,1] <- coefs[,1]*-1
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")

raw.means[1,6:7] <- as.numeric(HPDinterval(zip.cow$Sol[,1]) - mean(zip.cow$Sol[,1]))

#raw.means$vars <- c("No-full-Out", "Yes-full-Out","No-medium-Out", "Yes-medium-Out", "No-No-Out", "Yes-No-Out",
#                    "No-Full-Tree", "Yes-Full-Tree", "No-Medium-Tree", "Yes-Medium-Tree", "No-No-Tree", "Yes-No-Tree")

raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
raw.means$order <- c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)

# rearrange
raw.means$vars <- factor(raw.means$vars, levels = raw.means$vars[order(raw.means$order)])

library(ggplot2)
cow.fruit.p <- ggplot(raw.means[,], aes(x=vars, y=eff, fill=fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0) +
  geom_point(size=4, pch=21) +
  ylim(c(-7.5, 8)) +
  xlab("") +
  ylab("Odds ratio") +
  theme(axis.text.x  = element_text(size=14),
        axis.text.y  = element_text(size=14),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.8,0.8)) +
  annotate("text", label = "a)", y = -7.5, x = 12, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("black", "red"),
                    labels = c("unburned", "burned"))+
  guides(fill=guide_legend(reverse=TRUE), 
         colour=guide_legend(reverse=TRUE))




#### Veg cover ####

# cowberry
prior = list( R = list(V = diag(1), nu = 0.002), 
              #G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2),
                     G2=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 

cow.cov.mc <- MCMCglmm(cbind(VV_cover, total_cover-VV_cover)  ~ fire*retention*micro_hab.two, 
                       random = ~ site+nested_plot,
                       data=zero.dat, family = "multinomial2",  nitt = 80000, 
                       burnin = 15000, thin=25, pr = TRUE, pl = TRUE, prior=prior,
                       saveX = TRUE, saveZ = TRUE)
summary(cow.cov.mc)
plot(cow.cov.mc$VCV)

# Effect of stumps pn clearcuts on cowberry cover
exp(summary(cow.cov.mc)$solutions[5,1]) # 2.1 times increase near stumps

# bilberry
prior = list( R = list(V = diag(1), nu = 0.002), 
              #G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2),
                     G2=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 


bil.cov.mc <- MCMCglmm(cbind(VM_cover, total_cover-VM_cover)  ~ fire*retention*micro_hab.two, 
                       random = ~ site+nested_plot,
                       data=zero.dat, family = "multinomial2",  nitt = 180000, 
                       burnin = 15000, thin=25, pr = TRUE, pl = TRUE, prior=prior,
                       saveX = TRUE, saveZ = TRUE)
summary(bil.cov.mc)
plot(bil.cov.mc$VCV)

# Some predictions for plant cover
# by default, all random factors are marginalised
p.cow.cov <- predict(cow.cov.mc,   type="response", posterior = "mean")
p.bil.cov <- predict(bil.cov.mc,  type="response", posterior = "mean")
cbind(aggregate(VV_cover ~ fire*retention*micro_hab.two, zero.dat, mean),
      model.cowberry = aggregate(p.cow.cov ~ fire*retention*micro_hab.two, zero.dat, mean)$V1,
      VM_cover = aggregate(VM_cover ~ fire*retention*micro_hab.two, zero.dat, mean)$VM_cover,
      model.bilberry = aggregate(p.bil.cov ~ fire*retention*micro_hab.two, zero.dat, mean)$V1)
# looks pretty good!


# Plot effect on plant cover

# cowberry
raw.means <- aggregate(VV_cover ~ fire*retention*micro_hab.two, zero.dat, mean)
newDat <- raw.means[,1:4] 
X <- model.matrix(formula(~ fire*retention*micro_hab.two),
                  newDat)
coefs <- cow.cov.mc$Sol[,2:12]
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")

raw.means[1,6:7] <- as.numeric(HPDinterval(cow.cov.mc$Sol[,1]) - mean(cow.cov.mc$Sol[,1]))

#raw.means$vars <- c("No-full-Out", "Yes-full-Out","No-medium-Out", "Yes-medium-Out", "No-No-Out", "Yes-No-Out",
"No-Full-Tree", "Yes-Full-Tree", "No-Medium-Tree", "Yes-Medium-Tree", "No-No-Tree", "Yes-No-Tree")
raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
raw.means$order <- c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)

# rearrange
raw.means$vars <- factor(raw.means$vars, levels = raw.means$vars[order(raw.means$order)])
raw.means$vars.plot <- factor(paste(raw.means[,2], raw.means[,3], sep = "+"))

cow.cov.p  <- ggplot(raw.means[,], aes(x=vars.plot, y=eff, fill=fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0,position=position_dodge(width=0.5)) +
  geom_point(size=4, pch=21,position=position_dodge(width=0.5)) +
  #ylim(c(-4.5, 8)) +
  xlab("") +
  ylab("log odds ratio") +
  theme(axis.text.x  = element_text(size=14, color="black"),
        axis.text.y  = element_text(size=14, color="black"),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.8,0.8),
        legend.key = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = seq(-3,9, by=2), limits=c(-3,10))+
  scale_x_discrete(labels=c("flat", "tree","flat", "tree","flat", "tree")) +
  annotate("text", label = "a)", y = -3, x = 6.3, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("white", "black"),
                    labels = c("unburned", "burned"))+
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  annotation_custom(grob = textGrob(label=bquote(underline("  clearcut  ")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -5.0, ymax = -5.0) +
  annotation_custom(grob = textGrob(label=bquote(underline("retention")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -5.0, ymax = -5.0) +
  annotation_custom(grob = textGrob(label=bquote(underline("unlogged")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 4.75, xmax = 6.25, ymin = -5.0, ymax = -5.0)

cow.cov.p <- ggplotGrob(cow.cov.p)
cow.cov.p$layout[grepl("panel", cow.cov.p$layout$name), ]$clip <- "off"
#grid.draw(cow.cov.p)

# cow.cov.p <- ggplot(raw.means, aes(x=vars, y=eff, fill=fire)) + 
#   geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
#   geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
#                 lwd=0.7, width=0) +
#   geom_point(size=4, pch=21) +
#   ylim(c(-3, 10)) +
#   xlab("") +
#   ylab("Rate ratio") +
#   theme(axis.text.x  = element_text(size=14),
#         axis.text.y  = element_text(size=14),
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=14),
#         legend.title = element_blank(),
#         legend.position= c(0.8,0.8)) +
#   annotate("text", label = "a)", y = -3, x = 12, size = 10) +
#   coord_flip() +
#   scale_fill_manual(values=c("black", "red"),
#                     labels = c("unburned", "burned"))+
#   guides(fill=guide_legend(reverse=TRUE), 
#          colour=guide_legend(reverse=TRUE))

# bilberry
raw.means <- aggregate(VM_cover ~ fire*retention*micro_hab.two, zero.dat, mean)
newDat <- raw.means[,1:4] 
X <- model.matrix(formula(~ fire*retention*micro_hab.two),
                  newDat)
coefs <- bil.cov.mc$Sol[,2:12]
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")

raw.means[1,6:7] <- as.numeric(HPDinterval(bil.cov.mc$Sol[,1]) - mean(bil.cov.mc$Sol[,1]))

#raw.means$vars <- c("No-full-Out", "Yes-full-Out","No-medium-Out", "Yes-medium-Out", "No-No-Out", "Yes-No-Out",
#                    "No-Full-Tree", "Yes-Full-Tree", "No-Medium-Tree", "Yes-Medium-Tree", "No-No-Tree", "Yes-No-Tree")

raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
raw.means$order <- c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)

# rearrange
raw.means$vars <- factor(raw.means$vars, levels = raw.means$vars[order(raw.means$order)])
raw.means$vars.plot <- factor(paste(raw.means[,2], raw.means[,3], sep = "+"))

bil.cov.p  <- ggplot(raw.means[,], aes(x=vars.plot, y=eff, fill=fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0,position=position_dodge(width=0.5)) +
  geom_point(size=4, pch=21,position=position_dodge(width=0.5)) +
  #ylim(c(-4.5, 8)) +
  xlab("") +
  ylab("log odds ratio") +
  theme(axis.text.x  = element_text(size=14, color="black"),
        axis.text.y  = element_text(size=14, color="black"),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.8,0.2),
        legend.key = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = seq(-3,9, by=2), limits=c(-3,10))+
  scale_x_discrete(labels=c("flat", "tree","flat", "tree","flat", "tree")) +
  annotate("text", label = "c)", y = -3, x = 6.3, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("white", "black"),
                    labels = c("unburned", "burned"))+
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  annotation_custom(grob = textGrob(label=bquote(underline("  clearcut  ")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -5.0, ymax = -5.0) +
  annotation_custom(grob = textGrob(label=bquote(underline("retention")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -5.0, ymax = -5.0) +
  annotation_custom(grob = textGrob(label=bquote(underline("unlogged")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 4.75, xmax = 6.25, ymin = -5.0, ymax = -5.0)

bil.cov.p <- ggplotGrob(bil.cov.p)
bil.cov.p$layout[grepl("panel", bil.cov.p$layout$name), ]$clip <- "off"
#grid.draw(bil.cov.p)

# bil.cov.p <- ggplot(raw.means[,], aes(x=vars, y=eff, fill=fire)) + 
#   geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
#   geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
#                 lwd=0.7, width=0) +
#   geom_point(size=4, pch=21) +
#   ylim(c(-3, 10)) +
#   xlab("") +
#   ylab("Rate ratio") +
#   theme(axis.text.x  = element_text(size=14),
#         axis.text.y  = element_text(size=14),
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=14),
#         legend.title = element_blank(),
#         legend.position= c(0.8,0.2)) +
#   annotate("text", label = "b)", y = -3, x = 12, size = 10) +
#   coord_flip() +
#   scale_fill_manual(values=c("black", "red"),
#                     labels = c("unburned", "burned"))+
#   guides(fill=guide_legend(reverse=TRUE), 
#          colour=guide_legend(reverse=TRUE))


# Combine cover and fruit plots
# Figure 3
# Combine sub-test fruit plots
fig3.g1 <- arrangeGrob(cow.cov.p)
fig3.g2 <- arrangeGrob(cow.fruit.p)
fig3.g3 <- arrangeGrob(bil.cov.p)
fig3.g4 <- arrangeGrob(bil.fruit.p)
png("figure3_fruit_cover_plot.png", width=32, height=30, units="cm", res=300)
grid.arrange(fig3.g1, fig3.g2, fig3.g3, fig3.g4,
             ncol=2, nrow =2)
dev.off()

png("figure3_fruit_cover_plot.png", width=30, height=30, units="cm", res=300)
grid.arrange(cow.cov.p, cow.fruit.p, bil.fruit.p, bil.cov.p , ncol=2, nrow =2)
dev.off()

# Plant cover: sub-test ####
# Cut vs retention-cut ####
# Question: Is the effect of logging different if trees are retained in groups?
# Compare logged areas in Retention treatment with the logged treatment

logg.sub <- zero.dat[zero.dat$type == "open",]
logg.sub <- droplevels(logg.sub)

# cowberry
prior = list( R = list(V = diag(1), nu = 0.002), 
              #G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2),
                     G2=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 

cow.cov.logg.mc <- MCMCglmm(cbind(VV_cover, total_cover-VV_cover)  ~ fire*retention*micro_hab.two, 
                       random = ~ site+nested_plot,
                       data=logg.sub, family = "multinomial2",  nitt = 500000, 
                       burnin = 15000, thin=25, pr = TRUE, pl = TRUE, prior=prior,
                       saveX = TRUE, saveZ = TRUE)
summary(cow.cov.logg.mc)
plot(cow.cov.logg.mc$VCV)

# bilberry
prior = list( R = list(V = diag(1), nu = 0.002), 
              #G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2),
                     G2=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 


bil.cov.logg.mc <- MCMCglmm(cbind(VM_cover, total_cover-VM_cover)  ~ fire*retention*micro_hab.two, 
                       random = ~ site+nested_plot,
                       data=logg.sub, family = "multinomial2",  nitt = 550000, 
                       burnin = 15000, thin=25, pr = TRUE, pl = TRUE, prior=prior,
                       saveX = TRUE, saveZ = TRUE)
summary(bil.cov.logg.mc)
plot(bil.cov.logg.mc$VCV)
# effect of retention on flat ground
exp(summary(bil.cov.logg.mc)$solutions[3,1]) # ca 11 times

# Cowberry: Plot sub-logg model
raw.means <- aggregate(VV_cover ~ fire*retention*micro_hab.two, logg.sub, mean)
newDat <- raw.means[,1:4] 
X <- model.matrix(formula(~ fire*retention*micro_hab.two),
                  newDat)
coefs <- cow.cov.logg.mc$Sol[,2:8]
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")
raw.means[1,6:7] <- as.numeric(HPDinterval(cow.cov.logg.mc$Sol[,1]) - mean(cow.cov.logg.mc$Sol[,1]))

raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
raw.means$order <- c(1, 3, 5, 7, 2, 4, 6, 8)

# rearrange
raw.means$vars <- factor(raw.means$vars, levels = raw.means$vars[order(raw.means$order)])
raw.means$vars.plot <- factor(paste(raw.means[,2], raw.means[,3], sep = "+"))

cow.cov.logg.p <- ggplot(raw.means[,], aes(x=vars.plot, y=eff, fill=fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0,position=position_dodge(width=0.5)) +
  geom_point(size=4, pch=21,position=position_dodge(width=0.5)) +
  #ylim(c(-4.5, 8)) +
  xlab("") +
  ylab("log odds ratio") +
  theme(axis.text.x  = element_text(size=14, color="black"),
        axis.text.y  = element_text(size=14, color="black"),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.8,0.8),
        legend.key = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = seq(-2,7, by=2), limits=c(-2.2,7))+
  scale_x_discrete(labels=c("flat", "tree","flat", "tree"))+
  annotate("text", label = "a)", y = -2.0, x = 4.3, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("white", "black"),
                    labels = c("unburned", "burned"))+
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  annotation_custom(grob = textGrob(label=bquote(underline("     clearcut    ")), gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -3.7, ymax = -3.7) +
  annotation_custom(grob = textGrob(label=bquote(underline("retention (cut)")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -3.7, ymax = -3.7)

cow.cov.logg.p <- ggplotGrob(cow.cov.logg.p )
cow.cov.logg.p $layout[grepl("panel", cow.cov.logg.p $layout$name), ]$clip <- "off"
#grid.draw(cow.cov.logg.p)

# cow.cov.logg.p <- ggplot(raw.means[,], aes(x=vars, y=eff, fill=fire)) + 
#   geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
#   geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
#                 lwd=0.7, width=0) +
#   geom_point(size=4, pch=21) +
#   ylim(c(-2.2, 7)) +
#   xlab("") +
#   ylab("Odds ratio") +
#   theme(axis.text.x  = element_text(size=14),
#         axis.text.y  = element_text(size=14),
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=14),
#         legend.title = element_blank(),
#         legend.position= c(0.8,0.3)) +
#   annotate("text", label = "a)", y = -2, x = 8, size = 10) +
#   coord_flip() +
#   scale_fill_manual(values=c("black", "red"),
#                     labels = c("unburned", "burned"))+
#   guides(fill=guide_legend(reverse=TRUE), 
#          colour=guide_legend(reverse=TRUE))
# Negative effect of fire on cowberry plant cover less severe on logged areas with
# retention trees. But no general positive effect of retention trees without fire compared to
# standard clear-cut treatment.

# bilberry: Plot sub-logg model
raw.means <- aggregate(VM_cover ~ fire*retention*micro_hab.two, logg.sub, mean)
newDat <- raw.means[,1:4] 
X <- model.matrix(formula(~ fire*retention*micro_hab.two),
                  newDat)
coefs <- bil.cov.logg.mc$Sol[,2:8]
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")
raw.means[1,6:7] <- as.numeric(HPDinterval(bil.cov.logg.mc$Sol[,1]) - mean(bil.cov.logg.mc$Sol[,1]))

raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
raw.means$order <- c(1, 3, 5, 7, 2, 4, 6, 8)

# rearrange
raw.means$vars <- factor(raw.means$vars, levels = raw.means$vars[order(raw.means$order)])
raw.means$vars.plot <- factor(paste(raw.means[,2], raw.means[,3], sep = "+"))

bil.cov.logg.p <- ggplot(raw.means[,], aes(x=vars.plot, y=eff, fill=fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0,position=position_dodge(width=0.5)) +
  geom_point(size=4, pch=21,position=position_dodge(width=0.5)) +
  #ylim(c(-4.5, 8)) +
  xlab("") +
  ylab("log odds ratio") +
  theme(axis.text.x  = element_text(size=14, color="black"),
        axis.text.y  = element_text(size=14, color="black"),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.8,0.3),
        legend.key = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = seq(-2,7, by=2), limits=c(-2.2,7))+
  scale_x_discrete(labels=c("flat", "tree","flat", "tree"))+
  annotate("text", label = "c)", y = -2.0, x = 4.3, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("white", "black"),
                    labels = c("unburned", "burned"))+
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  annotation_custom(grob = textGrob(label=bquote(underline("     clearcut    ")), gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -3.7, ymax = -3.7) +
  annotation_custom(grob = textGrob(label=bquote(underline("retention (cut)")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -3.7, ymax = -3.7)

bil.cov.logg.p <- ggplotGrob(bil.cov.logg.p )
bil.cov.logg.p $layout[grepl("panel", bil.cov.logg.p $layout$name), ]$clip <- "off"
#grid.draw(bil.cov.logg.p)
# 
# bil.cov.logg.p <- ggplot(raw.means, aes(x=vars, y=eff, fill=fire)) + 
#   geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
#   geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
#                 lwd=0.7, width=0) +
#   geom_point(size=4, pch=21) +
#   ylim(c(-2.2, 7)) +
#   xlab("") +
#   ylab("Odds ratio") +
#   theme(axis.text.x  = element_text(size=14),
#         axis.text.y  = element_text(size=14),
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=14),
#         legend.title = element_blank(),
#         legend.position= c(0.8,0.3)) +
#   annotate("text", label = "b)", y = -2, x = 8, size = 10) +
#   coord_flip() +
#   scale_fill_manual(values=c("black", "red"),
#                     labels = c("unburned", "burned"))+
#   guides(fill=guide_legend(reverse=TRUE), 
#          colour=guide_legend(reverse=TRUE))
# Retention trees increases bilberry plant cover on both flat ground and near stumps.
# The negative effect of fire on bilberry cover on clear-cuts is not present
# in the retention treatment


# Retention-forest vs forest ####
# Question: Are retained tree groups similar to intact forest?
# Compare intact areas in the Retention treatment with intact forest
full.sub <- zero.dat[zero.dat$type == "gr",]
full.sub <- droplevels(full.sub)

prior = list( R = list(V = diag(1), nu = 0.002), 
              #G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2),
                     G2=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 

cow.cov.full.mc <- MCMCglmm(cbind(VV_cover, total_cover-VV_cover)  ~ fire*retention*micro_hab.two, 
                            random = ~ site+nested_plot,
                            data=full.sub, family = "multinomial2",  nitt = 150000, 
                            burnin = 15000, thin=25, pr = TRUE, pl = TRUE, prior=prior,
                            saveX = TRUE, saveZ = TRUE)
summary(cow.cov.full.mc)
plot(cow.cov.full.mc$VCV)

# bilberry
prior = list( R = list(V = diag(1), nu = 0.002), 
              #G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2),
                     G2=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 


bil.cov.full.mc <- MCMCglmm(cbind(VM_cover, total_cover-VM_cover)  ~ fire*retention*micro_hab.two, 
                            random = ~ site+nested_plot,
                            data=full.sub, family = "multinomial2",  nitt = 80000, 
                            burnin = 15000, thin=25, pr = TRUE, pl = TRUE, prior=prior,
                            saveX = TRUE, saveZ = TRUE)
summary(bil.cov.full.mc)
plot(bil.cov.full.mc$VCV)
# higher cover in unlogged than patches
exp(summary(bil.cov.full.mc)$solutions[3,1]) #21 times higher on flat ground
exp(sum(summary(bil.cov.full.mc)$solutions[c(3,4,7),1])- 
      sum(summary(bil.cov.full.mc)$solutions[4,1])) #16 times higher on flat ground

# Cowberry: Plot sub-full model
raw.means <- aggregate(VV_cover ~ fire*retention*micro_hab.two, full.sub, mean)
newDat <- raw.means[,1:4] 
X <- model.matrix(formula(~ fire*retention*micro_hab.two),
                  newDat)
coefs <- cow.cov.full.mc$Sol[,2:8]
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")
raw.means[1,6:7] <- as.numeric(HPDinterval(cow.cov.full.mc$Sol[,1]) - mean(cow.cov.full.mc$Sol[,1]))

raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
raw.means$order <- c(1, 3, 5, 7, 2, 4, 6, 8)

# rearrange
raw.means$vars <- factor(raw.means$vars, levels = raw.means$vars[order(raw.means$order)])
raw.means$vars.plot <- factor(paste(raw.means[,2], raw.means[,3], sep = "+"))

cow.cov.full.p <- ggplot(raw.means[,], aes(x=vars.plot, y=eff, fill=fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0,position=position_dodge(width=0.5)) +
  geom_point(size=4, pch=21,position=position_dodge(width=0.5)) +
  xlab("") +
  ylab("log odds ratio") +
  theme(axis.text.x  = element_text(size=14, color="black"),
        axis.text.y  = element_text(size=14, color="black"),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.8,0.8),
        legend.key = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = seq(-5,5, by=2), limits=c(-5, 5.5))+
  scale_x_discrete(labels=c("flat", "tree","flat", "tree"))+
  annotate("text", label = "b)", y = -5, x = 4.3, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("white", "black"),
                    labels = c("unburned", "burned"))+
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE))  +
  annotation_custom(grob = textGrob(label=bquote(underline(retention(patch))), gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -6.7, ymax = -6.7) +
  annotation_custom(grob = textGrob(label=bquote(underline("     unlogged    ")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -6.7, ymax = -6.7)

cow.cov.full.p <- ggplotGrob(cow.cov.full.p)
cow.cov.full.p$layout[grepl("panel", cow.cov.full.p$layout$name), ]$clip <- "off"
#grid.draw(cow.cov.full.p)

# cow.cov.full.p <- ggplot(raw.means, aes(x=vars, y=eff, fill=fire)) +
#   geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
#   geom_errorbar(aes(ymin=lo_95, ymax=up_95),
#                 lwd=0.7, width=0) +
#   geom_point(size=4, pch=21) +
#   ylim(c(-5, 5.5)) +
#   xlab("") +
#   ylab("Odds ratio") +
#   theme(axis.text.x  = element_text(size=14),
#         axis.text.y  = element_text(size=14),
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=14),
#         legend.title = element_blank(),
#         legend.position= c(0.8,0.3)) +
#   annotate("text", label = "a)", y = -5, x = 8, size = 10) +
#   coord_flip() +
#   scale_fill_manual(values=c("black", "red"),
#                     labels = c("unburned", "burned"))+
#   guides(fill=guide_legend(reverse=TRUE),
#          colour=guide_legend(reverse=TRUE))
# Slightly higher cowberry cover on retention than full retention. Full retention give 
# a small positive effect of fire on cowberry cover on flat ground; this positive effect
# is not observed in the retention treatment

# bilberry: Plot sub-full model
raw.means <- aggregate(VM_fruit ~ fire*retention*micro_hab.two, full.sub, mean)
newDat <- raw.means[,1:4] 
X <- model.matrix(formula(~ fire*retention*micro_hab.two),
                  newDat)
coefs <- bil.cov.full.mc$Sol[,2:8]
res <- apply(coefs,1, function (x) x %*% t(X[,-c(1)]))
raw.means$eff <- rowMeans(res)
cis <- t(apply(res, 1, function (x) HPDinterval(as.mcmc(x))))
raw.means <- cbind(raw.means, cis)
colnames(raw.means)[6:7] <- c("lo_95", "up_95")
raw.means[1,6:7] <- as.numeric(HPDinterval(bil.cov.full.mc$Sol[,1]) - mean(bil.cov.full.mc$Sol[,1]))

raw.means$vars <- paste(raw.means[,1], raw.means[,2], raw.means[,3], sep = "+")
raw.means$order <- c(1, 3, 5, 7, 2, 4, 6, 8)

# rearrange
raw.means$vars <- factor(raw.means$vars, levels = raw.means$vars[order(raw.means$order)])
raw.means$vars.plot <- factor(paste(raw.means[,2], raw.means[,3], sep = "+"))

bil.cov.full.p <- ggplot(raw.means[,], aes(x=vars.plot, y=eff, fill=fire)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
                lwd=0.7, width=0,position=position_dodge(width=0.5)) +
  geom_point(size=4, pch=21,position=position_dodge(width=0.5)) +
  #xlab("  retention (patch)               unlogged      ") +
  xlab("")+
  ylab("log odds ratio") +
  theme(axis.text.x  = element_text(size=14, color="black"),
        axis.text.y  = element_text(size=14, color="black"),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.position= c(0.2,0.8),
        legend.key = element_blank(),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = seq(-5,5, by=2), limits=c(-5, 5.5))+
  scale_x_discrete(labels=c("flat", "tree","flat", "tree"))+
  annotate("text", label = "d)", y = -5, x = 4.3, size = 10) +
  coord_flip() +
  scale_fill_manual(values=c("white", "black"),
                    labels = c("unburned", "burned")) +
  guides(fill=guide_legend(reverse=TRUE),
         colour=guide_legend(reverse=TRUE)) +
  annotation_custom(grob = textGrob(label=bquote(underline(retention(patch))), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -6.7, ymax = -6.7) +
  annotation_custom(grob = textGrob(label=bquote(underline("     unlogged    ")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -6.7, ymax = -6.7)

bil.cov.full.p <- ggplotGrob(bil.cov.full.p)
bil.cov.full.p$layout[grepl("panel", bil.cov.full.p$layout$name), ]$clip <- "off"
#grid.draw(bil.cov.full.p)

# bil.cov.full.p <- ggplot(raw.means, aes(x=vars, y=eff, fill=fire)) + 
#   geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
#   geom_errorbar(aes(ymin=lo_95, ymax=up_95), 
#                 lwd=0.7, width=0) +
#   geom_point(size=4, pch=21) +
#   ylim(c(-5, 5.5)) +
#   xlab("") +
#   ylab("Odds ratio") +
#   theme(axis.text.x  = element_text(size=14),
#         axis.text.y  = element_text(size=14),
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=14),
#         legend.title = element_blank(),
#         legend.position= c(0.8,0.3)) +
#   annotate("text", label = "b)", y = -5, x = 8, size = 10) +
#   coord_flip() +
#   scale_fill_manual(values=c("black", "red"),
#                     labels = c("unburned", "burned"))+
#   guides(fill=guide_legend(reverse=TRUE), 
#          colour=guide_legend(reverse=TRUE))
# Retention trees increases bilberry plant cover on both flat ground and near stumps.
# The negative effect of fire on bilberry cover on clear-cuts is not present
# in the retention treatment

# Combine sub-test cover plots
sm1.g1 <- arrangeGrob(cow.cov.logg.p)
sm1.g2 <- arrangeGrob(cow.cov.full.p)
sm1.g3 <- arrangeGrob(bil.cov.logg.p)
sm1.g4 <- arrangeGrob(bil.cov.full.p)
png("figureSM1_logg_full_comp_cov.png", width=30, height=30, units="cm", res=300)
grid.arrange(sm1.g1, sm1.g2, sm1.g3, sm1.g4,
             ncol=2, nrow =2)
dev.off()




# extra stuff
plot_zero_infl(zero.dat$VM_fruit)
library(ggplot2)
plot_zero_infl = function(variable) {
  rpois.od<-function (n, lambda,d=1) {
    if (d<=1)
      rpois(n, lambda)
    else if (d>1)
      rnbinom(n, size=(lambda/(d-1)), mu=lambda)
  }
  
  if( var(variable,na.rm=T) > mean(variable,na.rm=T)) { lab = paste0("Overdispersed (d=",round(var(variable,na.rm=T)/mean(variable,na.rm=T),2),") poisson generated using distribution mean and variance via neg.bin") 
  } else { lab = "Poisson generated with distribution mean"}
  
  qplot(rpois.od(n=NROW(variable),mean(variable,na.rm=T), var(variable,na.rm=T)/mean(variable,na.rm=T)), binwidth=1, fill=I("tomato3"), alpha=I(0.5)) + ggtitle(paste0("Slate=Real; Tomato=Generated\n",lab)) + 
    geom_histogram(aes(x=variable),fill="darkslategray4",alpha=0.5,binwidth=1) + xlab("")
}
