################################################################################################
# R code to reproduce results in 
# Granath et al. 
# Submitted to Journal of Applied Ecology
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
dd <- zero.dat %>% 
  group_by(site, fire, retention, type, micro_hab.two) %>% 
  summarise_at(vars(VV_cover, VV_fruit, 
                    VM_cover, VM_fruit),
               funs(mean)) %>%
  as.data.frame()

dd$micro_hab.two.ed <- interaction(dd$micro_hab.two, dd$type)
dd$micro_hab.two.ed2 <- ifelse(dd$retention == "elev", as.character(dd$micro_hab.two.ed), 
                               as.character(dd$micro_hab.two) )

# cowberry panel
cow.prod.raw <- ggplot(data=dd, aes(y=VV_fruit, x=micro_hab.two.ed2, fill = fire)) +
  geom_point(shape = 21, size=2, position=position_jitterdodge(jitter.width=0.95,dodge.width=0)) +
  scale_fill_manual(breaks = c("no", "yes"), values = c("no" = "white", "yes" = "black"),
                    labels = c("unburned", "burned")) +
  facet_grid(~ retention, scales = "free", space = "free", 
             labeller = as_labeller(c('cut' = "logged", 'elev' = "retention", 'full' = "unlogged"))) +
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
                   labels=c("flat", "tree", "flat\nunlogged", "flat\nlogged", "tree\nunlogged", "tree\nlogged", "flat", "tree")) +
  annotation_custom(
    grob = textGrob(label = "a)", gp = gpar(fontsize = 20)),
    ymin = 113,      # Vertical position of the textGrob
    #  ymax = 5,
    xmin = -16)
cow.gt <- ggplot_gtable(ggplot_build(cow.prod.raw))
cow.gt$layout$clip[cow.gt$layout$name == "panel"] <- "off"
grid.draw(cow.gt)

# bilberry panel
bil.prod.raw <- ggplot(data=dd, aes(y=VM_fruit, x=micro_hab.two.ed2, fill = fire)) +
  geom_point(shape = 21, size=2, position=position_jitterdodge(jitter.width=0.95,dodge.width=0)) +
  scale_fill_manual(breaks = c("no", "yes"), values = c("no" = "white", "yes" = "black"),
                    labels = c("unburned", "burned"), guide=FALSE) +
  facet_grid(~ retention, scales = "free", space = "free", 
             labeller = as_labeller(c('cut' = "logged", 'elev' = "retention", 'full' = "unlogged"))) +
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
                   labels=c("flat", "tree", "flat\nunlogged", "flat\nlogged", "tree\nunlogged", "tree\nlogged", "flat", "tree")) +
  annotation_custom(
    grob = textGrob(label = "b)", gp = gpar(fontsize = 20)),
    ymin = 12,      # Vertical position of the textGrob
    #  ymax = 5,
    xmin = -15.5)
bil.gt <- ggplot_gtable(ggplot_build(bil.prod.raw))
bil.gt$layout$clip[bil.gt$layout$name == "panel"] <- "off"
grid.draw(bil.gt)

# Plot Figure 1
png("figure1_berries_raw.png", width=22, height=24, units="cm", res=300)
grid.arrange(cow.gt , bil.gt, ncol=1, nrow =2)
dev.off()

# Calculate means per retention level
means <- zero.dat %>% 
  group_by(retention, fire) %>% 
  summarise_at(vars(VV_cover, VV_fruit, 
                    VM_cover, VM_fruit),
               funs(mean)) %>%
  as.data.frame()
means

# Calculate fruits per 1 m2 (sample plot size 0.16 m2)
means[5, c(4,6)] * (1/0.16)

# Cowberry fruits ####

# zip model cowberry
nitt = 150000 #low for testing
thin = 25 #low for testing
burnin = 15000 #low for testing
prior = list( R = list(V = diag(2), nu = 0.002, fix = 2), 
              G=list(G1=list(V=diag(2)*c(1,0.001), nu=0.002, fix=2), #alpha.mu=0, alpha.V=625^2
              G2=list(V=diag(2)*c(1,0.001), nu=0.002, fix=2))) 

zip.cow <- MCMCglmm(VV_fruit ~ trait -1 + at.level(trait, 1):(fire*retention*micro_hab.two), 
                random = ~ idh(trait):site + idh(trait):nested_plot, rcov = ~idh(trait):units,
                data=zero.dat, family = "zipoisson",  nitt = nitt, 
                burnin = burnin, thin=thin, prior=prior, pr = TRUE, pl = TRUE)
summary(zip.cow)
# Gives a warning but can be ignored. Contrasts are not straight forward to interpret.

# Calculate some effects
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

# Controll for plant cover by using cover as an offset
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
# Gives a warning, but can be ignored. Contrasts are not straight forward to interpret.


# Fruit sub-tests: cowberry####
# Question: Is the effect of logging different if trees are retained in groups compared to clearcut?
# Compare logged areas in Retention treatment with the logged (ie clearcut) treatment
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
  annotation_custom(grob = textGrob(label=bquote(underline("      logged     ")), gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -6.7, ymax = -6.7) +
  annotation_custom(grob = textGrob(label=bquote(underline("retention (logged)")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -6.7, ymax = -6.7)

cow.fruit.logg.p <- ggplotGrob(cow.fruit.logg.p )
cow.fruit.logg.p $layout[grepl("panel", cow.fruit.logg.p $layout$name), ]$clip <- "off"
grid.draw(cow.fruit.logg.p)


# Question: Are retained tree groups similar to intact(i.e. uncut) forest?
# Compare intact areas in the Retention treatment with intact forest
full.sub <- zero.dat[zero.dat$type == "gr",]
full.sub <- droplevels(full.sub)

zip.full.cow.com <- MCMCglmm(VV_fruit ~ trait -1 + at.level(trait, 1):(fire*retention*micro_hab.two), 
                         random = ~ idh(trait):nested_plot + idh(trait):site, rcov = ~idh(trait):units,
                         data=full.sub, family = "zipoisson",  nitt = nitt, 
                         burnin = burnin, thin=thin, prior=prior, pr = TRUE, pl = TRUE)
summary(zip.full.cow.com)

# Plot sub-full model (tree groups vs intact forest)
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
  annotation_custom(grob = textGrob(label=bquote(underline(retention(unlogged))), gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -8.9, ymax = -8.9) +
  annotation_custom(grob = textGrob(label=bquote(underline("     unlogged    ")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -8.9, ymax = -8.9)

cow.fruit.full.p <- ggplotGrob(cow.fruit.full.p)
cow.fruit.full.p$layout[grepl("panel", cow.fruit.full.p$layout$name), ]$clip <- "off"
grid.draw(cow.fruit.full.p)


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
# Gives a warning, but can be ignored. Contrasts are not straight forward to interpret.

# Controll for plant cover by using cover as an offset
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

# Poisson model bilberry for comparison with zip
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

# Evaluate fit of ZIP vs Poisson model
# Poisson model 
oz <- sum(zero.dat$VM_fruit == 0)
sim.pois <- simulate(pois.bil, type="response", posterior = "mean", nsim=1000)
dist.zeros.pois <- apply(sim.pois, 2, function (x) sum(x==0))
hist(dist.zeros.pois)
abline(v = oz, col = "red")

p.zip <- predict(zip.bil,   type="response", posterior = "mean")
p.pois <- predict(pois.bil,   type="response", posterior = "mean")
cbind(aggregate(VM_fruit ~ fire*retention*micro_hab.two, zero.dat, mean),
      zip = aggregate(p.zip ~ fire*retention*micro_hab.two, zero.dat, mean)$V1,
      pois = aggregate(p.pois ~ fire*retention*micro_hab.two, zero.dat, mean)$V1)
# poissoon model give extreme (unrealistic) values.

# Fruits sub-tests: bilberry####
# Question: Is the effect of logging different if trees are retained in groups compared to a clearcut?
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

# Calculate some effects
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
  annotation_custom(grob = textGrob(label=bquote(underline("      logged     ")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -6.7, ymax = -6.7) +
  annotation_custom(grob = textGrob(label=bquote(underline("retention (logged)")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -6.7, ymax = -6.7)

bil.fruit.logg.p <- ggplotGrob(bil.fruit.logg.p)
bil.fruit.logg.p$layout[grepl("panel", bil.fruit.logg.p$layout$name), ]$clip <- "off"
grid.draw(bil.fruit.logg.p)


# Question: Are retained tree groups similar to intact (ie uncut) forest?
# Compare intact areas in the Retention treatment with intact forest
full.sub <- zero.dat[zero.dat$type == "gr",]
full.sub <- droplevels(full.sub)

zip.full.bil.com <- MCMCglmm(VM_fruit ~ trait -1 + at.level(trait, 1):(fire*retention*micro_hab.two), 
                         random = ~ idh(trait):site + idh(trait):nested_plot, rcov = ~idh(trait):units,
                         data=full.sub, family = "zipoisson",  nitt = 150000, 
                         burnin = burnin, thin=thin, prior=prior, pr = TRUE, pl = TRUE)
summary(zip.full.bil.com)

# Calculate some effects
# effect of prescribed burning differ between retention-P (ie tree groups) and unlogged
# retention-P: no interaction with microhabitat so we use the coef for flat ground
exp(summary(zip.full.bil.com)$solutions[c(3),1]*(-1)) #-94% 

# unlogged: no significant interactions so use the the effect for 
# flat ground again.
exp(summary(zip.full.bil.com)$solutions[c(3),1]*(-1) + 
      summary(zip.full.bil.com)$solutions[6,1]) #+226% 


# Plot sub-full (retained tree groups vs uncut) model
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
  annotation_custom(grob = textGrob(label=bquote(underline(retention(unlogged))), 
                                    gp=gpar(fontsize=16),rot=90), 
                  xmin = 0.75, xmax = 2.25, ymin = -8.9, ymax = -8.9) +
  annotation_custom(grob = textGrob(label=bquote(underline("     unlogged    ")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -8.9, ymax = -8.9)

bil.fruit.full.p <- ggplotGrob(bil.fruit.full.p)
bil.fruit.full.p$layout[grepl("panel", bil.fruit.full.p$layout$name), ]$clip <- "off"


# Make figure 3
# Combine sub-test fruit plots
fig3.g1 <- arrangeGrob(cow.fruit.logg.p)
fig3.g2 <- arrangeGrob(cow.fruit.full.p)
fig3.g3 <- arrangeGrob(bil.fruit.logg.p)
fig3.g4 <- arrangeGrob(bil.fruit.full.p)
png("figure3_logg_full_comp_fruit_update.png", width=32, height=30, units="cm", res=300)
grid.arrange(fig3.g1, fig3.g2, fig3.g3, fig3.g4,
             ncol=2, nrow =2)
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
  annotation_custom(grob = textGrob(label=bquote(underline("   logged   ")), 
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
grid.draw(cow.fruit.p)


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
  annotation_custom(grob = textGrob(label=bquote(underline("   logged   ")), 
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
grid.draw(bil.fruit.p)


#bilberry fruit offset model (controll for cover)
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
summary(zip.bil)


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
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2),
              G2=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 

cow.cov.mc <- MCMCglmm(cbind(VV_cover, total_cover-VV_cover)  ~ fire*retention*micro_hab.two, 
                       random = ~ site+nested_plot,
                       data=zero.dat, family = "multinomial2",  nitt = 80000, 
                       burnin = 15000, thin=25, pr = TRUE, pl = TRUE, prior=prior,
                       saveX = TRUE, saveZ = TRUE)
summary(cow.cov.mc)
plot(cow.cov.mc$VCV)

# Calculate some effects
# Effect of stumps on clearcuts on cowberry cover
exp(summary(cow.cov.mc)$solutions[5,1]) # 2.1 times increase near stumps

# bilberry
prior = list( R = list(V = diag(1), nu = 0.002), 
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
  annotation_custom(grob = textGrob(label=bquote(underline("   logged   ")), 
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
grid.draw(cow.cov.p)


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
  annotation_custom(grob = textGrob(label=bquote(underline("   logged   ")), 
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
grid.draw(bil.cov.p)

# Combine cover and fruit plots
# Figure 2
fig2.g1 <- arrangeGrob(cow.cov.p)
fig2.g2 <- arrangeGrob(cow.fruit.p)
fig2.g3 <- arrangeGrob(bil.cov.p)
fig2.g4 <- arrangeGrob(bil.fruit.p)
png("figure2_fruit_cover_plot.png", width=32, height=30, units="cm", res=300)
grid.arrange(fig2.g1, fig2.g2, fig2.g3, fig2.g4,
             ncol=2, nrow =2)
dev.off()


# Plant cover: sub-test ####

# Question: Is the effect of logging different if trees are retained in groups?
# Compare logged areas in Retention treatment with logged on the clearcut treatment

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

# Cowberry: Plot sub-logg model (logged clearcut vs logged rention treatment)
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
  annotation_custom(grob = textGrob(label=bquote(underline("      logged     ")), gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -3.7, ymax = -3.7) +
  annotation_custom(grob = textGrob(label=bquote(underline("retention (logged)")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -3.7, ymax = -3.7)

cow.cov.logg.p <- ggplotGrob(cow.cov.logg.p )
cow.cov.logg.p $layout[grepl("panel", cow.cov.logg.p $layout$name), ]$clip <- "off"
grid.draw(cow.cov.logg.p)
# Negative effect of fire on cowberry plant cover less severe on logged areas with
# retention trees. But no general positive effect of retention trees without fire compared to
# standard clear-cut treatment.

# bilberry: Plot sub-logg model (logged clearcut vs logged rention treatment)
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
  annotation_custom(grob = textGrob(label=bquote(underline("      logged     ")), gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -3.7, ymax = -3.7) +
  annotation_custom(grob = textGrob(label=bquote(underline("retention (logged)")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -3.7, ymax = -3.7)

bil.cov.logg.p <- ggplotGrob(bil.cov.logg.p )
bil.cov.logg.p $layout[grepl("panel", bil.cov.logg.p $layout$name), ]$clip <- "off"
grid.draw(bil.cov.logg.p)
 # Retention trees increases bilberry plant cover on both flat ground and near stumps.
# The negative effect of fire on bilberry cover on clear-cuts is not present
# in the retention treatment


# Question: Are retained tree groups similar to intact (ie uncut) forest?
# Compare intact patches in the Retention treatment with intact forest
full.sub <- zero.dat[zero.dat$type == "gr",]
full.sub <- droplevels(full.sub)

prior = list( R = list(V = diag(1), nu = 0.002), 
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
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2),
                     G2=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 


bil.cov.full.mc <- MCMCglmm(cbind(VM_cover, total_cover-VM_cover)  ~ fire*retention*micro_hab.two, 
                            random = ~ site+nested_plot,
                            data=full.sub, family = "multinomial2",  nitt = 80000, 
                            burnin = 15000, thin=25, pr = TRUE, pl = TRUE, prior=prior,
                            saveX = TRUE, saveZ = TRUE)
summary(bil.cov.full.mc)
plot(bil.cov.full.mc$VCV)

# Calculate some effects
# higher cover in unlogged than patches
exp(summary(bil.cov.full.mc)$solutions[3,1]) #21 times higher on flat ground
exp(sum(summary(bil.cov.full.mc)$solutions[c(3,4,7),1])- 
      sum(summary(bil.cov.full.mc)$solutions[4,1])) #16 times higher on flat ground

# Cowberry: Plot sub-model (retention patch vs uncut forest)
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
  annotation_custom(grob = textGrob(label=bquote(underline(retention(unlogged))), gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -6.7, ymax = -6.7) +
  annotation_custom(grob = textGrob(label=bquote(underline("     unlogged    ")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -6.7, ymax = -6.7)

cow.cov.full.p <- ggplotGrob(cow.cov.full.p)
cow.cov.full.p$layout[grepl("panel", cow.cov.full.p$layout$name), ]$clip <- "off"
grid.draw(cow.cov.full.p)
# Slightly higher cowberry cover on retention than full retention. Full retention give 
# a small positive effect of fire on cowberry cover on flat ground; this positive effect
# is not observed in the retention treatment

# bilberry: Plot sub-model (retention patch vs uncut forest)
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
  annotation_custom(grob = textGrob(label=bquote(underline(retention(unlogged))), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 0.75, xmax = 2.25, ymin = -6.7, ymax = -6.7) +
  annotation_custom(grob = textGrob(label=bquote(underline("     unlogged    ")), 
                                    gp=gpar(fontsize=16),rot=90), 
                    xmin = 2.75, xmax = 4.25, ymin = -6.7, ymax = -6.7)

bil.cov.full.p <- ggplotGrob(bil.cov.full.p)
bil.cov.full.p$layout[grepl("panel", bil.cov.full.p$layout$name), ]$clip <- "off"
grid.draw(bil.cov.full.p)
# Retention trees increases bilberry plant cover on both flat ground and near stumps.
# The negative effect of fire on bilberry cover on clear-cuts is not present
# in the retention treatment

# Combine sub-test cover plots
# Figure 1 in Supplemental Material, SM1.
sm2.g1 <- arrangeGrob(cow.cov.logg.p)
sm2.g2 <- arrangeGrob(cow.cov.full.p)
sm2.g3 <- arrangeGrob(bil.cov.logg.p)
sm2.g4 <- arrangeGrob(bil.cov.full.p)
png("figureSM2_logg_full_comp_cov.png", width=30, height=30, units="cm", res=300)
grid.arrange(sm2.g1, sm2.g2, sm2.g3, sm2.g4,
             ncol=2, nrow =2)
dev.off()


# Multifunctionality ####
library(nlme) # for gls()
library("AICcmodavg") # for predictSE.gls()
library(boot) # for glm.diag()

# function to get approx 95% CI
glm.pred <- function(mod = NULL, new_dat = NULL) {
  if(class(mod)[1] == "glm")
        {pred <- predict(mod, new_dat, type = "response", se.fit = TRUE)}
  if(class(mod)[1] == "gls")
        {pred <- predictSE.gls(mod, new_dat, se.fit = TRUE)}
  
  new_dat$pred <-pred$fit
  new_dat$pred.lo <- new_dat$pred-2*pred$se.fit
  new_dat$pred.up <- new_dat$pred+2*pred$se.fit
  intc <- new_dat$pred[1]
  means <- round((new_dat$pred[2:6] / intc -1)*100)
  lo <- round((new_dat$pred.lo[2:6] / intc -1)*100)
  up <- round((new_dat$pred.up[2:6] / intc -1)*100)
  return(data.frame(means, lo, up))
}
  

# Berries
# fix df dd from code in the begining
VVberry <- aggregate(VV_fruit ~ site + fire + retention, dd, mean)
VMberry <- aggregate(VM_fruit ~ site + fire + retention, dd, mean)

# stats models
# gls cowberry
mod.VVberry.sr <- gls(VV_fruit ~ retention*fire, VVberry,
                   weights=varIdent(form=~1|retention*fire))
anova(mod.VVberry.sr)
summary(mod.VVberry.sr)
plot(mod.VVberry.sr)

#gamma cowberry
mod.VVberry.sr.gamma = glm(VV_fruit ~ retention*fire, VVberry,, family=Gamma(link = log) )
summary(mod.VVberry.sr.gamma)
plot(mod.VVberry.sr.gamma)
leuk.diag <- glm.diag(mod.VVberry.sr.gamma)
glm.diag.plots(mod.VVberry.sr.gamma, leuk.diag)

# predict  cowberry changes
new_dat <- unique(VVberry[, c("fire", "retention")])
new_dat <- new_dat[order(new_dat$fire, new_dat$retention),]
p.VVberry.gls <- glm.pred(mod=mod.VVberry.sr, new_dat=new_dat)
p.VVberry.gamma <- glm.pred(mod=mod.VVberry.sr.gamma, new_dat=new_dat)

# bilberry
#gls bilberry
mod.VMberry.sr <- gls(VM_fruit+0.1 ~ retention*fire, VMberry,
                      weights=varIdent(form=~1|retention*fire))
anova(mod.VMberry.sr)
summary(mod.VMberry.sr)
plot(mod.VMberry.sr)

#gamma bilberry
mod.VMberry.sr.gamma = glm(VM_fruit+0.1 ~ retention*fire, VMberry, family=Gamma(link = log) )
summary(mod.VMberry.sr.gamma)
plot(mod.VMberry.sr.gamma)
leuk.diag <- glm.diag(mod.VMberry.sr.gamma)
glm.diag.plots(mod.VMberry.sr.gamma, leuk.diag)

# predict bilberry changes
new_dat <- unique(VMberry[, c("fire", "retention")])
new_dat <- new_dat[order(new_dat$fire, new_dat$retention),]
p.VMberry.gls <- glm.pred(mod=mod.VMberry.sr, new_dat=new_dat)
p.VMberry.gamma <- glm.pred(mod=mod.VMberry.sr.gamma, new_dat=new_dat)


# pollinators
poll <- read.csv("Rodriguez-Kouki-2017.csv")

# get presence-absence for each site
poll2 <- aggregate(poll[,5:112],by = list(poll$SITE,poll$RET,poll$FIRE), FUN=mean)
poll2[, 4:111] <- ifelse(poll2[, 4:111]>0,1,0)

poll2$sprich <- apply(poll2[,4:111], 1, sum)
poll3 <- poll2[,c(1:3,112)]
colnames(poll3)[1:3] <- c("site", "treat", "fire") 
poll <- poll3[!(poll3$treat == "10"),] # remove 10% retention (not included in the berry data)
poll <- droplevels(poll)

# stats models
# gls pollinators
mod.poll.sr <- gls(sprich ~ treat*fire, poll,
                    weights=varIdent(form=~1|treat*fire))
anova(mod.poll.sr)
summary(mod.poll.sr)
plot(mod.poll.sr)

# gamma pollinators
mod.poll.sr.gamma = glm(sprich ~ treat*fire, poll, family=Gamma(link = log) )
summary(mod.poll.sr.gamma)
plot(mod.poll.sr.gamma)
leuk.diag <- glm.diag(mod.poll.sr.gamma)
glm.diag.plots(mod.poll.sr.gamma, leuk.diag)

# predict pollinators changes
new_dat <- unique(poll[, c("fire", "treat")])
new_dat <- new_dat[order(new_dat$fire, new_dat$treat),]
p.poll.gls <- glm.pred(mod=mod.poll.sr, new_dat=new_dat)
p.poll.gamma <- glm.pred(mod=mod.poll.sr.gamma, new_dat=new_dat)


# insects
insec <- read.csv("Heikkala-et-al-2016.csv")
# remove 10% retention (not included in the berry data) and keep only the latest data (yr 2011)
insec <- insec[!(insec$ret == "10") & insec$year == 2011,] 
insec$fire <- relevel(insec$fire, ref = "U")
insec <- droplevels(insec)

# stats models
# gls insects
mod.insec.sr <- gls(SpecNumb ~ ret*fire, insec,
                    weights=varIdent(form=~1|ret*fire))
anova(mod.insec.sr)
summary(mod.insec.sr)
plot(mod.plant.sr)

# gamma insects
mod.insec.sr.gamma = glm(SpecNumb ~ ret*fire, insec, family=Gamma(link = log) )
summary(mod.insec.sr.gamma)
plot(mod.insec.sr.gamma)
leuk.diag <- glm.diag(mod.insec.sr.gamma)
glm.diag.plots(mod.insec.sr.gamma, leuk.diag)

# predict insects changes
new_dat <- unique(insec[, c("fire", "ret")])
new_dat <- new_dat[order(new_dat$fire, new_dat$ret),]
p.insec.gls <- glm.pred(mod=mod.insec.sr, new_dat=new_dat)
p.insec.gamma <- glm.pred(mod=mod.insec.sr.gamma, new_dat=new_dat)

# plants
plant <- read.csv("fin_exp_plant_rich.csv")

# stats models
# gls plant
mod.plant.sr <- gls(plant_sr ~ retention*fire, plant,
         weights=varIdent(form=~1|retention*fire))
anova(mod.plant.sr)
summary(mod.plant.sr)
plot(mod.plant.sr)

#gamma plant
mod.plant.sr.gamma = glm(plant_sr ~ retention*fire, plant, family=Gamma(link = log) )
summary(mod.plant.sr.gamma)
plot(mod.plant.sr.gamma)
leuk.diag <- glm.diag(mod.plant.sr.gamma)
glm.diag.plots(mod.plant.sr.gamma, leuk.diag)

# predict plant changes
new_dat <- unique(plant[, c("fire", "retention")])
new_dat <- new_dat[order(new_dat$fire, new_dat$retention),]
p.plant.gls <- glm.pred(mod=mod.plant.sr, new_dat=new_dat)
p.plant.gamma <- glm.pred(mod=mod.plant.sr.gamma, new_dat=new_dat)

# gls bryo
mod.bryo.sr <- gls(bryo_sr ~ retention*fire, plant,
                    weights=varIdent(form=~1|retention*fire))
anova(mod.bryo.sr)
summary(mod.bryo.sr)
plot(mod.bryo.sr)

#gamma bryo
mod.bryo.sr.gamma = glm(bryo_sr ~ retention*fire, plant, family=Gamma(link = log) )
summary(mod.bryo.sr.gamma)
plot(mod.bryo.sr.gamma)
leuk.diag <- glm.diag(mod.bryo.sr.gamma)
glm.diag.plots(mod.bryo.sr.gamma, leuk.diag)

# predict bryo changes
new_dat <- unique(plant[, c("fire", "retention")])
new_dat <- new_dat[order(new_dat$fire, new_dat$retention),]
p.bryo.gls <- glm.pred(mod=mod.bryo.sr, new_dat=new_dat)
p.bryo.gamma <- glm.pred(mod=mod.bryo.sr.gamma, new_dat=new_dat)

# gls lichen
mod.lichen.sr <- gls(lichen_sr ~ retention*fire, plant,
                   weights=varIdent(form=~1|retention*fire))
anova(mod.lichen.sr)
summary(mod.lichen.sr)
plot(mod.lichen.sr)

#gamma lichen
mod.lichen.sr.gamma = glm(lichen_sr ~ retention*fire, plant, family=Gamma(link = log) )
summary(mod.lichen.sr.gamma)
plot(mod.lichen.sr.gamma)
leuk.diag <- glm.diag(mod.lichen.sr.gamma)
glm.diag.plots(mod.lichen.sr.gamma, leuk.diag)

# predict lichen changes
new_dat <- unique(plant[, c("fire", "retention")])
new_dat <- new_dat[order(new_dat$fire, new_dat$retention),]
p.lichen.gls <- glm.pred(mod=mod.lichen.sr, new_dat=new_dat)
p.lichen.gamma <- glm.pred(mod=mod.lichen.sr.gamma, new_dat=new_dat)

multi.table <- cbind(p.VVberry.gamma, p.VMberry.gamma, p.poll.gamma,
     p.insec.gamma, p.plant.gamma, p.bryo.gamma, p.lichen.gamma)
coln <- c("cowberry","lo","up", "bilberry","lo","up", "pollinators","lo","up",
         "beetles","lo","up", "vasc plants","lo","up", "bryophytes","lo","up", "lichens","lo","up")
colnames(multi.table) <- coln
write.csv(multi.table, "multi_table.csv")

# Plot multi table....not completed
# multi.table2 <- expand.grid(retention=levels(zero.dat$retention), fire = levels(zero.dat$fire))
# multi.table2 <- cbind(multi.table2, matrix(NA, ncol=ncol(multi.table), nrow = nrow(multi.table2))) 
# multi.table2[-1,3:ncol(multi.table2)] <- multi.table
# colnames(multi.table2)[3:ncol(multi.table2)] <- colnames(multi.table)
# multi.table2[1,3:ncol(multi.table2)] <- 0


# Modell checking extras ####
library(ggplot2)

# Examples
# expected percent zeros
ppois(0, mean(zero.dat$VV_fruit))*100
# percent zeros in the data
(sum(zero.dat$VV_fruit==0)/nrow(zero.dat) ) * 100

# check overdispersion
# Function to look at overdispersion
plot_zero_infl = function(variable) {
  rpois.od<-function (n, lambda,d=1) {
    if (d<=1)
      rpois(n, lambda)
    else if (d>1)
      rnbinom(n, size=(lambda/(d-1)), mu=lambda)
  }
  
  if( var(variable,na.rm=T) > mean(variable,na.rm=T)) { 
    lab = paste0("Overdispersed (d=", 
                 round(var(variable,na.rm=T)/mean(variable,na.rm=T),2),
                 ") poisson generated using distribution mean and variance via neg.bin") 
  } else { lab = "Poisson generated with distribution mean"}
  
  qplot(rpois.od(n=NROW(variable),mean(variable,na.rm=T), 
                 var(variable,na.rm=T)/mean(variable,na.rm=T)), 
        binwidth=1, fill=I("tomato3"), alpha=I(0.5)) + 
    ggtitle(paste0("Slate color=Real; Tomato color=Generated\n",lab)) + 
    geom_histogram(aes(x=variable),fill="darkslategray4",alpha=0.5,binwidth=1) + xlab("")
}

# Run function
plot_zero_infl(zero.dat$VV_fruit)

# Check random effects
ranef.mcmc1 <- colMeans(zip.cow$Sol[,14:31])
ranef.mcmc2 <- colMeans(zip.cow$Sol[,50:601])

par(mfrow=c(2,2))
hist(ranef.mcmc1)
qqnorm(ranef.mcmc1) ; qqline(ranef.mcmc1)
hist(ranef.mcmc2)
qqnorm(ranef.mcmc2) ; qqline(ranef.mcmc2)

# individual random effect
## observation level random-effects not
## stored, but "the latent variable minus the
## predictions on the link scale (i.e. type="terms")
## should be equivalent to, e.g., the observation-level
## random effect in glmer.
ranef.mcmc3 <- colMeans(zip.bil$Liab) - predict(zip.cow, 
                                                marginal = NULL, type = "terms")
hist(ranef.mcmc3)
qqnorm(ranef.mcmc3) ; qqline(ranef.mcmc3)


