#############################################
# Study 2 ordinal sensitivity analysis: H1, H2, H3 forest plots
# Run from Analysis/: Rscript fig_ANES_ordinal_paths.R
# H1 is unchanged by mediator coding and reproduces the existing H1 plot.
# H2 is a proportional-odds coefficient; H3 has six category contrasts.
# Covariate coding and outcome standardization match the analysis scripts.
#############################################
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggstance)
  library(survey)
})
d <- read.csv('anes_w3.csv')
platform_labels <- c(
  facebook_frequency='Facebook', instagram_frequency='Instagram',
  reddit_frequency='Reddit', tiktok_frequency='TikTok',
  twitter_frequency='Twitter', youtube_frequency='YouTube'
)
wellbeing_labels <- c(loneliness='Loneliness (r)', anhedonia='Anhedonia (r)',
                      depression='Depression (r)')
controls <- c('age','gender','race_ethnicity','income','education')

# H1: the same total-association plot, independent of mediator coding.
h1_source <- readLines('fig_ANES_PO_wellbeing_effects.R')
h1_source <- sub('Plots/ANES_PO_wellbeing.png', 'Plots/ANES_ordinal_PO_wellbeing.png',
                 h1_source, fixed=TRUE)
eval(parse(text=h1_source), envir=new.env(parent=.GlobalEnv))

# H2: standalone survey-weighted ordinal path-a models.
a_results <- bind_rows(lapply(names(platform_labels), function(m) {
  md <- d[complete.cases(d[,c(m,'political_orientation',controls,'w3weight')]) &
            is.finite(d$w3weight) & d$w3weight>0, , drop=FALSE]
  md$mediator_ord <- ordered(md[[m]],levels=1:7)
  design <- svydesign(ids=~1,weights=~w3weight,data=md)
  fit <- svyolr(mediator_ord ~ political_orientation + age + gender +
                  race_ethnicity + income + education,
                design=design,method='logistic',
                control=list(maxit=1000,reltol=1e-12))
  est <- coef(fit)['political_orientation']
  se <- sqrt(vcov(fit)['political_orientation','political_orientation'])
  df <- df.residual(fit)
  data.frame(mediator=m,estimate=unname(est),se=se,
             ci_low=unname(est-qt(.975,df)*se),
             ci_high=unname(est+qt(.975,df)*se),
             p=unname(2*pt(abs(est/se),df,lower.tail=FALSE)), n=nrow(md))
}))
stopifnot(nrow(a_results)==6,!anyNA(a_results))
write.csv(a_results,'anes_ordinal_plot_path_a_results.csv',row.names=FALSE)
a_source <- readLines('fig_ANES_PO_mediators.R')
a_source <- sub('anes_w3_path_a_results.csv','anes_ordinal_plot_path_a_results.csv',a_source,fixed=TRUE)
a_source <- a_source[!grepl('"total_platform_frequency" = "Social Media",',a_source,fixed=TRUE)]
a_source <- sub('Plots/ANES_PO_mediators_platforms.png','Plots/ANES_ordinal_PO_mediators_platforms.png',a_source,fixed=TRUE)
a_source <- sub('Social Media Use (H2)','Social Media Use (H2)\\nOrdinal log-odds of more frequent use',a_source,fixed=TRUE)
eval(parse(text=a_source),envir=new.env(parent=.GlobalEnv))

# H3: frequency category 1 (never) is the explicit reference. Ordered-factor
# polynomial contrasts are deliberately not used as a substitute for spacing.
b_results <- bind_rows(lapply(names(platform_labels),function(m) {
  bind_rows(lapply(names(wellbeing_labels),function(y) {
    md <- d[complete.cases(d[,c(y,m,'political_orientation',controls,'w3weight')]) &
              is.finite(d$w3weight) & d$w3weight>0, ,drop=FALSE]
    md$mediator_cat <- factor(md[[m]],levels=1:7)
    contrasts(md$mediator_cat) <- contr.treatment(7,base=1)
    md$outcome_z <- as.numeric(scale(md[[y]]))
    design <- svydesign(ids=~1,weights=~w3weight,data=md)
    fit <- svyglm(outcome_z ~ political_orientation + mediator_cat + age +
                    gender + race_ethnicity + income + education,design=design)
    terms <- paste0('mediator_cat',2:7)
    est <- coef(fit)[terms]
    se <- sqrt(diag(vcov(fit)))[terms]
    df <- df.residual(fit)
    data.frame(mediator=m,outcome=y,frequency_category=2:7,
               estimate=unname(est),se=unname(se),
               ci_low=unname(est-qt(.975,df)*se),
               ci_high=unname(est+qt(.975,df)*se),
               p=unname(2*pt(abs(est/se),df,lower.tail=FALSE)),n=nrow(md))
  }))
}))
stopifnot(nrow(b_results)==108,!anyNA(b_results))
write.csv(b_results,'anes_ordinal_plot_path_b_results.csv',row.names=FALSE)
plot_df <- b_results %>% mutate(
  mediator=factor(recode(mediator,!!!platform_labels),levels=unname(platform_labels)),
  outcome=factor(recode(outcome,!!!wellbeing_labels),levels=rev(unname(wellbeing_labels))),
  frequency_category=factor(frequency_category,levels=2:7),
  color_var=if_else(p<=.05,'sig','n.s.')
)
x_limit <- max(.2,ceiling(max(abs(c(plot_df$ci_low,plot_df$ci_high)))*10)/10)
dodge <- position_dodge(width=.8)
p <- ggplot(plot_df,aes(x=estimate,y=outcome,color=color_var,
                        shape=frequency_category,group=frequency_category))+
  geom_hline(yintercept=c(1.5,2.5),linewidth=.3,colour='grey75')+
  geom_vline(xintercept=0,colour='black',linetype='dashed')+
  ggstance::geom_errorbarh(aes(xmin=ci_low,xmax=ci_high),height=.12,
                          size=.45,position=dodge)+
  geom_point(size=2.5,position=dodge)+
  facet_wrap(~mediator,ncol=3)+
  coord_cartesian(xlim=c(-x_limit,x_limit))+
  scale_y_discrete(drop=FALSE)+
  scale_color_manual(values=c(sig='#FF1493','n.s.'='grey75'),guide='none')+
  scale_shape_manual(values=c('2'=16,'3'=17,'4'=15,'5'=18,'6'=3,'7'=4),
                     labels=paste0(2:7,' vs 1 (never)'),
                     name='Frequency category',guide=guide_legend(nrow=1))+
  labs(x='Associations Between Social Media Use and Wellbeing (H3)\nFrequency-category differences versus never use (outcome SD)',y='')+
  theme_minimal(base_size=15)+
  theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.background=element_rect(fill='white',colour=NA),
        plot.background=element_rect(fill='white',colour=NA),
        strip.text=element_text(face='bold',size=20,colour='black'),
        axis.text.y=element_text(size=15,lineheight=.95,colour='black'),
        axis.text.x=element_text(size=20,colour='black'),
        axis.title.x=element_text(size=24,colour='black',margin=margin(t=10)),
        legend.position='bottom',legend.text=element_text(size=14))
ggsave('Plots/ANES_ordinal_mediators_wellbeing_by_outcome_facets.png',
       plot=p,width=18,height=10,dpi=300)
