library(ggrepel)
library(ggplot2)
library(reshape2)
library(scales)
library(ggpubr)  
library(rstatix)


#Figure X??: Proportions of publications in the citation network sorted
## by article tyepe + country of origin + hospital affiliation

L <- data.frame("All_Orig" = c(92.59, 7.41),
                "China_Orig" = c(14.79, 85.21),
                "All_Rev" = c(96.00, 4.00),
                "China_Rev" = c(25.71, 74.29),
                "All_Oth" = c(100, NA),
                "China_Oth" = c(NA, NA),
                "Affil" = c("N", "Y"))
View(L)

M <- melt(L, id.vars = "Affil", measure.vars = c("All_Orig", "China_Orig",
                                                 "All_Rev", "China_Rev", 
                                                 "All_Oth","China_Oth"))

M$Label = c("Original research", "Original research", "Original research", 
            "Original research",
            "Reviews", "Reviews", "Reviews",
            "Reviews", 
            "Other", "Other", 
            "Other", "Other")

M$Label <- (factor(M$Label,
                     levels = c("Original research",
                                "Reviews",
                                "Other")))
View(M)

ggplot(M, mapping = aes(variable, value, fill = Affil))+
  geom_col(width = 0.4, alpha = 0.8)+
  geom_text(aes(label=sprintf("%1.f%%", value/1)), colour = "Black", 
            position = position_stack(vjust = 0.5), size = 3.5)+
  facet_wrap(~Label, scales = "free_x")+
  xlab("\nAffiliated Country") + 
  ylab("Hospital affiliation status (%)")+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  scale_x_discrete(breaks = levels(factor(M$variable)), 
                   labels = c("All other Countries \n(n = 54)",
                              "China \n(n = 284)",
                              "All other Countries \n(n = 100)",
                              "China \n(n = 70)", 
                              "All other Countries \n(n = 3)",
                              "China \n(n = 0)"))+
  scale_fill_manual(values = c("grey", "#5baede"), 
                    labels = c("Publications not from hospitals", 
                               "Publications from hospitals"))+
  theme_bw()+
  theme(legend.position = "top", legend.title = element_blank()) + 
  labs(fill = "")

#Figure X_2: Proportions of publications in the citation network sorted
## by article tyepe + country of origin + hospital affiliation
##Reviews and "Other" articles are merged into one panel

L <- data.frame("All_Orig" = c(92.6, 7.4),
                "China_Orig" = c(14.8, 85.2),
                "All_Rev" = c(96.1, 3.9),
                "China_Rev" = c(25.7, 74.3),
                "Affil" = c("N", "Y"))
View(L)

M <- melt(L, id.vars = "Affil", measure.vars = c("All_Orig", "China_Orig",
                                                 "All_Rev", "China_Rev"))

M$Label = c("Original research articles", "Original research articles", 
            "Original research articles", "Original research articles",
            "Reviews and other articles", "Reviews and other articles", 
            "Reviews and other articles", "Reviews and other articles")

M$Label <- (factor(M$Label,
                   levels = c("Original research articles",
                              "Reviews and other articles")))

M$p = c("***", NA, NA, NA)

M_y = c(105, NA, NA, NA, 105, NA, NA, NA)

View(M)

ggplot(M, mapping = aes(variable, value, fill = Affil))+
  geom_col(width = 0.4, alpha = 0.8)+
  geom_text(aes(label=sprintf("%1.f%%", value/1)), colour = "Black", 
            position = position_stack(vjust = 0.5), size = 3.5)+
  facet_wrap(~Label, scales = "free_x")+
  xlab("\nAffiliated Country") + 
  ylab("Proportion of publications (%)")+
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_discrete(breaks = levels(factor(M$variable)), 
                   labels = c("All other Countries \n(n = 54)",
                              "China \n(n = 284)",
                              "All other Countries \n(n = 103)",
                              "China \n(n = 70)"))+
  scale_fill_manual(values = c("grey", "#5baede"), 
                    labels = c("Publications not from hospitals", 
                               "Publications from hospitals"))+
  theme_bw()+
  theme(legend.position = "top", legend.title = element_blank()) + 
  labs(fill = "")+
  geom_text(M, mapping = aes(x = 1.5, y= 108, label = p), size=6)+
  geom_errorbarh(aes(xmax = 2, xmin = 1, y = M_y), height = 5)


#Figure Y??: Proportions of flagged publications in the citation network sorted
## by country of origin + hospital affiliation

L2 <- data.frame("China_Orig" = c(9.19, 90.80),
                "Affil" = c("N", "Y"))
View(L2)

M2 <- melt(L2, id.vars = "Affil", measure.vars = "China_Orig")

M2$Label = c("Original research", "Original research")

View(M2)

ggplot(M2, mapping = aes(variable, value, fill = Affil))+
  geom_col(width = 0.23, alpha = 0.8)+
  geom_text(aes(label=sprintf("%1.f%%", value/1)), colour = "Black", 
            position = position_stack(vjust = 0.5), size = 3.5)+
  facet_wrap(~Label, scales = "free_x")+
  xlab("\nAffiliated Country") + 
  ylab("Hospital affiliation status (%)")+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  scale_x_discrete(breaks = levels(factor(M2$variable)), 
                   labels = c("China \n(n = 87)"))+
  scale_fill_manual(values = c("grey", "#e8b84f"), 
                    labels = c("Publications not from hospitals", 
                               "Publications from hospitals"))+
  theme_bw()+
  theme(legend.position = "top", legend.title = element_blank()) + 
  labs(fill = "")


#Figure Z?? Graph showing different diseases mentioned within the citation network. 
df2 <- data.frame(Disease = c("Cancer",
                              "Other",
                              "Autophagy",
                              "COVID-19",
                              "Myogenesis",
                              "Diabetes",
                              "Osteoarthritis",
                              "Rheumatoid arthritis",
                              "Inflammation",
                              "Mammary tissue",
                              "Parkinson's disease"),
                  Original_research = c(274,34, 7, 4, 
                                        4, 3, 3, 3, 
                                        2, 2, 2),
                  Flagged = c(80,4, 0, 0, 
                              0, 0, 1, 1,
                              0, 0, 1), 
                  Not_flagged = c(194,30,7,4,
                                  4, 3, 2, 2, 
                                  2, 2, 1))

df2_melt <- melt(df2, id.vars = "Disease", measure.vars = c("Original_research", 
                                                            "Flagged", 
                                                            "Not_flagged"))

df2_melt$Disease <- (factor(df2_melt$Disease,
                            levels = c("Cancer",
                                       "Other",
                                       "Autophagy",
                                       "COVID-19",
                                       "Myogenesis",
                                       "Diabetes",
                                       "Osteoarthritis",
                                       "Rheumatoid arthritis",
                                       "Inflammation",
                                       "Mammary tissue",
                                       "Parkinson's disease")))

ggplot(df2_melt, aes(x=variable, y=Disease)) +
  geom_point(aes(colour=value), size=7, alpha=0.6) +
  geom_text_repel(aes(label=value), hjust=1.23, size=4)+
  scale_alpha_continuous(range=c(0.3, 0.7))+
  scale_x_discrete(labels=c("Original research \npublications", "Problematic \npublications", 
                            "Non-problematic \npublications"), position="top")+  
  scale_y_discrete(limits=rev)+
  scale_colour_gradient2(mid="grey", high="red",
                       breaks=seq(0,300, 50))+
  ylab("Studied topic")+
  theme_bw() +
  theme(axis.title.x = element_blank(),         
        panel.border = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())  


#Figure A??? 
#Proportions of countries with granted patents citing flagged articles
Patent_Country <- data.frame("US" = 75,
                             "Denmark" = 6.25,
                             "China" = 3.57,
                             "UK" = 3.57,
                             "Japan" = 2.68,
                             "Germany" = 2.68,
                             "Israel" = 1.79,
                             "Austria" = 1.79,
                             "Switzerland" = 0.89,
                             "Saudi_Arabia" = 0.89,
                             "Hong_Kong" = 0.89)

PC_melt <-melt(Patent_Country, measure.vars = c("US",
                                                "Denmark",
                                                "China",
                                                "UK",
                                                "Japan",
                                                "Germany",
                                                "Israel",
                                                "Austria",
                                                "Switzerland",
                                                "Saudi_Arabia",
                                                "Hong_Kong"))

PC_melt$variable <- factor(PC_melt$variable,
                           levels = c("Hong_Kong",
                                      "Saudi_Arabia",
                                      "Switzerland",
                                      "Austria",
                                      "Israel",
                                      "Germany",
                                      "Japan",
                                      "UK",
                                      "China",
                                      "Denmark",
                                      "US"))


ggplot(PC_melt, mapping = aes(variable, value))+
  geom_bar(stat="identity", alpha=0.8, fill="black")+
  scale_x_discrete(labels=c("Hong Kong (n=1)",
                            "Saudi Arabia (n=1)",
                            "Switzerland (n=1)",
                            "Austria (n=2)",
                            "Israel (n=2)",
                            "Germany (n=3)",
                            "Japan (n=3)",
                            "UK (n=4)",
                            "China (n=4)",
                            "Denmark (n=7)",
                            "US (n=84)"))+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  geom_text(aes(label=value), 
            hjust = -0.18, 
            size = 4,
            position = position_dodge(width = 1))+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab("Country of assignee\n")+
  ylab("\nProportions of all unique granted patents (%)")+
  coord_flip()

#Figure B_v2: Proportions of flagged review references, papers with 1+ errors are merged into a group
## called "Multiple errors".
###SPSS (Fisher's Exact Test Country and Affiliation) Results: Sequence only: p <.001
###Celll only: p <.001, Figure only: p <.001, Multiple errors: p <.001
###Choice of using horizontal bars instead of geom_signif or stat_pvalue_manual (fisher.test(x)) 
###is because we already found the significance level, putting the raw data into R 
###as a data-frame seemed like redoubling work I've already done. 

L_reviews_2 <- data.frame("All_Seq_only" = c(100, NA),
                        "China_Seq_only" = c(22.78, 77.22),
                        "All_Cell_only" = c(81.82, 18.18),
                        "China_Cell_only" = c(42.31, 57.69),
                        "All_Figure_only" = c(100, NA),
                        "China_Figure_only" = c(16.67, 83.33),
                        "All_multiple" = c(66.67, 33.33),
                        "China_multiple" = c(14.28, 85.71),
                        "All_Influenced_Peer_Review" = c(NA, NA),
                        "China_Influenced_Peer_Review" = c(100, NA),
                        "Affil" = c("N", "Y"))
View(L_reviews_2)

M_reviews_2 <- melt(L_reviews_2, id.vars = "Affil", measure.vars = c("All_Seq_only", "China_Seq_only",
                                                                 "All_Cell_only", "China_Cell_only", 
                                                                 "All_Figure_only", "China_Figure_only",
                                                                 "All_multiple", "China_multiple",
                                                                 "All_Influenced_Peer_Review", 
                                                                 "China_Influenced_Peer_Review"))

M_reviews_2$Label = c("Nucleotide sequence \nerror only", "Nucleotide sequence \nerror only", 
                    "Nucleotide sequence \nerror only", "Nucleotide sequence \nerror only",
                    "Cell line \nerror only", "Cell line \nerror only", 
                    "Cell line \nerror only", "Cell line \nerror only", 
                    "Figure \nerror only", "Figure \nerror only", 
                    "Figure \nerror only", "Figure \nerror only",
                    "Multiple errors", "Multiple errors",
                    "Multiple errors", "Multiple errors",
                    "Peer review", "Peer review", 
                    "Peer review", "Peer review")

M_reviews_2$Label <- (factor(M_reviews_2$Label,
                           levels = c("Nucleotide sequence \nerror only",
                                      "Cell line \nerror only",
                                      "Figure \nerror only",
                                      "Multiple errors",
                                      "Peer review")))
M_reviews_2$p = c("***", NA, NA, NA,
                  "***", NA, NA, NA,
                  "***", NA, NA, NA,
                  NA, NA, NA, NA,
                  NA, NA, NA, NA)

View(M_reviews_2)

y = c(105, 105, 105, 105, 
      105, 105, 105, 105,
      105, 105, 105, 105,
      NA, NA, NA, NA,
      NA, NA, NA, NA)

ggplot(M_reviews_2, mapping = aes(variable, value, fill = Affil))+
  geom_col(width = 0.4, alpha = 0.8)+
  geom_text(aes(label=sprintf("%1.f%%", value/1)), colour = "Black", 
            position = position_stack(vjust = 0.5), size = 3.5)+
  facet_wrap(~Label, scales = "free_x")+
  xlab("\nAffiliated Country") + 
  ylab("Percentage of problematic articles (%)")+
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_discrete(breaks = levels(factor(M_reviews_2$variable)), 
                   labels = c("All other Countries \n(n = 9)",
                              "China \n(n = 79)",
                              "All other Countries \n(n = 11)",
                              "China \n(n = 52)", 
                              "All other Countries \n(n = 10)",
                              "China \n(n = 12)",
                              "All other Countires \n(n = 3)",
                              "China \n(n = 28)",
                              "All other Countries \n(n = 0)",
                              "China \n(n = 1)"))+
  scale_fill_manual(values = c("grey", "#5baede"), 
                    labels = c("Publications not from hospitals", 
                               "Publications from hospitals"))+
  theme_bw()+
  geom_text(M_reviews_2, mapping = aes(x = 1.5, y= 108, label = p), size=6)+
  geom_errorbarh(aes(xmax = 2, xmin = 1, y = y), height = 5)+
  theme(legend.position = "top", legend.title = element_blank()) + 
  labs(fill = "")


#Figure B_v2: Proportions of flagged review references, papers with 1+ errors are merged into a group
## called "Multiple errors".
###SPSS (Fisher's Exact Test Country and Affiliation) Results: Sequence only: p <.001
###Celll only: p <.001, Figure only: p <.001, Multiple errors: p <.001
###Choice of using horizontal bars instead of geom_signif or stat_pvalue_manual (fisher.test(x)) 
###is because we already found the significance level, putting the raw data into R 
###as a data-frame seemed like redoubling work I've already done. 

L_reviews_3 <- data.frame("All_Seq_only" = c(100, NA),
                          "China_Seq_only" = c(22.78, 77.22),
                          "All_Cell_only" = c(81.82, 18.18),
                          "China_Cell_only" = c(42.31, 57.69),
                          "All_Figure_only" = c(100, NA),
                          "China_Figure_only" = c(16.67, 83.33),
                          "Affil" = c("N", "Y"))

M_reviews_2 <- melt(L_reviews_2, id.vars = "Affil", 
                    measure.vars = c("All_Seq_only", "China_Seq_only", 
                                     "All_Cell_only", "China_Cell_only", 
                                     "All_Figure_only", "China_Figure_only"))

M_reviews_2$Label = c("Nucleotide sequence \nerror only", "Nucleotide sequence \nerror only", 
                      "Nucleotide sequence \nerror only", "Nucleotide sequence \nerror only",
                      "Cell line \nerror only", "Cell line \nerror only", 
                      "Cell line \nerror only", "Cell line \nerror only", 
                      "Figure \nerror only", "Figure \nerror only", 
                      "Figure \nerror only", "Figure \nerror only")

M_reviews_2$Label <- (factor(M_reviews_2$Label,
                             levels = c("Nucleotide sequence \nerror only",
                                        "Cell line \nerror only",
                                        "Figure \nerror only")))
M_reviews_2$p = c("***", NA, NA, NA,
                  "***", NA, NA, NA,
                  "***", NA, NA, NA)

View(M_reviews_2)

y = c(105, 105, 105, 105, 
      105, 105, 105, 105,
      105, 105, 105, 105)

ggplot(M_reviews_2, mapping = aes(variable, value, fill = Affil))+
  geom_col(width = 0.4, alpha = 0.8)+
  geom_text(aes(label=sprintf("%1.f%%", value/1)), colour = "Black", 
            position = position_stack(vjust = 0.5), size = 3.5)+
  facet_wrap(~Label, scales = "free_x")+
  xlab("\nAffiliated Country") + 
  ylab("Percentage of problematic articles (%)")+
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_discrete(breaks = levels(factor(M_reviews_2$variable)), 
                   labels = c("All other Countries \n(n = 9)",
                              "China \n(n = 79)",
                              "All other Countries \n(n = 11)",
                              "China \n(n = 52)", 
                              "All other Countries \n(n = 10)",
                              "China \n(n = 12)"))+
  scale_fill_manual(values = c("grey", "#5baede"), 
                    labels = c("Publications not from hospitals", 
                               "Publications from hospitals"))+
  theme_bw()+
  geom_text(M_reviews_2, mapping = aes(x = 1.5, y= 108, label = p), size=6)+
  geom_errorbarh(aes(xmax = 2, xmin = 1, y = y), height = 5)+
  theme(legend.position = "top", legend.title = element_blank()) + 
  labs(fill = "")

#Figure B_supplementary??: Proportions of flagged review references with multiple error types sorted
## by type of error + country of origin + hospital affiliation

L_reviews_sup <- data.frame("All_Seq_Cell" = c(NA, 100),
                        "China_Seq_Cell" = c(21.43, 78.57),
                        "All_Seq_Fig" = c(NA, NA),
                        "China_Seq_Fig" = c(NA, 100),
                        "All_Cell_Fig" = c(100, NA),
                        "China_Cell_Fig" = c(16.67, 83.33),
                        "All_three" = c(NA, NA),
                        "China_three" = c(NA, 100),
                        "All_Other" = c(NA, NA),
                        "China_Other" = c(100, NA),
                        "Affil" = c("N", "Y"))
View(L_reviews_sup)

M_reviews_sup <- melt(L_reviews_sup, id.vars = "Affil", measure.vars = c("All_Seq_Cell", "China_Seq_Cell",
                                                                 "All_Seq_Fig", "China_Seq_Fig",
                                                                 "All_Cell_Fig","China_Cell_Fig",
                                                                 "All_three", "China_three",
                                                                 "All_Other", "China_Other"))

M_reviews_sup$Label = c("Nucleotide sequence \n+ Cell line\nerror only", "Nucleotide sequence \n+ Cell line\nerror only", 
                    "Nucleotide sequence \n+ Cell line\nerror only", "Nucleotide sequence \n+ Cell line\nerror only",
                    "Nucleotide sequence \n+Figure\nerror only", "Nucleotide sequence \n+Figure\nerror only", 
                    "Nucleotide sequence \n+Figure\nerror only", "Nucleotide sequence \n+Figure\nerror only",
                    "Cell line\n+Figure\nerror only", "Cell line\n+Figure\nerror only", 
                    "Cell line\n+Figure\nerror only", "Cell line\n+Figure\nerror only",
                    "Nucleotide sequence\n+Cell line + Figure\nerror", "Nucleotide sequence\n+Cell line + Figure\nerror",
                    "Nucleotide sequence\n+Cell line + Figure\nerror", "Nucleotide sequence\n+Cell line + Figure\nerror",
                    "Other", "Other",
                    "Other", "Other")

M_reviews_sup$Label <- (factor(M_reviews_sup$Label,
                           levels = c("Nucleotide sequence \n+ Cell line\nerror only",
                                      "Nucleotide sequence \n+Figure\nerror only",
                                      "Cell line\n+Figure\nerror only", "Nucleotide sequence\n+Cell line + Figure\nerror",
                                      "Other")))
View(M_reviews_sup)

ggplot(M_reviews_sup, mapping = aes(variable, value, fill = Affil))+
  geom_col(width = 0.4, alpha = 0.8)+
  geom_text(aes(label=sprintf("%1.f%%", value/1)), colour = "Black", 
            position = position_stack(vjust = 0.5), size = 3.5)+
  facet_wrap(~Label, scales = "free_x")+
  xlab("\nAffiliated Country") + 
  ylab("Percentages of problematic articles (%)")+
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_discrete(breaks = levels(factor(M_reviews_sup$variable)), 
                   labels = c("All other Countries \n(n = 1)",
                              "China \n(n = 14)",
                              "All other Countries \n(n = 0)",
                              "China \n(n = 8)",
                              "All other Countries \n(n = 2)",
                              "China \n(n = 6)",
                              "All other Countries \n(n = 0)",
                              "China \n(n=1)",
                              "All other Countries \n(n=0)",
                              "China\n(n=1)"))+
  scale_fill_manual(values = c("grey", "#5baede"), 
                    labels = c("Publications not from hospitals", 
                               "Publications from hospitals"))+
  theme_bw()+
  theme(legend.position = "top", legend.title = element_blank()) + 
  labs(fill = "")


#Figure C: Citation network years between publication of index and citation of index

C <- data.frame("Years" = c(0, 1, 2, 3, 4, 5, 6),
                "Neppers" = c(11.98, 33.33, 26.56, 17.70, 4.17, 3.13, 3.13),
                "No" = c(14.61, 37.45, 24.72, 12.73, 6.37, 2.25, 1.87),
                "Yes" = c(7.53, 17.20, 44.08, 20.43, 6.45, 3.23, 1.08))

C_melt <- melt(C, id.vars = "Years", measure.vars=c("Neppers", "No", "Yes"))

View(C_melt)

ggplot(C_melt, aes(Years, value, fill=variable))+
  geom_col(position = position_dodge(0.9), width=0.7, alpha=0.75)+
  geom_text(aes(label=sprintf("%.1f%%", value/1)), colour = "Black", 
            position = position_dodge(width=0.9), size = 3, vjust=-0.5)+
  xlab("\nCitation time (years)") + 
  ylab("Percentages of citing papers per category (%)\n")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  scale_x_continuous(breaks = seq(0, 6, by = 1))+
  scale_fill_manual(values = c("#808080", "#00cc00", "#ff0000"), 
                    labels = c("N/A (n=173)", 
                               "No (n=251)",
                               "Yes (n=87)"),
                    name="Publication is\nproblematic?")+
  scale_colour_manual(values = c("#808080", "#00cc00", "#ff0000"))+
  theme_bw()+
  geom_point(position = position_dodge(0.9))+
  geom_line(aes(linetype = variable), position = position_dodge(0.9),
            show.legend = FALSE)
  #facet_wrap(~variable, labeller = labeller(variable = c("Neppers" = "N/A", 
                                                         #"No" = "No",
                                                         #"Yes" = "Yes")))
ggplot(C_melt, aes(Years, value, fill=variable))+



#Alternatives to graph
C2 <- data.frame("Years" = c(0, 1, 2, 3, 4, 5, 6),
                 "Neppers" = c(33.33, 35.56, 32.28, 39.08, 25.81, 40.00, 50.00),
                 "No" = c(56.52, 55.55, 41.77, 39.08, 54.84, 40.00, 41.67),
                 "Yes" = c(10.15, 8.89, 25.95, 21.84, 19.35, 20.00, 8.33))

C3 <- data.frame("Years" = c(0, 1, 2, 3, 4, 5, 6),
                 "Yes" = c(7.53, 17.20, 44.08, 20.43, 6.45, 3.23, 1.08))

C2_melt <- melt(C2, id.vars = "Years", measure.vars=c("Neppers", "No", "Yes"))

ggplot(C_melt, aes(Years, value, fill=variable))+
  geom_point(position = position_dodge(0.9))+
  geom_line(position = position_dodge(0.9))

ggplot(C2_melt, aes(Years, value, fill=variable))+
  geom_col(width=0.65)+
  geom_text(aes(label=sprintf("%.2f%%", value/1)), colour = "Black", 
            position = position_stack(vjust=0.5), size = 3.5)+
  xlab("\nYears since publication of index") + 
  ylab("Proportions for each flagged status (%)\n")+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  scale_x_continuous(breaks = seq(0, 6, by = 1),
                     labels = c("0\n(n=69)", 
                                "1\n(n=180)",
                                "2\n(n=158)",
                                "3\n(n=87)",
                                "4\n(n=31)",
                                "5\n(n=15)",
                                "6\n(n=12)"))+
  scale_fill_manual(values = c("#80808079", "#00cc0088", "#ff000098"), 
                    labels = c("N/A", 
                               "No sequence\nerrors identified",
                               "Sequence\nerrors identified"),
                    name="Publication has sequence error?")+
  theme_bw()

ggplot(C3, aes(Years, Yes, fill="Yes"))+
  geom_col(width=0.75)+
  xlab("\nYears since publication of index") + 
  ylab("Proportions for each flagged status (%)\n")+
  scale_y_continuous(breaks = seq(0, 50, by = 5))+
  scale_x_continuous(breaks = seq(0, 6, by = 1))+
  scale_fill_manual(values = "#ff000098", 
                    labels = c("N/A", 
                               "No sequence errors identified",
                               "Sequence errors identified"))+
  theme_bw()
  

#Citation network: Location of citations
#X^2 test on SPSS showed: X^2 = 25.584, Likelihood ratio = 26.633
#df = 4, p-value = <.001

G <- data.frame("P" = c(40.0, 6.9, 3.5, 49.6, NA, NA, NA, NA),
                "NP" = c(NA, NA, NA, NA, 34.9, 1.3, 7.4, 56.4),
                "P_text" = c("n=1", "n=2", "n=3", "n=4", 
                             "n=100", "n=200", "n=300", "n=400"),
                "Category" = c("Introduction", "Methods", "Results", "Discussion",
                               "Introduction", "Methods", "Results", "Discussion"))

G_melt <- melt(G, id.vars=c("Category", "P_text"), 
               measure.vars=c("P", "NP"))

G_melt$p = c("***", NA, NA, NA, NA, NA, NA, NA)

G_melt_y = c(105, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA)


View(G_melt)

G_melt$Category <- (factor(G_melt$Category,
                           levels=c("Introduction",
                                    "Methods",
                                    "Results",
                                    "Discussion")))

ggplot(G_melt, aes(variable, value, fill=Category))+
  geom_col(width=0.6)+
  geom_text(aes(label=sprintf("%.1f%%", value/1)), colour = "Black", 
            position = position_stack(vjust=0.5), size = 4.5)+
  #geom_text(aes(label=P_text), 
            #position = position_stack(vjust = c(0.4,0.2,0.3,0.4)), size=3.5)+
  xlab("\n Publication category") + 
  ylab("Percentages of citations (%)\n")+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_x_discrete(labels=c("Citations by \nproblematic papers (n=115)",
                            "Citations by \nnon-problematic (n=298)"))+
  scale_fill_manual(name="Location of citation",
                    values = c("#ffd33c", "#a4ce4e", "#eae7d6", "#5F558F"))+
  theme_bw()

#  annotate("text", x = 2.42, y = 105, label = as.expression(bquote(X^2 ~ "= 25.584")))+
#  geom_text(G_melt, mapping = aes(x = 1.5, y= 108, label = p), size=6)+
#  geom_errorbarh(aes(xmax = 2, xmin = 1, y = G_melt_y), height = 5)

  


