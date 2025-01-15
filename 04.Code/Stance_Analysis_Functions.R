graph_stance <- function(input = "02.Intermediate/CleanedComments.rds",
                         strength_graph = "05.Figures/stance_strength_graph.pdf",
                         bar_graph = "05.Figures/stance_bar_graph.pdf",
                         strength_unique = "05.Figures/stance_strength_unique.pdf",
                         bar_unique = "05.Figures/stance_bar_unique.pdf"){
  comments <- readRDS(input)
  comments <- comments[!is.na(Approve_Score),]
  
  ggplot(comments, aes(Approve_Score)) + 
    geom_histogram() +  
    labs(x = "Probability Approve", y = "Number of Observations") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 10500))
  
  ggsave(strength_graph, units = "in", width = 5, height = 3)
  
  comments[, Stance := factor(x = Stance, levels = c("approves", "disapproves"),
                              labels = c("Approves of Merger", "Disapproves of Merger"))]
  
  ggplot(comments, aes(Stance)) +
    geom_bar() + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + 
    labs(y = "Number of Observations") +
    scale_y_continuous(expand = c(0,0), limits = c(0, 10500))
  
  ggsave(bar_graph, units = "in", width = 5, height = 3)
  
  # Now, Unique Comments Only
  comments.unique <- unique(comments[,.(Comment, Stance, Approve_Score)])
  
  ggplot(comments.unique, aes(Approve_Score)) + 
    geom_histogram() +  
    labs(x = "Probability Approve", y = "Number of Observations") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 800))
  
  ggsave(strength_unique, units = "in", width = 5, height = 3)
  
  
  ggplot(comments.unique, aes(Stance)) +
    geom_bar() + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + 
    labs(y = "Number of Observations") +
    scale_y_continuous(expand = c(0,0), limits = c(0, 800))
  
  ggsave(bar_unique, units = "in", width = 5, height = 3)
  
}

comment_submitted_graph <- function(input = "02.Intermediate/CleanedComments.rds",
                                   output = "05.Figures/stance_submission_timeline.pdf"){
  comment_data <- readRDS(input)
  
  # Remove Withdrawn Comment
  comment_data <- comment_data[Comment != "",]
  
  comment_data[, Day_Posted := pmin(as.numeric(Date - min(Date)),33)]
  comment_data[Stance == "approves of", Stance := "Approves of Merger"]
  comment_data[Stance == "disagrees with", Stance := "Disapproves of Merger"]
  
  ggplot(comment_data, aes(x = Day_Posted, fill = Stance)) +
    geom_histogram(binwidth = 1, center = 0.5) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + 
    labs(y = "Number of Observations", x = "Day Posted") +
    scale_y_continuous(expand = c(0,0), limits = c(0, 4000)) +
    scale_fill_manual(values = c("black", "lightgrey"))
  
  ggsave(output, units = "in", width = 5, height = 3)
}

stance_summary_statistics <- function(input = "02.Intermediate/CleanedComments.rds",
                                      output = "06.Tables/Stance_Taken_Summary.tex"){
  comments_data <- as.data.table(readRDS(input))
  
  process_data <- function(comments){
    approve_score <- five_statistic_row_make("P(Approves)", comments$Approve_Score)
    approve_approve <- five_statistic_row_make("Approving Comment P(Approves)", 
                                               comments[Stance == "approves"]$Approve_Score)
    approve_disapprove <- five_statistic_row_make("Disapproving Comment P(Approves)",
                                                  comments[Stance == "disagrees"]$Approve_Score)
    ny <- five_statistic_row_make("New York Comment", comments$`State/Province` == "NY")
    fl <- five_statistic_row_make("Florida Comment", comments$`State/Province` == "FL")
    ma <- five_statistic_row_make("Massachusetts Comment", comments$`State/Province` == "MA")
    pr <- five_statistic_row_make("Puerto Rico Comment", comments$`State/Province` == "PR")
    obs <- c("Observations", nrow(comments), "", "", "", "")
    return(rbind(approve_score, approve_approve, approve_disapprove, ny, fl, ma, pr, obs))
  }
  
  output_table <- process_data(comments_data)
  title_row <- c("", "Mean", "(SD)", "Minimum", "Median", "Maximum")
  rownames(output_table) <- NULL
  
  # Make Table
  kbl(output_table,
      format = "latex", col.names = title_row,
      escape = TRUE, booktabs = TRUE) %>%
    row_spec(row = 7, hline_after = TRUE) %>%
  save_kable(file = output)
  
}
