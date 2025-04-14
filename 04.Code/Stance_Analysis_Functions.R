comments_handle <- function(input = "01.Input/16.MergerComments/MergerComments.csv",
                            output = "02.Intermediate/CleanedComments.rds"){
  comments <- fread(input)
  
  # A few fields are the same for everyone - drop them.
  comments <- comments[, .(`Document ID`, `Posted Date`, `Is Withdrawn?`,
                           `Title`, `State/Province`, `Zip/Postal Code`,
                           `Country`, `Government Agency`, `Government Agency Type`,
                           `Comment`, `Reason Withdrawn`, `Submitter Representative`)]
  
  # Process Time to be Date
  comments$Date <- ymd(substr(comments$`Posted Date`, start = 1, stop = 10))
  
  # Clean Comments - Remove \n
  comments[, Comment := gsub(pattern = "\n", replacement = " ", x = Comment)]
  comments[, Comment := trimws(Comment)]
  
  # Now, Detect Stance on each Review
  classifier <- transformers$pipeline("zero-shot-classification", 
                                      model = 'MoritzLaurer/DeBERTa-v3-large-mnli-fever-anli-ling-wanli')
  
  template <- "The author of this comment {} of the merger."
  labels <- c("approves", "disapproves")
  
  # Restrict to only non-blank comments
  comments.check <- comments[Comment != "", .(`Document ID`, Comment)]
  comments.check <- unique(comments.check$Comment)
  
  i <- 1
  while(i < length(comments.check)){
    j <- min(length(comments.check), i + 50)
    current_sample <- comments.check[i:j]
    result <- classifier(current_sample, labels, hypothesis_template = template,
                         multi_label = FALSE)
    
    saveRDS(result, paste("02.Intermediate/StanceDetection/", i, "_through_", j,".rds", sep = ""))
    
    print(paste("Finished", i, "through", j, sep = " "))
    i <- j+1
  }
  
  file_head <- "02.Intermediate/StanceDetection/"
  file_list <- list.files(path = file_head)
  current_file <- c()
  for(i in 1:length(file_list)){
    current_file <- c(current_file,readRDS(paste(file_head, file_list[i], sep = "")))
  }
  
  comments.string <- c()
  stance_high <- c()
  stance_low <- c()
  stance_high.score <- c();
  stance_low.score <- c();
  
  for(j in 1:length(current_file)){
    comments.string <- c(comments.string, as.character(unlist(current_file[[j]][1])))
    stance_high <- c(stance_high, as.character(unlist(current_file[[j]][2]))[1])
    stance_low <- c(stance_low, as.character(unlist(current_file[[j]][2]))[2])
    stance_high.score <- c(stance_high.score, as.character(unlist(current_file[[j]][3]))[1])
    stance_low.score <- c(stance_low.score, as.character(unlist(current_file[[j]][3]))[2])
  }
  
  approve_score <- c();
  disapprove_score <- c();
  
  for(j in 1:length(stance_high)){
    if(stance_high[j] == "approves"){
      approve_score <- c(approve_score, stance_high.score[j])
      disapprove_score <- c(disapprove_score, stance_low.score[j])
    } else {
      approve_score <- c(approve_score, stance_low.score[j])
      disapprove_score <- c(disapprove_score, stance_high.score[j])
    }
  }
  
  table <- data.table(Comment = comments.string,
                      Stance = stance_high,
                      Approve_Score = approve_score,
                      Disapprove_Score = disapprove_score)
  
  comments <- merge(comments, table, by.x = "Comment", by.y = "Comment", all.x = TRUE)
  
  comments$Approve_Score <- as.double(comments$Approve_Score)
  comments$Disapprove_Score <- as.double(comments$Disapprove_Score)
  
  
  # Now, for sentiment score
  sentiment_analyzer <- transformers$pipeline("text-classification",
                                              "prosusai/finbert", truncation = TRUE)
  
  result <- c()
  i <- 1
  while(i < length(comments.check)){
    current_sample <- comments.check[i:min(i+49, length(comments.check))]
    result <- c(result, sentiment_analyzer(current_sample))
    print(paste("Finished", i, "through", min(i + 49, length(comments.check)), sep = " "))
    i <- i + 50
  }
  
  sentiments <- c();
  sentiment_probability <- c()
  
  for(i in 1:length(comments.check)){
    sentiments <- c(sentiments, as.character(result[[i]]$label))
    sentiment_probability <- c(sentiment_probability, as.numeric(result[[i]]$score))
  }
  
  sentiment_frame <- data.table(Comment = comments.check,
                                Sentiment = sentiments,
                                Sentiment_Score = sentiment_probability)
  
  comments <- merge(comments, sentiment_frame, by = "Comment", all.x = TRUE)
  
  write_rds(comments, output)
}

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


stance_sentiment_table <- function(input = "02.Intermediate/CleanedComments.rds",
                                   output = "06.Tables/Stance_Sentiment_Table.tex"){
  comments <- as.data.table(readRDS(input))
  
  comments <- comments %>%
    mutate(Duplicate = pmax(duplicated(Comment), 
                            duplicated(Comment, fromLast = TRUE))) %>%
    as.data.table()
  
  # Remove duplicates
  comments <- comments[Duplicate == 0,]
  
  # Approves Row
  approve <- c("Approves", nrow(comments[Stance == "approves" & Sentiment == "positive"]),
               nrow(comments[Stance == "approves" & Sentiment == "neutral"]),
               nrow(comments[Stance == "approves" & Sentiment == "negative"]))
  disapprove <- c("Disapproves", nrow(comments[Stance == "disagrees" & Sentiment == "positive"]),
               nrow(comments[Stance == "disagrees" & Sentiment == "neutral"]),
               nrow(comments[Stance == "disagrees" & Sentiment == "negative"]))
  
  title_row <- c("", "Positive", "Neutral", "Negative")
  table_out <- rbind(approve, disapprove)
  rownames(table_out) <- NULL
  
  # Make Table
  kbl(table_out,
      format = "latex", col.names = title_row,
      escape = TRUE, booktabs = TRUE) %>%
    add_header_above(header = c("Stance" = 1, "Sentiment" = 3)) %>%
    save_kable(file = output)
  
}

stance_summary_statistics <- function(input = "02.Intermediate/CleanedComments.rds",
                                      output = "06.Tables/Stance_Taken_Summary.tex"){
  comments_data <- as.data.table(readRDS(input))
  
  # Mark duplicated comments
  comments_data <- comments_data %>%
    mutate(Duplicate = pmax(duplicated(Comment), 
                            duplicated(Comment, fromLast = TRUE))) %>%
    as.data.table()
  
  
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
  output.unique <- process_data(comments_data[Duplicate == 0,])
  
  title_row <- c("", "Mean", "(SD)", "Minimum", "Median", "Maximum")
  output_table <- rbind(output_table, output.unique)
  rownames(output_table) <- NULL
  
  # Make Table
  kbl(output_table,
      format = "latex", col.names = title_row,
      escape = TRUE, booktabs = TRUE,
      linesep = "") %>%
    group_rows(group_label = "All Comments", start_row = 1, end_row = 8) %>%
    row_spec(row = 7, hline_after = TRUE) %>%
    row_spec(row = 8, hline_after = TRUE) %>%
    group_rows(group_label = "Unique Comments", start_row = 9, end_row = 16) %>%
    row_spec(row = 15, hline_after = TRUE) %>%
    row_spec(row = 16, hline_after = TRUE) %>%
  save_kable(file = output)
  
}
