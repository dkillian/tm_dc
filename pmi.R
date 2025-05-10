# Pointwise mutual information (PMI)


# pmi ex 1 ---- 

# "new" occurs in 1,000 sentences
# "york" occurs in 800 sentences
# "new york" occurs in 600 sentences
# 10,000 sentences

p_new <- 1e3/1e4 # .1
p_york <- 8e2/1e4 # .08
p_newyork <- 6e2/1e4 # .06

p_newyork / (p_new*p_york) # 7.5

log(p_newyork / (p_new*p_york)) # 2


# pmi ex 2 ---- 

texts <- c(
    "new york is a big city",
    "I love new york",
    "york is in the UK too",
    "the city is beautiful",
    "new places are exciting"
)

texts

library(text2vec)

# Tokenizer

tokens <- word_tokenizer(tolower(texts))
tokens

# Create vocabulary

?itoken
it <- itoken(tokens)
it

vocab <- create_vocabulary(it)
vocab

vectorizer <- vocab_vectorizer(vocab)


# Create Term Co-occurrence Matrix (TCM)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
tcm


# Convert to matrix
tcm_mat <- as.matrix(tcm)

# Total count of all co-occurrences
total <- sum(tcm_mat)

# Marginal probabilities
row_totals <- rowSums(tcm_mat)
col_totals <- colSums(tcm_mat)

# PMI computation function
pmi_matrix <- function(tcm, total, row_totals, col_totals) {
    pmi <- matrix(0, nrow=nrow(tcm), ncol=ncol(tcm),
                  dimnames = list(rownames(tcm), colnames(tcm)))
    for (i in seq_len(nrow(tcm))) {
        for (j in seq_len(ncol(tcm))) {
            if (tcm[i, j] > 0) {
                pij <- tcm[i, j] / total
                pi <- row_totals[i] / total
                pj <- col_totals[j] / total
                pmi[i, j] <- log2(pij / (pi * pj))
            }
        }
    }
    return(pmi)
}

pmi_mat <- pmi_matrix(tcm_mat, total, row_totals, col_totals)
pmi_mat

pmi_mat["new", "york"]
pmi_mat["new", "city"]
pmi_mat["city", "beautiful"]



compute_pmi <- function(texts, window_size = 5L, min_count = 1L) {
    library(text2vec)
    library(data.table)
    library(Matrix)
    
    # Tokenize and create iterator
    tokens <- word_tokenizer(tolower(texts))
    it <- itoken(tokens)
    
    # Build vocabulary
    vocab <- create_vocabulary(it)
    vocab <- prune_vocabulary(vocab, term_count_min = min_count)
    vectorizer <- vocab_vectorizer(vocab)
    
    # Create Term Co-occurrence Matrix (TCM)
    tcm <- create_tcm(it, vectorizer, skip_grams_window = window_size)
    tcm_mat <- as.matrix(tcm)
    
    # Compute totals and probabilities
    total <- sum(tcm_mat)
    row_totals <- rowSums(tcm_mat)
    col_totals <- colSums(tcm_mat)
    
    # Create candidate word pairs
    pairs <- CJ(word1 = rownames(tcm_mat), word2 = colnames(tcm_mat))[word1 != word2]
    
    # Extract counts
    pairs[, count := mapply(function(w1, w2) tcm_mat[w1, w2], word1, word2)]
    pairs <- pairs[count > 0]
    
    # Step 1: Compute probabilities
    pairs[, `:=`(
        p_word1 = row_totals[word1] / total,
        p_word2 = col_totals[word2] / total,
        p_joint = count / total
    )]
    
    # Step 2: Compute PMI
    pairs[, pmi := log2(p_joint / (p_word1 * p_word2))]
    
    # Return sorted result
    pairs[order(-pmi), .(word1, word2, count, pmi)]
}


texts
pmi_df <- compute_pmi(texts)

head(pmi_df)

library(igraph)

plot_pmi_network <- function(pmi_df, top_n = 20, min_pmi = 0) {
    library(igraph)
    library(ggraph)
    library(ggplot2)
    
    # Filter for top PMI pairs
    top_pairs <- pmi_df[pmi >= min_pmi][order(-pmi)][1:min(.N, top_n)]
    
    # Create igraph object
    g <- graph_from_data_frame(
        d = top_pairs[, .(word1, word2, weight = pmi)],
        directed = FALSE
    )
    
    # Plot with ggraph
    ggraph(g, layout = "fr") +
        geom_edge_link(aes(width = weight), edge_alpha = 0.6) +
        geom_node_point(size = 5, color = "steelblue") +
        geom_node_text(aes(label = name), vjust = 1.5, size = 4) +
        theme_minimal() +
        labs(title = "Top PMI Word Pairs Network", subtitle = paste("Top", top_n, "Pairs"))
}

plot_pmi_network(pmi_df, top_n = 10, min_pmi = 0.5)

# chi square test ---- 

# Create a matrix of observed frequencies
observed <- matrix(c(30, 20, 50, 40), nrow = 2, byrow = TRUE)
rownames(observed) <- c("Male", "Female")
colnames(observed) <- c("Product A", "Product B")
:contentReference[oaicite:34]{index=34}

# Display the contingency table
:contentReference[oaicite:36]{index=36}
observed

# Perform the Chi-Square Test
test_result <- chisq.test(observed)
:contentReference[oaicite:45]{index=45}
# View the test results
:contentReference[oaicite:47]{index=47}
test_result

# Expected is ( row_total*column_total ) / grand_total

e_ma <- ( rowSums(observed)[1] * colSums(observed)[1] ) / sum(observed)
e_ma

# Chi-square statistic is ( observed - expected )^2 / expected 

ch_ma <- (observed[1,1] - e_ma)^2 / e_ma
ch_ma

# degrees of freedom is (nrow-1) * (ncol-1)

observed
30/50 * 30/80 # .225
.375 * .6









