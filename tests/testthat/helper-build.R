tn <- function(complete_word, children_mapping) {
  new(
    "trie.node",
    is_complete_word = complete_word,
    children = rlang::new_environment(data = children_mapping)
  )
}

tr <- function(root_children) {
  new("trie",
      root = tn(complete_word = FALSE, children_mapping = root_children))
}
