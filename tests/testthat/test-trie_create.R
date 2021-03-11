test_that("trie_create: correctly creates an empty trie", {
  trie <- trie_create()

  expect_false(trie@root@is_complete_word)
  expect_length(trie@root@children, 0)
})
