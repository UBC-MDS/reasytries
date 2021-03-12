test_that("trie_add: return TRUE when adding a valid word", {
  trie <- tr(list())

  is_added <- trie_add(trie, "hi")

  expect_true(is_added)
})


test_that("trie_add: report error on input word containing characters other than letters", {
  trie <- tr(list())

  # Input word is an empty string
  expect_error(trie_add(trie, ""))

  # Input word is a string containing numbers
  expect_error(trie_add(trie, "5678"))

  # Input word is a string containing symbols
  expect_error(trie_add(trie, "abc&#^"))
})


test_that("trie_add: correctly add word", {
  trie <- tr(list())

  trie_add(trie, "hi")

  #   root
  #    |
  #    h
  #   /
  #  i

  expect_true(exists("h", envir = trie@root@children))
  expect_true(exists("i", envir = trie@root@children$h@children))
  expect_false(trie@root@children$h@is_complete_word)
  expect_true(trie@root@children$h@children$i@is_complete_word)

})


test_that("trie_add: correctly create new nodes", {
  trie <- tr(list())

  trie_add(trie, "hi")
  trie_add(trie, "he")

  #   root
  #    |
  #    h
  #   / \
  #  i   e

  expect_true(exists("h", envir = trie@root@children))
  expect_true(exists("i", envir = trie@root@children$h@children))
  expect_true(exists("e", envir = trie@root@children$h@children))
  expect_false(trie@root@children$h@is_complete_word)
  expect_true(trie@root@children$h@children$e@is_complete_word)

})

test_that("trie_add: correctly adds to existing nodes of children", {
  trie <- tr(list())

  trie_add(trie, "hi")
  trie_add(trie, "hip")

  #   root
  #    |
  #    h
  #   /
  #  i
  #  |
  #  p

  expect_true(exists("h", envir = trie@root@children))
  expect_true(exists("i", envir = trie@root@children$h@children))
  expect_true(exists("p", envir = trie@root@children$h@children$i@children))
  expect_true(trie@root@children$h@children$i@children$p@is_complete_word)
})
