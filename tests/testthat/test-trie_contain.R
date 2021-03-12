test_that("trie_contain: return FALSE from an empty trie", {
    trie <- tr(list())
    #   root

    contain_word <- trie_contain(trie, "ad")
    # trie_contain should return FALSE

    expect_false(contain_word)
    expect_length(trie@root@children, 0)
})

test_that("trie_contain: return FALSE from a trie without the target word", {
    trie <- tr(list(
        a = tn(FALSE, list(
            d = tn(TRUE, list())
        ))))
    #   root
    #    |
    #    a
    #   /
    #  d (complete_word)

    contain_word <- trie_contain(trie, "bc")
    #   trie_contain should return FALSE


    expect_false(contain_word)
    expect_false(exists("b", envir = trie@root@children))
})

test_that("trie_contain: return FALSE from a trie containing all letters but not the completed word", {
    trie <- tr(list(
        a = tn(FALSE, list(
            c = tn(FALSE, list(
                t = tn(TRUE, list())
            ))
        ))))
    #     root
    #      |
    #      a
    #     /
    #    c
    #   /
    #  t (complete word)

    contain_word <- trie_contain(trie, "ac")
    #   trie_contain should return FALSE


    expect_false(contain_word)
    expect_true(exists("a", envir = trie@root@children))
    expect_true(exists("c", envir = trie@root@children$a@children))
    expect_false(trie@root@children$a@children$c@is_complete_word)
})


test_that("trie_contain: return TRUE from a trie with the target word completed", {
    trie <- tr(list(
        a = tn(FALSE, list(
            d = tn(TRUE, list())
        ))))
    #   root
    #    |
    #    a
    #   /
    #  d (complete word)

    contain_word <- trie_contain(trie, "AD")
    #   trie_contain should return TRUE


    expect_true(contain_word)
    expect_true(exists("a", envir = trie@root@children))
    expect_true(exists("d", envir = trie@root@children$a@children))
    expect_true(trie@root@children$a@children$d@is_complete_word)
})

test_that("trie_contain: report error on invalid argument type", {
    trie <- tr(list())

    # Invalid argument type for word
    expect_error(trie_contain(trie, 5678))

    # Invalid argument type for trie
    expect_error(trie_contain("a string", "abc"))
})

test_that("trie_contain: report error on input word containing characters other than letters", {
    trie <- tr(list())

    # Input word is an empty string
    expect_error(trie_delete(trie, ""))

    # Input word is a string containing numbers
    expect_error(trie_contain(trie, "5678"))

    # Input word is a string containing symbols
    expect_error(trie_contain(trie, "abc&#^"))
})
