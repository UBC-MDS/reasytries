
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reasytries

<!-- badges: start -->

<!-- badges: end -->

The pyeasytries package contains classes and functions that efficiently
store and search words passed by the user based on a trie data
structure.

This package was developed as a project for UBC MDS-Vancouver program
DSCI 524 course.

## Installation

You can install the released version of reasytries from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("reasytries")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UBC-MDS/reasytries")
```

## Overview

Storing and searching words can be expensive in terms of time and
computing: for example, using a balanced binary search tree, the time
complexity for searching a word is \(O(mlogn)\), where \(m\) is the
length of the word and \(n\) is the number of keys in the tree. However,
with the trie data structure, the time complexity for searching can be
reduced to \(O(m)\). The `reasytries` package is a simple tool to aid
word insertion, deletion, and searching in the trie data structure.
Users can pass any words to be stored for later-on searching or printing
with certain prefix, and even modify the words in storage.

## Features (TO BE MODIFIED BY Mitchie)

  - `Class TrieNode`: creates a trie node.
  - `Class Trie`: initializes a trie data structure with or without
    adding any words.

Within `Class Trie`:

  - `contain(self, word)` function takes in a word and searches through
    the trie structure to check if it has been stored in the trie
    already.

  - `find_prefix(self, prefix)` function takes in a prefix string and
    searches through the trie structure and returns a list of all words
    stored with the given prefix.

  - `add(self, word_to_add)` function takes in a word and stores each
    letter of the new word in the trie data structure.

  - `delete(self, word_to_delete)` function takes in a word and deletes
    the letters of the word if they are contained in the trie already.

## R Ecosystem (TO BE MODIFIED BY Jordan)

This `pyeasytries` package aims to simplify and speed up the process of
searching through a fixed dictionary. In addition to time complexity
benefits, a tries data structure also allows to search for given words
inside the fixed dictionary when given a prefix. Users who are building
a search/filter bar from a fixed dictionary will find this package
useful\! As trie is a famous data structure, there are currently some
similar packages in Python such as
[pygtrie](https://pypi.org/project/pygtrie/).
