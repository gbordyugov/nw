# NW

A toy implementaion of [the Needleman-Wunsch algorithm](https://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm#Advanced_presentation_of_algorithm) in Haskell using dynamics programming with lazy arrays, motivated by [this page](http://jelv.is/blog/Lazy-Dynamic-Programming/).

 - calculates only one alignment with top scope
 - cannot distinguish between gap opening penalty and gap extension penalty
