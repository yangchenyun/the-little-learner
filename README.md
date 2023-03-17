# The Little Learner

Code wrote while having desserts with [The Little Learner](https://www.goodreads.com/book/show/62294487-the-little-learner)

I have implemented the definitions and concepts taught through the book.

This books are build based on concepts taughted in:
https://github.com/yangchenyun/little-scheme
https://github.com/yangchenyun/seasoned-schemer

## Setup to run the code
The code is written with Emacs, but could be ran and read with Jupyter NoteBook

Dependency:
- Racket
- Jupyter Notebook
- Jupyter Kernel for Racket

```sh
# install racket language
brew install racket

# install dependency - ZeroMQ
brew install zmq

# install racket kernel
raco pkg install iracket

# register jupyter kernel
raco iracket install
```
### workaround for zeromq dylib
Need to update racket to find `libzmq.5.dylib` according to [this issue](https://github.com/rmculpepper/iracket/issues/11#issuecomment-1107448577)

## Generate Jupyter Notebook

    jupytext *.ss --to notebook --set-kernel racket --update

## Extra Implementation

- Extend function in interlude I
- Naive implementation of gradient descent in appendix, without definint tensors
- Correlate in Chapter 13
