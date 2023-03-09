# The Little Learner

[The Little Learner](https://www.goodreads.com/book/show/62294487-the-little-learner) working code.

## Setup to run the code
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
May need to update racket to find `libzmq.5.dylib` according to [this issue](https://github.com/rmculpepper/iracket/issues/11#issuecomment-1107448577)

## Generate Jupyter Notebook

    jupytext *.ss --to notebook --set-kernel racket --update
