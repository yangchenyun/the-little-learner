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