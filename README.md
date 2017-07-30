# ccCell - cell automaton playground

A few weeks ago I read an [article](http://blog.stephenwolfram.com/2017/06/oh-my-gosh-its-covered-in-rule-30s/)
in Stephen Wolframs blog about a train station being covered in a pattern
generated with an elementary cell automaton. This got me interested in the topic
and as I am currently learning Haskell it came to my mind that this would make
for a great learning playground.

# Samples

You can find some sample images of the automatons after N
generations in the images/N/ directories.

# Implementation

As mentioned I am using this project to pick up some Haskell and to play around
a little.

I am using cabal to build the project. It also handles the dependencies nicely.

## Usage

If you are using cabal you can use the following to generate images of all 255
rules for this simple automatons after 100 generations. They will be output to
images/100

```
cabal run -- 100
```
