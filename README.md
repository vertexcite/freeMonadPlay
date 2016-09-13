# freeMonadPlay
Playing with the Free Monad example from http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

To build and run:
```
$ stack build
$ stack exec freeMonadPlay-exe
```

(You need `stack` from http://haskellstack.org )

Sample run:
```
Listing of program
input gets 'x'
outputs 'x'
bell
done

Run program requires user input, exception unless of form 'x' (where x can be any single char, and single quotes are required).
';'
';'
BELL

Monad laws test result: pass
```
