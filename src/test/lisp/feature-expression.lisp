(+
    1
    #+sbcl 10
    #+clisp 100
    #-sbcl 1000
    #+(not clisp) 10000
    #+(or sbcl clisp) 100000)
