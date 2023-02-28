(+
    12
    #+sbcl 13
    #+clisp 14
    #-sbcl 15
    #+(not clisp) 16
    #+(or sbcl clisp) 17)
