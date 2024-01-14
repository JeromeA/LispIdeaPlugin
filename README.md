# LispIdeaPlugin

An IntelliJ plugin to support Common Lisp.

## Approach

This plugin tries to support Common Lisp through static analysis. It's more work, but it allows better real-time features:
* As soon as a name is changed, all the references and usages are updated in real time.
* As soon as anything changes, it knows exactly what information just became stale.
* It knows about completions even in an incomplete SEXP.
* It can display modern IDE information in real time. Showing error messages in the code, or highlighting unused variables requires knowing what changes in the file can invalidate this information.

A much more advanced plugin for Common Lisp is [SLT](https://github.com/Enerccio/SLT). One of the reasons it's more advanced is because it doesn't need to be that smart: it just queries SLIME on demand.

## Limitations

* It's still at a very early development stage. It's unlikely to work on anything but my own personal projects.
* It doesn't support any package or system manager.
* It doesn't work well with macros that are not part of Common Lisp.
* It doesn't have a debugger.

