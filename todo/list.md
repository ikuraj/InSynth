## TODO list

* implement deffer and interactive loggers properly

### Optimizations

* Transformer can group declarations by type and then produce more optimized streamables (for both atoms and functions) - now, each function invocation, even with the same type, will result in its own separate tree of streamables

### Enhancements

* declaration (instead of just being abstract) can point to a specific declaration - requires change (modification) of the solver