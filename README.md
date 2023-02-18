`Breadcrumbs` is a lightweight distributed tracing library for Haskell with the `Effectful` effects system.

In simpler terms, it's a logging framework for execution of a program that aims to use the power of fancy microservice tools such as `Zipkin` to produce fancy
hierarchal flows. Consider it a stack trace/flame graph, except the unit of work is not function call but some higher structure in your program.

Currently only used in [Yaifl](https://github.com/ppkfs/yaifl) for tracing text adventure logic.