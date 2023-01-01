# `splitflap` 

[![CI](https://github.com/otherjoel/splitflap/actions/workflows/push.yml/badge.svg)](https://github.com/otherjoel/splitflap/actions)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.0-4baaaa.svg)](CODE_OF_CONDUCT.md)

🔖⚛️ RSS / Atom feed generation library for Racket.

Everything you supply is validated, so the result is always either a valid feed or an exception.

**Documentation is at <https://joeldueck.com/what-about/splitflap>**

Splitflap requires Racket 8.1 or higher. To install Splitflap from the command line:

    > raco pkg install splitflap

Or using DrRacket: click the **File** menu → **Install Package …** and enter `splitflap`.

If deploying Splitflap in a production environment, you probably want to use `splitflap-lib` instead
of `splitflap`. This will avoid fetching/building the docs, and will greatly reduce the number of
dependencies installed.
