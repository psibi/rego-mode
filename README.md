# rego-mode

Emacs Major mode for working
with [Rego](https://www.openpolicyagent.org/docs/latest/policy-language/) configuration
language.

## Prerequisites

* Make sure that you
  install [opa](https://github.com/open-policy-agent/opa)
  and it's PATH is available to emacs via `exec-path`.

## Demo

![Rego mode in Emacs](https://user-images.githubusercontent.com/737477/77818161-47609880-70f6-11ea-9137-7a227f9f25f1.gif "Rego mode in Emacs")

## Features

* Syntax highlighting (Using font lock)
* Basic indentation, commenting
* Automatic formatting on save (Configurable via variable). Uses `opa fmt` for it.
* REPL support

## License

Copyright © 2020 Sibi Prabakaran

Distributed under GNU GPL, version 3.

