# rego-mode

Emacs Major mode for working
with [Rego](https://www.openpolicyagent.org/docs/latest/policy-language/) configuration
language.

## Prerequisites

* Make sure that you
  install [opa](https://github.com/open-policy-agent/opa)
  and it's PATH is available to emacs via `exec-path`.

## Demo

![Rego mode in Emacs](https://user-images.githubusercontent.com/737477/77818443-43ce1100-70f8-11ea-8bee-913824f24769.gif "Rego mode in Emacs")

## Features

* Syntax highlighting (Using font lock)
* Basic indentation, commenting
* Automatic formatting on save (Configurable via variable). Uses `opa fmt` for it.
* REPL support

## License

Copyright © 2020 Sibi Prabakaran

Distributed under GNU GPL, version 3.

