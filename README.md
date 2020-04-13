# rego-mode

[![MELPA](https://melpa.org/packages/rego-mode-badge.svg)](https://melpa.org/#/rego-mode)

Emacs Major mode for working
with [Rego](https://www.openpolicyagent.org/docs/latest/policy-language/) configuration
language.

## Features

* Syntax highlighting (Using font lock)
* Basic indentation, commenting
* Automatic formatting on save (Configurable via variable). Uses `opa fmt` for it.
* REPL support. The function `rego-repl-show` will load a plain
  REPL. You can also use `rego-repl-with-data` to pass file or
  directory which will be loaded to the REPL.

## Demo

![Rego mode in Emacs](https://user-images.githubusercontent.com/737477/77818443-43ce1100-70f8-11ea-8bee-913824f24769.gif "Rego mode in Emacs")

## Prerequisites

* Make sure that you install
  [opa](https://github.com/open-policy-agent/opa) and it's available
  in your system.

## Usage

It's available via [MELPA](https://melpa.org/#/rego-mode) and the
following `use-package` configuration can be used:

``` emacs-lisp
(use-package rego-mode
  :ensure t
  :custom
  (rego-repl-executable "/home/sibi/bin/opa")
  (rego-opa-command "/home/sibi/bin/opa"))
```

You would have to modify the above elisp code to have appropriate
paths.

## License

Copyright © 2020 Sibi Prabakaran

Distributed under GNU GPL, version 3.
