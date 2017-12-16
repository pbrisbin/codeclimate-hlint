**NOTE**: The machinery needed to run HLint as a Code Climate engine was [merged upstream](https://github.com/ndmitchell/hlint/pull/383). Please see [https://github.com/ndmitchell/hlint](https://github.com/ndmitchell/hlint) for details and to report Issues. This project has been archived.

---

[![Circle CI](https://circleci.com/gh/pbrisbin/codeclimate-hlint.svg?style=svg)](https://circleci.com/gh/pbrisbin/codeclimate-hlint) [![Code Climate](https://codeclimate.com/github/pbrisbin/codeclimate-hlint/badges/gpa.svg)](https://codeclimate.com/github/pbrisbin/codeclimate-hlint)

Code Climate Engine to run HLint

## Installation

```
git clone https://github.com/pbrisbin/codeclimate-hlint
cd codeclimate-hlint
make release
```

## Usage

**.codeclimate.yml**

```yml
engines:
  hlint:
    enabled: true
```

```
codeclimate analyze
```

## Development

*Install [stack][]*

[stack]: https://github.com/commercialhaskell/stack

```
stack setup
stack build
stack test
```
