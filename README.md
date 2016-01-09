[![Circle CI](https://circleci.com/gh/pbrisbin/codeclimate-hlint.svg?style=svg)](https://circleci.com/gh/pbrisbin/codeclimate-hlint) [![Code Climate](https://codeclimate.com/github/pbrisbin/codeclimate-hlint/badges/gpa.svg)](https://codeclimate.com/github/pbrisbin/codeclimate-hlint)

Code Climate Engine to run HLint

## Installation

```
git clone https://github.com/pbrisbin/codeclimate-hlint
cd codeclimate-hlint
docker build -t codeclimate/codeclimate-hlint .
```

## Usage

**.codeclimate.yml**

```yml
engines:
  hlint:
    enabled: true
```

```
codeclimate analyze --dev
```

## Development

*Install [stack][]*

[stack]: https://github.com/commercialhaskell/stack

```
stack setup
stack build
stack test
```
