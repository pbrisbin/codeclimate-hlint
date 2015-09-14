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
