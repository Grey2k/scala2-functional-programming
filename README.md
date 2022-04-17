# Scala 2 sbt project template

## What included?

- [x] Git .gitignore file with IntelliJ / VSCode templates
- [x] Unit tests (scalatest + funsuite) 
- [x] Linter (scalafix)
- [x] Formatter (scalafmt)
- [x] Makefile commands config

## Makefile commands

Build & Run tests

```bash
make 
```

Run with all tests

```bash
make && make run
```

Just run

```bash
make run

# or
sbt run 
```

Run fmt

```bash
make fmt
```

Run tests

```bash
make test

# or
sbt test 
```

Run lint

```bash 
make lint

# or
sbt scalafix 
```
