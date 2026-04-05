# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**When finishing with a task, tests and integration tests need to pass**

## Build & Test Commands

- Build: `sbt compile`
- Test all: `sbt test`
- Test single suite: `sbt "testOnly TsEmitterTest"`
- Format: `sbt scalafmtAll`
- Format check: `sbt scalafmtCheck`

Test framework is munit. Tests are forked and run in parallel (each suite in its own JVM).

## Architecture

scala-ts3 generates TypeScript type definitions from a Scala AST. No macro-based derivation yet (PoC1.scala has a prototype using `derives`, but the active code uses a hand-built AST).

**Core pipeline: TsExpr → TsEmitter → TypeScript string output**

- `TsExpr` — ADT (enum) representing the TypeScript type system: primitives, literals, arrays, tuples, unions, intersections, interfaces, aliases, enums, type references, functions, indexed interfaces. `TsTypeReference` carries an optional `impl` (the resolved type) and `discriminator` (for tagged unions).
- `TsEmitter` — stateless emitter that pattern-matches on `TsExpr`:
  - `emit()` — renders an inline type expression (e.g. `string[]`, `(A | B)`)
  - `emitNamed()` — renders a top-level declaration (`export interface`, `export type`, `export enum`)
  - `discoverNamed()` — walks the type tree to collect all reachable named types; injects discriminator fields into tagged union members
  - `emitAll()` — discovers + emits all named types from a root expression
- `StyleOptions` — controls output style (semicolons, tagged union discriminator field name)

Key design choices:
- `ListMap` preserves field ordering in interfaces, function args, and enum entries.
- Optional fields: a union containing `TsUndefined` emits as `field?: T` (undefined is stripped).
- Tagged unions: `TsTypeReference` with a `discriminator` gets a literal discriminator field injected during `discoverNamed()`.

## Style Rules (apply to all output: code, comments, docs, commit messages)

- Be succinct. Say it once, say it short.
- No redundant comments. If the code is clear, don't comment it.
- No filler text, no restating the obvious, no "this function does X" before a function named X.
- When asked to "eliminate repetition" or "remove redundant comments", take it literally.
- No fluff, not fuzzy
- Don't remove FIXME/TODO comments unless the user explicitly asks. They track planned work.

## Scala Conventions

- Scala 3, curly braces. No significant whitespace.
- Immutable by default. Never mutate shared state.
- scalafmt handles formatting — don't manually format.
- No sbt-tpolecat or `-Werror`. Warnings don't fail the build, but fix them anyway — don't leave warnings behind.
- Prefer composition over inheritance.
- Functional core, imperative shell. Pure functions by default; isolate side effects at boundaries.
- Follow Scala community conventions (camelCase, standard idioms). Don't invent project-specific naming schemes.
- If a function is pure and doesn't use ZIO, use scalatest for tests.

## Before Writing Code

- Check if a rough design or architecture decision is needed first. Ask if unclear.
- Design around data structures. Get the data model right before implementing logic around it.
- Develop the critical path first — the hard, fundamental part stripped to essentials.
- Don't introduce abstractions preemptively. Duplication is cheaper than the wrong abstraction. Let patterns emerge.
- Think about module and package structure before creating new packages.
- Don't create fine-grained packages with one class each ("categoritis"). Organise by feature, not by category.
- Don't introduce DTOs if not needed. E.g. if kafka models are generated from an avro spec, you can map directly to domain models without any DTO.

## Writing Code

- One level of abstraction per function. Don't mix high-level orchestration with low-level details.
- Functions should fit on a screen (~80–100 lines max).
- Group code by functional cohesion (things that contribute to the same task), not by class-per-responsibility.
- Keep dependencies minimal. Don't add libraries for trivial tasks.
- No tactical DDD patterns or hexagonal architecture unless explicitly requested.
- If you don't know a library, read its docs or source on GitHub. Don't guess the API.

## Testing

- Write integration and e2e tests early. They catch what AI misses — AI reasons locally, tests verify globally.
- For UI: write Selenium e2e tests first. Use them to verify and self-correct.
- One test per desired external behavior, plus one test per bug.
- Tests target the API of a cohesive unit — not individual classes or internal methods.
- Use tests to find edge cases.
- Don't write tests before the implementation exists (no TDD).

## APIs and Interfaces

- Treat APIs as permanent. Don't change signatures without explicit approval.
- Be strict in what you accept and what you return. Don't silently tolerate malformed input.
- Minimize observable behavior surface — anything observable will be depended on.

## Conventions and Consistency

- Follow existing patterns in the codebase. When in doubt, match what's already there.
- Global project structure matters. Local style within a function or module is flexible.
- If a convention exists (naming, structure, patterns), follow it. Don't introduce alternatives.

## AI Workflow

- Don't over-engineer prompts or plans. Work with what's given plainly.
- After producing code, expect it to be reviewed and challenged. ~50% of output will need major changes.
- Never commit secrets, credentials, or API keys.
- When fixing bugs: reproduce with a test first, then fix.
- If a task is ambiguous, ask one clarifying question rather than guessing.
