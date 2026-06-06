# scala-tstype — generate TypeScript types from Scala 3

`scala-tstype` derives TypeScript type definitions from your Scala 3 models, so a
TypeScript frontend can share the exact shapes your backend produces. Case classes
become `interface`s, enums and sealed traits become unions, and the usual Scala types
(`Option`, `Either`, collections, `Map`, tuples, `java.time.*`, …) map to their natural
TypeScript equivalents.

## Installation

```scala
// Scala CLI
//> using dep org.bargsten::scala-tstype:0.1.0
// sbt
// "org.bargsten" %% "scala-tstype" % "0.1.0"
```

## Usage

Derive a `TsType[T]` for each model you want to export, then emit them to a `.ts` file:

```scala
import org.bargsten.tstype.*

enum Role:
  case Admin, Editor, Reader

case class User(id: Long, name: String, role: Role, email: Option[String])

@main
def gen(): Unit =
  val types = Seq(
    TsType.derive[Role],
    TsType.derive[User],
  )
  TsExport.toTypescriptFile(types, "user.ts")
```

produces `user.ts`:

```typescript
export type Role = ("Admin" | "Editor" | "Reader")

export interface User {
  id: number
  name: string
  role: Role
  email?: string
}
```

A complete, runnable example (nested references, `Map`, a tagged union, a wrapper type,
and the syntax DSL) lives in [`example/`](example/):

```bash
cd example && scala-cli run .   # writes users.ts, blog.ts, notifications.ts
```

Or from the repo root: `make run-example`.

## Features

- **Primitives & standard types** — `Boolean`/`String`/numeric types, `Option` (emits as
  `field?: T`), `Either` (union), `Seq`/`Set`/`Array` (`T[]`), `Map[K, V]`
  (`{ [key: K]: V }`), tuples, `UUID`/`URI`/`java.time.*` (string), and `java.lang.Enum`.
- **Case classes** → `export interface`.
- **Enums & sealed traits** → unions. Parameterless cases become string-literal unions;
  cases with fields become tagged unions with a configurable discriminator field
  (`StyleOptions(taggedUnionDiscriminator = Some("kind"))`).
- **Custom mappings** — provide your own `given TsType[X]` to override derivation, e.g.
  map a domain id type to a string:
  ```scala
  given TsType[UserId] = TsType(TsExpr.TsString)
  ```
- **Wrapper projection** — `TsType.sameAs[Wrapper, String]` emits a wrapper type as the
  underlying primitive. `TsType.external[T]("Name")` references a type defined elsewhere.
- **Syntax DSL** (`import org.bargsten.tstype.syntax.*`) — reshape a derived interface:
  `t - "field"`, `t -- Seq("a", "b")`, `t + ("extra" -> TsType.derive[Extra])`.
- **Name-clash resolution** — types sharing a simple name but different fully-qualified
  names are disambiguated automatically.

## Patterns & edge cases

Lessons from generating a real multi-module API.

**Collect ID/wrapper mappings in a per-module `tsi` object.** Opaque or wrapper id types
have no natural derivation, so map each to its underlying primitive once and export the
givens for downstream modules to import:

```scala
// user/api/tsi.scala
object tsi {
  given TsType[UserId]  = TsType(TsString)
  given TsType[GroupId] = TsType(TsString)
}

// common/tsi.scala — pull every module's mappings into scope before deriving
import org.bargsten.gravel.user.api.tsi.given
import org.bargsten.gravel.chat.api.tsi.given
```

A given must be in scope wherever a case class referencing that type is derived. Missing
one surfaces as a compile error at the `derive` call, not at runtime.

**Split output by feature.** Group `TsType` values per module and emit a file each — one
`toTypescriptFile` call per group keeps the generated `.ts` files aligned with your
frontend's module boundaries:

```scala
TsExport.toTypescriptFile(userTypes, "ts/user.ts")
TsExport.toTypescriptFile(chatTypes, "ts/chat.ts")
```

**Mix generated types with hand-written TypeScript.** `TsExport.toTypescript` returns the
declarations as a string instead of writing a file, so you can append constants the client
validates against — sourced from the same place the server enforces them:

```scala
val allowed = MediaType.values.map(t => s"\"${t.value}\"").mkString(", ")
val body =
  "// DO NOT EDIT: generated file by scala-tstype\n\n" +
    TsExport.toTypescript(assetTypes) +
    s"\nexport const ALLOWED: ReadonlySet<string> = new Set([$allowed])\n"
Files.writeString(Paths.get("ts/asset.ts"), body)
```

`toTypescriptFile` prepends a `DO NOT EDIT` header by default; override it with the `header`
argument or pass `header = ""` to drop it.

**Enum value vs. case name.** A Scala 3 `enum` derives a union of its *case names*. If the
serialized form differs (a custom `value`, a `java.lang.Enum` from another library), provide
a `given TsType[X]` that emits the literals the wire actually carries.

## Build

```bash
sbt compile   # Compile
sbt test      # Run tests
sbt console   # REPL
```

Or via `make`: `make build`, `make test`, `make run-example`, `make publish-local`.

## Release Strategy

We use
[Early SemVer](https://www.scala-lang.org/blog/2021/02/16/preventing-version-conflicts-with-versionscheme.html#early-semver-and-sbt-version-policy).

## Acknowledgements

Initially inspired by [scala-tsi](https://github.com/scala-tsi/scala-tsi).

## License

Apache License 2.0 — see [LICENSE](LICENSE).
