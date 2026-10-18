# scala-tstype — generate TypeScript types from Scala 3

`scala-tstype` derives TypeScript type definitions from your Scala 3 models, so a
TypeScript frontend can share the exact shapes your backend produces. Case classes
become `interface`s, enums and sealed traits become unions, and the usual Scala types
(`Option`, collections, `Map`, tuples, `java.time.*`, …) map to their natural
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

Or let the model derive it, and pass `summon[TsType[User]]`:

```scala
case class User(id: Long, name: String, role: Role, email: Option[String]) derives TsType
```

Declarations come in the order you pass the types, each followed by the types it
references (depth-first, each emitted once), so adding a type doesn't reshuffle the file.

A complete, runnable example (nested references, `Map`, a tagged union, a wrapper type,
and the syntax DSL) lives in [`example/`](example/):

```bash
cd example && scala-cli run .   # writes users.ts, blog.ts, notifications.ts
```

Or from the repo root: `make run-example`.

## Features

- **Primitives & standard types** — `Boolean`/`String`/numeric types, `Char` (string),
  `Option` (`field?: T` as a case class field, `T | null` elsewhere),
  `Seq`/`Set`/`Array` (`T[]`), `Map[K, V]` (`{ [key: K]: V }`), tuples,
  `UUID`/`URI`/`java.time.*` (string), and `java.lang.Enum`.
- **Valid output** — string literals are escaped; field names that aren't identifiers are
  quoted (`"content-type": string`).
- **Case classes** → `export interface`.
- **Enums & sealed traits** → unions. Parameterless cases become string-literal unions;
  cases with fields become tagged unions with a configurable discriminator field
  (`StyleOptions(taggedUnionDiscriminator = Some("kind"))`). The field is added in the union
  only, `type Shape = ((Circle & { type: "Circle" }) | ...)`, so a `Circle` used directly
  stays without it, as JSON codecs write it.
- **One declaration per name** — two different types that end up with the same TypeScript
  name (e.g. `Page[A]` and `Page[B]`) make `toTypescript` throw instead of emitting both.
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

## Matching your JSON codec

scala-tstype describes JSON; it doesn't produce any. The types are only right if your JSON
library writes what they describe:

| Scala | JSON the codec must write | TypeScript |
|---|---|---|
| field names | as declared | same |
| `Option[T]` field | omitted when `None` | `field?: T` |
| `Option[T]` elsewhere (`Seq[Option[T]]`, map values) | `null` (zio-json omits map entries) | `T \| null` |
| `Char` | `"c"` | `string` |
| `Long`, `BigInt`, `BigDecimal` | a number | `number`: `JSON.parse` loses precision above 2^53 |
| `Seq[T]`, fields with defaults | always written, also when empty/default | required |
| enum with only parameterless cases | `"Red"` | `"Red" \| "Green"` |
| sealed trait / enum with fields | `{"type": "Circle", ...}`: flat, `type` = simple class name | tagged union on `type` |
| a member used directly (`main: Circle`) | `{"r": 1.0}`: no tag | `Circle` |

### Example

```scala
sealed trait Shape
case class Circle(r: Double) extends Shape
case class Square(side: Double) extends Shape
enum Color { case Red, Green }
case class Drawing(main: Circle, shapes: Seq[Shape], color: Color, title: Option[String])

TsExport.toTypescriptFile(Seq(TsType.derive[Drawing]), "ts/drawing.ts")
```

```typescript
export interface Drawing {
  main: Circle
  shapes: Shape[]
  color: Color
  title?: string
}

export interface Circle {
  r: number
}

export type Shape = ((Circle & { type: "Circle" }) | (Square & { type: "Square" }))

export interface Square {
  side: number
}

export type Color = ("Red" | "Green")
```

`Drawing(Circle(1), Seq(Square(2)), Color.Red, None)` on the wire:

```json
{"main":{"r":1.0},"shapes":[{"type":"Square","side":2.0}],"color":"Red"}
```

The `type` field only exists in the JSON. In Scala the case class is the tag (`case Circle(r) =>`).
In TypeScript, comparing `type` narrows the union:

```typescript
function area(s: Shape): number {
  switch (s.type) {
    case "Circle": return Math.PI * s.r ** 2      // s: Circle & { type: "Circle" }
    case "Square": return s.side ** 2
    default: {
      const unhandled: never = s                  // a new Scala case → regenerate → compile error
      throw new Error(`unknown shape: ${JSON.stringify(unhandled)}`)
    }
  }
}

const s: Shape = { type: "Circle", r: 2 }          // the tag is required when building one
type TaggedCircle = Extract<Shape, { type: "Circle" }>
```

### jsoniter-scala

The defaults already use a flat `"type"` discriminator with the simple class name. Three
settings remain:

```scala
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}

object json {
  // case classes, sealed traits, enums with fields
  inline def codec[T]: JsonValueCodec[T] =
    JsonCodecMaker.make[T](
      CodecMakerConfig
        .withTransientEmpty(false)              // write empty collections: TS has them as required
        .withTransientDefault(false)            // write default values: same
        .withRequireDiscriminatorFirst(false)   // accept {"r":2,"type":"Circle"} from TS clients
    )

  // enums with only parameterless cases: "Red", not {"type":"Red"}
  inline def enumCodec[T]: JsonValueCodec[T] =
    JsonCodecMaker.make[T](CodecMakerConfig.withDiscriminatorFieldName(None))
}

object Drawing { given JsonValueCodec[Drawing] = json.codec }
object Color { given JsonValueCodec[Color] = json.enumCodec }
```

By default jsoniter rejects a tagged object whose first key isn't `type` ("expected key:
\"type\""). JavaScript keeps keys in insertion order, so `{ r: 2, type: "Circle" }` sent from
TypeScript fails without `withRequireDiscriminatorFirst(false)`.

### zio-json

zio-json wraps sealed traits by default (`{"Circle":{"r":1.0}}`), which doesn't match.
Annotate every sealed trait and every enum with fields:

```scala
import zio.json.*

@jsonDiscriminator("type")
sealed trait Shape
case class Circle(r: Double) extends Shape
case class Square(side: Double) extends Shape

enum Color { case Red, Green }                  // already "Red" by default: no annotation

object Circle { given JsonCodec[Circle] = DeriveJsonCodec.gen }
object Square { given JsonCodec[Square] = DeriveJsonCodec.gen }
object Shape { given JsonCodec[Shape] = DeriveJsonCodec.gen }
object Color { given JsonCodec[Color] = DeriveJsonCodec.gen }
```

`None` fields are omitted and empty collections written by default, and the discriminator
may appear at any position when decoding. Two traps:

- Don't set `SumTypeHandling.DiscriminatorField("type")` in a `JsonCodecConfiguration`
  in scope of parameterless enums: they become `{"type":"Red"}`, also with
  `enumValuesAsStrings = true`. Use the annotation instead.
- `@jsonHint("circle")` changes the tag on the wire; the TypeScript still says `"Circle"`.

### Large numbers

`Long`, `BigInt` and `BigDecimal` become `number`, and both codecs write them as JSON numbers.
JavaScript reads numbers as doubles, so ids above 2^53 or exact decimals arrive rounded.
Write such fields as strings on both sides:

```scala
given TsType[Long] = TsType(TsExpr.TsString)              // in scope where you derive
case class Account(@stringified id: Long, balance: BigDecimal) // jsoniter: "id":"9007199254740993"
```

### Either

There's no default for `Either`: codecs disagree on its shape (jsoniter
`{"type":"Left","value":1}`, zio-json `{"Left":1}`), and `derive` fails on it with
"Cannot derive TsType for scala.util.Either[...]". Model the alternatives as a sealed
trait instead, which gives a tagged union both sides understand. If your codec writes
`Either` your own way, provide a given for that shape, e.g. for the bare value:

```scala
given [L: TsType, R: TsType] => TsType[Either[L, R]] =
  TsType(summon[TsType[L]].get | summon[TsType[R]].get)
```

Checked with jsoniter-scala 2.41.2 and zio-json 1.1.0.

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
