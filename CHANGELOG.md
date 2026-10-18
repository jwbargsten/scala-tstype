# Changelog

Early SemVer: below 1.0, a minor version may break.

## 0.3.0

Breaking: union members get the tag in their own interface again, now optional.

- A member of a sealed trait or enum has `type?: "Circle"` in its interface; the union keeps requiring it,
  `((Circle & { type: "Circle" }) | ...)`. Code that typed tagged values with the member type (`send(msg: Circle)`
  with `{ type: "Circle", ... }`) compiles again, exhaustive `switch`es keep working, and a member used directly
  accepts JSON with or without the tag (circe, zio-json and default jsoniter write none there).
- README: exhaustive switches, circe setup.

## 0.2.0

Breaking: the generated TypeScript changes for tagged unions, `Option` outside fields, `Char`, `Either` and output
order; regenerate and run the frontend's type check.

- Tagged unions put the discriminator in the union, `((Circle & { type: "Circle" }) | ...)`; the member interface stays
  without it. A member also used directly (`c: Circle`) was emitted twice, and TS merged the two into an interface
  that required `type`.
- Two different declarations with the same name (e.g. `Page[A]` and `Page[B]`) make emitting throw instead of producing
  conflicting interfaces.
- `Option` outside a case class field (`Seq[Option[T]]`, map values) is `T | null`, as JSON codecs write it.
- `Char` is `string`.
- `Either` has no default: `derive` fails at compile time. Provide a given matching your codec.
- String literals are escaped; field and enum member names that aren't identifiers are quoted.
- `Duration`, `Period`, `MonthDay`, `ZoneId`, `ZoneOffset` map to `string`.
- Output order: input order, each type followed by the types it references (depth-first). Was hash order.
- Java enums are named by their qualified name, so same-named ones are disambiguated.
- A renamed recursive type refers to its new name.
- No compiler warning for recursive types.
- `TsEmitter.emitNamed`, `discoverNamed`, `resolveNameClashes` and `TsTypeMacros` are package-private; `TsType.getT` is
  removed (use `summon`).
- `+`/`++` in `syntax` report "add" instead of "subtract" on non-interfaces.
- README: matching the JSON codec (jsoniter-scala, zio-json), large numbers, `Either`, `derives TsType`.

## 0.1.0

First release.
