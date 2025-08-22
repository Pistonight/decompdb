# tyyaml
TyYAML (Type YAML) is a yaml-compatible format that stores types, layouts, and names.

## Type Representation
A `TYPE` in TyYAMl is represented as a sequence (an array):

```
[ TYPE_ID, SPEC ... ]
```
`TYPE_ID` is a string value that can be one of:
  - primitive types: `void`, `bool`, `u8`, `u16`, `u32`,
    `u64`, `i8`, `i16`, `i32`, `i64`, `f32`, `f64`
  - or a named type: `"NAME"`. Note the quotes around the name are part of the value. In the actual document, it would be further surrounded by single quotes `'"NAME"'`

The `SPEC ...` part of the sequence can be:
  - pointer spec: one element that is the string value `*`.
    Example: `[ u32, '*' ]` is `u32*` in C.
  - array spec: one element that is a sequence with one number, the length of the array. Example: `[ u32, [16] ]` is `u32 foo[16]` in C
  - subroutine spec: 2 elements. The first is the string value `()` to indicate this is a subroutine. The second is a sequence of `TYPE`s that are the function parameters. Example `[ u32, '()', [ [ u32 ], [ u32, '*'] ], '*' ]` is `u32 (*foo)(u32, u32*)` in C. Note the last `*` is part of another pointer spec. See below. 
  - member spec: 2 Elements. The first is another `TYPE_ID` that is the base type.
    The second is the string value `::`.

Multiple `SPEC` can follow the `TYPE_ID`. For example, `[ u32, '*', '*' ]` is `u32**`.
For member spec, there must be 1 or 2 more specs that follow:
  - pointer-to-member-data: there must be 1 more pointer spec '*'. Example `[ u32, '"Foo"', '::', '*' ]`
    is `u32 Foo::*bar` in CPP.
  - pointer-to-member-function: there must be 1 more subroutine spec and 1 more pointer spec.
    Example `[ u32, '"Foo"', '::', '()', [ [ u32 ], [ u32, '*' ] ], '*' ]` is `u32 (Foo::*bar)(u32, u32*)` in CPP.

## Struct Representation
A struct is represented as a YAML mapping:
  - `name` is the literal name of the struct, without quotes. For example, the literal YAML can be `name: 'Foo::bar'`
  - `size` is the size of the struct in hex, prefixed with `0x`, e.g. `size: 0x60`
  - `align` is the alisngment of the struct in hex, prefixed with `0x`, e.g. `size: 0x8`
  - `vtable` (optional) is the layout of the virtual function table for this struct.
    - `vtable` is a sequence of mappings. Each mapping has `name` and `type`. `name`
      is the name of the virtual function, and `type` is the `TYPE` of the function (which must be `TYPE_ID` of the return type, followed by a subroutine spec and a pointer spec).
    - Ctors and Dtors may be suffixed with `D0`, `D1`, etc, to disambiguate.
  - `members` is the layout of the members of the struct, which is a mapping.
    - the keys of the mapping are string hex values that is the offset of the member
      in the struct.
    - the value of the mapping is another mapping that describes the member:
      - `base` (optional): boolean to indicate if this member is a base class
      - `vtable` (optional): string for a name of a struct. The `vtable` for that struct
        will be used.
      - `name`: name of the member, omitted if `vtable` or `base` is true.
      - `type`: `TYPE` of the member, omitted if `vtable` or `base` is true. 
    - the entries in the mapping must be sorted by the offset (key).

An example struct:
```yaml
name: 'ksys::snd::InfoData'
size: 0x80
align: 0x1
vtable:
  - { name: '~InfoDataD1', type: [ void,'()', [['"ksys::snd::InfoData"','*']],'*' ] }
  - { name: '~InfoDataD0', type: [ void,'()', [['"ksys::snd::InfoData"','*']],'*' ] }
members:
  '0x0': { vtable: 'ksys::snd::InfoData' }
  '0x8': { name: 'mSingletonDisposerBuf_', type: [ u32,[8] ] }
  '0x28': { name: 'mRootIter', type: [ '"al::ByamlIter"','*' ] }
  '0x30': { name: 'mResHandle', type: [ '"ksys::res::Handle"' ] }
```

## Enum Representation
An enum is represented as a mapping.
- `name`: same as `name` for a struct.
- `size`: same as `size` for a struct.
- `enumerators` is a mapping of:
  - key is the name of the enumerator
  - value is the value of the enumerator in hex, prefix with `0x`

Example:
```yaml
name: 'GLSLCinitializationStatus'
size: 0x4
enumerators:
  GLSLC_INIT_ERROR_UNINITIALIZED: 0x0
  GLSLC_INIT_SUCCESS: 0x1
  GLSLC_INIT_ERROR_ALLOC_FAILURE: 0x2
  GLSLC_INIT_ERROR_NO_ALLOC_CALLBACKS_SET: 0x3
```

## Union Representation
A union is represented as a mapping.
- `name`: same as `name` for a struct.
- `size`: same as `size` for a struct.
- `align`: same as `align` for a struct.
- `members`: a mapping of:
  - key is the name of the union member
  - value is the `TYPE` of the union member.

Example:
```yaml
name: 'GLSLCsectionHeaderUnion'
size: 0x90
align: 0x1
members:
  genericHeader: [ '"GLSLCgenericHeader"' ]
  gpuCodeHeader: [ '"GLSLCgpuCodeHeader"' ]
  asmDumpHeader: [ '"GLSLCasmDumpHeader"' ]
  perfStatsHeader: [ '"GLSLCperfStatsHeader"' ]
  programReflectionHeader: [ '"GLSLCprogramReflectionHeader"' ]
  debugInfoHeader: [ '"GLSLCdebugInfoHeader"' ]
```

## Address Symbol Representation
An address symbol (name) is represented as a mapping:

- `func` or `data`: the name of the address/symbol. `func` indicates the address
  is a function, `data` indicates it's a data symbol.
- `type` (optional): the type of the data, or the return type of the function
- `args`: (optional, function only), an array of mappings of:
  - `name`: the name of the argument
  - `type`: the `TYPE` of the argument

Not all function symbols are required to have args listed.
