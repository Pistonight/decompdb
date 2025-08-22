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
  - POINTER: one element that is the string value `*`.
    Example: `[ u32,'*' ]` is `u32*`.
  - ARRAY: one element that is a sequence with one number, the length of the array.
    Example: `[ u32,[16] ]` is `u32[16]`.
  - SUBROUTINE: 2 elements should follow `TYPE_ID`: the string value `()`, and a sequence of `TYPE`s that are the function parameters.
    To form a pointer-to-subroutine type (i.e. a function pointer), another POINTER spec must follow.
    Example: `[ u32,'()',[[ u32 ],[ u32,'*' ]],'*' ]` is `u32 (*)(u32, u32*)`.
  - POINTER_TO_MEMBER: 2 Elements should follow `TYPE_ID`: the class `TYPE`, and the string value `::`.
    A POINTER or SUBROUTINE spec must then follow, to form a pointer-to-member-data or pointer-to-member-function type, respectively.
    Example: `[ u32,'"Foo"','::','*' ]` is `u32 Foo::*`, 
    `[ u32,'"Foo"','::','()',[[ u32 ],[ u32,'*' ]],'*' ]` is `u32 (Foo::*bar)(u32, u32*)`.

Once a `TYPE` is parsed, more `SPEC` can follow to continue building up the `TYPE`.
For example, `[ u32, '*', '*' ]` is `u32**`.

## Struct Representation
A struct is represented as a YAML mapping, for example:
```yaml
name: 'ksys::snd::InfoData'
size: 0x80
align: 0x1
vtable:
  - '~InfoDataD1': [ void,'()',[['"ksys::snd::InfoData"','*']],'*' ]
  - '~InfoDataD0': [ void,'()',[['"ksys::snd::InfoData"','*']],'*' ]
members:
  '0x00': { vtable: 'ksys::snd::InfoData' }
  '0x08': { 'mSingletonDisposerBuf_': [ u32,[8] ] }
  '0x28': { 'mRootIter': [ '"al::ByamlIter"','*' ] }
  '0x30': { 'mResHandle': [ '"ksys::res::Handle"' ] }
```

- `name` is the literal name of the struct, without quotes.
- `size` is the size of the struct in hex, prefixed with `0x`, as a numeric value.
- `align` is the alisngment of the struct in hex, prefixed with `0x`, as a numeric value.
- `vtable` (optional) is a sequence that represents the layout of the virtual function table for this struct.
  - Note that it would not contain information before the virtual functions array. (e.g. offset, RTTI).
  - It would contain virtual functions in the first base class (if any), and virtual functions in the derived class.
  - Each element in the sequence is a mapping, where the key is the `name` of the virtual function,
    and the value is the `TYPE` of that function - a pointer-to-subroutine type.
  - Ctors and Dtors may be suffixed with `C0`, `C1`, `C2`, `D0`, `D1` or `D2` to disambiguate.
- `members` is the layout of the members of the struct, which is a mapping.
  - The key of the mapping is the offset of the member in hex, prefixed with `0x`, as a string.
    The key should be 0-padded so that all keys have the same length.
  - The value of the mapping is another mapping, which can be one of:
    - `{ base: 'BaseClass' }` for a base class `BaseClass` at this offset.
    - `{ vtable: 'Class' }` for a virtual function table pointer that points to a function table that is the same type
      as the vtable of `Class`. Note that the vtable pointer might be part of the base class.
      In this case, TyYAML currently does not have the power to indicate the actual vtable type.
    - `{ 'mName': TYPE }` for a regular member with the name `mName` of type `TYPE`.
    - `{ pad: X }` for an implicit padding of `X` bytes. `X` is in hex, prefixed with `0x` as a numeric value.
      Note that implicit padding will never show up as the last member
  - The entries in the mapping must be sorted by the offset (key).


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
