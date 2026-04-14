# MSL regression tests

Small Metal Shading Language shaders that exercise rewriter rules known to
have been GLSL-centric. Each shader is compiled with `xcrun metal -c` both
as source and after minification through `--msl --format text`.

## Tests

| file | covers |
|---|---|
| `atomic.metal` | `atomic_*_explicit` side-effecting calls must survive the purity analysis |
| `const_array.metal` | a const array indexed once must not be inlined to `{..}[i]` |
| `texture_write.metal` | `tex.write(...)` method-syntax call must survive |
| `ref_param.metal` | writes through a `thread Ray&` parameter must survive |

## Running

```sh
./tests/msl/run.sh
```
