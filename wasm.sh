alias emcc='docker run --rm -u $(id -u):$(id -g)  -v $PWD:/src emscripten/emsdk emcc'
alias em++='docker run --rm -u $(id -u):$(id -g)  -v $PWD:/src emscripten/emsdk em++'
alias emrun='docker run -p 8080:8080 --rm -u $(id -u):$(id -g) -v $PWD:/src emscripten/emsdk emrun --no_browser --port 8080 '
alias wabt='docker run --rm -u $(id -u):$(id -g)  -v $PWD:/src lambeta/wabt'

function list-wabt {
  echo "wabt help"
  echo "wasm2c"
  echo "wasm-c-api-finalize"
  echo "wasm-c-api-hostref"
  echo "wasm-c-api-reflect"
  echo "wasm-c-api-table"
  echo "wasm-decompile"
  echo "wasm-opcodecnt"
  echo "wasm2wat"
  echo "wasm-c-api-global"
  echo "wasm-c-api-memory"
  echo "wasm-c-api-serialize"
  echo "wasm-c-api-threads"
  echo "wasm-interp"
  echo "wasm-strip"
  echo "wasm-c-api-callback"
  echo "wasm-c-api-hello"
  echo "wasm-c-api-multi"
  echo "wasm-c-api-start"
  echo "wasm-c-api-trap"
  echo "wasm-objdump"
  echo "wasm-validate"
}

