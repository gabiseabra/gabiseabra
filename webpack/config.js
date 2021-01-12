const path = require('path')
const {
  createConfig,
  setEnv,
  resolve,
  entryPoint,
  setOutput,
  babel
} = require('webpack-blocks')
const sass = require('./blocks/sass')
const glsl = require('./blocks/glsl')
const typescript = require('./blocks/typescript')
const purescript = require('./blocks/purescript')

const ROOT = path.resolve(__dirname, '..')

/**
 * Razzle plugin to modify the client bundle's webpack config
 */
module.exports = createConfig([
  entryPoint('./src/index.js'),
  setOutput('./public/build/index.js'),
  resolve({
    modules: [path.join(ROOT, 'node_modules')],
    alias: {
      '@styles': path.join(ROOT, 'src/Styles')
    }
  }),
  /**
   * Module resolvers
   */
  sass(),
  glsl(),
  babel(require('../.babelrc')),
  typescript(),
  purescript(),
  /**
   * Env vars
   * Set default values for the output bundle's process.env here
   */
  setEnv({
    // ...
  })
])
