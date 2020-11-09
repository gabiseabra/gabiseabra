const path = require('path')
const {
  createConfig,
  setEnv,
  resolve,
  entryPoint,
  setOutput,
  babel
} = require('webpack-blocks')
const css = require('./blocks/css')
const typescript = require('./blocks/typescript')
const purescript = require('./blocks/purescript')

const ROOT = path.resolve(__dirname, '..')

/**
 * Razzle plugin to modify the client bundle's webpack config
 */
module.exports = createConfig([
  entryPoint('./src/index.tsx'),
  setOutput('./public/build/index.js'),
  resolve({
    modules: [path.join(ROOT, 'node_modules')]
  }),
  /**
   * Module resolvers
   */
  css(),
  babel(),
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
