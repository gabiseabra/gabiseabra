const path = require('path')
const {
  createConfig,
  setEnv,
  resolve,
  entryPoint,
  setOutput,
  babel
} = require('webpack-blocks')
const typescript = require('./blocks/typescript')

const ROOT = path.resolve(__dirname, '..')

/**
 * Razzle plugin to modify the client bundle's webpack config
 */
module.exports =
  createConfig([
    entryPoint('./src/index.ts'),
    setOutput('./build/index.js'),
    resolve({
      modules: [
        path.join(ROOT, 'node_modules')
      ]
    }),
    /**
     * Module resolvers
     */
    babel(),
    typescript(),
    /**
     * Env vars
     * Set default values for the output bundle's process.env here
     */
    setEnv({
      // ...
    })
  ])
